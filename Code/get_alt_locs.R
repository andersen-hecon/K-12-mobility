library(tidyverse)
# library(tidytable)
library(sf)
library(geohashTools)
library(osmdata)
library(furrr)

# get OSM polygons first

# get a list of boxes of interest
usa<-tigris::states(cb=TRUE)|>
  filter(STATEFP<=56)|>
  summarise(
    do_union=TRUE
  )

bb0=st_bbox(usa)

d=3
bb=d*floor(bb0/d)

gh_precision=2*gh_delta(7L)

# p=0.05
# expand_mat=((diag(4)-matrix(rep(1,4),ncol = 2)%x%(diag(2)*0.5))*(1+p)+(matrix(rep(1,4),ncol = 2)%x%(diag(2)*0.5)))
# dimnames(expand_mat)=
#   list(
#     c( "xmin","ymin","xmax","ymax"),
#     c( "xmin","ymin","xmax","ymax")
#   )
# 
usa_grid<-
  st_make_grid(usa,cellsize=c(d,d),offset = bb[1:2])|>
  st_as_sf()|>
  st_join(
    usa,
    left=FALSE
  )|>
  mutate(bb=map(x,~st_bbox(.)))|>
  mutate(
    # make the squares 5% larger to ensure that everything is capture
    # bb=map(bb,~.%*%expand_mat),
    # bb[1]=pmin(pmax(bb[1],-180),180),
    # bb[3]=pmin(pmax(bb[3],-180),180),
    # bb[2]=pmin(pmax(bb[2],-90),90),
    # bb[4]=pmin(pmax(bb[4],-90),90),
  )

data<-
  map(
    usa_grid$bb,
    \(n){
      d=opq(n,timeout = 1200)|>
        add_osm_features(features=list("amenity"="library",
                                       "amenity"="bar",
                                       "amenity"="pub",
                                       "amenity"="cafe",
                                       "amenity"="restaurant",
                                       "amenity"="fast_food")
                         )|>
        osmdata_sf()|>
        unique_osmdata()
      
      d=list(
        d$osm_points|>mutate(geo_type="point"),
        d$osm_polygons|>mutate(geo_type="polygon")
        )
      
      d=map(d,~select(.,geo_type,starts_with("osm"),matches("^amenity$"),matches("geometry")))
        # enframe()|>
        # filter(
        #   map_lgl(value,~"data.frame" %in% class(.)),
        #   str_detect(name,"polygon")
        # )|>
        # as_tibble()%>%
        # .$value|>
        # map(
        #   ~as_tibble(.)|>
        #     st_set_geometry("geometry")
        # )
      
      return(d)
    },
    .progress=T
  )


data_simple<-data|>map(~list_rbind(.))|>list_rbind()


process_geom<-purrr::possibly(
  \(g) {
    g_sf<-
      st_sfc(g, crs=st_crs("WGS84"))|>
      st_as_sf()|>
      st_make_valid()
    
    bb=st_bbox(g_sf)
    
    bb[1:2]=floor(bb[1:2]/gh_precision)*gh_precision
    bb[3:4]=ceiling(bb[3:4]/gh_precision)*gh_precision
    
    bb_sf=st_as_sfc(bb)|>st_set_crs(st_crs("WGS84"))
    grid=st_make_grid(bb_sf,gh_precision)|>st_as_sf()
    
    grid<-
      grid|>
      # rowwise()|>
      mutate(
        c=st_coordinates(st_centroid(x)),
        # gh=gh_encode(c[2],c[1],precision=7L)
      )
    
    grid$gh=gh_encode(grid$c[,2],grid$c[,1],precision=7L)
    
    grid|>
      st_join(
        g_sf
      )|>
      st_drop_geometry()|>
      select(gh)
  }
)

polygons_gh<-
  data_simple|>
  filter(geo_type=="polygon")|>
  mutate(
    gh=map(geometry,~process_geom(.),.progress = T)
  )
  
points_gh<-
  data_simple|>
  filter(geo_type=="point")|>
  mutate(
    c=st_coordinates(geometry),
    gh=gh_encode(c[,"Y"],c[,"X"], precision=7L)
  )

combined_ghs<-
  bind_rows(
    polygons_gh|>
      select(geo_type,osm_id,amenity,gh)|>
      unnest(cols=c(gh)),
    points_gh|>
      as_tibble()|>
      select(geo_type,osm_id,amenity,gh)
  )|>
  filter(
    amenity %in% c("bar","cafe","fast_food","library","pub","restaurant")
  )

combined_ghs|>write_csv("Data/combined_osm_amenities.csv.gz")


all_amenity_gh<-
  combined_ghs|>
  select(-osm_id,-geo_type)|>
  mutate(
    amenity=case_match(amenity,
                       c("bar","pub")~"bar",
                       c("cafe")~"cafe",
                       c("library")~"library",
                       c("restaurant","fast_food")~"restaurant"
                       )
  )|>
  distinct()

all_amenity_gh|>
  write_csv(
    "Data/amenities.csv.gz"
  )
