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

d=2.5
bb=d*floor(bb0/d)

gh_precision=2*gh_delta(7L)

p=0.05
expand_mat=((diag(4)-matrix(rep(1,4),ncol = 2)%x%(diag(2)*0.5))*(1+p)+(matrix(rep(1,4),ncol = 2)%x%(diag(2)*0.5)))
dimnames(expand_mat)=
  list(
    c( "xmin","ymin","xmax","ymax"),
    c( "xmin","ymin","xmax","ymax")
  )

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

usa_grid<-
  usa_grid|>
  mutate(
    bb=map(bb,
           \(x) {
             x[1]=pmin(pmax(x[1],-180),180)
             x[3]=pmin(pmax(x[3],-180),180)
             x[2]=pmin(pmax(x[2],-90),90)
             x[4]=pmin(pmax(x[4],-90),90)
             
             return(x)
           })
    
  )
  
data<-
  map(
    usa_grid$bb,
    \(n){
      d=opq(n,timeout = 600)|>
        add_osm_features(features=list("building"="school","amenity"="school"))|>
        osmdata_sf()|>
        unique_osmdata()|>
        pluck("osm_polygons")
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

data2<-
  data|>
  list_rbind()|>
  select(osm_id,name,building,amenity,geometry)

# let's pick out the schools that we match WELL and then figure out the rest
school_coords<-
  bind_rows(
    read_csv("Input-Data/NCES_public_schools_2019_2020.csv")|>select(NCESSCH,LEAID,NAME,LAT,LON),
    read_csv("Input-Data/NCES_private_schools_2019_2020.csv")|>select(PPIN,NAME,LAT,LON)
  )

school_coords<-
  school_coords|>
  st_as_sf(coords=c("LON","LAT"),
           crs=st_crs("WGS84"))



# try to match OSM schools to NCES schools; assume that OSM is right to within a kilometer
school_merge<-
  data2|>
  st_set_geometry("geometry")|>
  st_make_valid()|>
  st_join(school_coords,join = st_is_within_distance,dist=units::set_units(1000,"m"),left=FALSE)|>
  inner_join(as_tibble(school_coords),by=join_by(NCESSCH,LEAID,PPIN,NAME))

# because some schools are CO LOCATED we need to first use exact distance and then look to names (I'm looking at you Albertville!)
school_merge<-
  school_merge|>
  mutate(
    a=st_area(geometry.x),
    d=st_distance(geometry.x,geometry.y,by_element = T),
    across(c(a,d),~units::drop_units(.)))

school_merge<-
  school_merge|>
  mutate(
    exact_location=if_else(d<=10,1,0) # allow for up to ten meters discrepancy
  )|>
  filter(
    exact_location==max(exact_location),.by=osm_id
  )

# now merge on NAME but only drop if the location is wrong too
school_merge<-
  school_merge|>
  mutate(
    # "school" is inconsistently used in these data, so remove it and then trim everything
    across(c(NAME,name),~str_squish(str_remove(str_to_upper(.),"SCHOOL"))),
    exact_name=if_else(str_to_upper(NAME)==str_to_upper(name),1,0,missing=0)
  )|>
  filter(exact_name==max(exact_name) | exact_location==1,.by=osm_id)


# now just pick the closest school
school_merge<-
  school_merge|>
  mutate(
    closest=if_else(d==min(d),1,0),
    .by=osm_id
  )
  
school_merge<-
  school_merge|>
  filter(
    pmax(exact_location,exact_name,closest)==1
  )



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

school_merge_gh<-
  school_merge|>
  as_tibble()|>
  mutate(
    gh=map(geometry.x,~process_geom(.))
  )|>
  select(
    osm_id,NCESSCH,LEAID,PPIN,NAME,gh
  )|>
  unnest(cols=c(gh))|>
  distinct(NCESSCH,LEAID,PPIN,NAME,gh)|>
  mutate(ring=0)

school_merge_gh<-
  school_merge_gh|>
  mutate(
  across(c(NCESSCH,LEAID,PPIN),~replace_na(.,"")),
  id=glue::glue("NCESSCH{NCESSCH}:LEAID{LEAID}:PPIN{PPIN}")
)
  
  
school_merge_gh|>
  write_csv("./Data/matched_schools_gh.csv.gz")


# now combine the two datasets
nces_ring_gh=read_csv("Data/nces_ring_gh.csv.gz")

combined<-
  nces_ring_gh|>
  anti_join(distinct(school_merge_gh,id))|>
  bind_rows(school_merge_gh)

combined|>
  write_csv("./Data/combined_schools_gh.csv.gz")



