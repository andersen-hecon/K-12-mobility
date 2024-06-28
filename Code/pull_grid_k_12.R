library(tidyverse)
# library(tidytable)
library(sf)
library(geohashTools)
library(osmdata)
library(furrr)

# get a list of boxes of interst
usa<-tigris::states(cb=TRUE)|>
  filter(STATEFP<=56)|>
  summarise(
    do_union=TRUE
  )

bb0=st_bbox(usa)

d=2.5
bb=d*floor(bb0/d)

gh_precision=2*gh_delta(7L)

usa_grid<-
  st_make_grid(usa,cellsize=c(d,d),offset = bb[1:2])|>
  st_as_sf()|>
  st_join(
    usa,
    left=FALSE
  )|>
  mutate(bb=map(x,~st_bbox(.)))


data<-
  map(
  usa_grid$bb,
  \(n){
    d=opq(n,timeout = 600)|>
      add_osm_features(features=list("building"="school","amenity"="school"))|>
      osmdata_sf()|>
      unique_osmdata()|>
      enframe()|>
      filter(
        map_lgl(value,~"data.frame" %in% class(.)),
        str_detect(name,"polygon")
      )|>
      as_tibble()%>%
      .$value|>
      map(
        ~as_tibble(.)|>
          st_set_geometry("geometry")
      )
  },
  .progress=T
  )

data2<-
  map(
    data,
    \(d) {
      d=if(length(d)>1) list_rbind(d)
      else d[[1]]
      
    })|>
  list_rbind()

data2|>
  janitor::clean_names()|>
  st_write("foo.geojson")


data2<-
  st_read("foo.geojson")

data2|>
  st_drop_geometry()|>
  write_csv("Data/k-12-fields.csv.gz")

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
  select(osm_id,name,building,amenity)|>
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

school_merge|>
  as_tibble()|>
  select(osm_id,NCESSCH,LEAID,PPIN)|>
  inner_join(st_drop_geometry(school_coords))|>
  write_csv("./Data/matched_schools.csv.gz")
  

unmatched<-
  school_coords|>
  as_tibble()|>
  select(-NAME)|>
  anti_join(
    school_merge,
    # by=join_by(NCESSCH,LEAID,PPIN)
  )|>
  distinct()|>
  mutate(
    c=st_coordinates(geometry),
    X=c[,"X"],
    Y=c[,"Y"],
    c=NULL
  )|>
  select(-geometry)

unmatched|>
  write_csv("./Data/unmatched_schools.csv.gz")


unmatched<-
  read_csv("./Data/unmatched_schools.csv.gz")


gh_to_sf<-purrr::possibly(geohashTools::gh_to_sf)

get_osm_nearby<-purrr::possibly(
  \(x,y){
    z0=opq_around(x,y,radius = 250,timeout=600)|>
      osmdata_sf()|>
      # unique_osmdata()|>
      enframe(name="type",value="geom")|>
      filter(
        map_lgl(geom,~"data.frame" %in% class(.))
      )|>
      mutate(
        geom=map2(geom,type,~as_tibble(.x)|>mutate(type=.y))
      )
    
    # now combine the polys
    z=if(nrow(z0)>1) z0$geom|>list_rbind() else z0$geom
    
    z<-
      z|>
      st_set_geometry("geometry")|>
      relocate(osm_id,type,name)|>
      mutate(type=str_remove(type,"^osm_"))
    
    # get rid of things from TIGER
    z<-
      z|>
      filter(
        replace_na(if_any(starts_with("boundary"),~replace_na(.!="administrative",TRUE)),TRUE)
      )
    
    if(!"amenity" %in% names(z)) z$amenity=NA_character_
    if(!"building" %in% names(z)) z$building=NA_character_
    
    
    # first-- look for SCHOOLS
    school_geoms<-
      z|>filter(if_any(-c(osm_id,geometry,type),~str_detect(str_to_lower(.),"school")))|>
      rename_with(~paste0("point_",.),-geometry)
    
    p_sf<-
      st_point(c(x,y))|>
      st_sfc()|>
      st_as_sf()|>
      rename(geometry=x)|>
      st_set_crs(st_crs("WGS84"))
    
    if(is.null(school_geoms) | nrow(school_geoms)==0) school_geoms=tibble(point_type="")
    
    if(!any(str_detect(school_geoms$point_type,"polygon"))) {
      print("no polygons")
      # so there are NO school polygons here, what do we do?
      if(!is.null(school_geoms) & any(str_detect(school_geoms$point_type,"point"))) {
        p_sf<-school_geoms|>
          select(-point_type)
      } else {
        print("no points")
      }
      
      # now we have some points, let's try to find a "good" set of candidates
      candidates<-
        z|>
        filter(str_detect(type,"polygon"),
               if_any(c(amenity,building),~!is.na(.)))|>
        st_join(p_sf, left=FALSE)
      
      if(nrow(candidates)==0) {
        print("no exact matches, trying nearest")
        # we didn't find anything, let's use a distance and just get all amenities and buildings that are nearby (100m)!
        candidates<-
          z|>
          filter(str_detect(type,"polygon"),
                 if_any(c(amenity,building),~!is.na(.)))|>
          st_join(p_sf, left=FALSE, join = st_is_within_distance, dist=units::set_units(100,"m"))
        
        print(nrow(candidates))
      }
      
      if(nrow(candidates)==0) {
        print("falling back to geohash 7")
        candidates<-
          p_sf|>
          mutate(
            c=st_coordinates(geometry),
            gh=geohashTools::gh_encode(c[,"X"],c[,"Y"], precision=7L),
            type="geohash",
            c=NULL
          )|>
          as_tibble()|>
          gh_to_sf()|>
          select(-gh)
          # geohashTools::gh_to_sf()
      }        

      school_geoms<-candidates
    }
    
    return(school_geoms)
    
  }
)
  

unmatched|>
  mutate(
    zz=map2(X,Y,
            ~get_osm_nearby(.x,.y))
  )


zzz|>
  mutate(
    zz2=map()
  )
  z=
    map(z,
        ~filter(.,if_any(-c(osm_id,geometry),~str_detect(.,"[Ss][Cc][Hh][Oo][Oo][Ll]")))|>janitor::remove_empty("cols")
    )
        
        
  
  z=if(length(z)>1) list_rbind(z)
  else if(length(z)==1) z[[1]]
  else z
}



um<-
  unmatched|>
  mutate(
    c=st_coordinates(geometry),
    cX=c[,"X"],
    cY=c[,"Y"]
  )|>
  slice(1:1000)|>
  rowwise()|>
  mutate(
    osm=list(
      opq_around(cX,cY,radius = 250)|>
        osmdata_sf()|>
        unique_osmdata()
    )
  )












#get geometry info

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

# sf_use_s2(T)
gh_list<-
  data2|>
  select(osm_id,geometry)|>
  mutate(
    geometry=map(geometry,~process_geom(.),..progress=TRUE)
  )


gh_list_long<-
  gh_list|>
  unnest(cols=c(geometry))

gh_list_long|>
  write_csv("Data/gh_list_osm_long_schools.csv.gz")

school_coords<-
  bind_rows(
    read_csv("Input-Data/NCES_public_schools_2019_2020.csv")|>select(NCESSCH,LEAID,NAME,LAT,LON),
    read_csv("Input-Data/NCES_private_schools_2019_2020.csv")|>select(PPIN,NAME,LAT,LON)
  )

school_coords<-
  school_coords|>
  st_as_sf(coords=c("LON","LAT"),
           crs=st_crs("WGS84"))


school_merge<-
  school_merge|>
  mutate(
    a=st_area(geometry.x),
    d=st_distance(geometry.x,geometry.y,by_element = T))

school_merge<-
  school_merge|>
  rowwise()|>
  mutate(
    # "school" is inconsistently used in these data, so remove it and then trim everything
    across(c(NAME,name),~str_trim(str_remove(str_to_upper(.),"SCHOOL"))),
    name_dist=adist(str_to_upper(NAME),str_to_upper(name))[1,1]
  )|>
  ungroup()

school_merge<-
  school_merge|>
  mutate(
    # d=units::drop_units(d),
    closest_dist=if_else(d==min(d),1,0,missing=0),
    closest_name=if_else(name_dist==min(name_dist),1,0,missing=0),
    exact_dist=if_else(d<=1,1,0,missing=0),
    exact_name=if_else(name_dist==0,1,0,missing=0),
    
    .by=osm_id
  )

close_schools<-
  school_merge|>
  filter(
    exact_name==max(exact_name),.by=osm_id
  )|>
  filter(
    exact_dist==max(exact_dist),.by=osm_id
  )|>
  filter(d==min(d),.by=osm_id)|>
  filter(name_dist==min(replace_na(name_dist,1000)),.by=osm_id)
  
  filter(closest_name==max(closest_name),.by=osm_id)


school_merge|>
  filter(name_dist==min(name_dist),.by=osm_id)

school_merge|>
  as_tibble()|>
  rowwise()|>
  mutate(
    ad=adist(str_to_upper(NAME),str_to_upper(name))
  )
closest_schools<-
  school_merge|>as_tibble()|>filter(d==min(d),.by=osm_id)

.lasttags<-
  data2|>
  



map_dfr(
  names(data2),
  \(n){
  en=sym(n)
  data2|>
    count(check=is.na(!!en))|>
    mutate(name=!!n)|>
    mutate(p=prop.table(n))
  }
)
  




data|>map(
  ~
  )

|>
  list_rbind()
  




xy_precision=2*gh_delta(7L)

k12=readr::read_csv("./Input-Data/Public_Schools_-7669544197405643438.csv")

get_nearby_osm<-
  purrr::possibly(
    function(lon,lat,radius=250,id=NULL) {
      osm_d<-
        opq_around(lon = lon,lat=lat,radius=radius)|>
        osmdata_sf()|>
        enframe()|>
        filter(
          str_detect(name,"^osm_"),
          !map_lgl(value,~is.null(.))
        )|>
        mutate(
          value=map(value,~filter(.,if_any(-geometry,~str_to_lower(.)=="school")))
        )|>
        filter(
          map_lgl(value,~nrow(.)>0)
        )
      
      keep_vars=c("name","amenity","building")
      osm_d<-
        osm_d|>
        mutate(
          value=map(
            value,
            \(v) {
              v=v|>select(any_of(c(keep_vars)))
              
              v<-
                v|>rownames_to_column(var="osm_id")
              for (var in keep_vars) {
                if (!(var %in% colnames(v))) {
                  v[[var]] <- ""
                }
              }
              
              
              
              return(v)
            }
          )
        )
      
      osm_d=if(nrow(osm_d)==1) osm_d$value[[1]]
      else bind_rows(osm_d$value)
      
    
      if(!is.null(id)) osm_d$id=id
      return(osm_d)
    }
  )

osm_responses<-
  pmap(
    list(k12$Longitude,k12$Latitude,k12$`NCES ID`),
       ~get_nearby_osm(..1,..2,id=..3),
       .progress = T,
  )

osm_responses|>
  list_rbind()|>
  mutate(
    geo_text=st_as_text(geometry)
  )|>
  select(-geometry)|>
  write_csv("foo.csv")
       

k12|>
  write_csv("foo.csv")


k12|>
  arrow::as_arrow_table()
  
  writexl::csv("foo.csv")
  arrow::write_parquet("foo.parquet")

bb=bboxes[1,]
bb_delta=c(-1,-1,1,1)*0.05


opq_enclosing(lon = -79.78564,lat=36.10911)

# school_sf<-
#   # opq(bb+bb_delta)|>
opq_around(lon = -79.78564,lat=36.10911,
           radius=250)|>
  # add_osm_features(list("amenity"="school",
  #                       "building"="school")
  #                 )|>
  osmdata_sf()|>
  enframe()|>
  filter(
    str_detect(name,"^osm_"),
    !map_lgl(value,~is.null(.))
  )|>
  mutate(
    value=map(value,~filter(.,if_any(-geometry,~str_to_lower(.)=="school")))
  )|>
  filter(
    map_lgl(value,~nrow(.)>0)
  )



# school_sf$osm_polygons|>
#   ggplot()+

  school_sf$osm_polygons|>filter(if_any(-geometry,~.=="school"))|>ggplot()+
    ggspatial::annotation_map_tile(zoom = 15)+
  geom_sf(alpha=0.3)
