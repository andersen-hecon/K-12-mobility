library(tidyverse)
# library(tidytable)
library(sf)
library(geohashTools)
library(osmdata)
library(furrr)

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
