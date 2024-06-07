library(tidyverse)
# library(tidytable)
library(sf)
library(geohashTools)
library(osmdata)

xy_precision=gh_delta(7L)

k12=readr::read_csv("https://stg-arcgisazurecdataprod1.az.arcgis.com/exportfiles-2023-105258/Public_Schools_-7669544197405643438.csv?sv=2018-03-28&sr=b&sig=cJbBcidZR0heujJyQkSyuX%2BJxOxL6BSMxDW96Nkb6To%3D&se=2024-06-06T21%3A09%3A48Z&sp=r")

k12$osm_response<-
  map2(k12$Longitude,k12$Latitude,
       ~opq_around(lon = .x,lat=.y,radius=250)|>
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
           )|>
         jsonify::to_json(.)
  )


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
