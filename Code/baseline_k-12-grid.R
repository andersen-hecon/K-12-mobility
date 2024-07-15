library(tidyverse)
library(sf)
library(geohashTools)


school_coords<-
  bind_rows(
    read_csv("Input-Data/NCES_public_schools_2019_2020.csv")|>select(NCESSCH,LEAID,NAME,LAT,LON),
    read_csv("Input-Data/NCES_private_schools_2019_2020.csv")|>select(PPIN,NAME,LAT,LON)
  )




school_coords$gh=gh_encode(school_coords$LAT,school_coords$LON,precision = 7L)

# construct the neighbors
matrix=
  bind_rows(
    tibble_row(ring=1,dir="ne",d_lat= 2,d_lon= 2),
    tibble_row(ring=1,dir="n" ,d_lat= 2,d_lon= 0),
    tibble_row(ring=1,dir="nw",d_lat= 2,d_lon=-2),
    tibble_row(ring=1,dir="se",d_lat=-2,d_lon= 2),
    tibble_row(ring=1,dir="s" ,d_lat=-2,d_lon= 0),
    tibble_row(ring=1,dir="sw",d_lat=-2,d_lon=-2),
    tibble_row(ring=1,dir= "e",d_lat= 0,d_lon= 2),
    tibble_row(ring=1,dir= "w",d_lat= 0,d_lon=-2),
    
    # now do the second ring
    tibble_row(ring=2,dir="ne2",d_lat= 4,d_lon= 4),
    tibble_row(ring=2,dir="nne",d_lat= 4,d_lon= 2),
    tibble_row(ring=2,dir="nn2",d_lat= 4,d_lon= 0),
    tibble_row(ring=2,dir="nnw",d_lat= 4,d_lon=-2),
    tibble_row(ring=2,dir="nw2",d_lat= 4,d_lon=-4),
    tibble_row(ring=2,dir="ene",d_lat= 2,d_lon= 4),
    tibble_row(ring=2,dir="wnw",d_lat= 2,d_lon=-4),
    tibble_row(ring=2,dir="ee2",d_lat= 0,d_lon= 4),
    tibble_row(ring=2,dir="ww2",d_lat= 0,d_lon=-4),
    tibble_row(ring=2,dir="se2",d_lat=-4,d_lon= 4),
    tibble_row(ring=2,dir="sse",d_lat=-4,d_lon= 2),
    tibble_row(ring=2,dir="ss2",d_lat=-4,d_lon= 0),
    tibble_row(ring=2,dir="ssw",d_lat=-4,d_lon=-2),
    tibble_row(ring=2,dir="sw2",d_lat=-4,d_lon=-4),
    tibble_row(ring=2,dir="ese",d_lat=-2,d_lon= 4),
    tibble_row(ring=2,dir="wsw",d_lat=-2,d_lon=-4),
    
    tibble_row(ring=0,dir="c",d_lat=0,d_lon=0),
  )

get_neighbors<-\(lat,lon, precision) {
  delta=gh_delta(precision)
  
  matrix|>
    mutate(
      lat=lat+d_lat*delta[[2]],
      lon=lon+d_lon*delta[[2]],
    )|>
    mutate(
      gh=gh_encode(lat,lon,precision)
    )|>
    select(name=dir,value=gh)|>
    pivot_wider(names_prefix="neighbor_")
}

delta=gh_delta(7L)

school_gh_ring<-
  school_coords|>
  cross_join(matrix)|>
  mutate(neighbor_gh=gh_encode(LAT+d_lat*delta[[2]],
                             LON+d_lon*delta[[2]],
                             7L)
  )|>
  select(NCESSCH,LEAID,PPIN,NAME,ring,gh=neighbor_gh)

school_gh_ring|>
  mutate(
    across(c(NCESSCH,LEAID,PPIN),~replace_na(.,"")),
    id=glue::glue("NCESSCH{NCESSCH}:LEAID{LEAID}:PPIN{PPIN}")
  )|>
  write_csv("Data/nces_ring_gh.csv.gz")



