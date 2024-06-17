library(tidyverse)
library(tidytable)
library(tidylog)
library(sf)
library(geohashTools)

# at GH 7 level the x and y precisions are the same!
xy_precision=2*gh_delta(7L)

college_campuses<-
  st_read("https://stg-arcgisazurecdataprod1.az.arcgis.com/exportfiles-2023-98239/Colleges_and_Universities_Campuses_-6550814776683239053.geojson?sv=2018-03-28&sr=b&sig=5fHEnscO%2BBhaEax%2FdEexfhW4J%2F%2BPVjH7vZ%2F8Yni7Q%2BM%3D&se=2024-06-07T03%3A06%3A26Z&sp=r")


# transform to degrees
old_crs=st_crs(college_campuses)
new_crs=st_crs(4326)
college_campuses<-
  college_campuses|>
  st_transform(crs=new_crs)

# now get the gh list for each campus
f0<-\(g,n) {
  g=st_sfc(g,crs=new_crs)|>st_as_sf()|>
    st_make_valid()
  
  grid<-gh_covering(g,precision = 7L)
  
  grid<-
    grid|>
    st_join(
      g,
      left=FALSE
    )
  
  
  gh=tibble(UNIQUEID=n,gh=grid|>rownames())
  return(gh)
}
f<-purrr::possibly(f0)

gh_list<-
  purrr::map2(
    college_campuses$geometry,
    college_campuses$UNIQUEID,
    ~f(.x,.y),
    .progress = T
  )|>
  list_rbind()
  
gh_list|>write_csv("./Data/campus_geohash_list.csv")
