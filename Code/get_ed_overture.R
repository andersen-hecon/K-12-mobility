library(tidyverse)
library(sf)

overture_cats<-
  read_delim("https://github.com/OvertureMaps/schema/raw/main/task-force-docs/places/overture_categories.csv",delim = ";")

comb_tags<-
  overture_cats|>
  filter(
    str_detect(`Overture Taxonomy`,"health_and_medical")
    |
      (
        str_detect(`Overture Taxonomy`,"education") & str_detect(`Overture Taxonomy`,"school")
        & !str_detect(`Overture Taxonomy`,"specialty_school") & !str_detect(`Overture Taxonomy`,"college_university")
      )
  )

target_tags<-
  comb_tags|>
  select(cat=`Category code`)|>
  distinct()


ov_health_ed<-read_csv("C:/Users/msander4/Box/RESEARCH - Martin Andersen/Data/Overture/health_ed.csv")

ov_health_ed<-
  ov_health_ed|>
  filter(country=="US")

# now let's look at the education locations

ed_tags<-
  comb_tags|>
  filter(
    (
      str_detect(`Overture Taxonomy`,"education") & str_detect(`Overture Taxonomy`,"school")
      & !str_detect(`Overture Taxonomy`,"specialty_school") & !str_detect(`Overture Taxonomy`,"college_university")
    )
  )

ov_education<-
  ov_health_ed|>
  filter(
    category.primary %in% ed_tags$`Category code`
  )
    
    if_any(starts_with("category"),~. %in% ed_tags$`Category code`)
  )

# ov_education|>
#   filter(if_all(starts_with("category"), ~if_else(. =="college_university",FALSE,TRUE,missing=TRUE)))|>
#   select(starts_with("category"))|>
#   count(category.primary,category.alternate.1,category.alternate.2)
# ?

nces_public<-
  read_csv("Input-Data/NCES_public_schools_2019_2020.csv")|>
  janitor::clean_names()|>
  filter(stfip<=56)|>
  st_as_sf(coords=c("lon","lat"),crs=st_crs("WGS84"))

ov_education_sf<-
  ov_education|>
  st_as_sf(coords=c("coords.X","coords.Y"),crs=st_crs("WGS84"))

counties<-tigris::counties(year=2019)

counties<-
  counties|>
  select(cnty=GEOID)
counties<-
  counties|>
  st_transform(crs=st_crs("WGS84"))

ov_ed_sf_cnty1<-
  ov_education_sf|>
  st_join(counties, left=FALSE)

ov_ed_sf_cnty2<-
  ov_education_sf|>
  anti_join(select(as_tibble(ov_ed_sf_cnty1),id))|>
  st_join(counties, left=FALSE, join = st_nearest_feature)

ov_education_sf<-
  bind_rows(
    ov_ed_sf_cnty1,
    ov_ed_sf_cnty2
  )

c=nces_public$cnty[[1]]

nces_c<-
  nces_public|>
  filter(cnty==c)
ov_c<-
  ov_education_sf|>
  filter(cnty==c)


name_matched<-
  nces_public|>
  st_drop_geometry()|>
  group_by(cnty)|>
  group_modify(
    \(nces_c,n) {
      print(n$cnty)
      ov_c=ov_education_sf|>
        filter(cnty==n$cnty, !is.na(name.primary))|>
                 st_drop_geometry()|>
        mutate(name=name.primary)
      
      fuzzyjoin::stringdist_inner_join(nces_c, ov_c,by="name",distance_col="a_d")|>
        select(-cnty)
      
    }
  )

# first let's mathc on name and county
for(c in unique(nces_public$cnty)){
  print(c)
  
}



|>
  st_join(filter(|>filter(cnty==c)),join=st_nearest_feature, left = FALSE)|>
  inner_join(as_tibble(ov_education_sf)|>select(id,geometry.ov=geometry))|>
  mutate(d=st_distance(geometry,geometry.ov, by_element = TRUE))
  



target_sf=nces_public|>
  filter(state=="HI")

donor_sf=ov_education_sf|>
  filter(region=="HI")

out_sf<-
  target_sf|>
  st_join(
    donor_sf, join = st_nearest_feature
  )

out_sf<-
  out_sf|>
  inner_join(as_tibble(donor_sf)|>rename(geometry.ov=geometry))|>
  mutate(
    d=st_distance(geometry,geometry.ov,by_element = T),
    n.d=stringdist::stringsim(name.primary,name)
    )|>
  filter(n.d==1 | max(n.d)<1, .by=id)|>
  filter(d==min(d),.by=id)

# now get the unmatched targets and donors
target_sf<-
  target_sf|>
  anti_join(st_drop_geometry(out_sf))

donor_sf<-
  donor_sf|>
  anti_join(st_drop_geometry(out_sf))

target_sf|>
  st_join(
    donor_sf, join = st_nearest_feature
  )


|>
  st_join(
    
    ,
          join = st_nearest_feature
  )

ov_education|>
  mutate(
    zip=str_extract(postcode,"^[0-9]{5}")
  )|>
  inner_join(nces_public, by=join_by(zip, name.primary==name))|>
  add_count(id)|>
  add_count(ncessch)


# school_coords<-
#   bind_rows(
    
    |>select(NCESSCH,LEAID,NAME,LAT,LON)
    read_csv("Input-Data/NCES_private_schools_2019_2020.csv")|>select(PPIN,NAME,LAT,LON)
  )
