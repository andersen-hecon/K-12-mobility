# install.packages("overtureR")

library(tidyverse)
library(overtureR)


NCES_data<-
  bind_rows(
    tibble(NCESSCH=NA_character_,LEAID=NA_character_,PPIN=NA_character_,NAME=NA_character_,LAT=NA_real_,LON=NA_real_,STREET=NA_character_, CITY=NA_character_, STATE=NA_character_, ZIP=NA_character_, STFIP=NA_character_, CNTY=NA_character_),
    read_csv("Input-Data/NCES_public_schools_2022_2023.csv")|>select(NCESSCH,LEAID,NAME,LAT,LON,STREET, CITY, STATE, ZIP, STFIP, CNTY),
    # read_csv("Input-Data/NCES_private_schools_2019_2020.csv")|>select(PPIN,NAME,LAT,LON,STREET, CITY, STATE, ZIP, STFIP, CNTY)
  )|>
  filter(STFIP<=56)

p=2L
print(2*geohashTools::gh_delta(p))

NCES_data_gh<-
  NCES_data|>
  mutate(gh=geohashTools::gh_encode(LAT,LON,precision=p))

working_list<-
  NCES_data|>
  mutate(gh=geohashTools::gh_encode(LAT,LON,precision=p))|>
  distinct(gh)|>
  geohashTools::gh_to_sf()|>
  rowwise()|>
  mutate(bb=list(sf::st_bbox(geometry)))|>
  sf::st_drop_geometry()|>
  ungroup()

td=tempdir()

gh_list=working_list|>pluck("gh")
for(w_gh in gh_list) {
  print(w_gh)
  
  w_bb0=working_list|>filter(gh==w_gh)|>pluck("bb",1)
  
  w_bb=w_bb0
  w_bb[c(1,3)]=(w_bb0[c(1,3)]-mean(w_bb0[c(1,3)]))*1.05+mean(w_bb0[c(1,3)])
  w_bb[1+c(1,3)]=(w_bb0[1+c(1,3)]-mean(w_bb0[1+c(1,3)]))*1.05+mean(w_bb0[1+c(1,3)])
  

  w_bb[1]=min(max(w_bb[1],-180),180)
  w_bb[3]=min(max(w_bb[3],-180),180)
  
  w_bb[2]=min(max(w_bb[2],-90),90)
  w_bb[4]=min(max(w_bb[4],-90),90)
  
  NCES_working_gh<-
    NCES_data_gh|>
    filter(gh==w_gh)|>
    sf::st_as_sf(coords=c("LON","LAT"),crs=sf::st_crs("WGS84"))
  
  
  school_land<-
    open_curtain(type="land_use",theme="base",bbox=w_bb)
  
  school_land_nces<-
    school_land|>
    collect_sf()|>
    sf::st_join(
      NCES_working_gh ,
      left=FALSE
    )
  
  
  school_land_nces<-
    school_land_nces|>
    filter(1L*(subtype=="education")==max(1L*(subtype=="education")),.by=c(NCESSCH,LEAID,PPIN))
  
  school_land_nces|>
    mutate(
      name.primary=names[["primary"]]
    )|>
    select(-where(is.list))|>
    mutate(
      geometry=sf::st_as_text(geometry)
    )|>
    write_csv(
      glue::glue("{td}/school_land_gh{w_gh}.csv.gz")
    )
}


school_land_nces<-
  read_csv(
    list.files(td, pattern="school_land_gh",full.names = T)
  )
  

school_land_nces|>
  write_csv("Data/overture_land.csv.gz")


school_land_nces<-
  school_land_nces|>
  filter(subtype=="education")|>
  distinct(NCESSCH,LEAID,PPIN)

NCES_data_gh1<-
  NCES_data_gh|>
  anti_join(school_land_nces)
  
working_list1<-
  working_list|>
  inner_join(distinct(NCES_data_gh1,gh))

rm(school_land_nces,school_land)

gh_list=working_list1|>pluck("gh")
for(w_gh in gh_list) {
  print(w_gh)
  
  w_bb0=working_list|>filter(gh==w_gh)|>pluck("bb",1)
  
  w_bb=w_bb0
  w_bb[c(1,3)]=(w_bb0[c(1,3)]-mean(w_bb0[c(1,3)]))*1.05+mean(w_bb0[c(1,3)])
  w_bb[1+c(1,3)]=(w_bb0[1+c(1,3)]-mean(w_bb0[1+c(1,3)]))*1.05+mean(w_bb0[1+c(1,3)])
  
  
  w_bb[1]=min(max(w_bb[1],-180),180)
  w_bb[3]=min(max(w_bb[3],-180),180)
  
  w_bb[2]=min(max(w_bb[2],-90),90)
  w_bb[4]=min(max(w_bb[4],-90),90)
  
  NCES_working_gh<-
    NCES_data_gh1|>
    filter(gh==w_gh)|>
    sf::st_as_sf(coords=c("LON","LAT"),crs=sf::st_crs("WGS84"))
  
  
  school_building<-
    open_curtain("building",bbox=w_bb)













  NCES_working_gh1<-
    NCES_working_gh|>
    anti_join(
      school_land_nces|>select(NCESSCH,LEAID,PPIN)|>sf::st_drop_geometry()
    )
  
  buildings<-
    open_curtain("building", bbox=w_bb)|>
    mutate(
      name.primary=names[["primary"]]
    )|>
    filter(
      subtype=="education" | 
        str_detect(str_to_lower(name.primary),"school")
    )|>
    select(id,geometry,subtype,class,name.primary,names.raw=names)|>
    collect_sf()
  
  buildings_nces<-
    buildings|>
    sf::st_join(
      NCES_working_gh1, left=FALSE,
      join = sf::st_is_within_distance,
      dist=units::set_units(25,"m")
    )
  
  # NCES_working_gh1<-
    NCES_working_gh1|>
    anti_join(
      buildings_nces|>select(NCESSCH,LEAID,PPIN)|>sf::st_drop_geometry()
    )
  
  
  
  sf::st_as_text()
  
}

w_gh=working_list|>slice_sample(n=1)|>pluck("gh")

w_bb=working_list|>filter(gh==w_gh)|>pluck("bb",1)




bb1=bb

NCES_working_gh_sf<-
  NCES_working_gh|>
  sf::st_as_sf(coords=c("LON","LAT"),crs=sf::st_crs("WGS84"))

buildings<-open_curtain("building", bbox=w_bb)
places<-open_curtain(type = "*", theme = "places", bbox=w_bb)

# now look at the different kinds of schools data in Overture--remember the Kernodle problem!

school_buildings<-
  buildings|>
  mutate(
    name.primary=names[["primary"]]
  )|>
  filter(
    subtype=="education" | 
    str_detect(str_to_lower(name.primary),"school")
  )|>
  select(id,geometry,subtype,class,name.primary,names.raw=names)|>
  collect_sf()

school_pois<-
  places|>
  mutate(
    name.primary=names[["primary"]],
    category.primary=categories[["primary"]],
    category.alternate=categories[["alternate"]],
    
  )|>
  filter(
    str_detect(category.primary,"school") | 
      str_detect(str_to_lower(name.primary),"school")
    )|>
  select(id,geometry,category.primary,category.alternate,name.primary)|>
  collect_sf()

# now let's try and match the NCES data


# some land use areas match multiple schools; let's try to find buildings
NCES_working_gh_sf|>
  sf::st_join(school_land,left=FALSE)|>
  sf::st_drop_geometry()|>
  filter(n()>1,.by=c(NCESSCH))

NCES_working_gh_sf|>
  sf::st_join(school_buildings,left=FALSE)|>
  sf::st_drop_geometry()
  filter(n()>1,.by=c(NCESSCH))




bb=c(xmin=-80,ymin=36,xmax=-79.5,ymax=36.5)


gso_buildings=open_curtain("building", bbox=bb)|>
  filter(subtype=="education")|>
  collect_sf()

gso_places=open_curtain(type = "*", theme = "places", bbox=bb)|>
  filter(str_detect(categories$primary,"school"))|>
  collect_sf()


gso_places<-
  gso_places|>
  mutate(name.primary=names[["primary"]])

ggplot()+ggspatial::annotation_map_tile(data = gso_buildings)+geom_sf(data=gso_buildings)+geom_sf(data=gso_places)+geom_sf_text(data=gso_places,mapping=aes(label=name.primary))+coord_sf(xlim=c(-79.9,-79.87),ylim=c(36.12,36.14))
36.1316336,-79.8848869


bb2=c(xmin=-79.885,ymin=36.125,xmax=-79.870,ymax=36.14)
q0=open_curtain("building", bbox=bb2)|>collect_sf()
q0_p=open_curtain(type = "*", theme = "places", bbox=bb2)|>collect_sf()


q0|>sf::st_join(q0_p,left=FALSE)|>ggplot()+ggspatial::annotation_map_tile()+geom_sf()
