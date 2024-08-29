library(overtureR)
library(tidyverse)
library(dplyr)
library(sf)

# collect ANY and ALL places of interest
duck_con=DBI::dbConnect(duckdb::duckdb(dbdir=tempfile(),config = list("memory_limit"="16GB")))

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

# now we have our target tags, get places that correspond

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

iwalk(
  usa_grid$bb,
  \(bb,n) {
    
    places<-open_curtain(type = "*", theme = "places",spatial_filter = bb,as_sf = TRUE, conn=duck_con)|>
      mutate(
        name.primary=names$primary,
        name.common=names$common,
        category.primary=categories$primary,
        category.alternate=categories$alternate
      )|>
      select(id,geometry,name.primary,name.common,category.primary, category.alternate,confidence,addresses,theme,type)|>
      unnest_wider(col = category.alternate,names_sep=".")|>
      filter(
        if_any(
          starts_with("category"),
          ~. %in% target_tags$cat
        )
      )|>
      sf::st_set_geometry("geometry")
    
    if(nrow(places)>0) {
      places<-
        places|>
        unnest(cols=c(name.common,addresses),keep_empty=TRUE)|>
        mutate(
          coords=sf::st_coordinates(geometry)|>as_tibble()
        )|>
        unnest(coords,names_sep = ".")|>
        select(-theme,-type)|>
        sf::st_drop_geometry()
      
      places|>
        select(
          id, name.primary, name.common, category.primary, category.alternate.1, category.alternate.2, confidence, 
          freeform, locality, postcode, region, country, coords.X, coords.Y  
        )|>
        write_csv(
          "C:/Users/msander4/Box/RESEARCH - Martin Andersen/Data/Overture/health_ed.csv",
          append=n!="1"
        )
    }
    
  }
)



ov_health_ed<-read_csv("C:/Users/msander4/Box/RESEARCH - Martin Andersen/Data/Overture/health_ed.csv")

ov_ll<-
  ov_health_ed|>
  mutate(
    bb.X=d*floor(coords.X/d),
    bb.Y=d*floor(coords.Y/d),
  )|>
  distinct(bb.X,bb.Y)

rm(ov_health_ed)

usa_grid2<-
  usa_grid|>
  mutate(
    bb.X=map_dbl(bb,~pluck(.,"xmin")),
    bb.Y=map_dbl(bb,~pluck(.,"ymin")),
  )|>
  anti_join(ov_ll)



iwalk(
  usa_grid2$bb,
  \(bb,n) {
    print(n)
    
    places<-open_curtain(type = "*", theme = "places",spatial_filter = bb,as_sf = TRUE, conn=duck_con)|>
      mutate(
        name.primary=names$primary,
        name.common=names$common,
        category.primary=categories$primary,
        category.alternate=categories$alternate
      )|>
      select(id,geometry,name.primary,name.common,category.primary, category.alternate,confidence,addresses,theme,type)|>
      unnest_wider(col = category.alternate,names_sep=".")|>
      filter(
        if_any(
          starts_with("category"),
          ~. %in% target_tags$cat
        )
      )|>
      sf::st_set_geometry("geometry")
    
    if(nrow(places)>0) {
      places<-
        places|>
        unnest(cols=c(name.common,addresses),keep_empty=TRUE)|>
        mutate(
          coords=sf::st_coordinates(geometry)|>as_tibble()
        )|>
        unnest(coords,names_sep = ".")|>
        select(-theme,-type)|>
        sf::st_drop_geometry()
      
      places|>
        select(
          id, name.primary, name.common, category.primary, category.alternate.1, category.alternate.2, confidence, 
          freeform, locality, postcode, region, country, coords.X, coords.Y  
        )|>
        write_csv(
          "C:/Users/msander4/Box/RESEARCH - Martin Andersen/Data/Overture/health_ed2.csv",
          append=n!="1"
        )
    }
    
  }
)



