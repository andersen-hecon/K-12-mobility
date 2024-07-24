library(tidyverse)
library(arrow)


# get the school status datasets

dates=tibble(date=
               seq(from=floor_date(as_date("2020-07-01"),"week",week_start = 1),
                   to  =floor_date(as_date("2021-07-01")-1,"week",week_start = 1),
                   by=7)
             )|>
  mutate(year=year(date))

# Load R2L data
R2L_data <- haven::read_dta("Input-Data/R2L_extract45d.dta")
R2L_data$leaid <- as.integer(R2L_data$leaid)

R2L_week_map <- readxl::read_excel("Input-Data/r2l_status_wk_map.xlsx")
R2L_week_map <- rename(R2L_week_map,statuswk=status_wk)
R2L_week_map$date <- as.Date(R2L_week_map$date)

R2L_data <-
  R2L_data|>
  inner_join(select(R2L_week_map,statuswk,date))|>
  mutate(year=year(date))|>
  select(leaid,year,date,R2L_status_now=currstat)

rm(R2L_week_map)

R2L_data<-
  R2L_data|>
  mutate(
    date=floor_date(date,"week",week_start=1)
  )|>
  mutate(
    R2L_start_date=min(date),
    .by=c(leaid,year)
  )|>
  complete(
    nesting(leaid,year,R2L_start_date),
    date
  )|>
  filter(year==year(date))|>
  group_by(leaid,year)|>
  arrange(date, .by_group = TRUE)|>
  fill(R2L_status_now,.direction = "downup")|>
  mutate(
    R2L_status_start=if_else(date==R2L_start_date,R2L_status_now,NA_character_)
  )|>
  fill(R2L_status_start,.direction = "downup")|>
  ungroup()|>
  select(leaid,year,date,R2L_start_date,R2L_status_start,R2L_status_now)


# Load MCH data
MCH_2020 <- readxl::read_excel("Input-Data/MCH_Fall_2020.xlsx")|>mutate(year=2020)
MCH_2021 <- readxl::read_excel("Input-Data/MCH_Spring_2021.xlsx")|>mutate(year=2021)
MCH_base=bind_rows(MCH_2020,MCH_2021)|>
  mutate(leaid=as.numeric(DistrictNCES))|>
  filter(Control=="Public",!is.na(leaid))

MCH<-
  MCH_base|>
  # clean the teaching methods
  mutate(
    across(
      c(TeachingMethod,TeachingMethodJan2021),
      ~case_match(.,
                  c("Full In-Person","On Premises")~"I",
                  c("Hybrid","Hybrid/Partial")~"H",
                  c("Online Only","Remote learning only")~"R",
                  c("Pending","Unknown","Other")~NA_character_,
                  .default = .
                  )
    )
  )|>
  mutate(
    MCH_status_start=case_when(
      year==2020~TeachingMethod,
      year==2021 & !is.na(TeachingMethodJan2021)~TeachingMethodJan2021,
      TRUE~TeachingMethod
    ),
    MCH_start_date=mdy(OpenDate)
  )|>
  select(leaid,year,MCH_status_start,MCH_start_date)|>
  distinct()|>
  filter(!is.na(MCH_status_start))|>
  filter(
    n()==1
    ,.by=c(leaid,year))

MCH<-
  MCH|>
  inner_join(distinct(R2L_data,date,year), by=join_by(year), relationship="many-to-many")

rm(MCH_2020,MCH_2021,MCH_base)

# Load Burbio data and clean it
Burbio_raw<- read_csv("Input-Data/Burbio_SY_2020_2021.csv", guess_max=100000)

Burbio<-
  Burbio_raw|>
  filter(`District website`!="http://www.cttech.org/index.html")|>
  filter(!is.na(`District NCES`), `District NCES`!="#N/A")|>
  mutate(date=floor_date(lubridate::mdy(`Record Date`),"week",week_start =1), # make sure that we are on Mondays
         Burbio_start_date=floor_date(lubridate::mdy(`Start Date`),"week",week_start =1), 
         leaid=as.numeric(`District NCES`),
         Burbio_status_now=case_when(`SD %V`>pmax(`SD %U`, `SD %T`, `SD %H`)~"R",
                                     `SD %H`>pmax(`SD %U`, `SD %T`, `SD %V`)~"H",
                                     `SD %T`>pmax(`SD %U`, `SD %V`, `SD %H`)~"I"),
         year=year(date))%>%
  filter(`County Population`==max(`County Population`),.by=c(leaid,date))|>
  select(date, Burbio_status_now,Burbio_start_date,leaid,year)|>
  group_by(leaid)|>
  arrange(date,.by_group = T)|>
  fill(Burbio_start_date,.direction = "down")|>
  ungroup()|>
  distinct()|>
  # there are some issues with multiple start dates, so whne there are multiple choices, choose the plurality
  mutate(
    check1=n(),
    .by=c(leaid,Burbio_start_date,year)
  )|>
  filter(check1==max(check1),.by=c(leaid,year,date))|>
  select(-check1)|>
  mutate(
    Burbio_start_date=
      min(replace_na(Burbio_start_date,as_date("2100-01-01")),na.rm = T),
    Burbio_start_date=if_else(Burbio_start_date==as_date("2100-01-01"),NA_Date_,Burbio_start_date),
    .by=c(leaid,year))|>
  filter(!is.na(leaid),!is.na(Burbio_start_date))|>
  distinct()|>
  ungroup()

Burbio<-
  Burbio|>
  bind_rows(distinct(R2L_data,year,date))|>
  complete(
    nesting(leaid,year,Burbio_start_date),
    date
  )|>
  filter(!is.na(leaid))|>
  filter(year==year(date))|>
  group_by(leaid,year)|>
  arrange(date, .by_group = TRUE)|>
  fill(Burbio_status_now,.direction = "downup")|>
  ungroup()|>
  select(leaid,year,date,Burbio_start_date,Burbio_status_now)

Burbio<-
  Burbio|>
  filter(date==Burbio_start_date)|>
  select(leaid,year,Burbio_status_start=Burbio_status_now)|>
  inner_join(Burbio)|>
  select(leaid,year,date,Burbio_start_date,Burbio_status_now,Burbio_status_start)
  
  
rm(Burbio_raw)
  
schools<-
  R2L_data|>
  full_join(MCH)|>
  full_join(Burbio)

rm(R2L_data,Burbio,MCH)

schools<-
  schools|>
  distinct(leaid)|>
  cross_join(dates)|>
  left_join(schools)|>
  group_by(leaid,year)|>
  arrange(date, .by_group = TRUE)|>
  fill(R2L_start_date:Burbio_status_now,.direction = "downup")|>
  ungroup()|>
  distinct()
  
schools|>
  select(leaid,year,date,contains("start_date"),contains("status_start"),contains("status_now"))|>
  write_csv("Data/school_policies.csv")



counties<-
  tigris::counties(cb=T,year=2020)|>
  select(county_fips=GEOID)|>
  sf::st_transform(
    crs=sf::st_crs("WGS84")
  )

schools_loc<-
  read_csv("Data/bigquery_gh_list.csv.gz")

amenity_assignments<-
  schools_loc|>
  filter(
    !str_detect(id,"^NCESSCH") | str_detect(id,"NCESSCH:.*:0")
    )
  
amenity_assignments<-
  amenity_assignments|>
  bind_cols(
    as_tibble(geohashTools::gh_decode(amenity_assignments$gh))
  )|>
  sf::st_as_sf(coords=c("longitude","latitude"),crs=sf::st_crs("WGS84"))

school_coords<-
  schools_loc|>
  filter(str_detect(id,"NCESSCH[0-9].*:0$"))|>
  separate_wider_regex(id, patterns = c("NCESSCH","nces_school_id"="[0-9]+",":LEAID",leaid="[0-9]+",":PPIN:","ring"="[0-9]+$"),cols_remove = FALSE)|>
  mutate(
    nces_school_id=str_remove(nces_school_id,paste0("^",leaid)),
    leaid=as.numeric(leaid),
    type="public"
  )

school_coords<-
  school_coords|>
  bind_cols(
    as_tibble(geohashTools::gh_decode(school_coords$gh))
  )|>
  sf::st_as_sf(coords=c("longitude","latitude"),crs=sf::st_crs("WGS84"))

amenity_assignments<-
  amenity_assignments|>
  sf::st_join(
    select(school_coords,leaid),
    join=sf::st_nearest_feature
  )

amenity_assignments<-
  amenity_assignments|>
  mutate(
    type=if_else(str_detect(id,"NCESSCH"),"private",str_extract(id,"(?<=:).*"))
  )

id_mapping<-
  bind_rows(
    amenity_assignments|>
      select(id,type,leaid,gh),
    school_coords|>
      select(id,type,leaid, gh)
  )|>
  sf::st_join(counties,left=FALSE)

id_mapping<-
  id_mapping|>
  sf::st_drop_geometry()|>
  count(id,type,leaid,county_fips)|>
  filter(n==max(n),.by=id)|>
  select(-n)

duck_con=DBI::dbConnect(duckdb::duckdb(dbdir=tempfile(),config = list("memory_limit"="96GB")))


ds<-
  open_dataset("./raw-data")|>
  to_duckdb(con=duck_con,table_name = "base")


denoms<-
  ds|>
  distinct(week_date,home_county_fips,denom)|>
  collect()


ref_pop<-
  tidycensus::get_estimates("county",year=2020,product = "population",variables = "POPESTIMATE")|>
  filter(variable=="POPESTIMATE")|>
  select(home_county_fips=GEOID,county_pop=value)

denoms<-
  denoms|>
  inner_join(ref_pop)|>
  mutate(s=county_pop/denom)

denoms2<-
  denoms|>
  select(-denom,-county_pop)|>
  as_arrow_table()|>
  to_duckdb(con=duck_con,table_name = "denoms2")

duck_id_mapping<-
  id_mapping|>
  as_arrow_table()|>
  to_duckdb(con=duck_con,table_name="ids")

mobility<-
  ds|>
  filter(week_date>="2019-07-01",
         week_date<"2021-07-01")|>
  inner_join(
    duck_id_mapping
  )|>
  compute()|>
  inner_join(denoms2)|>
  summarize(
    across(c(days_visited,visitors,duration),~sum(.*s),.names="scaled_{col}"),
    across(c(days_visited,visitors,duration),~sum(.)),
    .by=c(week_date,id,type,leaid,county_fips)
  )|>
  compute(name = "mobility")


DBI::dbExecute(duck_con,
               "COPY mobility TO 'Data/mobility-2019-2021.csv.gz' (FORMAT CSV, overwrite_or_ignore TRUE, compression gzip)"
               )



DBI::dbDisconnect(duck_con)

rm(list=ls())
gc()


schools<-read_csv("Data/school_policies.csv")

mobility<-read_csv("Data/mobility-2019-2021.csv.gz")


analytic<-
  mobility|>
  summarize(
    across(c(scaled_days_visited,scaled_visitors,
             days_visited,visitors),
           ~sum(.,na.rm=T)),
    .by=c(week_date,type,leaid,county_fips)
  )|>
  complete(
    nesting(type,leaid,county_fips),
    week_date,
    fill=list(
      scaled_days_visited=0,
      scaled_visitors=0,
      days_visited=0,
      visitors=0
    )
  )|>
  pivot_wider(
    names_from = type,
    values_from = c(scaled_days_visited,scaled_visitors,
                    days_visited,visitors),
    names_glue = "{type}_{.value}",
    values_fill = 0
  )

analytic<-
  analytic|>
  inner_join(
    schools,
    by=join_by(leaid,week_date==date),
  )

analytic<-
  analytic|>
  # inner_join(select(k12_enrollment,leaid,enrollment))|>
  mutate(across(c(R2L_start_date,Burbio_start_date),~if_else(is.na(MCH_start_date),.,MCH_start_date)),
         R2L_open=case_when(is.na(R2L_start_date) | year(week_date)!=year(R2L_start_date)~NA_real_,
                            week_date>=R2L_start_date~1,
                            week_date<R2L_start_date~0),
         MCH_open=case_when(is.na(MCH_start_date) | year(week_date)!=year(MCH_start_date)~NA_real_,
                            week_date>=MCH_start_date~1,
                            week_date<MCH_start_date~0),
         Burbio_open=case_when(is.na(Burbio_start_date) | year(week_date)!=year(Burbio_start_date)~NA_real_,
                               week_date>=Burbio_start_date~1,
                               week_date<Burbio_start_date~0),
  )

LAU_data<-
  read_tsv("Input-Data/la.data.0.CurrentU20-24.txt")|>
  inner_join(read_tsv("Input-Data/la.series.txt"), by="series_id")|>
  inner_join(read_tsv("Input-Data/la.area.txt"), by=c("area_code", "area_type_code"))

LAU_data<-
  LAU_data|>
  filter(area_type_code=="F")|>
  select(area_code,year,month=period,value,measure_code)|>
  mutate(measure=case_when(measure_code=="03"~"unemployment_rate",
                           measure_code=="04"~"unemployment",
                           measure_code=="05"~"employment",
                           measure_code=="06"~"labor_force",
                           measure_code=="07"~"employment-population ratio",
                           measure_code=="08"~"labor force participation rate",
                           measure_code=="09"~"civilian noninstitutional population",
                           TRUE~measure_code),
         month=as.numeric(str_extract(month,"[0-9]+")),
         county_fips=as.numeric(str_sub(area_code,3,7)),
         value=as.numeric(value))|>
  select(-area_code,-measure_code)|>
  filter(month %in% 1:12, county_fips<57000)|>
  pivot_wider(names_from=measure,values_from = value)

analytic<-
  analytic|>
  mutate(month=month(week_date),
         year=year(week_date),
         county_fips=as.numeric(county_fips))|>
  inner_join(LAU_data)

rm(LAU_data)


analytic|>
  write_csv("Data/mobility-analytic.csv.gz")
