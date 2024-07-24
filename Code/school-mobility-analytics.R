knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,error = TRUE, cache.lazy = FALSE)
library(tidyverse)
library(lubridate)
library(fixest)
library(modelsummary)
library(did)
library(DIDmultiplegt)
library(patchwork)
library(collapse)
library(doParallel)


demo<-
  map_dfr(
    list.files("Input-Data/nhgis0047_csv", pattern = "csv", full.names = TRUE),
    ~read_csv(.)
    )

demo<-
  demo|>
  select(-matches("M[0-9]{3}$"))


demo<-
  demo|>
  mutate(
    pct_00_04=(ALT0E003+ALT0E027)/ALT0E001,
    pct_05_09=(ALT0E004+ALT0E028)/ALT0E001,
    pct_10_14=(ALT0E005+ALT0E029)/ALT0E001,
    pct_15_17=(ALT0E006+ALT0E030)/ALT0E001,
    
    pct_hispanic=ALUKE012/ALUKE001,
    pct_nh_white=ALUKE003/ALUKE001,
    pct_nh_black=ALUKE004/ALUKE001,
    pct_nh_other=(ALUKE002-ALUKE003-ALUKE004)/ALUKE001,
    
    pct_fpl_000_049=ALWVE002/ALWVE001,
    pct_fpl_050_099=ALWVE003/ALWVE001,
    pct_fpl_100_124=ALWVE004/ALWVE001,
    pct_fpl_125_149=ALWVE005/ALWVE001,
    pct_fpl_150_184=ALWVE006/ALWVE001,
    pct_fpl_185_199=ALWVE007/ALWVE001,
    pct_fpl_200_999=ALWVE008/ALWVE001,
    
    
    percapita_income=ALX5E001,

    )|>
  mutate(
    pct_fpl_000_099=pct_fpl_000_049+pct_fpl_050_099,
    pct_fpl_100_199=pct_fpl_100_124+pct_fpl_125_149+pct_fpl_150_184+pct_fpl_185_199,
    pct_05_17=pct_05_09+pct_10_14+pct_15_17,
        
    pct_internet=1-AL1YE013/AL1YE001,
    pct_computer=AL1ZE002/AL1ZE001
  )

demo<-
  demo|>
  mutate(county_fips=as.numeric(paste0(STATEA,COUNTYA)),
         leaid=case_when(
           !is.na(SDELMA)~paste0(STATEA,SDELMA),
           !is.na(SDSECA)~paste0(STATEA,SDSECA),
           !is.na(SDUNIA)~paste0(STATEA,SDUNIA),
         ),
         leaid=as.numeric(leaid)
         )|>
  select(county_fips,leaid,pct_00_04:pct_computer)

# k12_enrollment<-
#   read_csv("Input-Data/k12_enrollment.csv")
# 
# k12_enrollment<-
#   k12_enrollment|>
#   mutate(across(c(leaid,enrollment),~as.numeric(.)))|>
#   drop_na()

analytic<-
  read_csv("Data/mobility-analytic.csv.gz")

# now bring in the demographic information, start with LEA and then get the county if necessary
demo_map<-
  analytic|>
  distinct(leaid,county_fips)

demo_merged<-
  inner_join(demo_map,select(demo,-county_fips),by="leaid")

demo_merged<-
  demo_map|>
  anti_join(demo_merged)|>
  inner_join(select(demo,-leaid),by="county_fips")|>
  bind_rows(demo_merged)
# 
# analytic<-
#   analytic|>
#   left_join(demo_merged)
#     
# estimate DID models with pairwise comparisons, define a T group and a C-group, restrict to observations that are C or T, but not not C and T

types=c("R","H","I")

pairwise_comps<-
  tibble(T_type=types)|>
  cross_join(tibble(C_type=types))

# now set up a dataset for DID estimation
DID_base_data_wide<-
  analytic|>
  distinct(county_fips,leaid,year)|>
  mutate(group_id=row_number())|>
  inner_join(analytic)|>
  mutate(
    across(
      c(ends_with("visited") | ends_with("visitors"),unemployment,employment,labor_force),
      ~replace_na(.,0)
      ),
    across(
      c(ends_with("visited") | ends_with("visitors"),unemployment,employment,labor_force),
      ~asinh(replace_na(.,0)),.names="log_{col}"
      )
    )|>
  select(group_id,date=week_date,leaid,county_fips,year,
         log_public_scaled_days_visited, log_restaurant_scaled_days_visited, log_cafe_scaled_days_visited, log_bar_scaled_days_visited, log_library_scaled_days_visited, log_private_scaled_days_visited,
         log_public_days_visited, log_restaurant_days_visited, log_cafe_days_visited, log_bar_days_visited, log_library_days_visited, log_private_days_visited, log_public_scaled_visitors, 
         log_restaurant_scaled_visitors, log_cafe_scaled_visitors, log_bar_scaled_visitors, log_library_scaled_visitors, log_private_scaled_visitors, log_public_visitors, log_restaurant_visitors, 
         log_cafe_visitors, log_bar_visitors, log_library_visitors, log_private_visitors, log_unemployment, log_employment, log_labor_force,
         public_days_visited,private_days_visited,bar_days_visited,cafe_days_visited,library_days_visited,restaurant_days_visited,unemployment,employment,labor_force,
         starts_with("R2L"),
         starts_with("MCH"),
         starts_with("Burbio"),
         )|>
  mutate(across(contains("date"),~floor(as.numeric(.)/7),.names="{col}"))|>
  pivot_longer(contains("R2L") | contains("MCH") | contains("Burbio"),names_to = c("vendor",".value"),names_pattern = "([A-Z|a-z|2]+)_(.*)")|>
  distinct()|>
  filter(!is.na(start_date),!is.na(status_start))|>
  mutate(relative_time=date-start_date)
  
# 
# DID_base_data_long<-
#   DID_base_data_wide|>
#   select(-public_days_visited,-private_days_visited,-bar_days_visited,-cafe_days_visited,-library_days_visited,-restaurant_days_visited,-unemployment,-employment,-labor_force)|>
#   pivot_longer(
#     c(log_public_scaled_days_visited, log_restaurant_scaled_days_visited, log_cafe_scaled_days_visited, log_bar_scaled_days_visited, log_library_scaled_days_visited, log_private_scaled_days_visited,
#       log_public_days_visited, log_restaurant_days_visited, log_cafe_days_visited, log_bar_days_visited, log_library_days_visited, log_private_days_visited, log_public_scaled_visitors, 
#       log_restaurant_scaled_visitors, log_cafe_scaled_visitors, log_bar_scaled_visitors, log_library_scaled_visitors, log_private_scaled_visitors, log_public_visitors, log_restaurant_visitors, 
#       log_cafe_visitors, log_bar_visitors, log_library_visitors, log_private_visitors,
#       log_unemployment,log_employment,log_labor_force),names_to="depvar",values_to="Y")|>
#   filter(!is.na(Y),is.finite(Y),
#          !is.na(start_date))|>
#   mutate(relative_time=date-start_date)
# 
# # remove groups that would be removed later on
# DID_base_data_long<-
#   DID_base_data_long|>
#   filter(mean(Y)>0,.by=c(leaid,county_fips,year,depvar))


DID_base_data_wide<-
  DID_base_data_wide|>
  inner_join(demo_merged)

# weights<-
#   DID_base_data_long|>
#   distinct(leaid,county_fips,year,vendor,enrollment,depvar)|>
#   group_by(county_fips,vendor,depvar)|>
#   mutate(weight=case_when(depvar %in% c("log_raw_visit_counts","log_raw_visitor_counts")~1,
#                           TRUE~enrollment/sum(enrollment)))|>
#   ungroup()
# 
# DID_base_data_long<-
#   DID_base_data_long|>
#   inner_join(weights)


MCH_R2L_horse_race_sample<-
  analytic|>
  filter(!is.na(R2L_status_start),!is.na(MCH_status_start),!is.na(R2L_open),!is.na(MCH_open))|>
  distinct(leaid,county_fips,year)

Burbio_MCH_R2L_horse_race_sample<-
  analytic|>
  filter(!is.na(R2L_status_start),!is.na(R2L_open),
         !is.na(MCH_status_start),!is.na(MCH_open),
         !is.na(Burbio_status_start),!is.na(Burbio_open),
         )|>
  distinct(leaid,county_fips,year)

base_DID=DID_base_data_wide|>distinct(leaid,county_fips,year,status_start,vendor,start_date,group_id)

DID_datasets<-
  bind_rows(
    base_DID|>mutate(districts="All"),
    base_DID|>mutate(districts="MCH_R2L")|>inner_join(MCH_R2L_horse_race_sample),
    base_DID|>mutate(districts="Burbio_MCH_R2L")|>inner_join(Burbio_MCH_R2L_horse_race_sample)
  )
# 
# 
# |>
#   select(-status_now)|>
#   drop_na()|>
#   group_by(vendor,depvar,districts)|>
#   nest()

# DID_datasets<-
#   DID_datasets|>
#   filter(case_when(districts=="All"~TRUE,
#                    depvar %in% c("log_raw_visit_counts","log_raw_visitor_counts","weekly_cases_per100k")~TRUE,
#                    TRUE~FALSE
#                    ))

DID_datasets<-
  pairwise_comps|>
  cross_join(DID_datasets)|>
  filter(status_start==T_type | status_start==C_type,year==2020)|>
  mutate(
    treat=if_else(status_start==T_type,start_date,0),
    treat_g=1L*(status_start==T_type)
  )|>
  mutate(
    flag=n()<=10,
    .by=c(T_type,C_type,vendor,districts,treat)
  )

dates=distinct(DID_base_data_wide,date)

cs_att_gt=purrr::partial(
  att_gt,
  tname="date",
  idname="group_id",
  gname="treat",
  control_group = "notyettreated",
  base_period = "universal",
  anticipation = 1,
  clustervars = "group_id",
  )

xf=as.formula(~pct_00_04+pct_05_09+pct_10_14+pct_15_17+pct_hispanic+pct_nh_black+pct_nh_other
              +pct_fpl_000_049+pct_fpl_050_099+pct_fpl_100_124+pct_fpl_125_149+pct_fpl_150_184
              +pct_fpl_185_199+pct_internet+pct_computer)

depvars=list(
  "log_public_scaled_days_visited", "log_public_scaled_visitors",
  "log_restaurant_scaled_days_visited", "log_restaurant_scaled_visitors",
  "log_cafe_scaled_days_visited",  "log_cafe_scaled_visitors",
  "log_bar_scaled_days_visited", "log_bar_scaled_visitors",
  "log_library_scaled_days_visited", "log_library_scaled_visitors",
  "log_private_scaled_days_visited", "log_private_scaled_visitors",
  
  "log_public_days_visited", "log_public_visitors",
  "log_restaurant_days_visited", "log_restaurant_visitors",
  "log_cafe_days_visited", "log_cafe_visitors",
  "log_bar_days_visited", "log_bar_visitors",
  "log_library_days_visited", "log_library_visitors",
  "log_private_days_visited", "log_private_visitors",
  
  "log_unemployment","log_employment","log_labor_force"
)

est_models<-function(sample,depvars, cs_depvars=depvars) {
  x0<-
    sample|>
    cross_join(dates)|>
    inner_join(DID_base_data_wide)
  
  map(depvars,
      \(depvar) {
        message("Doing variable ", depvar,"\r")
        twfe=
          bind_rows(
            feols(.[depvar]~treat_g*open|group_id+date,data=x0, lean = T, cluster=~county_fips)|>broom::tidy()|>mutate(type="twfe_DD"),
            feols(.[depvar]~i(relative_time,treat_g,ref=-2)|group_id+date,data=x0, lean = T, cluster=~county_fips)|>broom::tidy()|>mutate(type="twfe_event")
          )
        
        cs_uncond=cs_att_gt(data=filter(x0,!flag),yname=depvar)
        cs_cond  =cs_att_gt(data=filter(x0,!flag),yname=depvar,xformla=xf)
        
        agg_cs_uncond=aggte(cs_uncond,type="dynamic",na.rm=T)
        agg_cs_cond  =aggte(cs_cond  ,type="dynamic",na.rm=T)
        
        cs_results=bind_rows(
          aggte(cs_uncond,type="simple" ,na.rm=T)|>broom::tidy()|>mutate(type="cs_uncond_simple" ),
          aggte(cs_uncond,type="dynamic",na.rm=T)|>broom::tidy()|>mutate(type="cs_uncond_dynamic"),
          
          aggte(cs_cond  ,type="simple" ,na.rm=T)|>broom::tidy()|>mutate(type="cs_cond_simple"   ),
          aggte(cs_cond  ,type="dynamic",na.rm=T)|>broom::tidy()|>mutate(type="cs_cond_dynamic"  )
        )
        
        bind_rows(
          twfe,
          cs_results
        )
      })|>
    list_rbind()
  
}


model_results<-
  DID_datasets|>
  filter(T_type!=C_type)|>
  group_by(T_type,C_type,vendor,districts)|>
  group_modify(
    \(x,y) {
      message("Running group ",y,"\r", appendLF = F)
      z=x|>cross_join(y)
      
      est_models(z,depvars)
    }
  )

model_results|>
  write_csv("Output/model_results.csv")