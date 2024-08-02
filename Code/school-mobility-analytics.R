library(tidyverse)
library(lubridate)
library(fixest)
library(modelsummary)
library(did)
library(patchwork)

k12_enrollment<-read_csv("Input-Data/k12_enrollment.csv")|>
  mutate(leaid=as.numeric(leaid),
         enrollment=as.numeric(enrollment))|>
  select(leaid, enrollment)|>
  drop_na()

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


# I don't think I trust the demo_merged data, so go back to straight county-level data
demo_merged<-
  demo|>
  filter(is.na(leaid))|>
  select(-leaid)

# estimate DID models with pairwise comparisons, define a T group and a C-group, restrict to observations that are C or T, but not not C and T

types=c("R","H","I")

pairwise_comps<-
  tibble(T_type=types)|>
  cross_join(tibble(C_type=types))


# setup for an analysis of STARTING modalities for the fall of 2020
start_Fall_policies<-
  analytic|>
  filter(data_year==2020)|>
  select(leaid,county_fips,contains("R2L") | contains("MCH") | contains("Burbio"),-ends_with("now"),-ends_with("open"))|>
  distinct()|>
  pivot_longer(-c(leaid,county_fips),names_to=c("vendor",".value"),names_pattern = "([^_]*)_(.*)")|>
  drop_na()|>
  mutate(start_date=floor_date(start_date,"week",week_start=1))|>
  distinct()

Fall_analytic<-
  analytic|>
  select(
    -contains("R2L"), -contains("MCH"), -contains("Burbio")
  )|>
  select(
    leaid, county_fips, week_date,
    starts_with("bar"),starts_with("library"),starts_with("public"),starts_with("restaurant"),starts_with("private"),
    unemployment, employment, labor_force, population, weekly_cases_per100k
  )|>
  distinct()|>
  mutate(
    across(
      c(starts_with("bar"),starts_with("library"),starts_with("public"),starts_with("restaurant"),starts_with("private"),
        unemployment, employment, labor_force),
      ~asinh(.), .names="log_{col}")
  )|>
  inner_join(start_Fall_policies, relationship = "many-to-many")|>
  mutate(    
    open=1L*(week_date>=start_date)
  )|>
  mutate(
    group_id=as.numeric(fct(paste(leaid,county_fips)))
  )|>
  rename(date=week_date)

DID_base_data_wide<-
  Fall_analytic|>
  mutate(
    across(where(is.Date),~as.numeric((.-as_date("2019-01-07"))/7))
  )|>
  mutate(
    relative_time=date-start_date
  )|>
  filter(max(1L*(relative_time %in% -26:26))==1,.by=date)|>
  mutate(
    relative_time=pmin(pmax(relative_time,-26),26),
  )|>
  # left_join(demo_merged)|>
  left_join(k12_enrollment)

horse_race_samples<-
  start_Fall_policies|>
  select(leaid,county_fips,vendor)|>
  mutate(value=1, name=vendor)|>
  pivot_wider(values_fill = 0)|>
  mutate(across(c(MCH,R2L,Burbio),~max(.)),.by=c(leaid,county_fips))|>
  mutate(
    All=TRUE,
    MCH_R2L=if_all(c(MCH,R2L),~.==1),
    Burbio_MCH_R2L=if_all(c(MCH,R2L,Burbio),~.==1),
  )|>
  select(-R2L,-MCH,-Burbio)|>
  pivot_longer(-c(leaid,county_fips,vendor),names_to = "districts")|>
  filter(value)|>
  select(-value)|>
  distinct()|>
  filter(
    districts=="All"
    | (districts=="Burbio_MCH_R2L" & vendor %in% c("Burbio","MCH","R2L"))
    | (districts=="MCH_R2L" & vendor %in% c("MCH","R2L"))
  )



DID_datasets<-
  DID_base_data_wide|>
  distinct(leaid,county_fips,status_start,vendor,start_date,group_id)|>
  inner_join(horse_race_samples)

DID_datasets<-
  pairwise_comps|>
  cross_join(DID_datasets)|>
  filter(status_start==T_type | status_start==C_type,)|>
  mutate(
    treat=if_else(status_start==T_type,start_date,0),
    treat_g=1L*(status_start==T_type)
  )|>
  mutate(
    flag=n()<=10,
    .by=c(T_type,C_type,vendor,districts,treat)
  )|>
  mutate(
    #define an ORDER to T_type and C_type so that I can use ordered comparisons later on
    across(c(T_type,C_type),~factor(.,levels=c("I","H","R"),ordered=TRUE))
  )


DID_base_data_wide<-
  DID_base_data_wide|>
  mutate(
    log_enrollment=log(enrollment),
    population=mean(population,na.rm=T),
    log_population=log(mean(population,na.rm=T)),
    .by=c(county_fips)
  )


write_csv(DID_base_data_wide,"Data/DID_base_data_wide.csv")


DID_datasets<-
  DID_datasets|>
  mutate(job_id=as.numeric(factor(paste(T_type,C_type,districts,vendor))))
  

write_csv(DID_datasets,"Data/DID_datasets_jobs.csv")




# trim the dataset
# DID_base_data_wide<-
#   DID_base_data_wide|>
#   filter(max(1L*(relative_time %in% -25:25))==1,.by=date)

# dates=distinct(DID_base_data_wide,date)

cs_att_gt=purrr::partial(
  att_gt,
  tname="date",
  idname="group_id",
  gname="treat",
  control_group = "notyettreated",
  # base_period = "universal",
  anticipation = 2,
  clustervars = "county_fips",
  # weightsname="enrollment"
)


# get figure 1 data
fig1_data<-
  DID_datasets|>filter(job_id==1)|>
  # filter(districts=="All")|>
  # mutate(
  #   treat=if_else(treat %in% 82:90,treat,0) # this says that everything outside of August and September is treated as a did not open
  # )|>
  group_by(T_type,C_type,districts,vendor)|>
  group_modify(
    \(x0,y) {
      
      print(y)
      
      x<-
        x0|>
        inner_join(DID_base_data_wide)|>
        filter(max(1L*(relative_time %in% -15:15))==1,.by=date)
      
      x<-x|>mutate(w=population/sum(population),.by=c(date))|>mutate(w=w*n(),.by=date)
      
      x<-x|>
        mutate(
          log_weekly_cases_per100k=asinh(weekly_cases_per100k)
        )
      
      
      r<-
        map(
          list(
            "log_public_days_visited",
            "log_public_visitors",
            # "log_public_scaled_days_visited",
            # "log_public_scaled_visitors",
            "weekly_cases_per100k",
            "log_weekly_cases_per100k"
          ),
          \(y) {
            cs=cs_att_gt(data=filter(x,),yname=y)
            agg=aggte(cs,type="dynamic",na.rm = T, max_e=8, min_e=-12)
            
            print(agg)
            
            tibble(depvar=y,
                   CS_did=list(cs),
                   CS_agg=list(agg))
          }
        )|>
        list_rbind()
      
      # now make it easy to save r
      
      cs_to_df<-\(cs0) {
        cs=cs0[[1]]
        base_df=broom::tidy(cs)|>
          mutate(
            across(c(group),~as.character(.)),
            type="att_gt"
          )
        
        
        bind_rows(
          base_df,
          aggte(cs,type="dynamic", max_e=8,min_e=-12,na.rm = T)|>broom::tidy(),
          aggte(cs,type="group", max_e=8,min_e=-12,na.rm = T)|>broom::tidy(),
          aggte(cs,type="simple", max_e=8,min_e=-12,na.rm = T)|>broom::tidy(),
          aggte(cs,type="calendar", max_e=8,min_e=-12,na.rm = T)|>broom::tidy()
        )
      }
      
      r<-
        r|>
        group_by(depvar)|>
        reframe(
          cs_to_df(CS_did)
        )
        
        
      return(r)
        
    },
    .keep = TRUE
  )


fig1_data|>
  select(-CS_did)|>
  mutate(CS_agg=ap(CS_agg,~broom::tidy(.)))|>
  write_rds("fig1_data.rds")

fig1_data|>
  ungroup()|>
  filter(!str_detect(depvar,"scaled"))|>
  mutate(
    CS_df=map(CS_agg,~broom::tidy(.)),
    T_type=case_match(T_type,"H"~"Hybrid","I"~"In-Person","R"~"Remote")|>fct(levels=c("In-Person","Hybrid","Remote"))
  )|>
  select(-CS_did,-CS_agg)|>
  unnest(cols=c(CS_df))|>
  filter(event.time %in% -10:8)|>
  ggplot(aes(x=event.time,y=estimate, ymin=conf.low,ymax=conf.high, color=vendor))+
  geom_pointrange(position = position_dodge2(width=0.75))+
  geom_line(position = position_dodge2(width=0.75))+
  facet_grid(depvar~T_type, scales="free_y")

fig1_data_twfe<-
  DID_datasets|>
  filter(districts=="All", T_type==C_type)|>
  group_by(T_type,C_type,districts,vendor)|>
  group_modify(
    \(x,y) {
      
      print(y)
      
      x<-
        x|>
        inner_join(DID_base_data_wide)
      
      map(
        list(
          "log_public_days_visited",
          "log_public_visitors",
          "log_public_scaled_days_visited",
          "log_public_scaled_visitors",
          "weekly_cases_per100k"
        ),
        \(y) {
          fe=feols(.[y]~i(relative_time,treat_g,ref=c(-12,-2))|group_id+date,data=x)
          fe_df=broom::tidy(fe)
          did=feols(.[y]~i(open,treat_g,ref=c(0))|group_id+date,data=x)
          did_df=broom::tidy(did)
          
          tibble(depvar=y,
                 fe=list(fe),
                 fe_df=list(fe_df),
                 did=list(did),
                 did=list(did_df),
          )
        }
      )|>
        list_rbind()
    },
    .keep = TRUE
  )



fig1_data_twfe|>
  ungroup()|>
  mutate(
    T_type=case_match(T_type,"H"~"Hybrid","I"~"In-Person","R"~"Remote")|>fct(levels=c("In-Person","Hybrid","Remote"))
  )|>
  # select(-CS_did,-CS_agg)|>
  unnest(cols=c(fe_df))|>
  mutate(event.time=as.numeric(str_extract(term,"-?[0-9]+")))|>
  filter(event.time %in% -10:8)|>
  ggplot(aes(x=event.time,y=estimate, color=vendor))+
  geom_point(position = position_dodge2(width=0.75))+
  facet_grid(depvar~T_type, scales="free_y")

xf=as.formula(~pct_00_04+pct_05_09+pct_10_14+pct_15_17+pct_hispanic+pct_nh_black+pct_nh_other
              +pct_fpl_000_049+pct_fpl_050_099)
# +pct_fpl_100_124+pct_fpl_125_149+pct_fpl_150_184
# +pct_fpl_185_199+pct_internet+pct_computer)

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

cs_depvars=list(
  "log_public_scaled_days_visited", "log_public_scaled_visitors",
  "log_private_scaled_days_visited", "log_private_scaled_visitors",
  
  "log_public_days_visited", "log_public_visitors",
  "log_private_days_visited", "log_private_visitors",
  
  "log_unemployment","log_employment","log_labor_force"
)












# 400065        
# 
# mutate(
#   across(
#     c(ends_with("visited") | ends_with("visitors"),unemployment,employment,labor_force),
#     ~replace_na(.,0)
#   ),
#   across(
#     c(ends_with("visited") | ends_with("visitors"),unemployment,employment,labor_force),
#     ~asinh(.),.names="log_{col}"
#   )
# )|>
# 
# .last# now set up a dataset for DID estimation
# DID_base_data_wide<-
#   analytic|>
#   distinct(county_fips,leaid,year)|>
#   mutate(group_id=row_number())|>
#   inner_join(analytic)|>
#   mutate(
#     across(
#       c(ends_with("visited") | ends_with("visitors"),unemployment,employment,labor_force),
#       ~replace_na(.,0)
#       ),
#     across(
#       c(ends_with("visited") | ends_with("visitors"),unemployment,employment,labor_force),
#       ~asinh(.),.names="log_{col}"
#       )
#     )|>
#   select(group_id,date=week_date,leaid,county_fips,year,
#          log_public_scaled_days_visited, 
#          log_restaurant_scaled_days_visited, 
#          log_cafe_scaled_days_visited, 
#          log_bar_scaled_days_visited, 
#          log_library_scaled_days_visited, 
#          log_private_scaled_days_visited,
#          
#          public_scaled_days_visited, 
#          restaurant_scaled_days_visited, 
#          cafe_scaled_days_visited, 
#          bar_scaled_days_visited, 
#          library_scaled_days_visited, 
#          private_scaled_days_visited,
#          
#          log_public_days_visited, 
#          log_restaurant_days_visited, 
#          log_cafe_days_visited, 
#          log_bar_days_visited, 
#          log_library_days_visited, 
#          log_private_days_visited, 
#          
#          public_days_visited, 
#          restaurant_days_visited, 
#          cafe_days_visited, 
#          bar_days_visited, 
#          library_days_visited, 
#          private_days_visited, 
#          
#          log_public_scaled_visitors, 
#          log_restaurant_scaled_visitors, 
#          log_cafe_scaled_visitors, 
#          log_bar_scaled_visitors, 
#          log_library_scaled_visitors, 
#          log_private_scaled_visitors, 
#          
#          public_scaled_visitors, 
#          restaurant_scaled_visitors, 
#          cafe_scaled_visitors, 
#          bar_scaled_visitors, 
#          library_scaled_visitors, 
#          private_scaled_visitors, 
#          
#          log_public_visitors, 
#          log_restaurant_visitors, 
#          log_cafe_visitors, 
#          log_bar_visitors, 
#          log_library_visitors, 
#          log_private_visitors, 
#          
#          public_visitors, 
#          restaurant_visitors, 
#          cafe_visitors, 
#          bar_visitors, 
#          library_visitors, 
#          private_visitors, 
#          
#          log_unemployment, 
#          log_employment, 
#          log_labor_force,
#          unemployment,employment,labor_force,weekly_cases_per100k,population,
#          starts_with("R2L"),
#          starts_with("MCH"),
#          starts_with("Burbio"),
#          )|>
#   mutate(across(contains("date"),~floor(as.numeric(.)/7),.names="{col}"))|>
#   pivot_longer(contains("R2L") | contains("MCH") | contains("Burbio"),names_to = c("vendor",".value"),names_pattern = "([A-Z|a-z|2]+)_(.*)")|>
#   distinct()|>
#   filter(!is.na(start_date),!is.na(status_start))|>
#   mutate(relative_time=pmin(pmax(date-start_date,-12),12))
# 
# DID_base_data_wide<-
#   DID_base_data_wide|>
#   left_join(demo_merged)|>
#   left_join(k12_enrollment)

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
# MCH_R2L_horse_race_sample<-
horse_race_samples<-
  start_Fall_policies|>
  select(leaid,county_fips,name=vendor)|>
  mutate(value=TRUE)|>
  pivot_wider(values_fill = FALSE)|>
  mutate(
    All=TRUE,
    MCH_R2L=if_all(c(MCH,R2L),~.),
    Burbio_MCH_R2L=if_all(c(MCH,R2L,Burbio),~.),
  )|>
  select(-R2L,-MCH,-Burbio)|>
  pivot_longer(-c(leaid,county_fips),names_to = "districts")|>
  filter(value)|>
  select(-value)|>
  distinct()

# 
# 
# 
# 
# 
# MCH_R2L_horse_race_sample<-
#   start_Fall_policies|>
#   filter(!is.na(R2L_status_start),!is.na(MCH_status_start),!is.na(R2L_open),!is.na(MCH_open))|>
#   distinct(leaid,county_fips,year)
# 
# Burbio_MCH_R2L_horse_race_sample<-
#   analytic|>
#   filter(!is.na(R2L_status_start),!is.na(R2L_open),
#          !is.na(MCH_status_start),!is.na(MCH_open),
#          !is.na(Burbio_status_start),!is.na(Burbio_open),
#          )|>
#   distinct(leaid,county_fips,year)


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


setFixest_notes(FALSE)
est_models<-function(sample,depvars, cs_depvars=depvars) {
  x0<-
    sample|>
    cross_join(dates)|>
    inner_join(DID_base_data_wide)
  
  map(depvars,
      \(depvar) {
        message("Doing variable ", depvar,rep(" ", 32),"\r", appendLF = F)
        ret=
          bind_rows(
            feols(.[depvar]~treat_g*open|group_id+date,data=x0, lean = T, cluster=~county_fips,)|>broom::tidy()|>mutate(type="twfe_DD"),
            feols(.[depvar]~i(relative_time,treat_g,ref=-2)|group_id+date,data=x0, lean = T, cluster=~county_fips)|>broom::tidy()|>mutate(type="twfe_event")
          )
        # 
        # 
        # if(depvar %in% cs_depvars) {
        #   cs_uncond=cs_att_gt(data=filter(x0,!flag),yname=depvar)
        #   cs_cond  =cs_att_gt(data=filter(x0,!flag),yname=depvar,xformla=xf)
        #   
        #   agg_cs_uncond=aggte(cs_uncond,type="dynamic",na.rm=T)
        #   agg_cs_cond  =aggte(cs_cond  ,type="dynamic",na.rm=T)
        #   
        #   cs_results=bind_rows(
        #     aggte(cs_uncond,type="simple" ,na.rm=T)|>broom::tidy()|>mutate(type="cs_uncond_simple" ),
        #     aggte(cs_uncond,type="dynamic",na.rm=T)|>broom::tidy()|>mutate(type="cs_uncond_dynamic"),
        #     
        #     aggte(cs_cond  ,type="simple" ,na.rm=T)|>broom::tidy()|>mutate(type="cs_cond_simple"   ),
        #     aggte(cs_cond  ,type="dynamic",na.rm=T)|>broom::tidy()|>mutate(type="cs_cond_dynamic"  )
        #   )
        # 
        #   ret=bind_rows(ret,cs_results)
        # }
        
        ret$depvar=depvar
        
        return(ret)
        
      })|>
    list_rbind()
  
}


model_results<-
  DID_datasets|>
  group_by(T_type,C_type,vendor,districts)|>
  group_modify(
    \(x,y) {
      message("Running group ",y,"\r", appendLF = F)
      z=x|>cross_join(y)
      
      est_models(z,depvars,cs_depvars)
    },
    .keep=TRUE
  )

model_results|>
  write_csv("Output/twfe_model_results.csv", append = T)


# now select a set of models to run with CS-did



