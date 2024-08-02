library(tidyverse)
library(lubridate)
library(fixest)
library(modelsummary)
library(did)
library(patchwork)

job=Sys.getenv("SLURM_ARRAY_TASK_ID")
total_jobs=Sys.getenv("SLURM_ARRAY_TASK_COUNT")




DID_base_data_wide<-read_csv("Data/DID_base_data_wide.csv")
DID_datasets<-read_csv("Data/DID_datasets_jobs.csv")

if(job=="") {
  DID_datasets=DID_datasets
} else {
  DID_datasets=DID_datasets|>
    filter(
      (job_id %% as.numeric(total_jobs)) ==(as.numeric(job) %% as.numeric(total_jobs))
    )
}


cs_att_gt=purrr::partial(
  att_gt,
  tname="date",
  idname="group_id",
  gname="treat",
  control_group = "notyettreated",
  # base_period = "universal",
  anticipation = 2,
  # clustervars = "county_fips",
  # weightsname="enrollment"
)

# get figure 1 data
fig1_data<-
  DID_datasets|>
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
  write_csv(glue::glue("Output/cs-did-job{job}.csv.gz"))