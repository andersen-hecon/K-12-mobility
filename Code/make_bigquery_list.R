# create the geohash file for bigquery
library(tidyverse)

schools<-read_csv("Data/combined_schools_gh.csv.gz")


schools<-
  schools|>
  mutate(id=paste0(id,":",ring))|>
  distinct(id,gh)

schools|>arrow::write_parquet("Data/schools_gh_bq.parquet")

amenities<-read_csv("Data/amenities.csv.gz")

amenities<-
  amenities|>
  mutate(id=paste0(gh,":",amenity))|>
  distinct(id,gh)

amenities|>arrow::write_parquet("Data/amenities_gh_bq.parquet")

bind_rows(
  schools,
  amenities
)|>
  write_csv("Data/bigquery_gh_list.csv.gz")


