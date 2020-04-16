library(tidyverse)
library(butteR)
library(koboloadeR)
library(survey)
library(lubridate)
library(sf)
library(openxlsx)
library(srvyr)

#Call in the data we will use



source("scripts/functions/aok_aggregation_functions.R")
#source("scripts/functions/aok_cleaning_functions.R")
source("scripts/functions/aok_aggregate_by_county_wrapped.R")

#Call in the data we will use====
#Spatial data folder_path
admin_gdb<- "inputs/gis_data/boundaries"

#Clean data
df<-read.csv("inputs/2020_01/h2r_jan_consolidated_mog_baidoa_clean.csv", stringsAsFactors = FALSE)

#latest settlement data used
itemset<-read.csv("inputs/2020_01/itemsets.csv", stringsAsFactors = FALSE)
colnames(itemset)<-paste0("calc.",colnames(itemset))

#Baidoa data collection tool
a <- loadWorkbook ( "inputs/2020_01/SOM_H2R_Jan_1_Baidoa.xlsx")
sheetNames <- sheets(a)
for(i in 1:length(sheetNames))
{
  assign(sheetNames[i],readWorkbook(a,sheet = i))
}


# Spatial files regional, district, 10km hex, 6.7hm hex and settlments files


adm1<- st_read(admin_gdb,"Regional_boundary")
adm1<-st_transform(adm1,crs=4326)

adm2<- st_read(admin_gdb,"District_boundary" )
adm2<-st_transform(adm2,crs=4326)

#hex_10km <- st_read(admin_gdb,"SOM_H2R_Hex_Grid" )
#hex_10km <-st_transform(hex_10km,crs=4326)

#hex_6.7km <- st_read(admin_gdb,"Somalia_Hexagons" )
#hex_6.7km <-st_transform(hex_6.7km,crs=4326)


hex_400km <- st_read(admin_gdb,"Somalia_Hexagons_400" )
hex_400km <-st_transform(hex_400km,crs=4326)

som_settlements <- st_read(admin_gdb,"Somalia_Setlements_Update_2702" )
som_settlements <-st_transform(som_settlements,crs=4326)


#Data formattting for aggregation====


#Remove columns with only blanks

df <- Filter(function(x)!all(is.na(x) ), df)


#Create a new column that combines what was mapped as other and has nearest settlement given, keep only dataset with both columns and records have values
df <- df %>% filter(!info_settlement=="") %>%  mutate(finalsettlment= ifelse(info_settlement=="other",info_set_oth_near,info_settlement))



#Join with the settlement data as some districts are blank if chosen near settlement

names(itemset)[names(itemset) == "calc.name"] <- "finalsettlment"

item_geo <- itemset %>%  select(finalsettlment,calc.district,calc.region)

item_geo <- distinct(item_geo,finalsettlment, .keep_all= TRUE)

df <- left_join(df,item_geo, by = "finalsettlment")


#Ki- settlement level aggregation----------
#Columns select beased function applied

source("scripts/h2r_columns_for_aggregation.R")

ki_coverage <- df %>%
  select(calc.region,calc.district,finalsettlment, particip_again) %>%
  group_by(calc.region,calc.district,finalsettlment) %>%
  summarise(ki_num = length(particip_again))


#table join. Let us merge all these columns/fields into one database
analysis_df_list<-list(settlement_yes, settlement_equal,settlement_mscols)
# settlement_joined<-purrr::reduce(analysis_df_list, left_join(by= c("D.info_state","D.info_county", "D.info_settlement")))
settlement_data <-purrr::reduce(analysis_df_list, left_join)

## Combining settlement and county name for when we join to ArcGIS!! Also adding in the column for KI coverage

settlement_data <- settlement_data %>%
  ungroup() %>%
  mutate(D.ki_coverage = as.numeric(ki_coverage$ki_num))


# #Let us rearrange the columns in our database inthe same order appear on the tool
settlement_data <- settlement_data %>% select(order(match(names(settlement_data), names(df))))

#check missing column names

missing_columns<-colnames(df)[colnames(df)%in% colnames(settlement_data)==FALSE]


check_these<-missing_columns[missing_columns %in% not_needed_columns ==FALSE]

if(length(check_these>0)){
  print("WARNING you missed these: ")
  check_these %>% dput()
}



settlement_data <- settlement_data %>%  select(base:consent,calc.region, calc.district,finalsettlment,D.ki_coverage,info_settlement:particip_again)


write.csv(
  settlement_data,
  file = "outputs/som_H2r__clean_data_20200101.csv",
  na = "",
  row.names = FALSE)

#Spatial join----

#Join our data to settlment shapefile

settlement_data$P_CODE <- settlement_data$finalsettlment
settlement_data$month <- "20200101"
som_settlements_data <- inner_join(som_settlements,settlement_data )


som_settlements_data <-st_join( som_settlements_data, hex_400km)

names(som_settlements_data)[names(som_settlements_data) == "GRID_ID"] <- "hex_4000km"


#Settlement data with hexagons information

som_settlements_data <- som_settlements_data %>%
  select(OBJECTID_1,name,ADM1_NAME,ADM2_NAME,hex_4000km,base,assess_mode,consent,finalsettlment:particip_again,geometry)


setlement_level <- som_settlements_data %>%  select(name:particip_again) %>% filter(!is.na(D.ki_coverage))

#Reformatting the datato run in srvyr package for the as_survey function

setlement_level$still_inhabited <- forcats::fct_expand(setlement_level$still_inhabited,c("yes","no"))
setlement_level$livelihood_activ.money_rent <- forcats::fct_expand(setlement_level$livelihood_activ.money_rent,c("yes","no"))
setlement_level$livelihood_activ.none <- forcats::fct_expand(setlement_level$livelihood_activ.none,c("yes","no"))
setlement_level$livelihood_activ.humanitar_assistance <- forcats::fct_expand(setlement_level$livelihood_activ.humanitar_assistance,c("yes","no"))
setlement_level$education_available.ngoschool <- forcats::fct_expand(setlement_level$education_available.ngoschool,c("yes","no"))
setlement_level$main_radios.radio_xamar <- forcats::fct_expand(setlement_level$main_radios.radio_xamar,c("yes","no"))
setlement_level$main_radios.radio_xurmo <- forcats::fct_expand(setlement_level$main_radios.radio_xurmo,c("yes","no"))

dfsvy_h2r_district <-srvyr::as_survey(setlement_level)



h2r_columns <- setlement_level %>% select(when_left_prev:still_contact_htr, - contains(c("other","dontknow","noresponse"))) %>%  colnames() %>% dput()


#Region level aggregation-----------

region_h2r <-butteR::mean_proportion_table(design = dfsvy_h2r_district,
                      list_of_variables = h2r_columns,
                      aggregation_level = "ADM1_NAME",
                      round_to = 2,
                     return_confidence = FALSE,
                     na_replace = FALSE)







#District level aggregation ----------
district_h2r <-butteR::mean_proportion_table(design = dfsvy_h2r_district,
                                             list_of_variables = h2r_columns,
                                             aggregation_level = "ADM2_NAME",
                                             round_to = 2,
                                             return_confidence = FALSE,
                                             na_replace = FALSE)


District_summary <- setlement_level %>%  select(ADM2_NAME,ADM1_NAME,D.ki_coverage) %>%
  group_by(ADM2_NAME) %>%
  summarise(assessed_num = n(), ki_num=sum(D.ki_coverage) )

District_summary <- data.frame(District_summary) %>%  select("ADM2_NAME", "assessed_num" ,  "ki_num" )




som_settlements_summary <- som_settlements %>% select(ADM2_NAME,ADM1_NAME) %>%
  group_by(ADM2_NAME) %>%
  summarise(settlem_num = n())


som_settlements_summary <- data.frame(som_settlements_summary) %>%  select("ADM2_NAME", "settlem_num")

#table join. Let us merge all these columns/fields into one database
analysis_df_list<-list(District_summary, som_settlements_summary,district_h2r)
# settlement_joined<-purrr::reduce(analysis_df_list, left_join(by= c("D.info_state","D.info_county", "D.info_settlement")))
district_level <-purrr::reduce(analysis_df_list, left_join)





#Grid level aggregation---------

hex_400_h2r <-butteR::mean_proportion_table(design = dfsvy_h2r_district,
                                            list_of_variables = h2r_columns,
                                            aggregation_level = "hex_4000km",
                                            round_to = 2,
                                            return_confidence = FALSE,
                                            na_replace = FALSE)



grid_summary_400km <- setlement_level %>%  select(hex_4000km,ADM1_NAME,D.ki_coverage) %>%
  group_by(hex_4000km) %>%
  summarise(assessed_num = n(), ki_num=sum(D.ki_coverage) )



grid_summary_400km <- data.frame(grid_summary_400km) %>%  select("hex_4000km", "assessed_num" ,  "ki_num" )


grid_400km <- inner_join(grid_summary_400km,hex_400_h2r, by="hex_4000km")


names(grid_400km)[names(grid_400km) == "D.ki_coverage"] <- "D.ki_coverage"
names(grid_400km)[names(grid_400km) == "sett_num"] <- "sett_num"





#Removes "." for use in ArcGIS -----
#Better to export these directly as shapefile but the data can be used in other platforms and a simple join with existing shapefiles will do

#Settlement level
setlement_level <- setlement_level %>% select(everything(), - contains(c("other","dontknow","noresponse")))
names(setlement_level) <- gsub("\\.", "_", names(setlement_level))


#grid_level
grid_level <- grid_400km %>% select(everything(), - contains(c("other","dontknow","noresponse")))
names(grid_level) <- gsub("\\.", "_", names(grid_level))



#district_level

district_level <- district_level %>% select(everything(), - contains(c("other","dontknow","noresponse")))
names(district_level) <- gsub("\\.", "_", names(district_level))



#Exports datsets----

list_of_datasets <- list("settlement_aggregation" = setlement_level, "Aggregation by region" = region_h2r, "Aggregation by district" = district_level, "Aggregation by hex 400km"= grid_level)

#write.xlsx(list_of_datasets, file = "outputs/som_h2r_summary_20200101.xlsx")


write.csv(grid_level,"outputs/Aggregation by hex 400km.csv" )
write.csv(district_level,"outputs/Aggregation by district.csv" )
write.csv(district_level,"outputs/Aggregation by district.csv" )
write.csv(setlement_level,"outputs/settlement_aggregation.csv" )
