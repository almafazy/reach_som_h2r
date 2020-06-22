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

#Now we want to clean the data for some skiplogic (SL) errors introduced after settlement level aggregation.



# KI info
# left_behind_y_n

settlement_data$left_behind_who.men_18_59[settlement_data$left_behind_y_n != "yes"] <- "SL"
settlement_data$left_behind_who.b_0_11[settlement_data$left_behind_y_n != "yes"] <- "SL"
settlement_data$left_behind_who.women_60_eld[settlement_data$left_behind_y_n != "yes"] <- "SL"
settlement_data$left_behind_who.boys_12_17[settlement_data$left_behind_y_n != "yes"] <- "SL"
settlement_data$left_behind_who.men_60_eld[settlement_data$left_behind_y_n != "yes"] <- "SL"
settlement_data$left_behind_who.women_18_59[settlement_data$left_behind_y_n != "yes"] <- "SL"
settlement_data$left_behind_who.g_0_11[settlement_data$left_behind_y_n != "yes"] <- "SL"
settlement_data$left_behind_who.girls_12_17[settlement_data$left_behind_y_n != "yes"] <- "SL"

#pwd
settlement_data$pwd_left_behind[settlement_data$left_behind_y_n != "yes"] <- "SL"


#idp_new_arrivals

settlement_data$idp_arrived_from[settlement_data$idp_new_arrivals != "yes"] <- "SL"
settlement_data$idp_arrived_from_reg[settlement_data$idp_new_arrivals != "yes"] <- "SL"
settlement_data$idp_arrived_from_district[settlement_data$idp_new_arrivals != "yes"] <- "SL"
settlement_data$idp_arrived_reason.lack_jobs[settlement_data$idp_new_arrivals != "yes"] <- "SL"
settlement_data$idp_arrived_reason.no_services[settlement_data$idp_new_arrivals != "yes"] <- "SL"
settlement_data$idp_arrived_reason.lack_jobs[settlement_data$idp_new_arrivals != "yes"] <- "SL"
settlement_data$idp_arrived_reason.evictions[settlement_data$idp_new_arrivals != "yes"] <- "SL"
settlement_data$idp_arrived_reason.noresponse[settlement_data$idp_new_arrivals != "yes"] <- "SL"
settlement_data$idp_arrived_reason.dontknow[settlement_data$idp_new_arrivals != "yes"] <- "SL"
settlement_data$idp_arrived_reason.drought[settlement_data$idp_new_arrivals != "yes"] <- "SL"
settlement_data$idp_arrived_reason.flooding[settlement_data$idp_new_arrivals != "yes"] <- "SL"
settlement_data$idp_arrived_reason.other[settlement_data$idp_new_arrivals != "yes"] <- "SL"
settlement_data$idp_arrived_reason.conflict[settlement_data$idp_new_arrivals != "yes"] <- "SL"



settlement_data$idp_pull_factors.better_security[settlement_data$idp_new_arrivals != "yes"] <- "SL"
settlement_data$idp_pull_factors.noresponse[settlement_data$idp_new_arrivals != "yes"] <- "SL"
settlement_data$idp_pull_factors.better_services[settlement_data$idp_new_arrivals != "yes"] <- "SL"
settlement_data$idp_pull_factors.presence_jobs[settlement_data$idp_new_arrivals != "yes"] <- "SL"
settlement_data$idp_pull_factors.availability_shelters[settlement_data$idp_new_arrivals != "yes"] <- "SL"
settlement_data$idp_pull_factors.access_water[settlement_data$idp_new_arrivals != "yes"] <- "SL"
settlement_data$idp_pull_factors.other[settlement_data$idp_new_arrivals != "yes"] <- "SL"
settlement_data$idp_pull_factors.dontknow[settlement_data$idp_new_arrivals != "yes"] <- "SL"
settlement_data$idp_pull_factors.access_food[settlement_data$idp_new_arrivals != "yes"] <- "SL"



# Food Security and Nutrition

#nomarket

settlement_data$nomarket_why.bad_quality[settlement_data$access_market != "no_access"] <- "SL"
settlement_data$nomarket_why.dontknow[settlement_data$access_market != "no_access"] <- "SL"
settlement_data$nomarket_why.market_far[settlement_data$access_market != "no_access"] <- "SL"
settlement_data$nomarket_why.other[settlement_data$access_market != "no_access"] <- "SL"
settlement_data$nomarket_why.no_items[settlement_data$access_market != "no_access"] <- "SL"
settlement_data$nomarket_why.security[settlement_data$access_market != "no_access"] <- "SL"
settlement_data$nomarket_why.no_cash[settlement_data$access_market != "no_access"] <- "SL"

#marketregion
settlement_data$market_region[settlement_data$access_market != "yes_always" & settlement_data$access_market != "yes_restricted" ] <- "SL"
# marketdistrict
settlement_data$market_district[settlement_data$access_market != "yes_always" & settlement_data$access_market != "yes_restricted" ] <- "SL"
settlement_data$market_settlement[settlement_data$access_market != "yes_always" & settlement_data$access_market != "yes_restricted" ] <- "SL"
settlement_data$distance_to_market[settlement_data$access_market != "yes_always" & settlement_data$access_market != "yes_restricted" ] <- "SL"

#marketgoods
settlement_data$market_goods.clothes_sewing[settlement_data$access_market != "yes_always" & settlement_data$access_market != "yes_restricted" ] <- "SL"
settlement_data$market_goods.tools_seeds[settlement_data$access_market != "yes_always" & settlement_data$access_market != "yes_restricted" ] <- "SL"
settlement_data$market_goods.livestock[settlement_data$access_market != "yes_always" & settlement_data$access_market != "yes_restricted" ] <- "SL"
settlement_data$market_goods.food[settlement_data$access_market != "yes_always" & settlement_data$access_market != "yes_restricted" ] <- "SL"
settlement_data$market_goods.jerry_cans[settlement_data$access_market != "yes_always" & settlement_data$access_market != "yes_restricted" ] <- "SL"
settlement_data$market_goods.construction_materials[settlement_data$access_market != "yes_always" & settlement_data$access_market != "yes_restricted" ] <- "SL"
settlement_data$market_goods.mosquito_nets[settlement_data$access_market != "yes_always" & settlement_data$access_market != "yes_restricted" ] <- "SL"
settlement_data$market_goods.womens_materials[settlement_data$access_market != "yes_always" & settlement_data$access_market != "yes_restricted" ] <- "SL"
settlement_data$market_goods.fuel_cooking[settlement_data$access_market != "yes_always" & settlement_data$access_market != "yes_restricted" ] <- "SL"
settlement_data$market_goods.dontknow[settlement_data$access_market != "yes_always" & settlement_data$access_market != "yes_restricted" ] <- "SL"
settlement_data$market_goods.soap[settlement_data$access_market != "yes_always" & settlement_data$access_market != "yes_restricted" ] <- "SL"
settlement_data$market_goods.shoes[settlement_data$access_market != "yes_always" & settlement_data$access_market != "yes_restricted" ] <- "SL"




#lackfoods
settlement_data$lack_food_reasons.noland[settlement_data$skip_meals != "yes"] <- "SL"
settlement_data$lack_food_reasons.nomarket[settlement_data$skip_meals != "yes"] <- "SL"
settlement_data$lack_food_reasons.natural_causes[settlement_data$skip_meals != "yes"] <- "SL"
settlement_data$lack_food_reasons.other[settlement_data$skip_meals != "yes"] <- "SL"
settlement_data$lack_food_reasons.economic_causes[settlement_data$skip_meals != "yes"] <- "SL"
settlement_data$lack_food_reasons.dontknow[settlement_data$skip_meals != "yes"] <- "SL"
settlement_data$lack_food_reasons.security[settlement_data$skip_meals != "yes"] <- "SL"
settlement_data$lack_food_reasons_other[settlement_data$skip_meals != "yes"] <- "SL"


#Health

settlement_data$available_health_services.clinic[settlement_data$access_healthservices != "yes"] <- "SL"
settlement_data$available_health_services.none[settlement_data$access_healthservices != "yes"] <- "SL"
settlement_data$available_health_services.mobile_clinic[settlement_data$access_healthservices != "yes"] <- "SL"
settlement_data$available_health_services.hospital[settlement_data$access_healthservices != "yes"] <- "SL"
settlement_data$available_health_services.first_aid[settlement_data$access_healthservices != "yes"] <- "SL"
settlement_data$available_health_services.other[settlement_data$access_healthservices != "yes"] <- "SL"
settlement_data$available_health_services.midwife[settlement_data$access_healthservices != "yes"] <- "SL"
settlement_data$available_health_services.dontknow[settlement_data$access_healthservices != "yes"] <- "SL"
settlement_data$available_health_services.healer[settlement_data$access_healthservices != "yes"] <- "SL"
settlement_data$available_health_services.individual_pract[settlement_data$access_healthservices != "yes"] <- "SL"
settlement_data$available_health_services.drugstore[settlement_data$access_healthservices != "yes"] <- "SL"

#No_Health_Access

settlement_data$noaccess_health.none[settlement_data$access_healthservices != "yes"] <- "SL"
settlement_data$noaccess_health.m_over60[settlement_data$access_healthservices != "yes"] <- "SL"
settlement_data$noaccess_health.m_over18[settlement_data$access_healthservices != "yes"] <- "SL"
settlement_data$noaccess_health.g_under18[settlement_data$access_healthservices != "yes"] <- "SL"
settlement_data$noaccess_health.pwd[settlement_data$access_healthservices != "yes"] <- "SL"
settlement_data$noaccess_health.other[settlement_data$access_healthservices != "yes"] <- "SL"
settlement_data$noaccess_health.dontknow[settlement_data$access_healthservices != "yes"] <- "SL"
settlement_data$noaccess_health.b_under18[settlement_data$access_healthservices != "yes"] <- "SL"
settlement_data$noaccess_health.w_over60[settlement_data$access_healthservices != "yes"] <- "SL"
settlement_data$noaccess_health.w_over18[settlement_data$access_healthservices != "yes"] <- "SL"


# ngo support

settlement_data$ngo_support_type.none[settlement_data$ngo_support_y_n != "yes"] <- "SL"
settlement_data$ngo_support_type.livestock[settlement_data$ngo_support_y_n != "yes"] <- "SL"
settlement_data$ngo_support_type.cash_distrib[settlement_data$ngo_support_y_n != "yes"] <- "SL"
settlement_data$ngo_support_type.seeds_tools[settlement_data$ngo_support_y_n != "yes"] <- "SL"
settlement_data$ngo_support_type.other[settlement_data$ngo_support_y_n != "yes"] <- "SL"
settlement_data$ngo_support_type.vaccinations[settlement_data$ngo_support_y_n != "yes"] <- "SL"
settlement_data$ngo_support_type.dontknow[settlement_data$ngo_support_y_n != "yes"] <- "SL"
settlement_data$ngo_support_type.food_distrib[settlement_data$ngo_support_y_n != "yes"] <- "SL"
settlement_data$ngo_support_type.education_service[settlement_data$ngo_support_y_n != "yes"] <- "SL"
settlement_data$ngo_support_type.construction_materials_nfis[settlement_data$ngo_support_y_n != "yes"] <- "SL"
settlement_data$ngo_support_type.legal_support[settlement_data$ngo_support_y_n != "yes"] <- "SL"





settlement_data <- settlement_data %>%  select(base:consent,calc.region, calc.district,finalsettlment,D.ki_coverage,info_settlement:particip_again)


write.csv(
  settlement_data,
  file = "outputs/som_H2r__clean_data_20200101.csv",
  na = "",
  row.names = FALSE)

#Spatial join----

#Join our data to settlement shapefile

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
