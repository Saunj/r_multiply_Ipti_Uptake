library(redcapAPI)
library(dplyr)
library(lubridate)
#source("token.R")

first_date_of_period <- "01-05-2022"
last_date_of_period <- "31-05-2022"

first_date_of_period <- as_datetime(first_date_of_period, format = "%d-%m-%Y")
last_date_of_period <- as_datetime(last_date_of_period, format = "%d-%m-%Y")


api.url <- "https://maternal.isglobal.org/redcap/api/"

api.token <- ""

rcon <- redcapConnection(api.url, api.token)

my.fields <- c("record_id","hf_district1" ,
               "hf_district2","hf_district3","report_period_since",
               "report_period_til","opd_u5_total","malaria_total",
               "rdt_total","rdt_pos_total","micro_total",
               "micro_pos_total","deaths_total",
               "deaths_malaria_total","anaemia_total",
               "referrals_total","referrals_malaria_total",
               "sam_total","adr_total")

my.events <- c("event_1_arm_1")

mobidity.data<- exportRecords(
  rcon,
  factors            = F,
  labels             = F,
  fields             = my.fields,
  events             = my.events,
  form_complete_auto = F
)

# Collapse the hf values in just one column
hfs <- c("hf_district1" , "hf_district2" ,"hf_district3")
mobidity.data$hf <- rowSums(mobidity.data[, hfs], na.rm = T)

mobidity.data$report_period_since_y <- format(mobidity.data$report_period_since, format = "%Y")

mobidity.data$report_period_since_m <- format(mobidity.data$report_period_since, format = "%m")

mobidity.data$report_period_since_d <- format(mobidity.data$report_period_since, format = "%d")

filter <- mobidity.data$report_period_since_y == "2022" & mobidity.data$report_period_since_m == "02"

View(mobidity.data[which(filter), ])

mobidity.data <- mobidity.data[,c('record_id','hf','report_period_since',
                                  'report_period_til','opd_u5_total','malaria_total',
                                  'rdt_total','rdt_pos_total','micro_total','micro_pos_total',
                                  'deaths_total','deaths_malaria_total','anaemia_total','referrals_total',
                                  'referrals_malaria_total',
                                  'sam_total','adr_total')]

filter <- mobidity.data$report_period_since <= last_date_of_period
total_mobidity.data <-mobidity.data[filter, ]

filter <- mobidity.data$report_period_since >= first_date_of_period

period_mobidity.data <-mobidity.data[filter,c('record_id','hf','report_period_since',
                                           'report_period_til','opd_u5_total','malaria_total',
                                           'rdt_total','rdt_pos_total','micro_total','micro_pos_total',
                                           'deaths_total','deaths_malaria_total','anaemia_total','referrals_total',
                                           'referrals_malaria_total',
                                           'sam_total','adr_total')]
total_mobidity <- nrow(total_mobidity.data)

period_mobidity <- nrow(period_mobidity.data)

names(total_mobidity.data) <- c('Record_ID','HF','Period_Since','Period_Til','OPD_U5','Suspected_Malaria','RDT_Done','Pos_Malaria',
                          'Microscopy','Pos_Microscopy','Deaths','Mal_Deaths','Anaemia',
                          'Referrals','Mal_Referrals','Severe_Malnutrition','AD_Reaction')

names(period_mobidity.data) <- c('Record_ID','HF','Period_Since','Period_Til','OPD_U5','Suspected_Malaria','RDT_Done','Pos_Malaria',
                                'Microscopy','Pos_Microscopy','Deaths','Mal_Deaths','Anaemia',
                                'Referrals','Mal_Referrals','Severe_Malnutrition','AD_Reaction')

write.csv(total_mobidity.data, file = "Total_Mobidity.csv", row.names = F)

write.csv(period_mobidity.data, file = "period_Mobidity.csv", row.names = F)
