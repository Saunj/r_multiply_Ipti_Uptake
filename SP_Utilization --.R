library(redcapAPI)
library(dplyr)
library(lubridate)
#source("token.R")

first_date_of_period <- "01-02-2022"
last_date_of_period <- "30-06-2022"

first_date_of_period <- as_datetime(first_date_of_period, format = "%d-%m-%Y")
last_date_of_period <- as_datetime(last_date_of_period, format = "%d-%m-%Y")

api.url <- "https://maternal.isglobal.org/redcap/api/"

api.token <- ""

rcon <- redcapConnection(api.url, api.token)

my.fields <- c("record_id","hf_district1" ,
               "hf_district2","hf_district3","report_period_since",
               "report_period_til","sp_stock_begin","sp_received","sp_used",
               "sp_wasted","sp_stock_end","sp_forecast")

my.events <- c("event_1_arm_1")

sp.data<- exportRecords(
  rcon,
  factors            = F,
  labels             = F,
  fields             = my.fields,
  events             = my.events,
  form_complete_auto = F
)

# Collapse the hf values in just one column
hfs <- c("hf_district1","hf_district2","hf_district3")
sp.data$hf <- rowSums(sp.data[, hfs], na.rm = T)

sp.data<- sp.data[,c('record_id','hf','report_period_since',
                           'report_period_til','sp_stock_begin','sp_received',
                           'sp_used',
                           'sp_wasted','sp_stock_end',
                           'sp_forecast')]

filter <- sp.data$report_period_since <= last_date_of_period
total_sp.data<-sp.data[filter, ]

filter <- sp.data$report_period_since >= first_date_of_period

period_sp.data <-sp.data[filter,c('record_id','hf','report_period_since',
                                  'report_period_til','sp_stock_begin','sp_received',
                                  'sp_used',
                                  'sp_wasted','sp_stock_end',
                                  'sp_forecast')]
total_sp <- nrow(total_sp.data)

period_sp <- nrow(period_sp.data)

write.csv(total_sp.data, file = "Total_sp.csv", row.names = F)

write.csv(period_sp.data, file = "period_sp.csv", row.names = F)
