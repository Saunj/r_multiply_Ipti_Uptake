library(redcapAPI)
library(dplyr)
library(lubridate)
#source("token.R")

first_date_of_period <- "30-06-2022"
last_date_of_period <- "30-06-2022"

first_date_of_period <- as_datetime(first_date_of_period, format = "%d-%m-%Y")
last_date_of_period <- as_datetime(last_date_of_period, format = "%d-%m-%Y")

api.url <- "https://maternal.isglobal.org/redcap/api/"

api.token <- ""

rcon <- redcapConnection(api.url, api.token)

my.fields <- c("record_id","hf_district1" ,
               "hf_district2","hf_district3","report_period_since",
               "report_period_til","labels_stock_begin","labels_used","labels_stock_damaged",
               "labels_stock_end","labels_forecast")

my.events <- c("event_1_arm_1")

label.data<- exportRecords(
  rcon,
  factors            = F,
  labels             = F,
  fields             = my.fields,
  events             = my.events,
  form_complete_auto = F
)

# Collapse the hf values in just one column
hfs <- c("hf_district1","hf_district2","hf_district3")
label.data$hf <- rowSums(label.data[, hfs], na.rm = T)

label.data<- label.data[,c('record_id','hf','report_period_since',
                                  'report_period_til','labels_stock_begin','labels_used',
                                  'labels_stock_damaged',
                                  'labels_stock_end',
                                  'labels_forecast')]

filter <- label.data$report_period_since <= last_date_of_period
total_label.data <-label.data[filter, ]

filter <- label.data$report_period_since >= first_date_of_period

period_label.data <-label.data[filter,c('record_id','hf','report_period_since',
                                        'report_period_til','labels_stock_begin','labels_used',
                                        'labels_stock_damaged',
                                        'labels_stock_end',
                                        'labels_forecast')]

total_label <- nrow(total_label.data)

period_label <- nrow(period_label.data)

write.csv(total_label.data, file = "Total_Label_Utilization.csv", row.names = F)

write.csv(period_label.data, file = "Period_Label_Utilization.csv", row.names = F)