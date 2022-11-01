library(redcapAPI)
library(dplyr)
library(lubridate)
#source("token.R")

first_date_of_period <- "01-08-2022"
last_date_of_period <- "30-09-2022"

first_date_of_period <- as_datetime(first_date_of_period, format = "%d-%m-%Y")
last_date_of_period <- as_datetime(last_date_of_period, format = "%d-%m-%Y")

api.url <- ""

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
hf_name <- c(label.data$hf)

label.data$hf_name <- hf_name

label.data$hf_name  <- factor(label.data$hf_name ,
                        levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
                                   16,17,18,19,20,21,22,23,24,25),
                        labels = c("Binkolo","Kagbere","Kalangba","Kamabai","Kamaranka","Magbenteh","Makoloh","Mara","Tambiama","Malal","Mamankie","Mana II",
                                   "Mange","Minthomore","New Maforkie","SOG","Tagrin","BumbunaC HP","Hinistas","Makonkorie","Masengbeh","Masumbrie","MathampMCHP",
                                   "Rokimbie MCHP","Yele"))

label.data<- label.data[,c('record_id','hf','hf_name','report_period_since',
                                  'report_period_til','labels_stock_begin','labels_used',
                                  'labels_stock_damaged',
                                  'labels_stock_end',
                                  'labels_forecast')]

label_quantity_check <- c(label.data$labels_stock_begin - (label.data$labels_used + label.data$labels_stock_damaged))
label.data$label_quantity_check <- label_quantity_check

labelcheck <- c(label.data$labels_stock_end == label.data$label_quantity_check)
label.data$labelcheck <- labelcheck

label_balance_check <- c(label.data$labels_stock_begin == label.data$labels_stock_end | label.data$labels_stock_end < label.data$labels_stock_begin)
label.data$label_balance_check <- label_balance_check

#Multiply Inconsistency check

filter <-  label.data$labelcheck == 'FALSE'
labelcheck.data <- label.data[filter, ]
labelcheck.data <-labelcheck.data[which(!is.na(labelcheck.data$labelcheck)), ] 
labelcheck.data <- labelcheck.data[,c('record_id','hf','hf_name','report_period_since','labels_stock_begin','labels_used','labels_stock_damaged', 'labels_stock_end','label_quantity_check','labelcheck')]

filter <-  label.data$label_balance_check == 'FALSE'
label_balance_check.data <- label.data[filter, ]
label_balance_check.data <-label_balance_check.data[which(!is.na(label_balance_check.data$label_balance_check)), ] 
label_balance_check.data <- label_balance_check.data[,c('record_id','hf','hf_name','report_period_since','labels_stock_begin','labels_stock_end','label_balance_check')]

filter <- label.data$report_period_since <= last_date_of_period
total_label.data <-label.data[filter, ]

filter <- label.data$report_period_since >= first_date_of_period

period_label.data <-label.data[filter,c('record_id','hf','hf_name','report_period_since',
                                        'report_period_til','labels_stock_begin','labels_used',
                                        'labels_stock_damaged',
                                        'labels_stock_end',
                                        'labels_forecast')]

total_label <- nrow(total_label.data)

period_label <- nrow(period_label.data)

write.csv(total_label.data, file = "Total_Label_Utilization.csv", row.names = F)

write.csv(period_label.data, file = "Period_Label_Utilization.csv", row.names = F)

