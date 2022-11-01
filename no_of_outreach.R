library(redcapAPI)
library(dplyr) # group_by and mutate functions
library(tidyr) # pivot_wider function
library(lubridate)

first_date_of_period <- "01-08-2022"
last_date_of_period <- "30-09-2022"

first_date_of_period <- as_datetime(first_date_of_period, format = "%d-%m-%Y")
last_date_of_period <- as_datetime(last_date_of_period, format = "%d-%m-%Y")


api.url <- ""

api.token <-  ""

rcon <- redcapConnection(api.url, api.token)

my.fields <- c("record_id","hf_district1" , "hf_district2","hf_district3","report_period_since",
               "report_period_til","outreach_planned_total","outreach_conducted_total")

my.events <- c("event_1_arm_1")

outreach.data<- exportRecords(
  rcon,
  factors            = F,
  labels             = F,
  fields             = my.fields,
  events             = my.events,
  form_complete_auto = F
)

# Collapse the hf values in just one column
hfs <- c("hf_district1" , "hf_district2" ,"hf_district3")
outreach.data$hf <- rowSums(outreach.data[, hfs], na.rm = T)

outreach.data <- outreach.data[ c('record_id','hf',"report_period_since",'outreach_planned_total','outreach_conducted_total')]

conducted_outreach_check <- c(outreach.data$outreach_planned_total == outreach.data$outreach_conducted_total)

outreach.data$conducted_outreach_check <- conducted_outreach_check


filter <-  outreach.data$conducted_outreach_check == 'FALSE'
conducted_outreach_check.data <-  outreach.data[filter, ]
conducted_outreach_check.data <-conducted_outreach_check.data[which(!is.na(conducted_outreach_check.data$conducted_outreach_check)), ] 
conducted_outreach_check.data <- conducted_outreach_check.data[,c('record_id','hf',"report_period_since",'outreach_planned_total','outreach_conducted_total','conducted_outreach_check')]

#################################################################################################################################
library(redcapAPI)
library(dplyr) # group_by and mutate functions
library(tidyr) # pivot_wider function
library(lubridate)
first_date_of_period <- "01-08-2022"
last_date_of_period <- "30-09-2022"

first_date_of_period <- as_datetime(first_date_of_period, format = "%d-%m-%Y")
last_date_of_period <- as_datetime(last_date_of_period, format = "%d-%m-%Y")

api.url <- ""

api.token <-  ""

rcon <- redcapConnection(api.url, api.token)

my.fields <- c("record_id","hf_district1","hf_district2","hf_district3","report_period_since",
               "report_period_til","report_date","outreach","why_not_outreach")

my.events <- c("event_1_arm_1")

ss_outreach.data<- exportRecords(
  rcon,
  factors            = F,
  labels             = F,
  fields             = my.fields,
  events             = my.events,
  form_complete_auto = F
)

hfs <- c("hf_district1" , "hf_district2" ,"hf_district3")
ss_outreach.data$hf <- rowSums(ss_outreach.data[, hfs], na.rm = T)


ss_outreach.data <- ss_outreach.data[c('record_id','report_period_since','report_period_til','report_date','hf','outreach','why_not_outreach')]


filter <-  ss_outreach.data$outreach == 1
ss_outreach_check.data <-  ss_outreach.data[filter, ]
ss_outreach_check.data <-ss_outreach_check.data[which(!is.na(ss_outreach_check.data$outreach)), ] 
ss_outreach_check.data <- ss_outreach_check.data[,c('record_id','hf','report_period_since','outreach')]

conducted_outreach_check.data$id <- paste(conducted_outreach_check.data$hf, conducted_outreach_check.data$report_period_since, sep = "_")
ss_outreach_check.data$id <- paste(ss_outreach_check.data$hf, ss_outreach_check.data$report_period_since, sep = "_")

inconsistentcies_outreach.data <- merge(conducted_outreach_check.data , ss_outreach_check.data, by = 'id')


write.csv(outreach.data , file = "No of Outreach Performed.csv", row.names = F)




