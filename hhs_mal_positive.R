library(redcapAPI)
library(dplyr) # group_by and mutate functions
library(tidyr) # pivot_wider function
library(lubridate)

first_date_of_period <- ""
last_date_of_period <- ""

first_date_of_period <- as_datetime(first_date_of_period, format = "%d-%m-%Y")
last_date_of_period <- as_datetime(last_date_of_period, format = "%d-%m-%Y")


api.url <- ""

api.token <-  ""

rcon <- redcapConnection(api.url, api.token)

my.fields <- c('record_id','district','cluster_bombali', 'cluster_port_loko','cluster_tonkolili',
               'household','consent','child','rdt','rdt_result')

my.events <- c("household_baseline_arm_1","malaria_rdt_arm_1")
hhs.data<- exportRecords(
  rcon,
  factors            = F,
  labels             = F,
  fields             = my.fields,
  events             = my.events,
  form_complete_auto = F
)

#Combine ipti1 column of both fixed and outreach
clusterdistricts <- c('cluster_bombali','cluster_port_loko','cluster_tonkolili')  
hhs.data$cluster_district<- rowSums(hhs.data[, clusterdistricts], na.rm = T)

filter <-  hhs.data$redcap_event_name == "household_baseline_arm_1"
hhs.data1 <- hhs.data[filter, ]

hhs.data1 <- hhs.data[, c('record_id','district','cluster_district','household','consent')]

filter <-  hhs.data1$consent == 1
hhs.data2 <- hhs.data1[filter, ]

hhs.data2 <- hhs.data2[, c('record_id','district','cluster_district','household','consent')]

hhs.data2 <- hhs.data2[which(!is.na(hhs.data2$consent)), ]


#RDT Test
filter <-  hhs.data$redcap_event_name == "malaria_rdt_arm_1"
rdt.data <- hhs.data[filter, ]

rdt.data <- hhs.data[, c('record_id','rdt','rdt_result')]

rdt.data <- rdt.data[which(!is.na(rdt.data$rdt)), ]

filter <- rdt.data$rdt_result == 1
rdt.data1 <- rdt.data[filter, ]

hhtt.data <-  left_join(hhs.data2 , rdt.data1, by = 'record_id')

hhtt.data1 <- hhtt.data[which(!is.na(hhtt.data$rdt)), ]


write.csv(hhtt.data1, file = "HHS_Mal_Positive.csv", row.names = F)

