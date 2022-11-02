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

hf_name <- c(sp.data$hf)

sp.data$hf_name <- hf_name

sp.data$hf_name <- factor(sp.data$hf_name,
                              levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
                                         16,17,18,19,20,21,22,23,24,25),
                              labels = c("Binkolo","Kagbere","Kalangba","Kamabai","Kamaranka","Magbenteh","Makoloh","Mara","Tambiama","Malal","Mamankie","Mana II",
                                         "Mange","Minthomore","New Maforkie","SOG","Tagrin","BumbunaC HP","Hinistas","Makonkorie","Masengbeh","Masumbrie","MathampMCHP",
                                         "Rokimbie MCHP","Yele"))


sp.data <- sp.data[,c('record_id','hf','hf_name','report_period_since',
                           'report_period_til','sp_stock_begin','sp_received',
                           'sp_used',
                           'sp_wasted','sp_stock_end',
                           'sp_forecast')]


sp_quantity_check <- c((sp.data$sp_stock_begin +sp.data$sp_received) - (sp.data$sp_used + sp.data$sp_wasted))
sp.data$sp_quantity_check <- sp_quantity_check

spcheck <- c(sp.data$sp_stock_end == sp.data$sp_quantity_check)
sp.data$spcheck <- spcheck

sp_balance_check <- c(sp.data$sp_stock_begin == sp.data$sp_stock_end | sp.data$sp_stock_end  < (sp.data$sp_stock_begin + sp.data$sp_received))
sp.data$sp_balance_check <- sp_balance_check

#Multiply Inconsistency check

filter <-  sp.data$spcheck == 'FALSE'
spcheck.data <- sp.data[filter, ]
spcheck.data <- spcheck.data[which(!is.na(spcheck.data$spcheck)), ] 
spcheck.data <- spcheck.data[,c('record_id','hf','hf_name','report_period_since','sp_stock_begin','sp_used','sp_wasted', 'sp_stock_end','sp_quantity_check','spcheck')]

filter <-  sp.data$sp_balance_check == 'FALSE'
sp_balance_check.data <- sp.data[filter, ]
sp_balance_check.data <- sp_balance_check.data[which(!is.na(sp_balance_check.data$sp_balance_check)), ] 
sp_balance_check.data <- sp_balance_check.data[,c('record_id','hf','hf_name','report_period_since','sp_stock_begin','sp_stock_end','sp_balance_check')]

filter <- sp.data$report_period_since <= last_date_of_period
total_sp.data<-sp.data[filter, ]

filter <- sp.data$report_period_since >= first_date_of_period

period_sp.data <-sp.data[filter,c('record_id','hf','hf_name','report_period_since',
                                  'report_period_til','sp_stock_begin','sp_received',
                                  'sp_used',
                                  'sp_wasted','sp_stock_end',
                                  'sp_forecast')]
total_sp <- nrow(total_sp.data)

period_sp <- nrow(period_sp.data)

write.csv(total_sp.data, file = "Total_sp.csv", row.names = F)

write.csv(period_sp.data, file = "period_sp.csv", row.names = F)
