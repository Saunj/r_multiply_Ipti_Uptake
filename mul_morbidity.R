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

########################################################################################
hf_name <- c(mobidity.data$hf)

mobidity.data$hf_name<- hf_name

mobidity.data$hf_name<- factor(mobidity.data$hf_name,
                                 levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
                                            16,17,18,19,20,21,22,23,24,25),
                                 labels = c("Binkolo","Kagbere","Kalangba","Kamabai","Kamaranka","Magbenteh","Makoloh","Mara","Tambiama","Malal","Mamankie","Mana II",
                                            "Mange","Minthomore","New Maforkie","SOG","Tagrin","BumbunaC HP","Hinistas","Makonkorie","Masengbeh","Masumbrie","MathampMCHP",
                                            "Rokimbie MCHP","Yele"))
############################################################################



mobidity.data <- mobidity.data[,c('record_id','hf','hf_name','report_period_since',
                                  'report_period_til','opd_u5_total','malaria_total',
                                  'rdt_total','rdt_pos_total','micro_total','micro_pos_total',
                                  'deaths_total','deaths_malaria_total','anaemia_total','referrals_total',
                                  'referrals_malaria_total',
                                  'sam_total','adr_total')]

#checking for inconsistencies
mal_check <- c(mobidity.data$opd_u5_total == mobidity.data$malaria_total |mobidity.data$malaria_total< mobidity.data$opd_u5_total)
mobidity.data$mal_check <- mal_check

rdt_positivecheck <- c(mobidity.data$rdt_total == mobidity.data$rdt_pos_total |mobidity.data$rdt_pos_total < mobidity.data$rdt_total)
mobidity.data$rdt_positivecheck <- rdt_positivecheck

micro_positive_check <- c(mobidity.data$micro_total == mobidity.data$micro_pos_total |mobidity.data$micro_pos_total < mobidity.data$micro_total)
mobidity.data$micro_positive_check <- micro_positive_check

maldeathcheck <- c(mobidity.data$deaths_total == mobidity.data$deaths_malaria_total |mobidity.data$deaths_malaria_total < mobidity.data$deaths_total)
mobidity.data$maldeathcheck <- maldeathcheck

mal_referal_check <- c(mobidity.data$referrals_total==mobidity.data$referrals_malaria_total |mobidity.data$referrals_malaria_total < mobidity.data$referrals_total)
mobidity.data$mal_referal_check <- mal_referal_check

#Filter the inconsistent Total number of Malaria 
filter <-  mobidity.data$mal_check == 'FALSE'
mal_check.data <- mobidity.data[filter, ]
mal_check.data <- mal_check.data[which(!is.na(mal_check.data$mal_check)), ] 
mal_check.data <- mal_check.data[,c('record_id' ,'hf','hf_name','report_period_since','opd_u5_total','malaria_total','mal_check')]

#Filter the inconsistent Total number of rdt_positive check
filter <-  mobidity.data$rdt_positivecheck == 'FALSE'
rdt_positivecheck.data <- mobidity.data[filter, ]
rdt_positivecheck.data <- rdt_positivecheck.data[which(!is.na(rdt_positivecheck.data$rdt_positivecheck)), ] 
rdt_positivecheck.data<- rdt_positivecheck.data[,c('record_id' ,'hf','hf_name','report_period_since','malaria_total','rdt_pos_total','rdt_positivecheck')]

#Filter the inconsistent Total number of micro_positive_check

filter <-  mobidity.data$micro_positive_check == 'FALSE'
micro_positive_check.data <- mobidity.data[filter, ]
micro_positive_check.data <- micro_positive_check.data[which(!is.na(micro_positive_check.data$micro_positive_check)), ] 
micro_positive_check.data <- micro_positive_check.data[,c('record_id' ,'hf','hf_name','report_period_since','malaria_total','micro_total','micro_pos_total','micro_positive_check')]

#Filter the inconsistent Total number of maldeathcheck


filter <-  mobidity.data$maldeathcheck == 'FALSE'
maldeathcheck.data <- mobidity.data[filter, ]
maldeathcheck.data <-maldeathcheck.data[which(!is.na(maldeathcheck.data$maldeathcheck)), ] 
maldeathcheck.data  <- maldeathcheck.data [,c('record_id' ,'hf','hf_name','report_period_since','opd_u5_total','deaths_total','deaths_malaria_total','maldeathcheck')]

#Filter the inconsistent Total number of mal_referal_check
filter <-  mobidity.data$mal_referal_check == 'FALSE'
mal_referal_check.data <- mobidity.data[filter, ]
mal_referal_check.data <- mal_referal_check.data[which(!is.na(mal_referal_check.data$mal_referal_check)), ] 
mal_referal_check.data  <-mal_referal_check.data [,c('record_id' ,'hf','hf_name','report_period_since','opd_u5_total','referrals_total','referrals_malaria_total','mal_referal_check')]

filter <- mobidity.data$report_period_since <= last_date_of_period
total_mobidity.data <-mobidity.data[filter, ]

filter <- mobidity.data$report_period_since >= first_date_of_period

period_mobidity.data <-mobidity.data[filter,c('record_id','hf','hf_name','report_period_since',
                                           'report_period_til','opd_u5_total','malaria_total',
                                           'rdt_total','rdt_pos_total','micro_total','micro_pos_total',
                                           'deaths_total','deaths_malaria_total','anaemia_total','referrals_total',
                                           'referrals_malaria_total',
                                           'sam_total','adr_total')]
total_mobidity <- nrow(total_mobidity.data)

period_mobidity <- nrow(period_mobidity.data)


names(total_mobidity.data) <- c('Record_ID','hf','hf_name','Period_Since','Period_Til','OPD_U5','Suspected_Malaria','RDT_Done','Pos_Malaria',
                          'Microscopy','Pos_Microscopy','Deaths','Mal_Deaths','Anaemia',
                          'Referrals','Mal_Referrals','Severe_Malnutrition','AD_Reaction')

names(period_mobidity.data) <- c('Record_ID','hf','hf_name','Period_Since','Period_Til','OPD_U5','Suspected_Malaria','RDT_Done','Pos_Malaria',
                                'Microscopy','Pos_Microscopy','Deaths','Mal_Deaths','Anaemia',
                                'Referrals','Mal_Referrals','Severe_Malnutrition','AD_Reaction')

#Exporting data extract 
write.csv(total_mobidity.data, file = "Total_Mobidity.csv", row.names = F)
write.csv(period_mobidity.data, file = "period_Mobidity.csv", row.names = F)






