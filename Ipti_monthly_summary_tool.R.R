library(redcapAPI)
library(dplyr) # group_by and mutate functions
library(tidyr) # pivot_wider function
library(lubridate)
library(Hmisc)

first_date_of_period <- "01-08-2022"
last_date_of_period <- "30-09-2022"

first_date_of_period <- as_datetime(first_date_of_period, format = "%d-%m-%Y")
last_date_of_period <- as_datetime(last_date_of_period, format = "%d-%m-%Y")


api.url <- ""

api.token <-  ""

rcon <- redcapConnection(api.url, api.token)

my.fields <- c("record_id","hf_district1" ,
               "hf_district2","hf_district3","report_period_since",
               "report_period_til",'ipti1_fixed','ipti1_outreach',
               'ipti2_fixed','ipti2_outreach','ipti3_fixed','ipti3_outreach',
               'iptim6_fixed','iptim6_outreach','iptim12_fixed','iptim12_outreach',
               'iptim15_fixed','iptim15_outreach')

my.events <- c("event_1_arm_1")

iptiuptake.data<- exportRecords(
  rcon,
  factors            = F,
  labels             = F,
  fields             = my.fields,
  events             = my.events,
  form_complete_auto = F
)

# Collapse the hf values in just one column
hfs <- c("hf_district1" , "hf_district2" ,"hf_district3")
iptiuptake.data$hf <- rowSums(iptiuptake.data[, hfs], na.rm = T)
########################################################################################
hf_name <- c(iptiuptake.data$hf)

iptiuptake.data$hf_name <- hf_name

iptiuptake.data$hf_name<- factor(iptiuptake.data$hf_name,
                                 levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
                                            16,17,18,19,20,21,22,23,24,25),
                                 labels = c("Binkolo","Kagbere","Kalangba","Kamabai","Kamaranka","Magbenteh","Makoloh","Mara","Tambiama","Malal","Mamankie","Mana II",
                                            "Mange","Minthomore","New Maforkie","SOG","Tagrin","BumbunaC HP","Hinistas","Makonkorie","Masengbeh","Masumbrie","MathampMCHP",
                                            "Rokimbie MCHP","Yele"))
############################################################################

#Combine ipti1 column of both fixed and outreach
tipti1 <- c("ipti1_fixed","ipti1_outreach" )  
iptiuptake.data$ipti1<- rowSums(iptiuptake.data[, tipti1], na.rm = T)

#Combine ipti2 column of both fixed and outreach
tipti2 <- c("ipti2_fixed","ipti2_outreach" )
iptiuptake.data$ipti2<- rowSums(iptiuptake.data[, tipti2], na.rm = T)

#Combine ipti3 column of both fixed and outreach
tipti3 <- c("ipti3_fixed","ipti3_outreach" )
iptiuptake.data$ipti3<- rowSums(iptiuptake.data[, tipti3], na.rm = T)

#Combine ipti-m6 column of both fixed and outreach
tiptim6 <- c("iptim6_fixed","iptim6_outreach" )
iptiuptake.data$iptim6<- rowSums(iptiuptake.data[, tiptim6], na.rm = T)

#Combine ipti-m12 column of both fixed and outreach
tiptim12 <- c("iptim12_fixed","iptim12_outreach" )
iptiuptake.data$iptim12<- rowSums(iptiuptake.data[, tiptim12], na.rm = T)


#Combine ipti-m15 column of both fixed and outreach
tiptim15 <- c("iptim15_fixed","iptim15_outreach" )
iptiuptake.data$iptim15<- rowSums(iptiuptake.data[, tiptim15], na.rm = T)


print("Processing Ipti_uptake data")
filter <- !is.na(iptiuptake.data$report_period_since)

iptiuptake <- iptiuptake.data[filter, c('record_id','hf','hf_name','report_period_since',
                                        'ipti1','ipti2','ipti3','iptim6','iptim12','iptim15')]

filter <- iptiuptake$report_period_since <= last_date_of_period
total_uptake.data <-iptiuptake[filter, ]

filter <- iptiuptake$report_period_since >= first_date_of_period

period_uptake.data <-iptiuptake[filter,c('record_id','hf','hf_name','report_period_since',
                                    'ipti1','ipti2','ipti3','iptim6','iptim12','iptim15')]

total_uptake <- nrow(total_uptake.data)

period_uptake <- nrow(period_uptake.data) 

write.csv(total_uptake.data, file = "Total_Iptiuptake.csv", row.names = F)

write.csv(period_uptake.data, file = "Period_Iptiuptake.csv", row.names = F)

