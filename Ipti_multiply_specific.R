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

my.fields <- c('record_id','hf_district1','hf_district2','hf_district3','report_period_since',
               'report_period_til','ipti1_fixed','ipti1_outreach','rota2_fixed',
               'rota2_outreach','penta2_fixed','penta2_outreach','penta3_fixed',
               'penta3_outreach','ipv1_fixed','ipv1_outreach','ipv2_fixed','ipv2_outreach',
               'pcv2_fixed','pcv2_outreach','pcv3_fixed','pcv3_outreach','opv2_fixed','opv2_outreach',
               'opv3_fixed','opv3_outreach','opv3_outreach','mrv1_fixed','mrv1_outreach',
               'mrv2_fixed','mrv2_outreach','yf_fixed','yf_outreach','vita1_fixed','vita1_outreach',
               'vita2_fixed','vita2_outreach','ipti2_fixed','ipti2_outreach','ipti3_fixed','ipti3_outreach',
               'iptim6_fixed','iptim6_outreach','iptim12_fixed','iptim12_outreach',
               'iptim15_fixed','iptim15_outreach','ipti1_penta2_fixed','ipti1_penta2_outreach','ipti2_penta3_fixed',
               'ipti2_penta3_outreach','iptim6_vita1_fixed','iptim6_vita1_outreach','ipti3_mrv1_fixed','ipti3_mrv1_outreach',
               'iptim12_vita2_fixed','iptim12_vita2_outreach','iptim15_mrv2_fixed','iptim15_mrv2_outreach')

my.events <- c("event_1_arm_1")

data<- exportRecords(
  rcon,
  factors            = F,
  labels             = F,
  fields             = my.fields,
  events             = my.events,
  form_complete_auto = F
)



#Collapse the hf values in just one column

hfs <- c("hf_district1" , "hf_district2" ,"hf_district3")
data$hf <- rowSums(data[, hfs], na.rm = T)
hf_name <- c(data$hf)

data$hf_name <- hf_name

data$hf_name  <- factor(data$hf_name ,
                              levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
                                         16,17,18,19,20,21,22,23,24,25),
                              labels = c("Binkolo","Kagbere","Kalangba","Kamabai","Kamaranka","Magbenteh","Makoloh","Mara","Tambiama","Malal","Mamankie","Mana II",
                                         "Mange","Minthomore","New Maforkie","SOG","Tagrin","BumbunaC HP","Hinistas","Makonkorie","Masengbeh","Masumbrie","MathampMCHP",
                                         "Rokimbie MCHP","Yele"))
#Antigens

#Combine antigens column of both fixed and outreach
rota2s <- c("rota2_fixed" , "rota2_outreach")
data$rota2 <- rowSums(data[, rota2s], na.rm = T)

penta2s <- c("penta2_fixed" , "penta2_outreach")
data$penta2 <- rowSums (data [, penta2s], na.rm = T)

penta3s <- c("penta3_fixed" , "penta3_outreach")
data$penta3 <- rowSums(data [,penta3s] , na.rm = T)

ipv1s <- c("ipv1_fixed" , "ipv1_outreach")
data$ipv1 <- rowSums(data [, ipv1s] , na.rm = T)

ipv2s <- c("ipv2_fixed" , "ipv2_outreach")
data$ipv2 <- rowSums(data [, ipv2s], na.rm = T)

pcv3s <- c("pcv3_fixed" , "pcv3_outreach")
data$pcv3 <- rowSums(data [,pcv3s], na.rm = T)

opv2s <- c("opv2_fixed" , "opv2_outreach")
data$opv2 <- rowSums(data [,opv2s ], na.rm = T)

opv3s <- c("opv3_fixed" , "opv3_outreach")
data$opv3 <- rowSums(data [, opv3s], na.rm = T)

mrv1s <- c("mrv1_fixed" , "mrv1_outreach")
data$mrv1 <- rowSums(data [,mrv1s], na.rm = T)

mrv2s <- c("mrv2_fixed" , "mrv2_outreach")
data$mrv2 <- rowSums(data [,mrv2s], na.rm = T)

yfs <- c("yf_fixed" , "yf_outreach")
data$yf <- rowSums(data[,yfs], na.rm = T )

vita1s <- c("vita1_fixed" , "vita1_outreach" )
data$vita1 <- rowSums(data [,vita1s] , na.rm = T)

vita2s <- c("vita2_fixed" , "vita2_outreach")
data$vita2 <- rowSums(data [,vita2s] , na.rm = T)

data1 <- data[c('record_id','hf','hf_name','report_period_since','rota2',
                      'penta2','penta3','ipv1','ipv2','pcv3','opv2',
                      'opv3','mrv1','mrv2','yf','vita1','vita2')]

filter <- data1$report_period_since <= last_date_of_period
total_mul_speci_antigen.data <-data1[filter, ]

filter <- data1$report_period_since >= first_date_of_period

period_mul_speci_antigen.data <- data1[filter ,c('record_id','hf','hf_name','report_period_since','rota2',
                                                'penta2','penta3','ipv1','ipv2','pcv3','opv2',
                                                'opv3','mrv1','mrv2','yf','vita1','vita2')]
#IPTi-SP

#Combine ipti1 column of both fixed and outreach
tipti1 <- c("ipti1_fixed","ipti1_outreach" )  
data$ipti1<- rowSums(data[, tipti1], na.rm = T)

#Combine ipti2 column of both fixed and outreach
tipti2 <- c("ipti2_fixed","ipti2_outreach" )
data$ipti2<- rowSums(data[, tipti2], na.rm = T)

#Combine ipti3 column of both fixed and outreach
tipti3 <- c("ipti3_fixed","ipti3_outreach" )
data$ipti3<- rowSums(data[, tipti3], na.rm = T)

#Combine ipti-m6 column of both fixed and outreach
tiptim6 <- c("iptim6_fixed","iptim6_outreach" )
data$iptim6<- rowSums(data[, tiptim6], na.rm = T)

#Combine ipti-m12 column of both fixed and outreach
tiptim12 <- c("iptim12_fixed","iptim12_outreach" )
data$iptim12<- rowSums(data[, tiptim12], na.rm = T)


#Combine ipti-m15 column of both fixed and outreach
tiptim15 <- c("iptim15_fixed","iptim15_outreach" )
data$iptim15<- rowSums(data[, tiptim15], na.rm = T)

iptiuptake<- data[c('record_id','hf','hf_name','report_period_since','ipti1','ipti2','ipti3','iptim6','iptim12','iptim15')]

filter <- iptiuptake$report_period_since <= last_date_of_period
total_mul_iptispecific <-iptiuptake[filter, ]

filter <- iptiuptake$report_period_since >= first_date_of_period

period_mul_iptispecific <-iptiuptake[filter ,c('record_id','hf','hf_name','report_period_since','ipti1','ipti2','ipti3','iptim6','iptim12','iptim15')]

#IPTi & Antigen

ipti1_penta2s <- c("ipti1_penta2_fixed" , "ipti1_penta2_outreach")
data$ipti1_penta2 <- rowSums(data[,ipti1_penta2s], na.rm = T)

ipti2_penta3s <- c("ipti2_penta3_fixed" , "ipti2_penta3_outreach")
data$ipti2_penta3 <- rowSums(data[,ipti2_penta3s], na.rm = T)

iptim6_vita1s <- c("iptim6_vita1_fixed" , "iptim6_vita1_outreach")
data$iptim6_vita1 <- rowSums(data [,iptim6_vita1s], na.rm = T)

ipti3_mrv1s <- c("ipti3_mrv1_fixed" ,"ipti3_mrv1_outreach")
data$ipti3_mrv1 <- rowSums(data[,ipti3_mrv1s ] , na.rm = T)

iptim12_vita2s <- c("iptim12_vita2_fixed" ,"iptim12_vita2_outreach")
data$iptim12_vita2 <- rowSums(data[,iptim12_vita2s ], na.rm = T)

iptim15_mrv2S <- c("iptim15_mrv2_fixed" , "iptim15_mrv2_outreach")
data$iptim15_mrv2 <- rowSums(data[,iptim15_mrv2S ], na.rm = T)

ipti_antigen <- data[c('record_id','hf','hf_name','report_period_since','ipti1_penta2',
                       'ipti2_penta3','iptim6_vita1','ipti3_mrv1','iptim12_vita2',
                       'iptim15_mrv2')]

filter <- ipti_antigen$report_period_since <= last_date_of_period
total_ipti_antigen.data <-ipti_antigen[filter, ]

filter <- ipti_antigen$report_period_since >= first_date_of_period

period_ipti_antigen <-ipti_antigen[filter ,c('record_id','hf','hf_name','report_period_since','ipti1_penta2',
                       'ipti2_penta3','iptim6_vita1','ipti3_mrv1','iptim12_vita2',
                       'iptim15_mrv2')]

#IPTi Fixed VS Outreach

fix_n_outreach.data <- data[, c('record_id','hf','hf_name','report_period_since','ipti1_fixed','ipti1_outreach','ipti2_fixed','ipti2_outreach',
                                'ipti3_fixed','ipti3_outreach','iptim6_fixed','iptim6_outreach','iptim12_fixed','iptim12_outreach', 
                                'iptim15_fixed','iptim15_outreach')]

filter <- fix_n_outreach.data$report_period_since <= last_date_of_period
total_fix_n_outreach.data <-fix_n_outreach.data[filter, ]

filter <- fix_n_outreach.data$report_period_since >= first_date_of_period

period_fix_n_outreach.data <- fix_n_outreach.data[filter ,c('record_id','hf','hf_name','report_period_since','ipti1_fixed','ipti1_outreach','ipti2_fixed','ipti2_outreach',
                                                            'ipti3_fixed','ipti3_outreach','iptim6_fixed','iptim6_outreach','iptim12_fixed','iptim12_outreach', 
                                                            'iptim15_fixed','iptim15_outreach')]


#checking for inconsistencies
ipti_1_check <- c(data$rota2 == data$penta2 & data$opv2 == data$ipti1 & data$ipti1_penta2 == data$penta2)
data$ipti_1_check <-ipti_1_check

ipti_2_check <- c(data$ipti2 == data$opv3 & data$ipv1 == data$penta3 & data$pcv3 == data$ipti2_penta3)
data$ipti_2_check <- ipti_2_check

ipti_m6_check <- c(data$iptim6 == data$vita1 & data$iptim6_vita1 == data$iptim6)
data$ipti_m6_check <- ipti_m6_check

ipti_3_check <- c(data$ipti3 == data$mrv1 & data$yf == data$ipti3_mrv1 & data$ipti3 ==data$mrv1)
data$ipti_3_check <- ipti_3_check


ipti_m12_check <- c(data$iptim12 == data$vita2 & data$iptim12_vita2 ==data$iptim12)
data$ipti_m12_check <- ipti_m12_check

ipti_m15_check <- c(data$iptim15 == data$mrv2 & data$iptim15_mrv2 ==data$iptim15)
data$ipti_m15_check <- ipti_m15_check


#Filter the inconsistent Total number ofipti_1_check
filter <-  data$ipti_1_check == 'FALSE'
ipti_1_check.data <- data[filter, ]
ipti_1_check.data <- ipti_1_check.data[which(!is.na(ipti_1_check.data$ipti_1_check)), ] 
ipti_1_check.data <- ipti_1_check.data [,c('record_id' ,'hf','hf_name','report_period_since','ipti1','penta2','rota2','opv2','ipti1_penta2','ipti_1_check')]
write.csv(ipti_1_check.data, file = "Multiply_Ipti1_Specific_Data_Queries.csv", row.names = F)

#Filter the inconsistent Total number of ipti_2_check
filter <-  data$ipti_2_check == 'FALSE'
ipti_2_check.data <- data[filter, ]
ipti_2_check.data <- ipti_2_check.data[which(!is.na(ipti_2_check.data$ipti_2_check)), ]
ipti_2_check.data <- ipti_2_check.data [,c('record_id' ,'hf','hf_name','report_period_since','ipti2','penta3','ipv1','opv3','ipti2_penta3','ipti_2_check')]
write.csv(ipti_2_check.data, file = "Multiply_Ipti2_Specific_Data_Queries.csv", row.names = F)

#Filter the inconsistent Total number of ipti_m6_check
filter <-  data$ipti_m6_check == 'FALSE'
ipti_m6_check.data <- data[filter, ]
ipti_m6_check.data <- ipti_m6_check.data[which(!is.na(ipti_m6_check.data$ipti_m6_check)), ]
ipti_m6_check.data <- ipti_m6_check.data[,c('record_id' ,'hf','hf_name','report_period_since','iptim6','vita1','iptim6_vita1','ipti_m6_check')]
write.csv(ipti_m6_check.data, file = "Multiply_Iptim6_Specific_Data_Queries.csv", row.names = F)

#Filter the inconsistent Total number of ipti_3_check
filter <-  data$ipti_3_check == 'FALSE'
ipti_3_check.data <- data[filter, ]
ipti_3_check.data <- ipti_3_check.data[which(!is.na(ipti_3_check.data$ipti_3_check)), ]
ipti_3_check.data <- ipti_3_check.data[,c('record_id' ,'hf','hf_name','report_period_since','ipti3',
                                          'mrv1','yf','ipti3_mrv1','ipti_3_check')]

write.csv(ipti_3_check.data , file = "Multiply_Ipti3_Specific_Data_Queries.csv", row.names = F)

#Filter the inconsistent Total number of ipti_m12_check
filter <-  data$ipti_m12_check == 'FALSE'
ipti_m12_check.data <- data[filter, ]
ipti_m12_check.data <- ipti_m12_check.data[which(!is.na(ipti_m12_check.data$ipti_m12_check)), ]
ipti_m12_check.data <- ipti_m12_check.data[,c('record_id' ,'hf','hf_name','report_period_since','iptim12','vita2','iptim12_vita2','ipti_m12_check')]
write.csv(ipti_m12_check.data , file = "Multiply_Iptim12_Specific_Data_Queries.csv", row.names = F)

#Filter the inconsistent Total number of ipti_m15_check
filter <-  data$ipti_m15_check == 'FALSE'
ipti_m15_check.data <- data[filter, ]
ipti_m15_check.data <- ipti_m15_check.data[which(!is.na(ipti_m15_check.data$ipti_m15_check)), ]
ipti_m15_check.data <- ipti_m15_check.data[,c('record_id' ,'hf','hf_name','report_period_since','iptim15','mrv2','iptim15_mrv2','ipti_m15_check')]
write.csv(ipti_m15_check.data , file = "Multiply_Iptim15_Specific_Data_Queries.csv", row.names = F)

#Exporting data extract for both periodic and total dataset for the reporting period

write.csv(total_mul_speci_antigen.data, file = "Total_mul_speci_antigen.csv", row.names = F)

write.csv(total_mul_iptispecific,file = "Total_mul_iptispecific.csv", row.names = F)

write.csv(total_ipti_antigen.data , file = "Total_ipti_antigen.csv", row.names = F)



 










 


