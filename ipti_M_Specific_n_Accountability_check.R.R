library(arsenal)
library(daff)
library(tidyverse)
library(tidyr) 
library(lubridate)
first_date_of_period <- "01-08-2022"
last_date_of_period <- "30-09-2022"

first_date_of_period <- as_datetime(first_date_of_period, format = "%d-%m-%Y")
last_date_of_period <- as_datetime(last_date_of_period, format = "%d-%m-%Y")

routine_ipti.data <- read.csv("Total_Iptiuptake.csv", header =T)
Account_ipti.data <- read.csv("Total_Accountability_Iptiuptake.csv", header =T)
multi_speci_ipti.data <- read.csv("Total_mul_iptispecific.csv", header =T)


Account_ipti.data$id <- paste(Account_ipti.data$hf, Account_ipti.data$report_period_since, sep = "_") 

multi_speci_ipti.data $id <- paste(multi_speci_ipti.data $hf, multi_speci_ipti.data $report_period_since, sep = "_")

ipti.data <- left_join(Account_ipti.data ,multi_speci_ipti.data , by ='id' )


ipti_1_comment <- c(ipti.data$ipti1.x == ipti.data$ipti1.y |ipti.data$ipti1.y < ipti.data$ipti1.x )

ipti.data$ipti_1_comment <- ipti_1_comment


ipti_2_comment <- c(ipti.data$ipti2.x == ipti.data$ipti2.y |ipti.data$ipti2.y < ipti.data$ipti2.x )

ipti.data$ipti_2_comment <- ipti_2_comment


ipti_3_comment <- c(ipti.data$ipti3.x == ipti.data$ipti3.y |ipti.data$ipti3.y < ipti.data$ipti3.x )

ipti.data$ipti_3_comment <- ipti_3_comment



ipti_m6_comment <- c(ipti.data$iptim6.x == ipti.data$iptim6.y |ipti.data$iptim6.y < ipti.data$iptim6.x )
ipti.data$ipti_m6_comment <- ipti_m6_comment

ipti_m12_comment <- c(ipti.data$iptim12.x == ipti.data$iptim12.y |ipti.data$iptim12.y < ipti.data$iptim12.x )
ipti.data$ipti_m12_comment <- ipti_m12_comment

ipti_m15_comment <- c(ipti.data$iptim15.x == ipti.data$iptim15.y |ipti.data$iptim15.y < ipti.data$iptim15.x )
ipti.data$ipti_m15_comment <- ipti_m15_comment



ipti_consis.data <- ipti.data[, c('id','record_id.x' , 'hf.y','hf_name.x', 'report_period_since.y','ipti_1_comment','ipti_2_comment',
                                  'ipti_3_comment','ipti_m6_comment','ipti_m12_comment', 'ipti_m15_comment','ipti1.x','ipti1.y','ipti2.x',
                                  'ipti2.y','ipti3.x','ipti3.y','iptim6.x','iptim6.y','iptim12.x','iptim12.y','iptim15.x','iptim15.y')]

                                  
#Filter the inconsistent Ipti for Ipti 1                                  
filter <-  ipti_consis.data$ipti_1_comment == 'FALSE'
                                                                                                   
Ipti_1_inconsistency.data <- ipti_consis.data[filter, ]
Ipti_1_inconsistency.data <- Ipti_1_inconsistency.data[which(!is.na(Ipti_1_inconsistency.data$ipti_1_comment)), ] 

Ipti_1_inconsistency.data <- Ipti_1_inconsistency.data[,c('id','record_id.x' , 'hf.y','hf_name.x', 'report_period_since.y','ipti1.x','ipti1.y','ipti_1_comment')]

#Filter the inconsistent Ipti for Ipti 2

filter <-  ipti_consis.data$ipti_2_comment == 'FALSE'

Ipti_2_inconsistency.data <- ipti_consis.data[filter, ]
Ipti_2_inconsistency.data <- Ipti_2_inconsistency.data[which(!is.na(Ipti_2_inconsistency.data$ipti_2_comment)), ] 

Ipti_2_inconsistency.data <- Ipti_2_inconsistency.data[,c('id','record_id.x' , 'hf.y','hf_name.x', 'report_period_since.y','ipti2.x','ipti2.y','ipti_2_comment')]

#Filter the inconsistent Ipti for Ipti 3

filter <-  ipti_consis.data$ipti_3_comment == 'FALSE'

Ipti_3_inconsistency.data <- ipti_consis.data[filter, ]
Ipti_3_inconsistency.data <- Ipti_3_inconsistency.data[which(!is.na(Ipti_3_inconsistency.data$ipti_3_comment)), ] 

Ipti_3_inconsistency.data <- Ipti_3_inconsistency.data[,c('id','record_id.x' , 'hf.y','hf_name.x', 'report_period_since.y','ipti3.x','ipti3.y','ipti_3_comment')]


#Filter the inconsistent Ipti for Ipti m6
filter <-  ipti_consis.data$ipti_m6_comment == 'FALSE'
Ipti_m6_inconsistency.data <- ipti_consis.data[filter, ]
Ipti_m6_inconsistency.data <- Ipti_m6_inconsistency.data[which(!is.na(Ipti_m6_inconsistency.data$ipti_m6_comment)), ] 

Ipti_m6_inconsistency.data <- Ipti_m6_inconsistency.data[,c('id','record_id.x' , 'hf.y','hf_name.x', 'report_period_since.y','iptim6.x','iptim6.y','ipti_m6_comment')]

#Filter the inconsistent Ipti for Ipti m12
filter <-  ipti_consis.data$ipti_m12_comment == 'FALSE'
Ipti_m12_inconsistency.data <- ipti_consis.data[filter, ]
Ipti_m12_inconsistency.data <- Ipti_m12_inconsistency.data[which(!is.na(Ipti_m12_inconsistency.data$ipti_m6_comment)), ] 
Ipti_m12_inconsistency.data <- Ipti_m12_inconsistency.data[,c('id','record_id.x' , 'hf.y','hf_name.x', 'report_period_since.y','iptim12.x','iptim12.y','ipti_m12_comment')]

#Filter the inconsistent Ipti for Ipti m15
filter <-  ipti_consis.data$ipti_m15_comment == 'FALSE'
Ipti_m15_inconsistency.data <- ipti_consis.data[filter, ]
Ipti_m15_inconsistency.data <- Ipti_m15_inconsistency.data[which(!is.na(Ipti_m15_inconsistency.data$ipti_m6_comment)), ] 
Ipti_m15_inconsistency.data <- Ipti_m15_inconsistency.data[,c('id','record_id.x' , 'hf.y','hf_name.x', 'report_period_since.y','iptim15.x','iptim15.y','ipti_m15_comment')]

write.csv(Ipti_1_inconsistency.data, file = "Ipti_1_Discrepancy_Check.csv", row.names = F)

write.csv(Ipti_2_inconsistency.data, file = "Ipti_2_Discrepancy_Check.csv", row.names = F)

write.csv(Ipti_m6_inconsistency.data, file = "Ipti_m6_Discrepancy_Check.csv", row.names = F)

           