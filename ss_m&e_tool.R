library(redcapAPI)
library(dplyr) # group_by and mutate functions
library(tidyr) # pivot_wider function
library(lubridate)
library(Hmisc)

first_date_of_period <- "01-09-2022"
last_date_of_period <- "31-10-2022"

first_date_of_period <- as_datetime(first_date_of_period, format = "%d-%m-%Y")
last_date_of_period <- as_datetime(last_date_of_period, format = "%d-%m-%Y")


api.url <- ""

api.token <-  ""

rcon <- redcapConnection(api.url, api.token)

my.fields <- c("record_id","hf_district1","hf_district2","hf_district3","report_period_since",
               "report_period_til","children_weighted","sp_before_antigens","extra_dose","health_education",
               "vita1_ipti","vita2_ipti","mrv2_ipti","outreach","mothly_summary","sp_available",
               "scales_available","water_available","rdt_available","act_available")

my.events <- c("event_1_arm_1")

sstool.data<- exportRecords(
  rcon,
  factors            = F,
  labels             = F,
  fields             = my.fields,
  events             = my.events,
  form_complete_auto = F
)

# Collapse the hf values in just one column
hfs <- c("hf_district1" , "hf_district2" ,"hf_district3")
sstool.data$hf <- rowSums(sstool.data[, hfs], na.rm = T)

hf_name <- c(sstool.data$hf)

sstool.data$hf_name <- hf_name

sstool.data$hf_name <- factor(sstool.data$hf_name,
                              levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
                                         16,17,18,19,20,21,22,23,24,25),
                              labels = c("Binkolo","Kagbere","Kalangba","Kamabai","Kamaranka","Magbenteh","Makoloh","Mara","Tambiama","Malal","Mamankie","Mana II",
                                         "Mange","Minthomore","New Maforkie","SOG","Tagrin","BumbunaC HP","Hinistas","Makonkorie","Masengbeh","Masumbrie","MathampMCHP",
                                         "Rokimbie MCHP","Yele"))

sstool.data <- sstool.data[,c("record_id","hf","hf_name","report_period_since",
                              "report_period_til","children_weighted","sp_before_antigens","extra_dose","health_education",
                              "vita1_ipti","vita2_ipti","mrv2_ipti","outreach","mothly_summary","sp_available",
                              "scales_available","water_available","rdt_available","act_available")]


sstool.data$children_weighted <- factor(sstool.data$children_weighted,
                    levels = c(1,0),
                    labels = c("Yes" ,"No"))

sstool.data$sp_before_antigens <- factor(sstool.data$sp_before_antigens,
                                        levels = c(1,0),
                                        labels = c("Yes" ,"No"))

sstool.data$extra_dose <- factor(sstool.data$extra_dose,
                                         levels = c(1,0,66),
                                         labels = c("Yes" ,"No","Not Applicable"))

sstool.data$health_education <- factor(sstool.data$health_education,
                                 levels = c(1,0),
                                 labels = c("Yes" ,"No"))

sstool.data$vita1_ipti <- factor(sstool.data$vita1_ipti,
                                       levels = c(1,0,66),
                                       labels = c("Yes" ,"No","Not Applicable"))

sstool.data$vita2_ipti <- factor(sstool.data$vita2_ipti,
                                 levels = c(1,0,66),
                                 labels = c("Yes" ,"No","Not Applicable"))

sstool.data$mrv2_ipti <- factor(sstool.data$mrv2_ipti,
                                 levels = c(1,0,66),
                                 labels = c("Yes" ,"No","Not Applicable"))

sstool.data$outreach <- factor(sstool.data$outreach,
                                levels = c(1,0,66),
                                labels = c("Yes" ,"No","Not Applicable"))


sstool.data$mothly_summary <- factor(sstool.data$mothly_summary,
                               levels = c(1,0),
                               labels = c("Yes","No"))

sstool.data$sp_available <- factor(sstool.data$sp_available,
                                     levels = c(1,0),
                                     labels = c("Yes","No"))

sstool.data$scales_available <- factor(sstool.data$scales_available,
                                   levels = c(1,0),
                                   labels = c("Yes","No"))


sstool.data$water_available <- factor(sstool.data$water_available,
                                       levels = c(1,0),
                                       labels = c("Yes","No"))

sstool.data$rdt_available <- factor(sstool.data$rdt_available,
                                      levels = c(1,0),
                                      labels = c("Yes","No"))

sstool.data$act_available <- factor(sstool.data$act_available,
                                    levels = c(1,0),
                                    labels = c("Yes","No"))



filter <- sstool.data$report_period_since <= last_date_of_period
total_sstool.data  <-sstool.data [filter, ]

filter <- sstool.data$report_period_since >= first_date_of_period

period_sstool.data <-sstool.data [filter,c("record_id","hf","hf_name","report_period_since",
                                           "report_period_til","children_weighted","sp_before_antigens","extra_dose","health_education",
                                           "vita1_ipti","vita2_ipti","mrv2_ipti","outreach","mothly_summary","sp_available",
                                           "scales_available","water_available","rdt_available","act_available")]
