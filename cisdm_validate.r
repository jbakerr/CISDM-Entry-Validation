# Set Up Environment -----------------------------------------------------------

library(plyr) 
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)


setwd("/Users/baker/Code/Consulting/Thomasville/CISDM_CC_Validate")

# Load Files -------------------------------------------------------------------

# Caselist 

nms <- names(read_excel('caselist.xlsx', n_max = 0))


ct <- ifelse(grepl("Date", nms), "date", "guess")

caselist <- suppressWarnings(
  read_excel('caselist.xlsx', sheet = 1,skip = 1, col_types = ct)
)

colnames(caselist) <- make.names(colnames(caselist))


caselist <- caselist[,c(1:3, 6)]


# Services 

nms <- names(read_excel('services.xlsx', n_max = 0))


ct <- ifelse(grepl("Date", nms), "date", "guess")

services <- suppressWarnings(
  read_excel('services.xlsx', sheet = 1,skip = 1, col_types = ct)
)

colnames(services) <- make.names(colnames(services))

# services <- data.frame(apply(services, 2, function(x) gsub("^$|^ $", NA, x)))



# Drop students that recieve life skills - leadership training from caselist

students_to_drop <- services[services$Student.Support.Name == "Leadership  Training",]$Student.ID

caselist <- caselist[!(caselist$Student.ID %in% students_to_drop),]

# Determine Total Hours Per Student---------------------------------------------


stserv <- services %>%
  group_by(Student.ID) %>% 
  summarize(Hours = sum(Hours),
            num_serv = length(Student.ID), 
            service_date = tail(Support.Date, n =1 )
  )


previous_months_service <- services %>% 
  group_by(Student.ID) %>% 
  filter(month(Support.Date) == month(Sys.Date())-1) %>% 
  summarise(previous_months_service = sum(Hours))

month_serv <- services %>% 
  group_by(Student.ID) %>% 
  summarise(month_serv = length(unique(month(Support.Date))))  

stserv <- merge(stserv, previous_months_service, all = T)
stserv <- merge(stserv, month_serv, all = T)


caselist <- merge(caselist, stserv, all = T, by = "Student.ID")

# Determine Check and Connect Hours per Student --------------------------------

previous_months_cc_service <- services %>% 
  group_by(Student.ID) %>% 
  filter(month(Support.Date) == month(Sys.Date())-1 & (Student.Support.Name == "Attendance Monitoring" 
  | Student.Support.Name == "Classroom Behavior Modification")) %>% 
  summarise(previous_month_cc_services = length(unique(Support.Date)))

caselist <- merge(caselist, previous_months_cc_service, all = T)




# Determine Check and Connect Hours in Last Month per Student ------------------

caselist$caselist_need_cc_services <- ifelse(caselist$previous_month_cc_services <= 3 | is.na(caselist$previous_month_cc_services), T, F)

# Trim File
caselist_short <- caselist[caselist$caselist_need_cc_services == T,]

caselist_short <-  caselist_short[,c(1:4)]

caselist_short <- caselist_short[order(caselist_short$Home.School),]

caselist_short <- caselist_short[caselist_short$Enrollment.Status == "Enrolled",]

write.csv(caselist_short, "CC_CISDM_Validate.csv")
