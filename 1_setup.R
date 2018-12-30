## Install libraries
install.packages("googleAuthR")
install.packages("googleAnalyticsR")
install.packages("tidyverse")

## Load libraries
library("googleAuthR")
library("googleAnalyticsR")
library("tidyverse")

## Authorization with GA GA servers
ga_auth()

## Force new aurization, even if there is a cached token
## ga_auth(new_user = TRUE) 

## Google Analytics Account List
account_list <- google_analytics_account_list()

## Google Analytics View ID 
GA_id <- XXXXXXXX

## GA Query
ga <- google_analytics(GA_id, 
                       date_range = c("2018-01-01", "2018-12-31"),  
                       metrics = c("sessions"), 
                       dimensions = c("date"),
                       anti_sample = TRUE)
