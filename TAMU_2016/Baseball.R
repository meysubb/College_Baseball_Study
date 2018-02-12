library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(dplyr)
library(plyr)

#setwd("~/OneDrive/Mac_Desktop/Baseball_Scrapper_2016-master/R")
source("NCAA_division.R")
source("Load_Data.R")
#setwd("~/OneDrive/Mac_Desktop/Baseball_Scrapper_2016-master/R")

source("Plate_Discpline.R")
source("Run_Production.R")
source("Advanced_Sabermetrics.R")

### pitching data 
### minimum number of IP 
### 15 IP as a minimum 

TAMU <- subset(player_hitting, team_name == "Texas A&M")
TAMU <- TAMU %>% filter(AB>50)
avg_TAMU_OBPCt <- mean(TAMU$OBPpct)
avg_TAMU_BA <- mean(TAMU$BA)
