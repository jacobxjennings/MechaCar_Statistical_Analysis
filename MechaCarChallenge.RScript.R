library(tidyverse)
library(dplyr)
MechaCar <- read.csv(file='/Users/jacob/Desktop/Bootcamp/MechaCar_Statistical_Analysis/MechaCar_mpg.csv', check.names=F, stringsAsFactors = F) 
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar_mpg) 
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, MechaCar)) 
Suspension_Coil <- read.csv(file='/Users/jacob/Desktop/Bootcamp/MechaCar_Statistical_Analysis/Suspension_Coil.csv', check.names=F, stringsAsFactors = F) 
total_summary <- Suspension_Coil %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), StDev = sd(PSI)) 
lot_summary <- Suspension_Coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), StDev = sd(PSI)) 
t.test(Suspension_Coil$PSI, mu=1500) #t-test on all manufacturing lots
t.test(subset(Suspension_Coil, Manufacturing_Lot== "Lot1")$PSI, mu=1500) #t-test for lot 1
t.test(subset(Suspension_Coil, Manufacturing_Lot== "Lot2")$PSI, mu=1500) #t-test for lot 2
t.test(subset(Suspension_Coil, Manufacturing_Lot== "Lot3")$PSI, mu=1500) #t-test for lot 3
