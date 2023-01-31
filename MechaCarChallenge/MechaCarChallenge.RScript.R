library(tidyverse)
library(dplyr)
MechaCar <- read.csv(file='/Users/jacob/Desktop/Bootcamp/MechaCar_Statistical_Analysis/MechaCar_mpg.csv', check.names=F, stringsAsFactors = F) #create the MechaCar dataframe
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar_mpg) # generate a multiple linear regression model
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, MechaCar)) #generate summary statistics
Suspension_Coil <- read.csv(file='/Users/jacob/Desktop/Bootcamp/MechaCar_Statistical_Analysis/Suspension_Coil.csv', check.names=F, stringsAsFactors = F) #create the suspension_coil dataframe
total_summary <- Suspension_Coil %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), StDev = sd(PSI)) #create a summary statistics table
lot_summary <- Suspension_Coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), StDev = sd(PSI)) # create a lot summary statistics table
t.test(Suspension_Coil$PSI, mu=1500) #t-test on all manufacturing lots
t.test(subset(Suspension_Coil, Manufacturing_Lot== "Lot1")$PSI, mu=1500) #t-test for lot 1
t.test(subset(Suspension_Coil, Manufacturing_Lot== "Lot2")$PSI, mu=1500) #t-test for lot 2
t.test(subset(Suspension_Coil, Manufacturing_Lot== "Lot3")$PSI, mu=1500) #t-test for lot 3
