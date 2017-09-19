#Processing Data

library("lattice")  #Graphics for R
library("nutshell") # Contains example data sets

data(births2006.smpl) #load available data sets

#Knowing our data
births2006.smpl[1:5,]
# DOB_MM and DOB_WK - Day of birth according to the month or to the week
# DBWT - Baby weight
# WTGAIN - Weight gain by the mother due to pregnancy
# SEX - Sex of the baby
# APGAR5 - APGAR baby score
# DPLURAL - Single or Multiple bbirth
# ESTGEST - Gestation week estimation

dim(births2006.smpl)

births.dow = table(births2006.smpl$DOB_WK)
barchart(births.dow, ylab="Day of week")

barchart(births2006.smpl$DMETH_REC, ylab="Method of Delivery")


