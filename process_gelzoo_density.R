
#Purpose: read in gelzoo data from RP1

#Author: Kelly Robinson
#Date Created: 22 October 2018
#Date Last Modified: 22 October 2018

#libraries
library(plyr)
library(dplyr)

d <- read.csv("gel_RP1.csv", header = T, stringsAsFactors = F)

d.sum.taxa <- d %>% group_by(taxa) %>% summarise(mean.density = mean(std.density), sd.density = sd(std.density), n = n())
write.csv(x = d.sum.taxa, file = "GelZoo_Taxa_Stats.csv", row.names = F)

d.sum.stn <- d %>% group_by(stn) %>% summarise(mean.density = mean(std.density), sd.density = sd(std.density), n = n())
stn.coord <- unique(d[,c("stn","lat","lon")])
d.sum.stn.coord <- join(d.sum.stn, stn.coord, "stn")
write.csv(x = d.sum.stn.coord, file = "GelZoo_Stn_Stats.csv", row.names = F)


