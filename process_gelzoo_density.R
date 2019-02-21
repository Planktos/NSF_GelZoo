
#Purpose: read in gelzoo data from RP1

#Author: Kelly Robinson
#Date Created: 22 October 2018
#Date Last Modified: 22 October 2018

#libraries
library(plyr)
library(dplyr)

#read in and combine group density files
d1 <- read.csv("gel_RP1.csv", header = T, stringsAsFactors = F)
d1$cruise <- "RP1"

d2 <- read.csv("rapid_jellies.csv", header = T, stringsAsFactors = F)
d2 <- d2[,-1]
d2$cruise <- toupper(substr(d2$sample_id, 1,3))

d <- rbind(d1, d2)

taxa.n <- unique(d$taxa)
d$taxa<- gsub(d$taxa, pattern = "Ctenophora<Metazoa", replacement = "ctenophore")
d$taxa<- gsub(d$taxa, pattern = "Ctenophora", replacement = "ctenophore")
d$taxa <- tolower(d$taxa)

d.sum.taxa <- d %>% group_by(cruise, taxa) %>% summarise(mean.density = mean(std.density), sd.density = sd(std.density), n = n())
write.csv(x = d.sum.taxa, file = "GelZoo_Taxa_Stats.csv", row.names = F)

d.sum.stn <- d %>% group_by(stn) %>% summarise(mean.density = mean(std.density), sd.density = sd(std.density), n = n())
stn.coord <- unique(d[,c("stn","lat","lon")])
d.sum.stn.coord <- join(d.sum.stn, stn.coord, "stn")
write.csv(x = d.sum.stn.coord, file = "GelZoo_Stn_Stats.csv", row.names = F)


