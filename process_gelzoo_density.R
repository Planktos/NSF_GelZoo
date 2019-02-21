
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

d$taxa<- gsub(d$taxa, pattern = "Ctenophora<Metazoa", replacement = "ctenophore")
d$taxa<- gsub(d$taxa, pattern = "Ctenophora", replacement = "ctenophore")
d$taxa <- tolower(d$taxa)

#find missing groups and add them back in as zeros
taxa.n <- as.data.frame(unique(d$taxa))
colnames(taxa.n) <- "taxa"

df <- ddply(d, .variables = c("cruise","stn","sample_id"), function(n){
t <- merge(x = n, y = taxa.n, by = "taxa", all.y = T)
t$cruise <- unique(n$cruise)
t$stn <- unique(n$stn)
t$sample_id <- unique(n$sample_id)
t$lat <- unique(n$lat)
t$lon <- unique(n$lon)
t$std.density <- ifelse(is.na(t$std.density), 0, t$std.density)
t$count <- ifelse(is.na(t$count), 0, t$count)

return(t)

}, .progress = "text", .inform = T)

m <- merge(x = t, y = n1, by = c("taxa","cruise","stn"), all.x = T)


d.sum.taxa <- d %>% group_by(cruise, taxa) %>% summarise(mean.density = mean(std.density), sd.density = sd(std.density), n = n())
write.csv(x = d.sum.taxa, file = "GelZoo_Taxa_Stats.csv", row.names = F)

d.sum.stn <- d %>% group_by(stn) %>% summarise(mean.density = mean(std.density), sd.density = sd(std.density), n = n())
stn.coord <- unique(d[,c("stn","lat","lon")])
d.sum.stn.coord <- join(d.sum.stn, stn.coord, "stn")
write.csv(x = d.sum.stn.coord, file = "GelZoo_Stn_Stats.csv", row.names = F)


