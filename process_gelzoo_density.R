
#Purpose: read in gelzoo data from RP1

#Author: Kelly Robinson
#Date Created: 22 October 2018
#Date Last Modified: 22 October 2018

#libraries
library(plyr)
library(dplyr)
library(ggplot2)

#read in and combine group density files
d1 <- read.csv("gel_RP1.csv", header = T, stringsAsFactors = F)
d1$cruise <- "RP1"

d2 <- read.csv("rapid_jellies.csv", header = T, stringsAsFactors = F)
d2 <- d2[,-1]
d2$cruise <- toupper(substr(d2$sample_id, 1,3))

d <- rbind(d1, d2)
d <- d %>% distinct() #remove duplitcated records for RP3 in the original "rapid_jellies.csv" file Zach sent

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
t$date <- ifelse(is.na(t$date), unique(n$date), t$date)

return(t)

}, .progress = "text", .inform = T)

stn <- unique(df$stn)
write.csv(stn, file = "RAPID_stn.csv", row.names = F)
stn.info <- read.csv(file = "RAPID_stn_info.csv", stringsAsFactors = F)
df <- merge(df, stn.info, "stn")

write.csv(df, file = "RAPID_jellies_all.csv", row.names = F)

d.sum.taxa <- df %>% group_by(date, cruise, taxa) %>% summarise(mean.density = mean(std.density), sd.density = sd(std.density), n = n())
write.csv(x = d.sum.taxa, file = "GelZoo_Taxa_Stats.csv", row.names = F)

dp <- ddply(d.sum.taxa, .variables = c("cruise","date"), function(n){
  t <- merge(x = n, y = taxa.n, by = "taxa", all.y = T)
  t$cruise <- unique(n$cruise)
  t$mean.density <- ifelse(is.na(t$mean.density), 0, t$mean.density)
  t$sd.density <- ifelse(is.na(t$sd.density   ), 0, t$sd.density)
  t$n <- ifelse(is.na(t$n   ), 0, t$n)
  return(t)

}, .progress = "text", .inform = T)
dp$se <- dp$sd.density/sqrt(dp$n)
dp$cruise_date <- factor(dp$date, levels = c("Oct-2017","Jan-2018","Mar-2018"), labels = c("Oct-2017","Jan-2018","Mar-2018"))

colors <- c("Oct-2017"= "royalblue3", "Jan-2018" = "limegreen", "Mar-2018" ="tomato3")

b <- ggplot(dp, aes(x = taxa, y = mean.density, fill = cruise_date)) + geom_bar(position = "dodge", stat = "identity") +
     geom_errorbar(aes(ymin=mean.density-se, ymax=mean.density+se), width=.4, size = 1,        # Width of the error bars
                position=position_dodge(.9)) +
     scale_fill_manual(values=colors) + scale_x_discrete(name = "", drop=FALSE) +
     scale_y_continuous(name = "mean density (indiv./m3) ± 1 SE", expand = c(0,0), breaks = seq(0,round(max(dp$mean.density),0),5)) +
     coord_cartesian(ylim = c(0,45)) + theme_light(base_size = 30) +
     theme(axis.text.x = element_text(angle=45, hjust=1), legend.position = "none")
plot(b)

file = "figures/taxa_den_cruise.png"
png(file = file, width = 14, height = 10, units = "in", res = 300)
plot(b)
dev.off()

# by station locale
dl.sum.taxa <- df %>% group_by(date, gen_locale, taxa) %>% summarise(mean.density = mean(std.density), sd.density = sd(std.density), n = n())

dpl <- ddply(dl.sum.taxa, .variables = c("date"), function(n){
  t <- merge(x = n, y = taxa.n, by = "taxa", all.y = T)
  t$cruise <- unique(n$cruise)
  t$mean.density <- ifelse(is.na(t$mean.density), 0, t$mean.density)
  t$sd.density <- ifelse(is.na(t$sd.density   ), 0, t$sd.density)
  t$n <- ifelse(is.na(t$n   ), 0, t$n)
  return(t)

}, .progress = "text", .inform = T)
dpl$se <- dpl$sd.density/sqrt(dpl$n)
dpl$cruise_date <- factor(dpl$date, levels = c("Oct-2017","Jan-2018","Mar-2018"), labels = c("Oct-2017","Jan-2018","Mar-2018"))
dpl$locale <- factor(dpl$gen_locale, levels = c("nearshore","shelf","offshore"), labels = c("nearshore","shelf","offshore"))


b <- ggplot(dpl, aes(x = taxa, y = mean.density, fill = cruise_date)) + geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin=mean.density-se, ymax=mean.density+se), width=.4, size = 1,        # Width of the error bars
                position=position_dodge(.9)) +
  facet_grid(cruise_date~locale) +
  scale_fill_manual(values=colors) + scale_x_discrete(name = "", drop=FALSE) +
  scale_y_continuous(name = "mean density (indiv./m3) ± 1 SE", expand = c(0,0),
                     breaks = seq(0,round(max(dpl$mean.density),0),20)) +
                     #breaks = seq(0,round(max(log10(dpl$mean.density+1)),0),0.5)) +
  coord_cartesian(ylim = c(0,140)) +
  theme_light(base_size = 30) +
  theme(axis.text.x = element_text(angle=45, hjust=1), legend.position = "none", panel.grid.major.y = element_line(colour = "grey70"), panel.grid.minor.y = element_line(colour = "grey70"),
        panel.grid.major.x = element_blank(),
        strip.background = element_blank(), strip.text = element_text(colour = "black", face = "bold"))
plot(b)

#file = paste0(output.directory1, "Fig 7: 7_Parcel variance in relation to group density.png")
file = "figures/taxa_den_cruise_locale.png"
png(file = file, width = 14, height = 14, units = "in", res = 300)
plot(b)
dev.off()

d.sum.stn <- d %>% group_by(stn) %>% summarise(mean.density = mean(std.density), sd.density = sd(std.density), n = n())
stn.coord <- unique(d[,c("stn","lat","lon")])
d.sum.stn.coord <- join(d.sum.stn, stn.coord, "stn")
write.csv(x = d.sum.stn.coord, file = "GelZoo_Stn_Stats.csv", row.names = F)

