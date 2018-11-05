#title: "Data Wrangling: Case Study circuli SSM"
#author: "Lorna I Wilson"
#date: "Started September 12, 2018"

#Read circulus data into workspace
pl.circ = read.csv(file="CirclusSP.txt", header = TRUE, sep = ",")
names(pl.circ)

s4.circ = read.csv(file="S4circuli.txt", header = TRUE, sep = ",")
names(pl.circ)

############Read SST temp data into workspace
sst.area.sum = read.csv(file="SSTbyAreaSum.csv", header = TRUE, sep = ",")
sst.area.wint = read.csv(file="SSTbyAreaWint.csv", header = TRUE, sep = ",")
head(sst.area.wint)

pop.season.area = read.csv(file="SysAreas.csv", header = TRUE, sep = ",")
head(pop.season.area)


###########################################################
#Make specimen table
fish.spec = cbind.data.frame(pl.circ$image.name, pl.circ$sys, pl.circ$year, pl.circ$project.code, pl.circ$doy, pl.circ$fishlength)
dim(fish.spec)
fish.spec = unique(fish.spec)
dim(fish.spec)
#whew
names(fish.spec)=c("image.name", "sys", "year", "project.code", "doy", "fish.length")
#Maturity age from image name:
fish.spec$m.age = substr(fish.spec$image.name, 5,6)
#limit to only 1.3 and 1.4 ages
fish.spec = fish.spec[fish.spec$m.age %in% c(14, 13),]
#NEED TO SEPARATE KENAI RUNS

#generate circulus responses of summary stats by fish
library(moments)
library(dplyr)
pl.circ.resp = pl.circ %>% 
  group_by(image.name) %>% 
  summarise(n = n(), Mean = mean(meas), Median = (median(meas)),FiveP = quantile(meas,.05), NinetyfiveP = quantile(meas,.95), SD = sd(meas), Skew = skewness(meas), kurt = kurtosis(meas))

#need response and grouping variables in same dataframe
fish.spec = merge(pl.circ.resp, fish.spec, by = "image.name")
fish.spec$by = fish.spec$year - (as.numeric(substr(fish.spec$image.name, 5,5))+as.numeric(substr(fish.spec$image.name, 6,6)))

head(fish.spec)

#######Add SST to dataframe
#Merge area to pop
fish.spec = merge(fish.spec, pop.season.area, by.x = "sys", by.y = "Pop")
###Merge temps to area and years
#make common fields
sst.area.sum$YrArea = paste(sst.area.sum$Year, sst.area.sum$Area)
sst.area.wint$YrArea = paste(sst.area.wint$Year, sst.area.wint$Area)

fish.spec$YrSumArea = paste(fish.spec$year, fish.spec$Sum.Area)
fish.spec$YrWintArea = paste(fish.spec$year, fish.spec$Wint.area)

#Merge sum temps in
fish.spec = merge(fish.spec, sst.area.sum, by.x = "YrSumArea", by.y = "YrArea")
#remove extra columns from merge, Area and Year
fish.spec$Area = NULL
fish.spec$Year = NULL

head(fish.spec)

#Merge wint temps in
fish.spec = merge(fish.spec, sst.area.wint, by.x = "YrWintArea", by.y = "YrArea")
#remove extra columns from merge, Area and Year
fish.spec$Area = NULL
fish.spec$Year = NULL

head(fish.spec)


######################################3
#predict fish length
library(lme4)
lme.pl1 = lmer(fish.length ~  as.factor(m.age) + (1|year) + sum + wint +(1|sys) + (1|by), data = fish.spec) 
lme.pl2 = lmer(fish.length ~  as.factor(m.age) + (1|year)  + wint +(1|sys) + (1|by), data = fish.spec) 
lme.pl3 = lmer(fish.length ~  as.factor(m.age) + (1|year)  + sum +(1|sys) + (1|by), data = fish.spec) 
lme.pl4 = lmer(fish.length ~  as.factor(m.age) + (1|year) +(1|sys) + (1|by), data = fish.spec) 
library(AICcmodavg)

models<-list(lme.pl1, lme.pl2, lme.pl3, lme.pl4)
Modnames <- c('lme.pl1','lme.pl2','lme.pl3','lme.pl4')
aictab(cand.set = models, modnames = Modnames, sort = TRUE)
anova(lme.pl1, lme.pl3)


#Plot circ data
library(ggplot2)
library(ggridges) 
names(fish.spec)
p1 = ggplot(fish.spec, aes(x = Mean, y = sys, group = sys, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.05, 0.95)) +
  scale_fill_manual(name = "Quantile", values = c("#E69F00", "#999999", "#56B4E9"),
                    labels = c("(0, 0.05)", "(0.05, 0.95)", "(0.95, 1)")); p1
p1 = ggplot(fish.spec, aes(x = Mean, y = year, group = year, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.05, 0.95)) +
  scale_fill_manual(name = "Quantile", values = c("#E69F00", "#999999", "#56B4E9"),
                    labels = c("(0, 0.05)", "(0.05, 0.95)", "(0.95, 1)")); p1
#no obvious trends

p2 = ggplot(fish.spec, aes(x = Skew, y = sys, group = sys, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.05, 0.95)) +
  scale_fill_manual(name = "Quantile", values = c("#E69F00", "#999999", "#56B4E9"),
                    labels = c("(0, 0.05)", "(0.05, 0.95)", "(0.95, 1)")); p2
#Good that plots look somewhat similar among stocks
p2 = ggplot(fish.spec, aes(x = Skew, y = year, group = year, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.05, 0.95)) +
  scale_fill_manual(name = "Quantile", values = c("#E69F00", "#999999", "#56B4E9"),                    labels = c("(0, 0.05)", "(0.05, 0.95)", "(0.95, 1)")); p2
# hm, skewness in 1980 and previous are inconsistent

p2 = ggplot(fish.spec, aes(x = SD, y = year, group = year, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.05, 0.95)) +
  scale_fill_manual(name = "Quantile", values = c("#E69F00", "#999999", "#56B4E9"),                    labels = c("(0, 0.05)", "(0.05, 0.95)", "(0.95, 1)")); p2

p2 = ggplot(fish.spec, aes(x = kurt, y = year, group = year, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.05, 0.95)) +
  scale_fill_manual(name = "Quantile", values = c("#E69F00", "#999999", "#56B4E9"),                    labels = c("(0, 0.05)", "(0.05, 0.95)", "(0.95, 1)")); p2


#So, look at lengths by stock over time
p2 = ggplot(fish.spec, aes(year, fish.length, group = year)) + geom_boxplot(aes(fill = m.age)); p2
p2 + facet_grid(m.age~sys) + theme_bw() + ylim(425, 1200)


names(fish.spec)
#


