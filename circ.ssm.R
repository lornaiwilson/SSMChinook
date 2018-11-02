#title: "Data Wrangling: Case Study circuli SSM"
#author: "Lorna I Wilson"
#date: "Started September 12, 2018"

#Read circulus data into workspace
pl.circ = read.csv(file="CirclusSP.txt", header = TRUE, sep = ",")
names(pl.circ)

############Read SST temp data into workspace
#Sum: June - Sept
goa.sst = read.csv(file="goa.sst.csv", header = TRUE, sep = ",")
head(goa.sst)
bs.sst = read.csv(file="bs.sst.csv", header = TRUE, sep = ",") #June - Sept SST
head(bs.sst)

#add simple names
bs.sst = cbind.data.frame(rep("BS", dim(bs.sst)[1]), bs.sst)
names(bs.sst) = c("waters", "Yr", "sumSST", "wintSST")
head(bs.sst)

goa.sst = cbind.data.frame(rep("GOA", dim(goa.sst)[1]), goa.sst)
names(goa.sst) = c("waters", "Yr", "sumSST", "wintSST")
head(goa.sst)

#put all SST data in one table
sst.waters = rbind.data.frame(goa.sst, bs.sst)
head(sst.waters)

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

#add GOA and BS designations to data frame
waters = c("BS", "BS", "GOA", "GOA", "GOA", "BS", "GOA", "BS", "GOA", "GOA", "BS", "GOA")
sys.waters = cbind.data.frame(waters, unique(fish.spec$sys))
names(sys.waters) = c("waters", "sys")
fish.spec = merge(sys.waters, fish.spec, "sys") 
names(fish.spec)

###Add SST to dataframe
summary(fish.spec$waters)
summary(fish.spec$year)
#Common fields
fish.spec$YrWaters = paste(fish.spec$year, fish.spec$waters)
sst.waters$YrWaters = paste(sst.waters$Yr, sst.waters$waters)
head(fish.spec$YrWaters)
head(sst.waters$YrWaters)
#Merge
fish.spec = merge(fish.spec, sst.waters, "YrWaters")
head(fish.spec)


######################################3
#predict fish length
library(lme4)
lme.pl1 = lmer(fish.length ~  as.factor(m.age) + year + sumSST + wintSST +
                 (1|sys) + (1|by), data = fish.spec) 
lme.pl2 = lmer(fish.length ~  as.factor(m.age) + year  + wintSST +
                 (1|sys) + (1|by), data = fish.spec) 
lme.pl3 = lmer(fish.length ~  as.factor(m.age) + year  + sumSST +
                 (1|sys) + (1|by), data = fish.spec) 
lme.pl4 = lmer(fish.length ~  as.factor(m.age) + year  +
                 (1|sys) + (1|by), data = fish.spec) 
library(AICcmodavg)

models<-list(lme.pl1, lme.pl2, lme.pl3, lme.pl4)
Modnames <- c('lme.pl1','lme.pl2','lme.pl3','lme.pl4')
aictab(cand.set = models, modnames = Modnames, sort = TRUE)
anova(lme.pl1, lme.pl2)



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


