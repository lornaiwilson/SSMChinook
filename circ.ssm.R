#title: "Data Wrangling: Case Study circuli SSM"
#author: "Lorna I Wilson"
#date: "Started September 12, 2018"

#Read circulus data into workspace
pl.circ = read.csv(file="SP.4.csv", header = TRUE, sep = ",")
s4.circ = read.csv(file="S4.4.csv", header = TRUE, sep = ",")
s3.circ = read.csv(file="S3.4.csv", header = TRUE, sep = ",")
names(s3.circ)==names(s4.circ)
names(s4.circ)==names(pl.circ)


############Read SST temp data into workspace
sst.area.sum = read.csv(file="SSTbyAreaSum.csv", header = TRUE, sep = ",")
sst.area.wint = read.csv(file="SSTbyAreaWint.csv", header = TRUE, sep = ",")
head(sst.area.wint)

pop.season.area = read.csv(file="SysAreas.csv", header = TRUE, sep = ",")
head(pop.season.area)


###########################################################
#Make specimen table
fish.spec = cbind.data.frame(s3.circ$image.name, s3.circ$sys, s3.circ$year, s3.circ$project.code, s3.circ$doy, s3.circ$fishlength)
fish.spec = unique(fish.spec)
dim(fish.spec)
head(fish.spec)
#whew
names(fish.spec)=c("image.name", "sys", "year", "project.code", "doy", "fish.length")
#Maturity age from image name:
fish.spec$m.age = substr(fish.spec$image.name, 5,6)
#limit to only 1.3 and 1.4 ages
head(fish.spec$m.age)
unique(fish.spec$m.age)
fish.spec = fish.spec[fish.spec$m.age %in% c(14, 13),]
unique(fish.spec$sys)
#NEED TO SEPARATE KENAI RUNS

#generate circulus responses of summary stats by fish
library(moments)
library(dplyr)
pl.circ.resp = pl.circ %>% 
  group_by(image.name) %>% 
  summarise(Pl = sum(meas))

s4.circ.resp = s4.circ %>% 
  group_by(image.name) %>% 
  summarise(S4 = sum(meas))

s3.circ.resp = s3.circ %>% 
  group_by(image.name) %>% 
  summarise(S3 = sum(meas))



#need response and grouping variables in same dataframe
fish.spec = merge(s3.circ.resp, fish.spec, by = "image.name", all=T)
fish.spec = merge(pl.circ.resp, fish.spec, by = "image.name", all=T)
fish.spec = merge(s4.circ.resp, fish.spec, by = "image.name", all=T)

head(fish.spec)

fish.spec$by = fish.spec$year - (as.numeric(substr(fish.spec$image.name, 5,5))+as.numeric(substr(fish.spec$image.name, 6,6)))

summary(fish.spec)

#s3.circ.resp = s4.circ %>% 
#  group_by(image.name) %>% 
#  summarise(n = n(), Mean = mean(meas), Median = (median(meas)),FiveP = quantile(meas,.05), NinetyfiveP = quantile(meas,.95), SD = sd(meas), Skew = skewness(meas), kurt = kurtosis(meas))


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

cor.test(fish.spec$sum,fish.spec$wint)

######################################3
#predict fish length
library(lme4)
lme.pl2 = lmer(fish.length ~  as.factor(m.age) + (1|year)  + wint +(1|sys) + (1|by), data = fish.spec) 
lme.pl3 = lmer(fish.length ~  as.factor(m.age) + (1|year)  + sum +(1|sys) + (1|by), data = fish.spec) 
lme.pl4 = lmer(fish.length ~  as.factor(m.age) + (1|year) +(1|sys) + (1|by), data = fish.spec) 
library(AICcmodavg)
models<-list(lme.pl2, lme.pl3, lme.pl4)
Modnames <- c('lme.pl2','lme.pl3','lme.pl4')
aictab(cand.set = models, modnames = Modnames, sort = TRUE)
anova(lme.pl3, lme.pl2)

acf(resid(lme.pl2))
pacf(resid(lme.pl2)) #lag 8? 
library(forecast)
lme.pl2.ac = auto.arima(resid(lme.pl2)) #ARIMA(5,0,5) with zero mean; M5b.ac
lme.pl2.ac #Ar1 coeff = 0.0141, SE 0.0085
summary(lme.pl2)


#Plot circ data
library(ggplot2)
library(ggridges) 
names(fish.spec)
p1 = ggplot(fish.spec, aes(x =Pl, y = sys, group = sys, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.05, 0.95)) +
  scale_fill_manual(name = "Quantile", values = c("#E69F00", "#999999", "#56B4E9"),
                    labels = c("(0, 0.05)", "(0.05, 0.95)", "(0.95, 1)")); p1
p1 = ggplot(fish.spec, aes(x = Pl, y = year, group = year, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.05, 0.95)) +
  scale_fill_manual(name = "Quantile", values = c("#E69F00", "#999999", "#56B4E9"),
                    labels = c("(0, 0.05)", "(0.05, 0.95)", "(0.95, 1)")); p1





#So, look at lengths by stock over time
p2 = ggplot(fish.spec, aes(year, fish.length, group = year)) + geom_boxplot(aes(fill = m.age)); p2
p2 + facet_wrap(m.age~sys) + theme_bw() + ylim(425, 1200)


names(fish.spec)
#


