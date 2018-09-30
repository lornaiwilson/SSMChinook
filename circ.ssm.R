#title: "Data Wrangling: Case Study circuli SSM"
#author: "Lorna I wilson"
#date: "Started September 12, 2018"

pl.circ = read.csv(file="CirclusSP.txt", header = TRUE, sep = ",")
names(pl.circ)

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
#na.replace(fish.circ, '999')
names(fish.spec)

library(ggplot2)
library(ggridges) 
p2 = ggplot(fish.spec, aes(x = Skew, y = sys, group = sys, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.05, 0.95)) +
  scale_fill_manual(name = "Quantile", values = c("#E69F00", "#999999", "#56B4E9"),
                    labels = c("(0, 0.05)", "(0.05, 0.95)", "(0.95, 1)")); p2
#Good that plots look somewhat similar among stocks

#Does circuli spacing relate to change in length

#So, look at lengths by stock over time
p2 = ggplot(fish.spec, aes(year, fish.length, group = year)) + geom_boxplot(aes(fill = m.age)); p2
p2 + facet_grid(m.age~sys) + theme_bw() + ylim(425, 1200)


names(fish.spec)
######################################3
#predict fish length
library(lme4)
lme.pl2 = lmer(fish.length ~ Mean + as.factor(m.age) + year +
                 (1|sys) + (1|by), data = fish.spec) 
summary(lme.pl2)
#Yes, but do other circulus spacing features predict fish length as well as mean?

lme.pl3 = lmer(fish.length ~ SD + as.factor(m.age) + year +
                 (1|sys) + (1|by), data = fish.spec) 
#need to rescale SD
summary(lme.pl3)

lme.pl4 = lmer(fish.length ~ Skew + as.factor(m.age) + year +
                 (1|sys) + (1|by), data = fish.spec) 
summary(lme.pl4)

lme.pl5 = lmer(fish.length ~ kurt + as.factor(m.age) + year +
                 (1|sys) + (1|by), data = fish.spec) 
summary(lme.pl5)


library(AICcmodavg)

models<-list(lme.pl2, lme.pl3, lme.pl4, lme.pl5)
Modnames <- c('lme.pl2','lme.pl3','lme.pl4','lme.pl5')
aictab(cand.set = models, modnames = Modnames, sort = TRUE)


