#title: "Case Study circuli SSM"
#author: "Lorna I wilson"
#date: "September 12, 2018"

pl.circ = read.csv(file="CirclusSP.txt", header = TRUE, sep = ",")

names(pl.circ)
#Data cleaning
#subset to only females
pl.circ = pl.circ[pl.circ$sex %in% c(2),]
names(pl.circ)
summary(pl.circ$sex)

#Make specimen table
fish.spec = cbind.data.frame(pl.circ$image.name, pl.circ$sys, pl.circ$year, pl.circ$project.code, pl.circ$doy, pl.circ$fish.length)
dim(fish.spec)
fish.spec = unique(fish.spec)
dim(fish.spec)
#whew
names(fish.spec)=c("image.name", "sys", "year", "project.code", "doy", "fish.length")
#Maturity age from image name:
fish.spec$m.age = substr(fish.spec$image.name, 5,6)

library(moments)
library(dplyr)

#generate circulus responses of summary stats by fish
pl.circ.resp = pl.circ %>% 
  group_by(image.name) %>% 
  summarise(n = n(), Mean = mean(meas), Median = (median(meas)),FiveP = quantile(meas,.05), NinetyfiveP = quantile(meas,.95), SD = sd(meas), Skew = skewness(meas), kurt = kurtosis(meas))

#need response and grouping variables in same dataframe
fish.spec = merge(pl.circ.resp, fish.spec, by = "image.name")
fish.spec$by = fish.spec$year - (as.numeric(substr(fish.spec$image.name, 5,5))+as.numeric(substr(fish.spec$image.name, 6,6)))


#na.replace(fish.circ, '999')

library(ggplot2)
library(ggridges) 
p2 = ggplot(fish.spec, aes(x = Skew, y = sys, group = sys, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.05, 0.95)) +
  scale_fill_manual(name = "Quantile", values = c("#E69F00", "#999999", "#56B4E9"),
                    labels = c("(0, 0.05)", "(0.05, 0.95)", "(0.95, 1)"))
p2
#Good that plots look somewhat similar among stocks

names(fish.spec)
#Does circuli spacing relate to change in length

#So, look at lengths by stock over time
p2 = ggplot(fish.spec, aes(year, fish.length, group = year)) + geom_boxplot(aes(fill = m.age))
p2
p2 + facet_wrap(sys ~.) + theme_bw() + ylim(425, 1200)


library(lme4)
names(fish.spec)
#predict circulus spacing, but question is about fish length 
lme.pl1 = lmer(Mean ~ as.factor(fish.spec$m.age) + fish.spec$year +
                 (1|as.factor(fish.spec$sys)) + (1|as.factor(fish.spec$by)), data = fish.spec) 
summary(lme.pl1)
######################################3
#predict fish length
lme.pl2 = lmer(fish.length ~ fish.spec$Mean + as.factor(fish.spec$m.age) + fish.spec$year +
                 (1|as.factor(fish.spec$sys)) + (1|as.factor(fish.spec$by)), data = fish.spec) 
summary(lme.pl2)
#Yes, but do other circulus spacing features predict fish length as well as mean?
names(fish.spec)
lme.pl3 = lmer(fish.length ~ fish.spec$Skew + as.factor(fish.spec$m.age) + fish.spec$year +
                 (1|as.factor(fish.spec$sys)) + (1|as.factor(fish.spec$by)), data = fish.spec, na.rm = T) 
length(fish.spec$Skew)
summary(fish.spec$Skew)
length(fish.spec$Mean)

summary(lme.pl2)


