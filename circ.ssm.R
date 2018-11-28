#title: "Data Wrangling: Case Study circuli SSM"
#author: "Lorna I Wilson"
#date: Started September 12, 2018

getwd() #set wd to appropriate place

circ = read.csv(file="circuli.csv", header = TRUE, sep = ",")
names(circ)

circ = circ[circ$AdultAge %in% c(14, 13),]
circ = circ[circ$DayofYear > 100,]
circ = na.omit(circ, cols="AdultLength")
#circ = circ[circ$LastAnn > 300,]
summary(circ$LastAnn)

summary(circ$AdultLength)
###########################################################
#Look at lengths by stock over time
library(ggplot2)
p2 = ggplot(circ, aes(Year, AdultLength, group = Year, fill = as.factor(AdultAge)))+ geom_boxplot(aes(fill = as.factor(AdultAge)))+
  facet_grid(sys~AdultAge) + theme_bw() + ylim(625, 1000); p2

library(dplyr)
ann.ave = circ %>% 
  group_by(sys, Year, AdultAge) %>% 
  summarise(Ann = mean(LastAnn))
ann.ave = as.data.frame(ann.ave)

library(broom)
lmfits <- ann.ave %>%
  group_by(sys, AdultAge) %>% 
  do(tidy(lm(Ann ~ Year, data = .)))
lmfits

lmfits = as.data.frame(lmfits)
lmfits

#limit ciculi to only populations and ages that are decreasing in size
circ$sysage = paste(circ$sys, circ$AdultAge)
unique(circ$sysage)
circ.size = circ[circ$sysage %in% c('CP 14', 'CP 13','NC 14', 'TC 14','FY 14'),]

##############33#########################
#Summary circulus measurement table
library(dplyr)
names(circ.size)
circ.summ = circ.size %>% 
  dplyr::group_by(sys, AdultAge) %>% 
  dplyr::summarise(n = n(), First = min(Year), Last = max(Year))
circ.summ = as.data.frame(circ.summ)
circ.summ

#Plot circ data
library(ggplot2)
library(ggridges) 
p1 = ggplot(circ.size, aes(x=LastAnn, y = sysage, group = sysage, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.05, 0.95)) +
  scale_fill_manual(name = "Quantile", values = c("#E69F00", "#999999", "#56B4E9"),
                    labels = c("(0, 0.05)", "(0.05, 0.95)", "(0.95, 1)")); p1
p1 = ggplot(circ.size, aes(x=LastAnn, y = Year, group = Year, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.05, 0.95)) +
  scale_fill_manual(name = "Quantile", values = c("#E69F00", "#999999", "#56B4E9"),
                    labels = c("(0, 0.05)", "(0.05, 0.95)", "(0.95, 1)")); p1


#####################################3#######################
####predict fish length
#select fixed effects
lme.pl1 = lmer(AdultLength ~ as.factor(AdultAge)+(1|sys), data = circ.size, REML = FALSE)
lme.pl8 = lmer(AdultLength ~  as.factor(AdultAge)+(1|sys)+as.numeric(LA.sum), data = circ.size, REML = FALSE)
lme.pl9 = lmer(AdultLength ~  as.factor(AdultAge)+(1|sys)+as.numeric(LA.wint), data = circ.size, REML = FALSE) 
lme.pl10 = lmer(AdultLength ~  as.factor(AdultAge)+(1|sys)+as.numeric(DayofYear), data = circ.size, REML = FALSE)
lme.pl11 = lmer(AdultLength ~  as.factor(AdultAge)+(1|sys)+as.numeric(LA.sum)+as.numeric(DayofYear), data = circ.size, REML = FALSE) 
lme.pl12 = lmer(AdultLength ~  as.factor(AdultAge)+(1|sys)+as.numeric(LA.wint)+as.numeric(DayofYear), data = circ.size, REML = FALSE)
lme.pl13 = lmer(AdultLength ~  as.factor(AdultAge)+(1|sys)+Long, data = circ.size, REML = FALSE)
lme.pl14 = lmer(AdultLength ~  as.factor(AdultAge)+(1|sys)+Lat, data = circ.size, REML = FALSE)
lme.pl15 = lmer(AdultLength ~  as.factor(AdultAge)+(1|sys)+as.numeric(LA.sum)+Long, data = circ.size, REML = FALSE)
lme.pl16 = lmer(AdultLength ~  as.factor(AdultAge)+(1|sys)+as.numeric(LA.sum)+Lat, data = circ.size, REML = FALSE)
lme.pl17 = lmer(AdultLength ~  as.factor(AdultAge)+(1|sys)+as.numeric(LA.wint)+Long, data = circ.size, REML = FALSE) 
lme.pl18 = lmer(AdultLength ~  as.factor(AdultAge)+(1|sys)+as.numeric(LA.wint)+Lat, data = circ.size, REML = FALSE) 
lme.pl19 = lmer(AdultLength ~  as.factor(AdultAge)+(1|sys)+as.numeric(DayofYear)+Long, data = circ.size, REML = FALSE)
lme.pl20 = lmer(AdultLength ~  as.factor(AdultAge)+(1|sys)+as.numeric(DayofYear)+Lat, data = circ.size, REML = FALSE)
lme.pl21 = lmer(AdultLength ~  as.factor(AdultAge)+(1|sys)+as.numeric(LA.sum)+as.numeric(DayofYear)+Long, data = circ.size, REML = FALSE) 
lme.pl22 = lmer(AdultLength ~  as.factor(AdultAge)+(1|sys)+as.numeric(LA.sum)+as.numeric(DayofYear)+Lat, data = circ.size, REML = FALSE) 
lme.pl23 = lmer(AdultLength ~  as.factor(AdultAge)+(1|sys)+as.numeric(LA.wint)+as.numeric(DayofYear)+Long, data = circ.size, REML = FALSE)
lme.pl24 = lmer(AdultLength ~  as.factor(AdultAge)+(1|sys)+as.numeric(LA.wint)+as.numeric(DayofYear)+Lat, data = circ.size, REML = FALSE)

names(circ)
models<-list(lme.pl1,lme.pl8,lme.pl8,lme.pl10,lme.pl11,lme.pl12,lme.pl13,lme.pl14,lme.pl15,lme.pl16,lme.pl17,lme.pl18,lme.pl19,lme.pl20,lme.pl21,lme.pl22,lme.pl23,lme.pl24)
Modnames <- c('lme.pl1','lme.pl8','lme.pl9','lme.pl10','lme.pl11','lme.pl12','lme.pl13','lme.pl14','lme.pl15','lme.pl16','lme.pl17','lme.pl18','lme.pl19','lme.pl20','lme.pl21','lme.pl22','lme.pl23','lme.pl24')
aictab(cand.set = models, modnames = Modnames, sort = TRUE)
anova(lme.pl24, lme.pl12) #latitude doesn't really help

summary(lme.pl12)
#lme.pl12 = lmer(AdultLength ~  as.factor(AdultAge)+(1|sys)+as.numeric(LA.wint)+as.numeric(DayofYear), data = circ.size, REML = FALSE)
lme.pl1 = lmer(AdultLength ~  as.factor(AdultAge)+as.numeric(LA.wint)+as.numeric(DayofYear)+(1|Year)+(1|sys)+(1|BY), data = circ.size) 
lme.pl2 = lmer(AdultLength ~  as.factor(AdultAge)+as.numeric(LA.wint)+as.numeric(DayofYear)+(1|sys)+ (1|BY), data = circ.size) 
lme.pl3 = lmer(AdultLength ~  as.factor(AdultAge)+as.numeric(LA.wint)+as.numeric(DayofYear)+(1|Year)+ (1|BY), data = circ.size) 
lme.pl4 = lmer(AdultLength ~  as.factor(AdultAge)+as.numeric(LA.wint)+as.numeric(DayofYear)+(1|Year)+(1|sys), data = circ.size) 
lme.pl5 = lmer(AdultLength ~  as.factor(AdultAge)+as.numeric(LA.wint)+as.numeric(DayofYear)+(1|Year), data = circ.size) 
lme.pl6 = lmer(AdultLength ~  as.factor(AdultAge)+as.numeric(LA.wint)+as.numeric(DayofYear)+(1|sys), data = circ.size) 
lme.pl7 = lmer(AdultLength ~  as.factor(AdultAge)+as.numeric(LA.wint)+as.numeric(DayofYear) +(1|BY), data = circ.size) 
library(AICcmodavg)
models<-list(lme.pl1,lme.pl2, lme.pl3, lme.pl4, lme.pl5, lme.pl6, lme.pl7)
Modnames <- c('lme.pl1','lme.pl2','lme.pl3','lme.pl4','lme.pl5','lme.pl6','lme.pl7')
aictab(cand.set = models, modnames = Modnames, sort = TRUE)
summary(lme.pl1)
#lme.pl1
acf(resid(lme.pl1))
pacf(resid(lme.pl1)) 
library(forecast)
lme.pl1.ac = auto.arima(resid(lme.pl1)) 
summary(lme.pl1.ac)  #ARIMA(0,1,1)

library(MuMIn)
r.squaredGLMM(lme.pl1) #Conditional R2 = 0.56, main effects R2 = 0.23

#Account for autocorrelation
last.ann.ts = ts(residuals(lme.pl1), start=min(circ.size$Year), end=max(circ.size$Year), frequency=1)
last.ann.ts.prew = arima(last.ann.ts, order = c(0,1,1))

length(last.ann.ts.prew)

#Need to have same same sample size before adding adjusted length to data frame
length(circ.size$Name) #32511  #now 10403
length(resid(lme.pl1)) #same
length(circ.size$Name) == length(resid(lme.pl1)) #true

circ.size$adj.len = resid(lme.pl1)


names(circ.size)

#circ$LastAnn = head(as.numeric(circ$LastAnn))
names(circ.size)
library(moments)
last.ann.resp = circ.size %>% 
  group_by(Year, sys, AdultAge) %>% 
  summarise(n = n(), MeanAL = mean(adj.len),MeanLA = mean(LastAnn), MedianLA = median(LastAnn),FiveP = quantile(LastAnn,.05), 
            NinetyfiveP = quantile(LastAnn,.95), SD = sd(LastAnn), kurt = kurtosis(LastAnn),Skew = skewness(LastAnn))

last.ann.resp = as.data.frame(last.ann.resp)

names(last.ann.resp)
shapiro.test(last.ann.resp$MeanAL)
shapiro.test(last.ann.resp$MeanLA)
shapiro.test(last.ann.resp$Skew)
shapiro.test(last.ann.resp$kurt)

#LAst annulus vs. ajusted length, should be +
summary(lm(last.ann.resp$MeanAL~last.ann.resp$MeanLA))
ggplot(data = last.ann.resp, aes(x = MeanLA, y = MeanAL)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = T) + theme_bw()

#Length change over time?
summary(lm(last.ann.resp$MeanLA~last.ann.resp$Year)) #last ann does sig relate to yr
ggplot(data = last.ann.resp, aes(x = Year, y = MeanLA)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = T) + theme_bw() 

summary(lm(last.ann.resp$MeanAL~last.ann.resp$Year)) #no sig relation to Year
ggplot(data = last.ann.resp, aes(x = Year, y = MeanAL)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = T) + theme_bw()+
  geom_hline(yintercept=0, linetype="dashed", color = "red")


#Kurtosis and adj length
summary(lm(last.ann.resp$MeanAL~last.ann.resp$kurt)) #no relation to kurtosis
ggplot(data = last.ann.resp, aes(x = MeanAL, y = kurt)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = T) + theme_bw()+
  geom_hline(yintercept=3, linetype="dashed", color = "red")

summary(lm(last.ann.resp$Year~last.ann.resp$kurt))
#kurt(AL) no relation to yr, but negative as expected
ggplot(data = last.ann.resp, aes(x = Year, y = kurt)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = T) + theme_bw() +
  geom_hline(yintercept=3, linetype="dashed", color = "red")


#Skewness and adj length
summary(lm(last.ann.resp$MeanAL~last.ann.resp$Skew)) 
#no relation to skew, but opposite of expected
ggplot(data = last.ann.resp, aes(x = MeanAL, y = Skew)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = T) + theme_bw()+
  geom_hline(yintercept=0, linetype="dashed", color = "red")


#SD and adj length
summary(lm(last.ann.resp$MeanAL~last.ann.resp$SD)) 
#vague negative, as expected
ggplot(data = last.ann.resp, aes(x = MeanAL, y = SD)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = T) + theme_bw()

       
