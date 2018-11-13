#title: "Data Wrangling: Case Study circuli SSM"
#author: "Lorna I Wilson"
#date: Started September 12, 2018

circ = read.csv(file="circuli.csv", header = TRUE, sep = ",")
names(circ)

circ = circ[circ$AdultAge %in% c(14, 13),]
circ = circ[circ$DayofYear > 100,]
circ = na.omit(circ, cols="AdultLength")

summary(circ$AdultLength)



######################################3#######################
#predict fish length
library(lme4)
lme.pl1 = lmer(AdultLength ~  as.factor(AdultAge)+(1|Year)+(1|sys)+(1|BY), data = circ) 
lme.pl2 = lmer(AdultLength ~  as.factor(AdultAge)+(1|sys)+ (1|BY), data = circ) 
lme.pl3 = lmer(AdultLength ~  as.factor(AdultAge)+(1|Year)+ (1|BY), data = circ) 
lme.pl4 = lmer(AdultLength ~  as.factor(AdultAge)+(1|Year)+(1|sys), data = circ) 
lme.pl5 = lmer(AdultLength ~  as.factor(AdultAge)+(1|Year), data = circ) 
lme.pl6 = lmer(AdultLength ~  as.factor(AdultAge) +(1|sys), data = circ) 
lme.pl7 = lmer(AdultLength ~  as.factor(AdultAge) +(1|BY), data = circ) 

library(AICcmodavg)
models<-list(lme.pl1,lme.pl2, lme.pl3, lme.pl4, lme.pl5, lme.pl6, lme.pl7)
Modnames <- c('lme.pl1','lme.pl2','lme.pl3','lme.pl4','lme.pl5','lme.pl6','lme.pl7')
aictab(cand.set = models, modnames = Modnames, sort = TRUE)
#lme.pl1
lme.pl1 = lmer(AdultLength ~  as.factor(AdultAge)+(1|Year)+(1|sys)+(1|BY), data = circ, REML = FALSE)
lme.pl8 = lmer(AdultLength ~  as.factor(AdultAge)+(1|Year)+(1|sys)+(1|BY)+
                 as.numeric(LA.sum), data = circ, REML = FALSE)
lme.pl9 = lmer(AdultLength ~  as.factor(AdultAge)+(1|Year)+(1|sys)+(1|BY)+
                 as.numeric(LA.wint), data = circ, REML = FALSE) 
lme.pl10 = lmer(AdultLength ~  as.factor(AdultAge)+(1|Year)+(1|sys)+(1|BY)+
                 as.numeric(DayofYear), data = circ, REML = FALSE)
lme.pl11 = lmer(AdultLength ~  as.factor(AdultAge)+(1|Year)+(1|sys)+(1|BY)+
                 as.numeric(LA.sum)+as.numeric(DayofYear), data = circ, REML = FALSE) 
lme.pl12 = lmer(AdultLength ~  as.factor(AdultAge)+(1|Year)+(1|sys)+(1|BY)+
                  as.numeric(LA.wint)+as.numeric(DayofYear), data = circ, REML = FALSE)

models<-list(lme.pl1,lme.pl8,lme.pl8,lme.pl10,lme.pl11,lme.pl12 )
Modnames <- c('lme.pl1','lme.pl8','lme.pl9','lme.pl10','lme.pl11','lme.pl12')
aictab(cand.set = models, modnames = Modnames, sort = TRUE)

summary(lme.pl12)
acf(resid(lme.pl12))
pacf(resid(lme.pl12)) 
library(forecast)
lme.pl12.ac = auto.arima(resid(lme.pl12)) #ARIMA(1,0,0) with zero mean
summary(lme.pl12.ac)
lme.pl12.ac #Ar1 coeff = 0.0914, SE 0.0226
#ar1      ar2      ma1      ma2
#1.1723  -0.2012  -0.5476  -0.3523
#s.e.0.0148   0.0121   0.0143   0.0090


library(MuMIn)
r.squaredGLMM(lme.pl12) #Conditional R2 = 0.57, main effects R2 = 0.23
length(circ$Name) #32514
length(resid(lme.pl12)) #32514

circ$adj.len = resid(lme.pl12)

names(circ)

#########################
#Summary circulus measurement table
library(dplyr)
circ.summ = circ %>% 
  group_by(sys, AdultAge) %>% 
  summarise(n = n(), First = min(Year), Last = max(Year))
circ.summ

#Plot circ data
library(ggplot2)
library(ggridges) 
names(circ)
p1 = ggplot(circ, aes(x=Radius, y = sys, group = sys, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.05, 0.95)) +
  scale_fill_manual(name = "Quantile", values = c("#E69F00", "#999999", "#56B4E9"),
                    labels = c("(0, 0.05)", "(0.05, 0.95)", "(0.95, 1)")); p1
p1 = ggplot(circ, aes(x=Radius, y = Year, group = Year, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.05, 0.95)) +
  scale_fill_manual(name = "Quantile", values = c("#E69F00", "#999999", "#56B4E9"),
                    labels = c("(0, 0.05)", "(0.05, 0.95)", "(0.95, 1)")); p1

names(circ)
#Look at lengths by stock over time
p2 = ggplot(circ, aes(Year, AdultLength, group = Year, fill = as.factor(AdultAge)))+ geom_boxplot(aes(fill = as.factor(AdultAge)))+
  facet_wrap(sys~.) + theme_bw() + ylim(625, 1000)
p2

names(circ)
head(circ$LastAnn)
mode(circ$LastAnn)

circ$LastAnn = as.numeric(circ$LastAnn)
names(circ)
library(moments)
last.ann.resp = circ %>% 
  group_by(Year, sys, AdultAge) %>% 
  summarise(n = n(), MeanAL = mean(adj.len),MeanAL = mean(adj.len), MedianLA = median(LastAnn),FiveP = quantile(LastAnn,.05), 
            NinetyfiveP = quantile(LastAnn,.95), SD = sd(LastAnn), kurt = kurtosis(LastAnn),Skew = skewness(LastAnn))

last.ann.resp = as.data.frame(last.ann.resp)


head(last.ann.resp)
plot(last.ann.resp$MedianLA~last.ann.resp$MeanAL)

#Length change over time?
plot(last.ann.resp$MeanAL~last.ann.resp$Year)
summary(lm(last.ann.resp$MeanAL~last.ann.resp$Year)) #no relation to Year

summary(lm(last.ann.resp$MeanAL~last.ann.resp$MedianLA))
#postively related, good, this is unexplained variance


summary(lm(last.ann.resp$MeanAL~last.ann.resp$kurt)) #no relation to kurtosis
plot(last.ann.resp$kurt~last.ann.resp$MeanAL)
summary(lm(last.ann.resp$MeanAL~last.ann.resp$Skew)) #negatively related to skewness
plot(last.ann.resp$Skew~last.ann.resp$MeanAL)



       