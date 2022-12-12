library(lme4)
library(readr)
library(ggplot2)
library(merTools)
library(dplyr)
epldata_final <- read_csv("C:/Users/Haoyue/Desktop/epldata_final.csv")
head(epldata_final)


ggplot(data=epldata_final, aes(x=name,y=market_value)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  xlab("name") + ylab("value") + 
  facet_wrap( ~ club, ncol=5)

#level2
epldata_final$fpl_points2 <- scale(epldata_final$fpl_points,scale = F)
epldata_final$fpl_value2 <- scale(epldata_final$fpl_value,scale = F)



model1 <- lmer(market_value~1+(1|club),REML=FALSE, data=epldata_final)
summary(model1)
confint(model1)
ICC(outcome="market_value", group="club", data=epldata_final)


model2 <- lmer(market_value~fpl_points2+fpl_value2+(1|club),REML=FALSE, data=epldata_final)
summary(model2)
confint(model2)
model3 <- lmer(market_value~fpl_points2*fpl_value2+(1|club),REML=FALSE, data=epldata_final)
summary(model3)
confint(model3)
anova(model2, model3)
epldata_final$model3.1<-predict(model3, newdata=epldata_final)
theme_set(theme_bw(base_size = 7, base_family = "")) 

ggplot(data = epldata_final, aes(x = fpl_points, y=market_value,group=club))+
  coord_cartesian(ylim=c(10,80))+
  geom_point(aes(colour = club))+
  geom_smooth(method = "lm", se = TRUE,aes(colour = club))+
  xlab("fpl points")+ylab("market value")
ggplot(data = epldata_final, aes(x = fpl_points, y=model3.1,group=club))+
  coord_cartesian(ylim=c(10,80))+
  geom_point(aes(colour = club))+
  geom_smooth(method = "lm", se = TRUE,aes(colour = club))+
  xlab("fpl points")+ylab("market value-Model3.1")

model3.2 <- lmer(market_value~fpl_points2*fpl_value2+(1+fpl_points|club),REML=FALSE, data=epldata_final)
summary(model3.2)
#level3
ggplot(data = epldata_final, aes(x = fpl_points, y=market_value,group=club))+
  facet_grid(~big_club)+
  coord_cartesian(ylim=c(10,80))+
  geom_point(aes(colour = club))+
  geom_smooth(method = "lm", se = TRUE,aes(colour = club))+
  xlab("fpl points")+ylab("market value")


Plot.Means<-epldata_final %>% group_by(club) %>%  
  dplyr::summarize(marketM=mean(market_value, na.rm=TRUE),
                   big_clubM=mean(big_club, na.rm=TRUE))

market_with_club<-ggplot(data = Plot.Means, aes(x = reorder(club, -marketM), y=marketM))+
  geom_point(aes(size = big_clubM))+
  xlab("")+ylab("Market Value")+
  theme_bw()+
  theme(legend.position = "top")
market_with_club

aggregate(epldata_final$market_value, list(epldata_final$club), FUN=sum)


model4 <- lmer(market_value~1+(1|big_club)+(1|big_club:club),REML=FALSE, data=epldata_final)
summary(model4)
model5 <- lmer(market_value~fpl_points2+fpl_value2+(1|big_club)+(1|big_club:club),REML=FALSE, data=epldata_final)
summary(model5)
model6 <- lmer(market_value~fpl_points2*fpl_value2+(1|big_club)+(1|big_club:club),REML=FALSE, data=epldata_final)
summary(model6)
anova(model5,model6)
model6.2 <- lmer(market_value~fpl_points2*fpl_value2+(1+fpl_points2|big_club)+(1+fpl_points2|big_club:club),REML=FALSE, data=epldata_final)
summary(model6.2)

epldata_final$model6.2<-predict(model6.2, newdata=epldata_final)
theme_set(theme_bw(base_size = 7, base_family = "")) 


ggplot(data = epldata_final, aes(x = fpl_points, y=model6.2,group=club))+
  facet_grid(~big_club) +
  coord_cartesian(ylim=c(10,80))+
  geom_point(aes(colour = club))+
  geom_smooth(method = "lm", se = TRUE,aes(colour = club))+
  xlab("fpl points")+ylab("market value-Model6.2")