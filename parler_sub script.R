### read csv file #####

parler_sub <- read.csv(file="C:\\Users\\charl\\OneDrive\\Documents\\R\\Parler\\data\\Parler_sub.csv",  encoding="UTF-8", sep=",")

### glimpse at first few rows to check data read in properly ##

head(parler_sub)

library(ggplot2)
library(plyr)
library(jsonlite)
library(rjson)
library(ndjson)
library(data.table)
library (parallel)
library(dplyr)
library(tibble)
library(rlang)
library(tidyverse)
library(car)
library(MASS)
library(arm)

options(scipen = 999)

summary(parler_sub)

count(parler_sub, "QAnon.supporter")

sapply(parler_sub, sd)
### n=4989, 502 QAnon supporters versus 4487 nonQAnon ## 

## histogram to show distribution of variable ##

ggplot(data=parler_sub, mapping = aes(x=QAnon.supporter))+
  geom_histogram(binwidth = 0.5, fill="blue")

## separate variable based on QAnons versus non-QAnons to check distribution #

QAnons <- parler_sub %>%
  filter(QAnon.supporter > 0)

NonQAnons <- parler_sub %>%
  filter(QAnon.supporter < 1)

### plot QAnons first ## 

ggplot(data=QAnons, aes(x=QAnon.supporter, y=user_following))+
  geom_point()+
  labs(title= "QAnon supporters engagement with Parler")

## nonQAnons ##

ggplot(data=NonQAnons, aes(x=QAnon.supporter, y=user_following))+
  geom_point()+
  labs(title= "Non-QAnon supporters engagement with Parler")

## plot a different scatterplot 

ggplot(data=QAnons, mapping=aes(x=QAnon.supporter, y=user_following))+
  geom_point(alpha=0.3)+
  geom_smooth(method="gam")+
  scale_x_discrete(labels=abbreviate)+
  labs(x="QAnon bio supporter", y="User following",
    title = "Scatter plot of QAnon supporters in Parler user bios & user followers")

ggplot(data=NonQAnons, mapping=aes(x=QAnon.supporter, y=user_following))+
  geom_point(alpha=0.3)+
  geom_smooth(method="gam")+
  scale_x_discrete(labels=abbreviate)+
  labs(x="Non-QAnon supporter", y="User following",
       title = "Scatter plot of non-QAnon supporters in Parler user bios & user followers")

## plot box plots to better visualise variable and relationship with independent variables 

boxplot(user_following~QAnon.supporter, ylab="user_following", Xlab="parler_bios",
        col="light blue", data=parler_sub%>%
          mutate(user_following=user_following%>%log10))


boxplot(comments~QAnon.supporter, ylab = "parler user's comments", xlab = "parler bios",
        col="light blue", data=parler_sub%>%
          mutate(comments=comments%>%log10))


## relevelling the QAnon supporter variable ##

summary(QAnon.supporter$parler_sub)

### look at an intercept only model 

# relevel QAnon supporter variable to set non-QAnon bios as referece ##

parler_2 <- parler_sub %>% mutate(QAnon.supporter = as.factor(QAnon.supporter))

levels(parler_2$QAnon.supporter)

### try intercept only model ##

intercept <-glm(QAnon.supporter ~ 1, data=parler_2, family=binomial)

plot(intercept)

lm.diag.plots(intercept)

summary(intercept)

## try first logistic regression model ##

logit_1 <- glm(QAnon.supporter ~ comments + user_followers + user_following + posts, data=parler_2, family=binomial)

summary(logit_1)

### levels of bot variable ##

levels(parler_2$human)

### convert variable to a factor to relevel ## 

parler_3 <- parler_2 %>% mutate(human = as.factor(human))

levels(parler_3$human)
parler_3$human <- relevel(parler_3$human, ref="TRUE") ### changed reference category to true, rather than false ##

## additional model with bot variable added #

logit_2 <- glm(QAnon.supporter ~ comments + user_followers + user_following + posts + human, data=parler_3, family=binomial)

summary(logit_2)

### a third model with additional user engagement metrics - likes ##

logit_3 <- glm(QAnon.supporter ~ comments + user_followers + user_following + posts + likes + human, data=parler_3, family=binomial )

summary(logit_3)

## ANOVa to compare models (intercept with model 3) ##

anova(logit_1, logit_3, test = "Chi")### seems like a better model fit ##

### look at odds rations to compare against 1 ##

coef(logit_3)

## now take exponentiate coefficients ##

exp(coef(logit_3))

## exponentiate confidence interval ##

exp(confint(logit_3))

## model diagnostics ##

## cooks distance ##

cooks.distance(logit_3) ### influence of individual observations - all under 1 ##

dfbetas(logit_3)

### Variance inflator factor to check for multicolinearity ###

vif(logit_3)### half close to one, but user following and user followers 3.3 ish which could be a cause for concern ## 


cor.test(~ user_followers+user_following, data=parler_sub)

## create a new variable user followers and following #


glm(QAnon.supporter ~ comments + engagement + posts + likes + human, data=parler_3 %>% 
      mutate(engagement= user_followers + user_following), family=binomial) %>% summary()

## correlation test suggested variables highly correlated ##

cor.test(~ user_followers+user_following, data=parler_sub)
cor.test(~ likes + user_followers, data=parler_sub)
cor.test(~ comments + user_followers, data=parler_sub)


glm(QAnon.supporter ~ engagement, data=parler_3 %>% 
      mutate(engagement= user_followers + user_following), family=binomial) %>% summary()

### check residual plots ##

residualPlots(logit_3)

### binned plot to also check residuals ##

binnedplot(predict(logit_3), resid(logit_3)) ## shows evidence of 95% of residuals not being within line##

## try dropping user followers to check whether it improves model ##

logit_4 <- glm(QAnon.supporter ~ comments + user_following + posts + likes + human, data=parler_3, family=binomial )

summary(logit_4)

#### model diagnostics ##

cooks.distance(logit_4)
dfbetas(logit_4)

vif(logit_4) ### new VIF seems more reliable - highest now posts, 2.15 and user followers dropped to 1.28 

## exponentiate confidence intervals - compared to model three ##

exp(confint(logit_4))

binnedplot(predict(logit_4), resid(logit_4)) ## but binned plot now higher 



