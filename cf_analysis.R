#libraries
library(readr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(xtable)
library(Hmisc)
library(nnet)
library(reshape2)
library(ggbiplot)
library(corrplot)
library(lsmeans)
library(factoextra)
library(ggfortify)
library(compareGroups)
library(RColorBrewer)
library(MASS)
library(effects)
library(VIM)
library(mice)
library(multcompView)
library(stargazer)
library(lattice)
library(gridExtra)
library(car)
library(ggplot2)
library(GGally)
require(ggpubr)
require(tidyverse)
require(corrplot)
library(tidyr)
library(emmeans)

load("cf_behav_data.rdata")

df<-CF_ALL
#Define subsets
df_plac<- subset(df, DRUG=='Plac')
df_nalt <- subset (df, DRUG=='Nalt')

#Mixed-effects logistic regression models
m1 <- (glmer(Rating ~ ContextNum*EmotionNum + (1|Participant/Order), family=binomial, df_plac))
summary(m1)
car::Anova(m1, "III")
vif.lme(m1 )
plot(emmeans(m1,  ~ ContextNum*EmotionNum), horiz = F)

m2 <- (glmer(Rating ~ ContextNum*EmotionNum + (1|Participant/Order), family=binomial, df_nalt))
summary(m2)
car::Anova(m2, "III")
vif.lme(m2)
plot(emmeans(m2, ~ ContextNum*EmotionNum), horiz = F)

m3 <- (glmer(Rating ~ Context*EmotionNum + DRUG+(1|Participant/Order), family=binomial, df))
summary(m3)
car::Anova(m3, "III")
vif.lme(m3)
#RT model
m4 <- (glmer(Rating ~ Context*EmotionNum*DRUG+scale(RT)+(1|Participant/Order), family=binomial, df))
summary(m4)
car::Anova(m4, "III")

##Mixed-effect RT regression Models:
#RT with lag variable
m_rt_lag <- lmer(1/scale(RT) ~ 
                    1/scale(RT_lag)+  
                    ( 1 | uID/Order) ,
                  data = df_nalt[!df_nalt$outlier,],
                  control=lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(m_rt_lag)
car::Anova(m_rt_lag, type = 'III')
#basic model:
m_rt_base <- lmer(RT~ 
                    RT_lag +  ContextNum * EmotionNum * Rating+  
                    ( 1 | uID/Order) ,
                  data = df_nalt[!df_nalt$outlier,],
                  control=lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(m_rt_base)
car::Anova(m_rt_base, type = 'III')
#Log transformed RT
m_rt_base_log <- lmer(log(RT)~ 
                        log(RT) +  ContextNum * EmotionNum * Rating+  
                    ( 1 | uID/Order) ,
                  data = df_nalt[!df_nalt$outlier,],
                  control=lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(m_rt_base_log)
car::Anova(m_rt_base_log, type = 'III')
#Inversed RT
m_rt_base_inverse <- lmer(1/RT~ 
                    1/RT_lag +  ContextNum * EmotionNum * Rating+  
                    ( 1 | uID/Order) ,
                  data = df_nalt[!df_nalt$outlier,],
                  control=lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(m_rt_base_inverse)
car::Anova(m_rt_base_inverse, type = 'III')
#Scaled RT
m_rt_base_scale <- lmer(scale(RT)~ 
                            scale(RT_lag) +  ContextNum * EmotionNum * Rating+  
                            ( 1 | uID/Order) ,
                          data = df_nalt[!df_nalt$outlier,],
                          control=lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(m_rt_base_scale)
car::Anova(m_rt_base_scale, type = 'III')
#Scaled and inversed RT
m_rt_base_scale_invs <- lmer((1/scale(RT))~ 
                          (1/scale(RT_lag)) +  ContextNum * EmotionNum * Rating+  
                          ( 1 | uID/Order) ,
                        data = df_nalt[!df_nalt$outlier,],
                        control=lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(m_rt_base_scale_invs)
car::Anova(m_rt_base_scale_invs, type = 'III')

#Use character factors instead of numbers
m_rt_base_al <- lmer(1/scale(RT) ~ 
                      1/scale(RT_lag) +  ContextM * Emotion * Resp+  
                      ( 1 | uID/Order) ,
                    data = df_nalt[!df_nalt$outlier,],
                    control=lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(m_rt_base_al)
car::Anova(m_rt_base_al, type = 'III')
#RT basic+gender
m_rt_gender <- lmer(1/scale(RT) ~ 
                         1/scale(RT_lag) +  Gender * ContextNum * EmotionNum * Resp+  
                         ( 1 | uID/Order) ,
                       data = df_nalt[!df_nalt$outlier,],
                       control=lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(m_rt_gender)
car::Anova(m_rt_gender, type = 'III')
#RT with congruency
m_rt_congruent <- lmer(1/scale(RT) ~ 
                      1/scale(RT_lag) +  ifCongruent +  
                      ( 1 | uID/Order) ,
                    data = df_nalt[!df_nalt$outlier,],
                    control=lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(m_rt_congruent)
car::Anova(m_rt_congruent, type = 'III')
#RT with matching responses
m_rt_matchresp <- lmer(1/scale(RT) ~ 
                         1/scale(RT_lag) +  ifMatchResp +  
                         ( 1 | uID/Order) ,
                       data = df_nalt[!df_nalt$outlier,],
                       control=lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(m_rt_matchresp)
car::Anova(m_rt_matchresp, type = 'III')






