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

df<-merge(CF_ALL,CF_outscan_ALL,by = "uID",all = T)
df$hasoutscan<-FALSE
df$hasoutscan[which(!is.na(df$per_miss))]<-TRUE

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

m3 <- (glmer(Rating ~ Context*EmotionNum+DRUG+(1|Participant/Order), family=binomial, df))
summary(m3)
car::Anova(m3, "III")
vif.lme(m3)
#RT model
m4 <- (glmer(Rating ~ Context*EmotionNum*DRUG+scale(RT)+(1|Participant/Order), family=binomial, df))
summary(m4)
car::Anova(m4, "III")

m5 <- (glmer(Rating ~ Context*EmotionNum+(1|uID/Order/DRUG), family=binomial, df))
summary(m5)
car::Anova(m5, "III")

m2a <- (glmer(Rating ~ ContextNum*EmotionNum + (1|uID/Order), family=binomial, df))
summary(m2a)
car::Anova(m2a, "III")

m4x <- (glmer(Rating ~ Context*EmotionNum*DRUG+p_neu+scale(RT)+(1|uID/Order), family=binomial, df[which(df$hasoutscan),]))
summary(m4x)
car::Anova(m4x, "III")

m4y <- (glmer(Rating ~ Context*EmotionNum+DRUG*Context+DRUG*EmotionNum+p_neu+(1|uID/Order), family=binomial, df[which(df$hasoutscan),]))
summary(m4y)
car::Anova(m4y, "III")

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

stop("DON'T AUTO RUN FURTHER, BELOW ARE DANGER ZONE")
###############DECISION DIFFUSION MODEL ANALYSIS########################
#Drift rate can be influence by pre decision information and current task manipulation *context and faces
#boundary separation, starting bias, and non-decision time are only allow to vary by context because participant has no idea walking into what face in a given trial
save_all <- function() {
  save.image("recover.rda")
}
options(error = save_all)

library(brms)

ddm1 <- bf(RT | dec(decision) ~ 0 + Context:Emotion + 
                (0 + Context:Emotion|p|ID), 
              bs ~ 0 +  Context:Emotion + (0 + Context:Emotion|p|ID), 
              ndt ~ 0 +  Context:Emotion + (0 + Context:Emotion|p|ID),
              bias ~ 0 +  Context:Emotion + (0 + Context:Emotion|p|ID))
#p is used to do full model random effects

#Non discriminatory priors; no bias, weakly informative
prior1 <- c(
  prior("cauchy(0, 5)", class = "b"),
  set_prior("normal(1.5, 1)", class = "b", dpar = "bs"),
  set_prior("normal(0.2, 0.1)", class = "b", dpar = "ndt"),
  set_prior("normal(0.5, 0.2)", class = "b", dpar = "bias")
)

make_stancode(ddm1, 
              family = wiener(link_bs = "identity", 
                              link_ndt = "identity",
                              link_bias = "identity"),
              data = df[!df$outlier,], 
              prior = prior1)


tmp_dat <- make_standata(ddm1, 
                         family = wiener(link_bs = "identity", 
                                         link_ndt = "identity",
                                         link_bias = "identity"),
                         data = df[!df$outlier,], prior = prior1)
#Assign initiation function:
initfun <- function() {
  list(
    b = rnorm(tmp_dat$K),
    b_bs = runif(tmp_dat$K_bs, 1, 2),
    b_ndt = runif(tmp_dat$K_ndt, 0.15, 0.2461),
    b_bias = rnorm(tmp_dat$K_bias, 0.5, 0.1),
    sd_1 = runif(tmp_dat$M_1, 0.5, 1),
    z_1 = matrix(rnorm(tmp_dat$M_1*tmp_dat$N_1, 0, 0.01),
                 tmp_dat$M_1, tmp_dat$N_1),
    L_1 = diag(tmp_dat$M_1)
  )
}

fit_ddm1_all <- brm(ddm1, 
                  data =df[!df$outlier,],
                  family = wiener(link_bs = "identity", 
                                  link_ndt = "identity",
                                  link_bias = "identity"),
                  prior = prior1, inits = initfun,
                  iter = 1000, warmup = 500, 
                  chains = 4, cores = 4, 
                  control = list(max_treedepth = 15))
pred_ddm1_all <- predict(fit_ddm1_all, 
                       summary = FALSE, 
                       negative_rt = TRUE, 
                       nsamples = 500)

save(fit_ddm1_all, file = "ddm1_fit_all.rda",compress = "xz")
save(pred_ddm1_all, file = "ddm1_predictions_all.rda", compress = "xz")


pars <- parnames(fit_ddm1)

plot(fit_ddm1, pars = pars[1:12], N = 12, 
     ask = FALSE, exact_match = TRUE, newpage = TRUE, plot = TRUE)




