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

m3 <- (lmer(Rating_w_bias ~ ContextNum*EmotionNum+DRUG*ContextNum+DRUG*EmotionNum+(1|uID/Order), df))
summary(m3)
car::Anova(m3, "III")
vif.lme(m3)

m3 <- glmer(Resp ~ Accuracy+ Context*Emotion+Drug*Emotion+Drug*Context+(1|uID), family=binomial,
           data = df4[which(!df4$misstrial | df4$outlier),] , control=glmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1000000)))
summary(m3)
car::Anova(m3, "III")
vif.lme(m3)

m3 <- lmer(Rating-(Accuracy-0.5) ~ Context*Emotion+Drug*Emotion+Drug*Context+(1|uID/Order),
             data = df4[which(!df4$misstrial | df4$outlier),] , control=lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1000000)))
summary(m3)
car::Anova(m3, "III")
vif.lme(m3)


m3alt <- lmer(p ~ Outscan_rate+Context*Emotion+Drug*Context+Drug*Emotion+(1|uID/Run), CF_P_ALL[CF_P_ALL$resp=="Positive",])
summary(m3alt)
car::Anova(m3alt, "III")
vif.lme(m3alt)

m3alt_php<-lmer( (p-Outscan_rate) ~ Context*Emotion*Drug + (1|uID/Run), CF_P_ALL[CF_P_ALL$resp=="Positive",])
summary(m3alt_php)
car::Anova(m3alt_php, "III")
vif.lme(m3alt_php)
plot(effect("Context",m3alt_php), grid=TRUE)

m31fpp<-lmer(p ~ Outscan_rate+Context*Emotion+Drug*Context+Drug*Emotion+(1|uID/Run), CF_P_ALL[CF_P_ALL$resp=="Positive",])

m31<-lmer(p ~ Outscan_rate+Context*Emotion+Drug*Context+Drug*Emotion+(1|uID/Run), CF_P_ALL[CF_P_ALL$resp=="Positive",])

m35 <- glmer(Switch ~ Context*Emotion+Drug*Emotion+Drug*Context+(1|uID), family=binomial,
            data = df5[which(! (df5$misstrial | df5$outlier | is.na(df5$Switch))),] , control=glmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1000000)))
summary(m35)
car::Anova(m35, "III")
vif.lme(m35)

f35a<-Resp ~ Outscan_Resp + Context*Emotion+Drug*Emotion+Drug*Context+(1|uID)
m35a <- glmer(Resp ~ Outscan_Resp + Context*Emotion+Drug*Emotion+Drug*Context+(1|uID), family=binomial,
             data = df5[which(! (df5$misstrial | df5$outlier | is.na(df5$Switch))),] , control=glmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1000000)))
summary(m35a)
car::Anova(m35a, "III")
vif.lme(m35a)
library(lsmeans)
ls_m35a <- lsmeans(m35a, "Emotion", by = "Context")
plot(ls_m35a, horiz = F)
ls_m35a <- lsmeans(m35a, "Emotion", by = c("Drug","Context"))
plot(ls_m35a, horiz = F)

ee<-Effect(focal.predictors = c("Emotion","Drug","Context"),mod = m35a)
ggplot(as.data.frame(ee), aes(x = interaction(Emotion,Context), y = fit, color=Drug)) + geom_point() + 
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.4) + 
  theme_bw(base_size=12) #+ facet_wrap(~Drug)





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
                (0 + Context:Emotion|uID|DRUG), 
              bs ~ 0 +  Context:Emotion + (0 + Context:Emotion|uID|DRUG), 
              ndt ~ 0 +  Context:Emotion + (0 + Context:Emotion|uID|DRUG),
              bias ~ 0 +  Context:Emotion + (0 + Context:Emotion|uID|DRUG))
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

fit_ddm1_all_drug <- brm(ddm1, 
                  data =df[!df$outlier,],
                  family = wiener(link_bs = "identity", 
                                  link_ndt = "identity",
                                  link_bias = "identity"),
                  prior = prior1, inits = initfun,
                  iter = 1000, warmup = 500, 
                  chains = 4, cores = 4, 
                  control = list(max_treedepth = 15))
pred_ddm1_all_drug <- predict(fit_ddm1_all_drug, 
                       summary = FALSE, 
                       negative_rt = TRUE, 
                       nsamples = 500)

save(fit_ddm1_all_drug, file = "ddm1_fit_all_drug.rda",compress = "xz")
save(pred_ddm1_all_drug, file = "ddm1_predictions_all_drug.rda", compress = "xz")


pars <- parnames(fit_ddm1)

plot(fit_ddm1, pars = pars[1:12], N = 12, 
     ask = FALSE, exact_match = TRUE, newpage = TRUE, plot = TRUE)



as.data.frame(df1)->df2



#ext$b->test
#ext$b_bs->test
#ext$b_ndt->test
ext$b_bias->test

as.data.frame(test)->test1
names(test1)<-c("ContextPleasant:EmotionFearful","ContextUnpleasant:EmotionFearful",
                "ContextPleasant:EmotionHappy","ContextUnpleasant:EmotionHappy",
                "ContextPleasant:EmotionNeutral","ContextUnpleasant:EmotionNeutral")
test1$iterations<-seq_along(test1[,1])
reshape2::melt(test1,id="iterations")->test_melt

test_melt$context<-NA
test_melt$context[grep("ContextPleasant",test_melt$variable)]<-"Pleasant"
test_melt$context[grep("ContextUnpleasant",test_melt$variable)]<-"Unpleasant"

test_melt$emotion<-NA
test_melt$emotion[grep("EmotionHappy",test_melt$variable)]<-"Happy"
test_melt$emotion[grep("EmotionFearful",test_melt$variable)]<-"Fearful"
test_melt$emotion[grep("EmotionNeutral",test_melt$variable)]<-"Neutral"

ggplot(test_melt, aes(x=emotion, y=value, fill=context)) + geom_boxplot() + ylab("initial bias")

df<-df[order(df$Participant),]
condition=c("DRUG","Emotion","Context")
do.call(rbind,lapply(split(df,df$uID), function(x){
  ls<-list()
  
  for (ix in condition) {
    for (iz in unique(x[[ix]])){
      
    }
  }

}))

df$R_b_center<-scale((df$Rating_w_bias),center = T)
ggplot(df,mapping = aes(x=Emotion,y=p,color=Context)) + geom_violin() + facet_wrap( ~resp+Drug, ncol=2)


ggplot(df4[which(!df4$misstrial | df4$outlier),],mapping = aes(x=Emotion,y=Rating,color=Context))  + facet_wrap( ~Drug, ncol=2) +
  stat_summary(fun.y=mean, geom="bar",alpha=0.2) +
  stat_summary(fun.data = mean_cl_boot,geom="errorbar", width=0.1) 

data.frame(x=df4$Emotion,y=df4$Rating,y1=df4$Rating,y2=df4$Accuracy)



atest<-rbind(
data.frame(trial=1:1000, Rating=rbinom(1000,1,0.5),Emotion="Netural",Accuracy=0.5,Context="Pleasant"),
data.frame(trial=1001:2000, Rating=rbinom(1000,1,0.85),Emotion="Happy",Accuracy=0.80,Context="Pleasant"),
data.frame(trial=2001:3000, Rating=rbinom(1000,1,0.25),Emotion="Fearful",Accuracy=0.30,Context="Pleasant"),
data.frame(trial=1:1000, Rating=rbinom(1000,1,0.35),Emotion="Netural",Accuracy=0.5,Context="Unpleasant"),
data.frame(trial=1001:2000, Rating=rbinom(1000,1,0.60),Emotion="Happy",Accuracy=0.8,Context="Unpleasant"),
data.frame(trial=2001:3000, Rating=rbinom(1000,1,0.2),Emotion="Fearful",Accuracy=0.30,Context="Unpleasant")
)



ggplot(atest,mapping = aes(x=Emotion,y=Rating - (Accuracy-0.5),color=Context))  +
  stat_summary(fun.y=mean, geom="bar",alpha=0.2) +
  stat_summary(fun.data = mean_cl_boot,geom="errorbar", width=0.1) 


#Resp ~ Outscan_Resp + Context*Emotion+Drug*Emotion+Drug*Context+(1|uID), family=binomial,
data = df5[which(! (df5$misstrial | df5$outlier | is.na(df5$Switch))),]


ggplot(df5[which(!df5$misstrial | df5$outlier),],mapping = aes(x=Emotion,y=as.numeric(Resp),color=Emotion))  + facet_wrap( ~Context+Drug, ncol=2) +
  stat_summary(fun.y=mean, geom="bar",alpha=0.2) +
  stat_summary(fun.data = mean_cl_boot,geom="errorbar", width=0.1) 

df5<-df5[order(df5$ID)]
df5_s<-split(df5,df5$uID)
df5_p<-lapply(df5_s,genProbability,response = "Resp",
       condition = c("Context","Emotion","Drug"),excludeNA = T,missresp = NA,IDvar="uID")
df5_ap<-do.call(rbind,df5_p)
df5_ap<-df5_ap[df5_ap$resp=="Positive",]
cf_roi_all<-do.call(rbind,CF_roi_split)
alldf<-merge(df5_ap,cf_roi_all,by.x = c("ID","Drug"),by.y = c("uID","Drug"),all = T)
plot(x = na.omit(alldf$p[alldf$Emotion=="Fearful" & alldf$Context=="Pleasant" & alldf$Drug=="Nalt"]-alldf$p[alldf$Emotion=="Neutral" & alldf$Context=="Pleasant" & alldf$Drug=="Nalt"]),
     y = na.omit( (alldf$p_f.Caudate_L_71[alldf$Emotion=="Fearful" & alldf$Context=="Pleasant" & alldf$Drug=="Nalt"]) -  (alldf$p_n.Caudate_L_71[alldf$Emotion=="Neutral" & alldf$Context=="Pleasant" & alldf$Drug=="Nalt"])) )

plot(x = na.omit(alldf$p[alldf$Emotion=="Happy" & alldf$Context=="Pleasant" & alldf$Drug=="Plac"]-alldf$p[alldf$Emotion=="Happy" & alldf$Context=="Pleasant" & alldf$Drug=="Plac"]),
     y = na.omit( (alldf$p_h.Caudate_L_71[alldf$Emotion=="Happy" & alldf$Context=="Pleasant" & alldf$Drug=="Plac"]) -  (alldf$p_n.Caudate_L_71[alldf$Emotion=="Happy" & alldf$Context=="Pleasant" & alldf$Drug=="Plac"])) )

