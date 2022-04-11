#### BINARY LOGITS! #####
library(tidyverse)
library(nnet)
library(mlogit)
library(stargazer)
library(margins)
library(lmtest)
library(DescTools)

# import data with predicted classes
data = read_csv("/Users/marieke/SearchingForBias/data/immigration/df_predclass.csv")

# import data with predictors
data_reg = read_csv("/Users/marieke/SearchingForBias/data/immigration/df_reg.csv")
# join dataframes
data_reg <- data_reg %>% left_join(data, by="ID")
data_reg$opl_3cat.f <- as.factor(data_reg$opl_3cat)

# make dummies
data_reg$asylum <- ifelse(data_reg$class5==1, 1, 0)
data_reg$general <- ifelse(data_reg$class5==2, 1, 0)
data_reg$cultural <- ifelse(data_reg$class5==3, 1, 0)
data_reg$refugees <- ifelse(data_reg$class5==4, 1, 0)
data_reg$economic <- ifelse(data_reg$class5==5, 1, 0)

# binomial logits (in log odds)
#m1
m1_a <- glm(asylum ~ att_im_mean + MVH_att_importance_1+ +base_intpol + opl_3cat.f + base_lft + base_gender2, data=data_reg, family='binomial')
m1_g <- glm(general ~ att_im_mean + MVH_att_importance_1+ +base_intpol + opl_3cat.f + base_lft + base_gender2, data=data_reg, family='binomial')
m1_c <- glm(cultural ~ att_im_mean + MVH_att_importance_1+ +base_intpol + opl_3cat.f + base_lft + base_gender2, data=data_reg, family='binomial')
m1_r <- glm(refugees ~ att_im_mean + MVH_att_importance_1+ +base_intpol + opl_3cat.f + base_lft + base_gender2, data=data_reg, family='binomial')
m1_e <- glm(economic ~ att_im_mean + MVH_att_importance_1+ +base_intpol + opl_3cat.f + base_lft + base_gender2, data=data_reg, family='binomial')
models1 <- list(m1_a, m1_g, m1_c, m1_r, m1_e)
#m2
m2_a <- glm(asylum ~ att_im_mean + MVH_att_importance_1 + att_im_mean*MVH_att_importance_1 + base_intpol + opl_3cat.f + base_lft+ base_gender2, data=data_reg, family='binomial')
m2_g <- glm(general ~ att_im_mean + MVH_att_importance_1 + att_im_mean*MVH_att_importance_1 + base_intpol + opl_3cat.f + base_lft+ base_gender2, data=data_reg, family='binomial')
m2_c <- glm(cultural ~ att_im_mean + MVH_att_importance_1 + att_im_mean*MVH_att_importance_1 + base_intpol + opl_3cat.f + base_lft+ base_gender2, data=data_reg, family='binomial')
m2_r <- glm(refugees ~ att_im_mean + MVH_att_importance_1 + att_im_mean*MVH_att_importance_1 + base_intpol + opl_3cat.f + base_lft+ base_gender2, data=data_reg, family='binomial')
m2_e <- glm(economic ~ att_im_mean + MVH_att_importance_1 + att_im_mean*MVH_att_importance_1 + base_intpol + opl_3cat.f + base_lft+ base_gender2, data=data_reg, family='binomial')
models2 <- list(m2_a, m2_g, m2_c, m2_r, m2_e)
#m3
m3_a <- glm(asylum ~ att_im_mean+ MVH_att_importance_1 + base_polar + base_intpol+ opl_3cat.f + base_lft+ base_gender2, data=data_reg, family='binomial')
m3_g <- glm(general ~ att_im_mean+ MVH_att_importance_1 + base_polar + base_intpol+ opl_3cat.f + base_lft+ base_gender2, data=data_reg, family='binomial')
m3_c <- glm(cultural ~ att_im_mean+ MVH_att_importance_1 + base_polar + base_intpol+ opl_3cat.f + base_lft+ base_gender2, data=data_reg, family='binomial')
m3_r <- glm(refugees ~ att_im_mean+ MVH_att_importance_1 + base_polar + base_intpol+ opl_3cat.f + base_lft+ base_gender2, data=data_reg, family='binomial')
m3_e <- glm(economic ~ att_im_mean+ MVH_att_importance_1 + base_polar + base_intpol+ opl_3cat.f + base_lft+ base_gender2, data=data_reg, family='binomial')
models3 <- list(m3_a, m3_g, m3_c, m3_r, m3_e)

## functions for OR transformation
get_pvals <- function(fm) {
  summary(fm)$coefficients[,4]
}
get_ORsevals <- function(fm) {
  exp(summary(fm)$coefficients[,1])*summary(fm)$coefficients[,2]
}

p.values1 <- lapply(models1, get_pvals)
se.values1 <- lapply(models1, get_ORsevals)
p.values2 <- lapply(models2, get_pvals)
se.values2 <- lapply(models2, get_ORsevals)
p.values3 <- lapply(models3, get_pvals)
se.values3 <- lapply(models3, get_ORsevals)

# output models (estimates presented as OR)
stargazer(models1, type='text', apply.coef=exp, p=p.values1, t.auto=F, p.auto=F, se=se.values1, star.cutoffs=c(.05, .01, .001), keep.stat=c("n", "adj.rsq"), model.numbers=F, dep.var.caption="")
stargazer(models2, type='text', apply.coef=exp, p=p.values2, t.auto=F, p.auto=F, se=se.values2, star.cutoffs=c(.05, .01, .001), keep.stat=c("n", "adj.rsq"), model.numbers=F, dep.var.caption="")
stargazer(models3, type='text', apply.coef=exp, p=p.values3, t.auto=F, p.auto=F, se=se.values3, star.cutoffs=c(.05, .01, .001), keep.stat=c("n", "adj.rsq"), model.numbers=F, dep.var.caption="")

# latex output tables
stargazer(models1, type='latex', apply.coef=exp, p=p.values1, t.auto=F, p.auto=F, se=se.values1, star.cutoffs=c(.05, .01, .001), keep.stat=c("n", "adj.rsq"), model.numbers=F, dep.var.caption="", out="/Users/marieke/SearchingForBias/report/tables/m1_logit_OR.tex")
stargazer(models2, type='latex', apply.coef=exp, p=p.values2, t.auto=F, p.auto=F, se=se.values2, star.cutoffs=c(.05, .01, .001), keep.stat=c("n", "adj.rsq"), model.numbers=F, dep.var.caption="", out="/Users/marieke/SearchingForBias/report/tables/m2_logit_OR.tex")
stargazer(models3, type='latex', apply.coef=exp, p=p.values3, t.auto=F, p.auto=F, se=se.values3, star.cutoffs=c(.05, .01, .001), keep.stat=c("n", "adj.rsq"), model.numbers=F, dep.var.caption="", out="/Users/marieke/SearchingForBias/report/tables/m3_logit_OR.tex")

## LR tests
lrtest(m1_a, m2_a)
lrtest(m1_g, m2_g)
lrtest(m1_c, m2_c)
lrtest(m1_r, m2_r)
lrtest(m1_e, m2_e)

lrtest(m1_a, m3_a)
lrtest(m1_g, m3_g)
lrtest(m1_c, m3_c)
lrtest(m1_r, m3_r)
lrtest(m1_e, m3_e)

# Pseudo R2 (Nagelkerke)
round(PseudoR2(m1_a, which="Nagelkerke"), 4)
round(PseudoR2(m2_a, which="Nagelkerke"), 4)
round(PseudoR2(m3_a, which="Nagelkerke"), 4)

round(PseudoR2(m1_g, which="Nagelkerke"), 4)
round(PseudoR2(m2_g, which="Nagelkerke"), 4)
round(PseudoR2(m3_g, which="Nagelkerke"), 4)

round(PseudoR2(m1_c, which="Nagelkerke"), 4)
round(PseudoR2(m2_c, which="Nagelkerke"), 4)
round(PseudoR2(m3_c, which="Nagelkerke"), 4)

round(PseudoR2(m1_r, which="Nagelkerke"), 4)
round(PseudoR2(m2_r, which="Nagelkerke"), 4)
round(PseudoR2(m3_r, which="Nagelkerke"), 4)

round(PseudoR2(m1_e, which="Nagelkerke"), 4)
round(PseudoR2(m2_e, which="Nagelkerke"), 4)
round(PseudoR2(m3_e, which="Nagelkerke"), 4)