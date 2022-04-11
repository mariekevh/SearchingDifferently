# Climate Change search queries: LCA + Logits


## 1. LCA ANALYSIS

library(tidyverse)
options(scipen=999)
# read data
data = read_tsv("/Users/marieke/SearchingForBias/data/climate/df_analysis.csv")
#data = data %>%
  #select(ID, DICUSSIE:VRAAG)

cols = c("ACTIVISME", "CO2", "DISCUSSIE", "ENERGIE", "GEVOLG", "INDIVIDUEEL_GEDRAG",
         "INDUSTRIE", "INFO_OPLOSSINGEN", "INFO_STATUS_QUO", "MILIEU", "OORZAKEN",
         "POLITIEK_BELEID", "SCEPTICI", "SCEPTICI_PERSONEN", "STIKSTOF", "VERVOER", 
         "VOEDSELINDUSTRIE")

# make vars from number (0,1) into factors (1,2)
data[cols] <- lapply(data[cols], as.factor)
# they are now factors.
sapply(data, class) 
summary(data)

# running LCA
library(poLCA)

# content variables (XXX variables)
f <- cbind(ACTIVISME, CO2, DISCUSSIE, ENERGIE, GEVOLG, INDIVIDUEEL_GEDRAG,
           INDUSTRIE, INFO_OPLOSSINGEN, INFO_STATUS_QUO, MILIEU, OORZAKEN,
           POLITIEK_BELEID, SCEPTICI, SCEPTICI_PERSONEN, STIKSTOF, VERVOER, 
           VOEDSELINDUSTRIE)~1

# running lca for increasing number of clusters
LCAmodel2 <- poLCA(f, data, nclass=2, maxiter = 3000, nrep = 20, graphs=TRUE, na.rm=TRUE)
LCAmodel3 <- poLCA(f, data, nclass=3, maxiter = 3000, nrep = 20, graphs=TRUE, na.rm=TRUE)
LCAmodel4 <- poLCA(f, data, nclass=4, maxiter = 3000, nrep = 20, graphs=TRUE, na.rm=TRUE)
LCAmodel5 <- poLCA(f, data, nclass=5, maxiter = 3000, nrep = 20, graphs=TRUE, na.rm=TRUE)
LCAmodel6 <- poLCA(f, data, nclass=6, maxiter = 3000, nrep = 20, graphs=TRUE, na.rm=TRUE)
LCAmodel7 <- poLCA(f, data, nclass=7, maxiter = 3000, nrep = 20, graphs=TRUE, na.rm=TRUE)
LCAmodel8 <- poLCA(f, data, nclass=8, maxiter = 3000, nrep = 20, graphs=TRUE, na.rm=TRUE)
LCAmodel9 <- poLCA(f, data, nclass=9, maxiter = 3000, nrep = 20, graphs=TRUE, na.rm=TRUE)
LCAmodel10 <- poLCA(f, data, nclass=10, maxiter = 3000, nrep = 20, graphs=TRUE, na.rm=TRUE)

#storing model output
saveRDS(LCAmodel2, file =  "/Users/marieke/SearchingForBias/data/climate/LCAmodels/LCAmodel2.rds")
saveRDS(LCAmodel3, file =  "/Users/marieke/SearchingForBias/data/climate/LCAmodels/LCAmodel3.rds")
saveRDS(LCAmodel4, file =  "/Users/marieke/SearchingForBias/data/climate/LCAmodels/LCAmodel4.rds")
saveRDS(LCAmodel5, file =  "/Users/marieke/SearchingForBias/data/climate/LCAmodels/LCAmodel5.rds")
saveRDS(LCAmodel6, file =  "/Users/marieke/SearchingForBias/data/climate/LCAmodels/LCAmodel6.rds")
saveRDS(LCAmodel7, file =  "/Users/marieke/SearchingForBias/data/climate/LCAmodels/LCAmodel7.rds")
saveRDS(LCAmodel8, file =  "/Users/marieke/SearchingForBias/data/climate/LCAmodels/LCAmodel8.rds")
saveRDS(LCAmodel9, file =  "/Users/marieke/SearchingForBias/data/climate/LCAmodels/LCAmodel9.rds")
saveRDS(LCAmodel10, file = "/Users/marieke/SearchingForBias/data/climate/LCAmodels/LCAmodel10.rds")
#for table making purposes:
LCAmodel1 <- LCAmodel2

library(tidyLPA)

# getting other fit indices in a table
tab.modfit<-data.frame(matrix(rep(999,6),nrow=1))
names(tab.modfit)<-c("resid. df", "log-likelihood",
                     "BIC",
                     "AIC", "aBIC", "LMR-LRT p-value")

for(i in 2:10){
  tab.modfit<-rbind(tab.modfit,
                    c(get(paste("LCAmodel",i,sep=""))$resid.df,
                      get(paste("LCAmodel",i,sep=""))$llik,
                      get(paste("LCAmodel",i,sep=""))$bic,
                      get(paste("LCAmodel",i,sep=""))$aic,
                      (-2*get(paste("LCAmodel",i,sep=""))$llik) +
                        ((log((get(paste("LCAmodel",i,sep=""))$N + 2)/24)) *
                           get(paste("LCAmodel",i,sep=""))$npar),
                      calc_lrt(get(paste("LCAmodel",i,sep=""))$N, get(paste("LCAmodel",i-1,sep=""))$llik, get(paste("LCAmodel",i-1,sep=""))$npar, i-1, get(paste("LCAmodel",i,sep=""))$llik, get(paste("LCAmodel",i,sep=""))$npar, i)[4]
                    ))
}

#tab.modfit<-round(tab.modfit[-1,],3)
tab.modfit<-tab.modfit[-1,]
tab.modfit$`LMR-LRT pvalue`<-format(round(tab.modfit$`LMR-LRT p-value`, 3), nsmall=3)
tab.modfit$Nclass<-2:10
tab.modfit

library(xtable)
print(xtable(tab.modfit), type="latex", file="/Users/marieke/SearchingForBias/report/tables/climate/model_fit.tex")


# screeplots
# convert table into long format
library("forcats")
tab.modfit$Nclass <-as.factor(tab.modfit$Nclass)
tab.modfit
results2<-tidyr::gather(tab.modfit,label,value,2:5)
results2$value <- as.numeric(results2$value)

# pass long-format table on to ggplot
library(ggplot2)
fit.plot<-ggplot(results2) +
  geom_point(aes(x=Nclass,y=value),size=2) +
  geom_line(aes(Nclass, value, group = 1)) +
  theme_bw()+
  labs(x = "Number of classes", y="", title = "") +
  facet_grid(label ~. ,scales = "free") +
  theme_bw(base_size = 10, base_family = "") #+
fit.plot

ggsave("/Users/marieke/SearchingForBias/report/figures/climate/screeplot_modfit.png")


## COMPARING MODEL SOLUTIONS ####
library(broom)


## 4-CLASS MODEL
#probabilities (i.e. estimated class-conditional item response probabilities)
probs = tidy(LCAmodel4)
probs <- filter(probs, outcome=="2")
keeps=c("variable", "class", "estimate", "std.error")
probs <- probs[keeps]
probs <- pivot_wider(probs, names_from = "class", values_from = c("estimate", "std.error"), names_sep="_")
probs[2:ncol(probs)] <- round(probs[2:ncol(probs)], 3)
probs
print(xtable(probs), type="latex", file="/Users/marieke/SearchingForBias/report/tables/climate/class4_probs.tex")

# make a graph of probabilities.
lc <- reshape2::melt(LCAmodel4$probs, level=2)
lc <- filter(lc, Var2 != "Pr(1)")
lc$Var1_f = as.factor(lc$Var1)
levels(lc$Var1_f) = c("Solutions (12.1%)", "Politics & Information (33.5%)", "Consequences (34.3%)", "Factors & Actors (20.2%)")
lc$Var1_f <- factor(lc$Var1_f, levels=c("Solutions (12.1%)", "Politics & Information (33.5%)", "Consequences (34.3%)", "Factors & Actors (20.2%)"))
average <- group_by(lc, L2) %>% summarize(m=mean(value))
lc <- lc %>% left_join(average)
#lc$lowhigh <- ifelse(lc$value > lc$m, 'high', 'low')
zp1 <- ggplot(lc,aes(x = fct_inorder(L2), y = value)) #, fill=L2))
zp1 <- zp1 + geom_bar(stat = "identity", alpha=0.7) #, fill='grey') #, position = "stack")
zp1 <- zp1 + geom_point(aes(x=fct_inorder(L2), y=m), size=0.5, show.legend = FALSE)
zp1 <- zp1 + facet_wrap(Var1_f ~ ., ncol=1) #, scales = 'free_y') 
#zp1 <- zp1 + facet_grid(Var1_f ~ .) #, scales = 'free_y') 
#zp1 <- zp1 + scale_fill_brewer(type="seq") #, palette="Blues") #, labels = c("0 (not present)", "1 (present)")) +theme_bw()
zp1 <- zp1 + labs(x = "Search query indicators",y="Class-conditional item probabilities", fill ="")
zp1 <- zp1 + theme_minimal()
zp1 <- zp1 + theme(panel.grid.major.y=element_blank(),
                   axis.text.x=element_text(angle=90, vjust=0.5, hjust=1),
                   legend.position="right")
#zp1 <- zp1 + guides(fill = guide_legend(reverse=TRUE))
zp1 <- zp1 + scale_fill_grey()
zp1 <- zp1 + scale_x_discrete(labels=c("Activism", "Carbon", "Debate", "Energy", "Consequences", "Individiual behaviour", "Industry", "Solutions", "Trends and current state", "Environmental problems", "Causes", "Politics and policy", "Climate change denial", "Climate change deniers", "Nitrogen", "Transport", "Agriculture"))
print(zp1)
ggsave("/Users/marieke/SearchingForBias/report/figures/climate/class4_probs.png")

#output predicted classes from selected models so that we can use it in subsequent analyses:
data$class4=LCAmodel4$predclass 
data$class5=LCAmodel5$predclass
data$class6=LCAmodel6$predclass 
data$class7=LCAmodel7$predclass

# REGRESSION MODELS
library(tidyverse)
#library(nnet)
#library(mlogit)
library(stargazer)
library(margins)
library(lmtest)
library(DescTools)

# import data with predictors
data_reg = read_csv("/Users/marieke/SearchingForBias/data/climate/df_reg.csv")
# join dataframes
data_reg <- data_reg %>% left_join(data, by="ID")
data_reg$opl_3cat.f <- as.factor(data_reg$opl_3cat)

# make dummies
data_reg$cc_1 <- ifelse(data_reg$class4==1, 1, 0)
data_reg$cc_2 <- ifelse(data_reg$class4==2, 1, 0)
data_reg$cc_3 <- ifelse(data_reg$class4==3, 1, 0)
data_reg$cc_4 <- ifelse(data_reg$class4==4, 1, 0)

## functions for OR transformation
get_pvals <- function(fm) {
  summary(fm)$coefficients[,4]
}
get_ORsevals <- function(fm) {
  exp(summary(fm)$coefficients[,1])*summary(fm)$coefficients[,2]
}

#m1
m1_1 <- glm(cc_1 ~ att_cc_mean + MVH_att_importance_2+ +base_intpol + opl_3cat.f + base_lft + base_gender2, data=data_reg, family='binomial')
m1_2 <- glm(cc_2 ~ att_cc_mean + MVH_att_importance_2+ +base_intpol + opl_3cat.f + base_lft + base_gender2, data=data_reg, family='binomial')
m1_3 <- glm(cc_3 ~ att_cc_mean + MVH_att_importance_2+ +base_intpol + opl_3cat.f + base_lft + base_gender2, data=data_reg, family='binomial')
m1_4 <- glm(cc_4 ~ att_cc_mean + MVH_att_importance_2+ +base_intpol + opl_3cat.f + base_lft + base_gender2, data=data_reg, family='binomial')
models1 <- list(m1_1, m1_2, m1_3, m1_4)
#m2
m2_1 <- glm(cc_1 ~ att_cc_mean + MVH_att_importance_2 + att_cc_mean*MVH_att_importance_2 + base_intpol + opl_3cat.f + base_lft+ base_gender2, data=data_reg, family='binomial')
m2_2 <- glm(cc_2 ~ att_cc_mean + MVH_att_importance_2 + att_cc_mean*MVH_att_importance_2 + base_intpol + opl_3cat.f + base_lft+ base_gender2, data=data_reg, family='binomial')
m2_3 <- glm(cc_3 ~ att_cc_mean + MVH_att_importance_2 + att_cc_mean*MVH_att_importance_2 + base_intpol + opl_3cat.f + base_lft+ base_gender2, data=data_reg, family='binomial')
m2_4 <- glm(cc_4 ~ att_cc_mean + MVH_att_importance_2 + att_cc_mean*MVH_att_importance_2 + base_intpol + opl_3cat.f + base_lft+ base_gender2, data=data_reg, family='binomial')
models2 <- list(m2_1, m2_2, m2_3, m2_4)
#m3
m3_1 <- glm(cc_1 ~ att_cc_mean+ MVH_att_importance_2 + base_polar + base_intpol+ opl_3cat.f + base_lft+ base_gender2, data=data_reg, family='binomial')
m3_2 <- glm(cc_2 ~ att_cc_mean+ MVH_att_importance_2 + base_polar + base_intpol+ opl_3cat.f + base_lft+ base_gender2, data=data_reg, family='binomial')
m3_3 <- glm(cc_3 ~ att_cc_mean+ MVH_att_importance_2 + base_polar + base_intpol+ opl_3cat.f + base_lft+ base_gender2, data=data_reg, family='binomial')
m3_4 <- glm(cc_4 ~ att_cc_mean+ MVH_att_importance_2 + base_polar + base_intpol+ opl_3cat.f + base_lft+ base_gender2, data=data_reg, family='binomial')
models3 <- list(m3_1, m3_2, m3_3, m3_4)

p.values1 <- lapply(models1, get_pvals)
se.values1 <- lapply(models1, get_ORsevals)
p.values2 <- lapply(models2, get_pvals)
se.values2 <- lapply(models2, get_ORsevals)
p.values3 <- lapply(models3, get_pvals)
se.values3 <- lapply(models3, get_ORsevals)

# output models (estimates presented as OR)
stargazer(models1, type='text', apply.coef=exp, p=p.values1, t.auto=F, p.auto=F, se=se.values1, star.cutoffs=c(.05, .01, .001), keep.stat=c("n", "adj.rsq"), model.numbers=F, dep.var.caption="")
stargazer(models2, type='text', apply.coef=exp, p=p.values2, t.auto=F, p.auto=F, se=se.values2, star.cutoffs=c(.05, .01, .001), keep.stat=c("n", "adj.rsq"), model.numbers=F, dep.var.caption="")
stargazer(models3, type='text', apply.coef=exp, p=p.values3, t.auto=F, p.auto=F, se=se.values3, star.cutoffs=c(.05, .01, .001), keep.stat=c("n", "adj.rsq"), model.numbers=F, dep.var.caption = "")

# latex output tables
stargazer(models1, type='latex', apply.coef=exp, p=p.values1, t.auto=F, p.auto=F, se=se.values1, star.cutoffs=c(.05, .01, .001), keep.stat=c("n", "adj.rsq"), model.numbers=F, dep.var.caption="", out="/Users/marieke/SearchingForBias/report/tables/climate/m1_logit_OR.tex")
stargazer(models2, type='latex', apply.coef=exp, p=p.values2, t.auto=F, p.auto=F, se=se.values2, star.cutoffs=c(.05, .01, .001), keep.stat=c("n", "adj.rsq"), model.numbers=F, dep.var.caption="", out="/Users/marieke/SearchingForBias/report/tables/climate/m2_logit_OR.tex")
stargazer(models3, type='latex', apply.coef=exp, p=p.values3, t.auto=F, p.auto=F, se=se.values3, star.cutoffs=c(.05, .01, .001), keep.stat=c("n", "adj.rsq"), model.numbers=F, dep.var.caption="", out="/Users/marieke/SearchingForBias/report/tables/climate/m3_logit_OR.tex")

## LR tests
lrtest(m1_1, m2_1)
lrtest(m1_2, m2_2)
lrtest(m1_3, m2_3)
lrtest(m1_4, m2_4)

lrtest(m1_1, m3_1)
lrtest(m1_2, m3_2)
lrtest(m1_3, m3_3)
lrtest(m1_4, m3_4)

# Pseudo R2 (Nagelkerke)
round(PseudoR2(m1_1, which="Nagelkerke"), 4)
round(PseudoR2(m2_1, which="Nagelkerke"), 4)
round(PseudoR2(m3_1, which="Nagelkerke"), 4)

round(PseudoR2(m1_2, which="Nagelkerke"), 4)
round(PseudoR2(m2_2, which="Nagelkerke"), 4)
round(PseudoR2(m3_2, which="Nagelkerke"), 4)

round(PseudoR2(m1_3, which="Nagelkerke"), 4)
round(PseudoR2(m2_3, which="Nagelkerke"), 4)
round(PseudoR2(m3_3, which="Nagelkerke"), 4)

round(PseudoR2(m1_4, which="Nagelkerke"), 4)
round(PseudoR2(m2_4, which="Nagelkerke"), 4)
round(PseudoR2(m3_4, which="Nagelkerke"), 4)
