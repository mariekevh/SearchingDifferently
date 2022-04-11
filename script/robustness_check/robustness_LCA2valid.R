# Robustness check 
# looser selection criteria: 2 or 3 valid search queries 


library(tidyverse)
options(scipen=999)
# read data
data = read_tsv("/Users/marieke/SearchingForBias/data/immigration/df_analysis_2valid.csv")
data = data %>%
  select(ID, immigrant:asielzoeker, ADMISSION:STATISTICS)

cols = c("immigrant", "buitenlander", "gelukszoeker", "statushouder", 
         "allochtoon", "vluchteling", "expat", "asielzoeker", "ADMISSION", "AZC",
         "CAUSES", "CRIME", "CULTURE_RELIGION", "DEBATE", "ECONOMY", "EFFECTIVE", 
         "FINANCIAL_SUPPORT", "HOUSING", "INTEGRATION", "LANGUAGE", "NEWS", 
         "ORIGIN", "POLITICS", "PROBLEMS", "QUESTION", "RACISM", "SPECIFIC", "STATISTICS")   

# make vars from number (0,1) into factors (1,2)
data[cols] <- lapply(data[cols], as.factor)
# they are now factors.
sapply(data, class) 
summary(data)


########## LCA #############


# running LCA
library(poLCA)

# content variables (24 variables)
f <- cbind(immigrant, buitenlander, gelukszoeker, statushouder, allochtoon, 
           vluchteling, expat, asielzoeker,
           ADMISSION, AZC, CAUSES, CRIME, CULTURE_RELIGION, DEBATE, ECONOMY,
           FINANCIAL_SUPPORT, HOUSING, INTEGRATION, NEWS,
           ORIGIN, POLITICS, PROBLEMS, RACISM, STATISTICS)~1


# running lca for increasing number of clusters
LCAmodel2 <- poLCA(f, data, nclass=2, maxiter = 3000, nrep = 20, graphs=TRUE, na.rm=TRUE)
LCAmodel3 <- poLCA(f, data, nclass=3, maxiter = 3000, nrep = 20, graphs=TRUE, na.rm=TRUE)
LCAmodel4 <- poLCA(f, data, nclass=4, maxiter = 3000, nrep = 20, graphs=TRUE, na.rm=TRUE)
LCAmodel5 <- poLCA(f, data, nclass=5, maxiter = 3000, nrep = 20, graphs=TRUE, na.rm=TRUE)
LCAmodel6 <- poLCA(f, data, nclass=6, maxiter = 3000, nrep = 20, graphs=TRUE, na.rm=TRUE)
LCAmodel7 <- poLCA(f, data, nclass=7, maxiter = 3000, nrep = 20, graphs=TRUE, na.rm=TRUE)
LCAmodel8 <- poLCA(f, data, nclass=8, maxiter = 3000, nrep = 20, graphs=TRUE, na.rm=TRUE)

#####
#for table making purposes:
LCAmodel1 <- LCAmodel2


library(tidyLPA)
# calc_lrt --> null model = nested model (K-1), alternative model =  new model (K). 

# getting other fit indices in a table
tab.modfit<-data.frame(matrix(rep(999,6),nrow=1))
names(tab.modfit)<-c("resid. df", "log-likelihood",
                     "BIC",
                     "AIC", "aBIC", "LMR-LRT p-value")

for(i in 2:8){
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
tab.modfit$`LMR-LRT p-value`<-format(round(tab.modfit$`LMR-LRT p-value`, 3), nsmall=3)
tab.modfit$Nclass<-2:8
tab.modfit

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


## COMPARING MODEL SOLUTIONS ####
library(broom)
library(reshape2)


###### ADJUST THE FOLLOWING FOR WHICH MODEL SOLUTIONS ARE APPROPRIATE:

## 5-CLASS MODEL
#probabilities (i.e. estimated class-conditional item response probabilities)
probs = tidy(LCAmodel5)
probs <- filter(probs, outcome=="2")
keeps=c("variable", "class", "estimate", "std.error")
probs <- probs[keeps]
probs <- pivot_wider(probs, names_from = "class", values_from = c("estimate", "std.error"), names_sep="_")
probs[2:ncol(probs)] <- round(probs[2:ncol(probs)], 3)
probs

# make a graph of probabilities.
lc <- reshape2::melt(LCAmodel5$probs, level=2)
lc <- filter(lc, Var2 != "Pr(1)")
lc$Var1_f = as.factor(lc$Var1)

#levels(lc$Var1_f) = c("Asylum (19.5%)", "General (17.6%)", "Cultural (18.8%)", "Refugee (21.1%)", "Economic (23%)")
#lc$Var1_f <- factor(lc$Var1_f, levels=c("General (17.6%)", "Cultural (18.8%)", "Economic (23%)", "Asylum (19.5%)", "Refugee (21.1%)"))
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
zp1 <- zp1 + scale_x_discrete(labels=c("Immigrant", "Buitenlander", "Gelukszoeker", "Statushouder", "Allochtoon", "Vluchteling", "Expat", "Asielzoeker", "Admission", "Accommodation center", "Causes", "Crime", "Culture and religion", "Debate", "Economy", "Financial support", "Housing", "Integration", "News", "Origin", "Politics", "Problems", "Racism", "Statistics")) #rename x labels.
print(zp1)

