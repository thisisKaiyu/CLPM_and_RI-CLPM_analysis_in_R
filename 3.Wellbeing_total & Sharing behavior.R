# CLPM & RI-CLPM; Wellbeing_total and Sharing behavior
setwd("~/Documents/OneDrive - Vrije Universiteit Amsterdam/Research Project 1/Paper/Data&Scripts")
data <- read.csv("RP1_raw_data.csv", header = TRUE)

included <- data %>%
  dplyr::filter((apply(.[12:19] == -999,1,sum) <= 4 & apply(.[32:35] == -999,1,sum) <= 2) | #well-being(WB) & social mindfulness(SM)
                  (apply(.[12:19] == -999,1,sum) <= 4 & apply(.[36:39] == -999,1,sum) <= 2)) #well-being(WB) & sharing behaviors (DG)
#View(included)

# 3.CLPM & RI-CLPM####
### renaming variables ####
data <- included
colnames(data)
data[data == -999] <- NA
# Hypothesis1: Sharing Behaviors & Well Being ####
# a-sharing behaviors; m-well-being; 
data$WB041p_total <- data$WB041p_t001 + data$WB041p_t002
data$WB051p_total <- data$WB051p_t001 + data$WB051p_t002
data$WB061p_total <- data$WB061p_t001 + data$WB061p_t002
data$WB071p_total <- data$WB071p_t001 + data$WB071p_t002

## Wellbeing total
colnames(data)[which(colnames(data)=="WB041p_total")] <- "m1"
colnames(data)[which(colnames(data)=="WB051p_total")] <- "m2"
colnames(data)[which(colnames(data)=="WB061p_total")] <- "m3"
colnames(data)[which(colnames(data)=="WB071p_total")] <- "m4"

## sharing behaviors
colnames(data)[which(colnames(data)=="DG041p_cOTHs")] <- "a1"
colnames(data)[which(colnames(data)=="DG051p_cOTHs")] <- "a2"
colnames(data)[which(colnames(data)=="DG061p_cOTHs")] <- "a3"
colnames(data)[which(colnames(data)=="DG071p_cOTHs")] <- "a4"

#View(data)


# SEM with scaled varialbes #### 
# Get models:
source('SEM_basic_Mydata.R')

s_data <- data #duplicate the dataset before making rescaled variables


# sacleing data
# m-wellbeing;
s_data$m1 <- scale(s_data$m1,center = T,scale = T)
s_data$m2 <- scale(s_data$m2,center = T,scale = T)
s_data$m3 <- scale(s_data$m3,center = T,scale = T)
s_data$m4 <- scale(s_data$m4,center = T,scale = T)

# a-sharing behaviors
s_data$a1 <- scale(s_data$a1,center = T,scale = T)
s_data$a2 <- scale(s_data$a2,center = T,scale = T)
s_data$a3 <- scale(s_data$a3,center = T,scale = T)
s_data$a4 <- scale(s_data$a4,center = T,scale = T)
# View(s_data)


#### CROSS-LAGGED PANEL MODEL (VARIABLES RESCALED)
#
## CROSS-LAGGED PANEL MODEL (VARIABLES RESCALED) ####
fit_sCLPM<-cfa(CLPM,data=s_data,missing='fiml')
summary(fit_sCLPM, fit.measures = TRUE) 
# check modification indices
# modificationindices(fit_sCLPM) %>%
#   as_data_frame() %>%
#   arrange(-mi) %>%
#   filter(mi > 11) %>%
#   select(lhs, op, rhs, mi, epc) %>%
#   pander(caption="Largest MI values for scaled CLPM")


# Model 1 ("free" model) is the sCLPM (s = scaled data)
# fitMeasures(fit_sCLPM, c("chisq", "pvalue", "rmsea", "rmsea.ci.lower","rmsea.ci.upper","srmr"))
m1 <- fitMeasures(fit_sCLPM, c("chisq", "df","pvalue",'cfi','tli', "rmsea","srmr",'aic','bic'))

# Model 2a (unilateral with m -> a = 0)
fitCLPM_uni_2a<-cfa(CLPM_uni_2a,data=s_data, missing='fiml') 
summary(fitCLPM_uni_2a, fit.measures = TRUE) 
m2a <- fitMeasures(fitCLPM_uni_2a, c("chisq", "df","pvalue",'cfi','tli', "rmsea","srmr",'aic','bic'))

# Model 2b (unilateral with a -> m = 0)
fitCLPM_uni_2b<-cfa(CLPM_uni_2b,data=s_data, missing='fiml') 
summary(fitCLPM_uni_2b, fit.measures = TRUE) 
m2b <- fitMeasures(fitCLPM_uni_2b, c("chisq", "df","pvalue",'cfi','tli', "rmsea","srmr",'aic','bic'))

# Model 3 fully constraint, both directions = 0
fitCLPM_full<-cfa(CLPM_full,data=s_data, missing='fiml') 
summary(fitCLPM_full, fit.measures = TRUE)
m3 <- fitMeasures(fitCLPM_full, c("chisq", "df","pvalue",'cfi','tli', "rmsea","srmr",'aic','bic'))

sum_model_indices <- round(rbind(m1,m2a,m2b,m3),digits = 3)

# comparing step models (well-being item2 -> sharing behavior = 0, sharing behavior -> well-being item2 = 0, mutualistic)
anova(fitCLPM_uni_2a,fit_sCLPM) # non-sig difference, model can be reduced by well-being item2 -> sharing behavior = 0 without losing much structure, so no evidence that it is not 0, so we should fix it to 0
anova(fitCLPM_uni_2b,fit_sCLPM) # non-sig difference, model can be reduced by sharing behavior -> well-being item2 = 0 without losing much structure, so no evidence that it is not 0, so we should fix it to 0 
anova(fitCLPM_full,fit_sCLPM)

anova(fitCLPM_uni_2a,fitCLPM_full) 
anova(fitCLPM_uni_2b,fitCLPM_full) 
anova(fitCLPM_full,fit_sCLPM)

#### RANDOM-INTERCEPT CROSS-LAGGED PANEL MODEL (VARIABLES RESCALED)
#
## RANDOM-INTERCEPT CROSS-LAGGED PANEL MODEL (VARIABLES RESCALED) ####


# comparing models
library(lmtest)
lrtest(fit_sCLPM,fit_sRICLPM) # all significant improvements
anova(fit_sCLPM,fit_sRICLPM)

# check modification indices
# modificationindices(fit_sRICLPM) %>%
#   as_data_frame() %>%
#   arrange(-mi) %>%
#   filter(mi > 11) %>%
#   select(lhs, op, rhs, mi, epc) %>%
#   pander(caption="Largest MI values for scaled RI CLPM")


# Model 1 ("free" model) is the sRICLPM (s = scaled data)
fit_sRICLPM<-cfa(RICLPM,data=s_data,missing='fiml') #solved the issue of negative variances?
summary(fit_sRICLPM, fit.measures = TRUE) 
m1 <- fitMeasures(fit_sRICLPM, c("chisq", "df","pvalue",'cfi','tli', "rmsea","srmr",'aic','bic'))

# Model 2a (unilateral with m -> a = 0)
fitRICLPM_uni_2a<-cfa(RICLPM_uni_2a,data=s_data, missing='fiml') 
summary(fitRICLPM_uni_2a, fit.measures = TRUE) 
m2a <- fitMeasures(fitRICLPM_uni_2a, c("chisq", "df","pvalue",'cfi','tli', "rmsea","srmr",'aic','bic'))

# Model 2b (unilateral with a -> m = 0)
fitRICLPM_uni_2b<-cfa(RICLPM_uni_2b,data=s_data, missing='fiml') 
summary(fitRICLPM_uni_2b, fit.measures = TRUE) 
m2b <- fitMeasures(fitRICLPM_uni_2b, c("chisq", "df","pvalue",'cfi','tli', "rmsea","srmr",'aic','bic'))

# Model 3 fully constraint, both directions = 0
fitRICLPM_full<-cfa(RICLPM_full,data=s_data, missing='fiml') 
summary(fitRICLPM_full, fit.measures = TRUE)
m3 <- fitMeasures(fitRICLPM_full, c("chisq", "df","pvalue",'cfi','tli', "rmsea","srmr",'aic','bic'))

sum_model_indices <- round(rbind(m1,m2a,m2b,m3),digits = 3)

# comparing step models (well-being item2 -> sharing behavior = 0, sharing behavior -> well-being item2 = 0, mutualistic)
# lrtest(fitRICLPM_uni_2a,fit_sRICLPM) # non-sig difference, model can be reduced by well-being item2 -> sharing behavior = 0 without losing much structure, so no evidence that it is not 0, so we should fix it to 0
# lrtest(fitRICLPM_uni_2b,fit_sRICLPM) # non-sig difference, model can be reduced by sharing behavior -> well-being item2 = 0 without losing much structure, so no evidence that it is not 0, so we should fix it to 0 
anova(fitRICLPM_uni_2a,fit_sRICLPM)
anova(fitRICLPM_uni_2b,fit_sRICLPM)
anova(fitRICLPM_full,fit_sRICLPM)

anova(fitRICLPM_uni_2a,fitRICLPM_full)
anova(fitRICLPM_uni_2b,fitRICLPM_full)
anova(fitRICLPM_full,fit_sRICLPM)
