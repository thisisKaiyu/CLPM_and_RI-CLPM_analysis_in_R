#' title: "RP1_Kaiyu_Huang: SEM ANALYSES"
#' date "`r format(Sys.time(), '%d %B, %Y')`"
#' 
#' # RP1_Kaiyu_Huang: SEM ANALYSES
#' 
#+ setup, include = FALSE
knitr::opts_chunk$set(error = TRUE, warning = FALSE, message = FALSE, tidy = TRUE)

# Codes were adapted from those of Usami, Murayama & Hamaker (2019) and Nunez-Regueiro., F., Juhel, J., Bressoux, P., & Nurra, C. (2022).


rm(list = ls ()) #clean up working space
# manually change the working directory the Code one
setwd("~/Documents/OneDrive - Vrije Universiteit Amsterdam/Research Project 1/Paper/Data&Scripts/")


library(lavaan)
library(tidyverse)
library(rlang)
library(papaja)


data <- read.csv("RP1_raw_data.csv", header = TRUE)
#View(data)
# 1.data preprocessing####
# filter data
#    Only the children who had at least one repeated assessment of Well-being and DG/SoMi1 were included, 
#    which resulted in the exclusion of *** children and a final sample of *** children in the analyses.
# It means that from column 'WB041p_t001' to 'WB071p_t002', each participant has at least 3 columns of effective data;
# or 2 columns from DG041p_cOTHs to DG071p_cOTHs
# or 2 columns from SM041p_cSMSm to SM071p_cSMSm
nrow(data)
included <- data %>%
  dplyr::filter((apply(.[12:19] == -999,1,sum) <= 4 & apply(.[32:35] == -999,1,sum) <= 2) | #well-being(WB) & social mindfulness(SM)
                  (apply(.[12:19] == -999,1,sum) <= 4 & apply(.[36:39] == -999,1,sum) <= 2)) #well-being(WB) & sharing behaviors (DG)
View(included)
nrow(included)

excluded <- data %>%
  dplyr::filter(!(apply(.[12:19] == -999,1,sum) <= 4 & apply(.[32:35] == -999,1,sum) <= 2) & 
                  !(apply(.[12:19] == -999,1,sum) <= 4 & apply(.[36:39] == -999,1,sum) <= 2)) 
View(excluded)
nrow(excluded)

# compare gender proportion between included and excluded
table(included$MALE)
table(excluded$MALE)
i <- table(included$MALE)
e <- table(excluded$MALE)
prop.test(i,e)

# compare Well-being (across four waves on average)  between included and excluded
df_included <- included[12:19]
df_included[df_included == -999] <- NA
individual_mean_included <-
  as.array(apply(df_included,1,mean,na.rm = TRUE))
mean(individual_mean_included,na.rm = TRUE)
sd(individual_mean_included,na.rm = TRUE)
df_excluded <- excluded[12:19]
df_excluded[df_excluded == -999] <- NA
individual_mean_excluded <-
  as.array(apply(df_excluded,1,mean,na.rm = TRUE))
mean(individual_mean_excluded,na.rm = TRUE)
sd(individual_mean_excluded,na.rm = TRUE)
t.test(individual_mean_included,individual_mean_excluded,
       var.equal = TRUE)
cohen.d(individual_mean_included,individual_mean_excluded,
        var.equal = TRUE,na.rm = TRUE)

# compare SoMi (across four waves on average)  between included and excluded
df_included <- included[32:35]
df_included[df_included == -999] <- NA
individual_mean_included <-
  as.array(apply(df_included,1,mean,na.rm = TRUE))
mean(individual_mean_included,na.rm = TRUE)
sd(individual_mean_included,na.rm = TRUE)
df_excluded <- excluded[32:35]
df_excluded[df_excluded == -999] <- NA
individual_mean_excluded <-
  as.array(apply(df_excluded,1,mean,na.rm = TRUE))
mean(individual_mean_excluded,na.rm = TRUE)
sd(individual_mean_excluded,na.rm = TRUE)
t.test(individual_mean_included,individual_mean_excluded,
       var.equal = TRUE)
cohen.d(individual_mean_included,individual_mean_excluded,
        var.equal = TRUE,na.rm = TRUE)

# compare DG(sharing behaviors) (across four waves on average)  between included and excluded
df_included <- included[36:39]
df_included[df_included == -999] <- NA
individual_mean_included <-
  as.array(apply(df_included,1,mean,na.rm = TRUE))
mean(individual_mean_included,na.rm = TRUE)
sd(individual_mean_included,na.rm = TRUE)
df_excluded <- excluded[36:39]
df_excluded[df_excluded == -999] <- NA
individual_mean_excluded <-
  as.array(apply(df_excluded,1,mean,na.rm = TRUE))
mean(individual_mean_excluded,na.rm = TRUE)
sd(individual_mean_excluded,na.rm = TRUE)
t.test(individual_mean_included,individual_mean_excluded,
       var.equal = TRUE)
cohen.d(individual_mean_included,individual_mean_excluded,
        var.equal = TRUE,na.rm = TRUE)

# 2.descriptive statistics ####
data <- included
# calculate mean age (SD) of all participants at T1,T2,T3,T4
mean_T1 <- mean(data$AGE_41p[data$AGE_41p != -999])
sd_T1 <- sd(data$AGE_41p[data$AGE_41p != -999])
mean_T2 <- mean(data$AGE_51p[data$AGE_51p != -999])
sd_T2 <- sd(data$AGE_51p[data$AGE_51p != -999])
mean_T3 <- mean(data$AGE_61p[data$AGE_61p != -999])
sd_T3 <- sd(data$AGE_61p[data$AGE_61p != -999])
mean_T4 <- mean(data$AGE_71p[data$AGE_71p != -999])
sd_T4 <- sd(data$AGE_71p[data$AGE_71p != -999])
mean_T1
sd_T1
mean_T2
sd_T2
mean_T3
sd_T3
mean_T4
sd_T4

# calculate mean age (SD) of participants at each cohort and each time point. 
mean_T1_C3 <- mean(data$AGE_41p[data$AGE_41p != -999 & data$COH == 3])
mean_T2_C3 <- mean(data$AGE_51p[data$AGE_51p != -999 & data$COH == 3])
mean_T3_C3 <- mean(data$AGE_61p[data$AGE_61p != -999 & data$COH == 3])
mean_T4_C3 <- mean(data$AGE_71p[data$AGE_71p != -999 & data$COH == 3])

mean_T1_C2 <- mean(data$AGE_41p[data$AGE_41p != -999 & data$COH == 2])
mean_T2_C2 <- mean(data$AGE_51p[data$AGE_51p != -999 & data$COH == 2])
mean_T3_C2 <- mean(data$AGE_61p[data$AGE_61p != -999 & data$COH == 2])
mean_T4_C2 <- mean(data$AGE_71p[data$AGE_71p != -999 & data$COH == 2])

mean_T1_C1 <- mean(data$AGE_41p[data$AGE_41p != -999 & data$COH == 1])
mean_T2_C1 <- mean(data$AGE_51p[data$AGE_51p != -999 & data$COH == 1])
mean_T3_C1 <- mean(data$AGE_61p[data$AGE_61p != -999 & data$COH == 1])

sd_T1_C3 <- sd(data$AGE_41p[data$AGE_41p != -999 & data$COH == 3])
sd_T2_C3 <- sd(data$AGE_51p[data$AGE_51p != -999 & data$COH == 3])
sd_T3_C3 <- sd(data$AGE_61p[data$AGE_61p != -999 & data$COH == 3])
sd_T4_C3 <- sd(data$AGE_71p[data$AGE_71p != -999 & data$COH == 3])

sd_T1_C2 <- sd(data$AGE_41p[data$AGE_41p != -999 & data$COH == 2])
sd_T2_C2 <- sd(data$AGE_51p[data$AGE_51p != -999 & data$COH == 2])
sd_T3_C2 <- sd(data$AGE_61p[data$AGE_61p != -999 & data$COH == 2])
sd_T4_C2 <- sd(data$AGE_71p[data$AGE_71p != -999 & data$COH == 2])

sd_T1_C1 <- sd(data$AGE_41p[data$AGE_41p != -999 & data$COH == 1])
sd_T2_C1 <- sd(data$AGE_51p[data$AGE_51p != -999 & data$COH == 1])
sd_T3_C1 <- sd(data$AGE_61p[data$AGE_61p != -999 & data$COH == 1])

table_age <- data.frame(Column1 = c(mean_T1_C3,mean_T2_C3,mean_T3_C3,mean_T4_C3,0),
                    Column2 = c(sd_T1_C3,sd_T2_C3,sd_T3_C3,sd_T4_C3,0),
                    Column3 = c(0,mean_T1_C2,mean_T2_C2,mean_T3_C2,mean_T4_C2),
                    Column4 = c(0,sd_T1_C2,sd_T2_C2,sd_T3_C2,sd_T4_C2),
                    Column5 = c(0,0,mean_T1_C1,mean_T2_C1,mean_T3_C1),
                    Column6 = c(0,0,sd_T1_C1,sd_T2_C1,sd_T3_C1))
table_age <- round(table_age,digits = 2)
View(table_age)

# calculate sample size and gender proportion of each cohort
#   cohort3
n_T1_C3_girl <- sum(data$MALE[data$AGE_41p != -999 & data$COH == 3] == 0)
n_T1_C3_boy <- sum(data$MALE[data$AGE_41p != -999 & data$COH == 3] == 1)
n_T2_C3_girl <- sum(data$MALE[data$AGE_51p != -999 & data$COH == 3] == 0)
n_T2_C3_boy <- sum(data$MALE[data$AGE_51p != -999 & data$COH == 3] == 1)
n_T3_C3_girl <- sum(data$MALE[data$AGE_61p != -999 & data$COH == 3] == 0)
n_T3_C3_boy <- sum(data$MALE[data$AGE_61p != -999 & data$COH == 3] == 1)
n_T4_C3_girl <- sum(data$MALE[data$AGE_71p != -999 & data$COH == 3] == 0)
n_T4_C3_boy <- sum(data$MALE[data$AGE_71p != -999 & data$COH == 3] == 1)
N_total_C3_girl <- sum(data$MALE[data$COH == 3] == 0)
N_total_C3_boy <- sum(data$MALE[data$COH == 3] == 1)

cat(n_T1_C3_girl+n_T1_C3_boy,'(',n_T1_C3_boy,')')
cat(n_T2_C3_girl+n_T2_C3_boy,'(',n_T2_C3_boy,')')
cat(n_T3_C3_girl+n_T3_C3_boy,'(',n_T3_C3_boy,')')
cat(n_T4_C3_girl+n_T4_C3_boy,'(',n_T4_C3_boy,')')
cat(N_total_C3_girl+N_total_C3_boy,'(',N_total_C3_boy,')')

#  cohort2
n_T1_C2_girl <- sum(data$MALE[data$AGE_41p != -999 & data$COH == 2] == 0)
n_T1_C2_boy <- sum(data$MALE[data$AGE_41p != -999 & data$COH == 2] == 1)
n_T2_C2_girl <- sum(data$MALE[data$AGE_51p != -999 & data$COH == 2] == 0)
n_T2_C2_boy <- sum(data$MALE[data$AGE_51p != -999 & data$COH == 2] == 1)
n_T3_C2_girl <- sum(data$MALE[data$AGE_61p != -999 & data$COH == 2] == 0)
n_T3_C2_boy <- sum(data$MALE[data$AGE_61p != -999 & data$COH == 2] == 1)
n_T4_C2_girl <- sum(data$MALE[data$AGE_71p != -999 & data$COH == 2] == 0)
n_T4_C2_boy <- sum(data$MALE[data$AGE_71p != -999 & data$COH == 2] == 1)
N_total_C2_girl <- sum(data$MALE[data$COH == 2] == 0)
N_total_C2_boy <- sum(data$MALE[data$COH == 2] == 1)

cat(n_T1_C2_girl+n_T1_C2_boy,'(',n_T1_C2_boy,')')
cat(n_T2_C2_girl+n_T2_C2_boy,'(',n_T2_C2_boy,')')
cat(n_T3_C2_girl+n_T3_C2_boy,'(',n_T3_C2_boy,')')
cat(n_T4_C2_girl+n_T4_C2_boy,'(',n_T4_C2_boy,')')
cat(N_total_C2_girl+N_total_C2_boy,'(',N_total_C2_boy,')')

#  cohort1
n_T1_C1_girl <- sum(data$MALE[data$AGE_41p != -999 & data$COH == 1] == 0)
n_T1_C1_boy <- sum(data$MALE[data$AGE_41p != -999 & data$COH == 1] == 1)
n_T2_C1_girl <- sum(data$MALE[data$AGE_51p != -999 & data$COH == 1] == 0)
n_T2_C1_boy <- sum(data$MALE[data$AGE_51p != -999 & data$COH == 1] == 1)
n_T3_C1_girl <- sum(data$MALE[data$AGE_61p != -999 & data$COH == 1] == 0)
n_T3_C1_boy <- sum(data$MALE[data$AGE_61p != -999 & data$COH == 1] == 1)
N_total_C1_girl <- sum(data$MALE[data$COH == 1] == 0)
N_total_C1_boy <- sum(data$MALE[data$COH == 1] == 1)

cat(n_T1_C1_girl+n_T1_C1_boy,'(',n_T1_C1_boy,')')
cat(n_T2_C1_girl+n_T2_C1_boy,'(',n_T2_C1_boy,')')
cat(n_T3_C1_girl+n_T3_C1_boy,'(',n_T3_C1_boy,')')
cat(N_total_C1_girl+N_total_C1_boy,'(',N_total_C1_boy,')')

table(data$MALE==1)

# calculate sample size at each waves
ss_T1 <- cat(n_T1_C3_girl+n_T1_C3_boy+n_T1_C2_girl+n_T1_C2_boy+n_T1_C1_girl+n_T1_C1_boy)
ss_T2 <- cat (n_T2_C3_girl+n_T2_C3_boy+n_T2_C2_girl+n_T2_C2_boy+n_T2_C1_girl+n_T2_C1_boy)
ss_T3 <- cat (n_T3_C3_girl+n_T3_C3_boy+n_T3_C2_girl+n_T3_C2_boy+n_T3_C1_girl+n_T3_C1_boy)
ss_T4 <- cat (n_T4_C3_girl+n_T4_C3_boy+n_T4_C2_girl+n_T4_C2_boy)

#Descriptive and correlation analyses (Table2)
data <- included
data[data == -999] <- NA
data$WB041p_total <- data$WB041p_t001 + data$WB041p_t002
data$WB051p_total <- data$WB051p_t001 + data$WB051p_t002
data$WB061p_total <- data$WB061p_t001 + data$WB061p_t002
data$WB071p_total <- data$WB071p_t001 + data$WB071p_t002


library(matrixStats)
library(psych)
colnames(data)
selected_matrix <- as.matrix(data[, c(40:43,36:39,32:35)])
#selected_matrix <- as.matrix(data[, c(12,14,16,18,36:39,32:35)]) #wellbeing item1
#selected_matrix <- as.matrix(data[, c(13,15,17,19,36:39,32:35)])# wellbeing item2
# selected_matrix <- as.matrix(data[, c(22,25,28,31,36:39,32:35)]) #motivation
Mean <- round(as.matrix(colMeans(selected_matrix,na.rm = TRUE)),digits = 2)
SD <- round(as.matrix(colSds(selected_matrix,na.rm = TRUE)),digits = 2)
cor_matrix <- cor(selected_matrix,use = "pairwise.complete.obs")
cor_sig <- psych::corr.test(selected_matrix,use = "pairwise.complete.obs")

# Add significance labels to the correlation matrix
cor_matrix_labeled <- round(cor_matrix,digits = 2)
cor_matrix_labeled[cor_sig$p < 0.05] <- paste0(round(cor_matrix[cor_sig$p < 0.05], 2), "*")
cor_matrix_labeled[cor_sig$p < 0.01] <- paste0(round(cor_matrix[cor_sig$p < 0.01], 2), "**")
cor_matrix_labeled[cor_sig$p < 0.001] <- paste0(round(cor_matrix[cor_sig$p < 0.001], 2), "***")

# generate table2
library(rempsyc)
table2 <- cbind(Mean,SD,cor_matrix_labeled)
colnames(table2) <- c('M','SD','wb1','wb2','wb3','wb4','sb1','sb2','sb3','sb4','sm1','sm2','sm3','sm4')
rownames(table2) <- c('wb1','wb2','wb3','wb4','sb1','sb2','sb3','sb4','sm1','sm2','sm3','sm4')
#rempsyc::nice_table(table2)
#flextable::save_as_docx(table2,path = 'nicetable_table2.docx')

anova_sharing_behaviors <- aov(DG041p_cOTHs+DG051p_cOTHs+DG061p_cOTHs+DG071p_cOTHs ~ 1,data = as.data.frame(selected_matrix),na.action = na.exclude)
summary(anova_sharing_behaviors)



# 3.CLPM & RI-CLPM ####
# seperated into 6 scripts: 
  # 1.	Well-being item1 & Sharing behaviors
  # 2.	Well-being item2 & Sharing behaviors
  # 3.	Well-being_total & Sharing behaviors
  # 4.	Well-being item1 & Social mindfulness
  # 5.	Well-being item2 & Social mindfulness
  # 6.	Well-being_total & Social mindfulness
source('3.Wellbeing_total & Sharing behavior.R')


# #Cohort 3 Sharing behaviors
# Cohort3 <- included %>% 
#   dplyr::filter(.$COH == 3)
# Cohort3[Cohort3 == -999] <- NA
# 
# Cohort3$WB041p_total <- Cohort3$WB041p_t001 + Cohort3$WB041p_t002
# Cohort3$WB051p_total <- Cohort3$WB051p_t001 + Cohort3$WB051p_t002
# Cohort3$WB061p_total <- Cohort3$WB061p_t001 + Cohort3$WB061p_t002
# Cohort3$WB071p_total <- Cohort3$WB071p_t001 + Cohort3$WB071p_t002
# 
# cor_matrix <- cor(Cohort3[,36:43],use = "pairwise.complete.obs")
# cor_sig <- psych::corr.test(Cohort3[,36:43],use = "pairwise.complete.obs")
# 
# cor_matrix_labeled <- round(cor_matrix,digits = 2)
# cor_matrix_labeled[cor_sig$p < 0.05] <- paste0(round(cor_matrix[cor_sig$p < 0.05], 2), "*")
# cor_matrix_labeled[cor_sig$p < 0.01] <- paste0(round(cor_matrix[cor_sig$p < 0.01], 2), "**")
# cor_matrix_labeled[cor_sig$p < 0.001] <- paste0(round(cor_matrix[cor_sig$p < 0.001], 2), "***")
# cor_matrix_labeled
# 
# #Cohort 2 Sharing behaviors
# Cohort2 <- included %>% 
#   dplyr::filter(.$COH == 2)
# Cohort2[Cohort2 == -999] <- NA
# 
# Cohort2$WB041p_total <- Cohort2$WB041p_t001 + Cohort2$WB041p_t002
# Cohort2$WB051p_total <- Cohort2$WB051p_t001 + Cohort2$WB051p_t002
# Cohort2$WB061p_total <- Cohort2$WB061p_t001 + Cohort2$WB061p_t002
# Cohort2$WB071p_total <- Cohort2$WB071p_t001 + Cohort2$WB071p_t002
# 
# cor_matrix <- cor(Cohort2[,36:43],use = "pairwise.complete.obs")
# cor_sig <- psych::corr.test(Cohort2[,36:43],use = "pairwise.complete.obs")
# 
# cor_matrix_labeled <- round(cor_matrix,digits = 2)
# cor_matrix_labeled[cor_sig$p < 0.05] <- paste0(round(cor_matrix[cor_sig$p < 0.05], 2), "*")
# cor_matrix_labeled[cor_sig$p < 0.01] <- paste0(round(cor_matrix[cor_sig$p < 0.01], 2), "**")
# cor_matrix_labeled[cor_sig$p < 0.001] <- paste0(round(cor_matrix[cor_sig$p < 0.001], 2), "***")
# cor_matrix_labeled