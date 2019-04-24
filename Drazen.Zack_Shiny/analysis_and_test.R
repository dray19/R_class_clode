################ Statistical Programming Final Project
################ data from https://www.kaggle.com/amanajmera1/framingham-heart-study-dataset
library(e1071)
library(tidyverse)
library(mlbench)
library(psych)
library(skimr)
library(corrplot)
########### Input data
setwd("~/Desktop/Drazen.Zack_Shiny")
df <- read.csv("fram.csv")
str(df)
df1 <- df
########## Cleaning data
df1$male <- as.factor(df1$male)
levels(df1$male) <- c("Female", "Male")
names(df1)[1] <- "sex"
df1$education <- as.factor(df1$education)
levels(df1$education) <- c("Some High School","HS and GED", "Some College", "College")
df1$currentSmoker <- as.factor(df1$currentSmoker)
levels(df1$currentSmoker) <- c("nonsmoker", "smoker")
df1$BPMeds <- as.factor(df1$BPMeds)
levels(df1$BPMeds) <- c("No BP Meds", "BP Meds")
df1$diabetes <- as.factor(df1$diabetes)
levels(df1$diabetes) <- c("No", "Yes")
df1$TenYearCHD <- as.factor(df1$TenYearCHD)
levels(df1$TenYearCHD) <- c("No","Yes")
df1$prevalentStroke <-  as.factor(df1$prevalentStroke)
levels(df1$prevalentStroke) <- c("FALSE", "TRUE")
df1$prevalentHyp <- as.factor(df1$prevalentHyp)
levels(df1$prevalentHyp) <- c("FALSE", "TRUE")
################# NA'S
summary(df1)
overview <- skim_to_wide(df1)
df1 <-  drop_na(df1)
############ Stats and Correlation
summary(df1)
num <- df1[, c("age","cigsPerDay","totChol", "sysBP", "diaBP", "BMI", "heartRate", "glucose")]
not_num <- df1[, - which(names(df1) %in% c("age","cigsPerDay","totChol", "sysBP", "diaBP", "BMI", "heartRate", "glucose"))] 
########### Scatter Plot to see relationship 
var_list = combn(names(num)[1:8], 2, simplify=FALSE)
var_list
plot_list = list()
for (i in 1:28) {
  p = ggplot(num, aes_string(x=var_list[[i]][1], y=var_list[[i]][2])) +
    geom_point(size=2, col = "red",alpha = 0.7) + ggtitle(paste0(var_list[[i]][1], " vs. ", var_list[[i]][2])) +
    theme(plot.title = element_text(hjust = 0.5)) 
  plot_list[[i]] = p
}
plot_list
#############################
stat_cat <- function(x){
  mean <- sapply(num, tapply ,INDEX = x, mean)
  median <- sapply(num, tapply ,INDEX = x, median)
  sd <- sapply(num, tapply ,INDEX = x, sd)
  num_info <- list(Mean = mean, Median = median, SD = sd)
  return(num_info)
}
stats_by_factor <- apply(not_num, 2, stat_cat)
stats_by_factor
#### Correlation 
w <- cor(num)
w
corrplot(w, method = "circle")
corrplot(w, method = "pie")
corrplot(w, method = "number")
####### Correlation test 
## cigperDay
cor.test(df1$cigsPerDay, df1$totChol, method="pearson", alternative="two.sided", conf.level=0.95) # greater than 0.05
cor.test(df1$cigsPerDay, df1$sysBP, method="pearson", alternative="two.sided", conf.level=0.95) # less than 0.05
cor.test(df1$cigsPerDay, df1$diaBP, method="pearson", alternative="two.sided", conf.level=0.95) # less than 0.05
cor.test(df1$cigsPerDay, df1$BMI, method="pearson", alternative="two.sided", conf.level=0.95) # less than 0.05
cor.test(df1$cigsPerDay, df1$heartRate, method="pearson", alternative="two.sided", conf.level=0.95) # less than 0.05
cor.test(df1$cigsPerDay, df1$glucose, method="pearson", alternative="two.sided", conf.level=0.95) # less than 0.05
### heart rate
cor.test(df1$heartRate,df1$age, method="pearson", alternative="two.sided", conf.level=0.95)# greater than 0.05
cor.test(df1$heartRate, df1$glucose, method="pearson", alternative="two.sided", conf.level=0.95) # less than 0.05
#### glucose 
cor.test(df1$glucose, df1$totChol, method="pearson", alternative="two.sided", conf.level=0.95) # less than 0.05
cor.test(df1$glucose, df1$diaBP, method="pearson", alternative="two.sided", conf.level=0.95) # less than 0.05
#################### chi-square test for categorical variables 
library(gmodels)
library(lsr)
####### Sex
CrossTable(df1$sex, df1$TenYearCHD, expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
cramersV(df1$sex, df1$TenYearCHD)#LOW
CrossTable(df1$sex, df1$BPMeds, expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
cramersV(df1$sex, df1$BPMeds) # LOW
CrossTable(df1$sex, df1$diabetes, expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)# greater than 0.05
CrossTable(df1$sex, df1$currentSmoker, expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F) 
cramersV(df1$sex, df1$currentSmoker)# LOW
CrossTable(df1$sex, df1$education, expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
cramersV(df1$sex, df1$education) # Low
CrossTable(df1$sex, df1$prevalentStroke, expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)# greater than 0.05
CrossTable(df1$sex, df1$prevalentHyp, expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)# greater than 0.05
######## current smoker 
CrossTable(df1$currentSmoker, df1$diabetes, expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
cramersV(df1$currentSmoker, df1$diabetes) # Low
CrossTable(df1$currentSmoker, df1$TenYearCHD, expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)# greater than 0.05
CrossTable(df1$currentSmoker, df1$BPMeds, expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F) 
cramersV(df1$currentSmoker, df1$BPMeds)# Low
CrossTable(df1$currentSmoker, df1$education, expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F) 
cramersV(df1$currentSmoker, df1$education) # Low
CrossTable(df1$currentSmoker, df1$prevalentStroke, expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F) 
cramersV(df1$currentSmoker, df1$prevalentStroke)# Low
CrossTable(df1$currentSmoker, df1$prevalentHyp, expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F) 
cramersV(df1$currentSmoker, df1$prevalentHyp)# Low
####### diabetes 
CrossTable(df1$diabetes,df1$education, expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
cramersV(df1$diabetes, df1$education) # LOW
CrossTable(df1$diabetes,df1$TenYearCHD, expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
cramersV(df1$diabetes, df1$TenYearCHD)  # LOW
CrossTable(df1$diabetes,df1$prevalentHyp, expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
cramersV(df1$diabetes, df1$prevalentHyp) # Low
################ BPmeds
CrossTable(df1$BPMeds, df1$TenYearCHD, expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
cramersV(df1$BPMeds, df1$TenYearCHD) # low
CrossTable(df1$BPMeds, df1$prevalentHyp, expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
cramersV(df1$BPMeds, df1$prevalentHyp) # low
################## TenYearCHD
CrossTable(df1$TenYearCHD,df1$prevalentHyp, expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
cramersV(df1$TenYearCHD, df1$prevalentHyp) # low
################## prevalentStroke
CrossTable(df1$prevalentStroke, df1$prevalentHyp,expected=T, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
cramersV(df1$prevalentStroke, df1$prevalentHyp) # low
######################## ANOVA Functions created 
library(car)
anova_func <- function(x,y){
  r <- leveneTest(x ~ y)
  if(r$`Pr(>F)`[1] > 0.05){
    aov1 <- aov(x~y, df1)
   w <-  summary(aov1)
    t <- TukeyHSD(aov1)
    return(list(r,w, t))
  } else{
    c <- oneway.test(x~y, data=df1, var.equal=F)
    return(list(r,c))
  }
}
############################  Sex
sex_bi <- list()
for(var in colnames(df1)){
  if (class(df1[,var]) %in% c("factor","logical")){
    sex_bi[[var]] <- ggplot(data = df1) + geom_bar(aes_string(x = var, fill = "sex")) + 
      theme_linedraw() + ggtitle(paste0(var, " vs. Sex")) + xlab(var) + theme(plot.title = element_text(hjust = 0.5))
  } else if (class(df1[,var]) %in% c("numeric","double","integer")){
    sex_bi[[var]]  <- ggplot(data = df1) + geom_boxplot(aes_string(y = var, x = "sex", fill = "sex")) + 
      theme_linedraw() + ggtitle(paste0(var, " vs. Sex")) + xlab(var) + theme(plot.title = element_text(hjust = 0.5)) + 
      theme(legend.position = "none")
  }
}
sex_bi
anova_sex <- apply(df1[,c("age","cigsPerDay","totChol", "sysBP", "diaBP", "BMI", "heartRate", "glucose")], 2, anova_func, y= df1$sex)
anova_sex$age
anova_sex$cigsPerDay
anova_sex$totChol
anova_sex$sysBP
anova_sex$diaBP
anova_sex$BMI
anova_sex$heartRate
anova_sex$glucose
#################### Education 
edu_bi <- list()
for(var in colnames(df1)){
  if (class(df1[,var]) %in% c("factor","logical")){
    edu_bi[[var]] <- ggplot(data = df1) + geom_bar(aes_string(x = var, fill = "education")) + 
      theme_linedraw() + ggtitle(paste0(var, " vs. education")) + xlab(var) + theme(plot.title = element_text(hjust = 0.5)) 
  } else if (class(df1[,var]) %in% c("numeric","double","integer")){
    edu_bi[[var]]  <- ggplot(data = df1) + geom_boxplot(aes_string(y = var, x = "education", fill = "education")) + 
      theme_linedraw() + ggtitle(paste0(var, " vs. education")) + xlab(var) + theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position = "none")
  }
}
edu_bi
stats_by_factor[2]
anova_ed <- apply(df1[,c("age","cigsPerDay","totChol", "sysBP", "diaBP", "BMI", "heartRate", "glucose")], 2, anova_func, y= df1$education)
anova_ed$age
anova_ed$totChol
anova_ed$cigsPerDay
anova_ed$sysBP
anova_ed$diaBP
anova_ed$BMI
anova_ed$heartRate
anova_ed$glucose
################### current smoker
cs_bi <- list()
for(var in colnames(df1)){
  if (class(df1[,var]) %in% c("factor","logical")){
    cs_bi[[var]] <- ggplot(data = df1) + geom_bar(aes_string(x = var, fill = "currentSmoker")) + 
      theme_linedraw() + ggtitle(paste0(var, " vs. currentSmoker")) + xlab(var) + theme(plot.title = element_text(hjust = 0.5))
  } else if (class(df1[,var]) %in% c("numeric","double","integer")){
    cs_bi[[var]]  <- ggplot(data = df1) + geom_boxplot(aes_string(y = var, x = "currentSmoker", fill = "currentSmoker")) + 
      theme_linedraw() + ggtitle(paste0(var, " vs. currentSmoker")) + xlab(var) + theme(plot.title = element_text(hjust = 0.5)) + 
      theme(legend.position = "none")
  }
}
cs_bi
stats_by_factor[3]
anova_cs <- apply(df1[,c("age","cigsPerDay","totChol", "sysBP", "diaBP", "BMI", "heartRate", "glucose")], 2, anova_func, y= df1$currentSmoker)
anova_cs$age
anova_cs$cigsPerDay
anova_cs$totChol
anova_cs$sysBP
anova_cs$diaBP
anova_cs$BMI
anova_cs$heartRate
anova_cs$glucose
################ BPMeds
bm_bi <- list()
for(var in colnames(df1)){
  if (class(df1[,var]) %in% c("factor","logical")){
    bm_bi[[var]] <- ggplot(data = df1) + geom_bar(aes_string(x = var, fill = "BPMeds")) + 
      theme_linedraw() + ggtitle(paste0(var, " vs. BPMeds")) + xlab(var) + theme(plot.title = element_text(hjust = 0.5))
  } else if (class(df1[,var]) %in% c("numeric","double","integer")){
    bm_bi[[var]]  <- ggplot(data = df1) + geom_boxplot(aes_string(y = var, x = "BPMeds", fill = "BPMeds")) + 
      theme_linedraw() + ggtitle(paste0(var, " vs. BPMeds")) + xlab(var) + theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position = "none")
  }
}
bm_bi
stats_by_factor[4]
anova_med <- apply(df1[,c("age","cigsPerDay","totChol", "sysBP", "diaBP", "BMI", "heartRate", "glucose")], 2, anova_func, y= df1$BPMeds)
anova_med$age
anova_med$cigsPerDay
anova_med$sysBP
anova_med$totChol
anova_med$heartRate
anova_med$BMI
anova_med$diaBP
anova_med$glucose
######################  prevalentStroke
ps_bi <- list()
for(var in colnames(df1)){
  if (class(df1[,var]) %in% c("factor","logical")){
    ps_bi[[var]] <- ggplot(data = df1) + geom_bar(aes_string(x = var, fill = "prevalentStroke")) + 
      theme_linedraw() + ggtitle(paste0(var, " vs. prevalentStroke")) + xlab(var) + theme(plot.title = element_text(hjust = 0.5))
  } else if (class(df1[,var]) %in% c("numeric","double","integer")){
    ps_bi[[var]]  <- ggplot(data = df1) + geom_boxplot(aes_string(y = var, x = "prevalentStroke", fill = "prevalentStroke")) + 
      theme_linedraw() + ggtitle(paste0(var, " vs. prevalentStroke")) + xlab(var) + theme(plot.title = element_text(hjust = 0.5)) + 
      theme(legend.position = "none")
  }
}
ps_bi
stats_by_factor[5]
anova_stroke <- apply(df1[,c("age","cigsPerDay","totChol", "sysBP", "diaBP", "BMI", "heartRate", "glucose")], 2, anova_func, y= df1$prevalentStroke)
anova_stroke$age
anova_stroke$cigsPerDay
anova_stroke$totChol
anova_stroke$sysBP
anova_stroke$diaBP
anova_stroke$BMI
anova_stroke$heartRate
anova_stroke$glucose
###################### prevalentHyp
ph_bi <- list()
for(var in colnames(df1)){
  if (class(df1[,var]) %in% c("factor","logical")){
    ph_bi[[var]] <- ggplot(data = df1) + geom_bar(aes_string(x = var, fill = "prevalentHyp")) + 
      theme_linedraw() + ggtitle(paste0(var, " vs. prevalentHyp")) + xlab(var) + theme(plot.title = element_text(hjust = 0.5))
  } else if (class(df1[,var]) %in% c("numeric","double","integer")){
    ph_bi[[var]]  <- ggplot(data = df1) + geom_boxplot(aes_string(y = var, x = "prevalentHyp", fill = "prevalentHyp")) + 
      theme_linedraw() + ggtitle(paste0(var, " vs. prevalentHyp")) + xlab(var) + theme(plot.title = element_text(hjust = 0.5)) + 
      theme(legend.position = "none")
  }
}
ph_bi
stats_by_factor[6]
anova_hyp <- apply(df1[,c("age","cigsPerDay","totChol", "sysBP", "diaBP", "BMI", "heartRate", "glucose")], 2, anova_func, y= df1$prevalentHyp)
anova_hyp$age
anova_hyp$cigsPerDay
anova_hyp$totChol
anova_hyp$diaBP
anova_hyp$sysBP
anova_hyp$BMI
anova_hyp$heartRate
anova_hyp$glucose
###################### diabetes
d_bi <- list()
for(var in colnames(df1)){
  if (class(df1[,var]) %in% c("factor","logical")){
    d_bi[[var]] <- ggplot(data = df1) + geom_bar(aes_string(x = var, fill = "diabetes")) + 
      theme_linedraw() + ggtitle(paste0(var, " vs. diabetes")) + xlab(var) + theme(plot.title = element_text(hjust = 0.5))
  } else if (class(df1[,var]) %in% c("numeric","double","integer")){
    d_bi[[var]]  <- ggplot(data = df1) + geom_boxplot(aes_string(y = var, x = "diabetes", fill ="diabetes" )) + 
      theme_linedraw() + ggtitle(paste0(var, " vs. diabetes")) + xlab(var) + theme(plot.title = element_text(hjust = 0.5)) + 
      theme(legend.position = "none")
  }
}
d_bi
stats_by_factor[7]
anova_dia <- apply(df1[,c("age","cigsPerDay","totChol", "sysBP", "diaBP", "BMI", "heartRate", "glucose")], 2, anova_func, y= df1$diabetes)
anova_dia$age
anova_dia$cigsPerDay
anova_dia$totChol
anova_dia$sysBP
anova_dia$BMI
anova_dia$heartRate
anova_dia$glucose
anova_dia$diaBP
########################## Ten Year  CHD
ptlist_bi <- list()
for(var in colnames(df1)){
  if (class(df1[,var]) %in% c("factor","logical")){
    ptlist_bi[[var]] <- ggplot(data = df1) + geom_bar(aes_string(x = var, fill = "TenYearCHD")) + 
      theme_linedraw() + ggtitle(paste0(var, " vs. TenYearCHD")) + xlab(var) + theme(plot.title = element_text(hjust = 0.5))
  } else if (class(df1[,var]) %in% c("numeric","double","integer")){
    ptlist_bi[[var]]  <- ggplot(data = df1) + geom_boxplot(aes_string(y = var, x = "TenYearCHD", fill ="TenYearCHD" )) + 
      theme_linedraw() + ggtitle(paste0(var, " vs. TenYearCHD")) + xlab(var) + theme(plot.title = element_text(hjust = 0.5)) + 
      theme(legend.position = "none")
  }
}
ptlist_bi
stats_by_factor[8]
anova_chd <- apply(df1[,c("age","cigsPerDay","totChol", "sysBP", "diaBP", "BMI", "heartRate", "glucose")], 2, anova_func, y= df1$TenYearCHD)
anova_chd$age
anova_chd$cigsPerDay
anova_chd$totChol
anova_chd$sysBP
anova_chd$diaBP
anova_chd$BMI
anova_chd$heartRate
anova_chd$glucose
########################################## Two-way analysis of variance
aov1 <- aov(sysBP~diabetes + sex + diabetes*sex, df1)
summary(aov1)
######### simple main effects
df_male <- df1[df1$sex == "Male",]
aov2 <- aov(sysBP~ diabetes, df_male)
summary(aov2)
TukeyHSD(aov2)

df_f <- df1[df1$sex == "Female",]
aov3 <- aov(sysBP~ diabetes, df_f)
summary(aov3)
TukeyHSD(aov3)

df_yes <- df1[df1$diabetes == "Yes",]
aov4 <- aov(sysBP~ sex, df_yes)
summary(aov4)
TukeyHSD(aov4)

df_no <- df1[df1$diabetes == "No",]
aov5 <- aov(sysBP~ sex, df_no)
summary(aov5)
TukeyHSD(aov5)
###############################
aov1 <- aov(sysBP~TenYearCHD + sex + TenYearCHD*sex, df1)
summary(aov1)
######### simple main effects
df_male <- df1[df1$sex == "Male",]
aov2 <- aov(sysBP~ TenYearCHD, df_male)
summary(aov2)
TukeyHSD(aov2)

df_f <- df1[df1$sex == "Female",]
aov3 <- aov(sysBP~ TenYearCHD, df_f)
summary(aov3)
TukeyHSD(aov3)

df_yes <- df1[df1$TenYearCHD == "Yes",]
aov4 <- aov(sysBP~ sex, df_yes)
summary(aov4)
TukeyHSD(aov4)

df_no <- df1[df1$diabetes == "No",]
aov5 <- aov(sysBP~ sex, df_no)
summary(aov5)
TukeyHSD(aov5)
###############################
aov1 <- aov(diaBP~diabetes + sex + diabetes*sex, df1)
summary(aov1)

df_male <- df1[df1$sex == "Male",]
aov2 <- aov(diaBP~ diabetes, df_male)
summary(aov2)
TukeyHSD(aov2)

df_f <- df1[df1$sex == "Female",]
aov3 <- aov(diaBP~ diabetes, df_f)
summary(aov3)
TukeyHSD(aov3)

df_yes <- df1[df1$diabetes == "Yes",]
aov4 <- aov(diaBP~ sex, df_yes)
summary(aov4)
TukeyHSD(aov4)

df_no <- df1[df1$diabetes == "No",]
aov5 <- aov(diaBP~ sex, df_no)
summary(aov5)
TukeyHSD(aov5)
###################
aov1 <- aov(totChol~diabetes + sex + diabetes*sex, df1)
summary(aov1)

df_male <- df1[df1$sex == "Male",]
aov2 <- aov(totChol~ diabetes, df_male)
summary(aov2)
TukeyHSD(aov2)

df_f <- df1[df1$sex == "Female",]
aov3 <- aov(totChol~ diabetes, df_f)
summary(aov3)
TukeyHSD(aov3)

df_yes <- df1[df1$diabetes == "Yes",]
aov4 <- aov(totChol~ sex, df_yes)
summary(aov4)
TukeyHSD(aov4)

df_no <- df1[df1$diabetes == "No",]
aov5 <- aov(totChol~ sex, df_no)
summary(aov5)
TukeyHSD(aov5)
############################
aov1 <- aov(glucose~diabetes + TenYearCHD + diabetes*TenYearCHD, df1)
summary(aov1)

df_chd_yes <- df1[df1$TenYearCHD == "Yes",]
aov2 <- aov(glucose~ diabetes,df_chd_yes )
summary(aov2)
TukeyHSD(aov2)

df_chd_no <- df1[df1$TenYearCHD == "No",]
aov3 <- aov(glucose~ diabetes, df_chd_no)
summary(aov3)
TukeyHSD(aov3)

df_yes <- df1[df1$diabetes == "Yes",]
aov4 <- aov(glucose~ TenYearCHD, df_yes)
summary(aov4)
TukeyHSD(aov4)

df_no <- df1[df1$diabetes == "No",]
aov5 <- aov(glucose~ TenYearCHD, df_no)
summary(aov5)
TukeyHSD(aov5)
######################## three-way analysis of variance
aov1 <- aov(glucose~diabetes + TenYearCHD + sex + diabetes*TenYearCHD + sex*TenYearCHD + diabetes*sex + diabetes*sex*TenYearCHD, df1)
summary(aov1)
############# simple second order interaction effect
df_male <- df1[df1$sex == "Male",]
aov2 <- aov(glucose~ diabetes*TenYearCHD, df_male)
summary(aov2)
TukeyHSD(aov2)

df_f <- df1[df1$sex == "Female",]
aov3 <- aov(glucose~ diabetes* TenYearCHD, df_f)
summary(aov3)
TukeyHSD(aov3)

df_chd_yes <- df1[df1$TenYearCHD == "Yes",]
aov4<- aov(glucose~ diabetes*sex,df_chd_yes )
summary(aov4)
TukeyHSD(aov4)

df_chd_no <- df1[df1$TenYearCHD == "No",]
aov5 <- aov(glucose~ diabetes*sex, df_chd_no)
summary(aov5)
TukeyHSD(aov5)

df_yes <- df1[df1$diabetes == "Yes",]
aov6 <- aov(glucose~ TenYearCHD*sex, df_yes)
summary(aov6)
TukeyHSD(aov6)

df_no <- df1[df1$diabetes == "No",]
aov7 <- aov(glucose~ TenYearCHD*sex, df_no)
summary(aov7)
TukeyHSD(aov7)
################# simple main effects
####### Male & Yes Diabetes
df_male_yes <- df1[df1$sex == "Male" & df1$diabetes == "Yes",]
aov2 <- aov(glucose~ TenYearCHD, df_male_yes)
summary(aov2)
TukeyHSD(aov2)
####### Male & No Diabetes
df_male_no <- df1[df1$sex == "Male" & df1$diabetes == "No",]
aov2 <- aov(glucose~ TenYearCHD, df_male_no)
summary(aov2)
TukeyHSD(aov2)
#############
####### Female & Yes Diabetes
df_f_yes <- df1[df1$sex == "Female"& df1$diabetes == "Yes",]
aov3 <- aov(glucose~ TenYearCHD, df_f_yes)
summary(aov3)
TukeyHSD(aov3)
####### Female & No Diabetes
df_f_no <- df1[df1$sex == "Female"& df1$diabetes == "No",]
aov3 <- aov(glucose~ TenYearCHD, df_f_no)
summary(aov3)
TukeyHSD(aov3)
###############
####### CHD Yes & Diabetes No
df_yes_no <- df1[df1$TenYearCHD == "Yes"& df1$diabetes == "No",]
aov4 <- aov(glucose~ sex,df_yes_no )
summary(aov4)
TukeyHSD(aov4)
####### CHD Yes & Diabetes Yes
df_yes_yes <- df1[df1$TenYearCHD == "Yes"& df1$diabetes == "Yes",]
aov4 <- aov(glucose~ sex,df_yes_yes )
summary(aov4)
TukeyHSD(aov4)
#######################
####### CHD No & Diabetes No
df_no_no <- df1[df1$TenYearCHD == "No"& df1$diabetes == "No",]
aov5 <- aov(glucose~ sex, df_no_no)
summary(aov5)
TukeyHSD(aov5)
####### CHD No & Diabetes Yes
df_no_yes <- df1[df1$TenYearCHD == "No"& df1$diabetes == "Yes",]
aov5 <- aov(glucose~ sex, df_no_yes)
summary(aov5)
TukeyHSD(aov5)
#########################
### ####### CHD Yes & Male
df_yes_male <- df1[df1$TenYearCHD == "Yes"& df1$sex == "Male",]
aov6 <- aov(glucose~ diabetes, df_yes_male)
summary(aov6)
TukeyHSD(aov6)
############## CHD Yes & Female
df_yes_f <- df1[df1$TenYearCHD == "Yes"& df1$sex == "Female",]
aov6 <- aov(glucose~ diabetes, df_yes_f)
summary(aov6)
TukeyHSD(aov6)
#########################
############## CHD No & Male
df_no_male <- df1[df1$TenYearCHD == "No"& df1$sex =="Male",]
aov7 <- aov(glucose~ diabetes, df_no_male)
summary(aov7)
TukeyHSD(aov7)
############## CHD No & Female
df_no_f <- df1[df1$TenYearCHD == "No"& df1$sex =="Female",]
aov8 <- aov(glucose~ diabetes, df_no_f)
summary(aov8)
TukeyHSD(aov8)
#################################################################
#### Binomial Regression
df1$TenYearCHD <- relevel(df1$TenYearCHD, ref="Yes")
model <- glm(TenYearCHD ~ ., df1, family = binomial())
summary(model)
expd <- exp(coef(model))
print(expd)
intexp <- exp(confint(model))
print(intexp)
########## R square
require(fmsb)
require(BaylorEdPsych)
NagelkerkeR2(model)
PseudoR2(model)

