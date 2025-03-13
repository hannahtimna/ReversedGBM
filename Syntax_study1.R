## The Reversed Gateway Belief Model - Hannah Timna Logemann

# Experiment 1 - Data Set from "Combatting climate change misinformation: Evidence for longevity of inoculation and consensus messaging effects" (Maertens et al., 2020)
# Data Set is stored online (https://osf.io/ewjgt/) 

setwd("/Users/hannahtimnalogemann/Desktop/MACambridge/02Study1/03_Data")
library(readr)
study1<-read.csv("/Users/hannahtimnalogemann/Desktop/MACambridge/02Study1/03_Data/CCDecayExperimentCleanDataset.csv")


## Create Subset for Consensus and Balanced Group only 
dataexc <- study1[which(study1$Complete == TRUE), ]
filtereddata <- subset(dataexc, Condition=="Consensus"|Condition=="Balanced")

## Table for Frequency in Groups 
freq <- table(filtereddata$Condition)
print(freq)

##### Sample Demographics
#Create Subsets for Groups 
ConsensGroup <- subset(filtereddata, Condition =="Consensus")
BalanceGroup <- subset(filtereddata, Condition =="Balanced")

#Age 
mean(filtereddata$Age)
sd(filtereddata$Age)
min(filtereddata$Age)
max(filtereddata$Age)

mean(ConsensGroup$Age)
sd(ConsensGroup$Age)
min(ConsensGroup$Age)
max(ConsensGroup$Age)

mean(BalanceGroup$Age)
sd(BalanceGroup$Age)
min(BalanceGroup$Age)
max(BalanceGroup$Age)

filtereddata$Age_Category <- factor(filtereddata$Age_Category,
                           levels = c("18-29",
                                      "30-49",
                                      "50+"))
summary(filtereddata$Age_Category)

ConsensGroup$Age_Category <- factor(ConsensGroup$Age_Category,
                                    levels = c("18-29",
                                               "30-49",
                                               "50+"))
summary(ConsensGroup$Age_Category)

BalanceGroup$Age_Category <- factor(BalanceGroup$Age_Category,
                                    levels = c("18-29",
                                               "30-49",
                                               "50+"))
summary(BalanceGroup$Age_Category)

#Sex 
filtereddata$Sex <- factor(filtereddata$Sex)
ConsensGroup$Sex <- factor(ConsensGroup$Sex)
BalanceGroup$Sex <- factor(BalanceGroup$Sex)
summary(filtereddata$Sex)
summary(ConsensGroup$Sex)
summary(BalanceGroup$Sex)

#Education 
summary(filtereddata$Education)
summary(ConsensGroup$Education)
summary(BalanceGroup$Education)

sd(filtereddata$Education)
sd(ConsensGroup$Education)
sd(BalanceGroup$Education)

table(filtereddata$Education_Category)
table(ConsensGroup$Education_Category)
table(BalanceGroup$Education_Category)

#US State
filtereddata$US_State <- factor(filtereddata$US_State)
ConsensGroup$US_State <- factor(ConsensGroup$US_State)
BalanceGroup$US_State <- factor(BalanceGroup$US_State)
sort(summary(filtereddata$US_State))
sort(summary(ConsensGroup$US_State))
sort(summary(BalanceGroup$US_State))

#Ideology
mean(filtereddata$Ideology)
sd(filtereddata$Ideology)

mean(ConsensGroup$Ideology)
sd(ConsensGroup$Ideology)

mean(BalanceGroup$Ideology)
sd(BalanceGroup$Ideology)

table(filtereddata$Ideology_Category)
table(ConsensGroup$Ideology_Category)
table(BalanceGroup$Ideology_Category)


#Political Affiliation 
filtereddata$PoliticalParty <- factor(filtereddata$PoliticalParty)
ConsensGroup$PoliticalParty <- factor(ConsensGroup$PoliticalParty)
BalanceGroup$PoliticalParty <- factor(BalanceGroup$PoliticalParty)
summary(filtereddata$PoliticalParty)
summary(ConsensGroup$PoliticalParty)
summary(BalanceGroup$PoliticalParty)

#PSC Whole Sample 
mean(filtereddata$PSC.T1)
sd(filtereddata$PSC.T1)
mean(filtereddata$PSC.T2)
sd(filtereddata$PSC.T2)
mean(filtereddata$PSC.T3)
sd(filtereddata$PSC.T3)

mean(filtereddata$PSC_Diff_T2T1)
sd(filtereddata$PSC_Diff_T2T1)
mean(filtereddata$PSC_Diff_T3T2)
sd(filtereddata$PSC_Diff_T3T2)
mean(filtereddata$PSC_Diff_T3T1)
sd(filtereddata$PSC_Diff_T3T1)

#PSC Consensus Group 
mean(ConsensGroup$PSC.T1)
sd(ConsensGroup$PSC.T1)
mean(ConsensGroup$PSC.T2)
sd(ConsensGroup$PSC.T2)
mean(ConsensGroup$PSC.T3)
sd(ConsensGroup$PSC.T3)

mean(ConsensGroup$PSC_Diff_T2T1)
sd(ConsensGroup$PSC_Diff_T2T1)
mean(ConsensGroup$PSC_Diff_T3T2)
sd(ConsensGroup$PSC_Diff_T3T2)
mean(ConsensGroup$PSC_Diff_T3T1)
sd(ConsensGroup$PSC_Diff_T3T1)

#PSC Balanced Group
mean(BalanceGroup$PSC.T1)
sd(BalanceGroup$PSC.T1)
mean(BalanceGroup$PSC.T2)
sd(BalanceGroup$PSC.T2)
mean(BalanceGroup$PSC.T3)
sd(BalanceGroup$PSC.T3)

mean(BalanceGroup$PSC_Diff_T2T1)
sd(BalanceGroup$PSC_Diff_T2T1)
mean(BalanceGroup$PSC_Diff_T3T2)
sd(BalanceGroup$PSC_Diff_T3T2)
mean(BalanceGroup$PSC_Diff_T3T1)
sd(BalanceGroup$PSC_Diff_T3T1)

# Belief in Climate Change Whole Sample 
mean(filtereddata$Belief.T1)
sd(filtereddata$Belief.T1)
mean(filtereddata$Belief.T2)
sd(filtereddata$Belief.T2)
mean(filtereddata$Belief.T3)
sd(filtereddata$Belief.T3)

mean(filtereddata$Belief_Diff_T2T1)
sd(filtereddata$Belief_Diff_T2T1)
mean(filtereddata$Belief_Diff_T3T2)
sd(filtereddata$Belief_Diff_T3T2)
mean(filtereddata$Belief_Diff_T3T1)
sd(filtereddata$Belief_Diff_T3T1)

# Belief in Climate Change Consensus Group
mean(ConsensGroup$Belief.T1)
sd(ConsensGroup$Belief.T1)
mean(ConsensGroup$Belief.T2)
sd(ConsensGroup$Belief.T2)
mean(ConsensGroup$Belief.T3)
sd(ConsensGroup$Belief.T3)

mean(ConsensGroup$Belief_Diff_T2T1)
sd(ConsensGroup$Belief_Diff_T2T1)
mean(ConsensGroup$Belief_Diff_T3T2)
sd(ConsensGroup$Belief_Diff_T3T2)
mean(ConsensGroup$Belief_Diff_T3T1)
sd(ConsensGroup$Belief_Diff_T3T1)

# Belief in Climate Change Balanced Group 
mean(BalanceGroup$Belief.T1)
sd(BalanceGroup$Belief.T1)
mean(BalanceGroup$Belief.T2)
sd(BalanceGroup$Belief.T2)
mean(BalanceGroup$Belief.T3)
sd(BalanceGroup$Belief.T3)

mean(BalanceGroup$Belief_Diff_T2T1)
sd(BalanceGroup$Belief_Diff_T2T1)
mean(BalanceGroup$Belief_Diff_T3T2)
sd(BalanceGroup$Belief_Diff_T3T2)
mean(BalanceGroup$Belief_Diff_T3T1)
sd(BalanceGroup$Belief_Diff_T3T1)

# Worry about Climate Change Whole Sample 
mean(filtereddata$Worry.T1)
sd(filtereddata$Worry.T1)
mean(filtereddata$Worry.T2)
sd(filtereddata$Worry.T2)
mean(filtereddata$Worry.T3)
sd(filtereddata$Worry.T3)

mean(filtereddata$Worry_Diff_T2T1)
sd(filtereddata$Worry_Diff_T2T1)
mean(filtereddata$Worry_Diff_T3T2)
sd(filtereddata$Worry_Diff_T3T2)
mean(filtereddata$Worry_Diff_T3T1)
sd(filtereddata$Worry_Diff_T3T1)

#Worry about Climate Change Consensus Group 
mean(ConsensGroup$Worry.T1)
sd(ConsensGroup$Worry.T1)
mean(ConsensGroup$Worry.T2)
sd(ConsensGroup$Worry.T2)
mean(ConsensGroup$Worry.T3)
sd(ConsensGroup$Worry.T3)

mean(ConsensGroup$Worry_Diff_T2T1)
sd(ConsensGroup$Worry_Diff_T2T1)
mean(ConsensGroup$Worry_Diff_T3T2)
sd(ConsensGroup$Worry_Diff_T3T2)
mean(ConsensGroup$Worry_Diff_T3T1)
sd(ConsensGroup$Worry_Diff_T3T1)

#Worry about Climate Change Balanced Group 
mean(BalanceGroup$Worry.T1)
sd(BalanceGroup$Worry.T1)
mean(BalanceGroup$Worry.T2)
sd(BalanceGroup$Worry.T2)
mean(BalanceGroup$Worry.T3)
sd(BalanceGroup$Worry.T3)

mean(BalanceGroup$Worry_Diff_T2T1)
sd(BalanceGroup$Worry_Diff_T2T1)
mean(BalanceGroup$Worry_Diff_T3T2)
sd(BalanceGroup$Worry_Diff_T3T2)
mean(BalanceGroup$Worry_Diff_T3T1)
sd(BalanceGroup$Worry_Diff_T3T1)

#Belief in Human Causation Whole Sample
mean(filtereddata$HumanCausation.T1)
sd(filtereddata$HumanCausation.T1)
mean(filtereddata$HumanCausation.T2)
sd(filtereddata$HumanCausation.T2)
mean(filtereddata$HumanCausation.T3)
sd(filtereddata$HumanCausation.T3)

mean(filtereddata$HumanCausation_Diff_T2T1)
sd(filtereddata$HumanCausation_Diff_T2T1)
mean(filtereddata$HumanCausation_Diff_T3T2)
sd(filtereddata$HumanCausation_Diff_T3T2)
mean(filtereddata$HumanCausation_Diff_T3T1)
sd(filtereddata$HumanCausation_Diff_T3T1)

#Belief in Human Causation Consensus Group 
mean(ConsensGroup$HumanCausation.T1)
sd(ConsensGroup$HumanCausation.T1)
mean(ConsensGroup$HumanCausation.T2)
sd(ConsensGroup$HumanCausation.T2)
mean(ConsensGroup$HumanCausation.T3)
sd(ConsensGroup$HumanCausation.T3)

mean(ConsensGroup$HumanCausation_Diff_T2T1)
sd(ConsensGroup$HumanCausation_Diff_T2T1)
mean(ConsensGroup$HumanCausation_Diff_T3T2)
sd(ConsensGroup$HumanCausation_Diff_T3T2)
mean(ConsensGroup$HumanCausation_Diff_T3T1)
sd(ConsensGroup$HumanCausation_Diff_T3T1)

#Belief in Human Causation Balanced Group 
mean(BalanceGroup$HumanCausation.T1)
sd(BalanceGroup$HumanCausation.T1)
mean(BalanceGroup$HumanCausation.T2)
sd(BalanceGroup$HumanCausation.T2)
mean(BalanceGroup$HumanCausation.T3)
sd(BalanceGroup$HumanCausation.T3)

mean(BalanceGroup$HumanCausation_Diff_T2T1)
sd(BalanceGroup$HumanCausation_Diff_T2T1)
mean(BalanceGroup$HumanCausation_Diff_T3T2)
sd(BalanceGroup$HumanCausation_Diff_T3T2)
mean(BalanceGroup$HumanCausation_Diff_T3T1)
sd(BalanceGroup$HumanCausation_Diff_T3T1)

#Support for Public Action Whole Sample 
mean(filtereddata$SupportForAction.T1)
sd(filtereddata$SupportForAction.T1)
mean(filtereddata$SupportForAction.T2)
sd(filtereddata$SupportForAction.T2)
mean(filtereddata$SupportForAction.T3)
sd(filtereddata$SupportForAction.T3)

mean(filtereddata$SupportForAction_Diff_T2T1)
sd(filtereddata$SupportForAction_Diff_T2T1)
mean(filtereddata$SupportForAction_Diff_T3T2)
sd(filtereddata$SupportForAction_Diff_T3T2)
mean(filtereddata$SupportForAction_Diff_T3T1)
sd(filtereddata$SupportForAction_Diff_T3T1)

#Support for Public Action Consensus Group
mean(ConsensGroup$SupportForAction.T1)
sd(ConsensGroup$SupportForAction.T1)
mean(ConsensGroup$SupportForAction.T2)
sd(ConsensGroup$SupportForAction.T2)
mean(ConsensGroup$SupportForAction.T3)
sd(ConsensGroup$SupportForAction.T3)

mean(ConsensGroup$SupportForAction_Diff_T2T1)
sd(ConsensGroup$SupportForAction_Diff_T2T1)
mean(ConsensGroup$SupportForAction_Diff_T3T2)
sd(ConsensGroup$SupportForAction_Diff_T3T2)
mean(ConsensGroup$SupportForAction_Diff_T3T1)
sd(ConsensGroup$SupportForAction_Diff_T3T1)

#Support for Public Action Balanced Group
mean(BalanceGroup$SupportForAction.T1)
sd(BalanceGroup$SupportForAction.T1)
mean(BalanceGroup$SupportForAction.T2)
sd(BalanceGroup$SupportForAction.T2)
mean(BalanceGroup$SupportForAction.T3)
sd(BalanceGroup$SupportForAction.T3)

mean(BalanceGroup$SupportForAction_Diff_T2T1)
sd(BalanceGroup$SupportForAction_Diff_T2T1)
mean(BalanceGroup$SupportForAction_Diff_T3T2)
sd(BalanceGroup$SupportForAction_Diff_T3T2)
mean(BalanceGroup$SupportForAction_Diff_T3T1)
sd(BalanceGroup$SupportForAction_Diff_T3T1)

# Corr Table Dependent variables
install.packages("openxlsx")
library(openxlsx)

corr_sub <- subset(filtereddata, select = c(PSC.T1, PSC.T2, PSC.T3, Belief.T1, Belief.T2, Belief.T3, 
                                            Worry.T1, Worry.T2, Worry.T3, HumanCausation.T1, HumanCausation.T2,
                                            HumanCausation.T3, SupportForAction.T1, SupportForAction.T2,
                                            SupportForAction.T3))

corr_tab <- cor(corr_sub, method="spearman")
corr_tab
setwd("~/Desktop/MACambridge/00_Thesis")
write.csv2(corr_tab, file="corrtable.xlsx")
write.csv(corr_tab, file="corrtable2.xlsx")


#Sensitivity Power Analysis 
install.packages("pwr")
library(pwr)

groups <- 2
n <- sort(table(filtereddata$Condition[filtereddata$Complete == TRUE]))[1]
pow <- 0.95
sig <- 0.05
(pow_analysis <- pwr.anova.test(k = groups, n = n, f = NULL, sig.level = sig, power = pow))
(d <- (pow_analysis$f*2)) # Enough power for effect sizes d = .51

(pow_analysis2 <- pwr.chisq.test(w = NULL, N = 6, df = 5, sig.level = 0.05, power = 0.95))
(d2 <- (pow_analysis2$w*2)) # 3.6314

######################## Main Study 
install.packages("lsr")
library(lsr)
install.packages(plyr)
library(plyr)
install.packages("sjstats")
library(sjstats)
install.packages(tidyverse)
library(tidyverse)
install.packages("ggpubr")
library(ggpubr)
library(rstatix)
library(broom)

########### Prerequisite Testing - Model Assumption Checks 
# Homogeneity of variances (homoscedasticity) - true but p=.051 so disputable
install.packages("carData")
library(car)
leveneTest(PSC.T3 ~ Condition, data = filtereddata)

# Homogeneity of regression slopes - significant, meaning violation 
RegSlop <- aov(PSC.T3 ~ Condition*PSC.T2, data = filtereddata)
summary(RegSlop)

library(ggplot2)
scatter <- ggplot(filtereddata, aes(PSC.T2, PSC.T3, colour = filtereddata$Condition))
scatter + geom_point(aes(shape = filtereddata$Condition), size = 3) + 
  geom_smooth(method = "lm", aes(fill = filtereddata$Condition), alpha = 0.1) +
  labs(x = "PSC.T2", y = "PSC.T3")


# Normality of Residuals - Kolmogorov significant, violates assumption
install.packages("olsrr")
library(olsrr)

NormRes <- lm(PSC.T3 ~ PSC.T2 + Condition, data = filtereddata)
summary(NormRes)
plot(NormRes)
ols_plot_resid_qq(NormRes)
ols_test_normality(NormRes)

#Linearity Assumption - not linear - assumption violated 
ggscatter(
  filtereddata, x = "PSC.T2", y = "PSC.T3",
  color = "Condition", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Condition)
  )

# Outliers
boxplot(filtereddata$PSC.T3)
freq_table(filtereddata$PSC.T3 < 80)

outliers <- function(filtereddata) {
  mean_value <- mean(filtereddata$PSC.T3)
  sd_value <- sd(filtereddata$PSC.T3)
  threshold <- mean_value + 3*sd_value
  return(filtereddata$PSC.T3[filtereddata$PSC.T3 > threshold])
}

bigoutliers <- outliers(filtereddata)
print(bigoutliers)

print(sd(filtereddata$PSC.T3)*3>mean(filtereddata$PSC.T3))
print(sd(filtereddata$PSC.T2)*3>mean(filtereddata$PSC.T2))

# Cooks Distances - all < 1
CookModel <- lm(PSC.T3 ~ Condition + PSC.T2, data=filtereddata)
cooks.distance(CookModel)
cooks.distance(CookModel)[which.max(cooks.distance(CookModel))]
plot(CookModel,which=4)


####### H1 ANCOVA
# Normal ANCOVA
library(effectsize)
H11 <- aov(PSC.T3 ~ Condition + PSC.T2, data=filtereddata)
print(summary(H11), digits=7)
eta_squared(H11, partial = FALSE)
eta_squared(H11)
omega_squared(H11, partial = FALSE)
epsilon_squared(H11, partial = FALSE)

#Plot
library(ggplot2)
ggplot(data = filtereddata, aes(x = PSC.T2, y = PSC.T3, color = Condition)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme(panel.background = element_rect(fill = "white")) +
  labs(x = "Covariate (PSC.T2)", y = "Dependent variable (PSC.T3)", color = "Condition")

### Confidence Intervals for Group Means Reported in Graph
# T2 Consensus Group 
sample_dataCT2 <- ConsensGroup$PSC.T2
confidence_levelCT2 <- 0.95
sample_meanCT2 <- mean(sample_dataCT2)
standard_errorCT2 <- sd(sample_dataCT2) / sqrt(length(sample_dataCT2))
dfCT2 <- length(sample_dataCT2) - 1
t_valueCT2 <- qt((1 + confidence_levelCT2) / 2, dfCT2)
margin_of_errorCT2 <- t_valueCT2 * standard_errorCT2
lower_boundCT2 <- sample_meanCT2 - margin_of_errorCT2
upper_boundCT2 <- sample_meanCT2 + margin_of_errorCT2

cat("Sample Mean:", sample_meanCT2, "\n")
cat("Confidence Interval:", lower_boundCT2, "to", upper_boundCT2)

# T3 Consensus Group 
sample_dataCT3 <- ConsensGroup$PSC.T3
confidence_levelCT3 <- 0.95
sample_meanCT3 <- mean(sample_dataCT3)
standard_errorCT3 <- sd(sample_dataCT3) / sqrt(length(sample_dataCT3))
dfCT3 <- length(sample_dataCT3) - 1
t_valueCT3 <- qt((1 + confidence_levelCT3) / 2, dfCT3)
margin_of_errorCT3 <- t_valueCT3 * standard_errorCT3
lower_boundCT3 <- sample_meanCT3 - margin_of_errorCT3
upper_boundCT3 <- sample_meanCT3 + margin_of_errorCT3

cat("Sample Mean:", sample_meanCT3, "\n")
cat("Confidence Interval:", lower_boundCT3, "to", upper_boundCT3)

# T2 Balance Group 
sample_dataBT2 <- BalanceGroup$PSC.T2
confidence_levelBT2 <- 0.95
sample_meanBT2 <- mean(sample_dataBT2)
standard_errorBT2 <- sd(sample_dataBT2) / sqrt(length(sample_dataBT2))
dfBT2 <- length(sample_dataBT2) - 1
t_valueBT2 <- qt((1 + confidence_levelBT2) / 2, dfBT2)
margin_of_errorBT2 <- t_valueBT2 * standard_errorBT2
lower_boundBT2 <- sample_meanBT2 - margin_of_errorBT2
upper_boundBT2 <- sample_meanBT2 + margin_of_errorBT2

cat("Sample Mean:", sample_meanBT2, "\n")
cat("Confidence Interval:", lower_boundBT2, "to", upper_boundBT2)

# T3 Balance Group 
sample_dataBT3 <- BalanceGroup$PSC.T3
confidence_levelBT3 <- 0.95
sample_meanBT3 <- mean(sample_dataBT3)
standard_errorBT3 <- sd(sample_dataBT3) / sqrt(length(sample_dataBT3))
dfBT3 <- length(sample_dataBT3) - 1
t_valueBT3 <- qt((1 + confidence_levelBT3) / 2, dfBT3)
margin_of_errorBT3 <- t_valueBT3 * standard_errorBT3
lower_boundBT3 <- sample_meanBT3 - margin_of_errorBT3
upper_boundBT3 <- sample_meanBT3 + margin_of_errorBT3

cat("Sample Mean:", sample_meanBT3, "\n")
cat("Confidence Interval:", lower_boundBT3, "to", upper_boundBT3)

# standard error 
install.packages("plotrix")
library(plotrix)
mean(BalanceGroup$PSC.T3)
std.error(BalanceGroup$PSC.T3)
mean(BalanceGroup$PSC.T3) - std.error(BalanceGroup$PSC.T3)
mean(BalanceGroup$PSC.T3) + std.error(BalanceGroup$PSC.T3)

### Accounted for Normal Distribution - Log Transformed Values 
library(moments)

# Distribution of PSC.T3
ggdensity(filtereddata, x = "PSC.T3", fill = "lightgray", title = "PSC.T3") +
  scale_x_continuous(limits = c(0, 110)) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
skewness(filtereddata$PSC.T3, na.rm = TRUE)

# Distribution of PSC.T2
ggdensity(filtereddata, x = "PSC.T2", fill = "lightgray", title = "PSC.T2") +
  scale_x_continuous(limits = c(0, 110)) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
skewness(filtereddata$PSC.T2, na.rm = TRUE)

# new Subset & log transformation of PSC.T3 and PSC.T2
logfiltereddata <- subset(filtereddata)
logfiltereddata$PSC.T3 <- log10(max(logfiltereddata$PSC.T3+1) - logfiltereddata$PSC.T3)
skewness(logfiltereddata$PSC.T3, na.rm = TRUE)
logfiltereddata$PSC.T2 <- log10(max(logfiltereddata$PSC.T2+1) - logfiltereddata$PSC.T2)
skewness(logfiltereddata$PSC.T2, na.rm = TRUE)

## Assumption checking for log values 
# Homogeneity of regression slopes - Test for log Data 
logRegSlop <- aov(PSC.T3 ~ Condition*PSC.T2, data = logfiltereddata)
summary(logRegSlop)

# Normality of Residuals - Test for log Data - still sign. 
logNormRes <- lm(PSC.T3 ~ PSC.T2 + Condition, data = logfiltereddata)
summary(logNormRes)
plot(logNormRes)
ols_plot_resid_qq(logNormRes)
ols_test_normality(logNormRes)

# Linearity Assumption - Test for Log Data - not linear 
ggscatter(
  logfiltereddata, x = "PSC.T2", y = "PSC.T3",
  color = "Condition", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Condition)
  )

## new ANCOVA with log values
H1log <- aov(PSC.T3 ~ Condition + PSC.T2 , data=logfiltereddata)
print(summary(H1log), digits=7)
eta_squared(H1log, partial = FALSE)
eta_squared(H1log)
omega_squared(H1log, partial = FALSE)
epsilon_squared(H1log, partial = FALSE)

library(lsr)
## Other tests
# t Test with difference scores 
t.test(ConsensGroup$PSC_Diff_T3T2, BalanceGroup$PSC_Diff_T3T2)
cohensD(BalanceGroup$PSC_Diff_T3T2, y= ConsensGroup$PSC_Diff_T3T2, method = 'unequal')

# Mean Diff in Diffs 
mean(BalanceGroup$PSC_Diff_T3T2)
mean(ConsensGroup$PSC_Diff_T3T2)
mean(BalanceGroup$PSC_Diff_T3T2) - mean(ConsensGroup$PSC_Diff_T3T2)

# non parametric test - Mann Whitney U - not sign. 
wilcox.test(PSC_Diff_T3T2 ~ Condition, data=filtereddata, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)

boxplot(PSC.T3 ~ Condition, data = filtereddata, xlab= "Condition", ylab= "PSC.T3", 
        col=(c("gold","darkgreen")), min=60, max=100)

#### Hypothesis 2 Path Model 
library(semTools)
library(psych)
library(xtable)
library(apaTables)
library(stargazer)
library(semPlot)
library(lavaan)

# Reverse GBM Model 1 (Between Subjects, t1)
data <- filtereddata
data$HC <- data$'HumanCausation.T3'
data$PSC <- data$'PSC.T3'
data$WOR <- data$'Worry.T3'
data$BEL <- data$'Belief.T3'
data$SUP <- data$'SupportForAction.T3'
data$mis <- ifelse(data$Condition == 'Consensus', 0, 1)
rgbm.model <- '
              # Covariance
              BEL ~~ d*HC
              
              # Direct Effects
              PSC ~ a*mis     
              HC ~ b*PSC
              BEL ~ c*PSC
              WOR ~ e*PSC + f*HC + g*BEL
              SUP ~ h*BEL + i*HC + j*WOR
              
              # Indirect Effects
              B_W_S := g*j
              H_W_S := f*j
              P_W_S := e*j
              P_H1S := b*i
              P_H2S := b*f*j
              P_B2S := c*h
              P_B2S := c*g*j
              M_P_W := a*e
              M_P_H := a*b
              M_P_B := a*c
              M_S   := (a*b*i) + (a*c*h) + (a*e*j) + (a*b*f*j) + (a*c*g*j)
              
              # Total Effects
              Ptot  := (b*i) + (c*h) + (e*j) + (c*g*j) + (b*f*j)
              Htot  := i + (f*j)
              Btot  := h + (g*j)
              '
rgbm.fit <- sem(model=rgbm.model,data=data, test = 'Satorra-Bentler')
semPaths(rgbm.fit, layout = "tree2", edge.label.cex = .8, rotation = 2)
summary(rgbm.fit, standardized = TRUE, rsquare = TRUE, fit.measures = TRUE)
parameterEstimates(rgbm.fit, standardized = TRUE)
standardizedSolution(rgbm.fit)
print(xtable(standardizedSolution(rgbm.fit), digits = 3), type = "html")

# Model Plotting
rgbm_chi <- as.character(sprintf("%.2f",round(fitMeasures(rgbm.fit, c("chisq.scaled", "pvalue.scaled", "CFI.robust", "TLI.robust", 
                                                                      "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
                                                                      "srmr", "wrmr"))[1], digits = 2)))
rgbm_p <- as.character(sprintf("%.2f",round(fitMeasures(rgbm.fit, c("chisq.scaled", "pvalue.scaled", "CFI.robust", "TLI.robust", 
                                                                    "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
                                                                    "srmr", "wrmr"))[2], digits = 3)))
rgbm_CFI <- as.character(sprintf("%.2f",round(fitMeasures(rgbm.fit, c("chisq.scaled", "pvalue.scaled", "CFI.robust", "TLI.robust", 
                                                                      "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
                                                                      "srmr", "wrmr"))[3], digits = 2)))
rgbm_TLI <- as.character(sprintf("%.2f",round(fitMeasures(rgbm.fit, c("chisq.scaled", "pvalue.scaled", "CFI.robust", "TLI.robust", 
                                                                      "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
                                                                      "srmr", "wrmr"))[4], digits = 2)))
rgbm_RMSEA <- as.character(sprintf("%.2f",round(fitMeasures(rgbm.fit, c("chisq.scaled", "pvalue.scaled", "CFI.robust", "TLI.robust", 
                                                                        "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
                                                                        "srmr", "wrmr"))[5], digits = 3)))
rgbm_RMSEA_LL <- as.character(sprintf("%.2f",round(fitMeasures(rgbm.fit, c("chisq.scaled", "pvalue.scaled", "CFI.robust", "TLI.robust", 
                                                                           "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
                                                                           "srmr", "wrmr"))[6], digits = 3)))
rgbm_RMSEA_UL <- as.character(sprintf("%.2f",round(fitMeasures(rgbm.fit, c("chisq.scaled", "pvalue.scaled", "CFI.robust", "TLI.robust", 
                                                                           "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
                                                                           "srmr", "wrmr"))[7], digits = 3)))
rgbm_SRMR <- as.character(sprintf("%.2f",round(fitMeasures(rgbm.fit, c("chisq.scaled", "pvalue.scaled", "CFI.robust", "TLI.robust", 
                                                                       "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
                                                                       "srmr", "wrmr"))[8], digits = 3)))

rgbm_plot <- function()
{semPaths(rgbm.fit, layout = "tree2", what = "std", whatLabels = "std", 
          edge.label.cex = .65, rotation = 2)
  legend(x="topleft", y=1, 
         legend = c("",
                    expression(paste(bold("Model Fit"))),
                    "",
                    bquote(chi^2 ~ "=" ~ .(rgbm_chi) * "," ~ italic(p) ~ "=" ~ .(rgbm_p)),
                    bquote(italic(CFI) ~ "=" ~ .(rgbm_CFI)),
                    bquote(italic(TLI) ~ "=" ~ .(rgbm_TLI)),
                    bquote(italic(RMSEA) ~ "=" ~ .(rgbm_RMSEA)),
                    bquote("(90%" ~ italic(CI) ~ "[" * .(rgbm_RMSEA_LL) * "," ~ .(rgbm_RMSEA_UL) * "])"),
                    bquote(italic(SRMR) ~ "=" ~ .(rgbm_SRMR))),
         bty = "n", 
         cex = 0.6)}

rgbm_plot()

#Normality of RGBM
datask <- subset(data[, c("HC", "WOR", "mis", "BEL", "SUP", "PSC")])
mardiaKurtosis(datask, use = "everything")
mardiaSkew(datask, use = "everything")






###### Appendix 
## Group Differences Dependent Variables 
install.packages("carData")
library(car)
#PSC 
leveneTest(PSC.T1 ~ Condition, data=filtereddata)
leveneTest(PSC.T2 ~ Condition, data=filtereddata)
leveneTest(PSC.T3 ~ Condition, data=filtereddata)

t.test(ConsensGroup$PSC.T1, BalanceGroup$PSC.T1)
cohens_d(ConsensGroup$PSC.T1, y=BalanceGroup$PSC.T1, method = 'unequal')
mean(ConsensGroup$PSC.T1) -mean(BalanceGroup$PSC.T1)
mean(ConsensGroup$PSC.T1)
mean(BalanceGroup$PSC.T1)

t.test(ConsensGroup$PSC.T2, BalanceGroup$PSC.T2)
cohens_d(ConsensGroup$PSC.T2, y=BalanceGroup$PSC.T2, method = 'unequal')
mean(ConsensGroup$PSC.T2) - mean(BalanceGroup$PSC.T2)
mean(ConsensGroup$PSC.T2)
mean(BalanceGroup$PSC.T2)

t.test(ConsensGroup$PSC.T3, BalanceGroup$PSC.T3)
cohens_d(ConsensGroup$PSC.T3, y=BalanceGroup$PSC.T3, method = 'unequal')
mean(ConsensGroup$PSC.T3) - mean(BalanceGroup$PSC.T3)
mean(ConsensGroup$PSC.T3)
mean(BalanceGroup$PSC.T3)

t.test(ConsensGroup$PSC_Diff_T2T1, BalanceGroup$PSC_Diff_T2T1)
t.test(ConsensGroup$PSC_Diff_T3T2, BalanceGroup$PSC_Diff_T3T2)
mean(ConsensGroup$PSC_Diff_T3T2)-mean(BalanceGroup$PSC_Diff_T3T2)
cohens_d(ConsensGroup$PSC_Diff_T3T2, BalanceGroup$PSC_Diff_T3T2)
t.test(ConsensGroup$PSC_Diff_T3T1, BalanceGroup$PSC_Diff_T3T1)

#Belief in CC
leveneTest(Belief.T1 ~ Condition, data=filtereddata)
leveneTest(Belief.T2 ~ Condition, data=filtereddata)
leveneTest(Belief.T3 ~ Condition, data=filtereddata)

t.test(ConsensGroup$Belief.T1, BalanceGroup$Belief.T1)
t.test(ConsensGroup$Belief.T2, BalanceGroup$Belief.T2)
t.test(ConsensGroup$Belief.T3, BalanceGroup$Belief.T3)

t.test(ConsensGroup$Belief_Diff_T2T1, BalanceGroup$Belief_Diff_T2T1)
t.test(ConsensGroup$Belief_Diff_T3T2, BalanceGroup$Belief_Diff_T3T2)
t.test(ConsensGroup$Belief_Diff_T3T1, BalanceGroup$Belief_Diff_T3T1)

# Worry 
t.test(ConsensGroup$Worry.T1, BalanceGroup$Worry.T1)
t.test(ConsensGroup$Worry.T2, BalanceGroup$Worry.T2)
t.test(ConsensGroup$Worry.T3, BalanceGroup$Worry.T3)

leveneTest(Worry.T1 ~ Condition, data=filtereddata)
leveneTest(Worry.T2 ~ Condition, data=filtereddata)
leveneTest(Worry.T3 ~ Condition, data=filtereddata)

t.test(ConsensGroup$Worry_Diff_T2T1, BalanceGroup$Worry_Diff_T2T1)
t.test(ConsensGroup$Worry_Diff_T3T2, BalanceGroup$Worry_Diff_T3T2)
t.test(ConsensGroup$Worry_Diff_T3T1, BalanceGroup$Worry_Diff_T3T1)

# Belief in Human Causation 
t.test(ConsensGroup$HumanCausation.T1, BalanceGroup$HumanCausation.T1)
t.test(ConsensGroup$HumanCausation.T2, BalanceGroup$HumanCausation.T2)
t.test(ConsensGroup$HumanCausation.T3, BalanceGroup$HumanCausation.T3)

leveneTest(HumanCausation.T1 ~ Condition, data=filtereddata)
leveneTest(HumanCausation.T2 ~ Condition, data=filtereddata)
leveneTest(HumanCausation.T3 ~ Condition, data=filtereddata)

t.test(ConsensGroup$HumanCausation_Diff_T2T1, BalanceGroup$HumanCausation_Diff_T2T1)
t.test(ConsensGroup$HumanCausation_Diff_T3T2, BalanceGroup$HumanCausation_Diff_T3T2)
t.test(ConsensGroup$HumanCausation_Diff_T3T1, BalanceGroup$HumanCausation_Diff_T3T1)

# Support for Public Action
t.test(ConsensGroup$SupportForAction.T1, BalanceGroup$SupportForAction.T1)
t.test(ConsensGroup$SupportForAction.T2, BalanceGroup$SupportForAction.T2)
t.test(ConsensGroup$SupportForAction.T3, BalanceGroup$SupportForAction.T3)

leveneTest(SupportForAction.T1 ~ Condition, data=filtereddata)
leveneTest(SupportForAction.T2 ~ Condition, data=filtereddata)
leveneTest(SupportForAction.T2 ~ Condition, data=filtereddata)

t.test(ConsensGroup$SupportForAction_Diff_T2T1, BalanceGroup$SupportForAction_Diff_T2T1)
t.test(ConsensGroup$SupportForAction_Diff_T3T2, BalanceGroup$SupportForAction_Diff_T3T2)
t.test(ConsensGroup$SupportForAction_Diff_T3T1, BalanceGroup$SupportForAction_Diff_T3T1)


### Exploratory Analysis - Within-Subject Model rGBM 
dataWithin <- subset(filtereddata, Condition=="Balanced")
dataWithin$LongGroup <- 0
dataWithin2 <- rbind(dataWithin, dataWithin)
new_rows <- (nrow(dataWithin) + 1):nrow(dataWithin2)
dataWithin2$LongGroup[new_rows] <- 1
dataWithin2 <- dataWithin2[order(dataWithin2$id),]

# Reverse GBM Model (Within Subjects)
dataRGBM3 <- dataWithin2
dataRGBM3$HC <- ifelse(dataRGBM3$LongGroup == 1, dataRGBM3$HumanCausation.T3, dataRGBM3$HumanCausation.T2)
dataRGBM3$PSC <- ifelse(dataRGBM3$LongGroup == 1, dataRGBM3$PSC.T3, dataRGBM3$PSC.T2)
dataRGBM3$WOR <- ifelse(dataRGBM3$LongGroup == 1, dataRGBM3$Worry.T3, dataRGBM3$Worry.T2)
dataRGBM3$BEL <- ifelse(dataRGBM3$LongGroup == 1, dataRGBM3$Belief.T3, dataRGBM3$Belief.T2)
dataRGBM3$SUP <- ifelse(dataRGBM3$LongGroup == 1, dataRGBM3$SupportForAction.T3, dataRGBM3$SupportForAction.T2)
dataRGBM3$mis <- dataRGBM3$LongGroup
rgbm.model3 <- '
              # Covariance
              BEL ~~ d*HC
              
              # Direct Effects
              PSC ~ a*mis     
              HC ~ b*PSC
              BEL ~ c*PSC
              WOR ~ e*PSC + f*HC + g*BEL
              SUP ~ h*BEL + i*HC + j*WOR
              
              # Indirect Effects
              B_W_S := g*j
              H_W_S := f*j
              P_W_S := e*j
              P_H1S := b*i
              P_H2S := b*f*j
              P_B2S := c*h
              P_B2S := c*g*j
              M_P_W := a*e
              M_P_H := a*b
              M_P_B := a*c
              M_S   := (a*b*i) + (a*c*h) + (a*e*j) + (a*b*f*j) + (a*c*g*j)
              
              # Total Effects
              Ptot  := (b*i) + (c*h) + (e*j) + (c*g*j) + (b*f*j)
              Htot  := i + (f*j)
              Btot  := h + (g*j)
              '
rgbm.fit3 <- sem(model=rgbm.model3,data=dataRGBM3, test = 'Satorra-Bentler')
summary(rgbm.fit3, standardized = TRUE, rsquare = TRUE, fit.measures = TRUE)
parameterEstimates(rgbm.fit3, standardized = TRUE)
standardizedSolution(rgbm.fit3)
semPaths(rgbm.fit3, layout = "tree2", what = "std", whatLabels = "std", edge.label.cex = .65, rotation = 2)

#export table as excel
library(openxlsx)
table_data <- standardizedSolution(rgbm.fit3)
table_data <- round(table_data, digits = 3)
excel_file <- "table_output.xlsx"
write.xlsx(table_data, excel_file, rowNames = FALSE)
getwd()

# Model Plotting
rgbm3_chi <- as.character(sprintf("%.2f",round(fitMeasures(rgbm.fit3, c("chisq.scaled", "pvalue.scaled", "CFI.robust", "TLI.robust", 
                                                                        "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
                                                                        "srmr", "wrmr"))[1], digits = 2)))
rgbm3_p <- as.character(sprintf("%.2f",round(fitMeasures(rgbm.fit3, c("chisq.scaled", "pvalue.scaled", "CFI.robust", "TLI.robust", 
                                                                      "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
                                                                      "srmr", "wrmr"))[2], digits = 3)))
rgbm3_CFI <- as.character(sprintf("%.2f",round(fitMeasures(rgbm.fit3, c("chisq.scaled", "pvalue.scaled", "CFI.robust", "TLI.robust", 
                                                                        "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
                                                                        "srmr", "wrmr"))[3], digits = 2)))
rgbm3_TLI <- as.character(sprintf("%.2f",round(fitMeasures(rgbm.fit3, c("chisq.scaled", "pvalue.scaled", "CFI.robust", "TLI.robust", 
                                                                        "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
                                                                        "srmr", "wrmr"))[4], digits = 2)))
rgbm3_RMSEA <- as.character(sprintf("%.2f",round(fitMeasures(rgbm.fit3, c("chisq.scaled", "pvalue.scaled", "CFI.robust", "TLI.robust", 
                                                                          "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
                                                                          "srmr", "wrmr"))[5], digits = 3)))
rgbm3_RMSEA_LL <- as.character(sprintf("%.2f",round(fitMeasures(rgbm.fit3, c("chisq.scaled", "pvalue.scaled", "CFI.robust", "TLI.robust", 
                                                                             "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
                                                                             "srmr", "wrmr"))[6], digits = 3)))
rgbm3_RMSEA_UL <- as.character(sprintf("%.2f",round(fitMeasures(rgbm.fit3, c("chisq.scaled", "pvalue.scaled", "CFI.robust", "TLI.robust", 
                                                                             "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
                                                                             "srmr", "wrmr"))[7], digits = 3)))
rgbm3_SRMR <- as.character(sprintf("%.2f",round(fitMeasures(rgbm.fit3, c("chisq.scaled", "pvalue.scaled", "CFI.robust", "TLI.robust", 
                                                                         "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
                                                                         "srmr", "wrmr"))[8], digits = 3)))

rgbm3_plot <- function()
{semPaths(rgbm.fit3, layout = "tree2", what = "std", whatLabels = "std", 
          edge.label.cex = .65, rotation = 2)
  legend(x="topleft", y=1, 
         legend = c("",
                    expression(paste(bold("Model Fit"))),
                    "",
                    bquote(chi^2 ~ "=" ~ .(rgbm3_chi) * "," ~ italic(p) ~ "=" ~ .(rgbm3_p)),
                    bquote(italic(CFI) ~ "=" ~ .(rgbm3_CFI)),
                    bquote(italic(TLI) ~ "=" ~ .(rgbm3_TLI)),
                    bquote(italic(RMSEA) ~ "=" ~ .(rgbm3_RMSEA)),
                    bquote("(90%" ~ italic(CI) ~ "[" * .(rgbm3_RMSEA_LL) * "," ~ .(rgbm3_RMSEA_UL) * "])"),
                    bquote(italic(SRMR) ~ "=" ~ .(rgbm3_SRMR))),
         bty = "n", 
         cex = 0.5)}

rgbm3_plot()


