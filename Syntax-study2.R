## The Reversed Gateway Belief Model - Hannah Timna Logemann

# Experiment 2 - Data Set from "Psychological Booster Shots Targeting Memory Increase Long-Term Resistance Against Misinformation" (Meartens et al., 2023)
# Data Set is stored online: https://osf.io/9zxje/?view_only=44a8556694b54d09a2e2a9875071de2f

setwd("/Users/hannahtimnalogemann/Desktop/MACambridge/03Study2/03_Data")
library(readr)
study2<-read.csv("/Users/hannahtimnalogemann/Desktop/MACambridge/03Study2/03_Data/CCDecay2CleanDatasetWideA.csv")

##### Create Subset for Control Group only 
filtereddata <- subset(study2, Group=="Control")

##### Transformations - Create Post Variables that unite all Control Groups (T1, T2, T3)
#Belief in CC
BeliefInCCPost <- function(filtereddata) {
  filtereddata$BeliefInCC.Post <- ifelse(!is.na(filtereddata$BeliefInCC.T1), filtereddata$BeliefInCC.T1,
                                 ifelse(!is.na(filtereddata$BeliefInCC.T2), filtereddata$BeliefInCC.T2,
                                        ifelse(!is.na(filtereddata$BeliefInCC.T3), filtereddata$BeliefInCC.T3, NA)))
  return(filtereddata)
}

#Belief in Human Causation
HumanCausation.Post <- function(filtereddata) {
  filtereddata$HumanCausation.Post <- ifelse(!is.na(filtereddata$HumanCausation.T1), filtereddata$HumanCausation.T1,
                                         ifelse(!is.na(filtereddata$HumanCausation.T2), filtereddata$HumanCausation.T2,
                                                ifelse(!is.na(filtereddata$HumanCausation.T3), filtereddata$HumanCausation.T3, NA)))
  return(filtereddata)
}

# PSC
PSC.Post <- function(filtereddata) {
  filtereddata$PSC.Post <- ifelse(!is.na(filtereddata$PSC.T1), filtereddata$PSC.T1,
                                             ifelse(!is.na(filtereddata$PSC.T2), filtereddata$PSC.T2,
                                                    ifelse(!is.na(filtereddata$PSC.T3), filtereddata$PSC.T3, NA)))
  return(filtereddata)
}

# Worry
Worry.Post <- function(filtereddata) {
  filtereddata$Worry.Post <- ifelse(!is.na(filtereddata$Worry.T1), filtereddata$Worry.T1,
                                  ifelse(!is.na(filtereddata$Worry.T2), filtereddata$Worry.T2,
                                         ifelse(!is.na(filtereddata$Worry.T3), filtereddata$Worry.T3, NA)))
  return(filtereddata)
}

# Support For Action
SupportForAction.Post <- function(filtereddata) {
  filtereddata$SupportForAction.Post <- ifelse(!is.na(filtereddata$SupportForAction.T1), filtereddata$SupportForAction.T1,
                                  ifelse(!is.na(filtereddata$SupportForAction.T2), filtereddata$SupportForAction.T2,
                                         ifelse(!is.na(filtereddata$SupportForAction.T3), filtereddata$SupportForAction.T3, NA)))
  return(filtereddata)
}

# Adding transformed variables to dataset
filtereddata2 <- BeliefInCCPost(filtereddata)
filtereddata3 <- HumanCausation.Post(filtereddata2)
filtereddata4 <- PSC.Post(filtereddata3)
filtereddata5 <- Worry.Post(filtereddata4)
finaldata <- SupportForAction.Post(filtereddata5)

# Checking Transformation for Belief in CC T1, T2, T3 and Post
createTable <- function(finaldata) {
  tableData <- finaldata[, c("ID", "BeliefInCC.T1", "BeliefInCC.T2", "BeliefInCC.T3", "BeliefInCC.Post")]
  return(tableData)
}
CompBEL <- createTable(finaldata)
print(CompBEL)

## Transform Year of Birth to Years of Age 
# Assuming the current year is 2023
currentYear <- 2023

# Transform Age_Year to Age
finaldata$Age <- currentYear - finaldata$Age_Year

##### Sample Demographics
#Age 
mean(finaldata$Age)
sd(finaldata$Age)
max(finaldata$Age)
min(finaldata$Age)
median(finaldata$Age)

# Create Categories for Age 
finaldata$Age_Category <- cut(finaldata$Age, 
                              breaks = c(18, 30, 50, Inf), 
                              labels = c("18-29", "30-49", "50+"), 
                              include.lowest = TRUE)

summary(finaldata$Age_Category)

# Sex 
freq_table(finaldata$Gender)

# Education 
freq_table(finaldata$Education)
mean(finaldata$Education)
sd(finaldata$Education)
freq_table(finaldata$Education_Groups)
freq_table(finaldata$Education_Category)

# Ideology 
mean(finaldata$Ideology)
sd(finaldata$Ideology)
freq_table(finaldata$Ideology_Category1)
freq_table(finaldata$Ideology_Category2)

# Political Party 
freq_table(finaldata$PoliticalParty)

# State 
TableState <- freq_table(finaldata$State)
print(TableState, n= 800)

# News Social Media 
mean(finaldata$News_SocialMedia)
sd(finaldata$News_SocialMedia)
freq_table(finaldata$News_SocialMedia)

# News_Twitter 
mean(finaldata$News_Twitter)
sd(finaldata$News_Twitter)
freq_table(finaldata$News_Twitter)

# News_Outlet
view(finaldata$NewsOutlet)

##### Descriptives 
mean(finaldata$PSC.Pre)
sd(finaldata$PSC.Pre)
mean(finaldata$BeliefInCC.Pre)
sd(finaldata$BeliefInCC.Pre)
mean(finaldata$HumanCausation.Pre)
sd(finaldata$HumanCausation.Pre)
mean(finaldata$Worry.Pre)
sd(finaldata$Worry.Pre)
mean(finaldata$SupportForAction.Pre)
sd(finaldata$SupportForAction.Pre)

mean(finaldata$PSC.Post)
sd(finaldata$PSC.Post)
mean(finaldata$BeliefInCC.Post)
sd(finaldata$BeliefInCC.Post)
mean(finaldata$HumanCausation.Post)
sd(finaldata$HumanCausation.Post)
mean(finaldata$Worry.Post)
sd(finaldata$Worry.Post)
mean(finaldata$SupportForAction.Post)
sd(finaldata$SupportForAction.Post)

##### Create Diff Scores Variables PSC, HC, BEL, WOR, SUP
finaldata$PSC_Diff <- finaldata$PSC.Post - finaldata$PSC.Pre
finaldata$BeliefInCC_Diff <- finaldata$BeliefInCC.Post - finaldata$BeliefInCC.Pre
finaldata$HumanCausation_Diff <- finaldata$HumanCausation.Post - finaldata$HumanCausation.Pre
finaldata$Worry_Diff <- finaldata$Worry.Post - finaldata$Worry.Pre
finaldata$SupportForAction_Diff <- finaldata$SupportForAction.Post - finaldata$SupportForAction.Pre

mean(finaldata$PSC_Diff)
sd(finaldata$PSC_Diff)
mean(finaldata$BeliefInCC_Diff)
sd(finaldata$BeliefInCC_Diff)
mean(finaldata$HumanCausation_Diff)
sd(finaldata$HumanCausation_Diff)
mean(finaldata$Worry_Diff)
sd(finaldata$Worry_Diff)
mean(finaldata$SupportForAction_Diff)
sd(finaldata$SupportForAction_Diff)

############# Hypothesis 1 
# H3 Check of Assumptions for paired t-test
library(stats)
shapiro.test(finaldata$PSC_Diff)
hist(finaldata$PSC_Diff, freq = FALSE, main = "Histogram with Density Curve")
curve(dnorm(x, mean(finaldata$PSC_Diff), sd(finaldata$PSC_Diff)), 
      add = TRUE, col = "blue", lwd = 2)
skew(finaldata$PSC_Diff)
kurtosi(finaldata$PSC_Diff)
plot(density(finaldata$PSC_Diff), main = "Density Plot of PSC_Diff", xlab = "PSC_Diff", ylab = "Density")

# Outliers
boxplot(finaldata$PSC_Diff)
mean_value <- mean(finaldata$PSC_Diff)
sd_value <- sd(finaldata$PSC_Diff)
outliers <- finaldata$PSC_Diff[abs(finaldata$PSC_Diff - mean_value) > 3 * sd_value]
num_outliers <- length(outliers)
cat("Number of subjects who are more than 3 standard deviations away from the mean:", num_outliers, "\n")
cat("Subjects and their values:", outliers, "\n")

# creating variable for outliers and a subset without outliers 
finaldata$Outlier <- ifelse(abs(finaldata$PSC_Diff - mean_value) > 3 * sd_value, 1, 0)
finaldata_wo_outliers<- finaldata[finaldata$Outlier == 0, ]

# testing normal distribution agian without outliers
shapiro.test(finaldata_wo_outliers$PSC_Diff)
hist(finaldata_wo_outliers$PSC_Diff, freq = FALSE, main = "Histogram with Density Curve")
curve(dnorm(x, mean(finaldata_wo_outliers$PSC_Diff), sd(finaldata_wo_outliers$PSC_Diff)), 
      add = TRUE, col = "blue", lwd = 2)
skew(finaldata_wo_outliers$PSC_Diff)
kurtosi(finaldata_wo_outliers$PSC_Diff)
plot(density(finaldata_wo_outliers$PSC_Diff), main = "Density Plot of PSC_Diff", xlab = "PSC_Diff", ylab = "Density")

# H4 Check of Assumptions for path analysis
#Linearity Assumption - not linear - assumption violated 
library(ggplot2)
ggplot(finaldata, aes(x = PSC.Pre, y = BeliefInCC.Pre)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  xlab("PSC.Pre") +
  ylab("BeliefInCC.Pre") +
  ggtitle("Scatter Plot of PSC.Pre and BeliefInCC.Pre")

ggplot(finaldata, aes(x = PSC.Post, y = BeliefInCC.Post)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  xlab("PSC.Post") +
  ylab("BeliefInCC.Post") +
  ggtitle("Scatter Plot of PSC.Post and BeliefInCC.Post")

ggplot(finaldata, aes(x = PSC.Pre, y = Worry.Pre)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  
  xlab("PSC.Pre") +
  ylab("Worry.Pre") +
  ggtitle("Scatter Plot of PSC.Pre and Worry.Pre")

ggplot(finaldata, aes(x = PSC.Post, y = Worry.Post)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  
  xlab("PSC.Post") +
  ylab("Worry.Post") +
  ggtitle("Scatter Plot of PSC.Post and Worry.Post")

ggplot(finaldata, aes(x = PSC.Pre, y = HumanCausation.Pre)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  xlab("PSC.Pre") +
  ylab("HumanCausation.Pre") +
  ggtitle("Scatter Plot of PSC.Pre and HumanCausation.Pre")

ggplot(finaldata, aes(x = PSC.Post, y = HumanCausation.Post)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  xlab("PSC.Pre") +
  ylab("HumanCausation.Post") +
  ggtitle("Scatter Plot of PSC.Post and HumanCausation.Post")

ggplot(finaldata, aes(x = BeliefInCC.Pre, y = SupportForAction.Pre)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add regression line
  xlab("BeliefInCC.Pr") +
  ylab("SupportForAction.Pre") +
  ggtitle("Scatter Plot of BeliefInCC.Pre and SupportForAction.Pre")

ggplot(finaldata, aes(x = BeliefInCC.Post, y = SupportForAction.Post)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add regression line
  xlab("BeliefInCC.Post") +
  ylab("SupportForAction.Post") +
  ggtitle("Scatter Plot of BeliefInCC.Pre and SupportForAction.Post")

ggplot(finaldata, aes(x = Worry.Pre, y = SupportForAction.Pre)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add regression line
  xlab("Worry.Pre") +
  ylab("SupportForAction.Pre") +
  ggtitle("Scatter Plot of Worry.Pre and SupportForAction.Pre")

ggplot(finaldata, aes(x = Worry.Post, y = SupportForAction.Post)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add regression line
  xlab("Worry.Post") +
  ylab("SupportForAction.Post") +
  ggtitle("Scatter Plot of Worry.Post and SupportForAction.Post")

ggplot(finaldata, aes(x = HumanCausation.Pre, y = SupportForAction.Pre)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add regression line
  xlab("HumanCausation.Pre") +
  ylab("SupportForAction.Pre") +
  ggtitle("Scatter Plot of HumanCausation.Pre and SupportForAction.Pre")

ggplot(finaldata, aes(x = HumanCausation.Post, y = SupportForAction.Post)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add regression line
  xlab("HumanCausation.Post") +
  ylab("SupportForAction.Post") +
  ggtitle("Scatter Plot of HumanCausation.Post and SupportForAction.Post")

# Outliers - checking for outliers on all main variables of the RGBM
outliersPSC.Pre <- finaldata$PSC.Pre[abs(finaldata$PSC.Pre - (mean(finaldata$PSC.Pre))) > 3 * (sd(finaldata$PSC.Pre))]
num_outliersPSC.Pre <- length(outliersPSC.Pre)
cat("Number of subjects who are more than 3 standard deviations away from the mean:", num_outliersPSC.Pre, "\n")
cat("Subjects and their values:", outliersPSC.Pre, "\n")

outliersPSC.Post <- finaldata$PSC.Post[abs(finaldata$PSC.Post - (mean(finaldata$PSC.Post))) > 3 * (sd(finaldata$PSC.Post))]
num_outliersPSC.Post <- length(outliersPSC.Post)
cat("Number of subjects who are more than 3 standard deviations away from the mean:", num_outliersPSC.Post, "\n")
cat("Subjects and their values:", outliersPSC.Post, "\n")

outliersBEL.Pre <- finaldata$BeliefInCC.Pre[abs(finaldata$BeliefInCC.Pre - (mean(finaldata$BeliefInCC.Pre))) > 3 * (sd(finaldata$BeliefInCC.Pre))]
num_outliersBEL.Pre <- length(outliersBEL.Pre)
cat("Number of subjects who are more than 3 standard deviations away from the mean:", num_outliersBEL.Pre, "\n")
cat("Subjects and their values:", outliersBEL.Pre, "\n")

outliersBEL.Post <- finaldata$BeliefInCC.Post[abs(finaldata$BeliefInCC.Post - (mean(finaldata$BeliefInCC.Post))) > 3 * (sd(finaldata$BeliefInCC.Post))]
num_outliersBEL.Post <- length(outliersBEL.Pre)
cat("Number of subjects who are more than 3 standard deviations away from the mean:", num_outliersBEL.Post, "\n")
cat("Subjects and their values:", outliersBEL.Post, "\n")

outliersWOR.Pre <- finaldata$Worry.Pre[abs(finaldata$Worry.Pre - (mean(finaldata$Worry.Pre))) > 3 * (sd(finaldata$Worry.Pre))]
num_outliersWOR.Pre <- length(outliersWOR.Pre)
cat("Number of subjects who are more than 3 standard deviations away from the mean:", num_outliersWOR.Pre, "\n")
cat("Subjects and their values:", outliersWOR.Pre, "\n")

outliersWOR.Post <- finaldata$Worry.Post[abs(finaldata$Worry.Post - (mean(finaldata$Worry.Post))) > 3 * (sd(finaldata$Worry.Post))]
num_outliersWOR.Post <- length(outliersWOR.Post)
cat("Number of subjects who are more than 3 standard deviations away from the mean:", num_outliersWOR.Post, "\n")
cat("Subjects and their values:", outliersWOR.Post, "\n")

outliersHC.Pre <- finaldata$HumanCausation.Pre[abs(finaldata$HumanCausation.Pre - (mean(finaldata$HumanCausation.Pre))) > 3 * (sd(finaldata$HumanCausation.Pre))]
num_outliersHC.Pre <- length(outliersHC.Pre)
cat("Number of subjects who are more than 3 standard deviations away from the mean:", num_outliersHC.Pre, "\n")
cat("Subjects and their values:", outliersHC.Pre, "\n")

outliersHC.Post <- finaldata$HumanCausation.Post[abs(finaldata$HumanCausation.Post - (mean(finaldata$HumanCausation.Post))) > 3 * (sd(finaldata$HumanCausation.Post))]
num_outliersHC.Post <- length(outliersHC.Post)
cat("Number of subjects who are more than 3 standard deviations away from the mean:", num_outliersHC.Post, "\n")
cat("Subjects and their values:", outliersHC.Post, "\n")

outliersSUP.Pre <- finaldata$SupportForAction.Pre[abs(finaldata$SupportForAction.Pre - (mean(finaldata$SupportForAction.Pre))) > 3 * (sd(finaldata$SupportForAction.Pre))]
num_outliersSUP.Pre <- length(outliersSUP.Pre)
cat("Number of subjects who are more than 3 standard deviations away from the mean:", num_outliersSUP.Pre, "\n")
cat("Subjects and their values:", outliersSUP.Pre, "\n")

outliersSUP.Post <- finaldata$SupportForAction.Post[abs(finaldata$SupportForAction.Post - (mean(finaldata$SupportForAction.Post))) > 3 * (sd(finaldata$SupportForAction.Post))]
num_outliersSUP.Post <- length(outliersSUP.Post)
cat("Number of subjects who are more than 3 standard deviations away from the mean:", num_outliersSUP.Post, "\n")
cat("Subjects and their values:", outliersSUP.Post, "\n")

# Creating a variable that shows whether participants show outlier values on one of the main variables
finaldata$RGBMOutlier <- ifelse(
  abs(finaldata$BeliefInCC.Pre - mean(finaldata$BeliefInCC.Pre)) > 3 * sd(finaldata$BeliefInCC.Pre) |
    abs(finaldata$BeliefInCC.Post - mean(finaldata$BeliefInCC.Post)) > 3 * sd(finaldata$BeliefInCC.Post) |
    abs(finaldata$Worry.Pre - mean(finaldata$Worry.Pre)) > 3 * sd(finaldata$Worry.Pre) |
    abs(finaldata$Worry.Post - mean(finaldata$Worry.Post)) > 3 * sd(finaldata$Worry.Post) |
    abs(finaldata$HumanCausation.Pre - mean(finaldata$HumanCausation.Pre)) > 3 * sd(finaldata$HumanCausation.Pre) |
    abs(finaldata$HumanCausation.Post - mean(finaldata$HumanCausation.Post)) > 3 * sd(finaldata$HumanCausation.Post) |
    abs(finaldata$PSC.Pre - mean(finaldata$PSC.Pre)) > 3 * sd(finaldata$PSC.Pre) |
    abs(finaldata$PSC.Post - mean(finaldata$PSC.Post)) > 3 * sd(finaldata$PSC.Post) |
    abs(finaldata$SupportForAction.Pre - mean(finaldata$SupportForAction.Pre)) > 3 * sd(finaldata$SupportForAction.Pre)|
    abs(finaldata$SupportForAction.Post - mean(finaldata$SupportForAction.Post)) > 3 * sd(finaldata$SupportForAction.Post),
  1,
  0
)

freq_table(finaldata$RGBMOutlier)

# Effect of Exclusion of Outliers on Normal Distribution
finaldata_wo_RGBMoutliers<- finaldata[finaldata$RGBMOutlier == 0, ]

shapiro.test(finaldata_wo_RGBMoutliers$PSC.Pre)
skew(finaldata_wo_RGBMoutliers$PSC.Pre)
kurtosi(finaldata_wo_RGBMoutliers$PSC.Pre)

shapiro.test(finaldata_wo_RGBMoutliers$PSC.Post)
skew(finaldata_wo_RGBMoutliers$PSC.Post)
kurtosi(finaldata_wo_RGBMoutliers$PSC.Post)

shapiro.test(finaldata_wo_RGBMoutliers$BeliefInCC.Pre)
skew(finaldata_wo_RGBMoutliers$BeliefInCC.Pre)
kurtosi(finaldata_wo_RGBMoutliers$BeliefInCC.Pre)

shapiro.test(finaldata_wo_RGBMoutliers$BeliefInCC.Post)
skew(finaldata_wo_RGBMoutliers$BeliefInCC.Post)
kurtosi(finaldata_wo_RGBMoutliers$BeliefInCC.Post)

shapiro.test(finaldata_wo_RGBMoutliers$Worry.Pre)
skew(finaldata_wo_RGBMoutliers$Worry.Pre)
kurtosi(finaldata_wo_RGBMoutliers$Worry.Pre)

shapiro.test(finaldata_wo_RGBMoutliers$Worry.Post)
skew(finaldata_wo_RGBMoutliers$Worry.Post)
kurtosi(finaldata_wo_RGBMoutliers$Worry.Post)

shapiro.test(finaldata_wo_RGBMoutliers$HumanCausation.Pre)
skew(finaldata_wo_RGBMoutliers$HumanCausation.Pre)
kurtosi(finaldata_wo_RGBMoutliers$HumanCausation.Pre)

shapiro.test(finaldata_wo_RGBMoutliers$HumanCausation.Post)
skew(finaldata_wo_RGBMoutliers$HumanCausation.Post)
kurtosi(finaldata_wo_RGBMoutliers$HumanCausation.Post)

shapiro.test(finaldata_wo_RGBMoutliers$SupportForAction.Pre)
skew(finaldata_wo_RGBMoutliers$SupportForAction.Pre)
kurtosi(finaldata_wo_RGBMoutliers$SupportForAction.Pre)

shapiro.test(finaldata_wo_RGBMoutliers$SupportForAction.Post)
skew(finaldata_wo_RGBMoutliers$SupportForAction.Post)
kurtosi(finaldata_wo_RGBMoutliers$SupportForAction.Post)

#### Main Analyses 
###Hypothesis 3 - t-Test 
# t Test - sign,d=0.26
t.test(finaldata$PSC.Pre, finaldata$PSC.Post, paired = TRUE)
cohensD(finaldata$PSC.Pre, y= finaldata$PSC.Post, method = 'unequal')
mean(finaldata$PSC.Post)-mean(finaldata$PSC.Pre)
mean(finaldata$PSC.Post)
mean(finaldata$PSC.Pre)

## Confidence level for sample means 
# Pre
sample_data <- finaldata$PSC.Pre
confidence_level <- 0.95
sample_mean <- mean(sample_data)
standard_error <- sd(sample_data) / sqrt(length(sample_data))
df <- length(sample_data) - 1
t_value <- qt((1 + confidence_level) / 2, df)
margin_of_error <- t_value * standard_error
lower_bound <- sample_mean - margin_of_error
upper_bound <- sample_mean + margin_of_error

cat("Sample Mean:", sample_mean, "\n")
cat("Confidence Interval:", lower_bound, "to", upper_bound)

# Post
sample_data2 <- finaldata$PSC.Post
confidence_level2 <- 0.95
sample_mean2 <- mean(sample_data2)
standard_error2 <- sd(sample_data2) / sqrt(length(sample_data2))
df2 <- length(sample_data2) - 1
t_value2 <- qt((1 + confidence_level2) / 2, df2)
margin_of_error2 <- t_value2 * standard_error2
lower_bound2 <- sample_mean2 - margin_of_error2
upper_bound2 <- sample_mean2 + margin_of_error2

cat("Sample Mean:", sample_mean2, "\n")
cat("Confidence Interval:", lower_bound2, "to", upper_bound2)

## non parametric test - Wilcoxon Rank Sum Test - sign. 
wilcox.test(finaldata$PSC.Pre, finaldata$PSC.Post, paired = TRUE)

### Hypothesis 4 - Path Analysis 
# Transforming dataset to longdataset with Group 0=Pretest, 1=Posttest
# Adding a Group Variable for the dataset
finaldata$LongGroup <- 0
# Transform into new dataset with doubled rows
newlongdata <- rbind(finaldata, finaldata)
# Identify the indices of the newly added rows
new_rows <- (nrow(finaldata) + 1):nrow(newlongdata)
# Update the "Group" variable to 1 for the newly added rows
newlongdata$LongGroup[new_rows] <- 1
# Sort after ID 
newlongdata <- newlongdata[order(newlongdata$ID),]

# Transform new variables for the main variables of the RGBM that contain Pre and Post
newlongdata$PSC.all <- ifelse(newlongdata$LongGroup == 0, newlongdata$PSC.Pre, newlongdata$PSC.Post)
newlongdata$BeliefInCC.all <- ifelse(newlongdata$LongGroup == 0, newlongdata$BeliefInCC.Pre, newlongdata$BeliefInCC.Post)
newlongdata$HumanCausation.all <- ifelse(newlongdata$LongGroup == 0, newlongdata$HumanCausation.Pre, newlongdata$HumanCausation.Post)
newlongdata$Worry.all <- ifelse(newlongdata$LongGroup == 0, newlongdata$Worry.Pre, newlongdata$Worry.Post)
newlongdata$SupportForAction.all <- ifelse(newlongdata$LongGroup == 0, newlongdata$SupportForAction.Pre, newlongdata$SupportForAction.Post)

library(semTools)
library(psych)
library(xtable)
library(apaTables)
library(stargazer)
library(semPlot)
library(lavaan)


# Reverse GBM Model 2 (Within Subjects)
dataRGBM2 <- newlongdata
dataRGBM2$HC <- ifelse(dataRGBM2$LongGroup == 1, dataRGBM2$HumanCausation.Post, dataRGBM2$HumanCausation.Pre)
dataRGBM2$PSC <- ifelse(dataRGBM2$LongGroup == 1, dataRGBM2$PSC.Post, dataRGBM2$PSC.Pre)
dataRGBM2$WOR <- ifelse(dataRGBM2$LongGroup == 1, dataRGBM2$Worry.Post, dataRGBM2$Worry.Pre)
dataRGBM2$BEL <- ifelse(dataRGBM2$LongGroup == 1, dataRGBM2$BeliefInCC.Post, dataRGBM2$BeliefInCC.Pre)
dataRGBM2$SUP <- ifelse(dataRGBM2$LongGroup == 1, dataRGBM2$SupportForAction.all, dataRGBM2$SupportForAction.Pre)
dataRGBM2$mis <- dataRGBM2$LongGroup
rgbm.model2 <- '
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
rgbm.fit2 <- sem(model=rgbm.model2,data=dataRGBM2, test = 'Satorra-Bentler')
semPaths(rgbm.fit2, layout = "tree2", edge.label.cex = .8, rotation = 2)
summary(rgbm.fit2, standardized = TRUE, rsquare = TRUE, fit.measures = TRUE)
parameterEstimates(rgbm.fit2, standardized = TRUE)
standardizedSolution(rgbm.fit2)

#export table as excel
library(openxlsx)
table_data <- standardizedSolution(rgbm.fit2)
table_data <- round(table_data, digits = 3)
excel_file <- "table_output2.xlsx"
write.xlsx(table_data, excel_file, rowNames = FALSE)
getwd()


# Model Plotting
rgbm2_chi <- as.character(sprintf("%.2f",round(fitMeasures(rgbm.fit2, c("chisq.scaled", "pvalue.scaled", "CFI.robust", "TLI.robust", 
                                                                        "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
                                                                        "srmr", "wrmr"))[1], digits = 2)))
rgbm2_p <- as.character(sprintf("%.2f",round(fitMeasures(rgbm.fit2, c("chisq.scaled", "pvalue.scaled", "CFI.robust", "TLI.robust", 
                                                                      "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
                                                                      "srmr", "wrmr"))[2], digits = 3)))
rgbm2_CFI <- as.character(sprintf("%.2f",round(fitMeasures(rgbm.fit2, c("chisq.scaled", "pvalue.scaled", "CFI.robust", "TLI.robust", 
                                                                        "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
                                                                        "srmr", "wrmr"))[3], digits = 2)))
rgbm2_TLI <- as.character(sprintf("%.2f",round(fitMeasures(rgbm.fit2, c("chisq.scaled", "pvalue.scaled", "CFI.robust", "TLI.robust", 
                                                                        "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
                                                                        "srmr", "wrmr"))[4], digits = 2)))
rgbm2_RMSEA <- as.character(sprintf("%.2f",round(fitMeasures(rgbm.fit2, c("chisq.scaled", "pvalue.scaled", "CFI.robust", "TLI.robust", 
                                                                          "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
                                                                          "srmr", "wrmr"))[5], digits = 3)))
rgbm2_RMSEA_LL <- as.character(sprintf("%.2f",round(fitMeasures(rgbm.fit2, c("chisq.scaled", "pvalue.scaled", "CFI.robust", "TLI.robust", 
                                                                             "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
                                                                             "srmr", "wrmr"))[6], digits = 3)))
rgbm2_RMSEA_UL <- as.character(sprintf("%.2f",round(fitMeasures(rgbm.fit2, c("chisq.scaled", "pvalue.scaled", "CFI.robust", "TLI.robust", 
                                                                             "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
                                                                             "srmr", "wrmr"))[7], digits = 3)))
rgbm2_SRMR <- as.character(sprintf("%.2f",round(fitMeasures(rgbm.fit2, c("chisq.scaled", "pvalue.scaled", "CFI.robust", "TLI.robust", 
                                                                         "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
                                                                         "srmr", "wrmr"))[8], digits = 3)))

rgbm2_plot <- function()
{semPaths(rgbm.fit2, layout = "tree2", what = "std", whatLabels = "std", 
          edge.label.cex = .65, rotation = 2)
  legend(x="topleft", y=1, 
         legend = c("",
                    expression(paste(bold("Model Fit"))),
                    "",
                    bquote(chi^2 ~ "=" ~ .(rgbm2_chi) * "," ~ italic(p) ~ "=" ~ .(rgbm2_p)),
                    bquote(italic(CFI) ~ "=" ~ .(rgbm2_CFI)),
                    bquote(italic(TLI) ~ "=" ~ .(rgbm2_TLI)),
                    bquote(italic(RMSEA) ~ "=" ~ .(rgbm2_RMSEA)),
                    bquote("(90%" ~ italic(CI) ~ "[" * .(rgbm2_RMSEA_LL) * "," ~ .(rgbm2_RMSEA_UL) * "])"),
                    bquote(italic(SRMR) ~ "=" ~ .(rgbm2_SRMR))),
         bty = "n", 
         cex = 0.6)}

rgbm2_plot()

#Normality of RGBM
datask2 <- subset(dataRGBM2[, c("HC", "WOR", "mis", "BEL", "SUP", "PSC")])
mardiaKurtosis(datask2, use = "everything")
mardiaSkew(datask2, use = "everything")


####Appendix
### Sensitivity Analysis for Group 
ControlT1 <- subset(finaldata, Condition=="Control_T1")
ControlT2 <- subset(finaldata, Condition=="Control_T2")
ControlT3 <- subset(finaldata, Condition=="Control_T3")

mean(ControlT1$PSC.Pre)
mean(ControlT1$PSC.Post)

mean(ControlT2$PSC.Pre)
mean(ControlT2$PSC.Post)

mean(ControlT3$PSC.Pre)
mean(ControlT3$PSC.Post)

SensANOVA.Pre <- aov(PSC.Pre ~ Condition, data = finaldata,)
summary(SensANOVA.Pre)
eta_squared(SensANOVA.Pre)

SensANOVA.Post <- aov(PSC.Post ~ Condition, data = finaldata,)
summary(SensANOVA.Post)
eta_squared(SensANOVA.Post)

# H3 for Difference in Pre, Post for separate Control Groups
wilcox.test(ControlT1$PSC.Pre, ControlT1$PSC.Post, paired = TRUE)
wilcox.test(ControlT2$PSC.Pre, ControlT2$PSC.Post, paired = TRUE)
wilcox.test(ControlT3$PSC.Pre, ControlT3$PSC.Post, paired = TRUE)

