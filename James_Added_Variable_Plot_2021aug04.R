# Title: Added Variable Plots for James Dataset
# Author: Dr. Brian Ohsowski
# Date: 8-4-2021

# Libraries ==============
library(ggplot2)
library(car)
library(reshape)
library(gridExtra)
library(tidyverse)
library(tidyr)
library(caret)
library(leaps)
library(ggpubr)
library(rstatix)
library(plyr)

# Functions for Model Selection ===========
# Create Function for Model Information Extraction
# id      =  model id
# object  =  regsubsets object
# data    = data used to fit regsubsets
# outcome = outcome variable

get_model_formula <- function(id, object, outcome){
  # get models data
  models <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}


# Cross-validation Error Output Formula
get_cv_error <- function(model.formula, data){
  set.seed(1)
  train.control <- trainControl(method = "cv", number = 5)
  cv <- train(model.formula, data = data, method = "lm",
              trControl = train.control)
  cv$results$RMSE
}

# Functions to Create ggplot2 partial plots ===========

avPlots(LM1)
# Extract AV Plots data with a function
avPlots.invis <- function(MODEL, ...) {
  
  ff <- tempfile()
  png(filename = ff)
  OUT <- car::avPlots(MODEL, ...)
  dev.off()
  unlink(ff)
  OUT }

ggAVPLOTS  <- function(MODEL, YLAB = NULL) {
  #Extract the information for AV plots
  AVPLOTS <- avPlots.invis(MODEL)
  K       <- length(AVPLOTS)
  
  #Create the added variable plots using ggplot
  GGPLOTS <- vector('list', K)
  for (i in 1:K) {
    DATA         <- data.frame(AVPLOTS[[i]])
    GGPLOTS[[i]] <- ggplot2::ggplot(aes_string(x = colnames(DATA)[1], 
                                               y = colnames(DATA)[2]), 
                                    data = DATA) +
      theme_classic() + 
      geom_point(colour = 'blue') + 
      geom_smooth(method = 'lm', se = FALSE, 
                  color = 'red', formula = y ~ x, linetype = 'dashed') +
      xlab(paste0('Predictor Residual \n (', 
                  names(DATA)[1], ' | others)')) +
      ylab(paste0('Response Residual \n (',
                  ifelse(is.null(YLAB), 
                         paste0(names(DATA)[2], ' | others'), YLAB), ')')) }
  #Return output object
  GGPLOTS }


# Data Import =======
businessImport <- read.csv("masterdata.csv")

# Data Check ======
str(businessImport)
summary(businessImport)

# Data Processing and Transformations ===================
names(businessImport)
businessSubset <- businessImport[, c("amongCVF_AD", "amongCVF_CL", "amongCVF_HI",
                                     "amongCVF_MA", "Percent_KW_S1", "Percent_KW_S2",
                                     "Percent_KW_S3", "Percent_KW_S4", "Percent_KW_S5", 
                                     "GRI_weak_perc", "GRI_strg_perc")]

businessSubset2 <- transform(businessSubset, 
                      amongCVF_AD_LOG = log10(amongCVF_AD + 0.001),
                      amongCVF_CL_LOG = log10(amongCVF_CL + 0.001),
                      amongCVF_HI_LOG = log10(amongCVF_HI + 0.001),
                      amongCVF_MA_LOG = log10(amongCVF_MA + 0.001),
                      Percent_KW_S1_SQRT = sqrt(Percent_KW_S1),
                      Percent_KW_S2_SQRT = sqrt(Percent_KW_S2),
                      Percent_KW_S3_SQRT = sqrt(Percent_KW_S3),
                      Percent_KW_S4_SQRT = sqrt(Percent_KW_S4),
                      Percent_KW_S5_SQRT = sqrt(Percent_KW_S5),
                      GRI_weak_perc_SQRT = sqrt(GRI_weak_perc),
                      GRI_strg_perc_SQRT = sqrt(GRI_strg_perc)
                      )

business <- businessSubset2[,c(-1:-11)] # drop junk raw data
str(business) # data check

businessS1 <- business[,c(1:4, 5)]
businessS2 <- business[,c(1:4, 6)]
businessS3 <- business[,c(1:4, 7)]
businessS4 <- business[,c(1:4, 8)]
businessS5 <- business[,c(1:4, 9)]
businessWE <- business[,c(1:4, 10)]
businessST <- business[,c(1:4, 11)]





# Best Model Subset Selection Tools ==================
# https://bit.ly/3xngyWw

# Step 1 --------
# Input Y variable and All X variables 
MODEL1 <- regsubsets(Percent_KW_S1_SQRT ~.^2, data = businessS1, nvmax = 5)
summary(MODEL1) # Outputs Most Parsimonious Models

MODEL2 <- regsubsets(Percent_KW_S2_SQRT ~.^2, data = businessS2, nvmax = 5)
summary(MODEL2) # Outputs Most Parsimonious Models

MODEL3 <- regsubsets(Percent_KW_S3_SQRT ~.^2, data = businessS3, nvmax = 5)
summary(MODEL3) # Outputs Most Parsimonious Models

MODEL4 <- regsubsets(Percent_KW_S4_SQRT ~.^2, data = businessS4, nvmax = 5)
summary(MODEL4) # Outputs Most Parsimonious Models

MODEL5 <- regsubsets(Percent_KW_S5_SQRT ~.^2, data = businessS5, nvmax = 5)
summary(MODEL5) # Outputs Most Parsimonious Models

MODELW <- regsubsets(GRI_weak_perc_SQRT ~.^2, data = businessWE, nvmax = 5)
summary(MODELW) # Outputs Most Parsimonious Models

MODELS <- regsubsets(GRI_strg_perc_SQRT ~.^2, data = businessST, nvmax = 5)
summary(MODELS) # Outputs Most Parsimonious Models


# Step 2 -------
# Investigate several evaluation methods for model selection
res.sum <- summary(MODELW)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),  # Rsquared
  CP = which.min(res.sum$cp),         # Mallows Cp
  BIC = which.min(res.sum$bic)        # BIC Selection
)
res.sum$adjr2
res.sum$bic

# Formula Output 
get_model_formula(5, MODELW, "GRI_weak_perc_SQRT")

# Compute cross-validation error
model.ids <- 1:4
cv.errors <-  map(model.ids, get_model_formula, MODELW, "GRI_weak_perc_SQRT") %>%
  map(get_cv_error, data = businessWE) %>%
  unlist()

cv.errors # Output of Cross-Validation Errors
which.min(cv.errors)
coef(MODELW, 1)
summary(MODELW)

# Best Subset Models Chosen =====================
# Model S1
LM1 <- lm(Percent_KW_S1_SQRT ~ amongCVF_HI_LOG * amongCVF_MA_LOG , data = businessS1)
summary(LM1) # Model: NS (p = 0.1752)
plot(LM1) # Residual Test
plot(Percent_KW_S1_SQRT ~ amongCVF_MA_LOG , data = businessS1)
abline(LM1)
avPlots(LM1)

# Model S2 - there are 2 strong models
LM2 <- lm(Percent_KW_S2_SQRT ~ amongCVF_CL_LOG, data = businessS2)
summary(LM2) # Model: p = 0.001006
plot(LM2)
avPlots(LM2)
plot(Percent_KW_S2_SQRT ~ amongCVF_CL_LOG, data = businessS2)
abline(LM2)


LM2a <- lm(Percent_KW_S2_SQRT ~ amongCVF_MA_LOG + amongCVF_AD_LOG*amongCVF_HI_LOG, data = businessS2)
summary(LM2a) # Model: p = 0.002621 MA is the driver
plot(LM2a)
avPlots(LM2a)
plot(Percent_KW_S2_SQRT ~ amongCVF_MA_LOG + amongCVF_AD_LOG*amongCVF_HI_LOG, data = businessS2)
abline(LM2a)

# Model S3 - no significant models found
LM3 <- lm(Percent_KW_S3_SQRT ~ amongCVF_MA_LOG, data = businessS3)
summary(LM3) # Model: NS
plot(LM3)
plot(Percent_KW_S3_SQRT ~ amongCVF_MA_LOG , data = businessS3)
abline(LM3)

# Model S4
LM4 <- lm(Percent_KW_S4_SQRT ~ amongCVF_HI_LOG , data = businessS4)
summary(LM4) # Model: p = 0.005704 mod 1
plot(LM4)
avPlots(LM4)
plot(Percent_KW_S4_SQRT ~ amongCVF_HI_LOG , data = businessS4)
abline(LM4)

# Model S5 - no significant models
LM5 <- lm(Percent_KW_S5_SQRT ~ amongCVF_MA_LOG  , data = businessS5)
summary(LM5) # Model: NS
plot(LM5) 
plot(Percent_KW_S5_SQRT ~ amongCVF_MA_LOG, data = businessS5)
abline(LM5)

#Model Weak
LMW <- lm(GRI_weak_perc_SQRT ~ amongCVF_CL_LOG , data = businessWE)
summary(LMW) # Model: 0.003164 mod 1 driven by CL, negative slope
plot(LMW)
avPlots(LMW)
plot(GRI_weak_perc_SQRT ~ amongCVF_CL_LOG , data = businessWE)
abline(LMW)

LMWa <- lm(GRI_weak_perc_SQRT ~ amongCVF_MA_LOG + amongCVF_AD_LOG*amongCVF_HI_LOG , data = businessWE)
summary(LMWa) # Model: 0.002308 mod 2 - driven by MA
plot(LMWa) 
avPlots(LMWa)
plot(GRI_weak_perc_SQRT ~ amongCVF_MA_LOG + amongCVF_AD_LOG*amongCVF_HI_LOG, data = businessWE)
abline(LMWa)

#LMWb <- lm(GRI_weak_perc_SQRT ~  amongCVF_HI_LOG + amongCVF_AD_LOG*amongCVF_MA_LOG, data = businessWE)
#summary(LMWb) # Model: 0.0008293 mod 3 driven by HI
#avPlots(LMWb)

#LMWc <- lm(GRI_weak_perc_SQRT ~ amongCVF_HI_LOG + amongCVF_AD_LOG*amongCVF_HI_LOG + amongCVF_AD_LOG*amongCVF_MA_LOG + amongCVF_CL_LOG*amongCVF_HI_LOG, data = businessWE)
#summary(LMWc) # Model: 0.006604 mod 4

#LMWd <- lm(GRI_weak_perc_SQRT ~ amongCVF_HI_LOG + amongCVF_AD_LOG*amongCVF_CL_LOG + amongCVF_AD_LOG*amongCVF_HI_LOG + amongCVF_AD_LOG*amongCVF_MA_LOG + amongCVF_CL_LOG*amongCVF_HI_LOG, data = businessWE)
#summary(LMWd) # Model: 0.01189 mod 5

#Model Strong
LMS <- lm(GRI_strg_perc_SQRT ~ amongCVF_HI_LOG  , data = businessST)
summary(LMS) # Model: 0.004693 mod 1
plot(LMS) 
avPlots(LMS)
plot(GRI_strg_perc_SQRT ~ amongCVF_HI_LOG, data = businessST)
abline(LMS)


# Data Model ===================
LM <- lm(Percent_KW_S1_SQRT ~ amongCVF_AD_LOG + amongCVF_CL_LOG + amongCVF_HI_LOG + amongCVF_MA_LOG, data = business)
summary(LM)
# plot(LM) # Residual Check

# Run the Partial Plot Function (from above) =================
avPlots.invis(LM2)
ggAVPLOTS(LM2)


#Produce matrix of added variable plots
PLOTS <- ggAVPLOTS(LM2a)
K     <- length(PLOTS)
NCOL  <- ceiling(sqrt(K))
AVPLOTS <- do.call("arrangeGrob", c(PLOTS, ncol = NCOL, top = 'Added Variable Plots'))
ggsave('AV Plots - Trucking.jpg', width = 10, height = 10)




## linear plots
graphLine2 <- ggplot(data = businessS2, aes(x = amongCVF_CL_LOG, y = Percent_KW_S2_SQRT))
graphLine2 + 
  theme_classic() +                   
  geom_point(colour = "blue")+
  geom_smooth(method = "lm", se = FALSE)+
  stat_regline_equation(label.y = 1.9, label.x= 1.3, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 1.8, label.x= 1.3, aes(label = ..rr.label..)) 
  #stat_compare_means()  #to show p-values, doesnt work
summary(businessS2)

graphLine2a <- ggplot(data = businessS2, aes(x = amongCVF_MA_LOG + amongCVF_AD_LOG*amongCVF_HI_LOG, y = Percent_KW_S2_SQRT))
graphLine2a + 
  theme_classic() +                   
  geom_point(colour = "blue")+
  geom_smooth(method = "lm", se = FALSE)+
  #xlim(1.0, 3.5)+
  #ylim(0.3, 1.5)+
  stat_regline_equation(label.y = .5, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = .4, aes(label = ..rr.label..)) 

graphLine4 <- ggplot(data = businessS4, aes(x = amongCVF_HI_LOG, y = Percent_KW_S4_SQRT))
graphLine4 + 
  theme_classic() +                   
  geom_point(colour = "blue")+
  geom_smooth(method = "lm", se = FALSE)+
 # xlim(1.0, 3.5)+
  #ylim(0.3, 1.5)+  
  stat_regline_equation(label.y = 1.2, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 1.1, aes(label = ..rr.label..)) 

#weak-strong grouping
graphLineW <- ggplot(data = businessWE, aes(x = amongCVF_CL_LOG, y = GRI_weak_perc_SQRT))
graphLineW + 
  theme_classic() +                   
  geom_point(colour = "blue")+
  geom_smooth(method = "lm", se = FALSE)+
  stat_regline_equation(label.y = 1.9, label.x= 1.3, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 1.8, label.x= 1.3, aes(label = ..rr.label..))

graphLineWa <- ggplot(data = businessWE, aes(x = amongCVF_MA_LOG + amongCVF_AD_LOG*amongCVF_HI_LOG, y = GRI_weak_perc_SQRT))
graphLineWa + 
  theme_classic() +                   
  geom_point(colour = "blue")+
  geom_smooth(method = "lm", se = FALSE)+
  stat_regline_equation(label.y = 1.9, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 1.8, aes(label = ..rr.label..))

graphLineS <- ggplot(data = businessST, aes(x = amongCVF_HI_LOG, y = GRI_strg_perc_SQRT))
graphLineS + 
  theme_classic() +                   
  geom_point(colour = "blue")+
  geom_smooth(method = "lm", se = FALSE)+
  stat_regline_equation(label.y = 1.9,  aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 1.8,  aes(label = ..rr.label..))


# #bar plots for descriptive stats
# data_long <- gather(businessSubset, stage, value, Percent_KW_S1:Percent_KW_S5, factor_key=TRUE)
# data_long2 <- gather(data_long, CVFtype, value, amongCVF_AD:amongCVF_MA, factor_key=TRUE)
# data_long3 <- data_long2[,c(-1:-2)] 
# data_longAD <- filter(data_long3, CVFtype == "amongCVF_AD")
# data_longCL <- filter(data_long3, CVFtype == "amongCVF_CL")
# data_longHI <- filter(data_long3, CVFtype == "amongCVF_HI")
# data_longMA <- filter(data_long3, CVFtype == "amongCVF_MA")

businessbar <- businessSubset2[,c(1:11)] # drop junk raw data
str(businessbar) # data check
businessbar$id <- rownames(businessbar)

busbarmean <- businessbar[,c(5:9,12)] 
busbar <- melt(busbarmean, id=c("id"))

bargraphstagemeans <- ddply(busbar, c("variable"), summarise,
                  dataRep  = sum(!is.na(value)), #count per category
                  dataMean = mean(value, na.rm=T),  # mean
                  dataSD   = sd(value, na.rm=T),  #stand dev
                  dataSE   = dataSD / sqrt(dataRep))  #standard error
head(bargraphstagemeans)

### Load the graphing data for the x and y axis
stagebargraph <- ggplot(data = bargraphstagemeans, aes(x = variable, y = dataMean))
#### Set error bar limits using SD or SE around the mean
limits <- aes(ymax = dataMean + dataSD, ymin = dataMean - dataSD) 
dodge <- position_dodge(width = 0.9) # Dodge overlapping objects side-to-side
#### Run your graph
stagebargraph + 
  theme_classic() +                   
  geom_bar(stat="identity", position = dodge, colour = "black") +         
  geom_errorbar( limits, width = 0.2, position = dodge) +
  scale_fill_brewer(palette="Greens")# Limits



busbartypes <- businessbar[,c(1:4,12)] 
busbartypes2 <- melt(busbartypes, id=c("id"))

bargraphcvfmeans <- ddply(busbartypes2, c("variable"), summarise,
                  dataRep  = sum(!is.na(value)), #count per category
                  dataMean = mean(value, na.rm=T),  # mean
                  dataSD   = sd(value, na.rm=T),  #stand dev
                  dataSE   = dataSD / sqrt(dataRep))  #standard error
head(bargraphcvfmeans)

cvfbargraph <- ggplot(data = bargraphcvfmeans, aes(x = variable, y = dataMean))
#### Set error bar limits using SD or SE around the mean
limits <- aes(ymax = dataMean + dataSD, ymin = dataMean - dataSD) 
dodge <- position_dodge(width = 0.9) # Dodge overlapping objects side-to-side
#### Run your graph
cvfbargraph + 
  theme_classic() +                   
  geom_bar(stat="identity", position = dodge, colour = "black") +         
  geom_errorbar( limits, width = 0.2, position = dodge) +
  scale_fill_brewer(palette="Greens")# Limits




