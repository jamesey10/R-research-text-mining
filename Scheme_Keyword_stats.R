#libraries ----
library(tm)
library(vegan)
library(tcltk)
library(BiodiversityR) #for rank abundance curves
library(ggplot2)
library(plyr)
library(reshape)
library(reshape2)
library(dplyr)
library(purrr)
library(tidyr)
library(car)
library(texreg)
library(onewaytests)
library(stargazer)
library(lmtest)
library(tidyverse)

# raw data import ----
Import_Analysis_Data <- read.csv("TestData/SDmaster.csv")
Import_Analysis_Data$text <- as.factor(Import_Analysis_Data$text)
Import_Analysis_Data$document <- as.factor(Import_Analysis_Data$document)
Import_Analysis_Data$stages <- as.factor(Import_Analysis_Data$stages)
head(Import_Analysis_Data)
Import_Total_Words <- read.csv("TestData/TWmaster.csv")
head(Import_Total_Words)
# Total Word Count =============================
Total_Count <- ddply(Import_Total_Words, c("document"), summarise,
                     keywords    = sum(wordCount))
head(Total_Count)
# Stage Word Count =============================
Stage_Count <- ddply(Import_Analysis_Data, c("document", "stages"), summarise,
                     keywords    = sum(wordCount))
head(Stage_Count)

#find the possible NAs and add a 0 row 
#stage 5 - UNGC, SA8000, OECD, GreenC
Stage_Count <- Stage_Count %>% add_row(document = "SA8000.csv", stages = "STAGE 5" , keywords=0)
Stage_Count <- Stage_Count %>% add_row(document = "UNGC_merged.csv", stages = "STAGE 5" , keywords=0)
#Stage_Count <- Stage_Count %>% add_row(document = "GreenC", stages = "STAGE 5" , keywords=0)
Stage_Count <- Stage_Count %>% add_row(document = "OECD.csv", stages = "STAGE 5" , keywords=0)

# Merge the two data frames (total and stage count) to calculate the percentage ====
Stats_Data <- merge(Stage_Count, Total_Count, by = "document")
names(Stats_Data) <- c("document", "stages", "keywordCount", "totalCount")
Stats_Data$document <- as.factor(Stats_Data$document)
Stats_Data$stages <- as.factor(Stats_Data$stages)
str(Stats_Data)
head(Stats_Data)

# Calculate the percentage of keywords per stage in the entire document ====
Stats_Data$percentKeywords <- (Stats_Data$keywordCount / Stats_Data$totalCount) * 100
head(Stats_Data)

#classify stages as weak strong ====
Stats_Data$strength <- ifelse(Stats_Data$stages %in% c("STAGE 1", "STAGE 2", "STAGE 3"), "weak", "strong")
#rename docs
Stats_Data$document <- gsub("AA1000AP2018.csv",                         "AA1000", Stats_Data$document)
Stats_Data$document <- gsub("CircularEconomyMerged.csv",                "CircularEcon", Stats_Data$document)
Stats_Data$document <- gsub("Integrated-Thinking-Principles.csv",       "ITP", Stats_Data$document)
Stats_Data$document <- gsub("IIRF.csv",                                 "<IR>", Stats_Data$document)
Stats_Data$document <- gsub("NATcapital.csv",                           "NatCap", Stats_Data$document)
Stats_Data$document <- gsub("TNS_Canada.csv",                           "TNS", Stats_Data$document)
Stats_Data$document <- gsub("UNGC_merged.csv",                          "UNGC", Stats_Data$document)

Stats_Data$document <- gsub("CDP_merge.csv",                            "CDP", Stats_Data$document)
Stats_Data$document <- gsub("CDSB.csv",                                 "CDSB", Stats_Data$document)
Stats_Data$document <- gsub("doughnuteconomics-Raworth-2012.csv",       "Doughnut", Stats_Data$document)
Stats_Data$document <- gsub("GRI_merged.csv",                           "GRI", Stats_Data$document)
Stats_Data$document <- gsub("MIF.csv",                                  "MIF", Stats_Data$document)
Stats_Data$document <- gsub("planetaryboundaries-CranstonSteffen.csv",  "PlanetBound", Stats_Data$document)
Stats_Data$document <- gsub("SASB-merge.csv",                           "SASB", Stats_Data$document)
Stats_Data$document <- gsub("SDG.csv",                                  "SDG", Stats_Data$document)
Stats_Data$document <- gsub("TCFD.csv",                                 "TCFD", Stats_Data$document)

Stats_Data$document <- gsub("BSI8001.csv",                              "BS8001", Stats_Data$document)
Stats_Data$document <- gsub("ISO26000.csv",                             "ISO26000", Stats_Data$document)
Stats_Data$document <- gsub("OECD.csv",                                 "OECD", Stats_Data$document)
Stats_Data$document <- gsub("SDG_Compass.csv",                          "SDG_Compass", Stats_Data$document)

Stats_Data$document <- gsub("bcorp_merge.csv",                          "BCorp", Stats_Data$document)
Stats_Data$document <- gsub("EMAS2017.csv",                             "EMAS", Stats_Data$document)
Stats_Data$document <- gsub("SA8000.csv",                               "SA8000", Stats_Data$document)
Stats_Data$document <- gsub("UL3600.csv",                               "UL3600", Stats_Data$document)


## classify Documents  by PFGS 
Stats_Data$type <- Stats_Data$document
Stats_Data$type <- gsub("AA1000",       "Principle", Stats_Data$type)
Stats_Data$type <- gsub("CircularEcon", "Principle", Stats_Data$type)
Stats_Data$type <- gsub("ITP",          "Principle", Stats_Data$type)
Stats_Data$type <- gsub("IIRF",         "Principle", Stats_Data$type)
Stats_Data$type <- gsub("NATcapital",   "Principle", Stats_Data$type)
Stats_Data$type <- gsub("TNS",          "Principle", Stats_Data$type)
Stats_Data$type <- gsub("UNGC",         "Principle", Stats_Data$type)

Stats_Data$type <- gsub("CDP",             "Framework", Stats_Data$type)
Stats_Data$type <- gsub("CDSB",            "Framework", Stats_Data$type)
Stats_Data$type <- gsub("doughnut",        "Framework", Stats_Data$type)
Stats_Data$type <- gsub("GRI",             "Framework", Stats_Data$type)
#$type <- gsub("MIF",             "Framework", Stats_Data$type)
Stats_Data$type <- gsub("PlanetBound",     "Framework", Stats_Data$type)
Stats_Data$type <- gsub("SASB",            "Framework", Stats_Data$type)
Stats_Data$type <- gsub("SDG",             "Framework", Stats_Data$type)
Stats_Data$type <- gsub("TCFD",            "Framework", Stats_Data$type)

Stats_Data$type <- gsub("BSI8001",            "Guidelines", Stats_Data$type)
Stats_Data$type <- gsub("ISO26000",           "Guidelines", Stats_Data$type)
Stats_Data$type <- gsub("OECD",               "Guidelines", Stats_Data$type)
Stats_Data$type <- gsub("SDG_Compass",        "Guidelines", Stats_Data$type)
Stats_Data$type <- gsub("Framework_Compass",  "Guidelines", Stats_Data$type)

Stats_Data$type <- gsub("BCorp",          "Standards", Stats_Data$type)
Stats_Data$type <- gsub("EMAS",           "Standards", Stats_Data$type)
Stats_Data$type <- gsub("SA8000",         "Standards", Stats_Data$type)
Stats_Data$type <- gsub("UL3600",         "Standards", Stats_Data$type)

Stats_Data <- Stats_Data[-c(106, 107, 108, 109, 110), ]#remove UL3600

# Format wide dataframe ====
Stats_Data_wide <- reshape(Stats_Data, idvar = "document", timevar = "stages", direction = "wide")
Stats_Data_wide[is.na(Stats_Data_wide)] = 0
#back to long form, with no NAs 
#data_long <- melt(Stats_Data_wide, id.vars=c("document", "stages"))

# Remove duplicated wide format columns and rename total column
write.csv(Stats_Data_wide, "masterdata.csv")
master_data <- read.csv("masterdata.csv")
master_data <- select(master_data, - c("totalCount.STAGE.1", "totalCount.STAGE.2", "totalCount.STAGE.3", "totalCount.STAGE.4"))
names(master_data)[names(master_data) == "totalCount.STAGE.5"] <- "Total_Word_Count"

#Group weak (1,2,3) and strong (4,5)
master_data$weak <- (master_data$percentKeywords.STAGE.1+
                     master_data$percentKeywords.STAGE.2+
                     master_data$percentKeywords.STAGE.3)
master_data$strong <- (master_data$percentKeywords.STAGE.4+
                       master_data$percentKeywords.STAGE.5)


# Raw Data Assumptions - Normality ----
# if p < 0.05, not normal
# Stage 1
qqnorm(master_data$percentKeywords.STAGE.1, pch = 1, frame = FALSE) 
qqline(master_data$percentKeywords.STAGE.1, col = "steelblue", lwd = 2)
shapiro.test(master_data$percentKeywords.STAGE.1)  # not normal p < 0.05 
#transformation
summary(powerTransform(master_data$percentKeywords.STAGE.1)) 
master_data$Percent_S1_tf <- (log10(master_data$percentKeywords.STAGE.1+0.001))
qqnorm(master_data$Percent_S1_tf, pch = 1, frame = FALSE) 
qqline(master_data$Percent_S1_tf, col = "steelblue", lwd = 2)
shapiro.test(master_data$Percent_S1_tf) # p > 0.05 , 
# no transform p-value = 0.01988
#log10         p-value = 0.551
#sqrt          p-value = 0.6959

# Stage 2----
qqnorm(master_data$percentKeywords.STAGE.2, pch = 1, frame = FALSE) 
qqline(master_data$percentKeywords.STAGE.2, col = "steelblue", lwd = 2)
shapiro.test(master_data$percentKeywords.STAGE.2)  # not normal p < 0.05
#transformation
summary(powerTransform(master_data$percentKeywords.STAGE.2)) 
master_data$Percent_S2_tf <- (log10(master_data$percentKeywords.STAGE.2+0.001))
qqnorm(master_data$Percent_S2_tf, pch = 1, frame = FALSE) #not bowed
qqline(master_data$Percent_S2_tf, col = "steelblue", lwd = 2)
shapiro.test(master_data$Percent_S2_tf)  
# no transform p-value = 0.007122
#log10         p-value = 0.9302
#sqrt          p-value = 0.6588

# Stage 3----
qqnorm(master_data$percentKeywords.STAGE.3, pch = 1, frame = FALSE) 
qqline(master_data$percentKeywords.STAGE.3, col = "steelblue", lwd = 2)
shapiro.test(master_data$percentKeywords.STAGE.3)  # not normal p < 0.05
#transformation
summary(powerTransform(master_data$percentKeywords.STAGE.3)) 
master_data$Percent_S3_tf <- (log10(master_data$percentKeywords.STAGE.3+0.001))
qqnorm(master_data$Percent_S3_tf, pch = 1, frame = FALSE) 
qqline(master_data$Percent_S3_tf, col = "steelblue", lwd = 2)
shapiro.test(master_data$Percent_S3_tf)  
# no transform p-value = 0.001166
#log10         p-value = 0.6278
#sqrt          p-value = 0.05568

# Stage 4----
qqnorm(master_data$percentKeywords.STAGE.4, pch = 1, frame = FALSE) 
qqline(master_data$percentKeywords.STAGE.4, col = "steelblue", lwd = 2)
shapiro.test(master_data$percentKeywords.STAGE.4)  # not normal p < 0.05
#transformation
summary(powerTransform(master_data$percentKeywords.STAGE.4)) 
master_data$Percent_S4_tf <- (log10(master_data$percentKeywords.STAGE.4+0.001))
qqnorm(master_data$Percent_S4_tf, pch = 1, frame = FALSE)
qqline(master_data$Percent_S4_tf, col = "steelblue", lwd = 2)
shapiro.test(master_data$Percent_S4_tf) 
# no transform p-value = 0.0001494
# log10        p-value = 0.7377
# sqrt         p-value = 0.1339 

# Stage 5 ----
qqnorm(master_data$percentKeywords.STAGE.5, pch = 1, frame = FALSE) 
qqline(master_data$percentKeywords.STAGE.5, col = "steelblue", lwd = 2)
shapiro.test(master_data$percentKeywords.STAGE.5)  # not normal p < 0.05
#transformation
summary(powerTransform(master_data$percentKeywords.STAGE.5)) 
master_data$Percent_S5_tf <- (log10(master_data$percentKeywords.STAGE.5+0.001))
qqnorm(master_data$Percent_S5_tf, pch = 1, frame = FALSE) 
qqline(master_data$Percent_S5_tf, col = "steelblue", lwd = 2)
shapiro.test(master_data$Percent_S5_tf)  
# no transform p-value = 4.67e-07
# log10        p-value = 0.03754
# sqrt         p-value = 0.002253


# weak----
qqnorm(master_data$weak, pch = 1, frame = FALSE) 
qqline(master_data$weak, col = "steelblue", lwd = 2)
shapiro.test(master_data$weak)  # not normal p < 0.05
#transformation
summary(powerTransform(master_data$weak)) 
master_data$weak_tf <- (log10(master_data$weak+0.001))
qqnorm(master_data$weak_tf, pch = 1, frame = FALSE) 
qqline(master_data$weak_tf, col = "steelblue", lwd = 2)
shapiro.test(master_data$weak_tf)  
# no transform p-value = 0.001124
# log10        p-value = 0.4564
# sqrt         p-value = 0.03313

# strong
qqnorm(master_data$strong, pch = 1, frame = FALSE) 
qqline(master_data$strong, col = "steelblue", lwd = 2)
shapiro.test(master_data$strong)  # not normal p < 0.05
#transformation
summary(powerTransform(master_data$strong)) 
master_data$strong_tf <- (log10(master_data$strong+0.001))
qqnorm(master_data$strong_tf, pch = 1, frame = FALSE) 
qqline(master_data$strong_tf, col = "steelblue", lwd = 2)
shapiro.test(master_data$strong_tf)  
# no transform p-value = 4.087e-05
# log10        p-value = 0.4717
# sqrt         p-value = 0.02041

# tests 
# shapiro test not normal p < 0.05
# shapiro.test(Stats_Data$percentKeywords) #p-value =  4.648e-11
# shapiro.test(sqrt(Stats_Data$percentKeywords)) #p-value = 0.01382
# shapiro.test(log10(Stats_Data$percentKeywords+0.0001)) #p-value = 1.118e-12

boxplot(sqrt(percentKeywords) ~  stages, data = Stats_Data)
qqnorm(sqrt(Stats_Data$percentKeywords))
qqline(sqrt(Stats_Data$percentKeywords))

A1 <- aov(sqrt(percentKeywords) ~  stages, data = Stats_Data)
plot(A1)
summary(A1)
TukeyHSD(A1)

# Variance Homogeneity test -
A1$residuals
qqnorm(A1$residuals)
qqline(A1$residuals)
bartlett.test(A1$residuals, Stats_Data$stages)

# Residuals are independent
# Ho  p < 0.05: Errors are serially Uncorrelated
# Ha  p > 0.05: Errors are serially correlated
dwtest(A1)

# GRAPHS of Means =================================
graphSummary <- ddply(Stats_Data, c("stages"), summarise,
                      N    = sum(!is.na(percentKeywords)),
                      mean = mean(percentKeywords, na.rm=TRUE),
                      sd   = sd(percentKeywords, na.rm=TRUE),
                      se   = sd / sqrt(N))
graphSummary

# Label Automation 
ylabel <- "Percent Keyword (Total Keyword / Total Word Count)"
xlabel <- "Stage of Sustainability Continuum"

graphBar <- ggplot(data = graphSummary, aes(x = stages, y = mean))

limits <- aes(ymax = mean + se, ymin = mean - se)
dodge <- position_dodge(width = 0.6)

graphBar + theme_classic()+
  geom_errorbar(limits, width = 0.5,
                stat ="identity", position = dodge) +
  geom_point(stat = "identity", position = dodge, size = 3) +
  scale_colour_brewer(palette="Set1")+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) +
  labs(x = xlabel, y = ylabel)


# RANK ABUNDANCE CURVES  ====================================
# SDmaster <- read.csv("SDmaster.csv")
rankAbundance <- ddply(Stats_Data, c("document", "stages"), summarise,
                       keywordCount    = sum(percentKeywords))
head(rankAbundance)

RAwide <- reshape(rankAbundance, idvar = "document", timevar = "stages", direction = "wide")
RAwide[is.na(Stats_Data_wide)] = 0
write.csv(RAwide, "RAwide.csv")
# Stage Rank Abundance Graphs =============================

# Replace the Stage Number to visualize the various graphs
#stage1
main <- subset(rankAbundance, stages == "STAGE 1")  
summary(main)
row.names(main) <- main$document
names(main)
main$document <- NULL
main$stages <- NULL
rank <- t(main)
RankAbun <- rankabundance((rank), digits=3)
rankabunplot(RankAbun, scale='abundance', addit=FALSE, srt=55, 
             specnames=c(1,2,3,4,5,6,7,8,
                          9,10,11,12,13,14,15,16,
                          17,18,19,20,21,22),
             xlim=c(min(0),max(25)),
             ylim=c(min(0),max(4.35)))
#stage2
main <- subset(rankAbundance, stages == "STAGE 2")  
summary(main)
row.names(main) <- main$document
names(main)
main$document <- NULL
main$stages <- NULL
rank <- t(main)
RankAbun <- rankabundance((rank), digits=3)
rankabunplot(RankAbun, scale='abundance', addit=FALSE, srt=55, 
             specnames=c(1,2,3,4,5,6,7,8,
                          9,10,11,12,13,14,15,16,
                          17,18,19,20,21,22),
             xlim=c(min(0),max(25)),
             ylim=c(min(0),max(4.35)))

#stage3
main <- subset(rankAbundance, stages == "STAGE 3")  
summary(main)
row.names(main) <- main$document
names(main)
main$document <- NULL
main$stages <- NULL
rank <- t(main)
RankAbun <- rankabundance((rank), digits=3)
rankabunplot(RankAbun, scale='abundance', addit=FALSE, srt=55, 
             specnames=c(1,2,3,4,5,6,7,8,
                         9,10,11,12,13,14,15,16,
                         17,18,19,20,21,22),
             xlim=c(min(0),max(25)),
             ylim=c(min(0),max(4.35)))

#stage4
main <- subset(rankAbundance, stages == "STAGE 4")  
summary(main)
row.names(main) <- main$document
names(main)
main$document <- NULL
main$stages <- NULL
rank <- t(main)
RankAbun <- rankabundance((rank), digits=3)
rankabunplot(RankAbun, scale='abundance', addit=FALSE, srt=55, 
             specnames=c(1,2,3,4,5,6,7,8,
                         9,10,11,12,13,14,15,16,
                         17,18,19,20,21,22),
             xlim=c(min(0),max(25)),
             ylim=c(min(0),max(4.35)))
#stage5
main <- subset(rankAbundance, stages == "STAGE 5")  
summary(main)
row.names(main) <- main$document
names(main)
main$document <- NULL
main$stages <- NULL
rank <- t(main)
RankAbun <- rankabundance((rank), digits=3)
rankabunplot(RankAbun, scale='abundance', addit=FALSE, srt=55, 
             specnames=c(1,2,3,4,5,6,7,8,
                         9,10,11,12,13,14,15,16,
                         17,18,19,20,21,22),
             xlim=c(min(0),max(25)),
             ylim=c(min(0),max(4.35)))

#### ==== weak group vs strong group
boxplot(sqrt(percentKeywords) ~  strength, data = Stats_Data)
qqnorm(sqrt(Stats_Data$percentKeywords))
qqline(sqrt(Stats_Data$percentKeywords))

A2 <- aov(sqrt(percentKeywords) ~  strength, data = Stats_Data)
plot(A2)
summary(A2)
TukeyHSD(A2)

A2$residuals
qqnorm(A2$residuals)
qqline(A2$residuals)
bartlett.test(A2$residuals, Stats_Data$stages)

lm1 <- lm(weak_tf ~ strong_tf, data = master_data)
plot(lm1)
abline(lm1)
summary(lm1)
