# Required Libraries =================
library(tm)
library(tcltk)
library(ggplot2)
library(plyr)
library(reshape)
library(dplyr)
library(purrr)
library(tidyr)
library(car)  # for avplots
library(texreg)
library(stargazer)
library(broom)
library(dotwhisker)
library(nlme)
library(jtools)
library(lmtest)
library(gvlma)
library(summarytools)
library(moonBook)
library(ggiraph)
library(ggiraphExtra)
library(reshape)

#### STATS SECTION =====================================

# Analysis time!
# Assuming you did all the analysis as saved them as a .csv, you can just import the files
# Goal: We need standardized text counts (we don't want to bias our data by doc size)
# So, we must get a total word count for each document and total word count by stage
# GRI
Import_Analysis_Data <- read.csv("TestDataGRI/SDGmaster.csv")
Import_Analysis_Data$text <- as.factor(Import_Analysis_Data$text)
Import_Analysis_Data$document <- as.factor(Import_Analysis_Data$document)
Import_Analysis_Data$stages <- as.factor(Import_Analysis_Data$stages)
head(Import_Analysis_Data)
Import_Total_Words <- read.csv("TestDataGRI/TWDGmaster.csv")
head(Import_Total_Words)
# AR 
Import_Analysis_DataAR <- read.csv("TestDataAR/SDARmaster.csv")
Import_Analysis_DataAR$text <- as.factor(Import_Analysis_DataAR$text)
Import_Analysis_DataAR$document <- as.factor(Import_Analysis_DataAR$document)
Import_Analysis_DataAR$stages <- as.factor(Import_Analysis_DataAR$stages)
head(Import_Analysis_DataAR)
Import_Total_WordsAR <- read.csv("TestDataAR/TWDARmaster.csv")
head(Import_Total_WordsAR)

# Total Word Count =============================
# GRI
Total_Count <- ddply(Import_Total_Words, c("document"), summarise,
                     keywords    = sum(wordCount))
head(Total_Count)
# AR 
Total_CountAR <- ddply(Import_Total_WordsAR, c("document"), summarise,
                       keywords    = sum(wordCount))
head(Total_CountAR)

# Stage/CVF Type Word Count =============================
# GRI
Stage_Count <- ddply(Import_Analysis_Data, c("document", "stages"), summarise,
                     keywords    = sum(wordCount))
head(Stage_Count)
# AR 
Stage_CountAR <- ddply(Import_Analysis_DataAR, c("document", "stages"), summarise,
                       keywords    = sum(wordCount))
head(Stage_CountAR)

# Merge the two data frames (total and stage count) to calculate the percentage ====
# GRI
Stats_Data <- merge(Stage_Count, Total_Count, by = "document")
names(Stats_Data) <- c("document", "stages", "keywordCount", "totalCount")
Stats_Data$document <- as.factor(Stats_Data$document)
Stats_Data$stages <- as.factor(Stats_Data$stages)
str(Stats_Data)
head(Stats_Data)
# AR
Stats_DataAR <- merge(Stage_CountAR, Total_CountAR, by = "document")
names(Stats_DataAR) <- c("document", "type", "keywordCount", "totalCount")
Stats_DataAR$document <- as.factor(Stats_DataAR$document)
Stats_DataAR$stages <- as.factor(Stats_DataAR$type)
str(Stats_DataAR)
head(Stats_DataAR)

# Calculate the percentage of keywords per stage in the entire document ====
# GRI
Stats_Data$percentKeywords <- (Stats_Data$keywordCount / Stats_Data$totalCount) * 100
head(Stats_Data)
# AR
Stats_DataAR$percentKeywords <- (Stats_DataAR$keywordCount / Stats_DataAR$totalCount) * 100
head(Stats_DataAR)

# Format final dataframe====
Stats_Data_wide <- reshape(Stats_Data, idvar = "document", timevar = "stages", direction = "wide")
Stats_Data_wideAR <- reshape(Stats_DataAR, idvar = "document", timevar = "type", direction = "wide")
# Remove duplicated wide format columns and rename total column
# GRI
Stats_Data_wide <- select(Stats_Data_wide, - c("totalCount.STAGE 1", "totalCount.STAGE 2", "totalCount.STAGE 3", "totalCount.STAGE 4"))
names(Stats_Data_wide)[names(Stats_Data_wide) == "totalCount.STAGE 5"] <- "Total_Count"
# AR
Stats_Data_wideAR <- select(Stats_Data_wideAR, - c("totalCount.ADHOCRACY", "totalCount.HIERARCHY", "totalCount.CLAN"))
names(Stats_Data_wideAR)[names(Stats_Data_wideAR) == "totalCount.MARKET"] <- "Total_Count_AR"

# Merge with GRI database MetaData
GRI_meta_data <- read.csv("GRI_meta_data.csv")
master_data <- merge(GRI_meta_data, Stats_Data_wide, by.x="GRI_CSV_Filename", by.y="document")
master_data <- merge(master_data, Stats_Data_wideAR, by.x="AR_CSV_Filename", by.y="document")
# filter the latest observation for each company
master_data <- master_data %>% group_by(ï..Company) %>% slice(which.max(ReportYear))
# Rename Columns to something friendly
names(master_data)[names(master_data) == "keywordCount.STAGE 1"] <- "GRI_KW_Count_S1"
names(master_data)[names(master_data) == "keywordCount.STAGE 2"] <- "GRI_KW_Count_S2"
names(master_data)[names(master_data) == "keywordCount.STAGE 3"] <- "GRI_KW_Count_S3"
names(master_data)[names(master_data) == "keywordCount.STAGE 4"] <- "GRI_KW_Count_S4"
names(master_data)[names(master_data) == "keywordCount.STAGE 5"] <- "GRI_KW_Count_S5"
names(master_data)[names(master_data) == "keywordCount.STAGE 5"] <- "GRI_KW_Count_S5"
names(master_data)[names(master_data) == "percentKeywords.STAGE 1"] <- "Percent_KW_S1"
names(master_data)[names(master_data) == "percentKeywords.STAGE 2"] <- "Percent_KW_S2"
names(master_data)[names(master_data) == "percentKeywords.STAGE 3"] <- "Percent_KW_S3"
names(master_data)[names(master_data) == "percentKeywords.STAGE 4"] <- "Percent_KW_S4"
names(master_data)[names(master_data) == "percentKeywords.STAGE 5"] <- "Percent_KW_S5"
names(master_data)[names(master_data) == "percentKeywords.STAGE 5"] <- "Percent_KW_S5"
names(master_data)[names(master_data) == "Total_Count"]    <- "GRI_doc_wc"
names(master_data)[names(master_data) == "Total_Count_AR"] <- "AR_doc_wc"
names(master_data)[names(master_data) == "keywordCount.ADHOCRACY"] <- "KW_Count_AD"
names(master_data)[names(master_data) == "keywordCount.CLAN"]      <- "KW_Count_CL"
names(master_data)[names(master_data) == "keywordCount.HIERARCHY"] <- "KW_Count_HI"
names(master_data)[names(master_data) == "keywordCount.MARKET"]    <- "KW_Count_MA"
names(master_data)[names(master_data) == "percentKeywords.ADHOCRACY"] <- "Percent_KW_AD"
names(master_data)[names(master_data) == "percentKeywords.CLAN"]      <- "Percent_KW_CL"
names(master_data)[names(master_data) == "percentKeywords.HIERARCHY"] <- "Percent_KW_HI"
names(master_data)[names(master_data) == "percentKeywords.MARKET"]    <- "Percent_KW_MA"

# Look at what we have
str(master_data)
master_data=master_data %>% select(ï..Company,ReportYear,Sector, SectorParent, SPcode, losub,
                                    GRI_doc_wc, GRI_KW_Count_S1,GRI_KW_Count_S2,GRI_KW_Count_S3,GRI_KW_Count_S4,GRI_KW_Count_S5,
                                    Percent_KW_S1, Percent_KW_S2, Percent_KW_S3, Percent_KW_S4, Percent_KW_S5, 
                                    AR_doc_wc,KW_Count_AD,KW_Count_CL,KW_Count_HI, KW_Count_MA, 
                                    Percent_KW_AD, Percent_KW_CL, Percent_KW_HI, Percent_KW_MA
                                    ) %>% as.data.frame()
names(master_data)
stargazer(master_data,title="Descriptive Statistics", type = "text") #change to html for making a paper






#Additional GRI Columns
master_data$GRI_words <- master_data$GRI_KW_Count_S1+master_data$GRI_KW_Count_S2+master_data$GRI_KW_Count_S3+master_data$GRI_KW_Count_S4+master_data$GRI_KW_Count_S5
master_data$GRIWeakCount <- (master_data$GRI_KW_Count_S1+master_data$GRI_KW_Count_S2+master_data$GRI_KW_Count_S3)
master_data$GRIStrgCount <- (master_data$GRI_KW_Count_S4+master_data$GRI_KW_Count_S5)
master_data$GRI_weak_perc <- (master_data$GRIWeakCount / master_data$GRI_doc_wc) * 100
master_data$GRI_strg_perc <- (master_data$GRIStrgCount / master_data$GRI_doc_wc) * 100
master_data$amongGRI_s1 <- (master_data$GRI_KW_Count_S1/master_data$GRI_words)*100
master_data$amongGRI_s2 <- (master_data$GRI_KW_Count_S2/master_data$GRI_words)*100
master_data$amongGRI_s3 <- (master_data$GRI_KW_Count_S3/master_data$GRI_words)*100
master_data$amongGRI_s4 <- (master_data$GRI_KW_Count_S4/master_data$GRI_words)*100
master_data$amongGRI_s5 <- (master_data$GRI_KW_Count_S5/master_data$GRI_words)*100
master_data$amongGRIweak <- (master_data$GRIWeakCount/master_data$GRI_words)*100
master_data$amongGRIstrg <- (master_data$GRIStrgCount/master_data$GRI_words)*100
#Additional CVF Columns
master_data$CVF_words <- master_data$KW_Count_AD+master_data$KW_Count_CL+master_data$KW_Count_HI+master_data$KW_Count_MA
master_data$amongCVF_AD <- (master_data$KW_Count_AD/master_data$CVF_words)*100
master_data$amongCVF_CL <- (master_data$KW_Count_CL/master_data$CVF_words)*100
master_data$amongCVF_HI <- (master_data$KW_Count_HI/master_data$CVF_words)*100
master_data$amongCVF_MA <- (master_data$KW_Count_MA/master_data$CVF_words)*100

write.csv(master_data, "masterdata.csv")

####Some Group stats ====
# by GRI sector
SectorCount <- group_by(master_data, Sector) %>%
  summarise(count = n()) 
SectorCount
# by parent sector
SectorParentCount <- group_by(master_data, SectorParent) %>%
  summarise(count = n()) 
SectorParentCount
# by year
ReportYear <- group_by(master_data, ReportYear) %>%
  summarise(count = n()) 
ReportYear

#### Filter out some outliers, by row number ====
# master_data <- master_data[-c(13), ]
## Filter in the observations with both a certain sector
# master_data <- filter(master_data, master_data$SPcode==8)
# master_data <- filter(master_data, master_data$losub==3)

# Quick Boxplot ====
#boxplot(percentKeywords ~ stages, data = Stats_Data)
#boxplot(percentKeywords ~ type, data = Stats_DataAR)
#GRI raw words normality ====
#Normality among all raw keyword percent numbers

shapiro.test(master_data$Percent_KW_S1)
qqnorm(master_data$Percent_KW_S1, pch = 1, frame = FALSE) #very bowed
qqline(master_data$Percent_KW_S1, col = "steelblue", lwd = 2)
#transformation
summary(powerTransform(master_data$Percent_KW_S1)) #result is .5, suggesting to sqrt (or raise to a half power)
master_data$Percent_KW_S1_tf <- (master_data$Percent_KW_S1^0.5)
qqnorm(master_data$Percent_KW_S1_tf, pch = 1, frame = FALSE) #not bowed
qqline(master_data$Percent_KW_S1_tf, col = "steelblue", lwd = 2)
shapiro.test(master_data$Percent_KW_S1_tf) # p > 0.05 ,  sqrt transformation will be needed

shapiro.test(master_data$Percent_KW_S2)
qqnorm(master_data$Percent_KW_S2, pch = 1, frame = FALSE) #very bowed
qqline(master_data$Percent_KW_S2, col = "steelblue", lwd = 2)
#transformation
summary(powerTransform(master_data$Percent_KW_S2)) #result is .5, suggesting to sqrt (or raise to a half power)
master_data$Percent_KW_S2_tf <- (master_data$Percent_KW_S2^0.5)
qqnorm(master_data$Percent_KW_S2_tf, pch = 1, frame = FALSE) #not bowed
qqline(master_data$Percent_KW_S2_tf, col = "steelblue", lwd = 2)
shapiro.test(master_data$Percent_KW_S2_tf) # p > 0.05 ,  sqrt transformation will be needed

shapiro.test(master_data$Percent_KW_S3)
qqnorm(master_data$Percent_KW_S3, pch = 1, frame = FALSE) #very bowed
qqline(master_data$Percent_KW_S3, col = "steelblue", lwd = 2)
#transformation
summary(powerTransform(master_data$Percent_KW_S3)) #result is .5, suggesting to sqrt (or raise to a half power)
master_data$Percent_KW_S3_tf <- (master_data$Percent_KW_S3^0.5)
qqnorm(master_data$Percent_KW_S3_tf, pch = 1, frame = FALSE) #not bowed
qqline(master_data$Percent_KW_S3_tf, col = "steelblue", lwd = 2)
shapiro.test(master_data$Percent_KW_S3_tf) # p > 0.05 ,  sqrt transformation will be needed

shapiro.test(master_data$Percent_KW_S4)
qqnorm(master_data$Percent_KW_S4, pch = 1, frame = FALSE) #very bowed
qqline(master_data$Percent_KW_S4, col = "steelblue", lwd = 2)
#transformation
summary(powerTransform(master_data$Percent_KW_S4+.0001)) 
master_data$Percent_KW_S4_tf <- (sqrt(master_data$Percent_KW_S4))
qqnorm(master_data$Percent_KW_S4_tf, pch = 1, frame = FALSE) #not bowed
qqline(master_data$Percent_KW_S4_tf, col = "steelblue", lwd = 2)
shapiro.test(master_data$Percent_KW_S4_tf) 
# no transformation     p = 6.842e-14
# sqrt transformation   p = 7.408e-05
# log10 transformation  p = 2.2e-16

shapiro.test(master_data$Percent_KW_S5)
qqnorm(master_data$Percent_KW_S5, pch = 1, frame = FALSE) #very bowed
qqline(master_data$Percent_KW_S5, col = "steelblue", lwd = 2)
#transformation
summary(powerTransform(master_data$Percent_KW_S5+.0001)) 
master_data$Percent_KW_S5_tf <- (sqrt(master_data$Percent_KW_S5))
qqnorm(master_data$Percent_KW_S5_tf, pch = 1, frame = FALSE) #not bowed
qqline(master_data$Percent_KW_S5_tf, col = "steelblue", lwd = 2)
shapiro.test(master_data$Percent_KW_S5_tf) 
# no transformation    p = 2.2e-16
# sqrt transformation  p = 1.529e-06
# log10 transformation p = 4.083e-16

shapiro.test(master_data$amongGRI_s4)
qqnorm(master_data$amongGRI_s4, pch = 1, frame = FALSE) #very bowed
qqline(master_data$amongGRI_s4, col = "steelblue", lwd = 2)
#transformation
summary(powerTransform(master_data$amongGRI_s4+.0001)) 
master_data$amongGRI_s4_tf <- (sqrt(master_data$amongGRI_s4))
qqnorm(master_data$amongGRI_s4_tf, pch = 1, frame = FALSE) #not bowed
qqline(master_data$amongGRI_s4_tf, col = "steelblue", lwd = 2)
shapiro.test(master_data$amongGRI_s4_tf) 
# no transformation    p = 2.2e-16
# sqrt transformation  p = 1.986e-07
# log10 transformation p = 2.2e-16

shapiro.test(master_data$amongGRI_s5)
qqnorm(master_data$amongGRI_s5, pch = 1, frame = FALSE) #very bowed
qqline(master_data$amongGRI_s5, col = "steelblue", lwd = 2)
#transformation
summary(powerTransform(master_data$amongGRI_s5+.0001)) 
master_data$amongGRI_s5_tf <- (sqrt(master_data$amongGRI_s5))
qqnorm(master_data$amongGRI_s5_tf, pch = 1, frame = FALSE) #not bowed
qqline(master_data$amongGRI_s5_tf, col = "steelblue", lwd = 2)
shapiro.test(master_data$amongGRI_s5_tf) 
# no transformation    p = 2.2e-16
# sqrt transformation  p = 3.018e-07
# log10 transformation p = 2.2e-16

shapiro.test(master_data$GRI_weak_perc)
qqnorm(master_data$GRI_weak_perc, pch = 1, frame = FALSE) #very bowed
qqline(master_data$GRI_weak_perc, col = "steelblue", lwd = 2)
#transformation
summary(powerTransform(master_data$GRI_weak_perc)) 
master_data$GRI_weak_perc_tf <- (sqrt(master_data$GRI_weak_perc))
qqnorm(master_data$GRI_weak_perc_tf, pch = 1, frame = FALSE) #not bowed
qqline(master_data$GRI_weak_perc_tf, col = "steelblue", lwd = 2)
shapiro.test(master_data$GRI_weak_perc_tf) 
# no transformation    p = 0.1233
# sqrt transformation  p = 0.3861
# log10 transformation p = 8.739e-05

shapiro.test(master_data$GRI_strg_perc)
qqnorm(master_data$GRI_strg_perc, pch = 1, frame = FALSE) #very bowed
qqline(master_data$GRI_strg_perc, col = "steelblue", lwd = 2)
#transformation
summary(powerTransform(master_data$GRI_strg_perc+.00001)) 
master_data$GRI_strg_perc_tf <- (sqrt(master_data$GRI_strg_perc))
qqnorm(master_data$GRI_strg_perc_tf, pch = 1, frame = FALSE) #not bowed
qqline(master_data$GRI_strg_perc_tf, col = "steelblue", lwd = 2)
shapiro.test(master_data$GRI_strg_perc_tf) 
# no transformation    p = 3.685e-12
# sqrt transformation  p = 0.0004494  getting there
# log10 transformation p = 2.2e-16    but it looks really straight :(


#CVF raw words normality====
shapiro.test(master_data$amongCVF_AD)
qqnorm(master_data$amongCVF_AD, pch = 1, frame = FALSE) #very bowed
qqline(master_data$amongCVF_AD, col = "steelblue", lwd = 2)
#transformation
summary(powerTransform(master_data$amongCVF_AD)) #result is .5, suggesting to sqrt (or raise to a half power)
master_data$amongCVF_AD_tf <- (sqrt(master_data$amongCVF_AD))
qqnorm(master_data$amongCVF_AD, pch = 1, frame = FALSE) #not bowed
qqline(master_data$amongCVF_AD, col = "steelblue", lwd = 2)
shapiro.test(master_data$amongCVF_AD_tf) # p > 0.05 ,  sqrt transformation will be needed
# no transformation     p = 5.266e-12  
# sqrt transformation   p = 1.004e-08  
# log10 transformation  p = 1.783e-07  
# 1/Y transformation     p = 2.611e-11  

shapiro.test(master_data$amongCVF_CL)
qqnorm(master_data$amongCVF_CL, pch = 1, frame = FALSE) #very bowed
qqline(master_data$amongCVF_CL, col = "steelblue", lwd = 2)
#transformation
summary(powerTransform(master_data$amongCVF_CL)) #result is -1, suggesting 1/Y transformation
master_data$amongCVF_CL_tf <- (1/(master_data$amongCVF_CL))
qqnorm(master_data$amongCVF_CL, pch = 1, frame = FALSE) #not bowed
qqline(master_data$amongCVF_CL, col = "steelblue", lwd = 2)
shapiro.test(master_data$amongCVF_CL_tf) 
# no transformation     p = 9.996e-13   
# sqrt transformation   p = 5.071e-09   
# log10 transformation  p = 4.022e-05   
# 1/Y transformation    p = 0.849       

shapiro.test(master_data$amongCVF_HI)
qqnorm(master_data$amongCVF_HI, pch = 1, frame = FALSE) #very bowed
qqline(master_data$amongCVF_HI, col = "steelblue", lwd = 2)
#transformation
summary(powerTransform(master_data$amongCVF_HI)) #result is 0
master_data$amongCVF_HI_tf <- (sqrt(master_data$amongCVF_HI))
qqnorm(master_data$amongCVF_HI, pch = 1, frame = FALSE) #not bowed
qqline(master_data$amongCVF_HI, col = "steelblue", lwd = 2)
shapiro.test(master_data$amongCVF_HI_tf) 
# no transformation     p = 7.469e-05  
# sqrt transformation   p = 0.03901    
# log10 transformation  p = 0.5024     
# 1/Y tranformation     p = 0.0003655  

shapiro.test(master_data$amongCVF_MA)
qqnorm(master_data$amongCVF_MA, pch = 1, frame = FALSE) #very bowed
qqline(master_data$amongCVF_MA, col = "steelblue", lwd = 2)
#transformation
summary(powerTransform(master_data$amongCVF_MA)) #result is 4.47
master_data$amongCVF_MA_tf <- (sqrt(master_data$amongCVF_MA))
qqnorm(master_data$amongCVF_MA, pch = 1, frame = FALSE) #not bowed
qqline(master_data$amongCVF_MA, col = "steelblue", lwd = 2)
shapiro.test(master_data$amongCVF_MA_tf) 
# no transformation     p = 5.289e-10  
# sqrt transformation   p = 5.037e-12 
# log10 transformation  p = 4.361e-14  
# 1/Y tranformation     p = 2.2e-16    




#### MLM all/among====
mlms1am <- lm(sqrt(Percent_KW_S1) ~ log10(amongCVF_AD+0.001)+log10(amongCVF_CL+0.001)+log10(amongCVF_HI+0.001)+log10(amongCVF_MA+0.001), data = master_data)
mlms2am <- lm(sqrt(Percent_KW_S2) ~ log10(amongCVF_AD+0.001)+log10(amongCVF_CL+0.001)+log10(amongCVF_HI+0.001)+log10(amongCVF_MA+0.001), data = master_data)
mlms3am <- lm(sqrt(Percent_KW_S3) ~ log10(amongCVF_AD+0.001)+log10(amongCVF_CL+0.001)+log10(amongCVF_HI+0.001)+log10(amongCVF_MA+0.001), data = master_data)
mlms4am <- lm(sqrt(Percent_KW_S4) ~ log10(amongCVF_AD+0.001)+log10(amongCVF_CL+0.001)+log10(amongCVF_HI+0.001)+log10(amongCVF_MA+0.001), data = master_data)
mlms5am <- lm(sqrt(Percent_KW_S5) ~ log10(amongCVF_AD+0.001)+log10(amongCVF_CL+0.001)+log10(amongCVF_HI+0.001)+log10(amongCVF_MA+0.001), data = master_data)
stargazer(mlms1am,mlms2am,mlms3am,mlms4am,mlms5am, type = "text", single.row = T,  digits=3)
summary(mlms1am)


plot(mlms1am)
plot(mlms2am)
plot(mlms3am)
plot(mlms4am)
plot(mlms5am)



master_data$mlms1amres <- mlms1am$residuals
master_data$mlms2amres <- mlms2am$residuals
master_data$mlms3amres <- mlms3am$residuals
master_data$mlms4amres <- mlms4am$residuals
master_data$mlms5amres <- mlms5am$residuals
#Ho  p < 0.05: data is not normal
#Ha  p > 0.05: data is normal
shapiro.test(master_data$mlms1amres) #is normal
shapiro.test(master_data$mlms2amres) #is normal
shapiro.test(master_data$mlms3amres) #is normal
shapiro.test(master_data$mlms4amres) #not normal
shapiro.test(master_data$mlms5amres) #not normal
# p > 0.05: data is normal

# homeoelasticity test -
#                       none of these run :(
 leveneTest(sqrt(Percent_KW_S1) ~ sqrt(amongCVF_AD)+sqrt(amongCVF_CL)+sqrt(amongCVF_HI)+sqrt(amongCVF_MA), data = master_data)
 bartlett.test(sqrt(Percent_KW_S1) ~ sqrt(amongCVF_AD)+sqrt(amongCVF_CL)+sqrt(amongCVF_HI)+sqrt(amongCVF_MA), data = master_data)
# flinger test used because bartlett and levene test did not run
fligner.test(sqrt(Percent_KW_S1) ~ sqrt(amongCVF_AD)+sqrt(amongCVF_CL)+sqrt(amongCVF_HI)+sqrt(amongCVF_MA), data = master_data)

# Residuals are independent
# Ho  p < 0.05: Errors are serially UNcorrelated
# Ha  p > 0.05: errors are serially correlated
dwtest(mlms1am)
dwtest(mlms2am)
dwtest(mlms3am)
dwtest(mlms4am)
dwtest(mlms5am)
# p > 0.05, residuals are independent

gvmodel <- gvlma(mlms4am) #4 & 5 have problems
summary(gvmodel)


#### MLM all w/s / among cvf ====
mlmwealam <- lm(sqrt(GRI_weak_perc) ~ log10(amongCVF_AD+0.001)+log10(amongCVF_CL+0.001)+log10(amongCVF_HI+0.001)+log10(amongCVF_MA+0.001), data = master_data)
mlmstalam <- lm(sqrt(GRI_strg_perc) ~ log10(amongCVF_AD+0.001)+log10(amongCVF_CL+0.001)+log10(amongCVF_HI+0.001)+log10(amongCVF_MA+0.001), data = master_data)
stargazer(mlmwealam,mlmstalam, type = "text", single.row = T,  digits=3)

plot(mlmstalam)
plot(mlmwealam)

master_data$mlmwealam_res <- mlmwealam$residuals
master_data$mlmstalam_res <- mlmstalam$residuals
#Ho  p < 0.05: data is not normal
#Ha  p > 0.05: data is normal
shapiro.test(master_data$mlmwealam_res) #is normal
shapiro.test(master_data$mlmstalam_res) #not normal
# p > 0.05: data is normal

# homeoelasticity test -
#                       none of these run :(
#leveneTest(sqrt(GRI_weak_perc) ~ log10(amongCVF_AD+0.001)+log10(amongCVF_CL+0.001)+log10(amongCVF_HI+0.001)+log10(amongCVF_MA+0.001), data = master_data))
#bartlett.test(sqrt(GRI_weak_perc) ~ sqrt(amongCVF_AD)+sqrt(amongCVF_CL)+sqrt(amongCVF_HI)+sqrt(amongCVF_MA), data = master_data)
#fligner.test(sqrt(GRI_weak_perc) ~ sqrt(amongCVF_AD)+sqrt(amongCVF_CL)+sqrt(amongCVF_HI)+sqrt(amongCVF_MA), data = master_data)

# Residuals are independent
# Ho  p < 0.05: Errors are serially UNcorrelated
# Ha  p > 0.05: errors are serially correlated
dwtest(mlmwealam)
dwtest(mlmstalam)
# p > 0.05, residuals are independent

gvmodel_weak <- gvlma(mlmwealam)
summary(gvmodel_weak)
gvmodel_strong <- gvlma(mlmstalam)
summary(gvmodel_strong)

####notes====
# removing outlier COMPANY13 does not change any normality assumption testing results
# other transformations don't change normality assumptions test results

#smooothing? loess curve? kerneling? kriging?

# there is no normality test for simple non parametic regression? 
# https://www.sheffield.ac.uk/polopoly_fs/1.885202!/file/95_Normality_Check.pdf

# You don't need to assume Normal distributions to do regression. 
# Least squares regression is the BLUE estimator (Best Linear, Unbiased Estimator) regardless of the distributions
# https://stats.stackexchange.com/questions/75054/how-do-i-perform-a-regression-on-non-normal-data-which-remain-non-normal-when-tr

#If you've violated the normality assumption, you can fit a nonparametric regression model - Kabacoff
kruskal.test(sqrt(GRI_strg_perc) ~ sqrt(amongCVF_AD), data = master_data) #for ANOVA



#REOrder the models for CVF standards
 mlms1amreo <- lm(sqrt(Percent_KW_S1) ~ log10(amongCVF_CL+0.001)+log10(amongCVF_AD+0.001)+log10(amongCVF_HI+0.001)+log10(amongCVF_MA+0.001), data = master_data)
 mlms2amreo <- lm(sqrt(Percent_KW_S2) ~ log10(amongCVF_CL+0.001)+log10(amongCVF_AD+0.001)+log10(amongCVF_HI+0.001)+log10(amongCVF_MA+0.001), data = master_data)
 mlms3amreo <- lm(sqrt(Percent_KW_S3) ~ log10(amongCVF_CL+0.001)+log10(amongCVF_AD+0.001)+log10(amongCVF_HI+0.001)+log10(amongCVF_MA+0.001), data = master_data)
 mlms4amreo <- lm(sqrt(Percent_KW_S4) ~ log10(amongCVF_CL+0.001)+log10(amongCVF_AD+0.001)+log10(amongCVF_HI+0.001)+log10(amongCVF_MA+0.001), data = master_data)
 mlms5amreo <- lm(sqrt(Percent_KW_S5) ~ log10(amongCVF_CL+0.001)+log10(amongCVF_AD+0.001)+log10(amongCVF_HI+0.001)+log10(amongCVF_MA+0.001), data = master_data)
 stargazer(mlms1amreo,mlms2amreo,mlms3amreo,mlms4amreo,mlms5amreo, type = "text", single.row = T,  digits=3)

 mlmwealamreo <- lm(sqrt(GRI_weak_perc) ~ log10(amongCVF_CL+0.001)+log10(amongCVF_AD+0.001)+log10(amongCVF_HI+0.001)+log10(amongCVF_MA+0.001), data = master_data)
 mlmstalamreo <- lm(sqrt(GRI_strg_perc) ~ log10(amongCVF_CL+0.001)+log10(amongCVF_AD+0.001)+log10(amongCVF_HI+0.001)+log10(amongCVF_MA+0.001), data = master_data)
 stargazer(mlmwealamreo,mlmstalamreo, type = "text", single.row = T,  digits=3)

## Use added variable plots to see the plots
# partial regression plots
avPlots(mlms1amreo, main="Stage 1 CS" ,  cex=0, ylab="Stage 1 CS", grid=F, id=F)
avPlots(mlms2amreo, main="Stage 2 CS" ,  cex=0, ylab="Stage 2 CS", grid=F, id=F)
avPlots(mlms3amreo, main="Stage 3 CS" ,  cex=0, ylab="Stage 3 CS", grid=F, id=F)
avPlots(mlms4amreo, main="Stage 4 CS" ,  cex=0, ylab="Stage 4 CS", grid=F, id=F)
avPlots(mlms5amreo, main="Stage 5 CS" ,  cex=0, ylab="Stage 5 CS", grid=F, id=F)

avPlots(mlmwealamreo, main="Weak CS"  ,  cex=0, ylab="Weak CS"   , grid=F, id=F)
avPlots(mlmstalamreo, main="Strong CS",  cex=0, ylab="Strong CS" , grid=F, id=F)


#5 stages graphed
graphdata=master_data %>% select(  Percent_KW_S1, Percent_KW_S2, Percent_KW_S3, Percent_KW_S4, Percent_KW_S5, 
                                   amongCVF_CL, amongCVF_AD, amongCVF_HI, amongCVF_MA) %>% as.data.frame()

mgdata <- melt(graphdata, id=c("Percent_KW_S1",  "Percent_KW_S2", "Percent_KW_S3", "Percent_KW_S4", "Percent_KW_S5"))
mgdata$Percent_KW1_tf <- (sqrt(mgdata$Percent_KW_S1))
mgdata$Percent_KW2_tf <- (sqrt(mgdata$Percent_KW_S2))
mgdata$Percent_KW3_tf <- (sqrt(mgdata$Percent_KW_S3))
mgdata$Percent_KW4_tf <- (sqrt(mgdata$Percent_KW_S4))
mgdata$Percent_KW5_tf <- (sqrt(mgdata$Percent_KW_S5))
mgdata$value_tf       <- (log10(mgdata$value+0.001))

graphline_mg_cl <- ggplot(data= mgdata, aes(x=value_tf, y=Percent_KW1_tf , color=variable))
graphline_mg_cl + theme_classic() + geom_smooth(method="lm", se=FALSE)+facet_grid(.~variable)

graphline_mg_ad <- ggplot(data= mgdata, aes(x=amongCVF_AD_tf, y=value_tf, color=variable))
graphline_mg_ad + theme_classic() + geom_smooth(method="lm", se=FALSE)+facet_grid(.~variable)

graphline_mg_hi <- ggplot(data= mgdata, aes(x=amongCVF_HI_tf, y=value_tf, color=variable))
graphline_mg_hi + theme_classic() + geom_smooth(method="lm", se=FALSE)+facet_grid(.~variable)

graphline_mg_ma <- ggplot(data= mgdata, aes(x=amongCVF_MA_tf, y=value_tf, color=variable))
graphline_mg_ma + theme_classic() + geom_smooth(method="lm", se=FALSE)+facet_grid(.~variable)




#weak-strong aggregates
graphdata_we_st=master_data %>% select(amongCVF_CL, amongCVF_AD, amongCVF_HI, amongCVF_MA, GRI_weak_perc, GRI_strg_perc) %>% as.data.frame()

mgdata2 <- melt(graphdata_we_st, id=c("amongCVF_CL", "amongCVF_AD", "amongCVF_HI", "amongCVF_MA"))
mgdata2$amongCVF_CL_tf <- (log10(mgdata2$amongCVF_CL+0.001))
mgdata2$amongCVF_AD_tf <- (log10(mgdata2$amongCVF_AD+0.001))
mgdata2$amongCVF_HI_tf <- (log10(mgdata2$amongCVF_HI+0.001))
mgdata2$amongCVF_MA_tf <- (log10(mgdata2$amongCVF_MA+0.001))
mgdata2$value_tf       <- (sqrt(mgdata2$value))

graphline_mg_cl <- ggplot(data= mgdata2, aes(x=amongCVF_CL_tf, y=value_tf, color=variable))
graphline_mg_cl + theme_classic() + geom_smooth(method="lm", se=FALSE)+facet_grid(.~variable)

graphline_mg_ad <- ggplot(data= mgdata2, aes(x=amongCVF_AD_tf, y=value_tf, color=variable))
graphline_mg_ad + theme_classic() + geom_smooth(method="lm", se=FALSE)+facet_grid(.~variable)

graphline_mg_hi <- ggplot(data= mgdata2, aes(x=amongCVF_HI_tf, y=value_tf, color=variable))
graphline_mg_hi + theme_classic() + geom_smooth(method="lm", se=FALSE)+facet_grid(.~variable)

graphline_mg_ma <- ggplot(data= mgdata2, aes(x=amongCVF_MA_tf, y=value_tf, color=variable))
graphline_mg_ma + theme_classic() + geom_smooth(method="lm", se=FALSE)+facet_grid(.~variable)



m1 <- (lm(sqrt(Percent_KW_S1) ~ log10(amongCVF_AD+0.001), data=master_data))
plot(sqrt(Percent_KW_S1) ~ log10(amongCVF_AD+0.001), data=master_data)
abline(m1)



summary(mlms1am)
ggPredict(mlms1am, interactive=FALSE)

write.csv(master_data, "masterdata.csv")
