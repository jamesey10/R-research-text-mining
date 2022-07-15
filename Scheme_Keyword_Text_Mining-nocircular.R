# Title: Workflow for Word Count Frequencies
# Author: Dr. Brian Ohsowski and James Demastus MSc
# Date: JAN 15, 2021

# Required Libraries =================
library(tm)
library(vegan)
library(tcltk)
library(BiodiversityR)
library(ggplot2)
library(plyr)
library(reshape)
library(dplyr)
library(purrr)
library(tidyr)
library(car)
library(texreg)
library(onewaytests)
library(stargazer)


## Convert Text Files (.txt) to Comma Separated Files (.csv).  To do this.... ===================

## Open Excel
## Open the text file
## Step 1 of 3: Text Import Wizard > Delimiter Option (next)
## Step 2 of 3: Delimiter (none) and Text Qualifiers (none)
## Step 3 of 3: Column data format (General). Click Finish
## Save the Files as Comma Separated File (.csv)

# Create a master data frame with all .csv files in one folder ====================

# Import all .csv files in the folder
Import_All_CSV <- list.files(path = "TempData", pattern = ".csv$",  full.names = TRUE) %>%  map_df(~read.csv(.))

# Melt and remove all NA values from the dataframe
Remove_NAs <- melt(Import_All_CSV, measure.vars = names(Import_All_CSV))
Process_Data  <- na.omit(Remove_NAs)
# Change to UTF-8 text coding
Encoding(Process_Data$value) <- "UTF-8" 

# Remove Numbers, Special Characters, Spaces, and Wing Dings ======================

# Remove Special Characters
Process_Data$value <- gsub("0", "", Process_Data$value)
Process_Data$value <- gsub("1", "", Process_Data$value)
Process_Data$value <- gsub("2", "", Process_Data$value)
Process_Data$value <- gsub("3", "", Process_Data$value)
Process_Data$value <- gsub("4", "", Process_Data$value)
Process_Data$value <- gsub("5", "", Process_Data$value)
Process_Data$value <- gsub("6", "", Process_Data$value)
Process_Data$value <- gsub("7", "", Process_Data$value)
Process_Data$value <- gsub("8", "", Process_Data$value)
Process_Data$value <- gsub("9", "", Process_Data$value)
Process_Data$value <- gsub("-", "", Process_Data$value)
# Remove Double Spaces
Process_Data$value <- gsub("\\s+"," ",Process_Data$value)

# Keyword Count Consistency ======================

# Be thoughtful to ensure all iterations of key words are assessed
# Replace spaces with an underscore

Process_Data$value <- gsub("business model", "businessmodel", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("businessmodel", "businessmodel", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("business as usual", "businessasusual", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("businessasusual", "businessasusual", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("triple bottom line", "triplebottomline", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("triplebottomline", "triplebottomline", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("bottom line", "bottomline", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("bottomline", "bottomline", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("return on investment", "returnoninvestment", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("ROI ", "returnoninvestment", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("market share", "marketshare", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("market-share", "marketshare", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("market place", "marketplace", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("marketplace", "marketplace", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("market research", "marketresearch", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("marketresearch", "marketresearch", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("public relation", "publicrelation", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("public-relation", "publicrelation", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("competitive advantage", "competitiveadvantage", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("cost benefit", "costbenefit", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("cost-benefit", "costbenefit", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("market value", "marketvalue", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("value chain", "valuechain", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("value-chain", "valuechain", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("game chang", "gamechang", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("game-chang", "gamechang", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("global citizen", "globalcitizen", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("steady state", "steadystate", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("steady-state", "steadystate", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("natural system", "naturalsystem", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("natural-system", "naturalsystem", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("zero growth", "zerogrowth", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("zero-growth", "zerogrowth", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("carrying capaci", "carryingcapaci", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("carrying-capaci", "carryingcapacity", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("supply and demand", "supplyanddemand", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("supply demand", "supplyanddemand", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("planetary bound", "planetarybound", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("no growth", "nogrowth", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("no-growth", "nogrowth", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("non profit", "nonprofit", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("nonprofit", "nonprofit", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("not for profit", "notforprofit", Process_Data$value, ignore.case =TRUE, fixed=FALSE)
Process_Data$value <- gsub("the circular economy", "tce", Process_Data$value, ignore.case =TRUE, fixed=FALSE)


# Export .csv data to replace UTF-8 Code =======================================================================

# Note: Process_Results_Data.csv Data placed in a new folder named TestData

# Uncomment if writing a file.  Don't Overwrite Accidentally! *************
write.csv(Process_Data, "data26.csv")

# Important Note:  Delete Row
NO_UTF_Code <- read.csv("data26.csv")

Collapse_Data <- NO_UTF_Code %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(value = paste(value, collapse = " "))

Collapse_Data <- as.data.frame(Collapse_Data)

rownames(Collapse_Data) <- Collapse_Data$variable
Collapse_Data$variable <- NULL

# Transpose it!
Corpus_Processing <- t(Collapse_Data)

# Homogenize and strip punctuation and white space ========================
review_source <- VectorSource(Corpus_Processing)
corpus <- VCorpus(review_source) ##  a corpus is just all the files placed into one data frame

# Clean the text up with the following code
corpus <- tm_map(corpus, content_transformer(tolower)) # creates all lower case words
corpus <- tm_map(corpus, removePunctuation) # Removal of punctuation
corpus <- tm_map(corpus, stripWhitespace) # Removal of white space
corpus <- tm_map(corpus, removeWords, stopwords("english")) ## Removal of common of English words

# stopwords("english") ## To view these words, run this code


# Total Word Frequency for Each Document ==========================================
dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)

# Total Number of files in the list must be added as numbers 
Word_Count <- data.frame(dtm2['1',])
#  dtm2['1',],  dtm2['2',],  dtm2['3',],  dtm2['4',],  dtm2['5',],  dtm2['6',],   dtm2['7',] , dtm2['8',], 
#  dtm2['9',],	dtm2['10',], dtm2['11',],	dtm2['12',], dtm2['13',], dtm2['14',],	dtm2['15',],
#  dtm2['16',],	dtm2['17',], dtm2['18',],	dtm2['19',], dtm2['20',], dtm2['21',],	dtm2['22',], dtm2['23',],
#  dtm2['24',], dtm2['25',])

# Create Column names with the listed files in the folder
names(Word_Count) <- list.files(path = "TempData/", pattern="*.csv")

# Place row names (i.e. word) into a new column ===========================
Word_Count$text <- rownames(Word_Count)
rownames(Word_Count) <- NULL

head(Word_Count)

# Melt the data in a stacked, long-form format
Stacked_Data_Counts <- melt(Word_Count, id = c("text"))
names(Stacked_Data_Counts) <- c("text", "document", "count")

# Final Processing for Keyword Assessment ==========================

Keyword_Assessment_Object <- ddply(Stacked_Data_Counts, c("text", "document"), summarise,
                                                        wordCount = sum(count))
head(Keyword_Assessment_Object)


# WORD LIST =======================

# The below code is copy-pasted from LaTex code.
# * = wildcard symbol
# & = column divider

# ####### STAGE 1 #######
# & complian* & compliance, compliant
# & legal* & legal, legalized, legally, legality
# & regulat* & regulate, regulated, regulates, regulation, regulatory
# & risk* & risk, risks
#
# ####### STAGE 2 #######
#

# & biotechnolog* & biotechnology, biotechnologies
# & business as usual & business as usual
# & business model & business model
# & competitive advantag* & competitive advantage, competitive advantages
# & cost* & cost, costs, costly, costing, costed
# & cost-benefit* & cost-benefit, cost-benefits
# & customer* & customer, customers
# & demand* & demand, demands, demanding
# & efficienc* & efficiency, efficiencies
# & expens* & expense, expenses
# & growth & growth
# & market* & market, markets, marketing
# & market share* & market share, market shares
# & market value* & market value, market values
# & money & money
# & profit* & profit, profits, profited, profiting, profitable, profitability
# & public relations & public relations
# & retention & retention
# & return on investment & return on investment, ROI
# & sales & sales
# & strateg* & strategy, strategies, strategic, strategical, strategically
# & technolog* & technology, technologies
# & value chain* & value chain, value chains
#
# ####### STAGE 3 ########
#
# & collaborat* & collaborate, collaborates, collaborated, collaborating, collaborative, collaboratively
# & cooperat* & cooperate, cooperated, cooperating, cooperation, cooperative, cooperatives
# & ecoefficienc* & ecoefficiency, ecoefficiencies
# & game chang* & game changer, game changing
# & global citizen* & global citizen, global citizens, global citizenship
# & humanity & humanity
# & industry & industry
# & integrat* & integrate, integrates, integrating, integration, integrative
# & partnership* & partnership, partnerships
# & system* & system, systems, systemic
# & transform* & transform, transforms, transformed, transforming, transformation, transformations, transformative
#
# ####### STAGE 4 ########
# & carrying capacity & carrying capacity
# & consumption & consumption
# & degrowth & degrowth
# & holistic & holistic
# & interdependen* & interdependent, interdependence, interdependencies
# & natural system* & natural system, natural systems
# & preservation & preservation
# & planetary boundar* & planetary boundary, planetary boundaries
# & redistribution & redistribution
# & repair* & repair, repairs, repairing, repaired
# & restor* & restore, restored, restores, restoring, restoration, restorative
# & science* & science, sciences
# & scientific & scientific
# & steady state* & steady state, steady states
# & zero growth & zero growth
#
# ####### STAGE 5 ########
# & circular & circular
# & coevol* & coevolve, coevolving, coevolution
# & ecocentri* & ecocentric, ecocentrics, ecocentrism
# & ecoethic* & ecoethic, ecoethics
# & ecolog* & ecological, ecology
# & ecosystem* & ecosystem, ecosystems
# & flourish* & flourish, flourished, flourishes, flourishing
# & no growth & no growth
# & regenerat* & regenerate, regenerated, regenerating, regeneration, regenerative
# & resilien* & resilience, resilient


# SUBSETS of the Stages #####
regexes <- list(c("complian","STAGE 1"),
                c("legal","STAGE 1"),
                c("regulat","STAGE 1"),
                c("risk","STAGE 1"),

                c("businessasusual", "STAGE 2"),
                c("businessmodel", "STAGE 2"),
                c("biotechnolog", "STAGE 2"),
                c("competitiveadvantag", "STAGE 2"),
                c("cost|costs|costly|costing|costed", "STAGE 2"),
                c("customer", "STAGE 2"),
                c("demand", "STAGE 2"),
                c("efficienc", "STAGE 2"),
                c("expens", "STAGE 2"),
                c("growth", "STAGE 2"),
                c("market", "STAGE 2"),
                c("money", "STAGE 2"),
                c("expens", "STAGE 2"),
                c("profit", "STAGE 2"),
                c("publicrelations", "STAGE 2"),
                c("retention", "STAGE 2"),
                c("returnoninvestment", "STAGE 2"),
                c("sales", "STAGE 2"),
                c("strateg", "STAGE 2"),
                c("technolog", "STAGE 2"),
                c("valuechain", "STAGE 2"),

                c("collaborat", "STAGE 3"),
                c("cooperat", "STAGE 3"),
                c("ecoefficienc", "STAGE 3"),
                c("gamechang", "STAGE 3"),
                c("globalcitizen", "STAGE 3"),
                c("humanity", "STAGE 3"),
                c("industry", "STAGE 3"),
                c("integrat", "STAGE 3"),
                c("partnership", "STAGE 3"),
                c("system", "STAGE 3"),
                c("transform", "STAGE 3"),

                c("carryingcapacity", "STAGE 4"),
                c("consumption", "STAGE 4"),
                c("degrowth", "STAGE 4"),
                c("holistic", "STAGE 4"),
                c("interdependen", "STAGE 4"),
                c("natural system", "STAGE 4"),
                c("preservation", "STAGE 4"),
                c("planetaryboundar", "STAGE 4"),
                c("redistribution", "STAGE 4"),
                c("repair", "STAGE 4"),
                c("restor", "STAGE 4"),
                c("science", "STAGE 4"),
                c("scientific", "STAGE 4"),
                c("steadystate", "STAGE 4"),
                c("zerogrowth", "STAGE 4"),

                c("circular", "STAGE 5"),
                c("coevol", "STAGE 5"),
                c("ecocentri", "STAGE 5"),
                c("ecoethic", "STAGE 5"),
                c("ecolog", "STAGE 5"),
                c("ecosystem", "STAGE 5"),
                c("flourish", "STAGE 5"),
                c("nogrowth", "STAGE 5"),
                c("regenerat", "STAGE 5"),
                c("resilien", "STAGE 5")
                )

#Create a vector, the same length as the df
output_vector <- character(nrow(Keyword_Assessment_Object))

#For each regex..
for(i in seq_along(regexes)){

    #Grep through d$name, and when you find matches, insert the relevant 'tag' into
    #The output vector
    output_vector[grepl(x = Keyword_Assessment_Object$text, pattern = regexes[[i]][1], ignore.case = TRUE)] <- regexes[[i]][2]

}

#Insert that now-filled output vector into the data frame
Stages_Data <- Keyword_Assessment_Object
Stages_Data$stages <- output_vector
Stages_Data$stages <- as.factor(Stages_Data$stages)

# Replace Blank cells with NA and drop them
levels(Stages_Data$stages)[levels(Stages_Data$stages)==""] <- NA
Stages_Data <- na.omit(Stages_Data)

summary(Stages_Data)


# Take a look at your work and ID any issues with the regexs ===================
# Uncomment if writing a file.  Don't Overwrite Accidentally!
# Adjust Code Accordingly
 write.csv(Stages_Data, "TestData/Stages_Data_PB.csv")
 write.csv(Keyword_Assessment_Object, "TestData/Total_Words_Document_Data_PB.csv")
# an alarm to announce completion
 #browseURL('https://www.youtube.com/watch?v=QH2-TGUlwu4')

#### Combine the batches #### ====
# Merge batches of Stage Data GRI
PFGS01 <- read.csv("TestData/Stages_Data_AA1000.csv", head=(T))
PFGS02 <- read.csv("TestData/Stages_Data_BCorp.csv", head=(T))
PFGS03 <- read.csv("TestData/Stages_Data_BSI8001.csv", head=(T))
PFGS04 <- read.csv("TestData/Stages_Data_CDP.csv", head=(T))
PFGS05 <- read.csv("TestData/Stages_Data_CDSB.csv", head=(T))
PFGS06 <- read.csv("TestData/Stages_Data_CE.csv", head=(T))
PFGS07 <- read.csv("TestData/Stages_Data_EMAS.csv", head=(T))
#PFGS08 <- read.csv("TestData/Stages_Data_GreenAmerica.csv", head=(T))
#PFGS09 <- read.csv("TestData/Stages_Data_GreenC.csv", head=(T))
PFGS10 <- read.csv("TestData/Stages_Data_GRI.csv", head=(T))
PFGS11 <- read.csv("TestData/Stages_Data_IIRF.csv", head=(T))
PFGS12 <- read.csv("TestData/Stages_Data_ISO26000.csv", head=(T))
#PFGS13 <- read.csv("TestData/Stages_Data_MIF.csv", head=(T))
PFGS14 <- read.csv("TestData/Stages_Data_NatCapital.csv", head=(T))
PFGS15 <- read.csv("TestData/Stages_Data_OECD.csv", head=(T))
PFGS16 <- read.csv("TestData/Stages_Data_SA8000.csv", head=(T))
PFGS17 <- read.csv("TestData/Stages_Data_SASB.csv", head=(T))
PFGS18 <- read.csv("TestData/Stages_Data_SDG.csv", head=(T))
PFGS19 <- read.csv("TestData/Stages_Data_SDG_Compass.csv", head=(T))
PFGS20 <- read.csv("TestData/Stages_Data_TCFD.csv", head=(T))
PFGS21 <- read.csv("TestData/Stages_Data_TNS.csv", head=(T))
PFGS22 <- read.csv("TestData/Stages_Data_UL3600.csv", head=(T))
PFGS23 <- read.csv("TestData/Stages_Data_UNGC.csv", head=(T))
PFGS24 <- read.csv("TestData/Stages_Data_ITP.csv", head=(T))
PFGS25 <- read.csv("TestData/Stages_Data_doughnut.csv", head=(T))
PFGS26 <- read.csv("TestData/Stages_Data_PB.csv", head=(T))
SDmaster <- rbind(PFGS01, PFGS02, PFGS03, PFGS04, PFGS05, PFGS06, PFGS07,  
                          PFGS10, PFGS11, PFGS12,         PFGS14, PFGS15, PFGS16, 
                  PFGS17, PFGS18, PFGS19, PFGS20, PFGS21, PFGS22, PFGS23, PFGS24,
                  PFGS25, PFGS26)
write.csv(SDmaster, "TestData/SDmaster.csv", row.names = FALSE) 

# Merge batches of Total_Words_Document_Data_GRI
TWDG01 <- read.csv("TestData/Total_Words_Document_Data_AA1000.csv", head=(T))
TWDG02 <- read.csv("TestData/Total_Words_Document_Data_BCorp.csv", head=(T))
TWDG03 <- read.csv("TestData/Total_Words_Document_Data_BSI8001.csv", head=(T))
TWDG04 <- read.csv("TestData/Total_Words_Document_Data_CDP.csv", head=(T))
TWDG05 <- read.csv("TestData/Total_Words_Document_Data_CDSB.csv", head=(T))
TWDG06 <- read.csv("TestData/Total_Words_Document_Data_CE.csv", head=(T))
TWDG07 <- read.csv("TestData/Total_Words_Document_Data_EMAS.csv", head=(T))
#TWDG08 <- read.csv("TestData/Total_Words_Document_Data_GreenAmerica.csv", head=(T))
#TWDG09 <- read.csv("TestData/Total_Words_Document_Data_GreenC.csv", head=(T))
TWDG10 <- read.csv("TestData/Total_Words_Document_Data_GRI.csv", head=(T))
TWDG11 <- read.csv("TestData/Total_Words_Document_Data_IIRF.csv", head=(T))
TWDG12 <- read.csv("TestData/Total_Words_Document_Data_ISO26000.csv", head=(T))
#TWDG13 <- read.csv("TestData/Total_Words_Document_Data_MIF.csv", head=(T))
TWDG14 <- read.csv("TestData/Total_Words_Document_Data_NatCapital.csv", head=(T))
TWDG15 <- read.csv("TestData/Total_Words_Document_Data_OECD.csv", head=(T))
TWDG16 <- read.csv("TestData/Total_Words_Document_Data_SA8000.csv", head=(T))
TWDG17 <- read.csv("TestData/Total_Words_Document_Data_SASB.csv", head=(T))
TWDG18 <- read.csv("TestData/Total_Words_Document_Data_SDG.csv", head=(T))
TWDG19 <- read.csv("TestData/Total_Words_Document_Data_SDG_Compass.csv", head=(T))
TWDG20 <- read.csv("TestData/Total_Words_Document_Data_TCFD.csv", head=(T))
TWDG21 <- read.csv("TestData/Total_Words_Document_Data_TNS.csv", head=(T))
TWDG22 <- read.csv("TestData/Total_Words_Document_Data_UL3600.csv", head=(T))
TWDG23 <- read.csv("TestData/Total_Words_Document_Data_UNGC.csv", head=(T))
TWDG24 <- read.csv("TestData/Total_Words_Document_Data_ITP.csv", head=(T))
TWDG25 <- read.csv("TestData/Total_Words_Document_Data_doughnut.csv", head=(T))
TWDG26 <- read.csv("TestData/Total_Words_Document_Data_PB.csv", head=(T))
TWDGmaster <- rbind(TWDG01, TWDG02, TWDG03, TWDG04, TWDG05, TWDG06, TWDG07,        
                            TWDG10, TWDG11, TWDG12,         TWDG14, TWDG15, TWDG16,
                    TWDG17, TWDG18, TWDG19, TWDG20, TWDG21, TWDG22, TWDG23, TWDG24,
                    TWDG25, TWDG26)
write.csv(TWDGmaster, "TestData/TWmaster.csv", row.names = FALSE) 

#### STATS SECTION =====================================

# Analysis time!
# Assuming you did all the analysis as saved them as a .csv, you can just import the files
# Goal: We need standardized text counts (we don't want to bias our data by doc size)
# So, we must get a total word count for each document and total word count by stage

Import_Analysis_Data <- read.csv("TestData/SDmaster.csv")
Import_Analysis_Data$text <- as.factor(Import_Analysis_Data$text)
Import_Analysis_Data$document <- as.factor(Import_Analysis_Data$document)
Import_Analysis_Data$stages <- as.factor(Import_Analysis_Data$stages)
head(Import_Analysis_Data)
# Total Word Count =============================
Import_Total_Words <- read.csv("TestData/TWmaster.csv")
head(Import_Total_Words)
Total_Count <- ddply(Import_Total_Words, c("document"), summarise,
                  keywords    = sum(wordCount))
head(Total_Count)


# Stage/CVF Type Word Count =============================
Stage_Count <- ddply(Import_Analysis_Data, c("document", "stages"), summarise,
                      keywords    = sum(wordCount))
head(Stage_Count)

#find the possible NAs and add a 0 row 
#stage 5 - UNGC, SA8000, OECD, GreenC
Stage_Count <- Stage_Count %>% add_row(document = "SA8000.csv", stages = "STAGE 5" , keywords=0)
Stage_Count <- Stage_Count %>% add_row(document = "UNGC_merged.csv", stages = "STAGE 5" , keywords=0)
Stage_Count <- Stage_Count %>% add_row(document = "GreenC.csv", stages = "STAGE 5" , keywords=0)
Stage_Count <- Stage_Count %>% add_row(document = "OECD.csv", stages = "STAGE 5" , keywords=0)

# Merge the two data frames (total WC and stage KW count) to calculate the percentage ====
Stats_Data <- merge(Stage_Count, Total_Count, by = "document")
names(Stats_Data) <- c("document", "stages", "keywordCount", "totalCount")
Stats_Data$document <- as.factor(Stats_Data$document)
Stats_Data$stages <- as.factor(Stats_Data$stages)
str(Stats_Data)
head(Stats_Data)


# Calculate the percentage of keywords per stage in the entire document ====
Stats_Data$percentKeywords <- (Stats_Data$keywordCount / Stats_Data$totalCount) * 100
head(Stats_Data)


# Format final dataframe====
Stats_Data_wide <- reshape(Stats_Data, idvar = "document", timevar = "stages", direction = "wide")

# Remove duplicated wide format columns and rename total column
Stats_Data_wide <- select(Stats_Data_wide, - c("totalCount.STAGE 1", "totalCount.STAGE 2", "totalCount.STAGE 3", "totalCount.STAGE 4"))
names(Stats_Data_wide)[names(Stats_Data_wide) == "totalCount.STAGE 5"] <- "Total_Count"

master_data <- Stats_Data_wide

# Rename Columns to something friendly
names(master_data)[names(master_data) == "keywordCount.STAGE 1"] <- "Count_S1"
names(master_data)[names(master_data) == "keywordCount.STAGE 2"] <- "Count_S2"
names(master_data)[names(master_data) == "keywordCount.STAGE 3"] <- "Count_S3"
names(master_data)[names(master_data) == "keywordCount.STAGE 4"] <- "Count_S4"
names(master_data)[names(master_data) == "keywordCount.STAGE 5"] <- "Count_S5"
names(master_data)[names(master_data) == "percentKeywords.STAGE 1"] <- "Percent_S1"
names(master_data)[names(master_data) == "percentKeywords.STAGE 2"] <- "Percent_S2"
names(master_data)[names(master_data) == "percentKeywords.STAGE 3"] <- "Percent_S3"
names(master_data)[names(master_data) == "percentKeywords.STAGE 4"] <- "Percent_S4"
names(master_data)[names(master_data) == "percentKeywords.STAGE 5"] <- "Percent_S5"
names(master_data)[names(master_data) == "Total_Count"]    <- "Total_Word_Count"

write.csv(master_data, "masterdata.csv")
