rm(list=ls())
seedVal = 17869
options(warn=-1)
options(scipen=999)

library(dplyr)
library(reshape2)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(dummies)

## Home Health
setwd("C:\\Users\\SROY\\Documents\\CodeBase\\Datasets\\Prepared\\HHCompare_Revised_FlatFiles")
# Load Data
HHC <- read.csv2("HHC_SOCRATA_PRVDR.csv", header = TRUE, sep = ',')
# For state PA
HHC <- HHC %>% filter(State == 'PA' & City == 'PHILADELPHIA' &
                        Quality.of.patient.care.star.rating != '')
HHC <- HHC[,c("Provider.Name", "Zip", "Type.of.Ownership", "Quality.of.patient.care.star.rating")]
names(HHC) <- c("HHC.Provider.Name", "Zip", "HHC.Type.of.Ownership", "HHC.star.rating")

## Nursing Facility
setwd("C:\\Users\\SROY\\Documents\\CodeBase\\Datasets\\Prepared\\NursingHomeCompare_Revised_FlatFiles")
# Load Data
NHC <- read.csv2("ProviderInfo_Download.csv", header = TRUE, sep = ',')
# For state PA
NHC <- NHC %>% filter(STATE == 'PA' & CITY == 'PHILADELPHIA')
#select columns
incList <- c("provnum",
             "PROVNAME",
             "ZIP",
             "OWNERSHIP",
             "BEDCERT",
             "RESTOT",
             "overall_rating")
NHC <- NHC[,incList]
names(NHC) <- c("NHC..Provider.Number",
                "NHC..Provider.Name",
                "Zip",
                "NHC.Type.of.Ownership",
                "NHC.BEDCERT",
                "NHC.RESTOT",
                "NHC.star.rating")

## Hospital Readmissions
setwd("C:\\Users\\SROY\\Documents\\CodeBase\\Datasets\\Prepared\\Hospital_Revised_Flatfiles")
# Load Data
HOSGI <- read.csv2("Hospital General Information.csv", header = TRUE, sep = ',')
HOSRR <- read.csv2("READMISSION REDUCTION.csv", header = TRUE, sep = ',')
# For state PA
HOSGI <- HOSGI %>% filter(State == 'PA' & City == 'PHILADELPHIA')
HOSGI <- HOSGI[,c('Provider.ID', 'ZIP.Code', 'Hospital.Type', 'Emergency.Services')]

HOSRR <- HOSRR %>% filter(State == 'PA')# & Measure.Name == 'READM-30-HIP-KNEE-HRRP')
HOSRR <- HOSRR[,c('Provider.Number', 'Hospital.Name',
                  'Number.of.Discharges','Number.of.Readmissions',
                  "Excess.Readmission.Ratio", 'Measure.Name')]
names(HOSRR) <- c('Provider.ID','Hospital.Name','Number.of.Discharges','Number.of.Readmissions', 
                  "Excess.Readmission.Ratio", 'Measure.Name')

HOS <- inner_join(HOSRR, HOSGI, by=c("Provider.ID" = "Provider.ID"))
names(HOS) <- c('Provider.ID','Hospital.Name','Number.of.Discharges','Number.of.Readmissions', 
                "Excess.Readmission.Ratio", 'Measure.Name',
                'Zip', 'Hospital.Type', 'Emergency.Services')

## Create one single set
tt <- NHC[,c('Zip','NHC.star.rating','NHC.Type.of.Ownership')]
tt$Zip <- as.factor(tt$Zip)
tt$NHC.star.rating <- as.factor(tt$NHC.star.rating)
t1 <- dcast(tt, Zip ~ NHC.star.rating, value.var="NHC.star.rating")
names(t1) <- c('Zip', paste0('NHC.star.rating.count.',names(t1)[2:length(names(t1))]))
t2 <- dcast(tt, Zip ~ NHC.Type.of.Ownership, value.var="NHC.Type.of.Ownership")
names(t2) <- c('Zip', paste0('NHC.OwnTyp.',names(t2)[2:length(names(t2))]))
NHCCount <- inner_join(t1, t2)

tt <- HHC[,c('Zip','HHC.Type.of.Ownership','HHC.star.rating')]
tt$Zip <- as.factor(tt$Zip)
tt$HHC.star.rating <- as.factor(as.integer(as.character(tt$HHC.star.rating)))
t1 <- dcast(tt, Zip ~ HHC.star.rating, value.var="HHC.star.rating")
names(t1) <- c('Zip', paste0('HHC.star.rating.count.',names(t1)[2:length(names(t1))]))
t2 <- dcast(tt, Zip ~ HHC.Type.of.Ownership, value.var="HHC.Type.of.Ownership")
names(t2) <- c('Zip', paste0('HHC.OwnTyp.',names(t2)[2:length(names(t2))]))
HHCCount <- inner_join(t1, t2)

rm(tt,t1,t2)

# Merge with main data
HOSSub <- HOS %>% filter(Excess.Readmission.Ratio != 'Not Available') %>%
  select(Zip, Excess.Readmission.Ratio, Measure.Name)
HOSSub$Zip <- as.factor(HOSSub$Zip)

HOSSub1 <- left_join(HOSSub, NHCCount)
HOSSub1$Zip <- as.factor(HOSSub1$Zip)
HOSSub1<- left_join(HOSSub1, HHCCount, by = c("Zip" = "Zip"))

# Make NA values as 0
HOSSub1 <- data.frame(apply(HOSSub1, 2, function(x) ifelse(is.na(x), 0, x)))
HOSSub1 <- cbind(HOSSub1[,1:3], HOSSub1[,4:ncol(HOSSub1)] %>% mutate_if(is.factor, as.character))
HOSSub1 <- cbind(HOSSub1[,1:3], HOSSub1[,4:ncol(HOSSub1)] %>% mutate_if(is.character, as.integer))
HOSSub1$TotalNHC <- rowSums(HOSSub1[,4:8]) 
HOSSub1$TotalHHC <- rowSums(HOSSub1[,16:20])
HOSSub1$Excess.Readmission.Ratio <- as.numeric(as.character(HOSSub1$Excess.Readmission.Ratio))

HOSSub1 <- HOSSub1 %>% filter(TotalNHC > 0)

dummyMeasure <- dummy(HOSSub1$Measure.Name)
HOSSub1 <- cbind(HOSSub1[,-3], dummyMeasure)
names(HOSSub1)[25:30] <- c("Measure.READM30AMI", "Measure.READM30CABG",
                           "Measure.READM30COPD", "Measure.READM30HF",
                           "Measure.READM30HIPKNEE", "Measure.READM30PN")

# Apply linear model
yVar <- 'Excess.Readmission.Ratio'
xVar <- names(HOSSub1)[!names(HOSSub1) %in% c('Zip', yVar)]
modelForm <- as.formula(paste(yVar, "~", paste(xVar, collapse = '+ '),-1))

model.lm <- lm(modelForm, data = HOSSub1)
summary(model.lm)

#Stepwise
step.lm <- step(model.lm, direction="both")
summary(step.lm)

# Convert into classification problem
quantile(HOSSub1$Excess.Readmission.Ratio)
# Dividing Readmission ration into 4 categories
HOSSub1$ReadmissionRisk <- cut(HOSSub1$Excess.Readmission.Ratio, 
                              breaks = c(-Inf, 0.99485, 1.08530, Inf), 
                              labels = c("LowRisk","MediumRisk","HighRisk"))
# Decision tree
yVar <- 'ReadmissionRisk'
xVar <- names(HOSSub1)[!names(HOSSub1) %in% c('Zip', 'Excess.Readmission.Ratio', yVar)]
modelForm <- as.formula(paste(yVar, "~", paste(xVar, collapse = '+ '),-1))
model.dt <- rpart(formula = ReadmissionRisk ~ NHC.star.rating.count.1 + 
                    NHC.star.rating.count.3 + NHC.star.rating.count.4 + NHC.star.rating.count.5 + 
                    NHC.OwnTyp.For.profit...Corporation + HHC.star.rating.count.3 - 
                    1, data = HOSSub1, method="anova")
summary(model.dt)
rpart.plot(model.dt, type=4)

model.dt.anova <- rpart(modelForm, data = HOSSub1, method="anova")
rpart.plot(model.dt.anova)
fancyRpartPlot(model.dt.anova)

setwd("C:\\Users\\SROY\\Documents\\CodeBase\\Datasets\\Prepared")
HHC$HHC.Provider.Name <- gsub("*,*","", HHC$HHC.Provider.Name)
HOS$Hospital.Name <- gsub("*,*","", HOS$Hospital.Name)
NHC$NHC..Provider.Name <- gsub("*,*","", NHC$NHC..Provider.Name)


write.table(HHC, file = 'HHC.csv', quote = FALSE, row.names=FALSE, sep=",")
write.table(NHC, file = 'NHC.csv', quote = FALSE, row.names=FALSE, sep=",")
write.table(HOSSub1, file = 'HOSSub1.csv', quote = FALSE, row.names=FALSE, sep=",")
write.table(HOS, file = 'HOS.csv', quote = FALSE, row.names=FALSE, sep=",")

