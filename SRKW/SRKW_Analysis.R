


# Clear workspace
rm(list=ls(all=TRUE))

# set start time for purposes of timing code
strt <- Sys.time()

# Load required libraries
library(RODBC)
library(readxl)
library(doBy)

# Set the paths 
paths = list("C:\\data\\GitHub\\FRAM_R-Code\\SRKW\\SRKW_Inputs.xlsx",
             "C:\\data\\NOF\\2017\\Modeling\\Chinook\\NewDB\\2011-16 Final ChinFRAM_OldBP.mdb")

# Set the input file path for the database containing FRAM runs
DBpath = paths[[2]]

#Import static input data
kCal_Age <- read_excel(paths[[1]], "R_In_kCal-Age")
FishFlag <- read_excel(paths[[1]], "R_In_FishFlag")
ppnInland <- read_excel(paths[[1]], "R_In_ppnInland")
Needs <- read_excel(paths[[1]], "R_In_Needs")

runID = 439

# Pull Cohort & Mortality tables from FRAM database
con = odbcConnectAccess(DBpath)
Cohort78 = sqlQuery(con, as.is = TRUE, 
                 paste(sep = '',
                       "SELECT * FROM Cohort"))
Mort78 = sqlQuery(con, as.is = TRUE, 
                  paste(sep = '',
                        "SELECT * FROM Mortality"))
close(con)

# Filter to desired RunID and to Time Steps 1-3
Cohort78 <- Cohort78[Cohort78$RunID == runID & Cohort78$TimeStep < 4, c(3:5,10)]
Mort78 <- Mort78[Mort78$RunID == runID & Mort78$TimeStep < 4, c(3:10,12:15)]

# Round cohort sizes to nearest integer (this is for testing purposes only, as PopStat reports
# are rounded to integers)
Cohort78$MidCohort <- round(Cohort78$MidCohort,0)

# Convert from 78 stock format to 39 stock format
Cohort78$Stock <- ceiling(Cohort78$StockID/2)

# Combine Marked and Unmarked components of each stock
Cohort <- summaryBy(MidCohort~RunID+Stock+Age+TimeStep, data = Cohort78, FUN = sum)

# Merge with ppnInland
Cohort <- merge(Cohort, ppnInland)

# Merge with kCal_Age
Cohort <- merge(Cohort, kCal_Age)

# Calculate abundance in inland waters
Cohort$InlandAbundance <- Cohort$MidCohort.sum * Cohort$Proportion

# Calculate kCal in inland waters
Cohort$InlandkCal <- as.numeric(Cohort$InlandAbundance * Cohort$kCal_Selectivity)

# Sum by time step and age
CohortSummary <- summaryBy(InlandAbundance+InlandkCal~TimeStep+Age, data = Cohort, FUN = sum)
CohortSummary$InlandkCal.sum <- as.numeric(format(CohortSummary$InlandkCal.sum, scientific = FALSE))
CohortSummary$InlandkCal.sum <- round(CohortSummary$InlandkCal.sum, 0)

kCal_TS <- summaryBy(InlandkCal.sum~TimeStep, data = CohortSummary, FUN = sum)
colnames(kCal_TS)[2] <- "InlandkCal"

kCal_TS <- merge(kCal_TS, Needs)

########################################################
# Calculate total mortality by S-A-F-T
Mort78$TotMort <- rowSums(Mort78[ ,c(5:12)])
Mort78 <- Mort78[ ,c(1:4,13)]

# Convert from 78 stock format to 39 stock format
Mort78$Stock <- ceiling(Mort78$StockID/2)

# Combine Marked and Unmarked components of each stock
Mort <- summaryBy(TotMort~Stock+Age+FisheryID+TimeStep, data = Mort78, FUN = sum)

# Merge with kCal_Age and FishFlag
Mort <- merge(Mort, FishFlag)
Mort <- merge(Mort, kCal_Age)

# Calculate kCal of Total Mortality
Mort$kCalMort <- Mort$TotMort.sum * Mort$kCal_Selectivity

PSMort_SAT <- summaryBy(TotMort.sum+kCalMort~Stock+Age+TimeStep, data = Mort[Mort$Flag == 1,], FUN = sum)
PSMort_AT <- summaryBy(TotMort.sum+kCalMort~TimeStep+Age, data = Mort[Mort$Flag == 1,], FUN = sum)
colnames(PSMort_AT)[c(3:4)] <- c("TotMort", "kCalMort")
PSMort_AT$kCalMort <- as.numeric(format(PSMort_AT$kCalMort, scientific = FALSE))
PSMort_AT$TotMort <- round(PSMort_AT$TotMort, 0)
PSMort_AT$kCalMort <- round(PSMort_AT$kCalMort, 0)









nd <- Sys.time()
tm <- nd - strt
tm