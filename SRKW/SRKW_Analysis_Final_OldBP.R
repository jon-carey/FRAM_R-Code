#######################################################################################
# The purpose of this program is to process a specified set of paired FRAM model runs
# (one with fisheries of interest "turned on" and one with them "turned off")
# resulting in estimates of available Age 3-5 Chinook abundances and kilocalories
# in inland (Salish Sea) and coastal waters, to inform Southern Resident Killer Whale 
# analysis for a renewed PS Chinook RMP and Biological Opinion.
#
# Note: before running, ensure that the paths below are set to the correct directories
#   outfile_name; this is what the program will name the excel output file that it
#           generates.  If the file does not exist the program will create it.  If
#           it does exist it will be overwritten.  Ensure that the file is not open
#           before running the program or it will throw an error.
#   Path 1; SRKW input excel file - this file contains numerous tabs with static input
#           data necessary for the analysis. Double check that the RunIDs in the
#           'R_In_RunIDs' tab reference the appropriate runs in the databse below
#   Path 2; Access databse that contains the FRAM model runs referenced in column B of
#            the 'R_In_RunIDs' tab in the above input file (fisheries turned on)
#   Path 3; Access databse that contains the FRAM model runs referenced in column C of
#            the 'R_In_RunIDs' tab in the above input file (fisheries turned off)
#   Path 4; Output directory for saving tables and figures

# Warning: due the some of the packages used, this program requires that the 32-bit 
# version of R software is used.
#
# JC; Dec 2017
#######################################################################################


# Clear workspace
rm(list=ls(all=TRUE))

# set start time for purposes of timing code
strt <- Sys.time()

# install.packages("xlsx")

# Load required libraries
library(RODBC)
library(readxl)
library(doBy)
library(ggplot2)
library(xlsx)

# Round data? Prob only necessary for validating calcs with existing spreadsheets
RoundFlag = 0 # 0=No, 1=Yes

# Include natural mortality in abundances?
AbundType = 1 # 0=Yes, 1=No

# Set Excel Output file name
outfile_name = "test.xlsx"

# Set the paths:
#   1 = Excel input file
#   2 = FRAM db for 1st set of model runs (links to Col B in 'R_In_RunID' tab of above file)
#   3 = FRAM db for 2nd set of model runs (links to Col C in 'R_In_RunID' tab of above file)
#   4 = Output directory
paths = list("F:\\WDFW\\Documents\\FRAM\\SRKW\\R_Out\\Output Datasets\\2018.4.13\\SRKW_Inputs_OldBP_Pre_4.13.18.xlsx",
             "F:\\WDFW\\Documents\\FRAM\\SRKW\\R_Out\\Output Datasets\\2018.4.13\\OldBP_Preseason.mdb",
             "F:\\WDFW\\Documents\\FRAM\\SRKW\\R_Out\\Output Datasets\\2018.4.13\\OldBP_Preseason_ZeroPS.mdb",
             "F:\\WDFW\\Documents\\FRAM\\SRKW\\R_Out\\Output Datasets\\2018.4.13\\")

# Set the input file path for the database containing FRAM runs
DBpath1 = paths[[2]]
DBpath2 = paths[[3]]

# Set output directory
outfile = paths[[4]]

#Import static input data
kCal_Age <- read_excel(paths[[1]], "R_In_kCal-Age")
FishFlag <- read_excel(paths[[1]], "R_In_FishFlag") # Flags: 0=NonPS, 1=PreTermPS, 2=TermPS
StkDist <- read_excel(paths[[1]], "R_In_Distribution")
Needs <- read_excel(paths[[1]], "R_In_Needs")

# Remove unnecessary fields from StkDist
StkDist <- StkDist[ ,c(1,4:5)]

# Import list of RunIDs
RunIDs <- read_excel(paths[[1]], "R_In_RunID")

# Identify range of years for analysis
minYr <- min(RunIDs$Year)
maxYr <- max(RunIDs$Year)

# Create empty data frame summary files
Starting_Abundance_kCal <- as.data.frame(array(NA, c(0,6)))
colnames(Starting_Abundance_kCal) <- c("Year", "Run", "TimeStep", "Age", "Inland_Abundance",
                             "Inland_kCal")

AvailablePrey <- as.data.frame(array(NA, c(0,6)))
colnames(AvailablePrey) <- c("Year", "Run", "TimeStep", "Age", "Inland_Abundance",
                             "Inland_kCal")

kCal_to_Need <- as.data.frame(array(NA, c(0,8)))
colnames(kCal_to_Need) <- c("Year", "Run", "Region", "TimeStep", "Min_DPER_Avg", 
                            "Max_DPER_Avg", "Min_DPER_Max", "Min_DPER_Max")

PS.TermOnly_Removals <- as.data.frame(array(NA, c(0,6)))
colnames(PS.TermOnly_Removals) <- c("TimeStep", "Age", "TotMort", "kCalTotMort")

# Pull Cohort & Mortality tables from FRAM databases
con = odbcConnectAccess(DBpath1)
Cohort78_1 = sqlQuery(con, as.is = TRUE, 
                    paste(sep = '',
                          "SELECT * FROM Cohort"))
Mort78_1 = sqlQuery(con, as.is = TRUE, 
                  paste(sep = '',
                        "SELECT * FROM Mortality"))
close(con)

con = odbcConnectAccess(DBpath2)
Cohort78_2 = sqlQuery(con, as.is = TRUE, 
                      paste(sep = '',
                            "SELECT * FROM Cohort"))
Mort78_2 = sqlQuery(con, as.is = TRUE, 
                    paste(sep = '',
                          "SELECT * FROM Mortality"))
close(con)

# Trim to necessary RunID and make sure they are not the same for each set of runs
RunName1 <- colnames(RunIDs)[2]
RunName2 <- colnames(RunIDs)[3]

Cohort78_1 <- subset(Cohort78_1, RunID %in% unname(unlist(RunIDs[,2])))
Cohort78_1$RunID <- paste(RunName1, "_", Cohort78_1$RunID, sep = "")
Cohort78_2 <- subset(Cohort78_2, RunID %in% unname(unlist(RunIDs[,3])))
Cohort78_2$RunID <- paste(RunName2, "_", Cohort78_2$RunID, sep = "")
Mort78_1 <- subset(Mort78_1, RunID %in% unname(unlist(RunIDs[,2])))
Mort78_1$RunID <- paste(RunName1, "_", Mort78_1$RunID, sep = "")
Mort78_2 <- subset(Mort78_2, RunID %in% unname(unlist(RunIDs[,3])))
Mort78_2$RunID <- paste(RunName2, "_", Mort78_2$RunID, sep = "")

# Combine cohort and mortality tables for each set of runs
Cohort78 <- rbind(Cohort78_1, Cohort78_2)
Mort78 <- rbind(Mort78_1, Mort78_2)
rm(Cohort78_1, Cohort78_2, Mort78_1, Mort78_2)

############################################################
# A little pre-processing on the cohort and martality data #
############################################################

# Round cohort sizes to nearest integer (for testing only, as PopStat is rounded)
if(RoundFlag == 1) {
    Cohort78$MidCohort <- round(Cohort78$MidCohort,0)
    Cohort78$WorkingCohort <- round(Cohort78$WorkingCohort,0)
    Cohort78$StartCohort <- round(Cohort78$StartCohort,0)
}

# Remove unneccessary fields
Cohort78 <- Cohort78[ , c(2:5,8:10)]
Mort78 <- Mort78[ , c(2:10,12:15)]

# Convert from 78 stock format to 39 stock format
Cohort78$Stock <- ceiling(Cohort78$StockID/2)
Mort78$Stock <- ceiling(Mort78$StockID/2)

# Calculate total mortality by S-A-F-T
ifelse(RoundFlag == 1, Mort78$TotMort <- round(rowSums(Mort78[ ,c(6:13)]),0),
       Mort78$TotMort <- rowSums(Mort78[ ,c(6:13)]))
Mort78 <- Mort78[ ,c(1:5,15,14)]

# Combine Marked and Unmarked components of each stock
if(AbundType == 0) {
    Cohort78$Cohort <- Cohort78$MidCohort + (Cohort78$StartCohort - Cohort78$WorkingCohort)
    Cohort <- summaryBy(StartCohort+Cohort~RunID+Stock+Age+TimeStep, data = Cohort78, FUN = sum)
}
if(AbundType == 1) {
    Cohort <- summaryBy(StartCohort+MidCohort~RunID+Stock+Age+TimeStep, data = Cohort78, FUN = sum)
}
colnames(Cohort)[5:6] <- c("StartCohort","Cohort")
Mort <- summaryBy(TotMort~RunID+Stock+Age+FisheryID+TimeStep, data = Mort78, FUN = sum)

#######################################################
# Process 'Likely' and 'No Action' runs for each year #
#######################################################
# Loop through each year
i=minYr
for(i in minYr:maxYr) {
    # Process 'Likely' (j=2) then 'No Action' (j=3) runs
    j=2
    for(j in 2:3) {
        # Identify RunID
        runID <- paste(colnames(RunIDs)[j], "_", RunIDs[RunIDs$Year == i, j], sep = "")
        
        # Identify Run Type
        runType <- colnames(RunIDs)[j]
        
        # Reset TermMortFlag
        TermMortFlag = 0
        
        # Filter data to correct RunID
        cohort <- Cohort[Cohort$RunID == runID, ]
        mort <- Mort[Mort$RunID == runID, ]

        ################################################################
        # Calculate Chinook and kCals removed by PS terminal fisheries #
        ################################################################
        # Merge with kCal_Age and FishFlag
        mort <- merge(mort, FishFlag)
        mort <- merge(mort, kCal_Age)
        
        if(sum(mort[mort$Flag == 2, 6]) > 0) {
            TermMortFlag = 1
            
            # Discount or exclude morts in PS terminal fisheries where SRKWs have not 
            # been or have rarely been observed
            mort$TotMort.TermExcl <- mort$TotMort.sum * mort$Weight
            
            # Calculate kCal of mortalities
            mort$kCalTotMort <- mort$TotMort.sum * mort$kCal_Selectivity
            mort$kCalTotMort.TermExcl <- mort$TotMort.TermExcl * mort$kCal_Selectivity
            
            # Summarize mortalities by age-timestep
            PSMort_TermOnly_AT <- summaryBy(TotMort.TermExcl+kCalTotMort.TermExcl~TimeStep+Age,
                                            data = mort[mort$Flag == 2,], FUN = sum)
            
            # A little formatting (column names, get rid of sci. notation, rounding, etc...)
            colnames(PSMort_TermOnly_AT)[c(3:4)] <- c("TotMort","kCalTotMort")
            PSMort_TermOnly_AT$kCalTotMort <- as.numeric(format(PSMort_TermOnly_AT$kCalTotMort,
                                                                scientific = FALSE))
            PSMort_TermOnly_AT$TotMort <- round(PSMort_TermOnly_AT$TotMort, 0)
            PSMort_TermOnly_AT$kCalTotMort <- round(PSMort_TermOnly_AT$kCalTotMort, 0)
            
            # Add 'Year' and 'Run' fields
            PSMort_TermOnly_AT$Year <- c(rep(i, dim(PSMort_TermOnly_AT)[1]))
            PSMort_TermOnly_AT$Run <- c(rep(runType, dim(PSMort_TermOnly_AT)[1]))
            
            # Reorder and remove unnecessary columns
            PSMort_TermOnly_AT <- PSMort_TermOnly_AT[PSMort_TermOnly_AT$TimeStep == 3,
                                                     c(5,6,1:4)]
            
            # Append to main output files
            PS.TermOnly_Removals <- rbind(PS.TermOnly_Removals, PSMort_TermOnly_AT)
        }
        
        ##########################################################
        # Calculate abundance and kCal of available Chinook prey #
        ##########################################################
        # Merge with ppnInland and kCal_Age
        cohort <- merge(cohort, StkDist)
        cohort <- merge(cohort, kCal_Age)
        
        # Calculate abundance in inland and coastal waters
        cohort$InlandAbundance <- cohort$Cohort * cohort$ppnInland
        cohort$CoastalAbundance <- cohort$Cohort * cohort$ppnCoastal
        
        cohort$InlandAbundance_Start <- cohort$StartCohort * cohort$ppnInland
        cohort$CoastalAbundance_Start <- cohort$StartCohort * cohort$ppnCoastal
        
        # Calculate kCal in inland and coastal waters
        cohort$InlandkCal <- cohort$InlandAbundance * cohort$kCal_Selectivity
        cohort$CoastalkCal <- cohort$CoastalAbundance * cohort$kCal_Selectivity
        
        cohort$InlandkCal_Start <- cohort$InlandAbundance_Start * cohort$kCal_Selectivity
        cohort$CoastalkCal_Start <- cohort$CoastalAbundance_Start * cohort$kCal_Selectivity
        
        # Sum by time step and age (over all stocks)
        cohortSummary <- summaryBy(InlandAbundance+InlandkCal+CoastalAbundance+CoastalkCal~TimeStep+Age, 
                                   data = cohort, FUN = sum)
        cohortSummary$InlandkCal.sum <- as.numeric(format(cohortSummary$InlandkCal.sum,
                                                          scientific = FALSE))
        cohortSummary$InlandkCal.sum <- round(cohortSummary$InlandkCal.sum, 0)
        colnames(cohortSummary)[3:6] <- c("Inland_Abundance", "Inland_kCal",
                                          "Coastal_Abundance","Coastal_kCal")
        
        cohortStart <- summaryBy(InlandAbundance_Start+InlandkCal_Start+CoastalAbundance_Start+CoastalkCal_Start~TimeStep+Age, 
                                 data = cohort, FUN = sum)
        cohortStart$InlandkCal_Start.sum <- as.numeric(format(cohortStart$InlandkCal_Start.sum,
                                                              scientific = FALSE))
        cohortStart$InlandkCal_Start.sum <- round(cohortStart$InlandkCal_Start.sum, 0)
        colnames(cohortStart)[3:6] <- c("Inland_Abundance", "Inland_kCal",
                                          "Coastal_Abundance","Coastal_kCal")
        
        # Add Year and RunType
        cohortSummary$Year <- c(rep(i, dim(cohortSummary)[1]))
        cohortSummary$Run <- c(rep(runType, dim(cohortSummary)[1]))
        cohortSummary <- cohortSummary[ , c(7,8,1:6)]
        
        cohortStart$Year <- c(rep(i, dim(cohortStart)[1]))
        cohortStart$Run <- c(rep(runType, dim(cohortStart)[1]))
        cohortStart <- cohortStart[ , c(7,8,1:6)]
        
        ###############################################################
        # Calculate ratios of available kCal to needs (over ages 3-5) #
        ###############################################################
        # Summarize inland and coastal kCals by time step
        kCal_TS <- summaryBy(Inland_kCal+Coastal_kCal~TimeStep, 
                             data = cohortSummary[cohortSummary$Age > 2, ], FUN = sum)
        colnames(kCal_TS)[2:3] <- c("Inland","Coastal")
        
        # Remove PS terminal fishery kCals (if exist)
        if(TermMortFlag == 1) {
            PS_T3_Term_kCal <- sum(PSMort_TermOnly_AT[PSMort_TermOnly_AT$Age != 2, 6])
            kCal_TS[3,2] <- kCal_TS[3,2] - PS_T3_Term_kCal
        }
        
        # Reshape into long format
        kCal_TS <- reshape(kCal_TS, direction = "long", varying = list(names(kCal_TS)[2:3]),
                           v.names = "kCal", idvar = "TimeStep", timevar = "Region",
                           times = c("Inland", "Coastal"))
        # Merge available kCal with needs and calculate ratios
        kCal_TS <- merge(kCal_TS, Needs)
        kCal_TS$Min_DPER <- round(kCal_TS$kCal / kCal_TS$MinPER, 2)
        kCal_TS$Max_DPER <- round(kCal_TS$kCal / kCal_TS$MaxPER, 2)
        # kCal_TS$Min_DPER_Max <- round(kCal_TS$kCal / kCal_TS$MinPER_Max, 2)
        # kCal_TS$Max_DPER_Max <- round(kCal_TS$kCal / kCal_TS$MaxPER_Max, 2)
        kCal_TS$Year <- c(rep(i, dim(kCal_TS)[1]))
        kCal_TS$Run <- c(rep(runType, dim(kCal_TS)[1]))
        
        kCal_TS <- kCal_TS[order(kCal_TS$Region,kCal_TS$TimeStep), c(9:10,2,1,7:8)]
        
        # Append to main summary files
        Starting_Abundance_kCal <- rbind(Starting_Abundance_kCal, cohortStart)
        AvailablePrey <- rbind(AvailablePrey, cohortSummary)
        kCal_to_Need <- rbind(kCal_to_Need, kCal_TS)
    }
}

# AvailablePrey <- AvailablePrey[AvailablePrey$TimeStep < 4, ]

###########################
# GENERATE SUMMARY TABLES #
###########################
# Set Column Names
ColumnNames <- c(paste(RunName1, "_T1", sep = ""),
                 paste(RunName1, "_T2", sep = ""),
                 paste(RunName1, "_T3", sep = ""),
                 paste(RunName2, "_T1", sep = ""),
                 paste(RunName2, "_T2", sep = ""),
                 paste(RunName2, "_T3", sep = ""))

# Summarize age 3-5 coastal abundances
Age3to5Chin_Coastal <- summaryBy(Coastal_Abundance~Year+Run+TimeStep,
                                        data = AvailablePrey[AvailablePrey$Age > 2 & AvailablePrey$TimeStep < 4, ],
                                        FUN = sum)
Age3to5Chin_Coastal <- reshape(Age3to5Chin_Coastal, idvar = c("Year","Run"), 
                                      timevar = "TimeStep", direction = "wide")
Age3to5Chin_Coastal <- reshape(Age3to5Chin_Coastal, idvar = "Year", 
                                      timevar = "Run", direction = "wide")
Age3to5Chin_Coastal[ ,c(2:7)] <- round(Age3to5Chin_Coastal[ ,c(2:7)], 0)
colnames(Age3to5Chin_Coastal)[2:7] <- c("T1","T2","T3","T1 ","T2 ","T3 ")

# Summarize age 3-5 coastal kCals
Kilos_Coastal <- summaryBy(Coastal_kCal~Year+Run+TimeStep, 
                          data = AvailablePrey[AvailablePrey$Age > 2 & AvailablePrey$TimeStep < 4, ], FUN = sum)
Kilos_Coastal <- reshape(Kilos_Coastal, idvar = c("Year","Run"), 
                                timevar = "TimeStep", direction = "wide")
Kilos_Coastal <- reshape(Kilos_Coastal, idvar = "Year", timevar = "Run",
                                direction = "wide")
colnames(Kilos_Coastal)[2:7] <- c("T1","T2","T3","T1 ","T2 ","T3 ")


# Summarize age 3-5 inland abundances
Age3to5Chin_Inland <- summaryBy(Inland_Abundance~Year+Run+TimeStep,
                                data = AvailablePrey[AvailablePrey$Age > 2 & AvailablePrey$TimeStep < 4, ], FUN = sum)
Age3to5Chin_Inland <- reshape(Age3to5Chin_Inland, idvar = c("Year","Run"), 
                              timevar = "TimeStep", direction = "wide")
Age3to5Chin_Inland <- reshape(Age3to5Chin_Inland, idvar = "Year", timevar = "Run", 
                              direction = "wide")
colnames(Age3to5Chin_Inland)[2:7] <- c("T1","T2","T3","T1 ","T2 ","T3 ")

# Summarize age 3-5 inland kCals
Kilos_Inland <- summaryBy(Inland_kCal~Year+Run+TimeStep, 
                          data = AvailablePrey[AvailablePrey$Age > 2 & AvailablePrey$TimeStep < 4, ], FUN = sum)
Kilos_Inland <- reshape(Kilos_Inland, idvar = c("Year","Run"), timevar = "TimeStep",
                        direction = "wide")
Kilos_Inland <- reshape(Kilos_Inland, idvar = "Year", timevar = "Run", 
                        direction = "wide")
colnames(Kilos_Inland)[2:7] <- c("T1","T2","T3","T1 ","T2 ","T3 ")

# Calculate abundance and kCal after terminal in TS 3 and add to above 2 inland tables
PS_Term_Mort_T3 <- summaryBy(TotMort+kCalTotMort~Year+Run+TimeStep,
                             data = PS.TermOnly_Removals[PS.TermOnly_Removals$Age > 2, ],
                             FUN = sum)
PS_Term_Mort_T3_Run1 <- PS_Term_Mort_T3[PS_Term_Mort_T3$Run == RunName1, ]
PS_Term_Mort_T3_Run2 <- PS_Term_Mort_T3[PS_Term_Mort_T3$Run == RunName2, ]

if(dim(Age3to5Chin_Inland)[1] == dim(PS_Term_Mort_T3_Run1)[1]) {
    Age3to5Chin_Inland[ ,4] <- Age3to5Chin_Inland[ ,4] - PS_Term_Mort_T3_Run1$TotMort.sum
    colnames(Age3to5Chin_Inland)[4] <- "AfterTerm_T3"
    Kilos_Inland[ ,4] <- Kilos_Inland[ ,4] - PS_Term_Mort_T3_Run1$kCalTotMort.sum
    colnames(Kilos_Inland)[4] <- "AfterTerm_T3"
}
if(dim(Age3to5Chin_Inland)[1] == dim(PS_Term_Mort_T3_Run2)[1]) {
    Age3to5Chin_Inland[ ,7] <- Age3to5Chin_Inland[ ,7] - PS_Term_Mort_T3_Run2$TotMort.sum
    colnames(Age3to5Chin_Inland)[7] <- "AfterTerm_T3"
    Kilos_Inland[ ,7] <- Kilos_Inland[ ,7] - PS_Term_Mort_T3_Run2$kCalTotMort.sum
    colnames(Kilos_Inland)[7] <- "AfterTerm_T3"
}

Age3to5Chin_Inland[ ,c(2:7)] <- round(Age3to5Chin_Inland[ ,c(2:7)], 0)
Kilos_Inland[ ,c(2:7)] <- round(Kilos_Inland[ ,c(2:7)], 0)

# Summarize Inland Needs table
SummaryNeeds_Inland <- kCal_to_Need[kCal_to_Need$Region == "Inland" ,c(3:4,1:2,5:6)]
SummaryNeeds_Inland <- reshape(SummaryNeeds_Inland, idvar = c("TimeStep", "Year"), 
                               timevar = "Run", direction = "wide")
SummaryNeeds_Inland <- SummaryNeeds_Inland[order(SummaryNeeds_Inland$TimeStep), 
                                           c(1:5,7:8)]
colnames(SummaryNeeds_Inland)[3:7] <- c("Region", "MinDPER_Avg", "MaxDPER_Avg", 
                                        "MinDPER_Avg ", "MaxDPER_Avg ")

# Summarize Coastal Needs table
SummaryNeeds_Coastal <- kCal_to_Need[kCal_to_Need$Region == "Coastal" ,c(3:4,1:2,5:6)]
SummaryNeeds_Coastal <- reshape(SummaryNeeds_Coastal, idvar = c("TimeStep", "Year"), 
                               timevar = "Run", direction = "wide")
SummaryNeeds_Coastal <- SummaryNeeds_Coastal[order(SummaryNeeds_Coastal$TimeStep), 
                                           c(1:5,7:8)]
colnames(SummaryNeeds_Coastal)[3:7] <- c("Region", "MinDPER_Avg", "MaxDPER_Avg", 
                                         "MinDPER_Avg ", "MaxDPER_Avg ")

# Summarize Inland kCal%Inc table
kCalInc_Inland <- as.data.frame(Kilos_Inland[ ,1])
kCalInc_Inland$Oct_Apr <- (Kilos_Inland[ ,5] - Kilos_Inland[ ,2]) / Kilos_Inland[ ,2]
kCalInc_Inland$May_Jun <- (Kilos_Inland[ ,6] - Kilos_Inland[ ,3]) / Kilos_Inland[ ,3]
kCalInc_Inland$Jul_Sep <- (Kilos_Inland[ ,7] - Kilos_Inland[ ,4]) / Kilos_Inland[ ,4]
kCalInc_Inland[ ,c(2:4)] <- round(kCalInc_Inland[ ,c(2:4)], 4)
colnames(kCalInc_Inland)[1] <- c("Year")

# Summarize Coastal kCal%Inc table
kCalInc_Coastal <- as.data.frame(Kilos_Coastal[ ,1])
kCalInc_Coastal$Oct_Apr <- (Kilos_Coastal[ ,5] - Kilos_Coastal[ ,2]) / Kilos_Coastal[ ,2]
kCalInc_Coastal$May_Jun <- (Kilos_Coastal[ ,6] - Kilos_Coastal[ ,3]) / Kilos_Coastal[ ,3]
kCalInc_Coastal$Jul_Sep <- (Kilos_Coastal[ ,7] - Kilos_Coastal[ ,4]) / Kilos_Coastal[ ,4]
kCalInc_Coastal[ ,c(2:4)] <- round(kCalInc_Coastal[ ,c(2:4)], 4)
colnames(kCalInc_Coastal)[1] <- c("Year")

# Generate Inland Abundance Summary Table
Inland_Start_Abundance <- summaryBy(Inland_Abundance~Year+Run+TimeStep,
                                  data = Starting_Abundance_kCal[Starting_Abundance_kCal$Age > 2, c(1:6)],
                                  FUN = sum)
colnames(Inland_Start_Abundance)[4] <- "Inland_Start_Abundance"

Inland_End_Abundance <- summaryBy(Inland_Abundance~Year+Run+TimeStep,
                                  data = AvailablePrey[AvailablePrey$Age > 2, c(1:6)],
                                  FUN = sum)
colnames(Inland_End_Abundance)[4] <- "Inland_End_Abundance"

Inland_Abundance <- merge(Inland_Start_Abundance, Inland_End_Abundance)

i=1
for(i in 1:dim(PS_Term_Mort_T3_Run1)[1]) {
    year <- PS_Term_Mort_T3_Run1[i,1]
    run <- PS_Term_Mort_T3_Run1[i,2]
    ts <- PS_Term_Mort_T3_Run1[i,3]
    morts <- PS_Term_Mort_T3_Run1[i,4]
    
    Inland_Abundance[Inland_Abundance$Year == year & Inland_Abundance$Run == run & Inland_Abundance$TimeStep == ts, 5] <- 
        Inland_Abundance[Inland_Abundance$Year == year & Inland_Abundance$Run == run & Inland_Abundance$TimeStep == ts, 5] - morts
}

i=1
for(i in 1:dim(PS_Term_Mort_T3_Run2)[1]) {
    year <- PS_Term_Mort_T3_Run2[i,1]
    run <- PS_Term_Mort_T3_Run2[i,2]
    ts <- PS_Term_Mort_T3_Run2[i,3]
    morts <- PS_Term_Mort_T3_Run2[i,4]
    
    Inland_Abundance[Inland_Abundance$Year == year & Inland_Abundance$Run == run & Inland_Abundance$TimeStep == ts, 5] <- 
        Inland_Abundance[Inland_Abundance$Year == year & Inland_Abundance$Run == run & Inland_Abundance$TimeStep == ts, 5] - morts
}

Inland_Abundance <- reshape(Inland_Abundance, idvar = c("Year", "TimeStep"),
                                    timevar = "Run", direction = "wide")

Inland_Abundance$FisheryMorts1 <- Inland_Abundance[ ,3] - Inland_Abundance[ ,4]
Inland_Abundance$FisheryMorts2 <- Inland_Abundance[ ,5] - Inland_Abundance[ ,6]
Inland_Abundance$PercentRed1 <- Inland_Abundance[ ,7] / Inland_Abundance[ ,3]
Inland_Abundance$PercentRed2 <- Inland_Abundance[ ,8] / Inland_Abundance[ ,5]

Inland_Abundance[ ,c(3:8)] <- round(Inland_Abundance[ ,c(3:8)], 0)
Inland_Abundance[ ,c(9:10)] <- round(Inland_Abundance[ ,c(9:10)], 3)
Inland_Abundance <- Inland_Abundance[ ,c(1:4,7,9,5:6,8,10)]
colnames(Inland_Abundance)[3:10] <- c("Start_Abundance", "End_Abundance", "Fishery_Morts", "Percent_Reduction",
                                      "Start_Abundance ", "End_Abundance ", "Fishery_Morts ", "Percent_Reduction ")

# Generate Inland kCal Summary Table
Inland_Start_kCal <- summaryBy(Inland_kCal~Year+Run+TimeStep,
                               data = Starting_Abundance_kCal[Starting_Abundance_kCal$Age > 2, c(1:6)],
                               FUN = sum)
colnames(Inland_Start_kCal)[4] <- "Inland_Start_kCal"

Inland_End_kCal <- summaryBy(Inland_kCal~Year+Run+TimeStep,
                             data = AvailablePrey[AvailablePrey$Age > 2, c(1:6)],
                             FUN = sum)
colnames(Inland_End_kCal)[4] <- "Inland_End_kCal"

Inland_kCal <- merge(Inland_Start_kCal, Inland_End_kCal)

i=1
for(i in 1:dim(PS_Term_Mort_T3_Run1)[1]) {
    year <- PS_Term_Mort_T3_Run1[i,1]
    run <- PS_Term_Mort_T3_Run1[i,2]
    ts <- PS_Term_Mort_T3_Run1[i,3]
    morts_kCal <- PS_Term_Mort_T3_Run1[i,5]
    
    Inland_kCal[Inland_kCal$Year == year & Inland_kCal$Run == run & Inland_kCal$TimeStep == ts, 5] <- 
        Inland_kCal[Inland_kCal$Year == year & Inland_kCal$Run == run & Inland_kCal$TimeStep == ts, 5] - morts_kCal
}

i=1
for(i in 1:dim(PS_Term_Mort_T3_Run2)[1]) {
    year <- PS_Term_Mort_T3_Run2[i,1]
    run <- PS_Term_Mort_T3_Run2[i,2]
    ts <- PS_Term_Mort_T3_Run2[i,3]
    morts_kCal <- PS_Term_Mort_T3_Run2[i,5]
    
    Inland_kCal[Inland_kCal$Year == year & Inland_kCal$Run == run & Inland_kCal$TimeStep == ts, 5] <- 
        Inland_kCal[Inland_kCal$Year == year & Inland_kCal$Run == run & Inland_kCal$TimeStep == ts, 5] - morts_kCal
}

Inland_kCal <- reshape(Inland_kCal, idvar = c("Year", "TimeStep"),
                       timevar = "Run", direction = "wide")

Inland_kCal$FisheryMorts1 <- Inland_kCal[ ,3] - Inland_kCal[ ,4]
Inland_kCal$FisheryMorts2 <- Inland_kCal[ ,5] - Inland_kCal[ ,6]
Inland_kCal$PercentRed1 <- Inland_kCal[ ,7] / Inland_kCal[ ,3]
Inland_kCal$PercentRed2 <- Inland_kCal[ ,8] / Inland_kCal[ ,5]

Inland_kCal[ ,c(3:8)] <- round(Inland_kCal[ ,c(3:8)], 0)
Inland_kCal[ ,c(9:10)] <- round(Inland_kCal[ ,c(9:10)], 3)
Inland_kCal <- Inland_kCal[ ,c(1:4,7,9,5:6,8,10)]
colnames(Inland_kCal)[3:10] <- c("Start_kCal", "End_kCal", "Fishery_Morts", "Percent_Reduction",
                                 "Start_kCal ", "End_kCal ", "Fishery_Morts ", "Percent_Reduction ")

# Generate Coastal Abundance Summary Table
Coastal_Start_Abundance <- summaryBy(Coastal_Abundance~Year+Run+TimeStep,
                                     data = Starting_Abundance_kCal[Starting_Abundance_kCal$Age > 2, c(1:4,7:8)],
                                     FUN = sum)
colnames(Coastal_Start_Abundance)[4] <- "Coastal_Start_Abundance"

Coastal_End_Abundance <- summaryBy(Coastal_Abundance~Year+Run+TimeStep,
                                   data = AvailablePrey[AvailablePrey$Age > 2, c(1:4,7:8)],
                                   FUN = sum)
colnames(Coastal_End_Abundance)[4] <- "Coastal_End_Abundance"

Coastal_Abundance <- merge(Coastal_Start_Abundance, Coastal_End_Abundance)

Coastal_Abundance <- reshape(Coastal_Abundance, idvar = c("Year", "TimeStep"),
                             timevar = "Run", direction = "wide")

Coastal_Abundance$FisheryMorts1 <- Coastal_Abundance[ ,3] - Coastal_Abundance[ ,4]
Coastal_Abundance$FisheryMorts2 <- Coastal_Abundance[ ,5] - Coastal_Abundance[ ,6]
Coastal_Abundance$PercentRed1 <- Coastal_Abundance[ ,7] / Coastal_Abundance[ ,3]
Coastal_Abundance$PercentRed2 <- Coastal_Abundance[ ,8] / Coastal_Abundance[ ,5]

Coastal_Abundance[ ,c(3:8)] <- round(Coastal_Abundance[ ,c(3:8)], 0)
Coastal_Abundance[ ,c(9:10)] <- round(Coastal_Abundance[ ,c(9:10)], 3)
Coastal_Abundance <- Coastal_Abundance[ ,c(1:4,7,9,5:6,8,10)]
colnames(Coastal_Abundance)[3:10] <- c("Start_Abundance", "End_Abundance", "Fishery_Morts", "Percent_Reduction",
                                       "Start_Abundance ", "End_Abundance ", "Fishery_Morts ", "Percent_Reduction ")

# Generate Coastal kCal Summary Table
Coastal_Start_kCal <- summaryBy(Coastal_kCal~Year+Run+TimeStep,
                                data = Starting_Abundance_kCal[Starting_Abundance_kCal$Age > 2, c(1:4,7:8)],
                                FUN = sum)
colnames(Coastal_Start_kCal)[4] <- "Coastal_Start_kCal"

Coastal_End_kCal <- summaryBy(Coastal_kCal~Year+Run+TimeStep,
                              data = AvailablePrey[AvailablePrey$Age > 2, c(1:4,7:8)],
                              FUN = sum)
colnames(Coastal_End_kCal)[4] <- "Coastal_End_kCal"

Coastal_kCal <- merge(Coastal_Start_kCal, Coastal_End_kCal)

Coastal_kCal <- reshape(Coastal_kCal, idvar = c("Year", "TimeStep"),
                        timevar = "Run", direction = "wide")

Coastal_kCal$FisheryMorts1 <- Coastal_kCal[ ,3] - Coastal_kCal[ ,4]
Coastal_kCal$FisheryMorts2 <- Coastal_kCal[ ,5] - Coastal_kCal[ ,6]
Coastal_kCal$PercentRed1 <- Coastal_kCal[ ,7] / Coastal_kCal[ ,3]
Coastal_kCal$PercentRed2 <- Coastal_kCal[ ,8] / Coastal_kCal[ ,5]

Coastal_kCal[ ,c(3:8)] <- round(Coastal_kCal[ ,c(3:8)], 0)
Coastal_kCal[ ,c(9:10)] <- round(Coastal_kCal[ ,c(9:10)], 3)
Coastal_kCal <- Coastal_kCal[ ,c(1:4,7,9,5:6,8,10)]
colnames(Coastal_kCal)[3:10] <- c("Start_kCal", "End_kCal", "Fishery_Morts", "Percent_Reduction",
                                  "Start_kCal ", "End_kCal ", "Fishery_Morts ", "Percent_Reduction ")

############################
# GENERATE SUMMARY FIGURES #
############################

#### INLAND ABUNDANCE FIGURES ####
figdat <- summaryBy(Inland_Abundance~Year+Run+TimeStep, 
                    data = AvailablePrey[AvailablePrey$Age > 2, ], FUN = sum)
figdat[figdat$TimeStep == 1, 3] <- "Oct-Apr"
figdat[figdat$TimeStep == 2, 3] <- "May-Jun"
figdat[figdat$TimeStep == 3, 3] <- "Jul-Sep"

# Add factors so things appear in the desired order
figdat$TimeStep <- factor(figdat$TimeStep, levels = c("Oct-Apr",
                                                      "May-Jun",
                                                      "Jul-Sep"))
figdat$Run <- factor(figdat$Run, levels = c(RunName2, RunName1))

# Create plot
p <- ggplot(data = figdat, aes(Year, Inland_Abundance.sum/1000, fill=Run))
p <- p + geom_bar(width=0.5, color="black", alpha=1, position="dodge",
                  stat="identity")
p <- p + facet_grid(TimeStep ~ .) +
    theme(strip.text.x=element_text(size=15)) +
    ggtitle("Chinook Age 3-5 Available to SRKW in Inland Waters") +
    theme(plot.title=element_text(size=15, face="bold")) + #make title bold
    theme(axis.title.x=element_text(size=10),
          axis.text.x=element_text(angle=90, size=10,color="black", hjust=1,
                                   vjust=0.25)) +
    theme(axis.title.y=element_text(size=10, vjust=1.3),
          axis.text.y=element_text(angle=0, size=10,color="black")) + #resize tick labels, move axis lablel (vjust)
    ylab("Chinook Age 3-5 (thousands)") + xlab("Fishing Year") +
    theme(legend.title=element_text(size=10, face="bold")) +
    theme(legend.text=element_text(size=10)) +
    theme(panel.grid.major.x=element_blank(),
          panel.grid.minor.x=element_blank()) + #removes the vertical background grid
    # theme(panel.grid.major.y=element_blank(),
    #       panel.grid.minor.y=element_blank()) + #removes the horizontal background grid
    # theme(panel.background=element_blank()) + #removes the gray filled back
    scale_x_continuous(breaks = c(minYr:maxYr)) + #this adds labels for all years to the x-axis
    theme(panel.background = element_rect(colour="black",size=1)) + #this adds a border
    theme(plot.margin=unit(c(2,-2,-4,2),"mm"))

ggsave(paste(outfile,"AbundanceCharts_Inland_",RunName2,".jpeg",sep=""),p,height=5,width=7.5)


#### COASTAL ABUNDANCE FIGURES ####
figdat <- summaryBy(Coastal_Abundance~Year+Run+TimeStep, 
                    data = AvailablePrey[AvailablePrey$Age > 2, ], FUN = sum)
figdat[figdat$TimeStep == 1, 3] <- "Oct-Apr"
figdat[figdat$TimeStep == 2, 3] <- "May-Jun"
figdat[figdat$TimeStep == 3, 3] <- "Jul-Sep"

# Add factors so things appear in the desired order
figdat$TimeStep <- factor(figdat$TimeStep, levels = c("Oct-Apr",
                                                      "May-Jun",
                                                      "Jul-Sep"))
figdat$Run <- factor(figdat$Run, levels = c(RunName2, RunName1))

# Create plot
p <- ggplot(data = figdat, aes(Year, Coastal_Abundance.sum/1000, fill=Run))
p <- p + geom_bar(width=0.5, color="black", alpha=1, position="dodge",
                  stat="identity")
p <- p + facet_grid(TimeStep ~ .) +
    theme(strip.text.x=element_text(size=15)) +
    ggtitle("Chinook Age 3-5 Available to SRKW in Coastal Waters") +
    theme(plot.title=element_text(size=15, face="bold")) + #make title bold
    theme(axis.title.x=element_text(size=10),
          axis.text.x=element_text(angle=90, size=10,color="black", hjust=1,
                                   vjust=0.25)) +
    theme(axis.title.y=element_text(size=10, vjust=1.3),
          axis.text.y=element_text(angle=0, size=10,color="black")) + #resize tick labels, move axis lablel (vjust)
    ylab("Chinook Age 3-5 (thousands)") + xlab("Fishing Year") +
    theme(legend.title=element_text(size=10, face="bold")) +
    theme(legend.text=element_text(size=10)) +
    theme(panel.grid.major.x=element_blank(),
          panel.grid.minor.x=element_blank()) + #removes the vertical background grid
    # theme(panel.grid.major.y=element_blank(),
    #       panel.grid.minor.y=element_blank()) + #removes the horizontal background grid
    # theme(panel.background=element_blank()) + #removes the gray filled back
    scale_x_continuous(breaks = c(minYr:maxYr)) + #this adds labels for all years to the x-axis
    theme(panel.background = element_rect(colour="black",size=1)) + #this adds a border
    theme(plot.margin=unit(c(2,-2,-4,2),"mm"))

ggsave(paste(outfile,"AbundanceCharts_Coastal_",RunName2,".jpeg",sep=""),p,height=5,width=7.5)


#### INLAND KILOCALORIE FIGURES ####
figdat <- summaryBy(Inland_kCal~Year+Run+TimeStep, 
                    data = AvailablePrey[AvailablePrey$Age > 2, ], FUN = sum)
figdat[figdat$TimeStep == 1, 3] <- "Oct-Apr"
figdat[figdat$TimeStep == 2, 3] <- "May-Jun"
figdat[figdat$TimeStep == 3, 3] <- "Jul-Sep"

# Add factors so things appear in the desired order
figdat$TimeStep <- factor(figdat$TimeStep, levels = c("Oct-Apr",
                                                      "May-Jun",
                                                      "Jul-Sep"))
figdat$Run <- factor(figdat$Run, levels = c(RunName2, RunName1))

# Create plot
p <- ggplot(data = figdat, aes(Year, Inland_kCal.sum/1000000, fill=Run))
p <- p + geom_bar(width=0.5, color="black", alpha=1, position="dodge",
                  stat="identity")
p <- p + facet_grid(TimeStep ~ .) +
    theme(strip.text.x=element_text(size=15)) +
    ggtitle("Kilcalories of Chinook Available to SRKW in Inland Waters") +
    theme(plot.title=element_text(size=15, face="bold")) + #make title bold
    theme(axis.title.x=element_text(size=10),
          axis.text.x=element_text(angle=90, size=10,color="black", hjust=1,
                                   vjust=0.25)) +
    theme(axis.title.y=element_text(size=10, vjust=1.3),
          axis.text.y=element_text(angle=0, size=10,color="black")) + #resize tick labels, move axis lablel (vjust)
    ylab("Chinook Kilocalories (millions)") + xlab("Fishing Year") +
    theme(legend.title=element_text(size=10, face="bold")) +
    theme(legend.text=element_text(size=10)) +
    theme(panel.grid.major.x=element_blank(),
          panel.grid.minor.x=element_blank()) + #removes the vertical background grid
    # theme(panel.grid.major.y=element_blank(),
    #       panel.grid.minor.y=element_blank()) + #removes the horizontal background grid
    # theme(panel.background=element_blank()) + #removes the gray filled back
    scale_x_continuous(breaks = c(minYr:maxYr)) + #this adds labels for all years to the x-axis
    theme(panel.background = element_rect(colour="black",size=1)) + #this adds a border
    theme(plot.margin=unit(c(2,-2,-4,2),"mm"))

ggsave(paste(outfile,"KilocalorieCharts_Inland_",RunName2,".jpeg",sep=""),p,height=5,width=7.5)


#### COASTAL KILOCALORIE FIGURES ####
figdat <- summaryBy(Coastal_kCal~Year+Run+TimeStep, 
                    data = AvailablePrey[AvailablePrey$Age > 2, ], FUN = sum)
figdat[figdat$TimeStep == 1, 3] <- "Oct-Apr"
figdat[figdat$TimeStep == 2, 3] <- "May-Jun"
figdat[figdat$TimeStep == 3, 3] <- "Jul-Sep"

# Add factors so things appear in the desired order
figdat$TimeStep <- factor(figdat$TimeStep, levels = c("Oct-Apr",
                                                      "May-Jun",
                                                      "Jul-Sep"))
figdat$Run <- factor(figdat$Run, levels = c(RunName2, RunName1))

# Create plot
p <- ggplot(data = figdat, aes(Year, Coastal_kCal.sum/1000000, fill=Run))
p <- p + geom_bar(width=0.5, color="black", alpha=1, position="dodge",
                  stat="identity")
p <- p + facet_grid(TimeStep ~ .) +
    theme(strip.text.x=element_text(size=15)) +
    ggtitle("Kilcalories of Chinook Available to SRKW in Coastal Waters") +
    theme(plot.title=element_text(size=15, face="bold")) + #make title bold
    theme(axis.title.x=element_text(size=10),
          axis.text.x=element_text(angle=90, size=10,color="black", hjust=1,
                                   vjust=0.25)) +
    theme(axis.title.y=element_text(size=10, vjust=1.3),
          axis.text.y=element_text(angle=0, size=10,color="black")) + #resize tick labels, move axis lablel (vjust)
    ylab("Chinook Kilocalories (millions)") + xlab("Fishing Year") +
    theme(legend.title=element_text(size=10, face="bold")) +
    theme(legend.text=element_text(size=10)) +
    theme(panel.grid.major.x=element_blank(),
          panel.grid.minor.x=element_blank()) + #removes the vertical background grid
    # theme(panel.grid.major.y=element_blank(),
    #       panel.grid.minor.y=element_blank()) + #removes the horizontal background grid
    # theme(panel.background=element_blank()) + #removes the gray filled back
    scale_x_continuous(breaks = c(minYr:maxYr)) + #this adds labels for all years to the x-axis
    theme(panel.background = element_rect(colour="black",size=1)) + #this adds a border
    theme(plot.margin=unit(c(2,-2,-4,2),"mm"))

ggsave(paste(outfile,"KilocalorieCharts_Coastal_",RunName2,".jpeg",sep=""),p,height=5,width=7.5)


#### COASTAL NEEDS FIGURES (Max PER)####
figdat <- summaryBy(Max_DPER_Avg~Year+Run+TimeStep, 
                    data = kCal_to_Need[kCal_to_Need$Region == "Coastal", ], FUN = sum)
figdat[figdat$TimeStep == 1, 3] <- "Oct-Apr"
figdat[figdat$TimeStep == 2, 3] <- "May-Jun"
figdat[figdat$TimeStep == 3, 3] <- "Jul-Sep"

# Add factors so things appear in the desired order
figdat$TimeStep <- factor(figdat$TimeStep, levels = c("Oct-Apr",
                                                      "May-Jun",
                                                      "Jul-Sep"))
figdat$Run <- factor(figdat$Run, levels = c(RunName2, RunName1))

# Create plot
p <- ggplot(data = figdat, aes(Year, Max_DPER_Avg.sum, fill=Run))
p <- p + geom_bar(width=0.5, color="black", alpha=1, position="dodge",
                  stat="identity")
p <- p + facet_grid(TimeStep ~ .) +
    theme(strip.text.x=element_text(size=15)) +
    ggtitle("Coastal Kilcalorie Availability to Needs Ratio (Max PER)") +
    theme(plot.title=element_text(size=15, face="bold")) + #make title bold
    theme(axis.title.x=element_text(size=10),
          axis.text.x=element_text(angle=90, size=10,color="black", hjust=1,
                                   vjust=0.25)) +
    theme(axis.title.y=element_text(size=10, vjust=1.3),
          axis.text.y=element_text(angle=0, size=10,color="black")) + #resize tick labels, move axis lablel (vjust)
    ylab("Available Kilocalories / Needs") + xlab("Fishing Year") +
    theme(legend.title=element_text(size=10, face="bold")) +
    theme(legend.text=element_text(size=10)) +
    theme(panel.grid.major.x=element_blank(),
          panel.grid.minor.x=element_blank()) + #removes the vertical background grid
    # theme(panel.grid.major.y=element_blank(),
    #       panel.grid.minor.y=element_blank()) + #removes the horizontal background grid
    # theme(panel.background=element_blank()) + #removes the gray filled back
    scale_x_continuous(breaks = c(minYr:maxYr)) + #this adds labels for all years to the x-axis
    theme(panel.background = element_rect(colour="black",size=1)) + #this adds a border
    theme(plot.margin=unit(c(2,-2,-4,2),"mm"))

ggsave(paste(outfile,"MaxNeeds_Coastal_",RunName2,".jpeg",sep=""),p,height=5,width=7.5)


#### INLAND NEEDS FIGURES (Max PER)####
figdat <- summaryBy(Max_DPER_Avg~Year+Run+TimeStep, 
                    data = kCal_to_Need[kCal_to_Need$Region == "Inland", ], FUN = sum)
figdat[figdat$TimeStep == 1, 3] <- "Oct-Apr"
figdat[figdat$TimeStep == 2, 3] <- "May-Jun"
figdat[figdat$TimeStep == 3, 3] <- "Jul-Sep"

# Add factors so things appear in the desired order
figdat$TimeStep <- factor(figdat$TimeStep, levels = c("Oct-Apr",
                                                      "May-Jun",
                                                      "Jul-Sep"))
figdat$Run <- factor(figdat$Run, levels = c(RunName2, RunName1))

# Create plot
p <- ggplot(data = figdat, aes(Year, Max_DPER_Avg.sum, fill=Run))
p <- p + geom_bar(width=0.5, color="black", alpha=1, position="dodge",
                  stat="identity")
p <- p + facet_grid(TimeStep ~ .) +
    theme(strip.text.x=element_text(size=15)) +
    ggtitle("Inland Kilcalorie Availability to Needs Ratio (Max PER)") +
    theme(plot.title=element_text(size=15, face="bold")) + #make title bold
    theme(axis.title.x=element_text(size=10),
          axis.text.x=element_text(angle=90, size=10,color="black", hjust=1,
                                   vjust=0.25)) +
    theme(axis.title.y=element_text(size=10, vjust=1.3),
          axis.text.y=element_text(angle=0, size=10,color="black")) + #resize tick labels, move axis lablel (vjust)
    ylab("Available Kilocalories / Needs") + xlab("Fishing Year") +
    theme(legend.title=element_text(size=10, face="bold")) +
    theme(legend.text=element_text(size=10)) +
    theme(panel.grid.major.x=element_blank(),
          panel.grid.minor.x=element_blank()) + #removes the vertical background grid
    # theme(panel.grid.major.y=element_blank(),
    #       panel.grid.minor.y=element_blank()) + #removes the horizontal background grid
    # theme(panel.background=element_blank()) + #removes the gray filled back
    scale_x_continuous(breaks = c(minYr:maxYr)) + #this adds labels for all years to the x-axis
    theme(panel.background = element_rect(colour="black",size=1)) + #this adds a border
    theme(plot.margin=unit(c(2,-2,-4,2),"mm"))

ggsave(paste(outfile,"MaxNeeds_Inland_",RunName2,".jpeg",sep=""),p,height=5,width=7.5)


###########
# EXPORT! #
###########

wb <- createWorkbook(type = "xlsx")

# Set up formats
TITLE_STYLE <- CellStyle(wb) + Font(wb,  heightInPoints=14, isItalic=TRUE, isBold=TRUE)

TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold = TRUE) +
    Border(color = "black", position = c("BOTTOM"), pen = c("BORDER_THIN"))

TABLE_COLNAMES_STYLE2 <- CellStyle(wb) + Font(wb, isBold = TRUE) + 
    Alignment(wrapText = TRUE) + 
    Border(color = "black", position = c("BOTTOM"), pen = c("BORDER_THIN"))

# Sheet 1; Abundance_Coastal
sheet1 <- createSheet(wb, sheetName = "Abundance_Coastal")
addDataFrame(Age3to5Chin_Coastal, sheet1, startRow = 2, colnamesStyle = TABLE_COLNAMES_STYLE)
setColumnWidth(sheet1, colIndex = c(1,2), colWidth = 5)
setColumnWidth(sheet1, colIndex = c(3:8), colWidth = 15)

rows <-createRow(sheet1,rowIndex=1)
sheetTitle <-createCell(rows, colIndex=3)
setCellValue(sheetTitle[[1,1]], RunName1)
setCellStyle(sheetTitle[[1,1]], TITLE_STYLE)
sheetTitle <-createCell(rows, colIndex=6)
setCellValue(sheetTitle[[1,1]], RunName2)
setCellStyle(sheetTitle[[1,1]], TITLE_STYLE)

addPicture(paste(outfile,"AbundanceCharts_Coastal_",RunName2,".jpeg",sep=""),
           sheet1, scale = 1, startRow = dim(Age3to5Chin_Coastal)[1]+5, startColumn = 1)

# Sheet 2; Kilocalories_Coastal
sheet2 <- createSheet(wb, sheetName = "Kilocalories_Coastal")
addDataFrame(Kilos_Coastal, sheet2, startRow = 2, colnamesStyle = TABLE_COLNAMES_STYLE)
setColumnWidth(sheet2, colIndex = c(1,2), colWidth = 5)
setColumnWidth(sheet2, colIndex = c(3:8), colWidth = 15)

rows <-createRow(sheet2,rowIndex=1)
sheetTitle <-createCell(rows, colIndex=3)
setCellValue(sheetTitle[[1,1]], RunName1)
setCellStyle(sheetTitle[[1,1]], TITLE_STYLE)
sheetTitle <-createCell(rows, colIndex=6)
setCellValue(sheetTitle[[1,1]], RunName2)
setCellStyle(sheetTitle[[1,1]], TITLE_STYLE)

addPicture(paste(outfile,"KilocalorieCharts_Coastal_",RunName2,".jpeg",sep=""),
           sheet2, scale = 1, startRow = dim(Kilos_Coastal)[1]+5, startColumn = 1)

# Sheet 3; kcal%Inc_Coastal
sheet3 <- createSheet(wb, sheetName = "kCal%Inc_Coastal")
addDataFrame(kCalInc_Coastal, sheet3, colnamesStyle = TABLE_COLNAMES_STYLE)
setColumnWidth(sheet3, colIndex = c(1,2), colWidth = 5)
setColumnWidth(sheet3, colIndex = c(3:5), colWidth = 12)

# Sheet 4; NeedsRatio_Coastal
sheet4 <- createSheet(wb, sheetName = "NeedsRatio_Coastal")
addDataFrame(SummaryNeeds_Coastal, sheet4, startRow = 2, colnamesStyle = TABLE_COLNAMES_STYLE2)
setColumnWidth(sheet4, colIndex = c(1,3), colWidth = 5)
setColumnWidth(sheet4, colIndex = c(5:12), colWidth = 15)

rows <-createRow(sheet4,rowIndex=1)
sheetTitle <-createCell(rows, colIndex=5)
setCellValue(sheetTitle[[1,1]], RunName1)
setCellStyle(sheetTitle[[1,1]], TITLE_STYLE)
sheetTitle <-createCell(rows, colIndex=7)
setCellValue(sheetTitle[[1,1]], RunName2)
setCellStyle(sheetTitle[[1,1]], TITLE_STYLE)

addPicture(paste(outfile,"MaxNeeds_Coastal_",RunName2,".jpeg",sep=""),
           sheet4, scale = 1, startRow = 2, startColumn = 10)

# Sheet 5; Abundance_Inland
sheet5 <- createSheet(wb, sheetName = "Abundance_Inland")
addDataFrame(Age3to5Chin_Inland, sheet5, startRow = 2, colnamesStyle = TABLE_COLNAMES_STYLE)
setColumnWidth(sheet5, colIndex = c(1,2), colWidth = 5)
setColumnWidth(sheet5, colIndex = c(3:9), colWidth = 15)

rows <-createRow(sheet5,rowIndex=1)
sheetTitle <-createCell(rows, colIndex=3)
setCellValue(sheetTitle[[1,1]], RunName1)
setCellStyle(sheetTitle[[1,1]], TITLE_STYLE)
sheetTitle <-createCell(rows, colIndex=6)
setCellValue(sheetTitle[[1,1]], RunName2)
setCellStyle(sheetTitle[[1,1]], TITLE_STYLE)

addPicture(paste(outfile,"AbundanceCharts_Inland_",RunName2,".jpeg",sep=""),
           sheet5, scale = 1, startRow = dim(Age3to5Chin_Inland)[1]+5, startColumn = 1)

# Sheet 6; Kilocalories_Inland
sheet6 <- createSheet(wb, sheetName = "Kilocalories_Inland")
addDataFrame(Kilos_Inland, sheet6, startRow = 2, colnamesStyle = TABLE_COLNAMES_STYLE)
setColumnWidth(sheet6, colIndex = c(1,2), colWidth = 5)
setColumnWidth(sheet6, colIndex = c(3:9), colWidth = 15)

rows <-createRow(sheet6,rowIndex=1)
sheetTitle <-createCell(rows, colIndex=3)
setCellValue(sheetTitle[[1,1]], RunName1)
setCellStyle(sheetTitle[[1,1]], TITLE_STYLE)
sheetTitle <-createCell(rows, colIndex=6)
setCellValue(sheetTitle[[1,1]], RunName2)
setCellStyle(sheetTitle[[1,1]], TITLE_STYLE)

addPicture(paste(outfile,"KilocalorieCharts_Inland_",RunName2,".jpeg",sep=""),
           sheet6, scale = 1, startRow = dim(Kilos_Inland)[1]+5, startColumn = 1)

# Sheet 7; kCal%Inc_Inland
sheet7 <- createSheet(wb, sheetName = "kCal%Inc_Inland")
addDataFrame(kCalInc_Inland, sheet7, colnamesStyle = TABLE_COLNAMES_STYLE)
setColumnWidth(sheet7, colIndex = c(1,2), colWidth = 5)
setColumnWidth(sheet7, colIndex = c(3:5), colWidth = 12)

# Sheet 8; NeedsRatio_Inland
sheet8 <- createSheet(wb, sheetName = "NeedsRatio_Inland")
addDataFrame(SummaryNeeds_Inland, sheet8, startRow = 2, colnamesStyle = TABLE_COLNAMES_STYLE2)
setColumnWidth(sheet8, colIndex = c(1,3), colWidth = 5)
setColumnWidth(sheet8, colIndex = c(5:12), colWidth = 15)

rows <-createRow(sheet8,rowIndex=1)
sheetTitle <-createCell(rows, colIndex=5)
setCellValue(sheetTitle[[1,1]], RunName1)
setCellStyle(sheetTitle[[1,1]], TITLE_STYLE)
sheetTitle <-createCell(rows, colIndex=7)
setCellValue(sheetTitle[[1,1]], RunName2)
setCellStyle(sheetTitle[[1,1]], TITLE_STYLE)

addPicture(paste(outfile,"MaxNeeds_Inland_",RunName2,".jpeg",sep=""),
           sheet8, scale = 1, startRow = 2, startColumn = 10)

# Sheet 9; Coastal_Abundance_Detail
sheet9 <- createSheet(wb, sheetName = "Coastal_Abundance_Detail")
addDataFrame(Coastal_Abundance, sheet9, startRow = 2, colnamesStyle = TABLE_COLNAMES_STYLE)
setColumnWidth(sheet9, colIndex = c(1), colWidth = 4)
setColumnWidth(sheet9, colIndex = c(2), colWidth = 5)
setColumnWidth(sheet9, colIndex = c(3), colWidth = 10)
setColumnWidth(sheet9, colIndex = c(4,8), colWidth = 18)
setColumnWidth(sheet9, colIndex = c(5,9), colWidth = 17)
setColumnWidth(sheet9, colIndex = c(6,10), colWidth = 15)
setColumnWidth(sheet9, colIndex = c(7,11), colWidth = 20)

rows <-createRow(sheet9,rowIndex=1)
sheetTitle <-createCell(rows, colIndex=4)
setCellValue(sheetTitle[[1,1]], RunName1)
setCellStyle(sheetTitle[[1,1]], TITLE_STYLE)
sheetTitle <-createCell(rows, colIndex=8)
setCellValue(sheetTitle[[1,1]], RunName2)
setCellStyle(sheetTitle[[1,1]], TITLE_STYLE)

# Sheet 10; Coastal_kCal_Detail
sheet10 <- createSheet(wb, sheetName = "Coastal_kCal_Detail")
addDataFrame(Coastal_kCal, sheet10, startRow = 2, colnamesStyle = TABLE_COLNAMES_STYLE)
setColumnWidth(sheet10, colIndex = c(1), colWidth = 4)
setColumnWidth(sheet10, colIndex = c(2), colWidth = 5)
setColumnWidth(sheet10, colIndex = c(3), colWidth = 10)
setColumnWidth(sheet10, colIndex = c(4,8), colWidth = 18)
setColumnWidth(sheet10, colIndex = c(5,9), colWidth = 17)
setColumnWidth(sheet10, colIndex = c(6,10), colWidth = 15)
setColumnWidth(sheet10, colIndex = c(7,11), colWidth = 20)

rows <-createRow(sheet10,rowIndex=1)
sheetTitle <-createCell(rows, colIndex=4)
setCellValue(sheetTitle[[1,1]], RunName1)
setCellStyle(sheetTitle[[1,1]], TITLE_STYLE)
sheetTitle <-createCell(rows, colIndex=8)
setCellValue(sheetTitle[[1,1]], RunName2)
setCellStyle(sheetTitle[[1,1]], TITLE_STYLE)

# Sheet 11; Inland_Abundance_Detail
sheet11 <- createSheet(wb, sheetName = "Inland_Abundance_Detail")
addDataFrame(Inland_Abundance, sheet11, startRow = 2, colnamesStyle = TABLE_COLNAMES_STYLE)
setColumnWidth(sheet11, colIndex = c(1), colWidth = 4)
setColumnWidth(sheet11, colIndex = c(2), colWidth = 5)
setColumnWidth(sheet11, colIndex = c(3), colWidth = 10)
setColumnWidth(sheet11, colIndex = c(4,8), colWidth = 18)
setColumnWidth(sheet11, colIndex = c(5,9), colWidth = 17)
setColumnWidth(sheet11, colIndex = c(6,10), colWidth = 15)
setColumnWidth(sheet11, colIndex = c(7,11), colWidth = 20)

rows <-createRow(sheet11,rowIndex=1)
sheetTitle <-createCell(rows, colIndex=4)
setCellValue(sheetTitle[[1,1]], RunName1)
setCellStyle(sheetTitle[[1,1]], TITLE_STYLE)
sheetTitle <-createCell(rows, colIndex=8)
setCellValue(sheetTitle[[1,1]], RunName2)
setCellStyle(sheetTitle[[1,1]], TITLE_STYLE)

# Sheet 12; Inland_kCal_Detail
sheet12 <- createSheet(wb, sheetName = "Inland_kCal_Detail")
addDataFrame(Inland_kCal, sheet12, startRow = 2, colnamesStyle = TABLE_COLNAMES_STYLE)
setColumnWidth(sheet12, colIndex = c(1), colWidth = 4)
setColumnWidth(sheet12, colIndex = c(2), colWidth = 5)
setColumnWidth(sheet12, colIndex = c(3), colWidth = 10)
setColumnWidth(sheet12, colIndex = c(4,8), colWidth = 18)
setColumnWidth(sheet12, colIndex = c(5,9), colWidth = 17)
setColumnWidth(sheet12, colIndex = c(6,10), colWidth = 15)
setColumnWidth(sheet12, colIndex = c(7,11), colWidth = 20)

rows <-createRow(sheet12,rowIndex=1)
sheetTitle <-createCell(rows, colIndex=4)
setCellValue(sheetTitle[[1,1]], RunName1)
setCellStyle(sheetTitle[[1,1]], TITLE_STYLE)
sheetTitle <-createCell(rows, colIndex=8)
setCellValue(sheetTitle[[1,1]], RunName2)
setCellStyle(sheetTitle[[1,1]], TITLE_STYLE)

saveWorkbook(wb, paste(outfile, outfile_name, sep = ""))

# # Export Summary Tables to CSV files
# write.csv(Age3to5Chin_Coastal, paste(sep="", outfile, "SummaryAge3to5Chinook_Coastal.csv"))
# write.csv(Kilos_Coastal, paste(sep="", outfile, "SummaryKilos_Coastal.csv"))
# write.csv(Age3to5Chin_Inland, paste(sep="", outfile, "SummaryAge3to5Chinook_Inland.csv"))
# write.csv(Kilos_Inland, paste(sep="", outfile, "SummaryKilos_Inland.csv"))
# write.csv(SummaryNeeds_Inland, paste(sep="", outfile, "SummaryNeeds_Inland.csv"))
# write.csv(SummaryNeeds_Coastal, paste(sep="", outfile, "SummaryNeeds_Coastal.csv"))
# write.csv(FishRedux_Inland, paste(sep="", outfile, "SummaryFisheryRedux_Inland.csv"))
# write.csv(FishRedux_Coastal, paste(sep="", outfile, "SummaryFisheryRedux_Coastal.csv"))

nd <- Sys.time()
tm <- nd - strt
tm