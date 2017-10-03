#######################################################################################
# The purpose of this program is to process a specified set of FRAM model runs 
# resulting in estimates of Age 3-5 Chinook abundances and available Kilocalories
# inside the Salish Sea, to inform Southern Resident Killer Whale analysis for  
# a renewed PS Chinook RMP and Biological Opinion.
#
# Note: before running, ensure that the paths below are set to the correct directories
#   Path 1; SRKW input excel file - this file contains numerous tabs with static input
#           data necessary for the analysis. Double check that the RunIDs in the
#           'R_In_RunIDs' tab reference the appropriate runs in the databse below
#   Path 2; Access databse that contains the FRAM model runs referenced in the 
#           'R_In_RunIDs' tab in the above input file
#   Path 3; Output directory for saving tables and figures
#
# JC; Aug 2017
#######################################################################################


# Clear workspace
rm(list=ls(all=TRUE))

# set start time for purposes of timing code
strt <- Sys.time()

# Load required libraries
library(RODBC)
library(readxl)
library(doBy)
library(ggplot2)

# Round data? Prob only necessary for validating calcs with existing spreadsheets
RoundFlag = 0 # 0=No, 1=Yes

# Process abundances before natural mortality or after pre-terminal fisheries?
AbundType = 0 # 0 = Before NatMort, 1 = After PT Fish Mort

# Set the paths:
#   1 = Excel input file
#   2 = FRAM db for 1st set of model runs (links to Col B in 'R_In_RunID' tab of above file)
#   3 = FRAM db for 2nd set of model runs (links to Col C in 'R_In_RunID' tab of above file)
#   4 = Output directory
paths = list("C:\\data\\FRAM\\SRKW\\R_In\\SRKW_Inputs_New.xlsx",
             "C:\\data\\FRAM\\SRKW\\R_In\\Valid2016_NewBP_Round5_Iter3.mdb",
             "C:\\data\\FRAM\\SRKW\\R_In\\Valid2016_NewBP_Round5_Iter3 - ZeroUS.mdb",
             "C:\\data\\FRAM\\SRKW\\R_Out\\")

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
AvailablePrey <- as.data.frame(array(NA, c(0,6)))
colnames(AvailablePrey) <- c("Year", "Run", "TimeStep", "Age", "Inland_Abundance",
                             "Inland_kCal")

kCal_to_Need <- as.data.frame(array(NA, c(0,8)))
colnames(kCal_to_Need) <- c("Year", "Run", "Region", "TimeStep", "Min_DPER_Avg", 
                            "Max_DPER_Avg", "Min_DPER_Max", "Min_DPER_Max")

PS_Removals <- as.data.frame(array(NA, c(0,6)))
colnames(PS_Removals) <- c("TimeStep", "Age", "TotMort", "kCalTotMort")

PS.TermExcl_Removals <- as.data.frame(array(NA, c(0,6)))
colnames(PS.TermExcl_Removals) <- c("TimeStep", "Age", "TotMort", "kCalTotMort")

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

Cohort78_1 <- subset(Cohort78_1, RunID %in% RunIDs[,2])
Cohort78_1$RunID <- paste(RunName1, "_", Cohort78_1$RunID, sep = "")
Cohort78_2 <- subset(Cohort78_2, RunID %in% RunIDs[,3])
Cohort78_2$RunID <- paste(RunName2, "_", Cohort78_2$RunID, sep = "")
Mort78_1 <- subset(Mort78_1, RunID %in% RunIDs[,2])
Mort78_1$RunID <- paste(RunName1, "_", Mort78_1$RunID, sep = "")
Mort78_2 <- subset(Mort78_2, RunID %in% RunIDs[,3])
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

# Filter data to Time Steps 1-3 and remove unneccessary fields
Cohort78 <- Cohort78[Cohort78$TimeStep < 4, c(2:5,8:10)]
Mort78 <- Mort78[Mort78$TimeStep < 4, c(2:10,12:15)]

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
    Cohort <- summaryBy(Cohort~RunID+Stock+Age+TimeStep, data = Cohort78, FUN = sum)
}
if(AbundType == 1) {
    Cohort <- summaryBy(MidCohort~RunID+Stock+Age+TimeStep, data = Cohort78, FUN = sum)
}
colnames(Cohort)[5] <- "Cohort"
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
        
        # Filter data to correct RunID
        cohort <- Cohort[Cohort$RunID == runID, ]
        mort <- Mort[Mort$RunID == runID, ]
        
        ##########################################################
        # Calculate abundance and kCal of available Chinook prey #
        ##########################################################
        # Merge with ppnInland and kCal_Age
        cohort <- merge(cohort, StkDist)
        cohort <- merge(cohort, kCal_Age)
        
        # Calculate abundance in inland and coastal waters
        cohort$InlandAbundance <- cohort$Cohort * cohort$ppnInland
        cohort$CoastalAbundance <- cohort$Cohort * cohort$ppnCoastal
        
        # Calculate kCal in inland and coastal waters
        cohort$InlandkCal <- cohort$InlandAbundance * cohort$kCal_Selectivity
        cohort$CoastalkCal <- cohort$CoastalAbundance * cohort$kCal_Selectivity
        
        # Sum by time step and age (over all stocks)
        cohortSummary <- summaryBy(InlandAbundance+InlandkCal+CoastalAbundance+CoastalkCal~TimeStep+Age, 
                                   data = cohort, FUN = sum)
        cohortSummary$InlandkCal.sum <- as.numeric(format(cohortSummary$InlandkCal.sum,
                                                          scientific = FALSE))
        cohortSummary$InlandkCal.sum <- round(cohortSummary$InlandkCal.sum, 0)
        colnames(cohortSummary)[3:6] <- c("Inland_Abundance", "Inland_kCal",
                                          "Coastal_Abundance","Coastal_kCal")
        
        # Add Year and RunType
        cohortSummary$Year <- c(rep(i, dim(cohortSummary)[1]))
        cohortSummary$Run <- c(rep(runType, dim(cohortSummary)[1]))
        cohortSummary <- cohortSummary[ , c(7,8,1:6)]
        
        # Sum by time step (over ages 3-5)
        kCal_TS <- summaryBy(Inland_kCal+Coastal_kCal~TimeStep, 
                             data = cohortSummary[cohortSummary$Age > 2, ], FUN = sum)
        colnames(kCal_TS)[2:3] <- c("Inland","Coastal")
        
        kCal_TS <- reshape(kCal_TS, direction = "long", varying = list(names(kCal_TS)[2:3]),
                           v.names = "kCal", idvar = "TimeStep", timevar = "Region",
                           times = c("Inland", "Coastal"))
        
        kCal_TS <- merge(kCal_TS, Needs)
        kCal_TS$Min_DPER_Avg <- round(kCal_TS$kCal / kCal_TS$MinPER_Avg, 2)
        kCal_TS$Max_DPER_Avg <- round(kCal_TS$kCal / kCal_TS$MaxPER_Avg, 2)
        kCal_TS$Min_DPER_Max <- round(kCal_TS$kCal / kCal_TS$MinPER_Max, 2)
        kCal_TS$Max_DPER_Max <- round(kCal_TS$kCal / kCal_TS$MaxPER_Max, 2)
        kCal_TS$Year <- c(rep(i, dim(kCal_TS)[1]))
        kCal_TS$Run <- c(rep(runType, dim(kCal_TS)[1]))
        
        kCal_TS <- kCal_TS[order(kCal_TS$Region,kCal_TS$TimeStep), c(14:15,2,1,10:13)]
        
        # Append to main summary files
        AvailablePrey <- rbind(AvailablePrey, cohortSummary)
        kCal_to_Need <- rbind(kCal_to_Need, kCal_TS)
        
        ########################################################################
        # Calculate Chinook and kCals removed by PS fisheries in 'Likely' runs #
        ########################################################################
        if(j==2) {
            # Merge with kCal_Age and FishFlag
            mort <- merge(mort, FishFlag)
            mort <- merge(mort, kCal_Age)
            
            # Discount or exclude morts in PS terminal fisheries where SRKWs have not 
            # been or have rarely been observed
            mort$TotMort.TermExcl <- mort$TotMort.sum * mort$Weight
            
            # Calculate kCal of mortalities
            mort$kCalTotMort <- mort$TotMort.sum * mort$kCal_Selectivity
            mort$kCalTotMort.TermExcl <- mort$TotMort.TermExcl * mort$kCal_Selectivity
            
            # # Summarize mortalities by stock-age-timestep
            # PSMort_SAT <- summaryBy(TotMort.sum+kCalTotMort~Stock+Age+TimeStep, 
            #                         data = mort[mort$Flag >= 1,], FUN = sum)
            # PSMort_TermExcl_SAT <- summaryBy(TotMort.TermExcl+kCalTotMort.TermExcl~Stock+Age+TimeStep,
            #                                  data = mort[mort$Flag >= 1,], FUN = sum)
            # PSMort_TermOnly_SAT <- summaryBy(TotMort.TermExcl+kCalTotMort.TermExcl~Stock+Age+TimeStep,
            #                                  data = mort[mort$Flag == 2,], FUN = sum)
            
            # Summarize mortalities by age-timestep
            # PSMort_AT <- summaryBy(TotMort.sum+kCalTotMort~TimeStep+Age, 
            #                        data = mort[mort$Flag >= 1,], FUN = sum)
            # PSMort_TermExcl_AT <- summaryBy(TotMort.TermExcl+kCalTotMort.TermExcl~TimeStep+Age,
            #                                 data = mort[mort$Flag >= 1,], FUN = sum)
            PSMort_TermOnly_AT <- summaryBy(TotMort.TermExcl+kCalTotMort.TermExcl~TimeStep+Age,
                                            data = mort[mort$Flag == 2,], FUN = sum)
            
            # A little formatting (column names, get rid of sci. notation, rounding, etc...)
            # colnames(PSMort_AT)[c(3:4)] <- c("TotMort", "kCalTotMort")
            # colnames(PSMort_TermExcl_AT)[c(3:4)] <- c("TotMort","kCalTotMort")
            colnames(PSMort_TermOnly_AT)[c(3:4)] <- c("TotMort","kCalTotMort")
            # PSMort_AT$kCalTotMort <- as.numeric(format(PSMort_AT$kCalTotMort, 
            #                                            scientific = FALSE))
            # PSMort_TermExcl_AT$kCalTotMort <- as.numeric(format(PSMort_TermExcl_AT$kCalTotMort,
            #                                                     scientific = FALSE))
            PSMort_TermOnly_AT$kCalTotMort <- as.numeric(format(PSMort_TermOnly_AT$kCalTotMort,
                                                                scientific = FALSE))
            # PSMort_AT$TotMort <- round(PSMort_AT$TotMort, 0)
            # PSMort_TermExcl_AT$TotMort <- round(PSMort_TermExcl_AT$TotMort, 0)
            PSMort_TermOnly_AT$TotMort <- round(PSMort_TermOnly_AT$TotMort, 0)
            # PSMort_AT$kCalTotMort <- round(PSMort_AT$kCalTotMort, 0)
            # PSMort_TermExcl_AT$kCalTotMort <- round(PSMort_TermExcl_AT$kCalTotMort, 0)
            PSMort_TermOnly_AT$kCalTotMort <- round(PSMort_TermOnly_AT$kCalTotMort, 0)
            
            # Add 'Year' and 'Run' fields
            # PSMort_AT$Year <- c(rep(i, dim(PSMort_AT)[1]))
            # PSMort_TermExcl_AT$Year <- c(rep(i, dim(PSMort_TermExcl_AT)[1]))
            PSMort_TermOnly_AT$Year <- c(rep(i, dim(PSMort_TermOnly_AT)[1]))
            # PSMort_AT$Run <- c(rep(runType, dim(PSMort_AT)[1]))
            # PSMort_TermExcl_AT$Run <- c(rep(runType, dim(PSMort_TermExcl_AT)[1]))
            PSMort_TermOnly_AT$Run <- c(rep(runType, dim(PSMort_TermOnly_AT)[1]))
            
            # Reorder and remove unnecessary columns
            # PSMort_AT <- PSMort_AT[order(PSMort_AT$TimeStep,PSMort_AT$Age), c(5,6,1:4)]
            # PSMort_TermExcl_AT <- PSMort_TermExcl_AT[order(PSMort_TermExcl_AT$TimeStep,
            #                                                PSMort_TermExcl_AT$Age), c(5,6,1:4)]
            PSMort_TermOnly_AT <- PSMort_TermOnly_AT[PSMort_TermOnly_AT$TimeStep == 3,
                                                     c(5,6,1:4)]
            
            # Append to main output files
            # PS_Removals <- rbind(PS_Removals, PSMort_AT)
            # PS.TermExcl_Removals <- rbind(PS.TermExcl_Removals, PSMort_TermExcl_AT)
            PS.TermOnly_Removals <- rbind(PS.TermOnly_Removals, PSMort_TermOnly_AT)
        }
    }
}

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
SummaryAge3to5Chin_Coastal <- summaryBy(Coastal_Abundance~Year+Run+TimeStep,
                                        data = AvailablePrey[AvailablePrey$Age > 2, ],
                                        FUN = sum)
SummaryAge3to5Chin_Coastal <- reshape(SummaryAge3to5Chin_Coastal, idvar = c("Year","Run"), 
                                      timevar = "TimeStep", direction = "wide")
SummaryAge3to5Chin_Coastal <- reshape(SummaryAge3to5Chin_Coastal, idvar = "Year", 
                                      timevar = "Run", direction = "wide")
colnames(SummaryAge3to5Chin_Coastal)[2:7] <- ColumnNames

# Summarize age 3-5 coastal kCals
SummaryKilos_Coastal <- summaryBy(Coastal_kCal~Year+Run+TimeStep, 
                          data = AvailablePrey[AvailablePrey$Age > 2, ], FUN = sum)
SummaryKilos_Coastal <- reshape(SummaryKilos_Coastal, idvar = c("Year","Run"), 
                                timevar = "TimeStep", direction = "wide")
SummaryKilos_Coastal <- reshape(SummaryKilos_Coastal, idvar = "Year", timevar = "Run",
                                direction = "wide")
colnames(SummaryKilos_Coastal)[2:7] <- ColumnNames


# Summarize age 3-5 inland abundances
SummaryAge3to5Chin <- summaryBy(Inland_Abundance~Year+Run+TimeStep,
                                data = AvailablePrey[AvailablePrey$Age > 2, ], FUN = sum)
SummaryAge3to5Chin <- reshape(SummaryAge3to5Chin, idvar = c("Year","Run"), 
                              timevar = "TimeStep", direction = "wide")
SummaryAge3to5Chin <- reshape(SummaryAge3to5Chin, idvar = "Year", timevar = "Run", 
                              direction = "wide")
colnames(SummaryAge3to5Chin)[2:7] <- ColumnNames

# Summarize age 3-5 inland kCals
SummaryKilos <- summaryBy(Inland_kCal~Year+Run+TimeStep, 
                          data = AvailablePrey[AvailablePrey$Age > 2, ], FUN = sum)
SummaryKilos <- reshape(SummaryKilos, idvar = c("Year","Run"), timevar = "TimeStep",
                        direction = "wide")
SummaryKilos <- reshape(SummaryKilos, idvar = "Year", timevar = "Run", 
                        direction = "wide")
colnames(SummaryKilos)[2:7] <- ColumnNames

# Calculate likely after terminal in time step 3 and add to above 2 Puget Sound tables
PS_Term_Mort_T3 <- summaryBy(TotMort+kCalTotMort~Year+Run+TimeStep,
                             data = PS.TermOnly_Removals[PS.TermOnly_Removals$Age > 2, ],
                             FUN = sum)
SummaryAge3to5Chin$Likely_AfterTerm_T3 <- SummaryAge3to5Chin[ ,4] - PS_Term_Mort_T3$TotMort.sum
colnames(SummaryAge3to5Chin)[8] <- paste(RunName1, "_AfterTerm_T3", sep = "")
SummaryAge3to5Chin <- SummaryAge3to5Chin[ ,c(1:4,8,5:7)]
SummaryKilos$Likely_AfterTerm_T3 <- SummaryKilos[ ,4] - PS_Term_Mort_T3$kCalTotMort.sum
colnames(SummaryKilos)[8] <- paste(RunName1, "_AfterTerm_T3", sep = "")
SummaryKilos <- SummaryKilos[ ,c(1:4,8,5:7)]

# Summarize Inland Needs table
SummaryNeeds_Inland <- kCal_to_Need[kCal_to_Need$Region == "Inland" ,c(3:4,1:2,5:8)]
SummaryNeeds_Inland <- reshape(SummaryNeeds_Inland, idvar = c("TimeStep", "Year"), 
                               timevar = "Run", direction = "wide")
SummaryNeeds_Inland <- SummaryNeeds_Inland[order(SummaryNeeds_Inland$TimeStep), 
                                           c(1:3,9:12,4:7)]
colnames(SummaryNeeds_Inland)[3:11] <- c("Region", 
                                         paste(RunName2, "_MinDPER_Avg", sep = ""),
                                         paste(RunName2, "_MaxDPER_Avg", sep = ""),
                                         paste(RunName2, "_MinDPER_Max", sep = ""),
                                         paste(RunName2, "_MaxDPER_Max", sep = ""),
                                         paste(RunName1, "_MinDPER_Avg", sep = ""),
                                         paste(RunName1, "_MaxDPER_Avg", sep = ""),
                                         paste(RunName1, "_MinDPER_Max", sep = ""),
                                         paste(RunName1, "_MaxDPER_Max", sep = ""))

# Summarize Coastal Needs table
SummaryNeeds_Coastal <- kCal_to_Need[kCal_to_Need$Region == "Coastal" ,c(3:4,1:2,5:8)]
SummaryNeeds_Coastal <- reshape(SummaryNeeds_Coastal, idvar = c("TimeStep", "Year"), 
                               timevar = "Run", direction = "wide")
SummaryNeeds_Coastal <- SummaryNeeds_Coastal[order(SummaryNeeds_Coastal$TimeStep), 
                                           c(1:3,9:12,4:7)]
colnames(SummaryNeeds_Coastal)[3:11] <- c("Region", 
                                          paste(RunName2, "_MinDPER_Avg", sep = ""),
                                          paste(RunName2, "_MaxDPER_Avg", sep = ""),
                                          paste(RunName2, "_MinDPER_Max", sep = ""),
                                          paste(RunName2, "_MaxDPER_Max", sep = ""),
                                          paste(RunName1, "_MinDPER_Avg", sep = ""),
                                          paste(RunName1, "_MaxDPER_Avg", sep = ""),
                                          paste(RunName1, "_MinDPER_Max", sep = ""),
                                          paste(RunName1, "_MaxDPER_Max", sep = ""))

# Summarize FisheryRedux table
SummaryFisheryRedux <- as.data.frame(SummaryKilos[ ,1])
SummaryFisheryRedux$Oct_Apr <- (SummaryKilos[ ,2] - SummaryKilos[ ,6]) / SummaryKilos[ ,6]
SummaryFisheryRedux$May_Jun <- (SummaryKilos[ ,3] - SummaryKilos[ ,7]) / SummaryKilos[ ,7]
SummaryFisheryRedux$Jul_Sep <- (SummaryKilos[ ,5] - SummaryKilos[ ,8]) / SummaryKilos[ ,8]
colnames(SummaryFisheryRedux)[1] <- c("Year")

# Export Summary Tables
write.csv(SummaryAge3to5Chin_Coastal, paste(sep="", outfile, "SummaryAge3to5Chinook_Coastal.csv"))
write.csv(SummaryKilos_Coastal, paste(sep="", outfile, "SummaryKilos_Coastal.csv"))
write.csv(SummaryAge3to5Chin, paste(sep="", outfile, "SummaryAge3to5Chinook_Inland.csv"))
write.csv(SummaryKilos, paste(sep="", outfile, "SummaryKilos_Inland.csv"))
write.csv(SummaryNeeds_Inland, paste(sep="", outfile, "SummaryNeeds_Inland.csv"))
write.csv(SummaryNeeds_Coastal, paste(sep="", outfile, "SummaryNeeds_Coastal.csv"))
write.csv(SummaryFisheryRedux, paste(sep="", outfile, "SummaryFisheryRedux_Inland.csv"))

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

ggsave(paste(outfile,"AbundanceCharts_Inland.jpg",sep=""),p,height=5,width=7.5)


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

ggsave(paste(outfile,"AbundanceCharts_Coastal.jpg",sep=""),p,height=5,width=7.5)


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

ggsave(paste(outfile,"KilocalorieCharts_Inland.jpg",sep=""),p,height=5,width=7.5)


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

ggsave(paste(outfile,"KilocalorieCharts_Coastal.jpg",sep=""),p,height=5,width=7.5)




nd <- Sys.time()
tm <- nd - strt
tm