


# Clear workspace
rm(list=ls(all=TRUE))

# set start time for purposes of timing code
strt <- Sys.time()

# Load required libraries
library(RODBC)
library(readxl)
library(doBy)

# Round mortalities? Prob only necessary for validating calcs with existing spreadsheets
RoundFlag = 1 # 0=No, 1=Yes

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

# Import list of RunIDs
RunIDs <- read_excel(paths[[1]], "R_In_RunID")

# Identify range of years for analysis
minYr <- min(RunIDs$Year)
maxYr <- max(RunIDs$Year)

# Pull Cohort & Mortality tables from FRAM database
con = odbcConnectAccess(DBpath)
Cohort78 = sqlQuery(con, as.is = TRUE, 
                    paste(sep = '',
                          "SELECT * FROM Cohort"))
Mort78 = sqlQuery(con, as.is = TRUE, 
                  paste(sep = '',
                        "SELECT * FROM Mortality"))
close(con)

# Create empty data frame output files
AvailablePrey <- as.data.frame(array(NA, c(0,6)))
colnames(AvailablePrey) <- c("Year", "Run", "TimeStep", "Age", "Inland_Abundance", "Inland_kCal")

kCal_to_Need <- as.data.frame(array(NA, c(0,6)))
colnames(kCal_to_Need) <- c("Year", "Run", "DietComp", "TimeStep", "Min_DPER", "Max_DPER")

PS_Removals <- as.data.frame(array(NA, c(0,6)))
colnames(PS_Removals) <- c("TimeStep", "Age", "TotMort", "kCalTotMort")

PS.TermExcl_Removals <- as.data.frame(array(NA, c(0,6)))
colnames(PS_Removals) <- c("TimeStep", "Age", "TotMort", "kCalTotMort")

############################################################
# A little pre-processing on the cohort and martality data #
############################################################

# Round cohort sizes to nearest integer (for testing only, as PopStat is rounded to integers)
if(RoundFlag == 1) {
    Cohort78$MidCohort <- round(Cohort78$MidCohort,0)
}

# Filter data to Time Steps 1-3 and remove unneccessary fields
Cohort78 <- Cohort78[Cohort78$TimeStep < 4, c(2:5,10)]
Mort78 <- Mort78[Mort78$TimeStep < 4, c(2:10,12:15)]

# Convert from 78 stock format to 39 stock format
Cohort78$Stock <- ceiling(Cohort78$StockID/2)
Mort78$Stock <- ceiling(Mort78$StockID/2)

# Calculate total mortality by S-A-F-T
ifelse(RoundFlag == 1, Mort78$TotMort <- round(rowSums(Mort78[ ,c(6:13)]),0),
       Mort78$TotMort <- rowSums(Mort78[ ,c(6:13)]))
Mort78 <- Mort78[ ,c(1:5,15,14)]

# Combine Marked and Unmarked components of each stock
Cohort <- summaryBy(MidCohort~RunID+Stock+Age+TimeStep, data = Cohort78, FUN = sum)
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
        runID <- RunIDs[RunIDs$Year == i, j]
        
        # Identify Run Type
        if(j == 2) {
            runType <- "Likely"
        }
        if(j == 3) {
            runType <- "No Action"
        }
        
        # Filter data to correct RunID
        cohort <- Cohort[Cohort$RunID == runID, ]
        mort <- Mort[Mort$RunID == runID, ]
        
        ##########################################################
        # Calculate abundance and kCal of available Chinook prey #
        ##########################################################
        # Merge with ppnInland and kCal_Age
        cohort <- merge(cohort, ppnInland)
        cohort <- merge(cohort, kCal_Age)
        
        # Calculate abundance in inland waters
        cohort$InlandAbundance <- cohort$MidCohort.sum * cohort$Proportion
        
        # Calculate kCal in inland waters
        cohort$InlandkCal <- cohort$InlandAbundance * cohort$kCal_Selectivity
        
        # Sum by time step and age
        cohortSummary <- summaryBy(InlandAbundance+InlandkCal~TimeStep+Age, 
                                   data = cohort, FUN = sum)
        cohortSummary$InlandkCal.sum <- as.numeric(format(cohortSummary$InlandkCal.sum,
                                                          scientific = FALSE))
        cohortSummary$InlandkCal.sum <- round(cohortSummary$InlandkCal.sum, 0)
        colnames(cohortSummary)[3] <- "Inland_Abundance"
        colnames(cohortSummary)[4] <- "Inland_kCal"
        
        # Add Year and RunType
        cohortSummary$Year <- c(rep(i, dim(cohortSummary)[1]))
        cohortSummary$Run <- c(rep(runType, dim(cohortSummary)[1]))
        cohortSummary <- cohortSummary[ , c(5,6,1:4)]
        
        kCal_TS <- summaryBy(Inland_kCal~TimeStep, data = cohortSummary, FUN = sum)
        colnames(kCal_TS)[2] <- "InlandkCal"
        
        kCal_TS <- merge(kCal_TS, Needs)
        kCal_TS$Min_DPER <- round(kCal_TS$InlandkCal / kCal_TS$MinPER, 2)
        kCal_TS$Max_DPER <- round(kCal_TS$InlandkCal / kCal_TS$MaxPER, 2)
        kCal_TS$Year <- c(rep(i, dim(kCal_TS)[1]))
        kCal_TS$Run <- c(rep(runType, dim(kCal_TS)[1]))
        
        kCal_TS <- kCal_TS[order(-kCal_TS$DietComp,kCal_TS$TimeStep), c(8,9,3,1,6,7)]
        
        # Append to main putput files
        AvailablePrey <- rbind(AvailablePrey, cohortSummary)
        kCal_to_Need <- rbind(kCal_to_Need, kCal_TS)
        
        #################################################################
        # Calculate number of Chinook and kCals removed by PS fisheries #
        #################################################################
        # Merge with kCal_Age and FishFlag
        mort <- merge(mort, FishFlag)
        mort <- merge(mort, kCal_Age)
        
        # Discount and exclude morts in PS terminal fisheries where SRKWs have not been or
            # have rarely been observed
        mort$TotMort.TermExcl <- mort$TotMort.sum * mort$Weight

        # Calculate kCal of mrtalities
        mort$kCalTotMort <- mort$TotMort.sum * mort$kCal_Selectivity
        mort$kCalTotMort.TermExcl <- mort$TotMort.TermExcl * mort$kCal_Selectivity
        

        PSMort_SAT <- summaryBy(TotMort.sum+kCalTotMort~Stock+Age+TimeStep, 
                                data = mort[mort$Flag == 1,], FUN = sum)
        PSMort_TermExcl_SAT <- summaryBy(TotMort.TermExcl+kCalTotMort.TermExcl~Stock+Age+TimeStep,
                                         data = mort[mort$Flag == 1,], FUN = sum)
        PSMort_AT <- summaryBy(TotMort.sum+kCalTotMort~TimeStep+Age, 
                               data = mort[mort$Flag == 1,], FUN = sum)
        PSMort_TermExcl_AT <- summaryBy(TotMort.TermExcl+kCalTotMort.TermExcl~TimeStep+Age,
                                         data = mort[mort$Flag == 1,], FUN = sum)
        colnames(PSMort_AT)[c(3:4)] <- c("TotMort", "kCalTotMort")
        colnames(PSMort_TermExcl_AT)[c(3:4)] <- c("TotMort","kCalTotMort")
        PSMort_AT$kCalTotMort <- as.numeric(format(PSMort_AT$kCalTotMort, scientific = FALSE))
        PSMort_TermExcl_AT$kCalTotMort <- as.numeric(format(PSMort_TermExcl_AT$kCalTotMort,
                                                            scientific = FALSE))
        PSMort_AT$TotMort <- round(PSMort_AT$TotMort, 0)
        PSMort_TermExcl_AT$TotMort <- round(PSMort_TermExcl_AT$TotMort, 0)
        PSMort_AT$kCalTotMort <- round(PSMort_AT$kCalTotMort, 0)
        PSMort_TermExcl_AT$kCalTotMort <- round(PSMort_TermExcl_AT$kCalTotMort, 0)
        
        PSMort_AT$Year <- c(rep(i, dim(PSMort_AT)[1]))
        PSMort_TermExcl_AT$Year <- c(rep(i, dim(PSMort_TermExcl_AT)[1]))
        PSMort_AT$Run <- c(rep(runType, dim(PSMort_AT)[1]))
        PSMort_TermExcl_AT$Run <- c(rep(runType, dim(PSMort_TermExcl_AT)[1]))
        
        PSMort_AT <- PSMort_AT[order(-PSMort_AT$TimeStep,PSMort_AT$Age), c(5,6,1:4)]
        PSMort_TermExcl_AT <- PSMort_TermExcl_AT[order(-PSMort_TermExcl_AT$TimeStep,
                                                       PSMort_TermExcl_AT$Age), c(5,6,1:4)]
        
        # Append to main putput files
        PS_Removals <- rbind(PS_Removals, PSMort_AT)
        PS.TermExcl_Removals <- rbind(PS.TermExcl_Removals, PSMort_TermExcl_AT)
    }
}

nd <- Sys.time()
tm <- nd - strt
tm