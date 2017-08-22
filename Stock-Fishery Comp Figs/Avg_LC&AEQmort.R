#####################################################################################
# This program compiles average landed catch and total AEQ mortality by
# stock-age-fishery-timestep from Old and New base period validation runs
# for a defined range of years (identified below by 'StartYr' and 'EndYr').
# It requires two input files:
#   1. A new base period validation run database (Item 1 in the 'paths' list below)
#   2. An old base period validation run database (Item 2 in the 'paths' list below)
# The third element of the 'paths' list is the directory in which the output files 
# will be saved.
#####################################################################################

# Clear workspace
rm(list=ls(all=TRUE))

# Load required libraries
library(RODBC)
library(doBy)

# set start time for purposes of timing code
strt <- Sys.time()

# Set the paths 
paths = list("C:\\data\\FRAM\\Base Period\\Validation\\Round 5\\Working\\Valid2016_NewBP_Round5_Iter3.mdb",
             "C:\\data\\FRAM\\Base Period\\Validation\\Round 5\\Working\\Valid2016_OldBP_8.16.2017_USE_ME.mdb",
             "C:\\data\\FRAM\\Base Period\\Validation\\Round 5\\Working\\ProfileFigures\\")
# Set the input file path for the database
newDB = paths[[1]]
oldDB = paths[[2]]
outfile = paths[[3]]

StartYr = 2007
EndYr = 2013

# Pull RunID tables from OldBP and NewBP databases
con = odbcConnectAccess(oldDB)
runID_old = sqlQuery(con, as.is = TRUE, 
                 paste(sep = '',
                       "SELECT * FROM RunID"))
close(con)

con = odbcConnectAccess(newDB)
runID_new = sqlQuery(con, as.is = TRUE, 
                 paste(sep = '',
                       "SELECT * FROM RunID"))
close(con)

# Identify old and new base period RunIDs for desired years
oldBP_runIDs <- na.omit(runID_old[runID_old$RunYear >= StartYr & runID_old$RunYear <= EndYr, 2])
newBP_runIDs <- na.omit(runID_new[runID_new$RunYear >= StartYr & runID_new$RunYear <= EndYr, 2])

# Identify old and new base period RunIDs for desired years
oldBP_baseID <- unique(runID_old$BasePeriodID)
newBP_baseID <- unique(runID_new$BasePeriodID)

# Pull necessary data from databases for identified RunIDs
con = odbcConnectAccess(oldDB)
oldBP_mort = sqlQuery(con, as.is = TRUE, 
                      paste(sep = '',
                            "SELECT Mortality.* ",
                            "FROM Mortality ",
                            "WHERE (((Mortality.RunID) Between ", min(oldBP_runIDs), " And ", max(oldBP_runIDs),"));"))

oldBP_AEQ = sqlQuery(con, as.is = TRUE, 
                     paste(sep = '',
                           "SELECT AEQ.* ",
                           "FROM AEQ ",
                           "WHERE (((AEQ.BasePeriodID) = ", oldBP_baseID, "));"))

oldBP_termID = sqlQuery(con, as.is = TRUE, 
                        paste(sep = '',
                              "SELECT TerminalFisheryFlag.* ",
                              "FROM TerminalFisheryFlag ",
                              "WHERE (((TerminalFisheryFlag.BasePeriodID) = ", oldBP_baseID, "));"))

oldBP_ShakerMortRate = sqlQuery(con, as.is = TRUE, 
                     paste(sep = '',
                           "SELECT ShakerMortRate.* ",
                           "FROM ShakerMortRate ",
                           "WHERE (((ShakerMortRate.BasePeriodID) = ", oldBP_baseID, "));"))
close(con)

con = odbcConnectAccess(newDB)
newBP_mort = sqlQuery(con, as.is = TRUE, 
                      paste(sep = '',
                            "SELECT Mortality.* ",
                            "FROM Mortality ",
                            "WHERE (((Mortality.RunID) Between ", min(newBP_runIDs), " And ", max(newBP_runIDs),"));"))

newBP_AEQ = sqlQuery(con, as.is = TRUE, 
                     paste(sep = '',
                           "SELECT AEQ.* ",
                           "FROM AEQ ",
                           "WHERE (((AEQ.BasePeriodID) = ", newBP_baseID, "));"))

newBP_termID = sqlQuery(con, as.is = TRUE, 
                        paste(sep = '',
                              "SELECT TerminalFisheryFlag.* ",
                              "FROM TerminalFisheryFlag ",
                              "WHERE (((TerminalFisheryFlag.BasePeriodID) = ", newBP_baseID, "));"))

newBP_ShakerMortRate = sqlQuery(con, as.is = TRUE, 
                                paste(sep = '',
                                      "SELECT ShakerMortRate.* ",
                                      "FROM ShakerMortRate ",
                                      "WHERE (((ShakerMortRate.BasePeriodID) = ", newBP_baseID, "));"))

StockID = sqlQuery(con, as.is = TRUE, 
                   paste(sep = '',
                         "SELECT * ",
                         "FROM Stock"))

FisheryID = sqlQuery(con, as.is = TRUE, 
                   paste(sep = '',
                         "SELECT * ",
                         "FROM Fishery"))
close(con)

# Sum landed catch and MSF landed catch to produce a total landed catch field
oldBP_mort$TotLanded <- rowSums(oldBP_mort[ ,c(7,12)])
newBP_mort$TotLanded <- rowSums(newBP_mort[ ,c(7,12)])

# Sum all mortality to produce a total mortality field
oldBP_mort$TotMort <- rowSums(oldBP_mort[ ,c(7:10,12:15)])
newBP_mort$TotMort <- rowSums(newBP_mort[ ,c(7:10,12:15)])

# calculate sublegal and legal encounters
oldBP_mort$LegEnc <- oldBP_mort$LandedCatch + oldBP_mort$MSFLandedCatch + (oldBP_mort$MSFNonRetention / 0.10)
oldBP_ShakerMortRate <- oldBP_ShakerMortRate[ ,c(2:4)]
oldBP_mort <- merge(oldBP_mort, oldBP_ShakerMortRate)
oldBP_mort$SubMort <- rowSums(oldBP_mort[ ,c(9,14)])
oldBP_mort$SubEnc <- oldBP_mort$SubMort / oldBP_mort$ShakerMortRate

newBP_mort$LegEnc <- newBP_mort$LandedCatch + newBP_mort$MSFLandedCatch + (newBP_mort$MSFNonRetention / 0.10)
newBP_ShakerMortRate <- newBP_ShakerMortRate[ ,c(2:4)]
newBP_mort <- merge(newBP_mort, newBP_ShakerMortRate)
newBP_mort$SubMort <- rowSums(newBP_mort[ ,c(9,14)])
newBP_mort$SubEnc <- newBP_mort$SubMort / newBP_mort$ShakerMortRate

# Merge mort table with terminal fishery flags and AEQs
oldBP_mort <- merge(oldBP_mort, oldBP_termID[ c(2:4)], all.x = TRUE)
oldBP_mort$TerminalFlag[is.na(oldBP_mort$TerminalFlag)] <- 0
oldBP_mort <- merge(oldBP_mort, oldBP_AEQ[ ,c(2:5)], all.x = TRUE)

newBP_mort <- merge(newBP_mort, newBP_termID[ ,c(2:4)], all.x = TRUE)
newBP_mort$TerminalFlag[is.na(newBP_mort$TerminalFlag)] <- 0
newBP_mort <- merge(newBP_mort, newBP_AEQ[ ,c(2:5)], all.x = TRUE)

# compute AEQ total mortality
i=1
for(i in 1:dim(oldBP_mort)[1]) {
    if(oldBP_mort$TerminalFlag[i] == 0) {
        oldBP_mort$AEQmort[i] <- oldBP_mort$TotMort[i] * oldBP_mort$AEQ[i]
    }
    
    if(oldBP_mort$TerminalFlag[i] == 1) {
        oldBP_mort$AEQmort[i] <- oldBP_mort$TotMort[i]
    }
}

i=1
for(i in 1:dim(newBP_mort)[1]) {
    if(newBP_mort$TerminalFlag[i] == 0) {
        newBP_mort$AEQmort[i] <- newBP_mort$TotMort[i] * newBP_mort$AEQ[i]
    }
    
    if(newBP_mort$TerminalFlag[i] == 1) {
        newBP_mort$AEQmort[i] <- newBP_mort$TotMort[i]
    }
}

# convert from 78 to 39 stock format
oldBP_mort$stk <- ceiling(oldBP_mort$StockID/2)
newBP_mort$stk <- ceiling(newBP_mort$StockID/2)

# Sum values by stock in 39 stock format
oldBP_summary <- summaryBy(LegEnc+SubEnc+TotLanded+AEQmort~RunID+stk+Age+FisheryID+TimeStep, 
                           data = oldBP_mort, FUN = sum)
newBP_summary <- summaryBy(LegEnc+SubEnc+TotLanded+AEQmort~RunID+stk+Age+FisheryID+TimeStep, 
                           data = newBP_mort, FUN = sum)

# for each Stk-Age-Fish-TS compute averages over all years (RunIDs)
oldBP_summary_avg <- summaryBy(LegEnc.sum+SubEnc.sum+TotLanded.sum+AEQmort.sum~stk+Age+FisheryID+TimeStep, 
                               data = oldBP_summary, FUN = mean)
newBP_summary_avg <- summaryBy(LegEnc.sum+SubEnc.sum+TotLanded.sum+AEQmort.sum~stk+Age+FisheryID+TimeStep, 
                               data = newBP_summary, FUN = mean)

# Add a field to identify new and old base periods
oldID <- rep("OldBP", dim(oldBP_summary_avg)[1])
newID <- rep("NewBP", dim(newBP_summary_avg)[1])

oldBP_summary_avg <- cbind(oldID, oldBP_summary_avg)
newBP_summary_avg <- cbind(newID, newBP_summary_avg)

# Rename fields so old and new tables match
nms <- c("Source", "Stk", "Age", "Fish", "TS", "LegEnc", "SubEnc", "TotLanded", "AEQmort")
colnames(oldBP_summary_avg) <- nms
colnames(newBP_summary_avg) <- nms

# Combine old and new
summary_avg <- rbind(oldBP_summary_avg, newBP_summary_avg)

# Create output files
LC_avg <- summary_avg[ ,c(1:5,8)]
colnames(LC_avg)[6] <- "CWT"
AEQmort_avg <- summary_avg[ ,c(1:5,9)]
colnames(AEQmort_avg)[6] <- "CWT"

# export CSVs
write.csv(LC_avg, paste(sep='', outfile, StartYr, "-", substr(EndYr,3,4), "_LC_Avg.csv"))
write.csv(AEQmort_avg, paste(sep='', outfile, StartYr, "-", substr(EndYr,3,4), "_AEQmort_Avg.csv"))
write.csv(summary_avg, paste(sep='', outfile, StartYr, "-", substr(EndYr,3,4), "_summary_Avg.csv"))
# write.csv(StockID, paste(sep='', outfile, "StockID.csv"))
# write.csv(FisheryID, paste(sep='', outfile, "FisheryID.csv"))

nd <- Sys.time()
tm <- nd - strt
tm
