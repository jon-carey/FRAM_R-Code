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
close(con)

# Sort records by RunID, StockID, Age, FisheryID, TimeStep
oldBP_mort <- oldBP_mort[order(oldBP_mort$RunID, oldBP_mort$StockID, oldBP_mort$Age, oldBP_mort$FisheryID, oldBP_mort$TimeStep),]
newBP_mort <- newBP_mort[order(newBP_mort$RunID, newBP_mort$StockID, newBP_mort$Age, newBP_mort$FisheryID, newBP_mort$TimeStep),]

# Sum landed catch and MSF landed catch to produce a total landed catch field
oldBP_mort$TotLanded <- rowSums(oldBP_mort[ ,c(7,12)])
newBP_mort$TotLanded <- rowSums(newBP_mort[ ,c(7,12)])

# Sum all mortality to produce a total mortality field
oldBP_mort$TotMort <- rowSums(oldBP_mort[ ,c(7:10,12:15)])
newBP_mort$TotMort <- rowSums(newBP_mort[ ,c(7:10,12:15)])

# convert from 78 to 39 stock format
oldBP_mort$stk <- ceiling(oldBP_mort$StockID/2)
newBP_mort$stk <- ceiling(newBP_mort$StockID/2)

# Merge mort table with terminal fishery flags
oldBP_mort <- merge(oldBP_mort, oldBP_termID, all.x = TRUE)
oldBP_mort$TerminalFlag[is.na(oldBP_mort$TerminalFlag)] <- 0
oldBP_mort <- oldBP_mort[ ,c(1:19,21)]

newBP_mort <- merge(newBP_mort, newBP_termID, all.x = TRUE)
newBP_mort$TerminalFlag[is.na(newBP_mort$TerminalFlag)] <- 0
newBP_mort <- newBP_mort[ ,c(1:19,21)]

# Merge mort table with AEQs
oldBP_mort <- merge(oldBP_mort, oldBP_AEQ, all.x = TRUE)
oldBP_mort <- oldBP_mort[ ,c(1:20,22)]

newBP_mort <- merge(newBP_mort, newBP_AEQ, all.x = TRUE)
newBP_mort <- newBP_mort[ ,c(1:20,22)]

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

# sum landed catch by stock from 39 stock format
oldBP_LC_byYr <- summaryBy(TotLanded~RunID+stk+Age+FisheryID+TimeStep, data = oldBP_mort, FUN = sum)
newBP_LC_byYr <- summaryBy(TotLanded~RunID+stk+Age+FisheryID+TimeStep, data = newBP_mort, FUN = sum)

# Sum total mortality by stock from 39 stock format
oldBP_AEQmort_byYr <- summaryBy(AEQmort~RunID+stk+Age+FisheryID+TimeStep, data = oldBP_mort, FUN = sum)
newBP_AEQmort_byYr <- summaryBy(AEQmort~RunID+stk+Age+FisheryID+TimeStep, data = newBP_mort, FUN = sum)

# # ??
# newBP_mort_byYr_tst <- newBP_mort_byYr[newBP_mort_byYr$RunID == 52, ]
# oldBP_mort_byYr_tst <- oldBP_mort_byYr[oldBP_mort_byYr$RunID == 25, ]

# for each Stk-Age-Fish-TS compute average landed catch over all years (RunIDs)
oldBP_LC_avg <-summaryBy(TotLanded.sum~stk+Age+FisheryID+TimeStep, data = oldBP_LC_byYr, FUN = mean)
newBP_LC_avg <-summaryBy(TotLanded.sum~stk+Age+FisheryID+TimeStep, data = newBP_LC_byYr, FUN = mean)

# for each Stk-Age-Fish-TS compute average AEQ mortality over all years (RunIDs)
oldBP_AEQmort_avg <-summaryBy(AEQmort.sum~stk+Age+FisheryID+TimeStep, data = oldBP_AEQmort_byYr, FUN = mean)
newBP_AEQmort_avg <-summaryBy(AEQmort.sum~stk+Age+FisheryID+TimeStep, data = newBP_AEQmort_byYr, FUN = mean)

# # Pull out a test value for confirmation
# tst <- newBP_mort_sum[newBP_mort_sum$stk==1 & newBP_mort_sum$Age==4 & newBP_mort_sum$FisheryID==10 & newBP_mort_sum$TimeStep==3, ]

# Add a field to identify new and old base periods
oldID <- rep("OldBP", dim(oldBP_LC_avg)[1])
newID <- rep("NewBP", dim(newBP_LC_avg)[1])

oldBP_LC_avg <- cbind(oldID, oldBP_LC_avg)
newBP_LC_avg <- cbind(newID, newBP_LC_avg)

oldBP_AEQmort_avg <- cbind(oldID, oldBP_AEQmort_avg)
newBP_AEQmort_avg <- cbind(newID, newBP_AEQmort_avg)

# Rename fields so old and new tables match
nms <- c("Source", "Stk", "Age", "Fish", "TS", "CWT")

colnames(oldBP_LC_avg) <- nms
colnames(newBP_LC_avg) <- nms

colnames(oldBP_AEQmort_avg) <- nms
colnames(newBP_AEQmort_avg) <- nms

# Combine old and new
LC_avg <- rbind(oldBP_LC_avg, newBP_LC_avg)
AEQmort_avg <- rbind(oldBP_AEQmort_avg, newBP_AEQmort_avg)

# export CSVs
write.csv(LC_avg, paste(sep='', outfile, StartYr, "-", substr(EndYr,3,4), "_LC_Avg.csv"))
write.csv(AEQmort_avg, paste(sep='', outfile, StartYr, "-", substr(EndYr,3,4), "_AEQmort_Avg.csv"))


