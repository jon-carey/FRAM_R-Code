##################################################################################
# The purpose of this program is to assists in generating pre-season model run
#   abundances using the average run by updating age 2 recruit scalars for stocks 
#   with age 2 forecasts.  It takes the existing age 2 recruit scalar (which 
#   presumably has been determined using age-2-from-3 methodology) and multiplies
#   it by the ratio of forecast to mature cohort in the existing run.
#
# Required inputs:
#   1. ChinRSScalarsMR...xlsm (path 1)
#   2. FRAM database (path 2)
##################################################################################

# Clear workspace
rm(list=ls(all=TRUE))

library(readxl)
library(RODBC)
library(doBy)

# Set RunID
runID <- 11

# Set paths
Dir <- "C:\\data\\NOF\\2018\\Modeling\\Chinook\\"
paths = list(paste(Dir, "Abundance\\ChinRSScalarsMR18_3.2.18.xlsm", sep=""),
             paste(Dir, "Model Runs\\2018 NOF ChinFRAM - Test.mdb", sep=""))

forecasts <- read_excel(paths[[1]], "Age2forR")
forecasts <- forecasts[!(forecasts$Forecast == "2s3s"), ]

# Pull RunID and BackwardsFRAM tables from Validation Databse
con = odbcConnectAccess(paths[[2]])
Cohort = sqlQuery(con, as.is = TRUE,
                  paste(sep = '', "SELECT * FROM Cohort"))
StockRecruit = sqlQuery(con, as.is = TRUE,
                        paste(sep = '', "SELECT * FROM StockRecruit"))
close(con)

# Trim StockRecruit Table to correct RunID and save a copy
StockRecruit <- StockRecruit[StockRecruit$RunID == runID, ]
StockRecruit_Orig <- StockRecruit

# Trim Cohor Table to correct RunID, Age 2 only, and StockIDs of interest
Cohort <- Cohort[Cohort$RunID == runID & Cohort$Age == 2, ]
Cohort <- Cohort[Cohort$StockID %in% forecasts$StockID, ]

# Summarize Age 2 mature cohorts by stock
Age2Mature <- summaryBy(MatureCohort~StockID, data = Cohort[Cohort$TimeStep %in% c(1:3), ], FUN = sum)
forecasts <- merge(forecasts,Age2Mature, all.x = TRUE)
forecasts$MatureCohort.sum[is.na(forecasts$MatureCohort.sum)] <- 0.01
forecasts$Forecast <- as.numeric(forecasts$Forecast)
forecasts$Adjustment <- forecasts$Forecast / forecasts$MatureCohort.sum


i=1
for(i in 1:dim(forecasts)[1]) {
    stk <- forecasts$StockID[i]
    age <- forecasts$Age[i]
    adj <- forecasts$Adjustment[i]
    OrigScalar <- StockRecruit[StockRecruit$StockID == stk & StockRecruit$Age == age, 5]
    NewScalar <- round(OrigScalar * adj, 4)
    
    StockRecruit[StockRecruit$StockID == stk & StockRecruit$Age == age, 5] <- NewScalar
}

# First delete all records in StockRecruit table for specified RunID
con = odbcConnectAccess(paths[[2]])
sqlQuery(con, as.is = TRUE,
         paste(sep = '',
               "DELETE StockRecruit.RunID ",
               "FROM StockRecruit ",
               "WHERE (((StockRecruit.RunID)=",runID,"))"))

# Next append new records to StockRecruit table for specified RunID
sqlSave(con, StockRecruit, tablename = "StockRecruit", rownames = FALSE,
        append = TRUE)

close(con)


