#######################################################################################
###### CODE TO UPDATE AGE 2 RECRUIT SCALARS USING BROOD YEAR AGE 2 FROM 3 METHOD ######
#######################################################################################
# JC; 8/26/2016
# NOTES:
# 1. Make sure the year and file path to database are set correctly below
# 2. Updated to pull year from new 'RunYear' field in RunID table (8/15/17)


# Clear workspace
rm(list=ls(all=TRUE))

# Load required libraries
library(RODBC)

# set start time for purposes of timing code
strt <- Sys.time()

# Set Year
year = 2013

# Set the paths 
paths = list("C:\\data\\FRAM\\Base Period\\Validation\\Round 5\\Working\\Valid2016_NewBP_Round5_Iter1.mdb")
# Set the input file path for the database
infile = paths[[1]]

#####################################
# Pull necessary data from database #
#####################################

con = odbcConnectAccess(infile)
RunID = sqlQuery(con, as.is = TRUE, 
                   paste(sep = '',
                         "SELECT * FROM RunID"))
Cohort = sqlQuery(con, as.is = TRUE, 
                  paste(sep = '',
                        "SELECT * FROM Cohort"))
StockRecruit = sqlQuery(con, as.is = TRUE, 
                 paste(sep = '',
                       "SELECT * FROM StockRecruit"))
close(con)

# Subset RunID to necessary years
RunID <- subset(RunID,RunYear %in% c(year,year+1))
RunID_y <- RunID[RunID$RunYear == year,2]
RunID_yp1 <- RunID[RunID$RunYear == year+1,2]

# Subeset Cohort and StockRecruit to desired RunIDs
runIDs <- unique(RunID$RunID)
Cohort <- subset(Cohort, RunID %in% runIDs)
StockRecruit <- subset(StockRecruit, RunID %in% runIDs)

i=1
for(i in 1:dim(StockRecruit)[1]) {
    if(StockRecruit$Age[i] == 2 & StockRecruit$RunID[i] == RunID_y) {
        # Get Age 3 Time Step 1 starting cohort from 'year+1'
        stk <- StockRecruit$StockID[i]
        age <- 3
        ts <- 1
        A3T1 <- Cohort[Cohort$RunID == RunID_yp1 & Cohort$StockID == stk & Cohort$Age == age & Cohort$TimeStep == ts,8]
        if(length(A3T1)==0) {
            A3T1 <- 0
        }
        A3T1 <- round(A3T1,0)
        
        # Get Age 3 Time Step 4 starting cohort from 'year'
        stk <- StockRecruit$StockID[i]
        age <- 3
        ts <- 4
        A3T4 <- Cohort[Cohort$RunID == RunID_y & Cohort$StockID == stk & Cohort$Age == age & Cohort$TimeStep == ts,8]
        if(length(A3T4)==0) {
            A3T4 <- 0
        }
        A3T4 <- round(A3T4,0)
        
        # Updated Age 2 Recruit Scalar
        NewRecruitScalar <- round(A3T1 / A3T4 * StockRecruit[i,5],4)
        
        # Get primary key for record in StockRecruit table
        pk <- StockRecruit[i,1]
        
        con = odbcConnectAccess(infile)
        sqlQuery(con, as.is = TRUE,
                 paste(sep = '',
                       "UPDATE StockRecruit SET StockRecruit.RecruitScaleFactor = ",NewRecruitScalar," WHERE (((StockRecruit.PrimaryKey)=",pk,"))"))
        close(con)
    }
}

nd <- Sys.time()
tm <- nd - strt
tm
    