################################################################################
# PROGRAM TO PULL ANNUAL TERMINAL RUN DATA FROM EXCEL FILE WITH BKFRAM TEMPLATE
# AND LOAD IT INTO THE "BACKWARDSFRAM" TABLE OF A VALIDATION DATABASE
#
#JC; AUG 2016

# 1. Added code to delete existing records for years being updated
# 2. Revised appending code so it only updates for years specified
# 3. Added "BP" variable for defining BP in excel template (to account for 
#    different Cowlitz and Willamette abundances) 
# 4. Added "KeepFlags" variable to allow the option to retain the existing 
#    BK flags that are in the database.
#
#JC; DEC 2016

# 1. Added functionality to deal with RunID tables w/ and w/out RunYear field
#
#JC; JUL 2017

# 1. Modified code to work with and without "SLC_" prefix in RunName when adding
#    Year and BP fields to RunID table
# 2. Removed dependency on column number in RunID table when assigning runID,
#    year, and bp in main loop
#
#JC; AUG 2020
################################################################################

# Clear workspace
rm(list=ls(all=TRUE))

# set start time for purposes of timing code
strt <- Sys.time()

library(RDCOMClient)
library(XLConnect)
library(readxl)
library(RODBC)

# Set paths
Dir <- "C:\\Users\\jonathan.carey\\Documents\\FRAM\\Calibration_Validation\\Chinook\\2020\\Abundances\\"
paths = list(paste(Dir, "Valid2020_FRAM_StockData_7.28.20.xlsm", sep=""),
             paste(Dir, "Valid2020_BK_junk.mdb", sep=""))

infile = paths[[1]]
Outfile = paths[[2]]

StartYr = 1988
EndYr = 2018

BP = "New"

# Retain existing flags? 1=Yes, 0=No
KeepFlags = 0


#####################################################################
# POPULATE TAMM STOCK & FISHERY DATA FOR ALL YEARS FROM MASTER FILE #
#####################################################################

# BKData <- as.data.frame(array(NA, c(116,6)))
# colnames(BKData) <- c("YEAR", "StockID", "TargetEscAge3", "TargetEscAge4", 
#                          "TargetEscAge5", "TargetFlag")

#BK df population
i=StartYr
for(i in StartYr:EndYr) {
  #Does some blackbox wizardry
  xlApp <- COMCreate("Excel.Application")
  #xlApp[["Visible"]] <- TRUE
  #Gets our workbook
  wb    <- xlApp[["Workbooks"]]$Open(infile)
  #Gets our FRAMsheet
  FRAMsheet <- wb$Worksheets("FRAMEscapeV2")
  
  # Updates year cell to year i
  Yearcell <- FRAMsheet$Cells(12,8)
  Yearcell[["Value"]] <- i
  
  # Updates base period cell to BP
  BPcell <- FRAMsheet$Cells(15,8)
  BPcell[["Value"]] <- BP
  
  wb$Save()
  
  xlApp$Quit()
  
  # Read in BK data for year i
  iData <- read_excel(infile, "R_In")
  
  # Add BK data for year i into main data table, 'StockData'
  if(i==StartYr) {
    BKData <- iData
  }
  if(!(i==StartYr)) {
    BKData <- rbind(BKData, iData)
  }
}

# Pull RunID and BackwardsFRAM tables from Validation Databse
con = odbcConnectAccess(Outfile)
RunID = sqlQuery(con, as.is = TRUE, 
                 paste(sep = '',
                       "SELECT * FROM RunID"))
BackwardsFRAM = sqlQuery(con, as.is = TRUE,
                         paste(sep = '',
                               "SELECT * FROM BackwardsFRAM"))
close(con)

# Sort by RunID and add year and BasePeriod fields
RunID <- RunID[order(RunID$RunID), ]
RunID$Year <- ifelse(substr(RunID$RunName,1,3) == "SLC",substr(RunID$RunName,15,18),substr(RunID$RunName,11,14))
RunID$BP <- ifelse(substr(RunID$RunName,1,3) == "SLC",substr(RunID$RunName,20,22),substr(RunID$RunName,16,18))

# Get existing BK flags
BKflags <- BackwardsFRAM[ ,c(1,2,6)]

# Append BK data to BackwardsFRAM table
i=StartYr
for (i in StartYr:EndYr) {
  RunID_temp <- RunID[RunID$Year == i,]
  runID <- RunID_temp$RunID
  year <- RunID_temp$Year
  bp <- RunID_temp$BP
  rm(RunID_temp)

  if(bp == "Old") {
    BKFRAM <- subset(BKData, YEAR == year & StockID <114)
    BKFRAM$RunID <- c(rep(runID, dim(BKFRAM)[1]))
    BKFRAM <- BKFRAM[ ,c(7,2:6)]
    BKFRAM <- format(BKFRAM, scientific = FALSE)
    BKFRAM$RunID <- as.integer(BKFRAM$RunID)
    BKFRAM$StockID <- as.integer(BKFRAM$StockID)
    
    if(KeepFlags == 1) {
      bkflags <- subset(BKflags, BKflags$RunID == runID)
      colnames(BKFRAM)[6] <- "BadFlags"
      BKFRAM <- merge(BKFRAM,bkflags)
      BKFRAM <- BKFRAM[ ,c(1:5,7)]
    }
  }
  
  if(bp == "New") {
    BKFRAM <- subset(BKData, YEAR == year)
    BKFRAM$RunID <- c(rep(runID, dim(BKFRAM)[1]))
    BKFRAM <- BKFRAM[ ,c(7,2:6)]
    BKFRAM$TargetEscAge3 <- format(BKFRAM$TargetEscAge3, scientific = FALSE)
    BKFRAM$TargetEscAge4 <- format(BKFRAM$TargetEscAge4, scientific = FALSE)
    BKFRAM$TargetEscAge5 <- format(BKFRAM$TargetEscAge5, scientific = FALSE)
    BKFRAM$RunID <- as.integer(BKFRAM$RunID)
    BKFRAM$StockID <- as.integer(BKFRAM$StockID)
    
    if(KeepFlags == 1) {
      bkflags <- subset(BKflags, BKflags$RunID == runID)
      colnames(BKFRAM)[6] <- "BadFlags"
      BKFRAM <- merge(BKFRAM,bkflags)
      BKFRAM <- BKFRAM[ ,c(1:5,7)]
    }
  }
  
  # UPDATE DATABASE
  con = odbcConnectAccess(Outfile)
  
  # First delete records in BackwardsFRAM table for RunID of year 'i'
  sqlQuery(con, as.is = TRUE,
           paste(sep = '',
                 "DELETE BackwardsFRAM.RunID ",
                 "FROM BackwardsFRAM ",
                 "WHERE (((BackwardsFRAM.RunID)=",runID,"))"))
  
  # Next append new BKFRAM abundances to BackwardsFRAM table for RunID of year 'i'
  sqlSave(con, BKFRAM, tablename = "BackwardsFRAM", rownames = FALSE,
          append = TRUE)
  
  close(con)
}

# i=1
# for (i in 1:dim(RunID)[1]) {
#     runID <- RunID$RunID[i]
#     year <- RunID$Year[i]
#     bp <- RunID$BP[i]
#     
#     if(bp == "Old") {
#         BKFRAM <- subset(BKData, YEAR == year & StockID <114)
#         BKFRAM$RunID <- c(rep(runID, dim(BKFRAM)[1]))
#         BKFRAM <- BKFRAM[ ,c(7,2:6)]
#         BKFRAM <- format(BKFRAM, scientific = FALSE)
#     }
#     
#     if(bp == "New") {
#         BKFRAM <- subset(BKData, YEAR == year)
#         BKFRAM$RunID <- c(rep(runID, dim(BKFRAM)[1]))
#         BKFRAM <- BKFRAM[ ,c(7,2:6)]
#         BKFRAM <- format(BKFRAM, scientific = FALSE)
#     }
#     
#     con = odbcConnectAccess(Outfile)
#     sqlSave(con, BKFRAM, tablename = "BackwardsFRAM", rownames = FALSE, 
#             append = TRUE)
#     close(con)
# }

nd <- Sys.time()
tm <- nd - strt
tm


