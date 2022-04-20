#######################################################################################
############## Code to assist with modeling SOF CNR impacts in Coho FRAM ##############
#----------------------------------- 5.15.2019, JC -----------------------------------#
#######################################################################################
# Follow these steps to implement:
#   1. Set the path to the file directory (see "Set directory" below).
#   2. Make any necessary run specific updates to input data (proportion CNR by 
#      fishery/time step, gear target adjustment, KMZ fall troll quotas or landing 
#      limits, etc, in 'SOF_CNR_R_In.xlsx').
#   3. Ensure that the 'SOF_CNR_R_In.xlsx' file is located in the directory specified
#      in (1) above.  If necessary update the file name in the paths list below.
#   3. Get KOHM effort output file from Mike, save in the directory itentified in
#      (1) above.  If necessary update the file name in the paths list below. 
#   4. Identify the name of the coho FRAM access database in the paths list below.
#   5. Identify and set the RunID for the model run (see "Set RunID" below).
#   6. Get effort estimate for Brookings Sport in Oct from Craig F., update below.
#   7. Ctrl+A to select all of the code, then run. The code will read in the necessary
#      data, calculate the fishery scalars, insert them into the database, and zero 
#      out the relevent non-retention inputs. It will then prompt the user to reload
#      then run the model run in FRAM.  Once this is complete, select the 'OK' button
#      in the popup box and the program will continue by reading in the mortality
#      output from the run that was just completed, calculating the non-retention
#      inputs, inserting them into the database, and reverting the fishery scalars 
#      back to zero. Once this is complete, another popup box will prompt the user to
#      again reload and run the model run.  This time it can be run with the TAMM and
#      will produce the fimal results.

# Required Files:
#   1. 'SOF_CNR_R_In_XXXX.xlsx' file: this includes base period effort, proportion
#      of each fishery that is non-retention, gear target adjustments, and KMZ fall
#      troll quotas and landings limits (for inclusion with KOHM effort output).
#   2. KOHM effort output text file
#   3. Coho FRAM database
#######################################################################################

# Clear the workspace
rm(list=ls())

# Load packages
library(tidyverse); library(RODBC)
library(readxl);    library(xlsx)
library(svDialogs); library(doBy)
library(reshape)

# Set directory
dir <- "C:\\Users\\jonathan.carey\\Documents\\PFMC\\STT\\2018\\Hoopa Litigation\\CNR Methodology\\Preseason comparison\\"

# Set paths
paths = list(paste(dir, "SOF_CNR_R_In_Coho1731.xlsx", sep = ""),
             paste(dir, "KOHM.output.effort.2017.final.txt", sep = ""),
             paste(dir, "2013-19 Coho Preseason.mdb", sep = ""))

# Set RunID
RunID <- 12

# Effot estimate for Brookings Sport in October (from Craig)
Oct_Brook_Spt <- 2721

# Get Run Name from RunID table in database
con <- odbcConnectAccess(paths[[3]])
RunName = sqlQuery(con, as.is = TRUE,
                   paste(sep = '',
                         "SELECT RunID.RunName ",
                            "FROM RunID ",
                            "WHERE (((RunID.RunID)=",RunID,"));"))

close(con)

# Load base period effort data and KMZ fall quota data
Trl_Base <- read_excel(paths[[1]], "Trl_Base")
Spt_Base <- read_excel(paths[[1]], "Spt_Base")
KMZ_Trl_Quota <- read_excel(paths[[1]], "Fall_KMZ_Troll")

# Load KOHM Effort Output
dat <- readLines(paths[[2]])

#----- Summarize Troll Effort and calculate effort scalars -----#
T1 <- read_table(dat[c(7,10:14)])
T1 <- rbind(T1[c(1:3), c(2:14)], colSums(T1[c(4,5), c(2:14)]))
T1$Jan_Jun <- rowSums(T1[,c(5:10)])

T2 <- read_table(dat[c(37,40:44)])
T2 <- rbind(T2[c(1:3), c(2:6)], colSums(T2[c(4,5), c(2:6)]))
T2$Oct_Dec <- rowSums(T2[ ,c(2:4)])

FishNm <- c("KO", "KC", "FB", "SF_MO")

Trl_Effort <- cbind(FishNm, T1[ ,c(14,11,12)], T2[ ,c(1,6)])
Trl_Effort[1,5] <- Trl_Effort[1,5] + KMZ_Trl_Quota[2,3] / KMZ_Trl_Quota[2,4] # adds effort to KO based on quota/landing limit
Trl_Effort[2,5] <- Trl_Effort[2,5] + KMZ_Trl_Quota[1,3] / KMZ_Trl_Quota[1,4] # adds effort to KC based on quota/landing limit
Trl_Rate <- cbind(Trl_Base[ ,2], round(Trl_Effort[ ,c(2:6)] / Trl_Base[ ,c(3:7)], 4))
Trl_Rate[c(3,4), 6] <- 0 # sets Oct-Dec to 0 for FB and SoCal troll (no BPER)
rm(T1,T2)

#----- Summarize Sport Effort and calculate effort scalars -----#
S1 <- read_table(dat[c(19,22:26)])
S1 <- rbind(S1[c(1:3), c(2:14)], colSums(S1[c(4,5), c(2:14)]))
S1$Jan_Jun <- rowSums(S1[,c(5:10)])

S2 <- read_table(dat[c(49,52:56)])
S2 <- rbind(S2[c(1:3), c(2:6)], colSums(S2[c(4,5), c(2:6)]))
S2$Oct_Dec <- rowSums(S2[ ,c(2:4)])

Spt_Effort <- cbind(FishNm, S1[ ,c(14,11,12)], S2[ ,c(1,6)])
Spt_Effort[1,6] <- Spt_Effort[1,6] + Oct_Brook_Spt
Spt_Rate <- cbind(Spt_Base[ ,2], round(Spt_Effort[ ,c(2:6)] / Spt_Base[ ,c(3:7)], 4))
rm(S1,S2)

#----- Update database with effort scalars and zero out non-retention inputs -----#
Scalars_P1 <- rbind(Trl_Rate, Spt_Rate)
colnames(Scalars_P1)[2:6] <- c(1:5)
Scalars_P1 <- melt(Scalars_P1, id=1)
colnames(Scalars_P1)[2] <- "TimeStep"

# Pull existing FisheryScalars and CNR data
con <- odbcConnectAccess(paths[[3]])
Scalars_DB = sqlQuery(con, as.is = TRUE, 
                      paste(sep = '',
                            "SELECT FisheryScalers.* ",
                            "FROM FisheryScalers ",
                            "WHERE (((FisheryScalers.RunID)=",RunID,") ",
                            "AND ((FisheryScalers.FisheryID) In (3,4,5,6,7,8,15,16)));"))
close(con)

# Update with new fishery scalars
Scalars_P1 <- merge(Scalars_DB, Scalars_P1, all.x = TRUE)
Scalars_P1 <- Scalars_P1[order(Scalars_P1$FisheryID,
                               Scalars_P1$TimeStep) ,c(3,4,1,2,5,15,7:14)]
colnames(Scalars_P1)[6] <- "FisheryScaleFactor"

# Set FisheryFlags to 1
Scalars_P1$FisheryFlag <- rep(1, dim(Scalars_P1)[1])

# Delete existing FisheryScalars data
con <- odbcConnectAccess(paths[[3]])
sqlQuery(con, as.is = TRUE,
         paste(sep = '',
               "DELETE FisheryScalers.* ",
               "FROM FisheryScalers ",
               "WHERE (((FisheryScalers.RunID)=",RunID,") ",
               "AND ((FisheryScalers.FisheryID) In (3,4,5,6,7,8,15,16)));"))

# Insert new fishery scalars
sqlSave(con, Scalars_P1, tablename = "FisheryScalers", rownames = FALSE, append = TRUE)

# Delete existing non-retention inputs
sqlQuery(con, as.is = TRUE,
         paste(sep = '',
               "DELETE NonRetention.* ",
               "FROM NonRetention ",
               "WHERE (((NonRetention.RunID)=",RunID,") ",
               "AND ((NonRetention.FisheryID) In (3,4,5,6,7,8,15,16)));"))

close(con)


##########################################################################
#---------- Pause R script to reload and run model run in FRAM ----------#
dlgMessage(paste("Reload and run the model run then click OK."))$res
##########################################################################


#----- Calculate non-retention mortality inputs -----#
# Pull resulting mortality ouput from model run after it has been run with new scalars
con <- odbcConnectAccess(paths[[3]])
Morts_DB = sqlQuery(con, as.is = TRUE,
                    paste(sep = '',
                          "SELECT Mortality.* ",
                          "FROM Mortality ",
                          "WHERE (((Mortality.RunID)=",RunID,") ",
                          "AND ((Mortality.FisheryID) In (3,4,5,6,7,8,15,16)));"))
close(con)

# Summarize landed catch
Landed <- summaryBy(LandedCatch~FisheryID+TimeStep, data = Morts_DB, FUN = sum, keep.names = TRUE)

# Load and rearrange other input data
Ppn_NonRet <- as.data.frame(read_excel(paths[[1]], "Ppn_NonRet"))
Gear_Target_Adj <- as.data.frame(read_excel(paths[[1]], "Gear_Target_Adj"))
Rel_Mort_Rate <- as.data.frame(read_excel(paths[[1]], "Rel_Mort_Rate"))

colnames(Ppn_NonRet)[3:7] <- c(1:5)
Ppn_NonRet <- melt(Ppn_NonRet[ ,c(2:7)], id=1, variable_name = "TimeStep")
colnames(Ppn_NonRet)[3] <- "Ppn_NonRet"

colnames(Gear_Target_Adj)[3:7] <- c(1:5)
Gear_Target_Adj <- melt(Gear_Target_Adj[ ,c(2:7)], id=1, variable_name = "TimeStep")
colnames(Gear_Target_Adj)[3] <- "Gear_Target_Adj"

# Calculate CNR inputs for Pass 2 run
CNR <- merge(Landed, Rel_Mort_Rate[ ,c(2,3)])
CNR <- merge(CNR, Ppn_NonRet)
CNR <- merge(CNR, Gear_Target_Adj)

CNR$Input <- round(CNR$LandedCatch * (CNR$MortRate + 0.05) * CNR$Ppn_NonRet * CNR$Gear_Target_Adj, 1)
CNR <- CNR[order(CNR$FisheryID,CNR$TimeStep), ]

# Generate table with updated CNR values for appending to database
CNR_P2 <- as.data.frame(array(0, c(dim(CNR)[1],8)))
colnames(CNR_P2) <- c("RunID", "FisheryID", "TimeStep", "NonRetentionFlag",
                      "CNRInput1", "CNRInput2", "CNRInput3", "CNRInput4")
CNR_P2$RunID <- rep(RunID, dim(CNR)[1])
CNR_P2$FisheryID <- CNR$FisheryID
CNR_P2$TimeStep <- CNR$TimeStep
CNR_P2$NonRetentionFlag <- rep(1, dim(CNR)[1])
CNR_P2$CNRInput1 <- CNR$Input

# Delet existing fishery scalars from database
con <- odbcConnectAccess(paths[[3]])
sqlQuery(con, as.is = TRUE,
         paste(sep = '',
               "DELETE FisheryScalers.* ",
               "FROM FisheryScalers ",
               "WHERE (((FisheryScalers.RunID)=",RunID,") ",
               "AND ((FisheryScalers.FisheryID) In (3,4,5,6,7,8,15,16)));"))

# Append original fishery scalars to databse
sqlSave(con, Scalars_DB, tablename = "FisheryScalers", rownames = FALSE, append = TRUE)

# Pull PrimaryKey field from NonRetention table to determine max value
PK <- sqlQuery(con, as.is = TRUE,
               paste(sep = '',
                     "SELECT NonRetention.PrimaryKey ",
                     "FROM NonRetention;"))
PK_Max <- max(PK)

# Generate PrimarkKey values for CNR Pass 2 inputs
CNR_P2$PrimaryKey <- seq(from = max(PK)+1, to = max(PK)+dim(CNR_P2)[1],by = 1)
CNR_P2 <- CNR_P2[ ,c(9,1:8)]

# Append new CNR inputs to databse
sqlSave(con, CNR_P2, tablename = "NonRetention", rownames = FALSE, append = TRUE)

close(con)


#################################################
#--- Export an excel file for record keeping ---#
#################################################
wb <- createWorkbook(type = "xlsx")

# Set up formats
TITLE_STYLE <- CellStyle(wb) + Font(wb,  heightInPoints=14, isItalic=TRUE, isBold=TRUE)

TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold = TRUE) +
  Border(color = "black", position = c("BOTTOM"), pen = c("BORDER_THIN"))

# Sheet 1; Troll Data
sheet1 <- createSheet(wb, sheetName = "Troll Data")
addDataFrame(Trl_Base, sheet1, startRow = 2, colnamesStyle = TABLE_COLNAMES_STYLE)
addDataFrame(Trl_Effort, sheet1, startRow = 10, colnamesStyle = TABLE_COLNAMES_STYLE)
addDataFrame(Trl_Rate, sheet1, startRow = 18, colnamesStyle = TABLE_COLNAMES_STYLE)

rows <-createRow(sheet1,rowIndex=1)
table1Title <-createCell(rows, colIndex=2)
setCellValue(table1Title[[1,1]], "Troll Base Effort")
setCellStyle(table1Title[[1,1]], TITLE_STYLE)

rows <-createRow(sheet1,rowIndex=9)
table2Title <-createCell(rows, colIndex=2)
setCellValue(table2Title[[1,1]], "Troll Modeled Effort")
setCellStyle(table2Title[[1,1]], TITLE_STYLE)

rows <-createRow(sheet1,rowIndex=17)
table3Title <-createCell(rows, colIndex=2)
setCellValue(table3Title[[1,1]], "Troll Effort Scalers")
setCellStyle(table3Title[[1,1]], TITLE_STYLE)

# Sheet 2; Sport Data
sheet2 <- createSheet(wb, sheetName = "Sport Data")
addDataFrame(Spt_Base, sheet2, startRow = 2, colnamesStyle = TABLE_COLNAMES_STYLE)
addDataFrame(Spt_Effort, sheet2, startRow = 10, colnamesStyle = TABLE_COLNAMES_STYLE)
addDataFrame(Spt_Rate, sheet2, startRow = 18, colnamesStyle = TABLE_COLNAMES_STYLE)

rows <-createRow(sheet2,rowIndex=1) 
table1Title <-createCell(rows, colIndex=2)
setCellValue(table1Title[[1,1]], "Sport Base Effort")
setCellStyle(table1Title[[1,1]], TITLE_STYLE)

rows <-createRow(sheet2,rowIndex=9)
table2Title <-createCell(rows, colIndex=2)
setCellValue(table2Title[[1,1]], "Sport Modeled Effort")
setCellStyle(table2Title[[1,1]], TITLE_STYLE)

rows <-createRow(sheet2,rowIndex=17)
table3Title <-createCell(rows, colIndex=2)
setCellValue(table3Title[[1,1]], "Sport Effort Scalers")
setCellStyle(table3Title[[1,1]], TITLE_STYLE)

# Sheet 3; Pass 1 Scalers
sheet3 <- createSheet(wb, sheetName = "Pass 1 Scalers")
addDataFrame(Scalars_P1, sheet3, startRow = 2, colnamesStyle = TABLE_COLNAMES_STYLE)

rows <-createRow(sheet3,rowIndex=1) 
table1Title <-createCell(rows, colIndex=2)
setCellValue(table1Title[[1,1]], "Pass 1 Scalers")
setCellStyle(table1Title[[1,1]], TITLE_STYLE)

# Sheet 4; Pass 2 CNR
sheet4 <- createSheet(wb, sheetName = "Pass 2 CNR")
addDataFrame(CNR, sheet4, startRow = 2, colnamesStyle = TABLE_COLNAMES_STYLE)
addDataFrame(CNR_P2, sheet4, startRow = 2, startColumn = 10, 
             colnamesStyle = TABLE_COLNAMES_STYLE)

rows <-createRow(sheet4,rowIndex=1) 
table1Title <-createCell(rows, colIndex=2)
setCellValue(table1Title[[1,1]], "CNR Calculation")
setCellStyle(table1Title[[1,1]], TITLE_STYLE)
table2Title <- createCell(rows, colIndex = 11)
setCellValue(table2Title[[1,1]], "Pass 2 CNR Inputs")
setCellStyle(table2Title[[1,1]], TITLE_STYLE)

saveWorkbook(wb, paste(dir, "SOF_CNR_R_Out; ", RunName[1,1], ".xlsx", sep = ""))


##################################################################################
#-------------- Pause R script to reload and run model run in FRAM --------------#
dlgMessage(paste("Don't forget to reload and run the model run again!"))$res
##################################################################################
           
           
