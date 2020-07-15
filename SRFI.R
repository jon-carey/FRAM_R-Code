# Clear workspace
rm(list=ls(all=TRUE))

# Load required libraries
library(RODBC)
library(doBy)

# Set RunID
run_id <- 188

# define SRFI base period ER (THIS SHOULD ONLY CHANGE WITH A NEW BASE PERIOD CALIBRATION!)
SRFI_BP_ER = 0.46847

db_path <- "C:\\Users\\jonathan.carey\\Documents\\PFMC\\STT\\2020\\FRAM\\Chinook\\Chinook_NOF_2020_FRAM.mdb"

con = odbcConnectAccess(db_path)
Esc = sqlQuery(con, as.is = TRUE,
                paste(sep = '',
                      "SELECT Escapement.* ",
                      "FROM Escapement ",
                      "WHERE (((Escapement.RunID)=",run_id,") ",
                      "AND ((Escapement.StockID) In (54)));"))
Mort = sqlQuery(con, as.is = TRUE,
                paste(sep = '',
                      "SELECT Mortality.* ",
                      "FROM Mortality ",
                      "WHERE (((Mortality.RunID)=",run_id,") ",
                      "AND ((Mortality.StockID) In (54)));"))
RunID = sqlQuery(con, as.is = TRUE,
                   paste(sep = '',
                         "SELECT * FROM RunID"))
BPID <- RunID[RunID$RunID == run_id, 6]
TermFlag = sqlQuery(con, as.is = TRUE,
                paste(sep = '',
                      "SELECT TerminalFisheryFlag.* ",
                      "FROM TerminalFisheryFlag ",
                      "WHERE (TerminalFisheryFlag.BasePeriodID)=",BPID,";"))
AEQ = sqlQuery(con, as.is = TRUE,
                paste(sep = '',
                      "SELECT AEQ.* ",
                      "FROM AEQ ",
                      "WHERE (((AEQ.BasePeriodID)=",BPID,") ",
                      "AND ((AEQ.StockID) In (54)));"))
close(con)

# add terminal flag to mort table
Mort <- merge(Mort, TermFlag[ ,c(2:4)], all.x = TRUE)
Mort$TerminalFlag[is.na(Mort$TerminalFlag)] <- 0

# add AEQ
Mort <- merge(Mort, AEQ[ ,c(2:5)], all.x = TRUE)

# if terminal, set AEQ to 1
i=1
for(i in 1:dim(Mort)[1]) {
  if(Mort$TerminalFlag[i] == 1) {
    Mort$AEQ[i] <- 1 
  }
}

# sum total mort and AEQ mort
Mort$Tot_Mort <- rowSums(Mort[ ,c(7:10,12:15)])
Mort$AEQ_Mort <- Mort$Tot_Mort * Mort$AEQ

Age_3_4_Mort <- sum(Mort[Mort$Age %in% c(3,4) & Mort$TimeStep %in% c(1:3), 20])
Tot_AEQ_Mort <- sum(Mort[ , 20])
Escapement <- sum(Esc$Escapement)
SRFI_ER <- Age_3_4_Mort / (Tot_AEQ_Mort + Escapement)

SRFI <- SRFI_ER / SRFI_BP_ER
round(SRFI,4)


