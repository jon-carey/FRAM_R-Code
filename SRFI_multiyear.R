# Clear workspace
rm(list=ls(all=TRUE))

# Load required libraries
library(RODBC)
library(doBy)

# Load FRAM functions
source("C:\\Users\\jonathan.carey\\Documents\\GitHub\\FRAM_R-Code\\FRAM_functions.R")

# Set RunID
run_IDs <- c(70)

# define SRFI base period ER (THIS SHOULD ONLY CHANGE WITH A NEW BASE PERIOD CALIBRATION!)
SRFI_BP_ER = 0.46847

db_path <- "C:\\Users\\jonathan.carey\\Documents\\PFMC\\STT\\2021\\FRAM\\Chinook\\Chinook_NOF_2021_FRAM.mdb"
outfile <- "C:\\Users\\jonathan.carey\\Documents\\PFMC\\STT\\2021\\FRAM\\Chinook\\Chin3721\\SRFI_Chin3721.csv"

SRFI_log <- data.frame(matrix(ncol = 3, nrow = length(run_IDs)))
colnames(SRFI_log) <- c("RunID", "Alt", "SRFI")

for(j in 1:length(run_IDs)) {
  run_id <- run_IDs[j]
  
  # pull necessary data from FRAM database
  Esc <- pull_Escapement(db_path, run_id, stock = 54)
  Mort <- pull_Mortality(db_path, run_id, stock = 54)
  RunID <- pull_RunID(db_path)
  bpID <- RunID[RunID$RunID == run_id, 6]
  TermFlag <- pull_TerminalFisheryFlag(db_path, bpID)
  AEQ <- pull_AEQ(db_path, bpID)
  AEQ <- AEQ[AEQ$StockID == 54, ]
  
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
  
  SRFI <- round(SRFI_ER / SRFI_BP_ER,4)
  
  SRFI_log_j <- c(run_id, paste("Alt",j,sep = ""), SRFI)
  
  SRFI_log[j, ] <- SRFI_log_j
}

write.csv(SRFI_log, outfile)
