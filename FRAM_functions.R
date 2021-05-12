##############################################################################
# Series of functions to update FRAM runs for various zero fishing scenarios #
##############################################################################
#
# 1. pull_AEQ: pulls AEQ table from a specified database, 
#              option to specify bpID
# 2. pull_Cohort: pulls Cohort table from a specified database, 
#                 option to specify runID, stock, age, timestep
# 3. pull_Escapement: pulls Escapement table from a specified database, 
#                     option to specify runID, stock, age, timestep
# 4. pull_Mortality: pulls Mortality table from a specified database, 
#                    option to specify runID, stock, age, fishery, timestep
# 5. pull_RunID: pulls RunID table from a specified database, 
#                option to specify runID, stock, age, timestep
# 6. pull_TerminalFisheryFlag: pulls TerminalFisheryFlag table from a 
#                              specified database, option to specify bpID
# 7. ZeroPS_SRKW: zeros out all PS fishery and CNR inputs (with exception of 
#                 Hood Canal sport & net and 13A net), updates remaining ISBM
#                 fishery flags to scalers. Can accomodate multiple runIDs.
# 8. ZeroPS_SRKW: zeros out all PS fishery and CNR inputs, updates remaining 
#                 ISBM fishery flags to scalers. Can accomodate multiple runIDs.
#
#
#----------------------------------------------------------------------------#
# Function to pull AEQ table from a FRAM database
pull_AEQ <- function(db_path, bpID) {
  con = odbcConnectAccess(db_path)
  
  if(missing(bpID)) {
    AEQ = sqlQuery(con, as.is = TRUE,
                     paste(sep = '',
                           "SELECT * FROM AEQ"))
  } else {
    bpID_string <- toString(sprintf("%s", bpID))
    AEQ = sqlQuery(con, as.is = TRUE,
                     paste(sep = '',
                           "SELECT AEQ.* ",
                           "FROM AEQ ",
                           "WHERE (((AEQ.BasePeriodID) In (",bpID_string,")));"))
  }
  
  close(con)
  return(AEQ[order(AEQ$BasePeriodID,AEQ$StockID,AEQ$Age,AEQ$TimeStep), ])
}
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Function to pull Cohort table from a FRAM database
pull_Cohort <- function(db_path, runID=NULL, stock=NULL, age=NULL, timestep=NULL) {
  x <- list()
  if(is.null(runID) == FALSE) {
    x[["RunID"]] <- runID
  }
  if(is.null(stock) == FALSE) {
    x[["StockID"]] <- stock
  }
  if(is.null(age) == FALSE) {
    x[["Age"]] <- age
  }
  if(is.null(timestep) == FALSE) {
    x[["TimeStep"]] <- timestep
  }
  
  con = odbcConnectAccess(db_path)
  if(length(x) == 0) {
    Cohort <- sqlQuery(con, as.is = TRUE,
                       paste(sep = '',
                             "SELECT * FROM Cohort"))
  }
  
  if(length(x) == 1) {
    x1_string <- toString(sprintf("%s", x[[1]]))
    Cohort = sqlQuery(con, as.is = TRUE,
                      paste(sep = '',
                            "SELECT Cohort.* ",
                            "FROM Cohort ",
                            "WHERE (((Cohort.",names(x[1]),") In (",x1_string,")));"))
  }
  
  if(length(x) > 1) {
    x1_string <- toString(sprintf("%s", x[[1]]))
    where_clause <- paste("WHERE (((Cohort.",names(x[1]),") In (",x1_string,"))",sep = "")
    
    for(i in 2:length(x)) {
      xi_string <- toString(sprintf("%s", x[[i]]))
      if(i == 2) {
        and_clause <- paste(" AND ((Cohort.",names(x[i]),") In (",xi_string,"))",sep = "")
      }
      if(i > 2) {
        and_clause <- paste(and_clause," AND ((Cohort.",names(x[i]),") In (",xi_string,"))",sep = "")
      }
    }
    
    Cohort = sqlQuery(con, as.is = TRUE,
                      paste(sep = '',
                            "SELECT Cohort.* ",
                            "FROM Cohort ",
                            where_clause,
                            and_clause,");"))
  }
  close(con)
  
  return(Cohort[order(Cohort$RunID,Cohort$StockID,Cohort$Age,Cohort$TimeStep), ])
}
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Function to pull Escapement table from a FRAM database
pull_Escapement <- function(db_path, runID=NULL, stock=NULL, age=NULL, timestep=NULL) {
  x <- list()
  if(is.null(runID) == FALSE) {
    x[["RunID"]] <- runID
  }
  if(is.null(stock) == FALSE) {
    x[["StockID"]] <- stock
  }
  if(is.null(age) == FALSE) {
    x[["Age"]] <- age
  }
  if(is.null(timestep) == FALSE) {
    x[["TimeStep"]] <- timestep
  }
  
  con = odbcConnectAccess(db_path)
  if(length(x) == 0) {
    Escapement <- sqlQuery(con, as.is = TRUE,
                           paste(sep = '',
                                 "SELECT * FROM Escapement"))
  }
  
  if(length(x) == 1) {
    x1_string <- toString(sprintf("%s", x[[1]]))
    Escapement = sqlQuery(con, as.is = TRUE,
                          paste(sep = '',
                                "SELECT Escapement.* ",
                                "FROM Escapement ",
                                "WHERE (((Escapement.",names(x[1]),") In (",x1_string,")));"))
  }
  
  if(length(x) > 1) {
    x1_string <- toString(sprintf("%s", x[[1]]))
    where_clause <- paste("WHERE (((Escapement.",names(x[1]),") In (",x1_string,"))",sep = "")
    
    for(i in 2:length(x)) {
      xi_string <- toString(sprintf("%s", x[[i]]))
      if(i == 2) {
        and_clause <- paste(" AND ((Escapement.",names(x[i]),") In (",xi_string,"))",sep = "")
      }
      if(i > 2) {
        and_clause <- paste(and_clause," AND ((Escapement.",names(x[i]),") In (",xi_string,"))",sep = "")
      }
    }
    
    Escapement = sqlQuery(con, as.is = TRUE,
                          paste(sep = '',
                                "SELECT Escapement.* ",
                                "FROM Escapement ",
                                where_clause,
                                and_clause,");"))
  }
  close(con)
  
  return(Escapement[order(Escapement$RunID,Escapement$StockID,Escapement$Age,Escapement$TimeStep), ])
}
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Function to pull Mortality table from a FRAM database
pull_Mortality <- function(db_path, runID=NULL, stock=NULL, age=NULL, fishery=NULL, timestep=NULL) {
  x <- list()
  if(is.null(runID) == FALSE) {
    x[["RunID"]] <- runID
  }
  if(is.null(stock) == FALSE) {
    x[["StockID"]] <- stock
  }
  if(is.null(age) == FALSE) {
    x[["Age"]] <- age
  }
  if(is.null(fishery) == FALSE) {
    x[["FisheryID"]] <- fishery
  }
  if(is.null(timestep) == FALSE) {
    x[["TimeStep"]] <- timestep
  }
  
  con = odbcConnectAccess(db_path)
  if(length(x) == 0) {
    Mort <- sqlQuery(con, as.is = TRUE,
                     paste(sep = '',
                           "SELECT * FROM Mortality"))
  }
  
  if(length(x) == 1) {
    x1_string <- toString(sprintf("%s", x[[1]]))
    Mort = sqlQuery(con, as.is = TRUE,
                    paste(sep = '',
                          "SELECT Mortality.* ",
                          "FROM Mortality ",
                          "WHERE (((Mortality.",names(x[1]),") In (",x1_string,")));"))
  }
  
  if(length(x) > 1) {
    x1_string <- toString(sprintf("%s", x[[1]]))
    where_clause <- paste("WHERE (((Mortality.",names(x[1]),") In (",x1_string,"))",sep = "")
    
    for(i in 2:length(x)) {
      xi_string <- toString(sprintf("%s", x[[i]]))
      if(i == 2) {
        and_clause <- paste(" AND ((Mortality.",names(x[i]),") In (",xi_string,"))",sep = "")
      }
      if(i > 2) {
        and_clause <- paste(and_clause," AND ((Mortality.",names(x[i]),") In (",xi_string,"))",sep = "")
      }
    }
    
    Mort = sqlQuery(con, as.is = TRUE,
                    paste(sep = '',
                          "SELECT Mortality.* ",
                          "FROM Mortality ",
                          where_clause,
                          and_clause,");"))
  }
  close(con)
  
  return(Mort[order(Mort$RunID,Mort$StockID,Mort$Age,Mort$FisheryID,Mort$TimeStep), ])
}
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Function to pull RunID table from a FRAM database
pull_RunID <- function(db_path, runID) {
  con = odbcConnectAccess(db_path)
  
  if(missing(runID)) {
    RunID = sqlQuery(con, as.is = TRUE,
                     paste(sep = '',
                           "SELECT * FROM RunID"))
  } else {
    runID_string <- toString(sprintf("%s", runID))
    RunID = sqlQuery(con, as.is = TRUE,
                     paste(sep = '',
                           "SELECT RunID.* ",
                           "FROM RunID ",
                           "WHERE (((RunID.RunID) In (",runID_string,")));"))
  }
  
  close(con)
  return(RunID[order(RunID$RunID), ])
}
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Function to pull TerminalFisheryFlag table from a FRAM database
pull_TerminalFisheryFlag <- function(db_path, bpID) {
  con = odbcConnectAccess(db_path)
  
  if(missing(bpID)) {
    TermFlag = sqlQuery(con, as.is = TRUE,
                   paste(sep = '',
                         "SELECT * FROM TerminalFisheryFlag"))
  } else {
    bpID_string <- toString(sprintf("%s", bpID))
    TermFlag = sqlQuery(con, as.is = TRUE,
                   paste(sep = '',
                         "SELECT TerminalFisheryFlag.* ",
                         "FROM TerminalFisheryFlag ",
                         "WHERE (((TerminalFisheryFlag.BasePeriodID) In (",bpID_string,")));"))
  }
  
  close(con)
  return(TermFlag[order(TermFlag$BasePeriodID,TermFlag$FisheryID,TermFlag$TimeStep), ])
}
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Function to update a FRAM run for SRKW "Zero PS"

ZeroPS_SRKW <- function(runID, db_path) {
  con = odbcConnectAccess(db_path)
  
  runID_string <- toString(sprintf("%s", runID))
  
  # query to zero out PS fisheries (with exception of 'A 12 Sport', 'NT HC Net', 'Tr HC Net', 'Tr HC Net', and 'Tr 13A Net')
  sqlQuery(con, as.is = TRUE,
           paste(sep = '',
                 "UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 2, FisheryScalers.Quota = 0 ",
                 "WHERE (((FisheryScalers.FisheryID)>=36 And (FisheryScalers.FisheryID) Not In (64,65,66,70,71)) AND ((FisheryScalers.RunID) In (",runID_string,"))) ",
                 "OR (((FisheryScalers.FisheryID)=17) AND ((FisheryScalers.RunID) In (",runID_string,")) AND ((FisheryScalers.TimeStep) In (1,4)));"))
  
  # query to update remaining ISBM flags from 2 to 1
  sqlQuery(con, as.is = TRUE,
           paste(sep = '',
                 "UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 1 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND (((FisheryScalers.FisheryID) In (4,5,6,7,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,64,65,66,70,71)) ",
                 "OR ((FisheryScalers.FisheryID)=17 And (FisheryScalers.TimeStep) In (2,3))) AND ((FisheryScalers.FisheryFlag)=2));"))
  
  # query to update remaining ISBM flags from 8 to 7
  sqlQuery(con, as.is = TRUE,
           paste(sep = '',
                 "UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 7 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND (((FisheryScalers.FisheryID) In (4,5,6,7,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,64,65,66,70,71)) ",
                 "OR ((FisheryScalers.FisheryID)=17 And (FisheryScalers.TimeStep) In (2,3))) AND ((FisheryScalers.FisheryFlag)=8));"))
  
  # query to update remaining ISBM flags from 28 to 17
  sqlQuery(con, as.is = TRUE,
           paste(sep = '',
                 "UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 17 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND (((FisheryScalers.FisheryID) In (4,5,6,7,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,64,65,66,70,71)) ",
                 "OR ((FisheryScalers.FisheryID)=17 And (FisheryScalers.TimeStep) In (2,3))) AND ((FisheryScalers.FisheryFlag)=28));"))
  
  # query to zero out non-retention in PS fisheries
  sqlQuery(con, as.is = TRUE,
           paste(sep = '',
                 "UPDATE NonRetention SET NonRetention.CNRInput1 = 0, NonRetention.CNRInput2 = 0, NonRetention.CNRInput3 = 0, NonRetention.CNRInput4 = 0 ",
                 "WHERE (((NonRetention.RunID) In (",runID_string,")) AND ((NonRetention.FisheryID)>=36 And (NonRetention.FisheryID) Not In (64,65,66,70,71)));"))
  
  close(con)
}
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Function to update a FRAM run for "Zero PS"

ZeroPS <- function(runID, db_path) {
  con = odbcConnectAccess(db_path)
  
  runID_string <- toString(sprintf("%s", runID))
  
  # query to zero out all PS fisheries
  sqlQuery(con, as.is = TRUE,
           paste(sep = '',
                 "UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 2, FisheryScalers.Quota = 0 ",
                 "WHERE ((FisheryScalers.FisheryID)>=36 AND ((FisheryScalers.RunID) In (",runID_string,"))) ",
                 "OR (((FisheryScalers.FisheryID)=17) AND ((FisheryScalers.RunID) In (",runID_string,")) AND ((FisheryScalers.TimeStep) In (1,4)));"))
  
  # query to update remaining ISBM flags from 2 to 1
  sqlQuery(con, as.is = TRUE,
           paste(sep = '',
                 "UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 1 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND (((FisheryScalers.FisheryID) In (4,5,6,7,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35)) ",
                 "OR ((FisheryScalers.FisheryID)=17 And (FisheryScalers.TimeStep) In (2,3))) AND ((FisheryScalers.FisheryFlag)=2));"))
  
  # query to update remaining ISBM flags from 8 to 7
  sqlQuery(con, as.is = TRUE,
           paste(sep = '',
                 "UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 7 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND (((FisheryScalers.FisheryID) In (4,5,6,7,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35)) ",
                 "OR ((FisheryScalers.FisheryID)=17 And (FisheryScalers.TimeStep) In (2,3))) AND ((FisheryScalers.FisheryFlag)=8));"))
  
  # query to update remaining ISBM flags from 28 to 17
  sqlQuery(con, as.is = TRUE,
           paste(sep = '',
                 "UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 17 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND (((FisheryScalers.FisheryID) In (4,5,6,7,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35)) ",
                 "OR ((FisheryScalers.FisheryID)=17 And (FisheryScalers.TimeStep) In (2,3))) AND ((FisheryScalers.FisheryFlag)=28));"))
  
  # query to zero out non-retention in PS fisheries
  sqlQuery(con, as.is = TRUE,
           paste(sep = '',
                 "UPDATE NonRetention SET NonRetention.CNRInput1 = 0, NonRetention.CNRInput2 = 0, NonRetention.CNRInput3 = 0, NonRetention.CNRInput4 = 0 ",
                 "WHERE (((NonRetention.RunID) In (",runID_string,")) AND ((NonRetention.FisheryID)>=36));"))
  
  close(con)
}
#----------------------------------------------------------------------------#
