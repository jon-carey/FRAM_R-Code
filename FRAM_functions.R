##############################################################################
#######  Collection of various functions for use with a FRAM database  #######
##############################################################################
#
# 1. pull_AEQ(db_path, bpID): 
#       pulls AEQ table from a specified database, 
#       option to specify bpID
#
# 2. pull_BackwardsFRAM(db_path, runID): 
#       pulls BackwardsFRAM table from a specified database, 
#       option to specify runID
#
# 3. pull_Cohort(db_path, runID, stock, age, timestep): 
#       pulls Cohort table from a specified database, 
#       option to specify runID, stock, age, timestep
#
# 4. pull_Escapement(db_path, runID, stock, age, timestep): 
#       pulls Escapement table from a specified database, 
#       option to specify runID, stock, age, timestep
#
# 5. pull_Fishery(db_path): 
#       pulls Fishery table from a specified database 
#       
# 6. pull_FisheryModelStockProportion(db_path, bpID): 
#       pulls FisheryModelStockProportion table from a specified database, 
#       option to specify bpID
#
# 7. pull_FisheryScalers(db_path, runID, fishery, timestep): 
#       pulls FisheryScalers table from a specified database, 
#       option to specify runID, fishery, timestep
#
# 8. pull_MaturationRate(db_path, bpID): 
#       pulls MaturationRate table from a specified database, 
#       option to specify bpID
#
# 9. pull_Mortality(db_path, runID, stock, age, fishery, timestep): 
#       pulls Mortality table from a specified database, 
#       option to specify runID, stock, age, fishery, timestep
#
# 10. pull_NaturalMortality(db_path, bpID): 
#       pulls NaturalMortality table from a specified database, 
#       option to specify bpID
#
# 11. pull_RunID(db_path, runID): 
#       pulls RunID table from a specified database, 
#       option to specify runID
#
# 12. pull_ShakerMortRate(db_path, bpID): 
#       pulls ShakerMortRate table from a specified database, 
#       option to specify bpID
#
# 13. pull_Stock(db_path): 
#       pulls Stock table from a specified database
#
# 14. pull_TerminalFisheryFlag(db_path, bpID): 
#       pulls TerminalFisheryFlag table from a 
#       specified database, option to specify bpID
#
# 15. ZeroPS_SRKW(db_path, runID): 
#       zeros out all PS fishery and CNR inputs (with exception of 
#       Hood Canal sport & net and 13A net), updates remaining ISBM
#       fishery flags to scalers. Can accomodate multiple runIDs.
#
# 16. ZeroPS_SRKW_2021(db_path, runID): 
#       zeros out all PS fishery and CNR inputs for fisheries relevant
#       to SRKW analyses for PS fisheries, based on 2021 discussions 
#       with PS comanagers (see 'FRAM Fishery Exclusions_rev9.30.21.xlsx').
#       updates remaining ISBM fishery flags to scalers. 
#       Can accomodate multiple runIDs.
#
# 17. ZeroPS(db_path, runID): 
#       zeros out all PS fishery and CNR inputs, updates remaining 
#       ISBM fishery flags to scalers. Can accomodate multiple runIDs.
#
# 18. calc_SRFI(db_path, runID, SRFI_BP_ER, outfile): 
#       calculates SRFI values for a supplied list of RunIDs; requires a
#       SRFI_BP_ER to be supplied, will output a csv to outfile.
#
# 19. calc_SRFI_BP_ER(db_path):
#       calculates the SRFI base period ER (1988-1993) for the denominator
#       in the SRFI calculation. Requires 'db_path' that refers to the
#       relevant validation database. Returns a single value.
#
##############################################################################

# required packages
library(RODBC)
library(odbc)
# library(doBy)

#----------------------------------------------------------------------------#
# Function to pull AEQ table from a FRAM database
pull_AEQ <- function(db_path, bpID) {
  
  # set up query
  if(missing(bpID)) {
    qry = paste(sep = '',
                "SELECT * FROM AEQ")
  } else {
    bpID_string <- toString(sprintf("%s", bpID))
    qry = paste(sep = '',
                "SELECT AEQ.* ",
                "FROM AEQ ",
                "WHERE (((AEQ.BasePeriodID) In (",bpID_string,")));")
  }
  
  # run query
  if(version$arch == "x86_64") { # if running 64-bit R, use odbc package
    
    driverName = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", db_path)
    chnl = dbConnect(odbc(), .connection_string = driverName)
    AEQ = dbGetQuery(chnl, qry)
    dbDisconnect(chnl)
    
  } else if(version$arch == "i386") { # if using 32-bit R, use RODBC package
    
    con = odbcConnectAccess(db_path)
    AEQ = sqlQuery(con, as.is = TRUE, qry)
    close(con)
    
  }
  
  return(AEQ[order(AEQ$BasePeriodID,AEQ$StockID,AEQ$Age,AEQ$TimeStep), ])
  
}
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Function to pull BackwardsFRAM table from a FRAM database
pull_BackwardsFRAM <- function(db_path, runID) {
  
  # set up query
  if(missing(runID)) {
    qry = paste(sep = '',
                "SELECT * FROM BackwardsFRAM")
  } else {
    runID_string <- toString(sprintf("%s", runID))
    qry = paste(sep = '',
                "SELECT BackwardsFRAM.* ",
                "FROM BackwardsFRAM ",
                "WHERE (((BackwardsFRAM.RunID) In (",runID_string,")));")
  }
  
  # run query
  if(version$arch == "x86_64") { # if running 64-bit R, use odbc package
    
    driverName = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", db_path)
    chnl = dbConnect(odbc(), .connection_string = driverName)
    bkFRAM = dbGetQuery(chnl, qry)
    dbDisconnect(chnl)
    
  } else if(version$arch == "i386") { # if using 32-bit R, use RODBC package
    
    con = odbcConnectAccess(db_path)
    bkFRAM = sqlQuery(con, as.is = TRUE, qry)
    close(con)
    
  }
  
  return(bkFRAM[order(bkFRAM$RunID), ])
  
}
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Function to pull Cohort table from a FRAM database
pull_Cohort <- function(db_path, runID=NULL, stock=NULL, age=NULL, timestep=NULL) {
  
  # determine which arguments are provided
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
  
  # set up query
  if(length(x) == 0) {
    qry <- paste(sep = '',
                 "SELECT * FROM Cohort")
  }
  
  if(length(x) == 1) {
    x1_string <- toString(sprintf("%s", x[[1]]))
    qry = paste(sep = '',
                "SELECT Cohort.* ",
                "FROM Cohort ",
                "WHERE (((Cohort.",names(x[1]),") In (",x1_string,")));")
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
    
    qry = paste(sep = '',
                "SELECT Cohort.* ",
                "FROM Cohort ",
                where_clause,
                and_clause,");")
  }
  
  # run query
  if(version$arch == "x86_64") { # if running 64-bit R, use odbc package
    
    driverName = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", db_path)
    chnl = dbConnect(odbc(), .connection_string = driverName)
    Cohort = dbGetQuery(chnl, qry)
    dbDisconnect(chnl)
    
  } else if(version$arch == "i386") { # if using 32-bit R, use RODBC package
    
    con = odbcConnectAccess(db_path)
    Cohort = sqlQuery(con, as.is = TRUE, qry)
    close(con)
    
  }
  
  return(Cohort[order(Cohort$RunID,Cohort$StockID,Cohort$Age,Cohort$TimeStep), ])
  
}
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Function to pull Escapement table from a FRAM database
pull_Escapement <- function(db_path, runID=NULL, stock=NULL, age=NULL, timestep=NULL) {
  
  # determine which arguments are provided
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
  
  # set up query
  if(length(x) == 0) {
    qry <- paste(sep = '',
                 "SELECT * FROM Escapement")
  }
  
  if(length(x) == 1) {
    x1_string <- toString(sprintf("%s", x[[1]]))
    qry = paste(sep = '',
                "SELECT Escapement.* ",
                "FROM Escapement ",
                "WHERE (((Escapement.",names(x[1]),") In (",x1_string,")));")
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
    
    qry = paste(sep = '',
                "SELECT Escapement.* ",
                "FROM Escapement ",
                where_clause,
                and_clause,");")
  }
  
  # run query
  if(version$arch == "x86_64") { # if running 64-bit R, use odbc package
    
    driverName = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", db_path)
    chnl = dbConnect(odbc(), .connection_string = driverName)
    Escapement = dbGetQuery(chnl, qry)
    dbDisconnect(chnl)
    
  } else if(version$arch == "i386") { # if using 32-bit R, use RODBC package
    
    con = odbcConnectAccess(db_path)
    Escapement = sqlQuery(con, as.is = TRUE, qry)
    close(con)
    
  }
  
  return(Escapement[order(Escapement$RunID,Escapement$StockID,Escapement$Age,Escapement$TimeStep), ])
  
}
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Function to pull Fishery table from a FRAM database
pull_Fishery <- function(db_path, VersionNumber=1) {
  
  # set up query
  qry <- paste0("SELECT Fishery.* ",
                "FROM Fishery ",
                "WHERE (((Fishery.VersionNumber) = ",VersionNumber,"));")
  
  # run query
  if(version$arch == "x86_64") { # if running 64-bit R, use odbc package
    
    driverName = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", db_path)
    chnl = dbConnect(odbc(), .connection_string = driverName)
    FishID = dbGetQuery(chnl, qry)
    dbDisconnect(chnl)
    
  } else if(version$arch == "i386") { # if using 32-bit R, use RODBC package
    
    con = odbcConnectAccess(db_path)
    FishID = sqlQuery(con, as.is = TRUE, qry)
    close(con)
    
  }
  
  return(FishID[order(FishID$FisheryID), ])
  
}
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Function to pull pull_FisheryModelStockProportion table from a FRAM database
pull_FisheryModelStockProportion <- function(db_path, bpID) {
  
  # set up query
  if(missing(bpID)) {
    qry = paste(sep = '',
                "SELECT * FROM FisheryModelStockProportion")
  } else {
    bpID_string <- toString(sprintf("%s", bpID))
    qry = paste(sep = '',
                "SELECT FisheryModelStockProportion.* ",
                "FROM FisheryModelStockProportion ",
                "WHERE (((FisheryModelStockProportion.BasePeriodID) In (",bpID_string,")));")
  }
  
  # run query
  if(version$arch == "x86_64") { # if running 64-bit R, use odbc package
    
    driverName = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", db_path)
    chnl = dbConnect(odbc(), .connection_string = driverName)
    MSP = dbGetQuery(chnl, qry)
    dbDisconnect(chnl)
    
  } else if(version$arch == "i386") { # if using 32-bit R, use RODBC package
    
    con = odbcConnectAccess(db_path)
    MSP = sqlQuery(con, as.is = TRUE, qry)
    close(con)
    
  }
  
  return(MSP[order(MSP$BasePeriodID,MSP$FisheryID), ])
  
}
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Function to pull FisheryScalers table from a FRAM database
pull_FisheryScalers <- function(db_path, runID=NULL, fishery=NULL, timestep=NULL) {
  
  # determine which arguments are provided
  x <- list()
  if(is.null(runID) == FALSE) {
    x[["RunID"]] <- runID
  }
  if(is.null(fishery) == FALSE) {
    x[["FisheryID"]] <- fishery
  }
  if(is.null(timestep) == FALSE) {
    x[["TimeStep"]] <- timestep
  }
  
  # set up query
  if(length(x) == 0) {
    qry <- paste(sep = '',
                 "SELECT * FROM FisheryScalers")
  }
  
  if(length(x) == 1) {
    x1_string <- toString(sprintf("%s", x[[1]]))
    qry = paste(sep = '',
                "SELECT FisheryScalers.* ",
                "FROM FisheryScalers ",
                "WHERE (((FisheryScalers.",names(x[1]),") In (",x1_string,")));")
  }
  
  if(length(x) > 1) {
    x1_string <- toString(sprintf("%s", x[[1]]))
    where_clause <- paste("WHERE (((FisheryScalers.",names(x[1]),") In (",x1_string,"))",sep = "")
    
    for(i in 2:length(x)) {
      xi_string <- toString(sprintf("%s", x[[i]]))
      if(i == 2) {
        and_clause <- paste(" AND ((FisheryScalers.",names(x[i]),") In (",xi_string,"))",sep = "")
      }
      if(i > 2) {
        and_clause <- paste(and_clause," AND ((FisheryScalers.",names(x[i]),") In (",xi_string,"))",sep = "")
      }
    }
    
    qry = paste(sep = '',
                "SELECT FisheryScalers.* ",
                "FROM FisheryScalers ",
                where_clause,
                and_clause,");")
  }
  
  # run query
  if(version$arch == "x86_64") { # if running 64-bit R, use odbc package
    
    driverName = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", db_path)
    chnl = dbConnect(odbc(), .connection_string = driverName)
    FishScalers = dbGetQuery(chnl, qry)
    dbDisconnect(chnl)
    
  } else if(version$arch == "i386") { # if using 32-bit R, use RODBC package
    
    con = odbcConnectAccess(db_path)
    FishScalers = sqlQuery(con, as.is = TRUE, qry)
    close(con)
    
  }
  
  return(FishScalers[order(FishScalers$RunID,FishScalers$FisheryID,FishScalers$TimeStep), ])
  
}
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Function to pull Maturation Rate table from a FRAM database
pull_MaturationRate <- function(db_path, bpID) {
  
  # set up query
  if(missing(bpID)) {
    qry = paste(sep = '',
                "SELECT * FROM MaturationRate")
  } else {
    bpID_string <- toString(sprintf("%s", bpID))
    qry = paste(sep = '',
                "SELECT MaturationRate.* ",
                "FROM MaturationRate ",
                "WHERE (((MaturationRate.BasePeriodID) In (",bpID_string,")));")
  }
  
  # run query
  if(version$arch == "x86_64") { # if running 64-bit R, use odbc package
    
    driverName = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", db_path)
    chnl = dbConnect(odbc(), .connection_string = driverName)
    MatRate = dbGetQuery(chnl, qry)
    dbDisconnect(chnl)
    
  } else if(version$arch == "i386") { # if using 32-bit R, use RODBC package
    
    con = odbcConnectAccess(db_path)
    MatRate = sqlQuery(con, as.is = TRUE, qry)
    close(con)
    
  }
  
  return(MatRate[order(MatRate$BasePeriodID,MatRate$StockID,MatRate$Age,MatRate$TimeStep), ])
  
}
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Function to pull Mortality table from a FRAM database
pull_Mortality <- function(db_path, runID=NULL, stock=NULL, age=NULL, fishery=NULL, timestep=NULL) {
  
  # determine which arguments are provided
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
  
  # set up query
  if(length(x) == 0) {
    qry <- paste(sep = '',
                 "SELECT * FROM Mortality")
  }
  
  if(length(x) == 1) {
    x1_string <- toString(sprintf("%s", x[[1]]))
    qry = paste(sep = '',
                "SELECT Mortality.* ",
                "FROM Mortality ",
                "WHERE (((Mortality.",names(x[1]),") In (",x1_string,")));")
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
    
    qry = paste(sep = '',
                "SELECT Mortality.* ",
                "FROM Mortality ",
                where_clause,
                and_clause,");")
  }
  
  # run query
  if(version$arch == "x86_64") { # if running 64-bit R, use odbc package
    
    driverName = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", db_path)
    chnl = dbConnect(odbc(), .connection_string = driverName)
    Mort = dbGetQuery(chnl, qry)
    dbDisconnect(chnl)
    
  } else if(version$arch == "i386") { # if using 32-bit R, use RODBC package
    
    con = odbcConnectAccess(db_path)
    Mort = sqlQuery(con, as.is = TRUE, qry)
    close(con)
    
  }
  
  return(Mort[order(Mort$RunID,Mort$StockID,Mort$Age,Mort$FisheryID,Mort$TimeStep), ])
  
}
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Function to pull Natural Mortality table from a FRAM database
pull_NaturalMortality <- function(db_path, bpID) {
  
  # set up query
  if(missing(bpID)) {
    qry = paste(sep = '',
                "SELECT * FROM NaturalMortality")
  } else {
    bpID_string <- toString(sprintf("%s", bpID))
    qry = paste(sep = '',
                "SELECT NaturalMortality.* ",
                "FROM NaturalMortality ",
                "WHERE (((NaturalMortality.BasePeriodID) In (",bpID_string,")));")
  }
  
  # run query
  if(version$arch == "x86_64") { # if running 64-bit R, use odbc package
    
    driverName = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", db_path)
    chnl = dbConnect(odbc(), .connection_string = driverName)
    NatMort = dbGetQuery(chnl, qry)
    dbDisconnect(chnl)
    
  } else if(version$arch == "i386") { # if using 32-bit R, use RODBC package
    
    con = odbcConnectAccess(db_path)
    NatMort = sqlQuery(con, as.is = TRUE, qry)
    close(con)
    
  }
  
  return(NatMort[order(NatMort$BasePeriodID,NatMort$Age,NatMort$TimeStep), ])
  
}
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Function to pull RunID table from a FRAM database
pull_RunID <- function(db_path, runID) {
  
  # set up query
  if(missing(runID)) {
    qry = paste(sep = '',
                "SELECT * FROM RunID")
  } else {
    runID_string <- toString(sprintf("%s", runID))
    qry = paste(sep = '',
                "SELECT RunID.* ",
                "FROM RunID ",
                "WHERE (((RunID.RunID) In (",runID_string,")));")
  }
  
  # run query
  if(version$arch == "x86_64") { # if running 64-bit R, use odbc package
    
    driverName = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", db_path)
    chnl = dbConnect(odbc(), .connection_string = driverName)
    RunID = dbGetQuery(chnl, qry)
    dbDisconnect(chnl)
    
  } else if(version$arch == "i386") { # if using 32-bit R, use RODBC package
    
    con = odbcConnectAccess(db_path)
    RunID = sqlQuery(con, as.is = TRUE, qry)
    close(con)
    
  }
  
  return(RunID[order(RunID$RunID), ])
  
}
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Function to pull ShakerMortRate table from a FRAM database
pull_ShakerMortRate <- function(db_path, bpID) {
  
  # set up query
  if(missing(bpID)) {
    qry = paste(sep = '',
                "SELECT * FROM ShakerMortRate")
  } else {
    bpID_string <- toString(sprintf("%s", bpID))
    qry = paste(sep = '',
                "SELECT ShakerMortRate* ",
                "FROM ShakerMortRate ",
                "WHERE (((ShakerMortRate.BasePeriodID) In (",bpID_string,")));")
  }
  
  # run query
  if(version$arch == "x86_64") { # if running 64-bit R, use odbc package
    
    driverName = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", db_path)
    chnl = dbConnect(odbc(), .connection_string = driverName)
    ShakerMort = dbGetQuery(chnl, qry)
    dbDisconnect(chnl)
    
  } else if(version$arch == "i386") { # if using 32-bit R, use RODBC package
    
    con = odbcConnectAccess(db_path)
    ShakerMort = sqlQuery(con, as.is = TRUE, qry)
    close(con)
    
  }
  
  return(ShakerMort[order(ShakerMort$BasePeriodID,ShakerMort$FisheryID,ShakerMort$TimeStep), ])
  
}
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Function to pull Stock table from a FRAM database
pull_Stock <- function(db_path, StockVersion=5) {
  
  # set up query
  qry <- paste0("SELECT Stock.* ",
                "FROM Stock ",
                "WHERE (((Stock.StockVersion) = ",StockVersion,"));")
  
  # run query
  if(version$arch == "x86_64") { # if running 64-bit R, use odbc package
    
    driverName = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", db_path)
    chnl = dbConnect(odbc(), .connection_string = driverName)
    StkID = dbGetQuery(chnl, qry)
    dbDisconnect(chnl)
    
  } else if(version$arch == "i386") { # if using 32-bit R, use RODBC package
    
    con = odbcConnectAccess(db_path)
    StkID = sqlQuery(con, as.is = TRUE, qry)
    close(con)
    
  }
  
  return(StkID[order(StkID$StockID), ])
  
}
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Function to pull TerminalFisheryFlag table from a FRAM database
pull_TerminalFisheryFlag <- function(db_path, bpID) {
  
  # set up query
  if(missing(bpID)) {
    qry = paste(sep = '',
                "SELECT * FROM TerminalFisheryFlag")
  } else {
    bpID_string <- toString(sprintf("%s", bpID))
    qry = paste(sep = '',
                "SELECT TerminalFisheryFlag.* ",
                "FROM TerminalFisheryFlag ",
                "WHERE (((TerminalFisheryFlag.BasePeriodID) In (",bpID_string,")));")
  }
  
  # run query
  if(version$arch == "x86_64") { # if running 64-bit R, use odbc package
    
    driverName = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", db_path)
    chnl = dbConnect(odbc(), .connection_string = driverName)
    TermFlag = dbGetQuery(chnl, qry)
    dbDisconnect(chnl)
    
  } else if(version$arch == "i386") { # if using 32-bit R, use RODBC package
    
    con = odbcConnectAccess(db_path)
    TermFlag = sqlQuery(con, as.is = TRUE, qry)
    close(con)
    
  }
  
  return(TermFlag[order(TermFlag$BasePeriodID,TermFlag$FisheryID,TermFlag$TimeStep), ])
  
}
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Function to update a FRAM run for SRKW "Zero PS"

ZeroPS_SRKW <- function(db_path, runID) {
  
  runID_string <- toString(sprintf("%s", runID))
  
  # set up queries-------------------------------------------
  # query to zero out PS fisheries (with exception of 'A 12 Sport', 'NT HC Net', 'Tr HC Net', 'Tr HC Net', and 'Tr 13A Net')
  qry1 <- paste(sep = '',
                "UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 2, FisheryScalers.Quota = 0 ",
                "WHERE (((FisheryScalers.FisheryID)>=36 And (FisheryScalers.FisheryID) Not In (64,65,66,70,71)) AND ((FisheryScalers.RunID) In (",runID_string,"))) ",
                "OR (((FisheryScalers.FisheryID)=17) AND ((FisheryScalers.RunID) In (",runID_string,")) AND ((FisheryScalers.TimeStep) In (1,4)));")
  
  # query to update remaining ISBM flags from 2 to 1
  qry2 <- paste(sep = '',
                "UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 1 ",
                "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                "AND (((FisheryScalers.FisheryID) In (4,5,6,7,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,64,65,66,70,71)) ",
                "OR ((FisheryScalers.FisheryID)=17 And (FisheryScalers.TimeStep) In (2,3))) AND ((FisheryScalers.FisheryFlag)=2));")
  
  # query to update remaining ISBM flags from 8 to 7
  qry3 <- paste(sep = '',
                "UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 7 ",
                "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                "AND (((FisheryScalers.FisheryID) In (4,5,6,7,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,64,65,66,70,71)) ",
                "OR ((FisheryScalers.FisheryID)=17 AND (FisheryScalers.TimeStep) In (2,3))) AND ((FisheryScalers.FisheryFlag)=8));")
  
  # query to update remaining ISBM flags from 28 to 17
  qry4 <- paste(sep = '',
                "UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 17 ",
                "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                "AND (((FisheryScalers.FisheryID) In (4,5,6,7,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,64,65,66,70,71)) ",
                "OR ((FisheryScalers.FisheryID)=17 AND (FisheryScalers.TimeStep) In (2,3))) AND ((FisheryScalers.FisheryFlag)=28));")
  
  # query to zero out non-retention in PS fisheries
  qry5 <- paste(sep = '',
                "UPDATE NonRetention SET NonRetention.CNRInput1 = 0, NonRetention.CNRInput2 = 0, NonRetention.CNRInput3 = 0, NonRetention.CNRInput4 = 0 ",
                "WHERE (((NonRetention.RunID) In (",runID_string,")) AND ((NonRetention.FisheryID)>=36 AND (NonRetention.FisheryID) Not In (64,65,66,70,71)));")
  
  
  # run queries-------------------------------------------
  if(version$arch == "x86_64") { # if running 64-bit R, use odbc package
    
    driverName = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", db_path)
    chnl = dbConnect(odbc(), .connection_string = driverName)
    dbGetQuery(chnl, qry1)
    dbGetQuery(chnl, qry2)
    dbGetQuery(chnl, qry3)
    dbGetQuery(chnl, qry4)
    dbGetQuery(chnl, qry5)
    dbDisconnect(chnl)
    
  } else if(version$arch == "i386") { # if using 32-bit R, use RODBC package
    
    con = odbcConnectAccess(db_path)
    sqlQuery(con, as.is = TRUE, qry1)
    sqlQuery(con, as.is = TRUE, qry2)
    sqlQuery(con, as.is = TRUE, qry3)
    sqlQuery(con, as.is = TRUE, qry4)
    sqlQuery(con, as.is = TRUE, qry5)
    close(con)
    
  }
}
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Function to update a FRAM run for SRKW "Zero PS" based on results of 2021
# comanager discussions

ZeroPS_SRKW_2021 <- function(db_path, runID) {
  
  runID_string <- toString(sprintf("%s", runID))
  
  # set up queries-------------------------------------------
  # query to update remaining ISBM flags from 2 to 1
  qry1 <- paste(sep = '',
                "UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 1 ",
                "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                "AND ((FisheryScalers.FisheryFlag)=2) ",
                "AND ((FisheryScalers.FisheryID) Not In (1,2,3,8,9,10,11)));")
  
  # query to update remaining ISBM flags from 8 to 7
  qry2 <- paste(sep = '',
                "UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 7 ",
                "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                "AND ((FisheryScalers.FisheryFlag)=8) ",
                "AND ((FisheryScalers.FisheryID) Not In (1,2,3,8,9,10,11)));")
  
  # query to update remaining ISBM flags from 28 to 17
  qry3 <- paste(sep = '',
                "UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 17 ",
                "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                "AND ((FisheryScalers.FisheryFlag)=28) ",
                "AND ((FisheryScalers.FisheryID) Not In (1,2,3,8,9,10,11)));")
  
  # query to zero out non-retention in PS fisheries
  qry4 <- paste(sep = '',
                "UPDATE NonRetention SET NonRetention.CNRInput1 = 0, NonRetention.CNRInput2 = 0, NonRetention.CNRInput3 = 0, NonRetention.CNRInput4 = 0 ",
                "WHERE (((NonRetention.RunID) In (",runID_string,")) AND ((NonRetention.FisheryID)>=36 And (NonRetention.FisheryID) Not In (48,51,52,60,61,62,63,64,65,66,68,69,70,71)));")
  
  # query to zero out PS fisheries (with exception of terminal fisheries identified for exclusion from analysis - see "FRAM Fishery Exclusions_rev9.30.21.xlsx")
  qry5 <- paste(sep = '',
                "UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 2, FisheryScalers.Quota = 0 ",
                "WHERE ((FisheryScalers.RunID) In (",runID_string,") ",
                "AND ((FisheryScalers.TimeStep) In (1,4) AND ((FisheryScalers.FisheryID)=17 OR (FisheryScalers.FisheryID)>=36)) ",
                "OR ((FisheryScalers.RunID) In (",runID_string,") AND (FisheryScalers.TimeStep)=2 AND ((FisheryScalers.FisheryID)>=36 AND (FisheryScalers.FisheryID) Not In (48))) ",
                "OR ((FisheryScalers.RunID) In (",runID_string,") AND (FisheryScalers.TimeStep)=3 AND ((FisheryScalers.FisheryID)>=36 AND (FisheryScalers.FisheryID) Not In (48,51,52,60,61,62,63,64,65,66,68,69,70,71))));")
  
  
  # run queries-------------------------------------------
  if(version$arch == "x86_64") { # if running 64-bit R, use odbc package
    
    driverName = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", db_path)
    chnl = dbConnect(odbc(), .connection_string = driverName)
    dbGetQuery(chnl, qry1)
    dbGetQuery(chnl, qry2)
    dbGetQuery(chnl, qry3)
    dbGetQuery(chnl, qry4)
    dbGetQuery(chnl, qry5)
    dbDisconnect(chnl)
    
  } else if(version$arch == "i386") { # if using 32-bit R, use RODBC package
    
    con = odbcConnectAccess(db_path)
    sqlQuery(con, as.is = TRUE, qry1)
    sqlQuery(con, as.is = TRUE, qry2)
    sqlQuery(con, as.is = TRUE, qry3)
    sqlQuery(con, as.is = TRUE, qry4)
    sqlQuery(con, as.is = TRUE, qry5)
    close(con)
    
  }
}
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Function to update a FRAM run for "Zero PS"

ZeroPS <- function(db_path, runID) {
  
  runID_string <- toString(sprintf("%s", runID))
  
  # set up queries-------------------------------------------
  # query to zero out all PS fisheries
  qry1 <- paste(sep = '',
                "UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 2, FisheryScalers.Quota = 0 ",
                "WHERE ((FisheryScalers.FisheryID)>=36 AND ((FisheryScalers.RunID) In (",runID_string,"))) ",
                "OR (((FisheryScalers.FisheryID)=17) AND ((FisheryScalers.RunID) In (",runID_string,")) AND ((FisheryScalers.TimeStep) In (1,4)));")
  
  # query to update remaining ISBM flags from 2 to 1
  qry2 <- paste(sep = '',
                "UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 1 ",
                "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                "AND (((FisheryScalers.FisheryID) In (4,5,6,7,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35)) ",
                "OR ((FisheryScalers.FisheryID)=17 And (FisheryScalers.TimeStep) In (2,3))) AND ((FisheryScalers.FisheryFlag)=2));")
  
  # query to update remaining ISBM flags from 8 to 7
  qry3 <- paste(sep = '',
                "UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 7 ",
                "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                "AND (((FisheryScalers.FisheryID) In (4,5,6,7,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35)) ",
                "OR ((FisheryScalers.FisheryID)=17 And (FisheryScalers.TimeStep) In (2,3))) AND ((FisheryScalers.FisheryFlag)=8));")
  
  # query to update remaining ISBM flags from 28 to 17
  qry4 <- paste(sep = '',
                "UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 17 ",
                "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                "AND (((FisheryScalers.FisheryID) In (4,5,6,7,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35)) ",
                "OR ((FisheryScalers.FisheryID)=17 And (FisheryScalers.TimeStep) In (2,3))) AND ((FisheryScalers.FisheryFlag)=28));")
  
  # query to zero out non-retention in PS fisheries
  qry5 <- paste(sep = '',
                "UPDATE NonRetention SET NonRetention.CNRInput1 = 0, NonRetention.CNRInput2 = 0, NonRetention.CNRInput3 = 0, NonRetention.CNRInput4 = 0 ",
                "WHERE (((NonRetention.RunID) In (",runID_string,")) AND ((NonRetention.FisheryID)>=36));")
  
  
  # run queries-------------------------------------------
  if(version$arch == "x86_64") { # if running 64-bit R, use odbc package
    
    driverName = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", db_path)
    chnl = dbConnect(odbc(), .connection_string = driverName)
    dbGetQuery(chnl, qry1)
    dbGetQuery(chnl, qry2)
    dbGetQuery(chnl, qry3)
    dbGetQuery(chnl, qry4)
    dbGetQuery(chnl, qry5)
    dbDisconnect(chnl)
    
  } else if(version$arch == "i386") { # if using 32-bit R, use RODBC package
    
    con = odbcConnectAccess(db_path)
    sqlQuery(con, as.is = TRUE, qry1)
    sqlQuery(con, as.is = TRUE, qry2)
    sqlQuery(con, as.is = TRUE, qry3)
    sqlQuery(con, as.is = TRUE, qry4)
    sqlQuery(con, as.is = TRUE, qry5)
    close(con)
    
  }
}
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Function to calculate SRFI
# Requires: db_path, series of RunIDs, SRFI_BP_ER, and an outfile path

calc_SRFI <- function(db_path, runID, SRFI_BP_ER, outfile) {
  SRFI_log <- data.frame(matrix(ncol = 5, nrow = length(runID)))
  colnames(SRFI_log) <- c("RunID", "RunName", "SRFI_BP_ER", "SRFI_ER", "SRFI")

  for(j in 1:length(runID)) {
    run_id <- runID[j]

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

    SRFI <- round(SRFI_ER / SRFI_BP_ER, 4)

    SRFI_log_j <- c(run_id, RunID[RunID$RunID == run_id, 4], SRFI_BP_ER, SRFI_ER, SRFI)

    SRFI_log[j, ] <- SRFI_log_j
  }

  write.csv(SRFI_log, outfile)
  return(SRFI_log[order(SRFI_log$RunID), ])
}
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Function to calculate SRFI base period ER (1988-1993)
# Requires: db_path

calc_SRFI_BP_ER <- function(db_path) {
  run_IDs <- pull_RunID(db_path)
  run_IDs <- run_IDs[run_IDs$RunYear >= 1988 & run_IDs$RunYear <= 1993, c(2,4,6,11)]
  
  SRFI_log <- data.frame(matrix(ncol = 3, nrow = dim(run_IDs)[1]))
  colnames(SRFI_log) <- c("RunID", "Year", "SRFI_ER")
  
  for(i in 1:dim(run_IDs)[1]) {
    run_id <- run_IDs$RunID[i]
    
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
    for(j in 1:dim(Mort)[1]) {
      if(Mort$TerminalFlag[j] == 1) {
        Mort$AEQ[j] <- 1 
      }
    }
    
    # sum total mort and AEQ mort
    Mort$Tot_Mort <- rowSums(Mort[ ,c(7:10,12:15)])
    Mort$AEQ_Mort <- Mort$Tot_Mort * Mort$AEQ
    
    Age_3_4_Mort <- sum(Mort[Mort$Age %in% c(3,4) & Mort$TimeStep %in% c(1:3), 20])
    Tot_AEQ_Mort <- sum(Mort[ , 20])
    Escapement <- sum(Esc$Escapement)
    SRFI_ER <- Age_3_4_Mort / (Tot_AEQ_Mort + Escapement)
    
    # record values
    SRFI_log_i <- c(run_id, run_IDs[i,4], SRFI_ER)
    SRFI_log[i, ] <- SRFI_log_i
  }
  
  return(mean(as.numeric(SRFI_log$SRFI_ER)))
}
#----------------------------------------------------------------------------#

