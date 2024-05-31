#--------------------------------------------------------------------------------------------------------#
# --------------------------------------- SRKW FUNCTIONS LIBRARY --------------------------------------- #
#--------------------------------------------------------------------------------------------------------#
#
#  1.  Quotas_to_scalers (db_path, runID)
#  2.  ZeroSEAK_SRKW (db_path, runID)
#  3.  ZeroBC_SRKW (db_path, runID)
#  4.  ZeroPFMC_SRKW (db_path, runID)
#  5.  ZeroPS_SRKW_2021 (db_path, runID)
#  6.  ZeroPS_SRKW_2021_TS3 (db_path, runID)
#  7.  ZeroUS_SRKW( db_path, runID)
#  8.  ZeroALL_SRKW (db_path, runID)
#  9.  CENTV_SRKW (SRFC_trun, SRFC_harv, ZeroPFMC_flag, RunYear)
#  10. NCA_SRKW (KRFC_cohort, KRFC_harv, ROPI, ZeroPFMC_flag, RunYear, process_harvest)
#  11. SRKW_FRAM_Shelton_TS1 (numAlt, runID, paths_in)
#  12. SRKW_FRAM_Shelton (Years, RunType_base, RunType_zero, ZeroPFMC_flag, paths_in, exc_sac)
#  13. to.matrix (x, years)
#  14. SRKW_SummarizR (Years, regions, in_data, mod_type)
#  15. SRKW_extract (SRKW_out, scenarios, regions, summaries)
#  16. SRKW_BoxPlots (SRKW_out, metric, scenarios, regions, by_scenario, by_region, save_to_file, 
#                     out_path, RunType_base_name)
#
#--------------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------------#
# Function to update all fishery flags in a FRAM database (db_path) for a given set of RunIDS (runID)
# from quotas (2,8,28) to scalers (1,7,17).
#--------------------------------------------------------------------------------------------------------#

Quotas_to_scalers <- function(db_path, runID) {
  
  runID_string <- toString(sprintf("%s", runID))
  
  # set up queries-------------------------------------------
  # query to update all fishery flags of 2 to 1
  qry1 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 1 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND ((FisheryScalers.FisheryFlag)=2));")
  
  # query to update all fishery flags of 8 to 7
  qry2 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 7 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND ((FisheryScalers.FisheryFlag)=8));")
  
  # query to update all fishery flags of 28 to 17
  qry3 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 17 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND ((FisheryScalers.FisheryFlag)=28));")
  
  
  # run queries-------------------------------------------
  if(version$arch == "x86_64") { # if running 64-bit R, use odbc package
    
    driverName = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", db_path)
    chnl = dbConnect(odbc(), .connection_string = driverName)
    dbGetQuery(chnl, qry1)
    dbGetQuery(chnl, qry2)
    dbGetQuery(chnl, qry3)
    dbDisconnect(chnl)
    
  } else if(version$arch == "i386") { # if using 32-bit R, use RODBC package
    
    con = odbcConnectAccess(db_path)
    sqlQuery(con, as.is = TRUE, qry1)
    sqlQuery(con, as.is = TRUE, qry2)
    sqlQuery(con, as.is = TRUE, qry3)
    close(con)
    
  }
}
#--------------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------------#
# Function to update one or more FRAM runs (runID) from a given database (db_path) to zero out
# SEAK fisheries for SRKW assessments
#--------------------------------------------------------------------------------------------------------#

ZeroSEAK_SRKW <- function(db_path, runID) {
  
  runID_string <- toString(sprintf("%s", runID))
  
  # set up queries-------------------------------------------
  # query to update ISBM flags from 2 to 1
  qry1 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 1 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND ((FisheryScalers.FisheryFlag)=2) ",
                 "AND ((FisheryScalers.FisheryID) Not In (1,2,3,8,9,10,11)));")
  
  # query to update ISBM flags from 8 to 7
  qry2 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 7 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND ((FisheryScalers.FisheryFlag)=8) ",
                 "AND ((FisheryScalers.FisheryID) Not In (1,2,3,8,9,10,11)));")
  
  # query to update ISBM flags from 28 to 17
  qry3 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 17 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND ((FisheryScalers.FisheryFlag)=28) ",
                 "AND ((FisheryScalers.FisheryID) Not In (1,2,3,8,9,10,11)));")
  
  # query to zero out non-retention in SEAK fisheries
  qry4 <- paste0("UPDATE NonRetention SET NonRetention.CNRInput1 = 0, NonRetention.CNRInput2 = 0, NonRetention.CNRInput3 = 0, NonRetention.CNRInput4 = 0 ",
                 "WHERE (((NonRetention.RunID) In (",runID_string,")) AND ((NonRetention.FisheryID) <= 3));")
  
  # query to zero out SEAK fisheries
  qry5 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 2, FisheryScalers.Quota = 0 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) AND ((FisheryScalers.FisheryID) <= 3));")
  
  
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
#--------------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------------#
# Function to update one or more FRAM runs (runID) from a given database (db_path) to zero out
# BC fisheries for SRKW assessments
#--------------------------------------------------------------------------------------------------------#

ZeroBC_SRKW <- function(db_path, runID) {
  
  runID_string <- toString(sprintf("%s", runID))
  
  # set up queries-------------------------------------------
  # query to update ISBM flags from 2 to 1
  qry1 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 1 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND ((FisheryScalers.FisheryFlag)=2) ",
                 "AND ((FisheryScalers.FisheryID) Not In (1,2,3,8,9,10,11)));")
  
  # query to update ISBM flags from 8 to 7
  qry2 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 7 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND ((FisheryScalers.FisheryFlag)=8) ",
                 "AND ((FisheryScalers.FisheryID) Not In (1,2,3,8,9,10,11)));")
  
  # query to update ISBM flags from 28 to 17
  qry3 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 17 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND ((FisheryScalers.FisheryFlag)=28) ",
                 "AND ((FisheryScalers.FisheryID) Not In (1,2,3,8,9,10,11)));")
  
  # query to zero out non-retention in BC fisheries
  qry4 <- paste0("UPDATE NonRetention SET NonRetention.CNRInput1 = 0, NonRetention.CNRInput2 = 0, NonRetention.CNRInput3 = 0, NonRetention.CNRInput4 = 0 ",
                 "WHERE (((NonRetention.RunID) In (",runID_string,")) AND ((NonRetention.FisheryID) In (4,5,6,7,8,9,10,11,12,13,14,15)));")
  
  # query to zero out BC fisheries
  qry5 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 2, FisheryScalers.Quota = 0 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) AND ((FisheryScalers.FisheryID) In (4,5,6,7,8,9,10,11,12,13,14,15)));")
  
  
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
#--------------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------------#
# Function to update one or more FRAM runs (runID) from a given database (db_path) to zero out
# PFMC fisheries for SRKW assessments
#--------------------------------------------------------------------------------------------------------#

ZeroPFMC_SRKW <- function(db_path, runID) {
  
  runID_string <- toString(sprintf("%s", runID))
  
  # set up queries-------------------------------------------
  # query to update ISBM flags from 2 to 1
  qry1 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 1 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND ((FisheryScalers.FisheryFlag)=2) ",
                 "AND ((FisheryScalers.FisheryID) Not In (1,2,3,8,9,10,11)));")
  
  # query to update ISBM flags from 8 to 7
  qry2 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 7 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND ((FisheryScalers.FisheryFlag)=8) ",
                 "AND ((FisheryScalers.FisheryID) Not In (1,2,3,8,9,10,11)));")
  
  # query to update ISBM flags from 28 to 17
  qry3 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 17 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND ((FisheryScalers.FisheryFlag)=28) ",
                 "AND ((FisheryScalers.FisheryID) Not In (1,2,3,8,9,10,11)));")
  
  # query to zero out non-retention in PFMC fisheries
  qry4 <- paste0("UPDATE NonRetention SET NonRetention.CNRInput1 = 0, NonRetention.CNRInput2 = 0, NonRetention.CNRInput3 = 0, NonRetention.CNRInput4 = 0 ",
                 "WHERE (((NonRetention.RunID) In (",runID_string,")) AND ((NonRetention.FisheryID) In (16,17,18,20,21,22,26,27,30,31,32,33,34,35)));")
  
  # query to zero out PFMC fisheries
  qry5 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 2, FisheryScalers.Quota = 0 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) AND ((FisheryScalers.FisheryID) In (16,18,20,21,22,26,27,30,31,32,33,34,35))) ",
                 "OR (((FisheryScalers.RunID) In (",runID_string,")) AND ((FisheryScalers.FisheryID)=17) AND ((FisheryScalers.TimeStep) In (2,3)));")
  
  
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
#--------------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------------#
# Function to update one or more FRAM runs (runID) from a given database (db_path) to zero out
# Puget Sound fisheries based on results of 2021 comanager discussions regarding which terminal
# fisheries to leave 'on'. See "FRAM Fishery Exclusions_rev9.30.21.xlsx"
#--------------------------------------------------------------------------------------------------------#

ZeroPS_SRKW_2021 <- function(db_path, runID) {
  
  runID_string <- toString(sprintf("%s", runID))
  
  # set up queries-------------------------------------------
  # query to update remaining ISBM flags from 2 to 1
  qry1 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 1 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND ((FisheryScalers.FisheryFlag)=2) ",
                 "AND ((FisheryScalers.FisheryID) Not In (1,2,3,8,9,10,11)));")
  
  # query to update remaining ISBM flags from 8 to 7
  qry2 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 7 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND ((FisheryScalers.FisheryFlag)=8) ",
                 "AND ((FisheryScalers.FisheryID) Not In (1,2,3,8,9,10,11)));")
  
  # query to update remaining ISBM flags from 28 to 17
  qry3 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 17 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND ((FisheryScalers.FisheryFlag)=28) ",
                 "AND ((FisheryScalers.FisheryID) Not In (1,2,3,8,9,10,11)));")
  
  # query to zero out non-retention in PS fisheries
  qry4 <- paste0("UPDATE NonRetention SET NonRetention.CNRInput1 = 0, NonRetention.CNRInput2 = 0, NonRetention.CNRInput3 = 0, NonRetention.CNRInput4 = 0 ",
                 "WHERE (((NonRetention.RunID) In (",runID_string,")) AND ((NonRetention.FisheryID)>=36 And (NonRetention.FisheryID) Not In (48,51,52,60,61,62,63,64,65,66,68,69,70,71)));")
  
  # query to zero out PS fisheries (with exception of terminal fisheries identified for exclusion from analysis - see "FRAM Fishery Exclusions_rev9.30.21.xlsx")
  qry5 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 2, FisheryScalers.Quota = 0 ",
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
#--------------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------------#
# Function to update one or more FRAM runs (runID) from a given database (db_path) to zero out
# Puget Sound fisheries IN TS3 ONLY based on results of 2021 comanager discussions regarding which 
# terminal fisheries to leave 'on'. See "FRAM Fishery Exclusions_rev9.30.21.xlsx"
#--------------------------------------------------------------------------------------------------------#

ZeroPS_SRKW_2021_TS3 <- function(db_path, runID) {
  
  runID_string <- toString(sprintf("%s", runID))
  
  # set up queries-------------------------------------------
  # query to update remaining ISBM flags from 2 to 1
  qry1 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 1 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND ((FisheryScalers.TimeStep)=3) ",
                 "AND ((FisheryScalers.FisheryFlag)=2) ",
                 "AND ((FisheryScalers.FisheryID) Not In (1,2,3,8,9,10,11)));")
  
  # query to update remaining ISBM flags from 8 to 7
  qry2 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 7 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND ((FisheryScalers.TimeStep)=3) ",
                 "AND ((FisheryScalers.FisheryFlag)=8) ",
                 "AND ((FisheryScalers.FisheryID) Not In (1,2,3,8,9,10,11)));")
  
  # query to update remaining ISBM flags from 28 to 17
  qry3 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 17 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND ((FisheryScalers.TimeStep)=3) ",
                 "AND ((FisheryScalers.FisheryFlag)=28) ",
                 "AND ((FisheryScalers.FisheryID) Not In (1,2,3,8,9,10,11)));")
  
  # query to zero out non-retention in PS fisheries
  qry4 <- paste0("UPDATE NonRetention SET NonRetention.CNRInput1 = 0, NonRetention.CNRInput2 = 0, NonRetention.CNRInput3 = 0, NonRetention.CNRInput4 = 0 ",
                 "WHERE (((NonRetention.RunID) In (",runID_string,")) AND ((NonRetention.TimeStep)=3) AND ((NonRetention.FisheryID)>=36 And (NonRetention.FisheryID) Not In (48,51,52,60,61,62,63,64,65,66,68,69,70,71)));")
  
  # query to zero out PS fisheries (with exception of terminal fisheries identified for exclusion from analysis - see "FRAM Fishery Exclusions_rev9.30.21.xlsx")
  qry5 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 2, FisheryScalers.Quota = 0 ",
                 "WHERE ((FisheryScalers.RunID) In (",runID_string,") AND (FisheryScalers.TimeStep)=3 AND ((FisheryScalers.FisheryID)>=36 AND (FisheryScalers.FisheryID) Not In (48,51,52,60,61,62,63,64,65,66,68,69,70,71)));")
  
  
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
#--------------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------------#
# Function to update one or more FRAM runs (runID) from a given database (db_path) to zero out all
# U.S. fisheries for SRKW assessments. Decisions on closing/not closing specific Puget Sound fisheries 
# were informed based on 2021 comanager discussions. See "FRAM Fishery Exclusions_rev9.30.21.xlsx"
#--------------------------------------------------------------------------------------------------------#

ZeroUS_SRKW <- function(db_path, runID) {
  
  runID_string <- toString(sprintf("%s", runID))
  
  # set up queries-------------------------------------------
  # query to update ISBM flags from 2 to 1
  qry1 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 1 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND ((FisheryScalers.FisheryFlag)=2) ",
                 "AND ((FisheryScalers.FisheryID) Not In (1,2,3,8,9,10,11)));")
  
  # query to update ISBM flags from 8 to 7
  qry2 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 7 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND ((FisheryScalers.FisheryFlag)=8) ",
                 "AND ((FisheryScalers.FisheryID) Not In (1,2,3,8,9,10,11)));")
  
  # query to update ISBM flags from 28 to 17
  qry3 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 17 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND ((FisheryScalers.FisheryFlag)=28) ",
                 "AND ((FisheryScalers.FisheryID) Not In (1,2,3,8,9,10,11)));")
  
  # query to zero out non-retention in US fisheries
  qry4 <- paste0("UPDATE NonRetention SET NonRetention.CNRInput1 = 0, NonRetention.CNRInput2 = 0, NonRetention.CNRInput3 = 0, NonRetention.CNRInput4 = 0 ",
                 "WHERE (((NonRetention.RunID) In (",runID_string,")) AND (((NonRetention.FisheryID) <= 3) OR ((NonRetention.FisheryID) >= 16 And (NonRetention.FisheryID) Not In (48,51,52,60,61,62,63,64,65,66,68,69,70,71))));")
  
  # query to zero out SEAK, PFMC, and PS fisheries 
  # (with exception of select PS terminal fisheries identified for exclusion from analysis - see "FRAM Fishery Exclusions_rev9.30.21.xlsx")
  qry5 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 2, FisheryScalers.Quota = 0 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND (((FisheryScalers.FisheryID) <= 3) ", # SEAK
                 "OR ((FisheryScalers.FisheryID) In (16,17,18,19,20,21,22,23,24,25,26,27,30,31,32,33,34,35)) ", # PFMC & winter troll & WA coast
                 "OR ((FisheryScalers.TimeStep) In (1,4) AND (FisheryScalers.FisheryID)>=36) ", # PS winter
                 "OR ((FisheryScalers.TimeStep)=2 AND ((FisheryScalers.FisheryID)>=36 AND (FisheryScalers.FisheryID) Not In (48))) ", # PS spring
                 "OR ((FisheryScalers.TimeStep)=3 AND ((FisheryScalers.FisheryID)>=36 AND (FisheryScalers.FisheryID) Not In (48,51,52,60,61,62,63,64,65,66,68,69,70,71)))));") # PS summer
  
  
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
#--------------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------------#
# Function to update one or more FRAM runs (runID) from a given database (db_path) to zero out all 
# U.S. and BC fisheries for SRKW assessments (with the exception of agreed-to PS terminal fisheries).
# Decisions on closing/not closing spcific Puget Sound fisheries were informed based on 2021 comanager 
# discussions. See "FRAM Fishery Exclusions_rev9.30.21.xlsx"
#--------------------------------------------------------------------------------------------------------#

ZeroALL_SRKW <- function(db_path, runID) {
  
  runID_string <- toString(sprintf("%s", runID))
  
  # set up queries-------------------------------------------
  # query to update ISBM flags from 2 to 1
  qry1 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 1 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND ((FisheryScalers.FisheryFlag)=2) ",
                 "AND ((FisheryScalers.FisheryID) Not In (1,2,3,8,9,10,11)));")
  
  # query to update ISBM flags from 8 to 7
  qry2 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 7 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND ((FisheryScalers.FisheryFlag)=8) ",
                 "AND ((FisheryScalers.FisheryID) Not In (1,2,3,8,9,10,11)));")
  
  # query to update ISBM flags from 28 to 17
  qry3 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 17 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) ",
                 "AND ((FisheryScalers.FisheryFlag)=28) ",
                 "AND ((FisheryScalers.FisheryID) Not In (1,2,3,8,9,10,11)));")
  
  # query to zero out non-retention in all US & BC fisheries (with exception of select PS terminal fisheries)
  qry4 <- paste0("UPDATE NonRetention SET NonRetention.CNRInput1 = 0, NonRetention.CNRInput2 = 0, NonRetention.CNRInput3 = 0, NonRetention.CNRInput4 = 0 ",
                 "WHERE (((NonRetention.RunID) In (",runID_string,")) AND ((NonRetention.FisheryID) Not In (48,51,52,60,61,62,63,64,65,66,68,69,70,71)));")
  
  # query to zero out all US & BC fisheries (with exception of select PS terminal fisheries)
  qry5 <- paste0("UPDATE FisheryScalers SET FisheryScalers.FisheryFlag = 2, FisheryScalers.Quota = 0 ",
                 "WHERE (((FisheryScalers.RunID) In (",runID_string,")) AND ((FisheryScalers.TimeStep) In (1,4))) ",
                 "OR (((FisheryScalers.RunID) In (",runID_string,")) AND ((FisheryScalers.FisheryID) Not In (48)) AND ((FisheryScalers.TimeStep) In (2))) ",
                 "OR (((FisheryScalers.RunID) In (",runID_string,")) AND ((FisheryScalers.FisheryID) Not In (48,51,52,60,61,62,63,64,65,66,68,69,70,71)) AND ((FisheryScalers.TimeStep) In (3)));")
                 
  
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
#--------------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------------#
# Function to process cohort sizes and harvest for 'CENTV' Shelton stock (basically SRFC)
#
# Arguments:
#   1. SRFC_trun - SRFC river run data from the SRKW_Input.xlsx file.
#   2. SRFC_harv - SRFC harvest data from the SRKW_Input.xlsx file.
#   3. ZeroPFMC_flag - flag indicating whether the zero fishing scenario includes closure of PFMC fisheries
#       0 = no, PFMC fisheries remain open in the zero fishing scenario
#       1 = yes, PFMC fisheries are closed in the zero fishing scenario
#   4. RunYear - a single RunYear value for the year to be processed.
#
# Output:
#   1. centv_out - list of two data frames:
#       - CENTV_cohort - data frame of 'CENTV' starting cohorts by age and time step for the specified 
#         year and PFMC harvest scenario.
#       - CENTV_harvest - data frame of 'CENTV' harvest by age and time step for the specified year and
#         PFMC harvest scenario.
#--------------------------------------------------------------------------------------------------------#

CENTV_SRKW <- function(SRFC_trun, SRFC_harv, ZeroPFMC_flag, RunYear) {
  # 'CENTV" Shelton Stock
  # assume 20% anual natural mortality; equates to 1.842347% monthly
  m = 1-(1-0.2)^(1/12)#0.01842347
  
  # assume all harvest takes place on first day of month 
  # August 1 abundance = river run size scaled up to account for one month's mortality, plus August harvest
  # July 1 abundance = August 1 abundance scaled up to account for one month's mortallity, plus July harvest, etc
  
  Month <- c(8,7,6,5,4,3,2,1,12,11,10,9) # monthly steps for backwards cohort reconstruction
  
  TS_lut <- data.frame(TimeStep = c(1,1,1,1,1,1,1,2,2,3,3,3), 
                       Month = c(10,11,12,1,2,3,4,5,6,7,8,9))
  
  SRFC <- as.data.frame(Month) %>% 
    mutate(RunYear = RunYear,
           SheltonStk = "CENTV",
           Age = 3, .before = Month) %>% 
    mutate(StartCohort = NA)
  
  for(i in 1:length(SRFC$Month)) {
    if(i == 1) {
      SRFC_TRS <- SRFC_trun %>% filter(Year == RunYear) %>% pull()
      SRFC$StartCohort[i] <- SRFC_TRS / (1-m) + SRFC_harv %>% filter(year == RunYear & month == SRFC$Month[i]) %>% pull()
    } else {
      SRFC$StartCohort[i] <- SRFC$StartCohort[i-1] / (1-m) + SRFC_harv %>% filter(year == RunYear & month == SRFC$Month[i]) %>% pull()
    }
  }
  
  if(ZeroPFMC_flag == 0) {
    CENTV_cohort <- SRFC %>% 
      left_join(., TS_lut %>% filter(Month %in% c(10,5,7)), by = join_by(Month)) %>% 
      drop_na() %>% 
      select(-Month) %>% 
      relocate(TimeStep, .before = StartCohort) %>% 
      arrange(TimeStep)
  } else if(ZeroPFMC_flag == 1) {
    for(i in 11:1) {
      SRFC$StartCohort[i] <- SRFC$StartCohort[i+1] * (1-m)
    }
    CENTV_cohort <- SRFC %>% 
      left_join(., TS_lut %>% filter(Month %in% c(10,5,7)), by = join_by(Month)) %>% 
      drop_na() %>% 
      select(-Month) %>% 
      relocate(TimeStep, .before = StartCohort) %>% 
      arrange(TimeStep)
  }
  
  CENTV_harvest <- SRFC_harv %>% 
    filter(year == RunYear) %>% 
    left_join(., TS_lut, by = join_by(month == Month)) %>% 
    group_by(year, TimeStep) %>% 
    summarise(Mort = sum(H), .groups = "drop") %>% 
    rename(RunYear = year) %>% 
    mutate(SheltonStk = "CENTV",
           Age = 3, .after = RunYear)
  
  centv_out <- list(CENTV_cohort = CENTV_cohort,
                    CENTV_harvest = as.data.frame(CENTV_harvest))
  
  return(centv_out)
}
#--------------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------------#
# Function to process cohort sizes and harvest for 'NCA' Shelton stock (KRFC + Rogue)
#
# Arguments:
#   1. KRFC_cohort - KRFC cohort data from the SRKW_Input.xlsx file.
#   2. KRFC_harv - KRFC harvest data from the SRKW_Input.xlsx file.
#   3. ROPI - Rogue ocean production index data from the SRKW_Input.xlsx file.
#   4. ZeroPFMC_flag - flag indicating whether the zero fishing scenario includes closure of PFMC fisheries
#       0 = no, PFMC fisheries remain open in the zero fishing scenario
#       1 = yes, PFMC fisheries are closed in the zero fishing scenario
#   5. RunYear - a single RunYear value for the year to be processed.
#   6. process_harvest - flag indicating whether or not to process harvest
#       0 = no
#       1 = yes
#
# Output:
#   1. nca_out - list of one or two data frames:
#       - NCA_cohort - data frame of 'NCA' starting cohorts by age and time step for the specified 
#         year and PFMC harvest scenario.
#       - NCA_harvest - data frame of 'NCA' harvest by age and time step for the specified year and
#         PFMC harvest scenario, only included if 'process_harvest' argument == 1.
#--------------------------------------------------------------------------------------------------------#

NCA_SRKW <- function(KRFC_cohort, KRFC_harv, ROPI, ZeroPFMC_flag, RunYear, process_harvest=1) {
  
  TS_lut <- data.frame(TimeStep = c(1,1,1,1,1,1,1,2,2,3,3,3), 
                       Month = c(10,11,12,1,2,3,4,5,6,7,8,9))
  
  sep <- KRFC_cohort %>% 
    filter(mgmtyr == RunYear & month == 9) %>% 
    select(mgmtyr, age, totalpop) %>% 
    rename(sep = totalpop)
  
  ROPI <- ROPI %>% 
    filter(Year == RunYear) %>% 
    pivot_longer(2:4, names_to = "Age", values_to = "ROPI") %>% 
    mutate(Age = as.numeric(substring(Age, 6)))
  
  if(ZeroPFMC_flag == 0) {
    NCA_cohort <- KRFC_cohort %>% 
      filter(mgmtyr == RunYear,
             month %in% c(9,10,5,7)) %>% 
      left_join(., sep, by = join_by(mgmtyr, age)) %>% 
      left_join(., ROPI, by = join_by(mgmtyr == Year, age == Age), keep = FALSE) %>% 
      mutate(ROPI = ROPI * (totalpop / sep)) %>% 
      mutate(StartCohort = totalpop + ROPI) %>% 
      filter(month != 9) %>% 
      left_join(., TS_lut %>% filter(Month %in% c(10,5,7)), by = join_by(month == Month)) %>% 
      rename(RunYear = mgmtyr, Age = age) %>% 
      mutate(SheltonStk = "NCA") %>% 
      select(RunYear, SheltonStk, Age, TimeStep, StartCohort)
  } else if(ZeroPFMC_flag == 1) {
    NCA_cohort <- KRFC_cohort %>% 
      filter(mgmtyr == RunYear) %>% 
      mutate(yearly_nat_mort = ifelse(age == 3 & month %in% c(9:12, 1:4), 0.5, 0.2)) %>% 
      mutate(totalpop_zero = NA) %>% 
      select(mgmtyr, month, age, totalpop, yearly_nat_mort, totalpop_zero)
    
    for(i in 1:dim(NCA_cohort)[1]) {
      if(NCA_cohort$month[i] == 9) {
        NCA_cohort$totalpop_zero[i] <- NCA_cohort$totalpop[i]
      } else {
        age_i <- NCA_cohort$age[i]
        prior_month <- ifelse(NCA_cohort$month[i] == 1, 12, NCA_cohort$month[i] - 1)
        prior_month_pop <- NCA_cohort %>% filter(age == age_i & month == prior_month) %>% pull(totalpop_zero)
        nat_mort <- NCA_cohort %>% filter(age == age_i & month == prior_month) %>% pull(yearly_nat_mort)
        NCA_cohort$totalpop_zero[i] <- prior_month_pop * (1 - nat_mort) ^ (1/12)
      }
    }
    
    NCA_cohort <- NCA_cohort %>% 
      left_join(., sep, by = join_by(mgmtyr, age)) %>% 
      left_join(., ROPI, by = join_by(mgmtyr == Year, age == Age), keep = FALSE) %>% 
      mutate(ROPI = ROPI * (totalpop_zero / sep)) %>% 
      mutate(StartCohort = totalpop_zero + ROPI) %>% 
      filter(month != 9) %>% 
      left_join(., TS_lut %>% filter(Month %in% c(10,5,7)), by = join_by(month == Month)) %>% 
      rename(RunYear = mgmtyr, Age = age) %>% 
      mutate(SheltonStk = "NCA") %>% 
      select(RunYear, SheltonStk, Age, TimeStep, StartCohort) %>% 
      drop_na()
  }
  
  if(process_harvest == 1) {
    NCA_harvest <- KRFC_cohort %>% 
      filter(mgmtyr == RunYear) %>% 
      left_join(., sep, by = join_by(mgmtyr, age)) %>% 
      left_join(., ROPI, by = join_by(mgmtyr == Year, age == Age), keep = FALSE) %>% 
      mutate(ROPI = ROPI * (totalpop / sep)) %>% 
      left_join(., KRFC_harv %>% filter(mgmtyr == RunYear) %>% select(mgmtyr, month, age, oceanimpacts, ER),
                by = join_by(mgmtyr, month, age)) %>% 
      mutate(ROPI_harv = ROPI * ER) %>% 
      mutate(Mort = oceanimpacts + ROPI_harv) %>% 
      left_join(., TS_lut, by = join_by(month == Month)) %>% 
      group_by(mgmtyr, age, TimeStep) %>% 
      summarise(Mort = sum(Mort), .groups = "drop") %>% 
      rename(RunYear = mgmtyr,
             Age = age) %>% 
      mutate(SheltonStk = "NCA", .after = RunYear)
  }
  
  nca_out <- list(NCA_cohort = NCA_cohort,
                  if(exists("NCA_harvest")) {NCA_harvest = NCA_harvest})
  
  return(nca_out)
}
#--------------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------------#
# Function to calculate TS1 starting abundances using FRAM/Shelton for preseason STT modeling
#
# Arguments:
#   1. numAlt - number of model run alternative to evaluate; must be 1, 2, or 3.
#   2. runID - vector of runIDs; length must be equal to numAlt.
#   3. paths_in - list of paths where element 1 is the path to the FRAM database and elements 2-4 are
#                 paths to the model run specific 'SRKW_In_ChinXXXX.xlsx' files. Length must be at least
#                 equal to numAlt + 1.
#
# Output:
#   1. fram_shelton_TS1_out - list of two data frames:
#       - SRKW_Abund_TS1 - data frame of TS1 starting abundances by region for each model run/alternative
#       - SRKW_Abund_by_stock - data frame of starting abundances by Shelton stock, FRAM stock, age, and
#                               time step for each model run/alternative
#--------------------------------------------------------------------------------------------------------#

SRKW_FRAM_Shelton_TS1 <- function(numAlt, runID, paths_in) {
  # check inputs
  if(length(runID) < numAlt) {
    stop("ERROR: Check RunIDs in 'runID' - not enough provided for number of Alternatives specified.")
  }
  if(length(paths_in) < numAlt+1) {
    stop("ERROR: Check paths in 'paths_in' - not enough elements for number of Alternatives specified.")
  }
  
  # Set infile paths
  DBpath <- paths_in[[1]]
  
  if(numAlt == 3) {
    infiles <- list(paths_in[[2]], paths_in[[3]], paths_in[[4]])
  } else if (numAlt == 2) {
    infiles <- list(paths_in[[2]], paths_in[[3]])
  } else if (numAlt == 1) {
    infiles <- list(paths_in[[2]])
  }
  
  for(z in 1:numAlt) {
    infile <- infiles[[z]]
    
    # Extract relevant data from input file
    Stock_xwalk <- read_excel(infile, sheet="FRAM to Shelton Stks")
    UpColSpr <- read_excel(infile, sheet="Up Col Spr Abundances")
    SRFC_trun <- read_excel(infile, sheet="SRFC Run Dat")
    SRFC_harv <- read_excel(infile, sheet="SRFC Harv Dat")
    KRFC_cohort <- read_excel(infile, sheet="KRFC")
    ROPI <- read_excel(infile, sheet="ROPI")
    SheltonDists <- read_excel(infile, sheet="Shelton Dists")
    
    # Pull RunID, NaturalMortality, Stock, and Cohort, tables from FRAM database; add 39 stock format field (Stock) to Stock and Cohort tables
    RunID <- pull_RunID(db_path = DBpath, runID = runID[z])
    nat_mort <- pull_NaturalMortality(DBpath)
    stock_fram <- pull_Stock(DBpath) %>% 
      mutate(Stock = ceiling(StockID / 2))
    cohort <- pull_Cohort(db_path = DBpath, runID = runID[z], age = c(3:5), timestep = c(1:3)) %>% 
      select(-PrimaryKey) %>% 
      mutate(Stock = ceiling(StockID / 2))
    
    # Get Stock names by 39 format StockID, merge into main data frame
    StkNms <- stock_fram %>% 
      select(StockLongName, Stock) %>% 
      distinct(Stock, .keep_all = TRUE) %>% 
      mutate(StockLongName = substring(StockLongName, 10)) %>% 
      left_join(., Stock_xwalk %>% rename(Stock = StockID), by = join_by(Stock))
    cohort <- left_join(cohort, StkNms, by = join_by(Stock))
    
    # add year into cohort table
    cohort <- left_join(cohort, RunID %>% select(RunID, RunYear), by = join_by(RunID))
    
    # Combine Marked and Unmarked components of each stock, remove Central Valley (b/c SRFC added in later)
    cohort_Shelton_Stk <- cohort %>% 
      group_by(RunYear, SheltonStk, StockLongName, Age, TimeStep) %>% 
      summarise(StartCohort = sum(StartCohort), .groups = "drop") %>% 
      filter(SheltonStk != "CENTV")
    
    # ---------- NON-FRAM STOCK PROCESSING ---------- #
    # 'UCOLSP' Shelton Stock
    # expand TRS by age to include natural mortality
    UpColSpr <- UpColSpr %>% 
      filter(RunYear == RunID$RunYear) %>% 
      left_join(., nat_mort %>% select(-BasePeriodID), by = join_by(Age, TimeStep)) %>% 
      mutate(StartCohort = StartCohort.sum / (1 - NaturalMortalityRate)) %>% 
      select(-NaturalMortalityRate, -StartCohort.sum)
    
    # 'CENTV' Shelton Stock
    centv_out <- CENTV_SRKW(SRFC_trun = SRFC_trun, SRFC_harv = SRFC_harv, ZeroPFMC_flag = 0, RunYear = RunID$RunYear)
    SRFC_cohort <- centv_out[[1]] %>% 
      mutate(RunYear = as.numeric(RunYear))
    
    # 'NCA' Shelton Stock
    nca_out <- NCA_SRKW(KRFC_cohort, KRFC_harv = NA, ROPI, ZeroPFMC_flag = 0, RunYear = RunID$RunYear, process_harvest = 0)
    NCA_cohort <- nca_out[[1]]
    
    # combine 'UCOLSPR', 'CENTV', and 'NCA' into 'SOFabundances' data frame, add StockLongName, combine with FRAM data
    NonFRAM_stks <- data.frame(SheltonStk = c("CENTV", "NCA", "UCOLSPR"),
                               StockLongName = c("Sacramento Fall", "Rogue/Klamath Fall", "Upper Columbia Spring"))
    
    SOFabundances <- bind_rows(SRFC_cohort, NCA_cohort, UpColSpr) %>% 
      left_join(., NonFRAM_stks, by = join_by(SheltonStk)) %>% 
      relocate(StockLongName, .before = Age) %>% 
      mutate(RunYear = as.character(RunYear))
    # ----------------------------------------------- #
    
    cohort_Shelton_Stk <- bind_rows(cohort_Shelton_Stk, SOFabundances)
    
    # combine with Shelton distributions and apportion StartingCohort into region
    cohort_by_area <- cohort_Shelton_Stk %>% 
      left_join(., SheltonDists, by = join_by(SheltonStk, TimeStep)) %>% 
      mutate_at(vars(7:12), ~.*StartCohort) %>% 
      mutate(Alt = paste0("Alt", z))
    
    # summarise abundances by year, TS, area
    SRKW_abundances <- cohort_by_area %>% 
      pivot_longer(c(7:10,12), names_to = "Area", values_to = "Abundance") %>% 
      mutate(Area = factor(Area, levels = c("NOF", "OR", "CALI", "SWWCVI", "SALISH"))) %>% 
      group_by(RunYear, TimeStep, Area, Alt) %>% 
      summarise(Abundance = sum(Abundance), .groups = "drop") %>% 
      relocate(Alt, .after = Abundance) %>% 
      relocate(Area, .after = Abundance) %>% 
      arrange(Area)
    
    if(exists("Cohort_by_stock_area") == FALSE) {
      Cohort_by_stock_area <- cohort_by_area
    } else {
      Cohort_by_stock_area <- bind_rows(Cohort_by_stock_area, cohort_by_area)
    }
    
    if(exists("SRKW_Abundances") == FALSE) {
      SRKW_Abundances <- SRKW_abundances
    } else {
      SRKW_Abundances <- bind_rows(SRKW_Abundances, SRKW_abundances)
    }
  }
  
  SRKW_abund_TS1 <- SRKW_Abundances %>% 
    filter(TimeStep == 1) %>% 
    mutate(Abundance = round(Abundance / 1000, 3)) %>% 
    pivot_wider(names_from = Alt, values_from = Abundance)
  
  fram_shelton_TS1_out <- list(SRKW_abund_TS1 = SRKW_abund_TS1,
                               Cohort_by_stock_area = Cohort_by_stock_area)
  
  return(fram_shelton_TS1_out)
}
#--------------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------------#
# Function to calculate pre- and post fishing regional abundances using FRAM/Shelton for a base scenario
# (often validation runs) and a modified scenario (often a zero fishing scenario).
#
# Arguments:
#   1. Years - years to be processed. Note: There must be a single model run for each specified year in
#              both the 'base' and 'zero' databases identified in below paths.
#   2. RunType_base - this is the name of the base model run scenario (often validation runs).
#   3. RunType_zero - this is the name of the modified model run scenario (often a zero fishing scenario)
#   4. ZeroPFMC_flag - flag indicating whether the zero fishing scenario includes closure of PFMC fisheries
#       0 = no, PFMC fisheries remain open in the zero fishing scenario
#       1 = yes, PFMC fisheries are closed in the zero fishing scenario
#   3. paths_in - list of paths where element 1 is the path to the 'SRKW_Input.xlsx' file and elements 2 
#                 & 3 are paths to the 'base' scenario and 'zero' scenario databases, respectively.
#   4. exc_sac - flag indicating whether or not to remove Central Valley stock FRAM info from the 
#                assessment.  This defaults to 1 (yes, remove it) and should probably always be 1, just 
#                left the functionality in case there was some rare occasion where we wanted to include it.
#
# Output:
#   1. fram_shelton_out - list of two data frames:
#       - Abundance - data frame of raw abundance output
#       - kCal - data frame of raw kCal output
#--------------------------------------------------------------------------------------------------------#

SRKW_FRAM_Shelton <- function(Years, RunType_base, RunType_zero, ZeroPFMC_flag, paths_in, exc_sac=1) {
  
  # Set the input file path for the database containing FRAM runs
  infile = paths_in[[1]]
  DBpath1 = paths_in[[2]]
  DBpath2 = paths_in[[3]]
  
  # Read in data from Excel input file
  Stock_xwalk <- read_excel(infile, sheet="FRAM to Shelton Stks")
  SheltonDists <- read_excel(infile, sheet="Shelton Dists")
  StockRegion <- read_excel(infile, sheet="StockRegion")
  FisheryRegion <- read_excel(infile, sheet="FisheryRegion")
  UpColSpr <- read_excel(infile, sheet="Up Col Spr Abundances")
  SRFC_trun <- read_excel(infile, sheet="SRFC Run Dat")
  SRFC_harv <- read_excel(infile, sheet="SRFC Harv Dat")
  KRFC_cohort <- read_excel(infile, sheet="KRFC")
  KRFC_harv <- read_excel(infile, sheet="KRFC Harvest")
  ROPI <- read_excel(infile, sheet="ROPI")
  kcalDF <- read_excel(infile, sheet="kCal")
  
  # Pull necessary data from base FRAM database
  Cohort_1 = pull_Cohort(db_path = DBpath1, age = c(3:5), timestep = c(1:3))
  Mort_1 = pull_Mortality(db_path = DBpath1, age = c(3:5), timestep = c(1:3))
  RunID_1 = pull_RunID(db_path = DBpath1)
  NatMort = pull_NaturalMortality(db_path = DBpath1, bpID = max(RunID_1$BasePeriodID))
  
  # Pull necessary data from zero FRAM database
  Cohort_2 = pull_Cohort(db_path = DBpath2, age = c(3:5), timestep = c(1:3))
  Mort_2 = pull_Mortality(db_path = DBpath2, age = c(3:5), timestep = c(1:3))
  RunID_2 = pull_RunID(db_path = DBpath2)
  
  # Add run type and combine RunID tables, filter to desired years
  RunID_1 <- RunID_1 %>% 
    mutate(RunType = RunType_base) %>% 
    select(RunID, RunYear, RunType)
  RunID_2 <- RunID_2 %>% 
    mutate(RunType = RunType_zero) %>% 
    select(RunID, RunYear, RunType)
  RunIDs <- bind_rows(RunID_1, RunID_2) %>% 
    filter(RunYear %in% Years)
  
  # -------------------- Non-FRAM Processing -------------------- #
  
  # ---- Upper Columbia Spring ---- (assumig no ocean harvest so same for both scenarios)
  UCOLSPR_cohort <- UpColSpr %>% 
    filter(RunYear %in% Years) %>% 
    rename(StartCohort = StartCohort.sum) %>% 
    left_join(., NatMort %>% select(-BasePeriodID), by = join_by(Age, TimeStep)) %>% 
    mutate(StartCohort = StartCohort / (1 - NaturalMortalityRate)) %>% 
    select(-NaturalMortalityRate) %>% 
    bind_rows(.,.) %>% 
    mutate(RunType = c(rep(RunType_base,dim(.)[1]/2), rep(RunType_zero,dim(.)[1]/2)), .before = RunYear)
  
  # ---- Central Valley (SRFC) ----
  for(i in Years) {
    centv_out_base <- CENTV_SRKW(SRFC_trun, SRFC_harv, ZeroPFMC_flag = 0, RunYear = i)
    centv_out_zero <- CENTV_SRKW(SRFC_trun, SRFC_harv, ZeroPFMC_flag, RunYear = i)
    
    if(exists("CENTV_cohort") == FALSE) {
      CENTV_cohort <- bind_rows(centv_out_base[[1]] %>% mutate(RunType = RunType_base, .before = RunYear),
                                centv_out_zero[[1]] %>% mutate(RunType = RunType_zero, .before = RunYear))
    } else {
      CENTV_cohort <- bind_rows(CENTV_cohort,
                                centv_out_base[[1]] %>% mutate(RunType = RunType_base, .before = RunYear),
                                centv_out_zero[[1]] %>% mutate(RunType = RunType_zero, .before = RunYear))
    }
    
    if(exists("CENTV_harvest") == FALSE) {
      CENTV_harvest <- as.data.frame(centv_out_base[[2]])
    } else {
      CENTV_harvest <- bind_rows(CENTV_harvest, as.data.frame(centv_out_base[[2]]))
    }
  }
  
  # ---- Northern CA (KRFC & Rogue) ---- 
  for(i in Years) {
    nca_out_base <- NCA_SRKW(KRFC_cohort, KRFC_harv, ROPI, ZeroPFMC_flag = 0, RunYear = i)
    nca_out_zero <- NCA_SRKW(KRFC_cohort, KRFC_harv, ROPI, ZeroPFMC_flag = ZeroPFMC_flag, RunYear = i)
    
    if(exists("NCA_cohort") == FALSE) {
      NCA_cohort <- bind_rows(nca_out_base[[1]] %>% mutate(RunType = RunType_base, .before = RunYear),
                              nca_out_zero[[1]] %>% mutate(RunType = RunType_zero, .before = RunYear))
    } else {
      NCA_cohort <- bind_rows(NCA_cohort,
                              nca_out_base[[1]] %>% mutate(RunType = RunType_base, .before = RunYear),
                              nca_out_zero[[1]] %>% mutate(RunType = RunType_zero, .before = RunYear)) %>% 
        arrange(RunType)
    }
    
    if(exists("NCA_harvest") == FALSE) {
      NCA_harvest <- as.data.frame(nca_out_base[[2]])
    } else {
      NCA_harvest <- bind_rows(NCA_harvest, as.data.frame(nca_out_base[[2]]))
    }
  }
  
  # combine CENTV and NCA into a single SOF dataset
  SOF_cohort <- bind_rows(CENTV_cohort, NCA_cohort)
  SOF_harvest <- bind_rows(CENTV_harvest, NCA_harvest)
  
  # -------------------- Main Processing -------------------- #
  # combine cohort tables, add run type & year, convert to 39 stock format
  Cohort_1 <- Cohort_1 %>% 
    mutate(RunType = RunType_base)
  Cohort_2 <- Cohort_2 %>% 
    mutate(RunType = RunType_zero)
  Cohort <- bind_rows(Cohort_1, Cohort_2) %>% 
    left_join(RunIDs, ., by = join_by(RunID, RunType)) %>% 
    mutate(StockID = ceiling(StockID / 2)) %>% 
    left_join(., Stock_xwalk, by = join_by(StockID)) %>% 
    filter(if(exc_sac == 1) StockID != 35 else StockID > 0) %>%
    mutate(StockID = as.character(StockID)) %>% 
    group_by(RunType, RunYear, SheltonStk, StockID, Age, TimeStep) %>% 
    summarise(StartCohort = sum(StartCohort), .groups = "drop")
  
  # append non-FRAM stocks to cohort table
  Cohort <- bind_rows(SOF_cohort, UCOLSPR_cohort) %>% 
    mutate(StockID = ifelse(SheltonStk == "CENTV", 35, SheltonStk)) %>% 
    mutate(RunYear = as.character(RunYear)) %>% 
    bind_rows(Cohort, .) %>% 
    as.data.frame(.)
  
  # join cohort table with Shelton distributions, apply distributions to StartCohorts
  Cohort <- Cohort %>% 
    left_join(., SheltonDists, by = join_by(SheltonStk, TimeStep)) %>% 
    mutate_at(vars(8:13), ~.*StartCohort) %>% 
    rename_with(str_to_title, .cols = c(8,11:12)) %>% 
    rename_with(~paste0("preFish.", .x), .cols = c(8:13))
  
  # combine mort tables, add run type & year, convert to 39 stock format
  Mort_1 <- Mort_1 %>% 
    mutate(RunType = RunType_base)
  Mort_2 <- Mort_2 %>% 
    mutate(RunType = RunType_zero)
  Mort <- bind_rows(Mort_1, Mort_2) %>% 
    left_join(RunIDs, ., by = join_by(RunID, RunType)) %>% 
    mutate(StockID = ceiling(StockID / 2)) %>% 
    left_join(., Stock_xwalk, by = join_by(StockID)) %>% 
    filter(if(exc_sac == 1) StockID != 35 else StockID > 0) %>%
    mutate(StockID = as.character(StockID)) %>% 
    rowwise() %>% 
    mutate(Mort = sum(LandedCatch, NonRetention, Shaker, DropOff,
                      MSFLandedCatch, MSFNonRetention, MSFShaker, MSFDropOff)) %>% 
    group_by(RunType, RunYear, SheltonStk, StockID, Age, FisheryID, TimeStep) %>% 
    summarise(Mort = sum(Mort), .groups = "drop")
  
  # add fishery flag and regtion for hybrid approach
  Mort <- Mort %>% 
    left_join(., FisheryRegion %>% select(-FisheryName, -FisheryTitle), by = join_by(FisheryID, TimeStep))
  
  # extract mort records for adhoc style processing, summarize
  Mort_adHocStyle <- Mort %>% 
    filter(Mort_Dist_Type == 1) %>% 
    group_by(RunType, RunYear, SheltonStk, StockID, Age, TimeStep) %>% 
    summarise(Mort = sum(Mort), .groups = "drop") %>% 
    mutate(StockID = as.character(StockID))
  
  # append SOF morts to adhoc morts table
  SOF_harvest <- SOF_harvest %>% 
    mutate(StockID = ifelse(SheltonStk == "CENTV", 35, SheltonStk), .after = SheltonStk) %>% 
    mutate(RunType = RunType_base, .before = RunYear) %>% 
    mutate(RunYear = as.character(RunYear))
  if(ZeroPFMC_flag == 0) {
    SOF_harvest <- SOF_harvest %>% 
      mutate(RunType = RunType_zero) %>% 
      bind_rows(SOF_harvest, .)
  }
  Mort_adHocStyle <- bind_rows(Mort_adHocStyle, SOF_harvest)
  
  # join adhoc morts table with Shelton distributions, apply distributions to morts
  Mort_adHocStyle <- Mort_adHocStyle %>% 
    left_join(., SheltonDists, by = join_by(SheltonStk, TimeStep)) %>% 
    mutate_at(vars(8:13), ~.*Mort) %>% 
    rename_with(str_to_title, .cols = c(8,11:12)) %>% 
    rename_with(~paste0("mort.", .x), .cols = c(8:13)) %>% 
    select(-Mort) %>% 
    pivot_longer(starts_with("mort"), names_to = "Region", values_to = "Mort")
  
  # extract mort records for hybrid processing
  Mort_hybridStyle <- Mort %>% 
    filter(Mort_Dist_Type == 2)
  if(dim(Mort_hybridStyle)[1] > 0) {
    Mort_hybridStyle <- Mort_hybridStyle %>% 
      group_by(RunType, RunYear, SheltonStk, StockID, Age, TimeStep, Region) %>% 
      summarise(Mort = sum(Mort), .groups = "drop") %>% 
      left_join(., StockRegion, by = join_by(SheltonStk)) %>% 
      left_join(., SheltonDists, by = join_by(SheltonStk, TimeStep)) %>% 
      mutate_at(vars(10:15), ~.*Mort) %>% 
      rename_with(str_to_title, .cols = c(10,13:14)) %>% 
      rename_with(~paste0("mort.", .x), .cols = c(10:15)) %>% 
      mutate(mort.Salish = ifelse(substring(Region,6) == RegionStk, 
                                  ifelse(RegionStk == "Salish", Mort, 0), mort.Salish)) %>% 
      mutate(mort.NOF = ifelse(substring(Region,6) == RegionStk, 
                               ifelse(RegionStk == "NOF", Mort, 0), mort.NOF)) %>% 
      mutate(mort.OR = ifelse(substring(Region,6) == RegionStk, 
                              ifelse(RegionStk == "OR", Mort, 0), mort.OR)) %>% 
      mutate(mort.Cali = ifelse(substring(Region,6) == RegionStk, 
                                ifelse(RegionStk == "Cali", Mort, 0), mort.Cali)) %>% 
      mutate(mort.North = ifelse(substring(Region,6) == RegionStk, 
                                 ifelse(RegionStk == "North", Mort, 0), mort.North)) %>% 
      mutate(mort.SWWCVI = ifelse(substring(Region,6) == RegionStk, 
                                  ifelse(RegionStk == "SWWCVI", Mort, 0), mort.SWWCVI)) %>% 
      select(-Region, -Mort, -RegionStk) %>% 
      pivot_longer(starts_with("mort"), names_to = "Region", values_to = "Mort")
  }
  
  # combine adhoc and hybrid mort tables
  Mort <- Mort_adHocStyle %>% 
    bind_rows(., Mort_hybridStyle) %>% 
    group_by(RunType, RunYear, SheltonStk, StockID, Age, TimeStep, Region) %>% 
    summarise(Mort = sum(Mort), .groups = "drop") %>% 
    mutate(Region = factor(Region, levels = c("mort.Salish", "mort.NOF", "mort.OR", 
                                              "mort.Cali", "mort.North", "mort.SWWCVI"))) %>% 
    arrange(RunType, RunYear, SheltonStk, StockID, Age, TimeStep, Region) %>% 
    pivot_wider(names_from = Region, values_from = Mort)
  
  # join cohort and mort tables, calculate post-fishery abundances
  Cohort <- Cohort %>% 
    left_join(., Mort, by = join_by(RunType, RunYear, SheltonStk, StockID, Age, TimeStep)) %>% 
    mutate_at(vars(14:19), ~replace_na(., 0)) %>% 
    mutate(postFish.Salish = preFish.Salish - mort.Salish) %>% 
    mutate(postFish.NOF = preFish.NOF - mort.NOF) %>% 
    mutate(postFish.OR = preFish.OR - mort.OR) %>% 
    mutate(postFish.Cali = preFish.Cali - mort.Cali) %>% 
    mutate(postFish.North = preFish.North - mort.North) %>% 
    mutate(postFish.SWWCVI = preFish.SWWCVI - mort.SWWCVI) %>%
    mutate(Mort = across(starts_with("mort")) %>% rowSums - mort.SWWCVI, .before = preFish.Salish) %>% 
    mutate(PostFisheryAbund = across(starts_with("post")) %>% rowSums - postFish.SWWCVI, .before = preFish.Salish)
  
  # create kcal table
  kCal <- Cohort %>% 
    left_join(., kcalDF %>% select(-MeanFL), by = join_by(StockID, Age, TimeStep)) %>% 
    mutate_at(vars(7:27), ~.*kCal)
  
  # summarize across StockID and age
  Cohort <- Cohort %>% 
    select(-StockID, -Age) %>% 
    group_by(RunType, RunYear, SheltonStk, TimeStep) %>% 
    summarise(across(everything(), sum), .groups = "drop") %>% 
    arrange(RunType, RunYear, SheltonStk, TimeStep) %>% 
    as.data.frame()
  
  kCal <- kCal %>% 
    select(-StockID, -Age, -kCal) %>% 
    group_by(RunType, RunYear, SheltonStk, TimeStep) %>% 
    summarise(across(everything(), sum), .groups = "drop") %>% 
    arrange(RunType, RunYear, SheltonStk, TimeStep) %>% 
    as.data.frame()
  
  fram_shelton_out <- list(Abundance = Cohort,
                           kCal = kCal)
  
  return(fram_shelton_out)
}
#--------------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------------#
# function to convert a data frame (x) to a matrix with years as row labels
#--------------------------------------------------------------------------------------------------------#

to.matrix <- function(x, years) {
  m <- as.matrix(x[,-1])
  rownames(m) <- c(min(years):max(years))
  return(m)
}
#--------------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------------#
# Function to summarize FRAM/Shelton output across multiple scenarios and calculate nominal and percent
# differences.
#
# Arguments:
#   1. Years - range of years to be processed. Might get an error if these are not consecutive.
#   2. regions - character vector of Shelton regions to be processed.  Default is to include the five 
#                main areas: Salish, NOF, OR, Cali, SWWCVI.
#   3. in_data - list of n data frames that are raw output from SRKW_FRAM_Shelton().
#   4. mod_type - set to either "fishery" or "abundance".  Default is to "fishery" which is for 
#                 comparing zero fishing scenarios against a set of base runs.  Set to "abundance" for 
#                 comparing scenarios that modify starting abundances (i.e., to assess the effects of 
#                 hatchery prey program).  The difference between these two options is in how the percent 
#                 differences are calculated - for "fishery" they're calculated relative to the starting 
#                 abundances from the zero fishing scenarios, whereas for "abundance" they're calculated 
#                 relative to the starting abundances from the base model runs.
#
# Output:
#   1. SRKW_out.RData - a nested list structured as follows:
#       - Level 1 contains n+1 elements, where n = length(in_data).  Each 'n' element represents a 
#         scenario (e.g., a specific zero fishing scenario or a specific hatchery prey increase scenario).
#         The final element is a record of what the 'mod_type' input was.
#       - Level 2 contains an element for each region specified in the 'regions' argument plus an 
#         additional element that contains the raw 'in_data' for the specific scenario.
#       - Level 3 contains set of six data frames specific to the scenario/region defines in levels 1 & 2.
#         Metrics are:
#           - 'preFish_base' - pre-fishing abundances in the base model runs
#           - 'preFish_mod' - pre-fishing abundances in the modified (i.e., scenario) model runs
#           - 'postFish_base' - post-fishing abundances in the base model runs
#           - 'postFish_mod' - post-fishing abundances in the modified (i.e., scenario) model runs
#           - 'difference_nominal' - difference in post-fishing abundance (# fish) between base and 
#                                    modified model runs (i.e., postFish_mod - postFish_base).
#           - 'difference_percent' - percent change in TS3 post-fishing abundance relative to TS1 
#                                    pre-fishing abundance.  Note that the 'mod_type' argument determines 
#                                    whether the base or modified pre-fishing TS1 abundance is used in 
#                                    the denominator.
#--------------------------------------------------------------------------------------------------------#

SRKW_SummarizR <- function(Years, 
                           regions = c("Salish", "NOF", "OR", "Cali", "SWWCVI"), 
                           in_data, 
                           mod_type = "fishery") {
  
  SRKW_out <- list()
  s_dat <- list()
  
  for(n in 1:length(in_data)) {
    dat <- in_data[[n]]
    scenario <- names(in_data)[n]
    base <- dat %>% 
      filter(RunType != scenario) %>% 
      pull(RunType) %>% unique()
    
    for(i in 1:length(regions)) {
      pre_base <- dat %>% 
        filter(RunType == base) %>% 
        select(RunYear, TimeStep, paste0("preFish.",regions[i])) %>% 
        rename(preFish = paste0("preFish.",regions[i])) %>% 
        group_by(RunYear, TimeStep) %>% 
        summarise(preFish = sum(preFish), .groups = "drop") %>% 
        pivot_wider(names_from = TimeStep, values_from = preFish)
      pre_base <- to.matrix(pre_base, Years)
      pre_base_ts1 <- cbind(pre_base[,1],pre_base[,1],pre_base[,1])
      
      pre_mod <- dat %>% 
        filter(RunType == scenario) %>% 
        select(RunYear, TimeStep, paste0("preFish.",regions[i])) %>% 
        rename(preFish = paste0("preFish.",regions[i])) %>% 
        group_by(RunYear, TimeStep) %>% 
        summarise(preFish = sum(preFish), .groups = "drop") %>% 
        pivot_wider(names_from = TimeStep, values_from = preFish)
      pre_mod <- to.matrix(pre_mod, Years)
      pre_mod_ts1 <- cbind(pre_mod[,1],pre_mod[,1],pre_mod[,1])
      
      post_base <- dat %>% 
        filter(RunType == "Scenario_RMP") %>% 
        select(RunYear, TimeStep, paste0("postFish.",regions[i])) %>% 
        rename(postFish = paste0("postFish.",regions[i])) %>% 
        group_by(RunYear, TimeStep) %>% 
        summarise(postFish = sum(postFish), .groups = "drop") %>% 
        pivot_wider(names_from = TimeStep, values_from = postFish)
      post_base <- to.matrix(post_base, Years)
      
      post_mod <- dat %>% 
        filter(RunType == scenario) %>% 
        select(RunYear, TimeStep, paste0("postFish.",regions[i])) %>% 
        rename(postFish = paste0("postFish.",regions[i])) %>% 
        group_by(RunYear, TimeStep) %>% 
        summarise(postFish = sum(postFish), .groups = "drop") %>% 
        pivot_wider(names_from = TimeStep, values_from = postFish)
      post_mod <- to.matrix(post_mod, Years)
      
      diff_nom <- post_mod - post_base
      
      if(mod_type == "fishery") { # fishery % change is measured relative to pre-fishing TS1 abundance in the zero fishery runs
        diff_pct <- diff_nom / pre_mod_ts1
      } else if(mod_type == "abundance") { # % change from hatchery increase is measured relative to pre-fishing TS1 abundance from the base runs
        diff_pct <- diff_nom / pre_base_ts1
      }
      
      reg_dat <- list(preFish_base = pre_base,
                      preFish_mod = pre_mod,
                      postFish_base = post_base,
                      postFish_mod = post_mod,
                      difference_nominal = diff_nom,
                      difference_percent = diff_pct)
      
      s_dat[[regions[i]]] <- reg_dat
    }
    
    s_dat$raw_data <- dat
    SRKW_out[[scenario]] <- s_dat
    
  }
  
  SRKW_out$mod_type <- mod_type
  return(SRKW_out)
  
}


#--------------------------------------------------------------------------------------------------------#
# Function to extract specified data from 'SRKW_out.RData' file.
#
# Arguments:
#   1. SRKW_out -  the desired SRKW_out.RData file produced by the SRKW_SummarizR() function.
#   2. scenarios - character vector of scenarios to be extracted (selecting from level 1 of the 
#                  SRKW_out.RData file). Default is to include all six 'zero fishing' scenarios.
#   3. regions -   character vector of Shelton regions to be extracted (selecting from level 2 of the 
#                  SRKW_out.RData file). Default is to include all five regions.
#   4. summaries - character vector of metrics to be included. Options include: preFish_base, preFish_mod,
#                  postFish_base, postFish_mod, difference_nominal, difference_percent (see description 
#                  of output for SRKW_SummarizR() function for details).
#
# Output:
#   1. A data frame that contains the desired data by metric, year, scenario, time step, and region.
#--------------------------------------------------------------------------------------------------------#

SRKW_extract <- function(SRKW_out, 
                         scenarios = c("ZeroALL", "ZeroBC", "ZeroPFMC", "ZeroPS", "ZeroSEAK", "ZeroUS"), 
                         regions = c("Salish", "NOF", "OR", "Cali", "SWWCVI"),
                         summaries = c("preFish_base", "preFish_mod", "postFish_base", 
                                       "postFish_mod", "difference_nominal", "difference_percent")) {
  
  dat_all <- data.frame(Metric = as.character(), Year = as.character(), Scenario = as.character(), Region = as.character(), 
                        Oct_Apr = as.numeric(), May_Jun = as.numeric(), Jul_Sep = as.numeric())
  
  # pull specified ('summaries') data across 'scenarios' and write to csv
  for(k in 1:length(summaries)) {
    for(i in 1:length(scenarios)) {
      scenario <- scenarios[i]
      
      dat_scenario <- data.frame(Year = as.character(), Region = as.character(), Oct_Apr = as.numeric(),
                                 May_Jun = as.numeric(), Jul_Sep = as.numeric())
      
      for(j in 1:length(regions)) {
        region <- regions[j]
        dat_region <- as.data.frame(SRKW_out[[scenario]][[region]][[summaries[k]]])
        dat_region$Region <- region
        dat_region$Year <- rownames(dat_region)
        
        dat_region <- dat_region %>% 
          rename(Oct_Apr = 1, May_Jun = 2, Jul_Sep = 3) %>% 
          select(Year, Region, 1, 2, 3)
        
        dat_scenario <- rbind(dat_scenario, dat_region)
        rm(dat_region)
      }
      
      rownames(dat_scenario) <- c(1:dim(dat_scenario)[1])
      assign(paste0("dat_", scenario), dat_scenario)
      
      dat_scenario <- dat_scenario %>% 
        mutate(Scenario = scenario, .after = Year)
      
      if(exists("dat_all_temp")) {
        dat_all_temp <- rbind(dat_all_temp, dat_scenario)
      } else {
        dat_all_temp <- dat_scenario
      }
      rm(dat_scenario)
    }
    
   dat_all_temp <- dat_all_temp %>% 
     mutate(Metric = summaries[k], .before = Year)
   
   dat_all <- bind_rows(dat_all, dat_all_temp)
   
   rm(dat_all_temp)
    
  }
  
  dat_all <- dat_all %>% 
    pivot_longer(cols = 5:7, names_to = "TimeStep", values_to = "value")
  
  # if percent change only, format as percents
  if(summaries == "difference_percent") {
    dat_all$value <- formattable::percent(dat_all$value, digits = 2)
  }
  
  dat_all_wide <- dat_all %>% 
    pivot_wider(names_from = Region, values_from = value)
  
  return(dat_all_wide)
  
}
#--------------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------------#
# Function to generate box plots comparing abundance differences (nominal or %) between scenarios.
#
# Arguments:
#   1. SRKW_out - The desired SRKW_out.RData file produced by the SRKW_SummarizR() function.
#   2. metric - Metric to be used in the box plots (either 'difference_nominal" or 'difference_percent').
#   3. scenarios - Character vector of scenarios to be included in the figures. 
#                  Default is to include all six 'zero fishing' scenarios.
#   4. regions - Character vector of Shelton regions to be included in the figures.
#                Default is to include all five regions.
#   5. by_scenario - Determines whether (TRUE) or not (FALSE) to generate a set of individual plots
#                    specific to each specified scenario that compare across regions.
#   6. by_region - Determines whether (TRUE) or not (FALSE) to generate a set of individual plots specific 
#                  to each specified region that compare across scenarios.
#                  postFish_base, postFish_mod, difference_nominal, difference_percent (see description 
#                  of output for SRKW_SummarizR() function for details).
#   7. save_to_file - Determines whether (TRUE) or not (FALSE) each file should be saved as a .jpeg
#   8. out_path - location where plots will be saved if save_to_file == TRUE.
#   9. RunType_base_name - character value describing base model runs used in naming the plots if
#                          save_to_file == TRUE.
#
# Output:
#   1. A list of the plots generated.
#   2. If save_to_file == TRUE, all plots will also be saved in the folder specified by 'out_path'.
#--------------------------------------------------------------------------------------------------------#

SRKW_BoxPlots <- function(SRKW_out, 
                          metric = "difference_percent", 
                          scenarios = c("ZeroALL", "ZeroBC", "ZeroPFMC", "ZeroPS", "ZeroSEAK", "ZeroUS"), 
                          regions = c("Salish", "NOF", "OR", "Cali", "SWWCVI"),
                          by_scenario = TRUE,
                          by_region = TRUE, 
                          save_to_file = TRUE, 
                          out_path, 
                          RunType_base_name) {
  
  metric_options <- c("difference_nominal", "difference_percent")
  
  if(!(metric %in% metric_options)) {
    stop(paste0("Invalid metric. Please provide a valid metric from: ", paste(metric_options, collapse = ", "), "."))
  }
  
  dat <- SRKW_extract(summaries = metric, SRKW_out = SRKW_out) %>% 
    pivot_longer(cols = 5:9, names_to = "Region", values_to = "value") %>% 
    mutate(value = formattable::percent(value, digits = 2)) %>% 
    mutate(Scenario = factor(Scenario, levels = c("ZeroSEAK", "ZeroPFMC", "ZeroPS", "ZeroUS", "ZeroBC", "ZeroALL"))) %>% 
    mutate(Region = factor(Region, levels = c("SWWCVI", "Salish", "NOF", "OR", "Cali"))) %>% 
    mutate(TimeStep = factor(TimeStep, levels = c("Oct_Apr", "May_Jun", "Jul_Sep")))
  
  if(SRKW_out$mod_type == "fishery") {
    change_dir <- "Reduction"
  } else if(SRKW_out$mod_type == "abundance") {
    change_dir <- "Increase"
  }
  y_lab <- ifelse(metric == "difference_percent", paste0("Percent ", change_dir, " in Abundance"),
                  ifelse(metric == "difference_nominal", paste0(change_dir, " in Abundance"), 
                         "Chinook Abundance"))
  
  metric_name <- ifelse(metric == "difference_percent", "percent_diff",
                        ifelse(metric == "difference_nominal", "nominal_diff", metric))
  
  fig_title <- 
    
    # box plots: all
    p_all <- ggplot(dat %>% filter(Region %in% regions), aes(x=Region, y=value, fill= Region)) +
    geom_boxplot(position=position_dodge(0.85), alpha = 0.75) +
    facet_grid(TimeStep~Scenario) +
    scale_fill_manual(values = c("SWWCVI" = "#5F6366", "Salish" = "#4D6D9A", "NOF" = "#F4A460", "OR" = "#86B3D1", "Cali" = "gray80")) +
    scale_y_continuous(labels = ifelse(metric == "difference_percent",
                                       scales::percent_format(accuracy = 1),
                                       scales::label_number(scale = 1e-3, suffix = "K"))) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    guides(color=FALSE) +
    theme(legend.position="bottom") +
    labs(y = y_lab)
  
  if(save_to_file == TRUE) {
    ggsave(paste0(out_path, "SRKW_BoxPlots_", RunType_base_name, "_", metric_name, "_all.jpeg"), p_all, width = 6.5, height = 6.5)
  }
  
  # box plots: by region
  if(by_region == TRUE) {
    p_region <- list()
    for(r in 1:length(regions)) {
      p_region[[regions[r]]] <- ggplot(dat %>% filter(Region == regions[r]), aes(x=Scenario, y=value)) +
        geom_boxplot(position=position_dodge(0.85), alpha = 0.75, fill = "#4D6D9A") +
        facet_wrap(~TimeStep) +
        scale_y_continuous(labels = ifelse(metric == "difference_percent",
                                           scales::percent_format(accuracy = 1),
                                           scales::label_number(scale = 1e-3, suffix = "K"))) +
        scale_x_discrete(labels = c("ZeroSEAK" = "SEAK", "ZeroPFMC" = "PFMC", "ZeroPS" = "PS", "ZeroUS" = "US", "ZeroBC" = "BC", "ZeroALL" = "ALL")) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        guides(color=FALSE) +
        theme(legend.position="none") +
        labs(title = paste0("Effect of regional fisheries on ", regions[r], " Chinook abundance"),
             y = y_lab)
      
      if(save_to_file == TRUE) {
        ggsave(paste0(out_path, "SRKW_BoxPlots_", RunType_base_name, "_", metric_name, "_", regions[r], ".jpeg"), 
               p_region[[regions[r]]], width = 6.5, height = 4)
      }
    }
  }
  
  # box plots: by scenario
  if(by_scenario == TRUE) {
    p_scenario <- list()
    for(s in 1:length(scenarios)) {
      p_scenario[[scenarios[s]]] <- ggplot(dat %>% filter(Region %in% regions & Scenario == scenarios[s]) , aes(x=Region, y=value)) +
        geom_boxplot(position=position_dodge(0.85), alpha = 0.75, fill = "#90d0a1") +
        facet_wrap(~TimeStep) +
        scale_y_continuous(labels = ifelse(metric == "difference_percent",
                                           scales::percent_format(accuracy = 1),
                                           scales::label_number(scale = 1e-3, suffix = "K"))) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        guides(color=FALSE) +
        theme(legend.position="bottom") +
        labs(title = paste0("Effect of ", substr(scenarios[s], 5, nchar(scenarios[s])), " fisheries on regional Chinook abundances"),
             y = y_lab)
      
      if(save_to_file == TRUE) {
        ggsave(paste0(out_path, "SRKW_BoxPlots_", RunType_base_name, "_", metric_name, "_", scenarios[s], ".jpeg"), 
               p_scenario[[scenarios[s]]], width = 6.5, height = 4)
      }
    }
  }
  
  SRKW_box_plots <- list(all = p_all, by_region = p_region, by_scenario = p_scenario)
  return(SRKW_box_plots)
  
}


