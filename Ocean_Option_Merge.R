########################################################################################
#SQL QUERY TO UPDATE OCEAN FISHERY INPUTS IN ONE RUNID USING VALUES FROM ANOTHER RUNID#
########################################################################################

# Clear workspace
rm(list=ls(all=TRUE))

# Load required libraries
library(RODBC)

# Set the paths to the most recent CRC database and output folder
paths = list("C:\\data\\NOF\\2018\\Modeling\\Chinook\\Model Runs\\2018 NOF ChinFRAM.mdb")

infile = paths[[1]]


# Identify the RunID from which you want to take the Ocean Fishery Inputs
RunID_1 = 54

# Identify the RunID to which you want to update the Ocean Fishery Inputs
RunID_2 = 64

con = odbcConnectAccess2007(infile)
# Extract Ocean Fishery data from source RunID
OceanDat1 = sqlQuery(con, as.is = TRUE, 
                 paste(sep = '',
                       "SELECT FisheryScalers.* ",
                       "FROM FisheryScalers ",
                       "WHERE (((FisheryScalers.RunID)=",RunID_1,") AND ((FisheryScalers.FisheryID) In (16,18,20,21,22,26,27,30,31,32,33,34,35)));"))
OceanDat2 = sqlQuery(con, as.is = TRUE, 
                     paste(sep = '',
                           "SELECT FisheryScalers.* ",
                           "FROM FisheryScalers ",
                           "WHERE (((FisheryScalers.RunID)=",RunID_1,") AND ((FisheryScalers.FisheryID)=17) AND ((FisheryScalers.TimeStep) In (2,3)));"))


# Pull PrimaryKey values from FisheryScalers table
PK = sqlQuery(con, as.is = TRUE, 
              paste(sep = '',
                    "SELECT FisheryScalers.PrimaryKey ",
                    "FROM FisheryScalers;"))
close(con)

# Combine OceanDat1 & OceanDat2
OceanDat <- rbind(OceanDat1,OceanDat2)

# Determine max of existing PrimaryKeys in FisheryScalers Table
MaxPK <- max(PK)

# Identify new PK values for update
PrimaryKey <- seq(MaxPK+1,MaxPK+dim(OceanDat)[1],1)

# Update new PK values in Ocean Dat
OceanData <- cbind(PrimaryKey, OceanDat[ ,c(2:13)])

# Update RunID in Ocean Dat from RunID_1 to RunID_2
OceanData$RunID <- RunID_2

# Add comment field to OceanData
CommentField <- "Comment"
OceanData[ ,CommentField] <- NA

con = odbcConnectAccess2007(infile)
# Query to delete existing Ocean Inputs
sqlQuery(con, as.is = TRUE,
         paste(sep = '',
               "DELETE FisheryScalers.* ",
               "FROM FisheryScalers ",
               "WHERE (((FisheryScalers.RunID)=",RunID_2,") AND ((FisheryScalers.FisheryID) In (16,18,20,21,22,26,27,30,31,32,33,34,35)));"))
sqlQuery(con, as.is = TRUE,
         paste(sep = '',
               "DELETE FisheryScalers.* ",
               "FROM FisheryScalers ",
               "WHERE (((FisheryScalers.RunID)=",RunID_2,") AND ((FisheryScalers.FisheryID)=17) AND ((FisheryScalers.TimeStep) In (2,3)));"))

# Query to append new Ocean Inputs
sqlSave(con, OceanData, tablename = "FisheryScalers", rownames = FALSE, append = TRUE)
close(con)





