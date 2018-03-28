####################################################################################
# This program will produce a set of figures that compare fishery stock compositions
# between two specified model runs for desired fishery/timesteps.
#
# Required Inputs:
#   1. FRAM database that contains the desired runs (path 1)
#   2. figlist.csv (provides list of desired fisheries/timesteps; path 2)
#   3. Output folder path (path 3)
####################################################################################

# Clear workspace
rm(list=ls(all=TRUE))

# Load required libraries
library(RODBC)
library(doBy)
library(ggplot2)

# Select RunIDs
runID <- c(67)

# Set RunID names
runIDnames <- c("Option 3")

# Identify figures to include in panel, pick any or all of the following:
# "TotalLanded", "LegalAEQ", "SublegalAEQ", "TotalAEQ"
figtypes <- c("LegalAEQ", "SublegalAEQ")

# Set the paths 
paths = list("C:\\data\\NOF\\2018\\Modeling\\Chinook\\Model Runs\\2018 NOF ChinFRAM.mdb",
             "C:\\data\\NOF\\2018\\Modeling\\Chinook\\Model Runs\\AVG-NALF\\figlist.csv",
             "C:\\data\\NOF\\2018\\Modeling\\Chinook\\Model Runs\\Chin1318-1518\\StockCompFigs\\")

# Set the input file path for the database
infile = paths[[1]]
figlist = read.csv(paths[[2]])
outfile = paths[[3]]

con = odbcConnectAccess(infile)
RunID = sqlQuery(con, as.is = TRUE, 
                 paste(sep = '',
                       "SELECT * FROM RunID"))
Stock = sqlQuery(con, as.is = TRUE,
                 paste(sep = '',
                       "SELECT * FROM Stock"))
Fishery = sqlQuery(con, as.is = TRUE,
                   paste(sep = '',
                         "SELECT * FROM Fishery"))
TerminalFlags = sqlQuery(con, as.is = TRUE,
                         paste(sep = '',
                               "SELECT * FROM TerminalFisheryFlag"))
AEQ = sqlQuery(con, as.is = TRUE,
               paste(sep = '',
                     "SELECT * FROM AEQ"))
Escapement = sqlQuery(con, as.is = TRUE, 
                      paste(sep = '',
                            "SELECT * FROM Escapement"))
Mortality = sqlQuery(con, as.is = TRUE, 
                     paste(sep = '',
                           "SELECT * FROM Mortality"))
close(con)

# Get Base Period ID
BPID <- unique(RunID$BasePeriodID)

# Trim Terminal Flags and AEQs to correct base period ID
TerminalFlags <- TerminalFlags[TerminalFlags$BasePeriodID %in% BPID, c(1:4)]
AEQ <- AEQ[AEQ$BasePeriodID %in% BPID, c(1:5)]

# Subset to desired years and stocks
# runID <- RunID[RunID$RunYear >= start_year & RunID$RunYear <= end_year, ]
mortality <- Mortality[Mortality$RunID %in% runID, ]
# mortality <- mortality[mortality$StockID %in% stock_list, ]
escapement <- Escapement[Escapement$RunID %in% runID, ]
# escapement <- escapement[escapement$StockID %in% stock_list, ]

# Merge mort data with terminal flags and AEQ values
MortData <- merge(mortality,RunID[,c(2,6)], all.x = TRUE)
MortData <- merge(MortData,TerminalFlags, all.x = TRUE)
MortData <- merge(MortData, AEQ, all.x = TRUE)

# Replace NAs in terminal flag field with 0
MortData$TerminalFlag[is.na(MortData$TerminalFlag)] <- 0

# For terminal fisheries set AEQ value to 1
i=1
for(i in 1:dim(MortData)[1]) {
    if(MortData$TerminalFlag[i] == 1) {
        MortData$AEQ[i] <- 1
    }
}

# Calculate TotalLanded
MortData$TotalLanded <- MortData$LandedCatch + MortData$MSFLandedCatch

#Create "AEQ'd" MortData table
MortData_AEQ <- MortData
MortData_AEQ$LandedCatch <- MortData_AEQ$LandedCatch * MortData_AEQ$AEQ
MortData_AEQ$NonRetention <- MortData_AEQ$NonRetention * MortData_AEQ$AEQ
MortData_AEQ$Shaker <- MortData_AEQ$Shaker * MortData_AEQ$AEQ
MortData_AEQ$DropOff <- MortData_AEQ$DropOff * MortData_AEQ$AEQ
MortData_AEQ$MSFLandedCatch <- MortData_AEQ$MSFLandedCatch * MortData_AEQ$AEQ
MortData_AEQ$MSFNonRetention <- MortData_AEQ$MSFNonRetention * MortData_AEQ$AEQ
MortData_AEQ$MSFShaker <- MortData_AEQ$MSFShaker * MortData_AEQ$AEQ
MortData_AEQ$MSFDropOff <- MortData_AEQ$MSFDropOff * MortData_AEQ$AEQ

MortData_AEQ <- MortData_AEQ[ ,c(2,6,3:5,1,8:11,13:16)]
MortData_AEQ$LandedAEQ <- MortData_AEQ$LandedCatch + MortData_AEQ$MSFLandedCatch
MortData_AEQ$LegalAEQ <- rowSums(MortData_AEQ[ ,c(7:8,10:12,14)])
MortData_AEQ$SublegalAEQ <- rowSums(MortData_AEQ[ ,c(9,13)])
MortData_AEQ$TotalAEQ <- rowSums(MortData_AEQ[ ,c(7:14)])

# Dataset with TotalLanded, LegalAEQ, SublegalAEQ, TotAEQ
MortDat <- MortData[order(MortData$RunID,MortData$StockID,MortData$Age,MortData$FisheryID,
                          MortData$TimeStep),
                    c(2,6,3:5,1,20)]
MortDat_AEQ <- MortData_AEQ[order(MortData_AEQ$RunID,MortData_AEQ$StockID,MortData_AEQ$Age,
                                  MortData_AEQ$FisheryID,MortData_AEQ$TimeStep),
                            c(1:6,16:18)]
TotAEQ <- merge(MortDat,MortDat_AEQ)

# Convert to 38 stock format
TotAEQ$StkNum <- ceiling(TotAEQ$StockID/2)
TotAEQ <- merge(TotAEQ,Stock[ ,c(3,6)])

# Trim 'U-' and 'M-' off stock names
i=1
for(i in 1:dim(TotAEQ)[1]) {
    TotAEQ$StockName[i] <- substring(TotAEQ$StockName[i],3)
}

# Assign names to RunIDs
i=1
for(i in 1:dim(TotAEQ)[1]) {
    j=1
    for(j in 1:length(runID)) {
        if(TotAEQ$RunID[i] == runID[j]) {
            TotAEQ$Run[i] <- runIDnames[j]
        }
    }
}

# Summarize
TotAEQ_Summary_Wide <- summaryBy(TotalLanded+LegalAEQ+SublegalAEQ+TotalAEQ~BasePeriodID+RunID+Run+StockName+FisheryID+TimeStep,
                             data = TotAEQ, FUN = sum)
colnames(TotAEQ_Summary_Wide)[c(1,7:10)] <- c("BP","TotalLanded","LegalAEQ","SublegalAEQ","TotalAEQ")

TotAEQ_Summary <- reshape(TotAEQ_Summary_Wide, varying = c("TotalLanded","LegalAEQ","SublegalAEQ","TotalAEQ"),
                          times = c("TotalLanded","LegalAEQ","SublegalAEQ","TotalAEQ"),
                          v.names = "Mortality", idvar = c("BP","RunID","Run","StockName","FisheryID","TimeStep"),
                          direction = "long")
colnames(TotAEQ_Summary)[7] <- "Type"

TotAEQ_Summary <- TotAEQ_Summary[TotAEQ_Summary$Type %in% figtypes, ]


i=2
for(i in 2:5) {
    ts <- i-1
    figlist_ts <- figlist[ ,c(1,i)]
    colnames(figlist_ts)[2] <- "TS"
    fishlist <- figlist_ts[figlist_ts$TS == 1, 1]
    
    if(length(fishlist) > 0) {
        j=1
        for(j in 1:length(fishlist)) {
            fishnm <- Fishery[Fishery$FisheryID == fishlist[j], 4]
            fishnm <- gsub(":","-",fishnm)
            title <- paste(fishlist[j], " ", fishnm,"; TimeStep ", ts, sep="")
            
            dat_ts <- TotAEQ_Summary[TotAEQ_Summary$FisheryID == fishlist[j] & TotAEQ_Summary$TimeStep == ts, ]
            
            # Chart for total catch by stock
            p <- ggplot(data=dat_ts, aes(StockName,Mortality,fill=Run))
            p <- p + geom_bar(width=0.5, color="black", alpha=1, position="dodge",
                              stat="identity")
            
            p <- p + facet_grid(Type ~ .) + 
                theme(strip.text.x=element_text(size=15)) +
                ggtitle(title) +
                theme(plot.title=element_text(size=15, face="bold")) +
                theme(axis.title.x=element_text(size=10),
                      axis.text.x=element_text(angle=90, size=10,color="black", hjust=1,
                                               vjust=0.25)) +
                theme(axis.title.y=element_text(size=10, vjust=1.3),
                      axis.text.y=element_text(angle=0, size=10,color="black")) + #resize tick labels, move axis lablel (vjust)
                # ylab(ylabel) + xlab("") +
                theme(legend.title=element_text(size=10, face="bold")) +
                theme(legend.text=element_text(size=10)) +
                theme(panel.grid.major.x=element_blank(),
                      panel.grid.minor.x=element_blank()) + #removes the background grid
                #             theme(panel.grid.major.y=element_blank(),
                #                   panel.grid.minor.y=element_blank()) + #removes the background grid
                #             theme(panel.background=element_blank()) + #removes the gray filled back
                scale_x_discrete(expand=c(0.025,0)) + #this gets labels closer to axis, gets rid of gap
                theme(panel.background = element_rect(colour="black",size=1)) + #this adds a border
                theme(plot.margin=unit(c(2,-2,-4,2),"mm"))
            
            if(length(figtypes) > 2) {
                ggsave(paste(outfile,title,".jpg",sep=""),p,height=7.5,width=7.5)
            }
            if(length(figtypes) <= 2) {
                ggsave(paste(outfile,title,".jpg",sep=""),p,height=3.5,width=7.5)
            }
        }
    }
}
    
