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
runID <- c(1,53)

# Set RunID names
runIDnames <- c("Final 2017","March Alt2")

# Identify figures to include in panel, pick any or all of the following:
# "TotalLanded", "LegalAEQ", "SublegalAEQ", "TotalAEQ"
figtypes <- c("TotalAEQ")

# Marked/Unmarked Separate (0) or Combined (1)?
MarkID <- 1

# Ages Separate (0) or Combined (1) or Both (2)?
AgeID <- 1

# Time Steps Separate (0) or Combined (1) or Both (2)?
TimeID <- 1

# Assess time steps 1-3 (0; non-Puget Sound stocks) or 2-4 (1; Puget Sound stocks)
TimeRange <- 0

# Set the paths 
paths = list("C:\\data\\NOF\\2018\\Modeling\\Chinook\\Model Runs\\2018 NOF ChinFRAM.mdb",
             "C:\\data\\NOF\\2018\\Modeling\\Chinook\\Model Runs\\Chin1018-1218\\StkLst.csv",
             "C:\\data\\NOF\\2018\\Modeling\\Chinook\\Model Runs\\Chin1018-1218\\Figs\\")

# Set the input file path for the database
infile = paths[[1]]
StockList = read.csv(paths[[2]])
outfile = paths[[3]]

# # Add Mark Status field to StockList
# for(i in 1:dim(StockList)[1]) {
#     StockList$MarkStatus[i] <- substr(StockList$StockName[i],1,1)
# }

# Trim 'U-' and 'M-' off stock names if combining across mark-status
if(MarkID == 0) {
    StockList$Stock <- StockList$StockName
}
if(MarkID == 1) {
    for(i in 1:dim(StockList)[1]) {
        StockList$Stock[i] <- substring(StockList$StockName[i],3)
    }
}

# Trim to desired columns
StockList <- StockList[StockList$FigID == 1 ,c(1:2,6:7)]

# Identify time step range
if(TimeRange == 0) {
    timerange <- c(1:3)
}
if(TimeRange == 1) {
    timerange <- c(2:4)
}


# Query Database
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

# Subset mortality and escapement to desired RunIDs
mortality <- Mortality[Mortality$RunID %in% runID & Mortality$StockID %in% StockList$StockID, ]
escapement <- Escapement[Escapement$RunID %in% runID & Escapement$StockID %in% StockList$StockID, ]

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
totAEQ <- merge(MortDat,MortDat_AEQ)

# Merge with StockList
totAEQ <- merge(totAEQ,StockList)

# Assign names to RunIDs
i=1
for(i in 1:dim(totAEQ)[1]) {
    j=1
    for(j in 1:length(runID)) {
        if(totAEQ$RunID[i] == runID[j]) {
            totAEQ$Run[i] <- runIDnames[j]
        }
    }
}

# Summarize
if(AgeID == 0 | AgeID == 2) { # keep age field
    if(TimeID == 0 | TimeID == 2) { # keep time step field
        TotAEQ_AgeTS_Wide <- summaryBy(TotalLanded+LegalAEQ+SublegalAEQ+TotalAEQ~BasePeriodID+RunID+Run+Stock+Age+FisheryID+TimeStep,
                                       data = totAEQ[totAEQ$TimeStep %in% timerange, ], FUN = sum)
        colnames(TotAEQ_AgeTS_Wide)[c(1,8:11)] <- c("BP","TotalLanded","LegalAEQ","SublegalAEQ","TotalAEQ")
        
        TotAEQ_AgeTS <- reshape(TotAEQ_AgeTS_Wide, varying = c("TotalLanded","LegalAEQ","SublegalAEQ","TotalAEQ"),
                                times = c("TotalLanded","LegalAEQ","SublegalAEQ","TotalAEQ"),
                                v.names = "Mortality", idvar = c("BP","RunID","Run","Stock","Age","FisheryID","TimeStep"),
                                direction = "long")
        colnames(TotAEQ_AgeTS)[8] <- "Type"
        
        TotAEQ_AgeTS <- TotAEQ_AgeTS[TotAEQ_AgeTS$Type %in% figtypes, ]
        
        TotAEQ_AgeTS <- merge(TotAEQ_AgeTS,Fishery[ ,c(3:4)], all.x = TRUE)
        
        for(i in 1:dim(TotAEQ_AgeTS)[1]) {
            TotAEQ_AgeTS$Age[i] <- paste("Age ", TotAEQ_AgeTS$Age[i], sep = "")
        }
    }
    if(TimeID == 1 | TimeID == 2) { # sum over time steps
        TotAEQ_Age_Wide <- summaryBy(TotalLanded+LegalAEQ+SublegalAEQ+TotalAEQ~BasePeriodID+RunID+Run+Stock+Age+FisheryID,
                                     data = totAEQ[totAEQ$TimeStep %in% timerange, ], FUN = sum)
        colnames(TotAEQ_Age_Wide)[c(1,7:10)] <- c("BP","TotalLanded","LegalAEQ","SublegalAEQ","TotalAEQ")
        
        TotAEQ_Age <- reshape(TotAEQ_Age_Wide, varying = c("TotalLanded","LegalAEQ","SublegalAEQ","TotalAEQ"),
                              times = c("TotalLanded","LegalAEQ","SublegalAEQ","TotalAEQ"),
                              v.names = "Mortality", idvar = c("BP","RunID","Run","Stock","Age","FisheryID"),
                              direction = "long")
        colnames(TotAEQ_Age)[7] <- "Type"
        
        TotAEQ_Age <- TotAEQ_Age[TotAEQ_Age$Type %in% figtypes, ]
        
        TotAEQ_Age <- merge(TotAEQ_Age,Fishery[ ,c(3:4)], all.x = TRUE)
        
        for(i in 1:dim(TotAEQ_Age)[1]) {
            TotAEQ_Age$Age[i] <- paste("Age ", TotAEQ_Age$Age[i], sep = "")
        }
    }
}
if(AgeID == 1 | AgeID == 2) { # sum over ages
    if(TimeID == 0 | TimeID == 2) { # keep time step field
        TotAEQ_TS_Wide <- summaryBy(TotalLanded+LegalAEQ+SublegalAEQ+TotalAEQ~BasePeriodID+RunID+Run+Stock+FisheryID+TimeStep,
                                    data = totAEQ[totAEQ$TimeStep %in% timerange, ], FUN = sum)
        colnames(TotAEQ_TS_Wide)[c(1,7:10)] <- c("BP","TotalLanded","LegalAEQ","SublegalAEQ","TotalAEQ")
        
        TotAEQ_TS <- reshape(TotAEQ_TS_Wide, varying = c("TotalLanded","LegalAEQ","SublegalAEQ","TotalAEQ"),
                             times = c("TotalLanded","LegalAEQ","SublegalAEQ","TotalAEQ"),
                             v.names = "Mortality", idvar = c("BP","RunID","Run","Stock","FisheryID","TimeStep"),
                             direction = "long")
        colnames(TotAEQ_TS)[7] <- "Type"
        
        TotAEQ_TS <- TotAEQ_TS[TotAEQ_TS$Type %in% figtypes, ]
        
        TotAEQ_TS <- merge(TotAEQ_TS,Fishery[ ,c(3:4)], all.x = TRUE)
    }
    if(TimeID == 1 | TimeID == 2) { # sum over time steps
        TotAEQ_Wide <- summaryBy(TotalLanded+LegalAEQ+SublegalAEQ+TotalAEQ~BasePeriodID+RunID+Run+Stock+FisheryID,
                                 data = totAEQ[totAEQ$TimeStep %in% timerange, ], FUN = sum)
        colnames(TotAEQ_Wide)[c(1,6:9)] <- c("BP","TotalLanded","LegalAEQ","SublegalAEQ","TotalAEQ")
        
        TotAEQ <- reshape(TotAEQ_Wide, varying = c("TotalLanded","LegalAEQ","SublegalAEQ","TotalAEQ"),
                          times = c("TotalLanded","LegalAEQ","SublegalAEQ","TotalAEQ"),
                          v.names = "Mortality", idvar = c("BP","RunID","Run","Stock","FisheryID"),
                          direction = "long")
        colnames(TotAEQ)[6] <- "Type"
        
        TotAEQ <- TotAEQ[TotAEQ$Type %in% figtypes, ]
        
        TotAEQ <- merge(TotAEQ,Fishery[ ,c(3:4)], all.x = TRUE)
    }
}

# Add factor levels to Fishery name so x-axis order is correct
Fishery <- Fishery[order(Fishery$FisheryID), ]
TotAEQ$FisheryName <- factor(TotAEQ$FisheryName,levels = c(Fishery$FisheryName), ordered = TRUE)
TotAEQ_TS$FisheryName <- factor(TotAEQ_TS$FisheryName,levels = c(Fishery$FisheryName), ordered = TRUE)
TotAEQ_Age$FisheryName <- factor(TotAEQ_Age$FisheryName,levels = c(Fishery$FisheryName), ordered = TRUE)
TotAEQ_AgeTS$FisheryName <- factor(TotAEQ_AgeTS$FisheryName,levels = c(Fishery$FisheryName), ordered = TRUE)

####################
# Generate Figures #
####################

i=1
for(i in 1:length(unique(TotAEQ$Stock))) { 
    stk <- unique(TotAEQ$Stock)[i]
    j=1
    for(j in 1:length(figtypes)) {
        ylabel <- figtypes[j]
        if(AgeID == 0 | AgeID == 2) {
            if(TimeID == 0 | TimeID == 2) { # By age and time step
                k=1
                for(k in 1:length(unique(TotAEQ_AgeTS$TimeStep))) {
                    title <- paste(stk, " ", ylabel, "; TimeStep ", unique(TotAEQ_AgeTS$TimeStep)[k], sep = "")
                    figdat <- TotAEQ_AgeTS[TotAEQ_AgeTS$TimeStep == unique(TotAEQ_AgeTS$TimeStep)[k] & TotAEQ_AgeTS$Type == figtypes[j], ]
                    
                    p <- ggplot(data=figdat, aes(FisheryName,Mortality,fill=Run))
                    p <- p + geom_bar(width=0.5, color="black", alpha=1, position="dodge",
                                      stat="identity")
                    
                    p <- p + facet_grid(Age ~ .) + 
                        theme_bw() + theme_light() + 
                        theme(strip.text.x=element_text(size=15)) +
                        ggtitle(title) +
                        theme(plot.title=element_text(size=15, face="bold")) +
                        theme(axis.title.x=element_text(size=10),
                              axis.text.x=element_text(angle=90, size=10,color="black", hjust=1,
                                                       vjust=0.25)) +
                        theme(axis.title.y=element_text(size=10, vjust=1.3),
                              axis.text.y=element_text(angle=0, size=10,color="black")) + #resize tick labels, move axis lablel (vjust)
                        ylab(ylabel) + xlab("") +
                        theme(legend.title=element_text(size=10, face="bold")) +
                        theme(legend.text=element_text(size=10)) +
                        scale_x_discrete(expand=c(0.025,0)) + #this gets labels closer to axis, gets rid of gap
                        theme(panel.background = element_rect(colour="black",size=1)) + #this adds a border
                        theme(plot.margin=unit(c(2,-2,-4,2),"mm"))
                    
                    ggsave(paste(outfile,title,".jpeg",sep = ""),p,height = 7.5, width = 7.5)
                }
            }
            if(TimeID == 1 | TimeID == 2) { # By age over all time steps
                title <- paste(stk, " ", ylabel, "; TimeSteps 1-3 ", sep = "")
                figdat <- TotAEQ_Age[TotAEQ_Age$Type == figtypes[j], ]
                
                p <- ggplot(data=figdat, aes(FisheryName,Mortality,fill=Run))
                p <- p + geom_bar(width=0.5, color="black", alpha=1, position="dodge",
                                  stat="identity")
                
                p <- p + facet_grid(Age ~ .) + 
                    theme_bw() + theme_light() + 
                    theme(strip.text.x=element_text(size=15)) +
                    ggtitle(title) +
                    theme(plot.title=element_text(size=15, face="bold")) +
                    theme(axis.title.x=element_text(size=10),
                          axis.text.x=element_text(angle=90, size=10,color="black", hjust=1,
                                                   vjust=0.25)) +
                    theme(axis.title.y=element_text(size=10, vjust=1.3),
                          axis.text.y=element_text(angle=0, size=10,color="black")) + #resize tick labels, move axis lablel (vjust)
                    ylab(ylabel) + xlab("") +
                    theme(legend.title=element_text(size=10, face="bold")) +
                    theme(legend.text=element_text(size=10)) +
                    scale_x_discrete(expand=c(0.025,0)) + #this gets labels closer to axis, gets rid of gap
                    theme(panel.background = element_rect(colour="black",size=1)) + #this adds a border
                    theme(plot.margin=unit(c(2,-2,-4,2),"mm"))
                
                ggsave(paste(outfile,title,".jpeg",sep = ""),p,height = 7.5, width = 7.5)
            }
        }
        if(AgeID == 1 | AgeID == 2) {
            if(TimeID == 0 | TimeID == 2) { # By time step over all ages
                title <- paste(stk, " ", ylabel, "; All Ages ", sep = "")
                figdat <- TotAEQ_TS[TotAEQ_TS$Type == figtypes[j], ]
                for(l in 1:dim(figdat)[1]) {
                    figdat$TimeStep[l] <- paste("Time Step ",figdat$TimeStep[l],sep = "")
                }
                
                p <- ggplot(data=figdat, aes(FisheryName,Mortality,fill=Run))
                p <- p + geom_bar(width=0.5, color="black", alpha=1, position="dodge",
                                  stat="identity")
                
                p <- p + facet_grid(TimeStep ~ .) + 
                    theme_bw() + theme_light() + 
                    theme(strip.text.x=element_text(size=15)) +
                    ggtitle(title) +
                    theme(plot.title=element_text(size=15, face="bold")) +
                    theme(axis.title.x=element_text(size=10),
                          axis.text.x=element_text(angle=90, size=10,color="black", hjust=1,
                                                   vjust=0.25)) +
                    theme(axis.title.y=element_text(size=10, vjust=1.3),
                          axis.text.y=element_text(angle=0, size=10,color="black")) + #resize tick labels, move axis lablel (vjust)
                    ylab(ylabel) + xlab("") +
                    theme(legend.title=element_text(size=10, face="bold")) +
                    theme(legend.text=element_text(size=10)) +
                    scale_x_discrete(expand=c(0.025,0)) + #this gets labels closer to axis, gets rid of gap
                    theme(panel.background = element_rect(colour="black",size=1)) + #this adds a border
                    theme(plot.margin=unit(c(2,-2,-4,2),"mm"))
                
                ggsave(paste(outfile,title,".jpeg",sep = ""),p,height = 7.5, width = 7.5)
            }
        }
    }
    if(TimeID == 1 | TimeID == 2) { # By mortality type over all ages and time steps
        title <- paste(stk, "; All Ages & Time Steps", sep = "")
        figdat <- TotAEQ
        
        p <- ggplot(data=figdat, aes(FisheryName,Mortality,fill=Run))
        p <- p + geom_bar(width=0.5, color="black", alpha=1, position="dodge",
                          stat="identity")
        
        p <- p + facet_grid(Type ~ .) + 
            theme_bw() + theme_light() + 
            theme(strip.text.x=element_text(size=15)) +
            ggtitle(title) +
            theme(plot.title=element_text(size=15, face="bold")) +
            theme(axis.title.x=element_text(size=10),
                  axis.text.x=element_text(angle=90, size=10,color="black", hjust=1,
                                           vjust=0.25)) +
            theme(axis.title.y=element_text(size=10, vjust=1.3),
                  axis.text.y=element_text(angle=0, size=10,color="black")) + #resize tick labels, move axis lablel (vjust)
            ylab("") + xlab("") +
            theme(legend.title=element_text(size=10, face="bold")) +
            theme(legend.text=element_text(size=10)) +
            scale_x_discrete(expand=c(0.025,0)) + #this gets labels closer to axis, gets rid of gap
            theme(panel.background = element_rect(colour="black",size=1)) + #this adds a border
            theme(plot.margin=unit(c(2,-2,-4,2),"mm")) +
            annotate("text", x = 25, y = 6500, label = "Final 2017 River Runsize: 38,880 M, 25,887 UM", size=2) +
            annotate("text", x = 25, y = 5500, label = "March Alt2 River Runsize: 34,570 M, 34,480 UM", size=2)
        
        if(length(figtypes) == 1) {
            ggsave(paste(outfile,title,".jpeg",sep = ""),p,height = 3.5, width = 7.5)
        }
        if(length(figtypes) > 1) {
            ggsave(paste(outfile,title,".jpeg",sep = ""),p,height = 7.5, width = 7.5)
        }
    }
}

