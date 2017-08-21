#######################################################################################
########### CODE TO GENERATE BASE PERIOD FISHERY STOCK COMPOSITION FIGURES ############
#######################################################################################

# Clear workspace
rm(list=ls(all=TRUE))

# set start time for purposes of timing code
strt <- Sys.time()

# # Load any required libraries
library(doBy)
library(readxl)
library(ggplot2)
library(grid)

# Define Y axis label (are the data to be used landed catch or AEQ mortality?)
ylabel = "Total AEQ Mortality"

# Include GSI comparisons? (Yes = 1; No = 0)
GSI_Include = 0

# Source dir
Dir <- "C:\\data\\FRAM\\Base Period\\ProfileFigures\\8.16.17; Round 5 AEQ\\"
#######################################################################################
# Set the paths
paths = list(paste(Dir,"CWTCatch_8.16.17_AEQmort.xlsx",sep=""),
             paste(Dir,"Fishery-By-Stock\\TotalCatch\\TimeSteps\\",sep=""),
             paste(Dir,"Fishery-By-Stock\\TotalCatch\\Totals\\",sep=""),
             paste(Dir,"Fishery-By-Stock\\Percent\\TimeSteps\\",sep=""),
             paste(Dir,"Fishery-By-Stock\\Percent\\Totals\\",sep=""),
             paste(Dir,"Fishery-By-Stock\\Pies\\TimeSteps\\",sep=""),
             paste(Dir,"Fishery-By-Stock\\Pies\\Totals\\",sep=""),
             paste(Dir,"GSI Figs\\GSIStock\\",sep=""),
             paste(Dir,"GSI Figs\\GSIStockGroup\\",sep=""))
#######################################################################################

# Set the output file path
outfile1a = paths[[2]]
outfile1b = paths[[3]]
outfile2a = paths[[4]]
outfile2b = paths[[5]]
outfile3a = paths[[6]]
outfile3b = paths[[7]]
outfile4a = paths[[8]]
outfile4b = paths[[9]]

# Read in source data
catch <- read_excel(paths[[1]], "CWTCatch")
stk <- read_excel(paths[[1]], "stk")
fish <- read_excel(paths[[1]], "fish")
GSI_dat <- read_excel(paths[[1]], "GSI")
GSI_stk <- read_excel(paths[[1]], "GSI_StkLUT")
MSP <- read_excel(paths[[1]], "MSP")

# Reorganize the data
catch <- catch[ ,c(1,7,8,5,6,9)]

# Remove Time Step 4
catch <- catch[catch$TS %in% c(1:3), ]

# Remove CWT = 0
catch <- catch[catch$CWT > 0, ]

# Sum over all ages
Catch <- summaryBy(CWT~Src+Fish+TS+Stk, data=catch, FUN=sum)
colnames(Catch)[5] <- "CWT"

# Reshape New and Old BP catch data into their own columns, side-by-side
Catch <- reshape(Catch, timevar = "Src", idvar = c("Fish","TS","Stk"), 
                  direction = "wide")
# replace NAs with 0
Catch$CWT.NewBP[is.na(Catch$CWT.NewBP)] <- 0
Catch$CWT.OldBP[is.na(Catch$CWT.OldBP)] <- 0
# Reshape back to original (this process added zero values for those that were missing
# when the other (old or new) had a value present)
Catch <- reshape(Catch, timevar = "Src", idvar = c("Fish","TS","Stk"), 
                  direction = "long")

# Merge Catch with stock and fishery names
Stk <- stk[ ,c(5,6,8)]
colnames(Stk)[1] <- "Stk"
colnames(Stk)[2] <- "StkOrder"
Fish <- fish[ ,c(3:5)]
colnames(Fish)[1] <- "Fish"
Catch <- merge(Stk,Catch)
Catch <- merge(Fish,Catch)
Catch <- Catch[ ,c(8,1:3,7,4:6,9)]
Catch <- Catch[order(Catch$Src,Catch$Fish,Catch$TS,Catch$Stk), ]
colnames(Catch)[1] <- "BP"
colnames(Catch)[9] <- "CWT"

# Add records for CWT by fishery and stock, summed over all time steps
Catch2 <- summaryBy(CWT~BP+Fish+FisheryName+FisheryTitle+Stk+StkOrder+StockName, 
                    data=Catch, FUN=sum)
colnames(Catch2)[8] <- "CWT"
Catch2$TS <- rep("Total", dim(Catch2)[1])
Catch2 <- Catch2[ ,c(1:4,9,5:8)]
Catch <- rbind(Catch,Catch2)
Catch <- Catch[order(Catch$BP,Catch$Fish,Catch$TS,Catch$Stk), ]

# Add factor levels to StockName so x-axis order is correct
Stk <- Stk[order(Stk$StkOrder), ]
Catch$StockName <- factor(Catch$StockName,levels=c(Stk$StockName),ordered=TRUE)


i=1
for(i in 1:length(unique(Catch$Fish))) {
    # identify fishery and subset data to fishery
    fishery <- unique(Catch$Fish)[i]
    dat <- subset(Catch, Catch$Fish == fishery)
    
    j=1
    for(j in 1:length(unique(dat$TS))) {
        ts <- unique(dat$TS)[j]
        dat_ts <- subset(dat, dat$TS == ts)
        if(ts %in% c(1:3)) {
            title <- as.character(paste(unique(dat_ts$Fish), " - ", 
                                        unique(dat_ts$FisheryTitle), "; Time Step ", ts, 
                                        sep=""))
        }
        if(ts == "Total") {
            title <- as.character(paste(unique(dat_ts$Fish), " - ",
                                        unique(dat_ts$FisheryTitle), "; All Time Steps", sep=""))
        }
        newSum <- summaryBy(CWT~BP, data=dat_ts, FUN=sum)[1,2]
        oldSum <- summaryBy(CWT~BP, data=dat_ts, FUN=sum)[2,2]
        
        # Calculate new stock comp proportions
        k=1
        for(k in 1:length(dat_ts$CWT)) {
            if(dat_ts$BP[k] == "NewBP") {
                dat_ts$CWT_prop[k] <- dat_ts$CWT[k] / newSum
            }
            if(dat_ts$BP[k] == "OldBP") {
                dat_ts$CWT_prop[k] <- dat_ts$CWT[k] / oldSum
            }
        }
        
        # Code for removing stocks that are inconsequential
        # (i.e. OldBP + NewBP sum to < 1% of catch
        removalList <- numeric()
        DFsize = length(dat_ts$Stk)/2

        l=1
        for (l in 1:DFsize) {
            StkIndex = dat_ts$Stk[l]
            derekpropcounts <-subset(dat_ts, dat_ts$Stk == StkIndex)
            derekpropcounts$CWT_prop[is.na(derekpropcounts$CWT_prop)] <- 0
            dereksum <- derekpropcounts$CWT_prop[1]+derekpropcounts$CWT_prop[2]
            if (dereksum < 0.01) {
                newremoval1 = l
                newremoval2 = l+(DFsize)
                removalList = c(removalList, newremoval1, newremoval2)
            }
        }

        if(length(removalList) > 0) {
            dat_ts = dat_ts[-removalList,]
        }
        
        # Get MSP for fishery
        NewMSP <- MSP[MSP$BP == "NewBP" & MSP$FisheryID == fishery, 3]
        OldMSP <- MSP[MSP$BP == "OldBP" & MSP$FisheryID == fishery, 3]
        
        # If non-model stocks are present, calculate new stock comps and add non-model stock prop
        if((1-NewMSP) + (1-OldMSP) > 0) {
            # Calculate new stock comp proportions
            k=1
            for(k in 1:length(dat_ts$CWT)) {
                if(dat_ts$BP[k] == "NewBP") {
                    dat_ts$CWT_prop[k] <- dat_ts$CWT[k] / newSum * NewMSP
                }
                if(dat_ts$BP[k] == "OldBP") {
                    dat_ts$CWT_prop[k] <- dat_ts$CWT[k] / oldSum * OldMSP
                }
            }
            
            # Add non-model stock prop
            newRec <- dat_ts[1,]
            
            newRec_new <- newRec
            newRec_new[1] <- "NewBP"
            newRec_new[c(6,7,9)] <- 0
            newRec_new[8] <- "Non-FRAM"
            newRec_new[10] <- 1 - NewMSP
            
            newRec_old <- newRec
            newRec_old[1] <- "OldBP"
            newRec_old[c(6,7,9)] <- 0
            newRec_old[8] <- "Non-FRAM"
            newRec_old[10] <- 1 - OldMSP
            
            dat_ts <- rbind(dat_ts, newRec_old, newRec_new)
        }
        
        
        ################################
        # Chart for total catch by stock
        ################################
        p <- ggplot(data=dat_ts, aes(StockName,CWT,fill=BP))
        p <- p + geom_bar(width=0.5, color="black", alpha=1, position="dodge",
                          stat="identity")

        p <- p + theme(strip.text.x=element_text(size=15)) +
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
            theme(panel.grid.major.x=element_blank(),
                  panel.grid.minor.x=element_blank()) + #removes the background grid
#             theme(panel.grid.major.y=element_blank(),
#                   panel.grid.minor.y=element_blank()) + #removes the background grid
#             theme(panel.background=element_blank()) + #removes the gray filled back
            scale_x_discrete(expand=c(0.025,0)) + #this gets labels closer to axis, gets rid of gap
            theme(panel.background = element_rect(colour="black",size=1)) + #this adds a border
            theme(plot.margin=unit(c(2,-2,-4,2),"mm"))
#         # Add red vertical line to separate PS stock groups
#         x <- unique(dat_ts$StkOrder)
#         x <- length(x[x < 21]) + 0.5
#         p <- p + geom_vline(aes(xintercept = x), color = "red", size = 1)

        # save
        if(ts %in% c(1:3)) {
            ggsave(paste(outfile1a,title,".jpg",sep=""),p,height=5,width=7.5)
        }
        if(ts == "Total") {
            ggsave(paste(outfile1b,title,".jpg",sep=""),p,height=5,width=7.5)
        }
        

        ###########################################
        # Chart for percent of total catch by stock
        ###########################################
        p <- ggplot(data=dat_ts, aes(StockName,CWT_prop,fill=BP))
        p <- p + geom_bar(width=0.5, color="black", alpha=1, position="dodge",
                          stat="identity")

        p <- p + theme(strip.text.x=element_text(size=15)) +
            ggtitle(title) +
            theme(plot.title=element_text(size=15, face="bold")) +
            theme(axis.title.x=element_text(size=10),
                  axis.text.x=element_text(angle=90, size=10,color="black", hjust=1,
                                           vjust=0.25)) +
            theme(axis.title.y=element_text(size=10, vjust=1.3),
                  axis.text.y=element_text(angle=0, size=10,color="black")) + #resize tick labels, move axis lablel (vjust)
            ylab(paste("Proportion of ", ylabel, sep="")) + xlab("") +
            theme(legend.title=element_text(size=10, face="bold")) +
            theme(legend.text=element_text(size=10)) +
            theme(panel.grid.major.x=element_blank(),
                  panel.grid.minor.x=element_blank()) + #removes the background grid
#             theme(panel.grid.major.y=element_blank(),
#                   panel.grid.minor.y=element_blank()) + #removes the background grid
#             theme(panel.background=element_blank()) + #removes the gray filled back
            scale_x_discrete(expand=c(0.025,0))+ #this gets labels closer to axis, gets rid of gap
            theme(panel.background = element_rect(colour="black",size=1)) + #this adds a border
            theme(plot.margin=unit(c(2,-2,-4,2),"mm"))
#         # Add red vertical line to separate PS stock groups
#         x <- unique(dat_ts$StkOrder)
#         x <- length(x[x < 21]) + 0.5
#         p <- p + geom_vline(aes(xintercept = x), color = "red", size = 1)

        # save
        if(ts %in% c(1:3)) {
            ggsave(paste(outfile2a,title,".jpg",sep=""),p,height=5,width=7.5)
        }
        if(ts == "Total") {
            ggsave(paste(outfile2b,title,".jpg",sep=""),p,height=5,width=7.5)
        }

        
#         #################################
#         # Chart for fishery pies by stock
#         #################################
#         n <- ceiling(length(unique(dat_ts$StockName)) / 2)
#         p <- ggplot(data=dat_ts, aes(BP,CWT_prop,fill=StockName))
#         p <- p + geom_bar(width=0.9, color="black", alpha=1, stat="identity")
#         p <- p + coord_polar(theta="y")       
#         p <- p + theme(strip.text.x=element_text(size=15)) +
#             ggtitle(title) +
#             theme(plot.title=element_text(size=15, face="bold")) +
#             theme(axis.title.x=element_text(size=12), 
#                 axis.text.x=element_text(size=10,color="black", hjust=1,
#                                         vjust=0.25)) +
#             theme(axis.title.y=element_text(size=12, vjust=1.3),
#                 axis.text.y=element_text(angle=0, size=10,color="black")) + #resize tick labels, move axis lablel (vjust)
#             ylab("") + xlab("Base Period") +
#             theme(legend.title=element_text(size=10, face="bold")) +
#             theme(legend.text=element_text(size=10)) +
#             guides(fill=guide_legend(nrow=n)) +
# #             theme(panel.grid.major.x=element_blank(), 
# #                   panel.grid.minor.x=element_blank()) + #removes the background grid
# #             theme(panel.grid.major.y=element_blank(), 
# #                   panel.grid.minor.y=element_blank()) + #removes the background grid
# #             theme(panel.background=element_blank()) + #removes the gray filled back
#             scale_x_discrete(expand=c(0.025,0))+ #this gets labels closer to axis, gets rid of gap
#             theme(panel.background = element_rect(colour="black",size=1)) + #this adds a border
#             theme(plot.margin=unit(c(2,-2,-4,2),"mm"))
#         
#         # save
#         if(ts %in% c(1:3)) {
#             ggsave(paste(outfile3a,title,".jpg",sep=""),p,height=7.5,width=7.5)
#         }
#         if(ts == "Total") {
#             ggsave(paste(outfile3b,title,".jpg",sep=""),p,height=7.5,width=7.5)
#         }        
    }
}

###############
# GSI Figures #
###############

# Filter Catch to GSI fisheries
GSI_fish <- fish[fish$GSI_Include == 1, ]
GSI_Catch <- Catch[Catch$Fish %in% GSI_fish$FisheryID, ]

# Merge GSI stock data into GSI catch
GSI_Stk <- GSI_stk[ ,c(1,3:4)]
GSI_Catch <- merge(GSI_Stk, GSI_Catch)

# Remove unneeded fields
GSI_Catch <- GSI_Catch[ ,c(4,5,8,2,11)]

# sum by GSI stock
GSI_Catch <- summaryBy(CWT~BP+Fish+TS+GSIStk, dat = GSI_Catch, FUN = sum)

# Filter to only TS 1-3
GSI_Catch <- GSI_Catch[GSI_Catch$TS %in% c(1:3), ]

# Calculate proportion of total catch by stock for each BP, Fishery, TS
sums <- summaryBy(CWT.sum~BP+Fish+TS, data = GSI_Catch, FUN = sum)
i=1
for(i in 1:dim(GSI_Catch)[1]) {
    BP <- GSI_Catch[i,1]
    Fsh <- GSI_Catch[i,2]
    TS <- GSI_Catch[i,3]
    sum_i <- sums[sums$BP == BP & sums$Fish == Fsh & sums$TS == TS, 4]
    msp_i <- MSP[MSP$BP == BP & MSP$FisheryID == Fsh, 3]
    exp_sum_i <- sum_i / msp_i
    GSI_Catch$prop[i] <- GSI_Catch$CWT.sum[i] / exp_sum_i
}

# Calculate % non-FRAM stock and add to GSI_Catch
NonFRAM <- summaryBy(prop~BP+Fish+TS, data = GSI_Catch, FUN = sum)
NonFRAM$Stat <- 1 - NonFRAM$prop.sum
NonFRAM$GSIStk <- c(rep(14,dim(NonFRAM)[1]))
NonFRAM <- NonFRAM[ ,c(1,2,3,6,5)]

# Remove unneeded fields
GSI_Catch <- GSI_Catch[ ,c(1:4,6)]
GSI_Dat <- GSI_dat[ ,c(5,1,3,6,10)]

# Make sure names in each data frame are identical
names(GSI_Catch) <- names(GSI_Dat)
names(NonFRAM) <- names(GSI_Dat)

# combine all three data frames
GSI_Data <- rbind(GSI_Catch,NonFRAM,GSI_Dat)

# make fishery and stock tables for merging later
GSI_Stk <- unique(GSI_stk[ ,c(3:4)])
GSI_Fish <- GSI_fish[ ,c(3:5)]

# Make stock table for adding factors later so x-axis order is correct
tempStk <- GSI_Stk[order(GSI_Stk$GSIStk) ,c(2,1)]

#################
# Figs by stock #
#################

i=1
for(i in 1:length(unique(GSI_Data$FisheryID))) {
    # identify fishery and subset data to fishery
    fishery <- unique(GSI_Data$FisheryID)[i]
    dat <- subset(GSI_Data, GSI_Data$FisheryID == fishery)
    
    # filter out GSI averages and 2013 & 2014
    # dat <- dat[dat$Src != "GSI_Avg", ]
    dat <- dat[dat$Src != "GSI_2012", ]
    dat <- dat[dat$Src != "GSI_2013", ]
    dat <- dat[dat$Src != "GSI_2014", ]
    
    j=1
    for(j in 1:length(unique(dat$TS))) {
        ts <- unique(dat$TS)[j]
        dat_ts <- subset(dat, dat$TS == ts)
        
        # 'if' statement to prevent making figure if no GSI data are present for TS
        if(length(unique(dat_ts$Src)) > 2) {
            # Reshape so unique records in "Src" become their own columns, side-by-side
            dat_ts <- reshape(dat_ts, timevar = "Src", idvar = c("FisheryID","TS","GSIStk"),
                              direction = "wide")
            # replace NAs with 0
            dat_ts[is.na(dat_ts)] <- 0
            
            # Reshape back to original (this process added zero values for those that were missing
            # when the other (old or new) had a value present)
            dat_ts <- reshape(dat_ts, timevar = "Src", idvar = c("FisheryID","TS","GSIStk"),
                              direction = "long")
            
            # Rename 
            names(dat_ts)[5] <- "Stat"
            
            # Add in fishery and stock names
            dat_ts <- merge(GSI_Stk,dat_ts)
            dat_ts <- merge(GSI_Fish,dat_ts)
            
            # Add factor levels to StockName so x-axis order is correct
            dat_ts$GSIStkName <- factor(dat_ts$GSIStkName, levels=c(tempStk$GSIStkName),
                                        ordered=TRUE)
            
            # remove stocks with zero presence
            removalList <- summaryBy(Stat~FisheryID+TS+GSIStk, data = dat_ts, FUN = sum)
            removalList <- removalList[removalList$Stat.sum == 0, ]
            dat_ts <- dat_ts[!(dat_ts$GSIStk %in% removalList$GSIStk), ]
            
            title <- as.character(paste(unique(dat_ts$FisheryID), " - ", 
                                        unique(dat_ts$FisheryTitle), "; Time Step ", ts, 
                                        sep=""))
            
            p <- ggplot(data=dat_ts, aes(GSIStkName,Stat,fill=Src))
            p <- p + geom_bar(width=0.75, color="black", alpha=1, position="dodge", 
                              stat="identity")
            
            p <- p + scale_fill_brewer(palette="BrBG") + #change the color scheme as needed
                theme(strip.text.x=element_text(size=15)) +
                ggtitle(title) +
                theme(plot.title=element_text(size=15, face="bold")) +
                theme(axis.title.x=element_text(size=10), 
                      axis.text.x=element_text(angle=90, size=10,color="black", hjust=1,
                                               vjust=0.25)) +
                theme(axis.title.y=element_text(size=10, vjust=1.3),
                      axis.text.y=element_text(angle=0, size=10,color="black")) + #resize tick labels, move axis lablel (vjust)
                ylab(paste("Proportion of ", ylabel, sep="")) + xlab("") + #ylim(0,0.6) + #ylim sets y axis bounds
                theme(legend.title=element_text(size=10, face="bold")) +
                theme(legend.text=element_text(size=10)) +
                theme(panel.grid.major.x=element_blank(), 
                      panel.grid.minor.x=element_blank()) + #removes the background grid
                scale_x_discrete(expand=c(0.025,0)) + #this gets labels closer to axis, gets rid of gap
                theme(panel.background = element_rect(colour="black",size=1)) + #this adds a border
                theme(plot.margin=unit(c(2,-2,-4,2),"mm"))
            
            # save plot
            ggsave(paste(outfile4a,title,".jpg",sep=""),p,height=5,width=7.5)
        }
    }
}

#######################
# Figs by stock group #
#######################

GSI_GrpStk <- unique(GSI_stk[ ,c(3,5)])
GSI_GrpData <- merge(GSI_GrpStk,GSI_Data)
GSI_GrpData <- summaryBy(Stat~Src+FisheryID+TS+GSIGrp, data = GSI_GrpData, FUN = sum)

# Rename 
names(GSI_GrpData)[5] <- "Stat"

GSI_GrpStk2 <- unique(GSI_stk[ ,c(5:6)])
tempStk <- GSI_GrpStk2[order(GSI_GrpStk2$GSIGrp) ,c(2,1)]

i=1
for(i in 1:length(unique(GSI_GrpData$FisheryID))) {
    # identify fishery and subset data to fishery
    fishery <- unique(GSI_GrpData$FisheryID)[i]
    dat <- subset(GSI_GrpData, GSI_GrpData$FisheryID == fishery)
    
    # filter out GSI averages and 2013 & 2014
    # dat <- dat[dat$Src != "GSI_Avg", ]
    dat <- dat[dat$Src != "GSI_2012", ]
    dat <- dat[dat$Src != "GSI_2013", ]
    dat <- dat[dat$Src != "GSI_2014", ]
    
    j=1
    for(j in 1:length(unique(dat$TS))) {
        ts <- unique(dat$TS)[j]
        dat_ts <- subset(dat, dat$TS == ts)
        
        # 'if' statement to prevent making figure if no GSI data are present for TS
        if(length(unique(dat_ts$Src)) > 2) {
            # Reshape so unique records in "Src" become their own columns, side-by-side
            dat_ts <- reshape(dat_ts, timevar = "Src", idvar = c("FisheryID","TS","GSIGrp"),
                              direction = "wide")
            # replace NAs with 0
            dat_ts[is.na(dat_ts)] <- 0
            
            # Reshape back to original (this process added zero values for those that were missing
            # when the other (old or new) had a value present)
            dat_ts <- reshape(dat_ts, timevar = "Src", idvar = c("FisheryID","TS","GSIGrp"),
                              direction = "long")
            
            # Rename 
            names(dat_ts)[5] <- "Stat"
            
            # Add in fishery and stock names
            dat_ts <- merge(GSI_GrpStk2,dat_ts)
            dat_ts <- merge(GSI_Fish,dat_ts)
            
            # Add factor levels to StockName so x-axis order is correct
            dat_ts$GSIGrpName <- factor(dat_ts$GSIGrpName, levels=c(tempStk$GSIGrpName),
                                        ordered=TRUE)
            
            # remove stocks with zero presence
            removalList <- summaryBy(Stat~FisheryID+TS+GSIGrp, data = dat_ts, FUN = sum)
            removalList <- removalList[removalList$Stat.sum == 0, ]
            dat_ts <- dat_ts[!(dat_ts$GSIGrp %in% removalList$GSIGrp), ]
            
            title <- as.character(paste(unique(dat_ts$FisheryID), " - ", 
                                        unique(dat_ts$FisheryTitle), "; Time Step ", ts, 
                                        sep=""))
            
            p <- ggplot(data=dat_ts, aes(GSIGrpName,Stat,fill=Src))
            p <- p + geom_bar(width=0.75, color="black", alpha=1, position="dodge", 
                              stat="identity")
            
            p <- p + scale_fill_brewer(palette="BrBG") + #change the color scheme as needed
                theme(strip.text.x=element_text(size=15)) +
                ggtitle(title) +
                theme(plot.title=element_text(size=15, face="bold")) +
                theme(axis.title.x=element_text(size=10), 
                      axis.text.x=element_text(angle=90, size=10,color="black", hjust=1,
                                               vjust=0.25)) +
                theme(axis.title.y=element_text(size=10, vjust=1.3),
                      axis.text.y=element_text(angle=0, size=10,color="black")) + #resize tick labels, move axis lablel (vjust)
                ylab(paste("Proportion of ", ylabel, sep="")) + xlab("") + #ylim(0,0.6) + #ylim sets y axis bounds
                theme(legend.title=element_text(size=10, face="bold")) +
                theme(legend.text=element_text(size=10)) +
                theme(panel.grid.major.x=element_blank(), 
                      panel.grid.minor.x=element_blank()) + #removes the background grid
                scale_x_discrete(expand=c(0.025,0)) + #this gets labels closer to axis, gets rid of gap
                theme(panel.background = element_rect(colour="black",size=1)) + #this adds a border
                theme(plot.margin=unit(c(2,-2,-4,2),"mm"))
            
            # save plot
            ggsave(paste(outfile4b,title,".jpg",sep=""),p,height=5,width=7.5)
        }
    }
}

nd <- Sys.time()
tm <- nd - strt
tm


