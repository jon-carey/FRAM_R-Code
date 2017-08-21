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

# Source dir
Dir <- "C:\\data\\FRAM\\Base Period\\ProfileFigures\\8.16.17; Round 5 AEQ\\"
#######################################################################################
# Set the paths to the MSF_Impacts and MSP databases for Baseline_RetRel function
paths = list(paste(Dir,"CWTCatch_8.16.17_AEQmort.xlsx",sep=""),
             paste(Dir,"Stock-By-Fishery\\With Escapement\\TotalCatch\\TimeSteps\\",sep=""),
             paste(Dir,"Stock-By-Fishery\\With Escapement\\TotalCatch\\Totals\\",sep=""),
             paste(Dir,"Stock-By-Fishery\\With Escapement\\Percent\\TimeSteps\\",sep=""),
             paste(Dir,"Stock-By-Fishery\\With Escapement\\Percent\\Totals\\",sep=""),
             paste(Dir,"Stock-By-Fishery\\With Escapement\\Pies\\TimeSteps\\",sep=""),
             paste(Dir,"Stock-By-Fishery\\With Escapement\\Pies\\Totals\\",sep=""),
             paste(Dir,"Stock-By-Fishery\\No Escapement\\TotalCatch\\TimeSteps\\",sep=""),
             paste(Dir,"Stock-By-Fishery\\No Escapement\\TotalCatch\\Totals\\",sep=""),
             paste(Dir,"Stock-By-Fishery\\No Escapement\\Percent\\TimeSteps\\",sep=""),
             paste(Dir,"Stock-By-Fishery\\No Escapement\\Percent\\Totals\\",sep=""),
             paste(Dir,"Stock-By-Fishery\\No Escapement\\Pies\\TimeSteps\\",sep=""),
             paste(Dir,"Stock-By-Fishery\\No Escapement\\Pies\\Totals\\",sep=""))
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
outfile5a = paths[[10]]
outfile5b = paths[[11]]
outfile6a = paths[[12]]
outfile6b = paths[[13]]

# Read in source data
catch <- read_excel(paths[[1]], "CWTCatch")
stk <- read_excel(paths[[1]], "stk")
fish <- read_excel(paths[[1]], "fish")

# Reorganize the data
catch <- catch[ ,c(1,7,8,5,6,9)]

# Remove Time Step 4
catch <- catch[catch$TS %in% c(1:3), ]

# Remove CWT = 0
catch <- catch[catch$CWT > 0, ]

# Sum over all ages
Catch <- summaryBy(CWT~Src+Fish+TS+Stk, data=catch, FUN=sum)
colnames(Catch)[5] <- "CWT"

# Reshape to New and Old BP catch data into their own columns, side-by-side
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
Stk <- stk[ ,c(5,8)]
colnames(Stk)[1] <- "Stk"
Fish <- fish[ ,c(3:5)]
colnames(Fish)[1] <- "Fish"
Catch <- merge(Stk,Catch)
Catch <- merge(Fish,Catch)
Catch <- Catch[ ,c(7,1:3,6,4,5,8)]
Catch <- Catch[order(Catch$Src,Catch$Fish,Catch$TS,Catch$Stk), ]
colnames(Catch)[1] <- "BP"
colnames(Catch)[8] <- "CWT"

# Add records for CWT by fishery and stock, summed over all time steps
Catch2 <- summaryBy(CWT~BP+Fish+FisheryName+FisheryTitle+Stk+StockName, 
                    data=Catch, FUN=sum)
colnames(Catch2)[7] <- "CWT"
Catch2$TS <- rep("Total", dim(Catch2)[1])
Catch2 <- Catch2[ ,c(1:4,8,5:7)]
Catch <- rbind(Catch,Catch2)
Catch <- Catch[order(Catch$BP,Catch$Fish,Catch$TS,Catch$Stk), ]

# Add factor levels to FisheryName so x-axis order is correct
Catch$FisheryName <- factor(Catch$FisheryName,levels=c(Fish$FisheryName),ordered=TRUE)


# i=1
# for(i in 1:length(unique(Catch$Stk))) {
#     # identify stock and subset data to stock
#     stock <- unique(Catch$Stk)[i]
#     dat <- subset(Catch, Catch$Stk == stock)
#     
#     j=1
#     for(j in 1:length(unique(dat$TS))) {
#         ts <- unique(dat$TS)[j]
#         dat_ts <- subset(dat, dat$TS == ts)
#         if(ts %in% c(1:3)) {
#             title <- as.character(paste(unique(dat_ts$Stk), " - ", 
#                                         unique(dat_ts$StockName), "; Time Step ", ts, 
#                                         sep=""))
#         }
#         if(ts == "Total") {
#             title <- as.character(paste(unique(dat_ts$Stk), " - ",
#                                         unique(dat_ts$StockName), "; All Time Steps", sep=""))
#         }
#         newSum <- summaryBy(CWT~BP, data=dat_ts, FUN=sum)[1,2]
#         oldSum <- summaryBy(CWT~BP, data=dat_ts, FUN=sum)[2,2]
#         k=1
#         for(k in 1:length(dat_ts$CWT)) {
#             if(dat_ts$BP[k] == "NewBP") {
#                 dat_ts$CWT_prop[k] <- dat_ts$CWT[k] / newSum
#             }
#             if(dat_ts$BP[k] == "OldBP") {
#                 dat_ts$CWT_prop[k] <- dat_ts$CWT[k] / oldSum
#             }
#         }
#         
#         
#         # Code for removing stocks that are inconsequential
#         # (i.e. OldBP + NewBP sum to < 1% of catch
#         RemovalsList <- numeric()
#         DFsize = length(dat_ts$Stk)/2
# 
#         l=1
#         for (l in 1:DFsize) {
#             FishIndex = dat_ts$Fish[l]
#             derekpropcounts <-subset(dat_ts, dat_ts$Fish == FishIndex)
#             derekpropcounts$CWT_prop[is.na(derekpropcounts$CWT_prop)] <- 0
#             dereksum <- derekpropcounts$CWT_prop[1]+derekpropcounts$CWT_prop[2]
#             if (dereksum < 0.01) {
#                 newremoval1 = l
#                 newremoval2 = l+(DFsize)
#                 RemovalsList = c(RemovalsList, newremoval1, newremoval2)
#             }
#         }
# 
#         dat_ts = dat_ts[-RemovalsList,]
#         
#         ###########################################
#         # Chart for total catch of sotck by fishery
#         ###########################################
#         p <- ggplot(data=dat_ts, aes(FisheryName,CWT,fill=BP))
#         p <- p + geom_bar(width=0.5, color="black", alpha=1, position="dodge", 
#                           stat="identity")
#         
#         p <- p + theme(strip.text.x=element_text(size=15)) +
#             ggtitle(title) +
#             theme(plot.title=element_text(size=15, face="bold")) +
#             theme(axis.title.x=element_text(size=10), 
#                   axis.text.x=element_text(angle=90, size=10,color="black", hjust=1,
#                                            vjust=0.25)) +
#             theme(axis.title.y=element_text(size=10, vjust=1.3),
#                   axis.text.y=element_text(angle=0, size=10,color="black")) + #resize tick labels, move axis lablel (vjust)
#             ylab("Total Catch by Fishery") + xlab("") +
#             theme(legend.title=element_text(size=10, face="bold")) +
#             theme(legend.text=element_text(size=10)) +
#             theme(panel.grid.major.x=element_blank(), 
#                   panel.grid.minor.x=element_blank()) + #removes the background grid
#             #             theme(panel.grid.major.y=element_blank(), 
#             #                   panel.grid.minor.y=element_blank()) + #removes the background grid
#             #             theme(panel.background=element_blank()) + #removes the gray filled back
#             scale_x_discrete(expand=c(0.025,0)) + #this gets labels closer to axis, gets rid of gap
#             theme(panel.background = element_rect(colour="black",size=1)) + #this adds a border
#             theme(plot.margin=unit(c(2,-2,-4,2),"mm"))
#         
#         # save
#         if(ts %in% c(1:3)) {
#             ggsave(paste(outfile1a,title,".jpg",sep=""),p,height=5,width=7.5)
#         }
#         if(ts == "Total") {
#             ggsave(paste(outfile1b,title,".jpg",sep=""),p,height=5,width=7.5)
#         }
#         
#         ######################################################
#         # Chart for percent of total catch of stock by fishery
#         ######################################################
#         p <- ggplot(data=dat_ts, aes(FisheryName,CWT_prop,fill=BP))
#         p <- p + geom_bar(width=0.5, color="black", alpha=1, position="dodge", 
#                           stat="identity")
#         
#         p <- p + theme(strip.text.x=element_text(size=15)) +
#             ggtitle(title) +
#             theme(plot.title=element_text(size=15, face="bold")) +
#             theme(axis.title.x=element_text(size=10), 
#                   axis.text.x=element_text(angle=90, size=10,color="black", hjust=1,
#                                            vjust=0.25)) +
#             theme(axis.title.y=element_text(size=10, vjust=1.3),
#                   axis.text.y=element_text(angle=0, size=10,color="black")) + #resize tick labels, move axis lablel (vjust)
#             ylab("Proportion of Total Catch by Fishery") + xlab("") +
#             theme(legend.title=element_text(size=10, face="bold")) +
#             theme(legend.text=element_text(size=10)) +
#             theme(panel.grid.major.x=element_blank(), 
#                   panel.grid.minor.x=element_blank()) + #removes the background grid
#             #             theme(panel.grid.major.y=element_blank(), 
#             #                   panel.grid.minor.y=element_blank()) + #removes the background grid
#             #             theme(panel.background=element_blank()) + #removes the gray filled back
#             scale_x_discrete(expand=c(0.025,0))+ #this gets labels closer to axis, gets rid of gap
#             theme(panel.background = element_rect(colour="black",size=1)) + #this adds a border
#             theme(plot.margin=unit(c(2,-2,-4,2),"mm"))
#         
#         # save
#         if(ts %in% c(1:3)) {
#             ggsave(paste(outfile2a,title,".jpg",sep=""),p,height=5,width=7.5)
#         }
#         if(ts == "Total") {
#             ggsave(paste(outfile2b,title,".jpg",sep=""),p,height=5,width=7.5)
#         }
#         
#         
#         ###########################################
#         # Chart for stock pies by fishery
#         ###########################################
#         n <- ceiling(length(unique(dat_ts$FisheryName)) / 2)
#         p <- ggplot(data=dat_ts, aes(BP,CWT_prop,fill=FisheryName))
#         p <- p + geom_bar(width=0.9, color="black", alpha=1, stat="identity")
#         p <- p + coord_polar(theta="y")
#         p <- p + theme(strip.text.x=element_text(size=15)) +
#             ggtitle(title) +
#             theme(plot.title=element_text(size=15, face="bold")) +
#             theme(axis.title.x=element_text(size=12), 
#                   axis.text.x=element_text(size=10,color="black", hjust=1,
#                                            vjust=0.25)) +
#             theme(axis.title.y=element_text(size=12, vjust=1.3),
#                   axis.text.y=element_text(angle=0, size=10,color="black")) + #resize tick labels, move axis lablel (vjust)
#             ylab("") + xlab("Base Period") +
#             theme(legend.title=element_text(size=10, face="bold")) +
#             theme(legend.text=element_text(size=10)) +
#             guides(fill=guide_legend(nrow=n)) +
#             #             theme(panel.grid.major.x=element_blank(), 
#             #                   panel.grid.minor.x=element_blank()) + #removes the background grid
#             #             theme(panel.grid.major.y=element_blank(), 
#             #                   panel.grid.minor.y=element_blank()) + #removes the background grid
#             #             theme(panel.background=element_blank()) + #removes the gray filled back
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
#     }
# }


# Filter Out Escapement and repeat
Catch <- Catch[!(Catch$Fish == 74), ]

i=1
for(i in 1:length(unique(Catch$Stk))) {
    # identify stock and subset data to stock
    stock <- unique(Catch$Stk)[i]
    dat <- subset(Catch, Catch$Stk == stock)
    
    j=1
    for(j in 1:length(unique(dat$TS))) {
        ts <- unique(dat$TS)[j]
        dat_ts <- subset(dat, dat$TS == ts)
        if(ts %in% c(1:3)) {
            title <- as.character(paste(unique(dat_ts$Stk), " - ", 
                                        unique(dat_ts$StockName), "; Time Step ", ts, 
                                        sep=""))
        }
        if(ts == "Total") {
            title <- as.character(paste(unique(dat_ts$Stk), " - ",
                                        unique(dat_ts$StockName), "; ", ts, sep=""))
        }
        newSum <- summaryBy(CWT~BP, data=dat_ts, FUN=sum)[1,2]
        oldSum <- summaryBy(CWT~BP, data=dat_ts, FUN=sum)[2,2]
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
        RemovalsList <- numeric()
        DFsize = length(dat_ts$Stk)/2
        
        l=1
        for (l in 1:DFsize) {
            FishIndex = dat_ts$Fish[l]
            derekpropcounts <-subset(dat_ts, dat_ts$Fish == FishIndex)
            derekpropcounts$CWT_prop[is.na(derekpropcounts$CWT_prop)] <- 0
            dereksum <- derekpropcounts$CWT_prop[1]+derekpropcounts$CWT_prop[2]
            if (dereksum < 0.01) {
                newremoval1 = l
                newremoval2 = l+(DFsize)
                RemovalsList = c(RemovalsList, newremoval1, newremoval2)
            }
        }
        
        if(length(RemovalsList) > 0) {
            dat_ts = dat_ts[-RemovalsList,]
        }
        
        
        ###########################################
        # Chart for total catch of sotck by fishery
        ###########################################
        p <- ggplot(data=dat_ts, aes(FisheryName,CWT,fill=BP))
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

        # save
        if(ts %in% c(1:3)) {
            ggsave(paste(outfile4a,title,".jpg",sep=""),p,height=5,width=7.5)
        }
        if(ts == "Total") {
            ggsave(paste(outfile4b,title,".jpg",sep=""),p,height=5,width=7.5)
        }
        
        ######################################################
        # Chart for percent of total catch of stock by fishery
        ######################################################
        p <- ggplot(data=dat_ts, aes(FisheryName,CWT_prop,fill=BP))
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
        
        # save
        if(ts %in% c(1:3)) {
            ggsave(paste(outfile5a,title,".jpg",sep=""),p,height=5,width=7.5)
        }
        if(ts == "Total") {
            ggsave(paste(outfile5b,title,".jpg",sep=""),p,height=5,width=7.5)
        }
        
        
        # ###########################################
        # # Chart for stock pies by fishery
        # ###########################################
        # n <- ceiling(length(unique(dat_ts$FisheryName)) / 2)
        # p <- ggplot(data=dat_ts, aes(BP,CWT_prop,fill=FisheryName))
        # p <- p + geom_bar(width=0.9, color="black", alpha=1, stat="identity")
        # p <- p + coord_polar(theta="y")
        # p <- p + theme(strip.text.x=element_text(size=15)) +
        #     ggtitle(title) +
        #     theme(plot.title=element_text(size=15, face="bold")) +
        #     theme(axis.title.x=element_text(size=12), 
        #           axis.text.x=element_text(size=10,color="black", hjust=1,
        #                                    vjust=0.25)) +
        #     theme(axis.title.y=element_text(size=12, vjust=1.3),
        #           axis.text.y=element_text(angle=0, size=10,color="black")) + #resize tick labels, move axis lablel (vjust)
        #     ylab("") + xlab("Base Period") +
        #     theme(legend.title=element_text(size=10, face="bold")) +
        #     theme(legend.text=element_text(size=10)) +
        #     guides(fill=guide_legend(nrow=n)) +
        #     #             theme(panel.grid.major.x=element_blank(), 
        #     #                   panel.grid.minor.x=element_blank()) + #removes the background grid
        #     #             theme(panel.grid.major.y=element_blank(), 
        #     #                   panel.grid.minor.y=element_blank()) + #removes the background grid
        #     #             theme(panel.background=element_blank()) + #removes the gray filled back
        #     scale_x_discrete(expand=c(0.025,0))+ #this gets labels closer to axis, gets rid of gap
        #     theme(panel.background = element_rect(colour="black",size=1)) + #this adds a border
        #     theme(plot.margin=unit(c(2,-2,-4,2),"mm"))
        # 
        # # save
        # if(ts %in% c(1:3)) {
        #     ggsave(paste(outfile6a,title,".jpg",sep=""),p,height=7.5,width=7.5)
        # }
        # if(ts == "Total") {
        #     ggsave(paste(outfile6b,title,".jpg",sep=""),p,height=7.5,width=7.5)
        # } 
    }
}

nd <- Sys.time()
tm <- nd - strt
tm


