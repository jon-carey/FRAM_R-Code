# Clear workspace
rm(list=ls(all=TRUE))

# set start time for purposes of timing code
strt <- Sys.time()

# Load required libraries
library(doBy)
library(ggplot2)

# Source dir
Dir <- "C:\\data\\FRAM\\Base Period\\Validation\\Round 5\\Working\\ProfileFigures\\"
#######################################################################################
# Set the paths
paths = list(paste(Dir,"2007-13_summary_Avg.csv",sep=""),
             paste(Dir,"FisheryID.csv",sep=""),
             paste(Dir,"StockID.csv",sep=""),
             paste(Dir,"Figs\\Encounter Facet\\",sep=""),
             paste(Dir,"Figs\\BP Facet\\",sep=""))
#######################################################################################

# Set the output file path
outfile1 = paths[[4]]
outfile2 = paths[[5]]

# Read infiles
dat <- read.csv(paths[[1]])
FishID <- read.csv(paths[[2]])
StkID <- read.csv(paths[[3]])

# Sum over all ages
dat <- summaryBy(LegEnc+SubEnc~Source+Stk+Fish+TS, data = dat, FUN = sum)

# Remove records with no legal or sublegal encounters
dat$exclID <- ifelse(dat$LegEnc + dat$SubEnc > 0, 1, 0)
dat <- dat[dat$exclID == 1, c(1:6)]

# Remove TS 4
dat <- dat[dat$TS < 4, ]

# merge with fishery and stock names
colnames(dat)[c(1:3,5:6)] <- c("BP", "StockID", "FisheryID", "LegEnc", "SubEnc")
dat <- merge(dat, FishID[ ,c(4,6)])
dat <- merge(dat, StkID)

# re-order columns
dat <- dat[ ,c(3,1,8:9,2,7,4:6)]

# # Add factor levels to StockName so x-axis order is correct
# StkID <- StkID[order(StkID$StkOrder), ]
# dat$StockName <- factor(dat$StockName,levels=c(StkID$StockName),ordered=TRUE)


i=1
for(i in 1:length(unique(dat$FisheryID))) {
    fishID <- unique(dat$FisheryID)[i]
    if(FishID[FishID$FisheryID == fishID, 7] == 1) {
        fishery <- FishID[FishID$FisheryID == fishID, 6]
        dat_fish <- dat[dat$FisheryID == fishID, ]
        
        j=1
        for(j in 1:length(unique(dat_fish$TS))) {
            ts <- unique(dat_fish$TS)[j]
            if(ts == 1) {
                time <- "Oct-Apr"
            }
            if(ts == 2) {
                time <- "May-Jun"
            }
            if(ts == 3) {
                time <- "Jul-Sep"
            }
            dat_fish_ts <- dat_fish[dat_fish$TS == ts, ]
            
            Sums <- summaryBy(LegEnc+SubEnc~BP, data=dat_fish_ts, FUN=sum)
            
            # Calculate new stock comp proportions
            k=1
            for(k in 1:dim(dat_fish_ts)[1]) {
                if(dat_fish_ts$BP[k] == "NewBP") {
                    dat_fish_ts$LegEnc_prop[k] <- dat_fish_ts$LegEnc[k] / Sums[1,2]
                    dat_fish_ts$SubEnc_prop[k] <- dat_fish_ts$SubEnc[k] / Sums[1,3]
                }
                if(dat_fish_ts$BP[k] == "OldBP") {
                    dat_fish_ts$LegEnc_prop[k] <- dat_fish_ts$LegEnc[k] / Sums[2,2]
                    dat_fish_ts$SubEnc_prop[k] <- dat_fish_ts$SubEnc[k] / Sums[2,3]
                }
            }
            dat_fish_ts[is.na(dat_fish_ts)] <- 0
            
            # Code for removing stocks that are inconsequential
            # (i.e. OldBP + NewBP sum to < 1% of catch
            removal_dat <- summaryBy(LegEnc_prop+SubEnc_prop~StockID, data = dat_fish_ts,
                                     FUN = sum)
            removal_dat$exclID <- rep(0,dim(removal_dat)[1])
            
            l=1
            for(l in 1:dim(removal_dat)[1]) {
                if(removal_dat$LegEnc_prop.sum[l] < 0.01 & removal_dat$SubEnc_prop.sum[l] < 0.01) {
                    removal_dat$exclID[l] <- 1
                }
            }
            
            keep_stks <- unique(removal_dat[removal_dat$exclID == 0, 1])
            
            dat_fish_ts <- dat_fish_ts[dat_fish_ts$StockID %in% keep_stks, ]
            
            ###################
            # For Proportions #
            ###################
            
            # reshape into long
            prop_dat <- dat_fish_ts[ ,c(1:7,10:11)]
            figdat <- reshape(prop_dat, direction = "long", varying = list(names(prop_dat)[8:9]),
                              v.names="Prop", idvar=c(names(prop_dat)[1:7]), timevar = "Encounter",
                              times = c("LegEnc","SubEnc"))
            
            title <- paste(fishID, " - ", fishery, "; ", time, sep = "")
            ylabel <- "Encounters"
            
            p <- ggplot(data=figdat, aes(StockName,Prop,fill=BP))
            p <- p + geom_bar(width=0.5, color="black", alpha=1, position="dodge",
                              stat="identity")
            
            p <- p + facet_grid(Encounter ~ .) +
                theme(strip.text.x=element_text(size=15)) +
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
                ggsave(paste(outfile1,title,".jpg",sep=""),p,height=5,width=7.5)
            }
            # if(ts == "Total") {
            #     ggsave(paste(outfile2b,title,".jpg",sep=""),p,height=5,width=7.5)
            # }
            
            q <- ggplot(data=figdat, aes(StockName,Prop,fill=Encounter))
            q <- q + geom_bar(width=0.5, color="black", alpha=1, position="dodge",
                              stat="identity")
            
            q <- q + facet_grid(BP ~ .) +
                theme(strip.text.x=element_text(size=15)) +
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
                ggsave(paste(outfile2,title,".jpg",sep=""),q,height=5,width=7.5)
            }
        }
    }
}

nd <- Sys.time()
tm <- nd - strt
tm
