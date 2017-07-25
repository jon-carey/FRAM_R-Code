################################################################################
# PROGRAM TO PULL ANNUAL STOCK AND FISHERY DATA FROM ASSICIATED VALIDATION RUN
# COMPILATION FILES THeN LOOP THROUGH A DIRECTORY OF YEARLY TAMM VALIDATION 
# FILES AND LOAD THE APPROPRIATE STOCK AND FISHERY INPUT VALUES INTO THE INPUT
# PAGE
#
#JC, DD; JULY 2016
################################################################################

# Clear workspace
rm(list=ls(all=TRUE))

# set start time for purposes of timing code
strt <- Sys.time()

library(RDCOMClient)
library(XLConnect)
library(readxl)

# Set paths
Dir <- "C:\\data\\FRAM\\Base Period\\Validation\\ForSecondRound\\Valid2016 - OldBP\\"
paths = list(paste(Dir, "Valid2016_TAMM_StockData_9.21.16.xlsx", sep=""),
             # paste(Dir, "Valid2016_TAMMFisheries_Final.xlsx", sep=""),
             paste(Dir, "TAMMTest\\", sep=""))

infile = paths[[1]]
# infile2 = paths[[2]]
TAMM1 = paths[[2]]


#####################################################################
# POPULATE TAMM STOCK & FISHERY DATA FOR ALL YEARS FROM MASTER FILE #
#####################################################################

StockData <- as.data.frame(array(NA, c(237,24)))
colnames(StockData) <- c("ID", seq(from=1992,to=2014,by=1))

# FishData <- as.data.frame(array(NA, c(144, 24)))
# colnames(FishData) <- c("ID", seq(from=1992,to=2014,by=1))

#Stock DF population
i=1992
for(i in 1992:2014) {
    #Does some blackbox wizardry
    xlApp <- COMCreate("Excel.Application")
    #xlApp[["Visible"]] <- TRUE
    #Gets our workbook
    wb    <- xlApp[["Workbooks"]]$Open(infile)
    #Gets our FRAMsheet
    FRAMsheet <- wb$Worksheets("StockInput")
    
    # Updates year cell to year i
    Yearcell <- FRAMsheet$Cells(1,3)
    Yearcell[["Value"]] <- i
    
    wb$Save()
    
    xlApp$Quit()
    
    # Read in TAMM stock data for year i
    iData <- read_excel(infile, "R_In")
    
    # Add TAMM stock data for year i into main data table, 'StockData'
    if(i==1992) {
        StockData$ID <- iData[ ,1]
        StockData[ ,i-1990] <- iData[ ,2]
    }
    if(!(i==1992)) {
        StockData[ ,i-1990] <- iData[ ,2]
    }
}

#Fishery DF population
for(i in 1992:2014) {
  #Does some blackbox wizardry
  xlApp <- COMCreate("Excel.Application")
  # xlApp[["Visible"]] <- TRUE
  #Gets our workbook
  wb    <- xlApp[["Workbooks"]]$Open(infile2)
  #Gets our FRAMsheet
  FRAMsheet <- wb$Worksheets("Template")
  
  # Updates year cell to year i
  Yearcell <- FRAMsheet$Cells(1,3)
  Yearcell[["Value"]] <- i
  
  wb$Save()
  
  xlApp$Quit()
  
  # Read in TAMM fishery data for year i
  #Necessary to set data types or else R will change strings to NAs
  data_types<-c("text","text")
  Ddata <- read_excel(infile2, "R_Input", col_types = data_types)
  
  # Add TAMM fishery data for year i into main data table, 'FishData'
  if(i==1992) {
    FishData$ID <- Ddata[ ,1]
    FishData[ ,i-1990] <- Ddata[ ,2]
  }
  if(!(i==1992)) {
    FishData[ ,i-1990] <- Ddata[ ,2]
  }
}



#######################################################
# IMPORT YEARLY STOCK DATA INTO INDIVIDUAL TAMM FILES #
#######################################################

##!!Need to add functionality here to loop through all TAMM files!!##

#DD: keep in mind that this is currently running 1992 - 2014 only. Update
#When we get stock data from 1988 - 1991
i=1992
for(i in 1992:2014) {
    # Set TAMM file path
    TAMM <- paste(TAMM1, "Valid2016_", i, ".xlsm",sep="")
    
    # Select appropriate data
    iData <- StockData[ ,c(1,i-1990)]
    
    #Change this later, it starts the index at year 1992 (index 6 in my df)
    Ddata <- FishData[ ,c(1,i-1990)]
    
    #Does some blackbox wizardry
    xlApp <- COMCreate("Excel.Application")
    #xlApp[["Visible"]] <- TRUE
    
    #Gets TAMM workbook
    wb    <- xlApp[["Workbooks"]]$Open(TAMM)
    
    #Gets TAMM Input Page
    Input_Page <- wb$Worksheets("Input Page")
    
    # Update cells with stock data for year i
    Sk1 <- Input_Page$Cells(14,11); Sk1[["Value"]] <- iData[1,2]
    Sk2 <- Input_Page$Cells(14,12); Sk2[["Value"]] <- iData[2,2]
    Sk3 <- Input_Page$Cells(14,13); Sk3[["Value"]] <- iData[3,2]
    Sk4 <- Input_Page$Cells(14,14); Sk4[["Value"]] <- iData[4,2]
    Sk5 <- Input_Page$Cells(15,11); Sk5[["Value"]] <- iData[5,2]
    Sk6 <- Input_Page$Cells(15,12); Sk6[["Value"]] <- iData[6,2]
    Sk7 <- Input_Page$Cells(15,13); Sk7[["Value"]] <- iData[7,2]
    Sk8 <- Input_Page$Cells(15,14); Sk8[["Value"]] <- iData[8,2]
    Sk9 <- Input_Page$Cells(16,11); Sk9[["Value"]] <- iData[9,2]
    Sk10 <- Input_Page$Cells(16,12); Sk10[["Value"]] <- iData[10,2]
    Sk11 <- Input_Page$Cells(16,13); Sk11[["Value"]] <- iData[11,2]
    Sk12 <- Input_Page$Cells(16,14); Sk12[["Value"]] <- iData[12,2]
    Sk13 <- Input_Page$Cells(18,2); Sk13[["Value"]] <- iData[13,2]
    Sk14 <- Input_Page$Cells(20,2); Sk14[["Value"]] <- iData[14,2]
    Sk15 <- Input_Page$Cells(21,2); Sk15[["Value"]] <- iData[15,2]
    Sk16 <- Input_Page$Cells(22,2); Sk16[["Value"]] <- iData[16,2]
    Sk17 <- Input_Page$Cells(23,2); Sk17[["Value"]] <- iData[17,2]
    Sk18 <- Input_Page$Cells(24,2); Sk18[["Value"]] <- iData[18,2]
    Sk19 <- Input_Page$Cells(25,2); Sk19[["Value"]] <- iData[19,2]
    Sk20 <- Input_Page$Cells(20,5); Sk20[["Value"]] <- iData[20,2]
    StSn1 <- Input_Page$Cells(30,11); StSn1[["Value"]] <- iData[21,2]
    StSn2 <- Input_Page$Cells(30,12); StSn2[["Value"]] <- iData[22,2]
    StSn3 <- Input_Page$Cells(30,13); StSn3[["Value"]] <- iData[23,2]
    StSn4 <- Input_Page$Cells(30,14); StSn4[["Value"]] <- iData[24,2]
    StSn5 <- Input_Page$Cells(31,11); StSn5[["Value"]] <- iData[25,2]
    StSn6 <- Input_Page$Cells(31,12); StSn6[["Value"]] <- iData[26,2]
    StSn7 <- Input_Page$Cells(31,13); StSn7[["Value"]] <- iData[27,2]
    StSn8 <- Input_Page$Cells(31,14); StSn8[["Value"]] <- iData[28,2]
    StSn9 <- Input_Page$Cells(32,11); StSn9[["Value"]] <- iData[29,2]
    StSn10 <- Input_Page$Cells(32,12); StSn10[["Value"]] <- iData[30,2]
    StSn11 <- Input_Page$Cells(32,13); StSn11[["Value"]] <- iData[31,2]
    StSn12 <- Input_Page$Cells(32,14); StSn12[["Value"]] <- iData[32,2]
    StSn13 <- Input_Page$Cells(33,11); StSn13[["Value"]] <- iData[33,2]
    StSn14 <- Input_Page$Cells(33,12); StSn14[["Value"]] <- iData[34,2]
    StSn15 <- Input_Page$Cells(33,13); StSn15[["Value"]] <- iData[35,2]
    StSn16 <- Input_Page$Cells(33,14); StSn16[["Value"]] <- iData[36,2]
    StSn17 <- Input_Page$Cells(35,2); StSn17[["Value"]] <- iData[37,2]
    StSn18 <- Input_Page$Cells(36,2); StSn18[["Value"]] <- iData[38,2]
    HC1 <- Input_Page$Cells(41,11); HC1[["Value"]] <- iData[39,2]
    HC2 <- Input_Page$Cells(41,12); HC2[["Value"]] <- iData[40,2]
    HC3 <- Input_Page$Cells(41,13); HC3[["Value"]] <- iData[41,2]
    HC4 <- Input_Page$Cells(41,14); HC4[["Value"]] <- iData[42,2]
    HC5 <- Input_Page$Cells(42,11); HC5[["Value"]] <- iData[43,2]
    HC6 <- Input_Page$Cells(42,12); HC6[["Value"]] <- iData[44,2]
    HC7 <- Input_Page$Cells(42,13); HC7[["Value"]] <- iData[45,2]
    HC8 <- Input_Page$Cells(42,14); HC8[["Value"]] <- iData[46,2]
    HC9 <- Input_Page$Cells(43,11); HC9[["Value"]] <- iData[47,2]
    HC10 <- Input_Page$Cells(43,12); HC10[["Value"]] <- iData[48,2]
    HC11 <- Input_Page$Cells(43,13); HC11[["Value"]] <- iData[49,2]
    HC12 <- Input_Page$Cells(43,14); HC12[["Value"]] <- iData[50,2]
    HC13 <- Input_Page$Cells(44,11); HC13[["Value"]] <- iData[51,2]
    HC14 <- Input_Page$Cells(44,12); HC14[["Value"]] <- iData[52,2]
    HC15 <- Input_Page$Cells(44,13); HC15[["Value"]] <- iData[53,2]
    HC16 <- Input_Page$Cells(44,14); HC16[["Value"]] <- iData[54,2]
    HC17 <- Input_Page$Cells(45,11); HC17[["Value"]] <- iData[55,2]
    HC18 <- Input_Page$Cells(45,12); HC18[["Value"]] <- iData[56,2]
    HC19 <- Input_Page$Cells(45,13); HC19[["Value"]] <- iData[57,2]
    HC20 <- Input_Page$Cells(45,14); HC20[["Value"]] <- iData[58,2]
    HC21 <- Input_Page$Cells(46,11); HC21[["Value"]] <- iData[59,2]
    HC22 <- Input_Page$Cells(46,12); HC22[["Value"]] <- iData[60,2]
    HC23 <- Input_Page$Cells(46,13); HC23[["Value"]] <- iData[61,2]
    HC24 <- Input_Page$Cells(46,14); HC24[["Value"]] <- iData[62,2]
    HC25 <- Input_Page$Cells(47,11); HC25[["Value"]] <- iData[63,2]
    HC26 <- Input_Page$Cells(47,12); HC26[["Value"]] <- iData[64,2]
    HC27 <- Input_Page$Cells(47,13); HC27[["Value"]] <- iData[65,2]
    HC28 <- Input_Page$Cells(47,14); HC28[["Value"]] <- iData[66,2]
    HC29 <- Input_Page$Cells(46,16); HC29[["Value"]] <- iData[237,2]
    SPS1 <- Input_Page$Cells(53,11); SPS1[["Value"]] <- iData[67,2]
    SPS2 <- Input_Page$Cells(53,12); SPS2[["Value"]] <- iData[68,2]
    SPS3 <- Input_Page$Cells(53,13); SPS3[["Value"]] <- iData[69,2]
    SPS4 <- Input_Page$Cells(53,14); SPS4[["Value"]] <- iData[70,2]
    SPS5 <- Input_Page$Cells(54,11); SPS5[["Value"]] <- iData[71,2]
    SPS6 <- Input_Page$Cells(54,12); SPS6[["Value"]] <- iData[72,2]
    SPS7 <- Input_Page$Cells(54,13); SPS7[["Value"]] <- iData[73,2]
    SPS8 <- Input_Page$Cells(54,14); SPS8[["Value"]] <- iData[74,2]
    SPS9 <- Input_Page$Cells(55,11); SPS9[["Value"]] <- iData[75,2]
    SPS10 <- Input_Page$Cells(55,12); SPS10[["Value"]] <- iData[76,2]
    SPS11 <- Input_Page$Cells(55,13); SPS11[["Value"]] <- iData[77,2]
    SPS12 <- Input_Page$Cells(55,14); SPS12[["Value"]] <- iData[78,2]
    SPS13 <- Input_Page$Cells(56,11); SPS13[["Value"]] <- iData[79,2]
    SPS14 <- Input_Page$Cells(56,12); SPS14[["Value"]] <- iData[80,2]
    SPS15 <- Input_Page$Cells(56,13); SPS15[["Value"]] <- iData[81,2]
    SPS16 <- Input_Page$Cells(56,14); SPS16[["Value"]] <- iData[82,2]
    SPS17 <- Input_Page$Cells(57,11); SPS17[["Value"]] <- iData[83,2]
    SPS18 <- Input_Page$Cells(57,12); SPS18[["Value"]] <- iData[84,2]
    SPS19 <- Input_Page$Cells(57,13); SPS19[["Value"]] <- iData[85,2]
    SPS20 <- Input_Page$Cells(57,14); SPS20[["Value"]] <- iData[86,2]
    SPS21 <- Input_Page$Cells(58,11); SPS21[["Value"]] <- iData[87,2]
    SPS22 <- Input_Page$Cells(58,12); SPS22[["Value"]] <- iData[88,2]
    SPS23 <- Input_Page$Cells(58,13); SPS23[["Value"]] <- iData[89,2]
    SPS24 <- Input_Page$Cells(58,14); SPS24[["Value"]] <- iData[90,2]
    SPS25 <- Input_Page$Cells(59,11); SPS25[["Value"]] <- iData[91,2]
    SPS26 <- Input_Page$Cells(59,12); SPS26[["Value"]] <- iData[92,2]
    SPS27 <- Input_Page$Cells(59,13); SPS27[["Value"]] <- iData[93,2]
    SPS28 <- Input_Page$Cells(59,14); SPS28[["Value"]] <- iData[94,2]
    SPS29 <- Input_Page$Cells(60,11); SPS29[["Value"]] <- iData[95,2]
    SPS30 <- Input_Page$Cells(60,12); SPS30[["Value"]] <- iData[96,2]
    SPS31 <- Input_Page$Cells(60,13); SPS31[["Value"]] <- iData[97,2]
    SPS32 <- Input_Page$Cells(60,14); SPS32[["Value"]] <- iData[98,2]
    SPS33 <- Input_Page$Cells(61,11); SPS33[["Value"]] <- iData[99,2]
    SPS34 <- Input_Page$Cells(61,12); SPS34[["Value"]] <- iData[100,2]
    SPS35 <- Input_Page$Cells(61,13); SPS35[["Value"]] <- iData[101,2]
    SPS36 <- Input_Page$Cells(61,14); SPS36[["Value"]] <- iData[102,2]
    SPS37 <- Input_Page$Cells(62,11); SPS37[["Value"]] <- iData[103,2]
    SPS38 <- Input_Page$Cells(62,12); SPS38[["Value"]] <- iData[104,2]
    SPS39 <- Input_Page$Cells(62,13); SPS39[["Value"]] <- iData[105,2]
    SPS40 <- Input_Page$Cells(62,14); SPS40[["Value"]] <- iData[106,2]
    SPS41 <- Input_Page$Cells(63,11); SPS41[["Value"]] <- iData[107,2]
    SPS42 <- Input_Page$Cells(63,12); SPS42[["Value"]] <- iData[108,2]
    SPS43 <- Input_Page$Cells(63,13); SPS43[["Value"]] <- iData[109,2]
    SPS44 <- Input_Page$Cells(63,14); SPS44[["Value"]] <- iData[110,2]
    SPS45 <- Input_Page$Cells(64,11); SPS45[["Value"]] <- iData[111,2]
    SPS46 <- Input_Page$Cells(64,12); SPS46[["Value"]] <- iData[112,2]
    SPS47 <- Input_Page$Cells(64,13); SPS47[["Value"]] <- iData[113,2]
    SPS48 <- Input_Page$Cells(64,14); SPS48[["Value"]] <- iData[114,2]
    SPS49 <- Input_Page$Cells(65,11); SPS49[["Value"]] <- iData[115,2]
    SPS50 <- Input_Page$Cells(65,12); SPS50[["Value"]] <- iData[116,2]
    SPS51 <- Input_Page$Cells(65,13); SPS51[["Value"]] <- iData[117,2]
    SPS52 <- Input_Page$Cells(65,14); SPS52[["Value"]] <- iData[118,2]
    SPS53 <- Input_Page$Cells(66,11); SPS53[["Value"]] <- iData[119,2]
    SPS54 <- Input_Page$Cells(66,12); SPS54[["Value"]] <- iData[120,2]
    SPS55 <- Input_Page$Cells(66,13); SPS55[["Value"]] <- iData[121,2]
    SPS56 <- Input_Page$Cells(66,14); SPS56[["Value"]] <- iData[122,2]
    SPS57 <- Input_Page$Cells(67,11); SPS57[["Value"]] <- iData[123,2]
    SPS58 <- Input_Page$Cells(67,12); SPS58[["Value"]] <- iData[124,2]
    SPS59 <- Input_Page$Cells(67,13); SPS59[["Value"]] <- iData[125,2]
    SPS60 <- Input_Page$Cells(67,14); SPS60[["Value"]] <- iData[126,2]
    SPS61 <- Input_Page$Cells(68,11); SPS61[["Value"]] <- iData[127,2]
    SPS62 <- Input_Page$Cells(68,12); SPS62[["Value"]] <- iData[128,2]
    SPS63 <- Input_Page$Cells(68,13); SPS63[["Value"]] <- iData[129,2]
    SPS64 <- Input_Page$Cells(68,14); SPS64[["Value"]] <- iData[130,2]
    SPS65 <- Input_Page$Cells(69,11); SPS65[["Value"]] <- iData[131,2]
    SPS66 <- Input_Page$Cells(69,12); SPS66[["Value"]] <- iData[132,2]
    SPS67 <- Input_Page$Cells(69,13); SPS67[["Value"]] <- iData[133,2]
    SPS68 <- Input_Page$Cells(69,14); SPS68[["Value"]] <- iData[134,2]
    SPS69 <- Input_Page$Cells(70,11); SPS69[["Value"]] <- iData[135,2]
    SPS70 <- Input_Page$Cells(70,12); SPS70[["Value"]] <- iData[136,2]
    SPS71 <- Input_Page$Cells(70,13); SPS71[["Value"]] <- iData[137,2]
    SPS72 <- Input_Page$Cells(70,14); SPS72[["Value"]] <- iData[138,2]
    SPS73 <- Input_Page$Cells(71,11); SPS73[["Value"]] <- iData[139,2]
    SPS74 <- Input_Page$Cells(71,12); SPS74[["Value"]] <- iData[140,2]
    SPS75 <- Input_Page$Cells(71,13); SPS75[["Value"]] <- iData[141,2]
    SPS76 <- Input_Page$Cells(71,14); SPS76[["Value"]] <- iData[142,2]
    SPS77 <- Input_Page$Cells(72,11); SPS77[["Value"]] <- iData[143,2]
    SPS78 <- Input_Page$Cells(72,12); SPS78[["Value"]] <- iData[144,2]
    SPS79 <- Input_Page$Cells(72,13); SPS79[["Value"]] <- iData[145,2]
    SPS80 <- Input_Page$Cells(72,14); SPS80[["Value"]] <- iData[146,2]
    SPS81 <- Input_Page$Cells(54,15); SPS81[["Value"]] <- iData[147,2]
    SPS82 <- Input_Page$Cells(53,17); SPS82[["Value"]] <- iData[148,2]
    SPS83 <- Input_Page$Cells(53,18); SPS83[["Value"]] <- iData[149,2]
    SPS84 <- Input_Page$Cells(54,17); SPS84[["Value"]] <- iData[150,2]
    SPS85 <- Input_Page$Cells(54,18); SPS85[["Value"]] <- iData[151,2]
    SPS86 <- Input_Page$Cells(55,17); SPS86[["Value"]] <- iData[152,2]
    SPS87 <- Input_Page$Cells(55,18); SPS87[["Value"]] <- iData[153,2]
    SPS88 <- Input_Page$Cells(56,17); SPS88[["Value"]] <- iData[154,2]
    SPS89 <- Input_Page$Cells(56,18); SPS89[["Value"]] <- iData[155,2]
    SPS90 <- Input_Page$Cells(57,17); SPS90[["Value"]] <- iData[156,2]
    SPS91 <- Input_Page$Cells(57,18); SPS91[["Value"]] <- iData[157,2]
    SPS92 <- Input_Page$Cells(58,17); SPS92[["Value"]] <- iData[158,2]
    SPS93 <- Input_Page$Cells(58,18); SPS93[["Value"]] <- iData[159,2]
    SPS94 <- Input_Page$Cells(59,17); SPS94[["Value"]] <- iData[160,2]
    SPS95 <- Input_Page$Cells(59,18); SPS95[["Value"]] <- iData[161,2]
    SPS96 <- Input_Page$Cells(60,17); SPS96[["Value"]] <- iData[162,2]
    SPS97 <- Input_Page$Cells(60,18); SPS97[["Value"]] <- iData[163,2]
    SPS98 <- Input_Page$Cells(61,17); SPS98[["Value"]] <- iData[164,2]
    SPS99 <- Input_Page$Cells(61,18); SPS99[["Value"]] <- iData[165,2]
    SPS100 <- Input_Page$Cells(62,17); SPS100[["Value"]] <- iData[166,2]
    SPS101 <- Input_Page$Cells(62,18); SPS101[["Value"]] <- iData[167,2]
    SPS102 <- Input_Page$Cells(63,17); SPS102[["Value"]] <- iData[168,2]
    SPS103 <- Input_Page$Cells(63,18); SPS103[["Value"]] <- iData[169,2]
    SPS104 <- Input_Page$Cells(64,17); SPS104[["Value"]] <- iData[170,2]
    SPS105 <- Input_Page$Cells(64,18); SPS105[["Value"]] <- iData[171,2]
    SPS106 <- Input_Page$Cells(65,17); SPS106[["Value"]] <- iData[172,2]
    SPS107 <- Input_Page$Cells(65,18); SPS107[["Value"]] <- iData[173,2]
    SPS108 <- Input_Page$Cells(66,17); SPS108[["Value"]] <- iData[174,2]
    SPS109 <- Input_Page$Cells(66,18); SPS109[["Value"]] <- iData[175,2]
    SPS110 <- Input_Page$Cells(67,17); SPS110[["Value"]] <- iData[176,2]
    SPS111 <- Input_Page$Cells(67,18); SPS111[["Value"]] <- iData[177,2]
    SPS112 <- Input_Page$Cells(68,17); SPS112[["Value"]] <- iData[178,2]
    SPS113 <- Input_Page$Cells(68,18); SPS113[["Value"]] <- iData[179,2]
    SPS114 <- Input_Page$Cells(69,17); SPS114[["Value"]] <- iData[180,2]
    SPS115 <- Input_Page$Cells(69,18); SPS115[["Value"]] <- iData[181,2]
    SPS116 <- Input_Page$Cells(70,17); SPS116[["Value"]] <- iData[182,2]
    SPS117 <- Input_Page$Cells(70,18); SPS117[["Value"]] <- iData[183,2]
    SPS118 <- Input_Page$Cells(71,17); SPS118[["Value"]] <- iData[184,2]
    SPS119 <- Input_Page$Cells(71,18); SPS119[["Value"]] <- iData[185,2]
    SPS120 <- Input_Page$Cells(72,17); SPS120[["Value"]] <- iData[186,2]
    SPS121 <- Input_Page$Cells(72,18); SPS121[["Value"]] <- iData[187,2]
    SPS122 <- Input_Page$Cells(54,20); SPS122[["Value"]] <- iData[188,2]
    SPS123 <- Input_Page$Cells(57,20); SPS123[["Value"]] <- iData[189,2]
    SPS124 <- Input_Page$Cells(57,22); SPS124[["Value"]] <- iData[190,2]
    SPS125 <- Input_Page$Cells(58,20); SPS125[["Value"]] <- iData[191,2]
    SPS126 <- Input_Page$Cells(61,20); SPS126[["Value"]] <- iData[192,2]
    SPS127 <- Input_Page$Cells(65,20); SPS127[["Value"]] <- iData[193,2]
    NS1 <- Input_Page$Cells(77,11); NS1[["Value"]] <- iData[194,2]
    NS2 <- Input_Page$Cells(77,12); NS2[["Value"]] <- iData[195,2]
    NS3 <- Input_Page$Cells(77,13); NS3[["Value"]] <- iData[196,2]
    NS4 <- Input_Page$Cells(77,14); NS4[["Value"]] <- iData[197,2]
    NS5 <- Input_Page$Cells(78,11); NS5[["Value"]] <- iData[198,2]
    NS6 <- Input_Page$Cells(78,12); NS6[["Value"]] <- iData[199,2]
    NS7 <- Input_Page$Cells(78,13); NS7[["Value"]] <- iData[200,2]
    NS8 <- Input_Page$Cells(78,14); NS8[["Value"]] <- iData[201,2]
    NS9 <- Input_Page$Cells(79,11); NS9[["Value"]] <- iData[202,2]
    NS10 <- Input_Page$Cells(79,12); NS10[["Value"]] <- iData[203,2]
    NS11 <- Input_Page$Cells(79,13); NS11[["Value"]] <- iData[204,2]
    NS12 <- Input_Page$Cells(79,14); NS12[["Value"]] <- iData[205,2]
    NS13 <- Input_Page$Cells(80,11); NS13[["Value"]] <- iData[206,2]
    NS14 <- Input_Page$Cells(80,12); NS14[["Value"]] <- iData[207,2]
    NS15 <- Input_Page$Cells(80,13); NS15[["Value"]] <- iData[208,2]
    NS16 <- Input_Page$Cells(80,14); NS16[["Value"]] <- iData[209,2]
    NS17 <- Input_Page$Cells(81,11); NS17[["Value"]] <- iData[210,2]
    NS18 <- Input_Page$Cells(81,12); NS18[["Value"]] <- iData[211,2]
    NS19 <- Input_Page$Cells(81,13); NS19[["Value"]] <- iData[212,2]
    NS20 <- Input_Page$Cells(81,14); NS20[["Value"]] <- iData[213,2]
    NS21 <- Input_Page$Cells(82,11); NS21[["Value"]] <- iData[214,2]
    NS22 <- Input_Page$Cells(82,12); NS22[["Value"]] <- iData[215,2]
    NS23 <- Input_Page$Cells(82,13); NS23[["Value"]] <- iData[216,2]
    NS24 <- Input_Page$Cells(82,14); NS24[["Value"]] <- iData[217,2]
    NS25 <- Input_Page$Cells(83,11); NS25[["Value"]] <- iData[218,2]
    NS26 <- Input_Page$Cells(83,12); NS26[["Value"]] <- iData[219,2]
    NS27 <- Input_Page$Cells(83,13); NS27[["Value"]] <- iData[220,2]
    NS28 <- Input_Page$Cells(83,14); NS28[["Value"]] <- iData[221,2]
    NS29 <- Input_Page$Cells(77,16); NS29[["Value"]] <- iData[222,2]
    JDF1 <- Input_Page$Cells(89,11); JDF1[["Value"]] <- iData[223,2]
    JDF2 <- Input_Page$Cells(89,12); JDF2[["Value"]] <- iData[224,2]
    JDF3 <- Input_Page$Cells(89,13); JDF3[["Value"]] <- iData[225,2]
    JDF4 <- Input_Page$Cells(89,14); JDF4[["Value"]] <- iData[226,2]
    JDF5 <- Input_Page$Cells(90,11); JDF5[["Value"]] <- iData[227,2]
    JDF6 <- Input_Page$Cells(90,12); JDF6[["Value"]] <- iData[228,2]
    JDF7 <- Input_Page$Cells(90,13); JDF7[["Value"]] <- iData[229,2]
    JDF8 <- Input_Page$Cells(90,14); JDF8[["Value"]] <- iData[230,2]
    JDF9 <- Input_Page$Cells(91,11); JDF9[["Value"]] <- iData[231,2]
    JDF10 <- Input_Page$Cells(91,12); JDF10[["Value"]] <- iData[232,2]
    JDF11 <- Input_Page$Cells(91,13); JDF11[["Value"]] <- iData[233,2]
    JDF12 <- Input_Page$Cells(91,14); JDF12[["Value"]] <- iData[234,2]
    JDF13 <- Input_Page$Cells(93,4); JDF13[["Value"]] <- iData[235,2]
    JDF14 <- Input_Page$Cells(94,4); JDF14[["Value"]] <- iData[236,2]
    
    
    
    #Adds fishery inputs
    SKNetSF<- Input_Page$Cells(106, 2);SKNetSF[["Value"]] <- Ddata[1,2]
    SKNetSFProp1<- Input_Page$Cells(106, 5);SKNetSFProp1[["Value"]] <- Ddata[2,2]
    SKNetSFProp2<- Input_Page$Cells(106, 6);SKNetSFProp2[["Value"]] <- Ddata[3,2]
    SKNetFWChin<- Input_Page$Cells(109, 2);SKNetFWChin[["Value"]] <- Ddata[4,2]
    SKNetFWChinSpr<- Input_Page$Cells(109, 4);SKNetFWChinSpr[["Value"]] <- Ddata[5,2]
    SKNetRivTest<- Input_Page$Cells(117, 2);SKNetRivTest[["Value"]] <- Ddata[6,2]
    SKNetRivTestPPNSpr<- Input_Page$Cells(117, 4);SKNetRivTestPPNSpr[["Value"]] <- Ddata[7,2]
    SKNetChum<- Input_Page$Cells(120, 2);SKNetChum[["Value"]] <- Ddata[8,2]
    SKNetChumProp1<- Input_Page$Cells(120, 5);SKNetChumProp1[["Value"]] <- Ddata[9,2]
    SKNetChumProp2<- Input_Page$Cells(120, 6);SKNetChumProp2[["Value"]] <- Ddata[10,2]
    STSNNetChin<- Input_Page$Cells(126, 2);STSNNetChin[["Value"]] <- Ddata[11,2]
    STSNNetChinProp1<- Input_Page$Cells(126, 4);STSNNetChinProp1[["Value"]] <- Ddata[12,2]
    STSNNetChinProp2<- Input_Page$Cells(126, 5);STSNNetChinProp2[["Value"]] <- Ddata[13,2]
    STSNNetChinProp3<- Input_Page$Cells(126, 6);STSNNetChinProp3[["Value"]] <- Ddata[14,2]
    STNetRivChin<- Input_Page$Cells(131, 2);STNetRivChin[["Value"]] <- Ddata[15,2]
    SNNetTest<- Input_Page$Cells(136, 2);SNNetTest[["Value"]] <- Ddata[16,2]
    STSNNet8DTS3Tr<- Input_Page$Cells(139, 2);STSNNet8DTS3Tr[["Value"]] <- Ddata[17,2]
    STSNNet8DTS4Tr<- Input_Page$Cells(140, 2);STSNNet8DTS4Tr[["Value"]] <- Ddata[18,2]
    STSNNet8DTS3NT<- Input_Page$Cells(141, 2);STSNNet8DTS3NT[["Value"]] <- Ddata[19,2]
    STSNNet8DTS4NT<- Input_Page$Cells(143, 2);STSNNet8DTS4NT[["Value"]] <- Ddata[20,2]
    STSNNet8ATS2NT<- Input_Page$Cells(144, 2);STSNNet8ATS2NT[["Value"]] <- Ddata[21,2]
    STSNNet8APink<- Input_Page$Cells(145, 2);STSNNet8APink[["Value"]] <- Ddata[22,2]
    STSNNet8APinkProp1<- Input_Page$Cells(145, 5);STSNNet8APinkProp1[["Value"]] <- Ddata[23,2]
    STSNNet8APinkProp2<- Input_Page$Cells(145, 6);STSNNet8APinkProp2[["Value"]] <- Ddata[24,2]
    HCNetMar<- Input_Page$Cells(153, 2);HCNetMar[["Value"]] <- Ddata[25,2]
    HCNetMarProp1<- Input_Page$Cells(153, 5);HCNetMarProp1[["Value"]] <- Ddata[26,2]
    HCNetMarProp2<- Input_Page$Cells(153, 6);HCNetMarProp2[["Value"]] <- Ddata[27,2]
    HCNetSkokRiv<- Input_Page$Cells(156, 2);HCNetSkokRiv[["Value"]] <- Ddata[28,2]
    HCNetQuilcene<- Input_Page$Cells(159, 2);HCNetQuilcene[["Value"]] <- Ddata[29,2]
    HCNet9A12A<- Input_Page$Cells(161, 2);HCNet9A12A[["Value"]] <- Ddata[30,2]
    HCNet9A12AProp1<- Input_Page$Cells(161, 5);HCNet9A12AProp1[["Value"]] <- Ddata[31,2]
    HCNet9A12AProp2<- Input_Page$Cells(161, 6);HCNet9A12AProp2[["Value"]] <- Ddata[32,2]
    HCNetHoodsport<- Input_Page$Cells(163, 2);HCNetHoodsport[["Value"]] <- Ddata[33,2]
    HCNetMarNT<- Input_Page$Cells(165, 2);HCNetMarNT[["Value"]] <- Ddata[34,2]
    HCNetMarNTProp1<- Input_Page$Cells(165, 5);HCNetMarNTProp1[["Value"]] <- Ddata[35,2]
    HCNetMarNTProp2<- Input_Page$Cells(165, 6);HCNetMarNTProp2[["Value"]] <- Ddata[36,2]
    HCNet9A12ANT<- Input_Page$Cells(168, 2);HCNet9A12ANT[["Value"]] <- Ddata[37,2]
    HCNet9A12ANTProp1<- Input_Page$Cells(168, 5);HCNet9A12ANTProp1[["Value"]] <- Ddata[38,2]
    HCNet9A12ANTProp2<- Input_Page$Cells(168, 6);HCNet9A12ANTProp2[["Value"]] <- Ddata[39,2]
    WhiteRNet<- Input_Page$Cells(178, 2);WhiteRNet[["Value"]] <- Ddata[40,2]
    SPSNet1011NTTS3<- Input_Page$Cells(180, 5);SPSNet1011NTTS3[["Value"]] <- Ddata[41,2]
    SPSNet1011NTTS4<- Input_Page$Cells(180, 6);SPSNet1011NTTS4[["Value"]] <- Ddata[42,2]
    SPSNet1011TrTS3<- Input_Page$Cells(181, 5);SPSNet1011TrTS3[["Value"]] <- Ddata[43,2]
    SPSNet1011TrTS4<- Input_Page$Cells(181, 6);SPSNet1011TrTS4[["Value"]] <- Ddata[44,2]
    SPSNet1011TestTS3<- Input_Page$Cells(182, 5);SPSNet1011TestTS3[["Value"]] <- Ddata[45,2]
    SPSNet1011TestTS4<- Input_Page$Cells(182, 6);SPSNet1011TestTS4[["Value"]] <- Ddata[46,2]
    SPSNet10ANTTS3<- Input_Page$Cells(183, 5);SPSNet10ANTTS3[["Value"]] <- Ddata[47,2]
    SPSNet10ATrTS3<- Input_Page$Cells(184, 5);SPSNet10ATrTS3[["Value"]] <- Ddata[48,2]
    SPSNet10ATrTS4<- Input_Page$Cells(184, 6);SPSNet10ATrTS4[["Value"]] <- Ddata[49,2]
    SPSNet10ATestTS3<- Input_Page$Cells(185, 5);SPSNet10ATestTS3[["Value"]] <- Ddata[50,2]
    SPSNet10ENTTS3<- Input_Page$Cells(186, 5);SPSNet10ENTTS3[["Value"]] <- Ddata[51,2]
    SPSNet10ETrTS3<- Input_Page$Cells(187, 5);SPSNet10ETrTS3[["Value"]] <- Ddata[52,2]
    SPSNet10ETrTS4<- Input_Page$Cells(187, 6);SPSNet10ETrTS4[["Value"]] <- Ddata[53,2]
    SPSNetDeepTS3<- Input_Page$Cells(189, 5);SPSNetDeepTS3[["Value"]] <- Ddata[54,2]
    SPSNetDeepTS4<- Input_Page$Cells(189, 6);SPSNetDeepTS4[["Value"]] <- Ddata[55,2]
    SPSNet13ATS3<- Input_Page$Cells(191, 5);SPSNet13ATS3[["Value"]] <- Ddata[56,2]
    SPSNet13ATS4<- Input_Page$Cells(191, 6);SPSNet13ATS4[["Value"]] <- Ddata[57,2]
    LkWANet<- Input_Page$Cells(193, 2);LkWANet[["Value"]] <- Ddata[58,2]
    LKSamNet<- Input_Page$Cells(194, 2);LKSamNet[["Value"]] <- Ddata[59,2]
    DuGrNet<- Input_Page$Cells(195, 2);DuGrNet[["Value"]] <- Ddata[60,2]
    PuyallupNet<- Input_Page$Cells(196, 2);PuyallupNet[["Value"]] <- Ddata[61,2]
    PuyallupTest<- Input_Page$Cells(197, 2);PuyallupTest[["Value"]] <- Ddata[62,2]
    NisqNet<- Input_Page$Cells(198, 2);NisqNet[["Value"]] <- Ddata[63,2]
    NisqTangle<- Input_Page$Cells(199, 2);NisqTangle[["Value"]] <- Ddata[64,2]
    NisqTanglePRM<- Input_Page$Cells(199, 5);NisqTanglePRM[["Value"]] <- Ddata[65,2]
    NisqExp<- Input_Page$Cells(200, 2);NisqExp[["Value"]] <- Ddata[66,2]
    NisqExpPRM<- Input_Page$Cells(200, 5);NisqExpPRM[["Value"]] <- Ddata[67,2]
    NisqBSeine<- Input_Page$Cells(201, 2);NisqBSeine[["Value"]] <- Ddata[68,2]
    NisqBSeineLandings<- Input_Page$Cells(201, 4);NisqBSeineLandings[["Value"]] <- Ddata[69,2]
    NisqBSeinePRM<- Input_Page$Cells(201, 5);NisqBSeinePRM[["Value"]] <- Ddata[70,2]
    McAllisterNet<- Input_Page$Cells(202, 2);McAllisterNet[["Value"]] <- Ddata[71,2]
    MinterNet<- Input_Page$Cells(203, 2);MinterNet[["Value"]] <- Ddata[72,2]
    ChambersNet<- Input_Page$Cells(204, 2);ChambersNet[["Value"]] <- Ddata[73,2]
    DuGrTangleNet<- Input_Page$Cells(205, 2);DuGrTangleNet[["Value"]] <- Ddata[74,2]
    NKSamNetEarlyCS<- Input_Page$Cells(213, 2);NKSamNetEarlyCS[["Value"]] <- Ddata[75,2]
    NKSamNetEarlyTrTS3<- Input_Page$Cells(216, 2);NKSamNetEarlyTrTS3[["Value"]] <- Ddata[76,2]
    NkSamSumFallTrTS2<- Input_Page$Cells(220, 2);NkSamSumFallTrTS2[["Value"]] <- Ddata[77,2]
    NKSamSumFallNTTS3<- Input_Page$Cells(221, 2);NKSamSumFallNTTS3[["Value"]] <- Ddata[78,2]
    NKSamSumFallTrTS3<- Input_Page$Cells(222, 2);NKSamSumFallTrTS3[["Value"]] <- Ddata[79,2]
    NKSamSumFallNTTS4<- Input_Page$Cells(223, 2);NKSamSumFallNTTS4[["Value"]] <- Ddata[80,2]
    NKSamSumFallTrTS4<- Input_Page$Cells(224, 2);NKSamSumFallTrTS4[["Value"]] <- Ddata[81,2]
    NKSamSumFallFWNet<- Input_Page$Cells(225, 2);NKSamSumFallFWNet[["Value"]] <- Ddata[82,2]
    NKSam7ENetTr<- Input_Page$Cells(230, 2);NKSam7ENetTr[["Value"]] <- Ddata[83,2]
    NKSam7ENetNT<- Input_Page$Cells(230, 2);NKSam7ENetNT[["Value"]] <- Ddata[84,2]
    DungBatNetTr<- Input_Page$Cells(236, 2);DungBatNetTr[["Value"]] <- Ddata[85,2]
    DungBatNetNT<- Input_Page$Cells(237, 2);DungBatNetNT[["Value"]] <- Ddata[86,2]
    ElwhaTr<- Input_Page$Cells(238, 2);ElwhaTr[["Value"]] <- Ddata[87,2]
    ElwhaTest<- Input_Page$Cells(240, 2);ElwhaTest[["Value"]] <- Ddata[88,2]
    HokoExtTermNet<- Input_Page$Cells(242, 2);HokoExtTermNet[["Value"]] <- Ddata[89,2]
    SKSp<- Input_Page$Cells(249, 2);SKSp[["Value"]] <- Ddata[90,2]
    SkSpSprProp<- Input_Page$Cells(249, 5);SkSpSprProp[["Value"]] <- Ddata[91,2]
    SKMSFSprUMEnc<- Input_Page$Cells(250, 2);SKMSFSprUMEnc[["Value"]] <- Ddata[92,2]
    SKMSFSprUMRet<- Input_Page$Cells(250, 4);SKMSFSprUMRet[["Value"]] <- Ddata[93,2]
    SKMSFSprMEnc<- Input_Page$Cells(251, 2);SKMSFSprMEnc[["Value"]] <- Ddata[94,2]
    SKMSFSprMRel<- Input_Page$Cells(251, 4);SKMSFSprMRel[["Value"]] <- Ddata[95,2]
    StillySp<- Input_Page$Cells(258, 2);StillySp[["Value"]] <- Ddata[96,2]
    SnoSp<- Input_Page$Cells(259, 2);SnoSp[["Value"]] <- Ddata[97,2]
    SkykMSFUM<- Input_Page$Cells(261, 2);SkykMSFUM[["Value"]] <- Ddata[98,2]
    SkykMSFUMRet<- Input_Page$Cells(261, 4);SkykMSFUMRet[["Value"]] <- Ddata[99,2]
    SkykMSFM<- Input_Page$Cells(262, 2);SkykMSFM[["Value"]] <- Ddata[100,2]
    SkykMSFMRel<- Input_Page$Cells(262, 4);SkykMSFMRel[["Value"]] <- Ddata[101,2]
    SkokRivSp<- Input_Page$Cells(265, 2);SkokRivSp[["Value"]] <- Ddata[102,2]
    SkokRivSelective<- Input_Page$Cells(265, 3);SkokRivSelective[["Value"]] <- Ddata[103,2]
    SkokRivMRel<- Input_Page$Cells(265, 5);SkokRivMRel[["Value"]] <- Ddata[104,2]
    SkokRivUMRet<- Input_Page$Cells(265, 6);SkokRivUMRet[["Value"]] <- Ddata[105,2]
    HCBTribsSp<- Input_Page$Cells(266, 2);HCBTribsSp[["Value"]] <- Ddata[106,2]
    HCCDTribsSp<- Input_Page$Cells(267, 2);HCCDTribsSp[["Value"]] <- Ddata[107,2]
    QuilceneSp<- Input_Page$Cells(268, 2);QuilceneSp[["Value"]] <- Ddata[108,2]
    LKWaSp<- Input_Page$Cells(271, 2);LKWaSp[["Value"]] <- Ddata[109,2]
    LKSamSp<- Input_Page$Cells(272, 2);LKSamSp[["Value"]] <- Ddata[110,2]
    DuGrSp<- Input_Page$Cells(273, 2);DuGrSp[["Value"]] <- Ddata[111,2]
    PuyallCarbSp<- Input_Page$Cells(274, 2);PuyallCarbSp[["Value"]] <- Ddata[112,2]
    PuyallCarbSpSprProp<- Input_Page$Cells(274, 5);PuyallCarbSpSprProp[["Value"]] <- Ddata[113,2]
    PuyallMSFMEnc<- Input_Page$Cells(275, 2);PuyallMSFMEnc[["Value"]] <- Ddata[114,2]
    PuyallMSFMRel<- Input_Page$Cells(275, 4);PuyallMSFMRel[["Value"]] <- Ddata[115,2]
    PuyallMSFUMEnc<- Input_Page$Cells(276, 2);PuyallMSFUMEnc[["Value"]] <- Ddata[116,2]
    PuyallMSFUMRet<- Input_Page$Cells(276, 4);PuyallMSFUMRet[["Value"]] <- Ddata[117,2]
    CarbMSFMEnc<- Input_Page$Cells(277, 2);CarbMSFMEnc[["Value"]] <- Ddata[118,2]
    CarbMSFMRel<- Input_Page$Cells(277, 4);CarbMSFMRel[["Value"]] <- Ddata[119,2]
    CarbMSFUMEnc<- Input_Page$Cells(278, 2);CarbMSFUMEnc[["Value"]] <- Ddata[120,2]
    CarbMSFUMRet<- Input_Page$Cells(278, 4);CarbMSFUMRet[["Value"]] <- Ddata[121,2]
    NisqSp<- Input_Page$Cells(279, 2);NisqSp[["Value"]] <- Ddata[122,2]
    NisqSelective<- Input_Page$Cells(279, 4);NisqSelective[["Value"]] <- Ddata[123,2]
    NisqMRel<- Input_Page$Cells(279, 5);NisqMRel[["Value"]] <- Ddata[124,2]
    NisqUMRet<- Input_Page$Cells(279, 6);NisqUMRet[["Value"]] <- Ddata[125,2]
    McAllisterSp<- Input_Page$Cells(280, 2);McAllisterSp[["Value"]] <- Ddata[126,2]
    MinterSp<- Input_Page$Cells(281, 2);MinterSp[["Value"]] <- Ddata[127,2]
    ChambersSp<- Input_Page$Cells(282, 2);ChambersSp[["Value"]] <- Ddata[128,2]
    DeschutesSp<- Input_Page$Cells(283, 2);DeschutesSp[["Value"]] <- Ddata[129,2]
    KennedySp<- Input_Page$Cells(284, 2);KennedySp[["Value"]] <- Ddata[130,2]
    SamWhatSp<- Input_Page$Cells(287, 2);SamWhatSp[["Value"]] <- Ddata[131,2]
    NKMSFEnc<- Input_Page$Cells(289, 2);NKMSFEnc[["Value"]] <- Ddata[132,2]
    NKMSFMRel<- Input_Page$Cells(289, 4);NKMSFMRel[["Value"]] <- Ddata[133,2]
    NKMSFUMRet<- Input_Page$Cells(289, 6);NKMSFUMRet[["Value"]] <- Ddata[134,2]
    DungSp<- Input_Page$Cells(294, 2);DungSp[["Value"]] <- Ddata[135,2]
    ElwhaSp<- Input_Page$Cells(295, 2);ElwhaSp[["Value"]] <- Ddata[136,2]
    HokoSp<- Input_Page$Cells(296, 2);HokoSp[["Value"]] <- Ddata[137,2]
    A8Sp <- Input_Page$Cells(101, 4);A8Sp[["Value"]] <- Ddata[138,2]
    HCPpn12CD <- Input_Page$Cells(153, 9);HCPpn12CD[["Value"]] <- Ddata[139,2]
    HCPpn9A <- Input_Page$Cells(161, 9);HCPpn9A[["Value"]] <- Ddata[140,2]
    HCPpn12CDNT <- Input_Page$Cells(165, 9);HCPpn12CDNT[["Value"]] <- Ddata[141,2]
    HCPpn9ANT <- Input_Page$Cells(168, 9);HCPpn9ANT[["Value"]] <- Ddata[142,2]
    WhiteRHR <- Input_Page$Cells(177, 2);WhiteRHR[["Value"]] <- Ddata[143,2]
    NooksackTnetMSFEncs <- Input_Page$Cells(214, 2);NooksackTnetMSFEncs [["Value"]] <- Ddata[144,2]
    
    wb$Save()

    
    xlApp$Quit()
}

nd <- Sys.time()
tm <- nd - strt
tm

