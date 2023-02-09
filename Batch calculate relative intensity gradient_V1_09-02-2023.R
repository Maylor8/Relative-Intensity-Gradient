# Author: Ben Maylor; bm259@leicester.ac.uk
# Created: 26/10/2022
# Last updated: 09/02/2023

# Description / Notes:
# Reads in individual GGIR 5s epoch files
# Generates relative intensity gradient by reading in participants 'max' acceleration values from a test score e.g. ISWT or other
# Generates report of intensity gradient (regression slope, intercept and coefficient) for each participant, per day.

# Instructions:
# 1. Edit paths and parameters in the pre-requisites section below.
# 2. press "source" to batch process and generate the report in the working directory.

# Libraries
library(dplyr)

##### Pre-requisites #####
# Ensure setwd, Metafiles and MaxData paths are updated and the filenames begin with the participant ID

# Set folder/file paths
  setwd("X:/Studyname") # Where you want the output file saving
  Metafiles <- "X:/Studyname/Meta files" # Where are the 5s epoch files?
  MaxData <- read.csv(file = "X:/Studyname/AccMax.csv") # Load in max intensity spreadsheet of all participants
  IDlength <- 8 # Number of characters from the left to extract unique ID

  
##### Automated from here #####
  
# Where is the folder with all the 5s meta files?
  File.names <- list.files(path = Metafiles, pattern = ".csv", full.names = T)

# Generate blank daysummary to be populated
  MasterOut <- data.frame(matrix(ncol = 6, nrow = 0))
  colnames(MasterOut)<- c("ID", "Date", "Day","Slope", "Intercept", "Rsquared")                      
  
  Count <- 1 # Starts counter for progress printout during the run

##### Begin looping through files
  
for (i in 1:length(File.names)){
  Data <- read.csv(File.names[i]) 
  
# Make ENMO mg
  Data$ENMO <- Data$ENMO * 1000 
# Format Date
  Data$Date <- as.POSIXct(Data$timestamp, format="%Y-%m-%d")
# Extract ID
  IDname <- substr(sub(".*/","", File.names[i]), 1, IDlength)
# Extract the maximum acceleration from MadData file (skips if no ID match)
  if (all(grep(IDname, MaxData$ID, fixed = T)<1)) next
  else {
  IDmaxacc <- MaxData[MaxData$ID == IDname,2]
    
# Calculate %s based on the max and up to 300% of acc max 
  iOutput <- data.frame(matrix(ncol = 0, nrow = 40))
  iOutput$rel_pct_start <- seq(0,195,5)
  iOutput$rel_pct_end <- seq(5,200,5)
  iOutput$mid_rel_mid <- iOutput$rel_pct_end - 2.5   
  iOutput$mg_start <- IDmaxacc * (iOutput$rel_pct_start/100)
  iOutput$mg_end <- IDmaxacc * (iOutput$rel_pct_end/100)       
  
# Generate sequence in table
Data <- Data %>% mutate(Bin.no = case_when(ENMO <  iOutput[1,5] ~ '2.5',
                                           ENMO >= iOutput[1,5] & ENMO < iOutput[2,5] ~'7.5',
                                           ENMO >= iOutput[2,5] & ENMO < iOutput[3,5] ~ '12.5',
                                           ENMO >= iOutput[3,5] & ENMO < iOutput[4,5] ~ '17.5',
                                           ENMO >= iOutput[4,5] & ENMO < iOutput[5,5] ~ '22.5',
                                           ENMO >= iOutput[5,5] & ENMO < iOutput[6,5] ~ '27.5',
                                           ENMO >= iOutput[6,5] & ENMO < iOutput[7,5] ~ '32.5',
                                           ENMO >= iOutput[7,5] & ENMO < iOutput[8,5] ~ '37.5',
                                           ENMO >= iOutput[8,5] & ENMO < iOutput[9,5] ~ '42.5',
                                           ENMO >= iOutput[9,5] & ENMO < iOutput[10,5] ~ '47.5',
                                           ENMO >= iOutput[10,5] & ENMO < iOutput[11,5] ~ '52.5',
                                           ENMO >= iOutput[11,5] & ENMO < iOutput[12,5] ~ '57.5',
                                           ENMO >= iOutput[12,5] & ENMO < iOutput[13,5] ~ '62.5',
                                           ENMO >= iOutput[13,5] & ENMO < iOutput[14,5] ~ '67.5',
                                           ENMO >= iOutput[14,5] & ENMO < iOutput[15,5] ~ '72.5',
                                           ENMO >= iOutput[15,5] & ENMO < iOutput[16,5] ~ '77.5',
                                           ENMO >= iOutput[16,5] & ENMO < iOutput[17,5] ~ '82.5',
                                           ENMO >= iOutput[17,5] & ENMO < iOutput[18,5] ~ '87.5',
                                           ENMO >= iOutput[18,5] & ENMO < iOutput[19,5] ~ '92.5',
                                           ENMO >= iOutput[19,5] & ENMO < iOutput[20,5] ~ '97.5',
                                           ENMO >= iOutput[20,5] & ENMO < iOutput[21,5] ~ '102.5',
                                           ENMO >= iOutput[21,5] & ENMO < iOutput[22,5] ~ '107.5',
                                           ENMO >= iOutput[22,5] & ENMO < iOutput[23,5] ~ '112.5',
                                           ENMO >= iOutput[23,5] & ENMO < iOutput[24,5] ~ '117.5',
                                           ENMO >= iOutput[24,5] & ENMO < iOutput[25,5] ~ '122.5',
                                           ENMO >= iOutput[25,5] & ENMO < iOutput[26,5] ~ '127.5',
                                           ENMO >= iOutput[26,5] & ENMO < iOutput[27,5] ~ '132.5',
                                           ENMO >= iOutput[27,5] & ENMO < iOutput[28,5] ~ '137.5',
                                           ENMO >= iOutput[28,5] & ENMO < iOutput[29,5] ~ '142.5',
                                           ENMO >= iOutput[29,5] & ENMO < iOutput[30,5] ~ '147.5',
                                           ENMO >= iOutput[30,5] & ENMO < iOutput[31,5] ~ '152.5',
                                           ENMO >= iOutput[31,5] & ENMO < iOutput[32,5] ~ '157.5',
                                           ENMO >= iOutput[32,5] & ENMO < iOutput[33,5] ~ '162.5',
                                           ENMO >= iOutput[33,5] & ENMO < iOutput[34,5] ~ '167.5',
                                           ENMO >= iOutput[34,5] & ENMO < iOutput[35,5] ~ '172.5',
                                           ENMO >= iOutput[35,5] & ENMO < iOutput[36,5] ~ '177.5',
                                           ENMO >= iOutput[36,5] & ENMO < iOutput[37,5] ~ '182.5',
                                           ENMO >= iOutput[37,5] & ENMO < iOutput[38,5] ~ '187.5',
                                           ENMO >= iOutput[38,5] & ENMO < iOutput[39,5] ~ '192.5',
                                           ENMO >= iOutput[39,5] & ENMO < iOutput[40,5] ~ '197.5',
                                           ENMO >= iOutput[40,5] & ENMO < iOutput[41,5] ~ '202.5',
                                           ENMO >= iOutput[41,5] & ENMO < iOutput[42,5] ~ '207.5',
                                           ENMO >= iOutput[42,5] & ENMO < iOutput[43,5] ~ '212.5',
                                           ENMO >= iOutput[43,5] & ENMO < iOutput[44,5] ~ '217.5',
                                           ENMO >= iOutput[44,5] & ENMO < iOutput[45,5] ~ '222.5',
                                           ENMO >= iOutput[45,5] & ENMO < iOutput[46,5] ~ '227.5',
                                           ENMO >= iOutput[46,5] & ENMO < iOutput[47,5] ~ '232.5',
                                           ENMO >= iOutput[47,5] & ENMO < iOutput[48,5] ~ '237.5',
                                           ENMO >= iOutput[48,5] & ENMO < iOutput[49,5] ~ '242.5',
                                           ENMO >= iOutput[49,5] & ENMO < iOutput[50,5] ~ '247.5',
                                           ENMO >= iOutput[50,5] & ENMO < iOutput[51,5] ~ '252.5',
                                           ENMO >= iOutput[51,5] & ENMO < iOutput[52,5] ~ '257.5',
                                           ENMO >= iOutput[52,5] & ENMO < iOutput[53,5] ~ '262.5',
                                           ENMO >= iOutput[53,5] & ENMO < iOutput[54,5] ~ '267.5',
                                           ENMO >= iOutput[54,5] & ENMO < iOutput[55,5] ~ '272.5',
                                           ENMO >= iOutput[55,5] & ENMO < iOutput[56,5] ~ '277.5',
                                           ENMO >= iOutput[56,5] & ENMO < iOutput[57,5] ~ '282.5',
                                           ENMO >= iOutput[57,5] & ENMO < iOutput[58,5] ~ '287.5',
                                           ENMO >= iOutput[58,5] & ENMO < iOutput[59,5] ~ '292.5',
                                           ENMO >= iOutput[59,5] & ENMO < iOutput[60,5] ~ '297.5',
                                           ENMO >= iOutput[60,5] ~ '300plus')) 
# Aggregate by day and category 
  i <- aggregate(ENMO ~ Date + Bin.no, Data, length)  

# Natural log of the data
  i$Binlog <- log(as.numeric(i$Bin.no))
  i$ENMOlog <- log(as.numeric(i$ENMO))

# Do regressions per day
  X <- 'Binlog'
  Y <- 'ENMOlog'
  Regress_formula <- paste(Y, X, sep = "~")
  
  Regression <- by(i, i$Date, function(x) lm(eval(parse(text=Regress_formula)), data = x))

# Extract regression outputs and format
  regress_slope <- do.call(rbind, lapply(Regression, summary))
  iR2 <- regress_slope[,"r.squared"]
  iR2 <- as.data.frame(iR2)
  iR2 <- t(iR2)
  iR2 <- cbind(rownames(iR2), data.frame(iR2, row.names = NULL))
  colnames(iR2) <- c("Date","Rsquared")
  iR2$Date <- gsub("X","", as.character(iR2$Date))
  iR2$Date <- as.Date(iR2$Date, format="%Y.%m.%d")
  iCoef <- regress_slope[,"coefficients"]
  iCoef <- as.data.frame(iCoef)
  iCoef <- t(iCoef)
  iCoef <- cbind(rownames(iCoef), data.frame(iCoef, row.names = NULL))
  iCoef <- iCoef[grep("Estimate", iCoef$`rownames(iCoef)`),]
  colnames(iCoef) <- c("Date","Intercept","Slope")
  iCoef$Date <- gsub("X","", as.character(iCoef$Date))
  iCoef$Date <- gsub(".Estimate","", as.character(iCoef$Date))
  iCoef$Date <- as.Date(iCoef$Date, format="%Y.%m.%d")

# Merge regression outputs and add ID and Day number  
  iOutput <- left_join(iR2, iCoef, by = "Date", keep=F)
  iOutput$ID <- IDname
  iOutput$Day <- as.numeric(factor(iOutput$Date))
# Re-order columns for output
  iOutput <- iOutput %>% select(ID, Date, Day, Slope, Intercept, Rsquared) 
# Add to Master Output
  MasterOut <-  rbind(MasterOut, iOutput)

# print name of file completed in the loop to enable progress tracking
  print(paste("File",Count,"of", length(File.names), "processed"))  
# Update loop count 
  Count <- Count + 1
# End Loop
}

}
##### Write Master Output file to working directory
write.csv(x = MasterOut, file = "relative intensity output.csv",row.names = F)  
  
#### End ####