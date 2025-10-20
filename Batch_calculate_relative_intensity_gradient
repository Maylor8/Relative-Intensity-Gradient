# Author: Ben Maylor; ben.maylor@ndph.ox.ac.uk
# Created: 26/10/2022
# Last updated: 01/08/2023

# Please cite both the article introducing the concept of a relative intensity gradient, and the github code as specified in the Github repository instructions.

# Description
# Reads in individual epoch files containing aggregated ENMO over any time period .e.g. 5s epoch data from GGIR
# Generates relative intensity gradient by reading in participants 'max' acceleration values from a test score e.g. ISWT or other (see associated article within github readme)
# Generates report of intensity gradient (regression slope, intercept and coefficient) for each participant, per day.
# Generates QC report for files not run due to errors/omissions in the 'Max' file.

# Instructions: 
# 1. Edit paths and parameters in the pre-requisites section below.
# 2. press "source" to batch process and generate the report in the working directory.
# 3. Optional: Edit the name of the output daysummary file right at the end of the code. Default is "Relative intensity daysummary YYYY-mm-dd.csv"

# Libraries
library(dplyr)

##### Pre-requisites
  setwd("X:/folder") # Where you want the output files saving
  Metafiles <- "X:/folder" # Where are the epoch .csv files?
  MaxData <- read.csv(file = "./Acceleration Max values.csv") # Load in max intensity spreadsheet of all participants
  IDlength <- 8 # Number of characters from the left of the filename to extract unique ID
  epochsize <- 5 # Epoch size of the data being read in, in seconds
##### End of Pre-requisites

##### Automated from here #####
  
# Where is the folder with all the 5s meta files?
  File.names <- list.files(path = Metafiles, pattern = ".csv", full.names = T)

# QC Data report for user
  QC <- as.data.frame(File.names)
  # QC for matching ID's
    QC$ID <- apply(QC, 1, function(row) {
              ID <- sub(".*/", "", row["File.names"])
              substr(ID, 1, IDlength)
              })
    QC <- QC[,c(2,1)]
    QCID <- QC[!(QC$ID %in% MaxData$ID), ]
    if (nrow(QCID) == 0) {
      print("No unmatched ID's")
    } else {
    QCID$Reason <- "No matching ID"
    }

  # QC for missing max values
    QCData <- MaxData %>%
              filter(ID %in% QC$ID & !grepl("^\\d+\\.?\\d*$", MaxData[,2])) %>%
              mutate(Reason = "No valid max data")

  # Generate report for skipped files
    if (nrow(QCID) == 0 && nrow(QCData) == 0) {
    print("No Data QC file needed")
    } else {
      QCout <- bind_rows(select(QCID,'ID','Reason'), 
                       select(QCData,'ID','Reason'))
    write.csv(QCout,file = paste0("Data QC ", format(Sys.Date(), "%Y-%m-%d"), ".csv"),row.names = FALSE)
    print("Data QC file generated in working directory for skipped files")
    }
  
# Clean data
  # Now remove non-numeric values in MaxData so they don't trip the loop
    MaxData <- MaxData %>% filter(!is.na(as.numeric(MaxData[,2]))) 
  # Regenerate list of file.names with only ones that have valid max values
    File.names <- merge(QC, MaxData, by = "ID", all = FALSE)
    File.names <- as.vector(File.names$File.names)
        
# Generate blank daysummary to be populated
  MasterOut <- data.frame(matrix(ncol = 6, nrow = 0))
  colnames(MasterOut)<- c("ID", "Date", "Day","Slope", "Intercept", "Rsquared")                      

# Progress output for user  
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
  # Extract the maximum acceleration from MaxData file
    IDmaxacc <- MaxData[MaxData$ID == IDname,2]
  
  # Calculate %s based on the max and up to 300% of acc max 
    iOutput <- data.frame(matrix(ncol = 0, nrow = 60))
    iOutput$rel_pct_start <- seq(0,295,5)
    iOutput$rel_pct_end <- seq(5,300,5)
    iOutput$mid_rel_mid <- iOutput$rel_pct_end - 2.5   
    iOutput$mg_start <- IDmaxacc * (iOutput$rel_pct_start/100)
    iOutput$mg_end <- IDmaxacc * (iOutput$rel_pct_end/100)       
    
  # Generate sequence in table
    breaks <- c(iOutput$mg_start,tail(iOutput$mg_end,n = 1),Inf)
    Data$Bin.no <- cut(Data$ENMO, breaks = breaks, labels = c(iOutput$mid_rel_mid,297.5))
    Data$Bin.no <- as.character(Data$Bin.no)
    
  # Aggregate by day and category 
    i <- aggregate(ENMO ~ Date + Bin.no, Data, length)  
    i$ENMO <- (i$ENMO * epochsize) / 60 # Converts the number of epochs to time, as is done in GGIR (function g.intensitygradient)
  
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
    
} # End for loop

##### Write Master Output file to working directory
  write.csv(x = MasterOut, file = paste0("Relative intensity daysummary ", format(Sys.Date(), "%Y-%m-%d"), ".csv"),row.names = F)  
  
#### End ####
