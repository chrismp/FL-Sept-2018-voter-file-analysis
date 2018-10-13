library(dplyr)
library(data.table)

YearMonthDayDirectory <- "20180911"
OutputDir <- "Output/"

# MAKE VOTER DETAIL DATAFRAME
  VoterDetailFiles <- list.files(
    path = paste0("../",YearMonthDayDirectory,"_VoterDetail"),
    pattern = "*.txt",
    full.names = TRUE
  )
  
  DFVoterDetail <- do.call(
    what = "rbind",
    args = lapply(
      X = VoterDetailFiles,
      FUN = function(fn) data.frame(
        Filename=fn, 
        # read.table(
        #   file = fn,
        #   header = FALSE,
        #   sep = "\t",
        #   comment.char = "",
        #   quote = ""
        # )
        fread(fn)
      )
    )
  )
  DFVoterDetail[1] <- NULL # Remove Filename column from data frame
  
  names(DFVoterDetail) <- c(
    "County Code",
    "Voter ID",
    "Name Last",
    "Name Suffix",
    "Name First",
    "Name Middle",
    "Requested public records exemption",
    "Residence Address Line 1",
    "Residence Address Line 2",
    "Residence City (USPS)",
    "Residence State",
    "Residence Zipcode",
    "Mailing Address Line 1",
    "Mailing Address Line 2",
    "Mailing Address Line 3",
    "Mailing City",
    "Mailing State",
    "Mailing Zipcode",
    "Mailing Country",
    "Gender",
    "Race",
    "Birth Date",
    "Registration Date",
    "Party Affiliation",
    "Precinct",
    "Precinct Group",
    "Precinct Split",
    "Precinct Suffix",
    "Voter Status",
    "Congressional District",
    "House District",
    "Senate District",
    "County Commission District",
    "School Board District",
    "Daytime Area Code",
    "Daytime Phone Number",
    "Daytime Phone Extension",
    "Email address"
  )
  gc()

# MAKE VOTER HISTORY FRAME
  VoterHistoryFiles <- list.files(
    path = paste0("../",YearMonthDayDirectory,"_VoterHistory"),
    pattern = "*.txt",
    full.names = TRUE
  )
  
  DFVoterHistory <- do.call(
    what = "rbind",
    args = lapply(
      X = VoterHistoryFiles,
      FUN = function(fn) data.frame(
        Filename=fn, 
        # read.table(
        #   file = fn,
        #   header = FALSE,
        #   sep = "\t",
        #   comment.char = "",
        #   quote = ""
        # )
        fread(fn)
      )
    )
  )
  gc()
  
  DFVoterHistory[1] <- NULL # Remove Filename column from data frame
  names(DFVoterHistory) <- c(
    "County Code",
    "Voter ID",
    "Election Date",
    "Election Type",
    "History Code"
  )
  # UniqueElectionDates <- unique(x = DFVoterHistory$`Election Date`)
  gc()