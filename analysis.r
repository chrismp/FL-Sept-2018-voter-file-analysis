require(dplyr)
require(ggplot2)
require(data.table)
require(lubridate)

options(scipen=999) # Remove scientific notation: https://stackoverflow.com/questions/5352099/how-to-disable-scientific-notation

YearMonthDayDirectory <- "20180911"
OutputDir <- "Output/"

# READ VOTER DETAIL FILE, AUG28 VOTER HISTORY FILE
  Aug28VoterHistory <- read.csv(
    file = paste0(OutputDir,"Aug28VoterHistory.csv"),
    header = T
  )
  
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
  
  names(DFVoterDetail) <- make.names(c(
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
  ))
  
  # Keep needed columns
  ColumnsToKeep <- c(
    "Voter.ID",
    "Gender",
    "Race",
    "Birth.Date",
    "Registration.Date",
    "Party.Affiliation"
  )
  DFVoterDetail2 <- DFVoterDetail[,ColumnsToKeep]

  
# ADD EXTRA COLUMNS FOR LATER ANALYSIS 
  # Race
  DFVoterDetail2$RaceLabel <- ifelse(
    test = DFVoterDetail2$Race==1,
    yes = "American Indian or Alaskan Native",
    no = ifelse(
      test = DFVoterDetail2$Race==2,
      yes = "Asian Or Pacific Islander",
      no = ifelse(
        test = DFVoterDetail2$Race==3,
        yes = "Black, Not Hispanic",
        no = ifelse(
          test = DFVoterDetail2$Race==4,
          yes = "Hispanic",
          no = ifelse(
            test = DFVoterDetail2$Race==5,
            yes = "White, Not Hispanic",
            no = ifelse(
              test = DFVoterDetail2$Race==6,
              yes = "Other",
              no = ifelse(
                test = DFVoterDetail2$Race==7,
                yes = "Multi‐racial",
                no = "Unknown"
              )
            )
          )
        )
      )
    )
  )
  
  # Age
  DFVoterDetail2$Birth.Date.Formatted <- as.Date(
    x = DFVoterDetail2$Birth.Date,
    format = "%m/%d/%Y"
  )
  DFVoterDetail2$Age <- time_length(
    x = difftime(
      time1 = as.Date("2018-08-28"),
      time2 = DFVoterDetail2$Birth.Date.Formatted
    ),
    unit = "years"
  )
  DFVoterDetail2$AgeGroup <- ifelse(
    test = DFVoterDetail2$Age < 30,
    yes = "18-29",
    no = ifelse(
      test = (DFVoterDetail2$Age>=30 & DFVoterDetail2$Age<=39),
      yes = "30-39",
      no = ifelse(
        test = (DFVoterDetail2$Age>=40 & DFVoterDetail2$Age<=49),
        yes = "40-49",
        no = ifelse(
          test = (DFVoterDetail2$Age>=50 & DFVoterDetail2$Age<=59),
          yes = "50-59",
          no = ifelse(
            test = (DFVoterDetail2$Age>60),
            yes = "60+",
            no = NA
          )
        )
      )
    )
  )
  
  # Gender
  DFVoterDetail2$SexLabel <- ifelse(
    test = DFVoterDetail2$Gender=='F',
    yes = "Female",
    no = ifelse(
      test = DFVoterDetail2$Gender=='M',
      yes = "Male",
      no = "Unknown"
    )
  )
  

# PRE-ANALYSIS
# AUG 28 FINAL VOTE COUNT BY COUNTY; http://archive.li/0MGnR
  VotesByCountyGroupBy <- group_by(
    .data = Aug28VoterHistory,
    County.Code
  )
  
  VotesByCounty <- summarise(
    .data = VotesByCountyGroupBy,
    Votes = n()
  )
  
  DFMerge <- merge(
    x = Aug28VoterHistory,
    y = DFVoterDetail2,
    by = "Voter.ID"
  )
  
  # Free up memory
  rm(
    # Aug28VoterHistory,
    DFVoterDetail,
    VotesByCounty,
    VotesByCountyGroupBy
  )
  gc()

  
# ANALYSIS
  FuncMakeSummaryCounts <- function(DataFrame, ...){
    GroupByDF <- DataFrame %>% group_by_(...) %>% summarise(Count = n())
    GroupByDF$PercentOfTotal <- GroupByDF$Count / sum(GroupByDF$Count)
    return(GroupByDF)
  }
  
  DataFramesToAnalyze <- list(
    DFVoterDetail2,
    filter(DFVoterDetail2, Party.Affiliation=="DEM"),
    filter(DFVoterDetail2, Party.Affiliation=="REP"),
    DFMerge,
    filter(DFMerge, Party.Affiliation=="DEM"),
    filter(DFMerge, Party.Affiliation=="REP")
  )
  
  DemographicsToSummarize <- c(
    "Party.Affiliation",
    "RaceLabel",
    "AgeGroup",
    "SexLabel"
  )
  
  i <- 0
  for(DataFrame in DataFramesToAnalyze) {
    PartyFilter <- ifelse(
      test = i==0 | i==3,
      yes = "AllParties",
      no = ifelse(
        test = i==1 | i==4,
        yes = "DemVoters",
        no = "RepVoters"
      )
    )
    
    ElectorateFilter <- ifelse(
      test = i<3,
      yes = "AllVoters",
      no = "Aug28Voters"
    )
    
    for (Demographic in DemographicsToSummarize) {
      assign(
        x = paste0("SummaryCount",ElectorateFilter,PartyFilter,Demographic),
        value = FuncMakeSummaryCounts(
          DataFrame = DataFrame,
          Demographic
        )
      )
    }
    i <- i+1
  }
  rm(
    DataFramesToAnalyze,
    DataFrame
  )
  gc()
