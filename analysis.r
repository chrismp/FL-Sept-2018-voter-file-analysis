library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)
library(extrafont)

source("localfunctions.r")

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
  # Party
  DFVoterDetail2$PartyFixed <- ifelse(
    test = DFVoterDetail2$Party.Affiliation=="DEM",
    yes = "Democrat",
    no = ifelse(
      test =  DFVoterDetail2$Party.Affiliation=="REP",
      yes = "Republican",
      no = "Neither"
    )
  )
  
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
  
  # Race 2
  DFVoterDetail2$RaceLabel2 <- ifelse(
    test = (DFVoterDetail2$RaceLabel=="Black, Not Hispanic") | (DFVoterDetail2$RaceLabel=="Hispanic") | (DFVoterDetail2$RaceLabel=="White, Not Hispanic") | (DFVoterDetail2$RaceLabel=="Unknown"),
    yes = DFVoterDetail2$RaceLabel,
    no = "Other"
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
  
  # TODO Remove voters who registered after July 30, 2018
  

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
    "PartyFixed",
    "RaceLabel",
    "RaceLabel2",
    "AgeGroup",
    "SexLabel"
  )
  
  DFList <- list()
  i <- 0
  j <- 1
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
    
    LoopingOverAllRegisteredVoters <- i < 3
    ElectorateFilter <- ifelse(
      test = LoopingOverAllRegisteredVoters,
      yes = "AllVoters",
      no = "Aug28Voters"
    )
    
    for (Demographic in DemographicsToSummarize) {
      DF <- FuncMakeSummaryCounts(
        DataFrame = DataFrame,
        Demographic
      )
      DF$Electorate <- ElectorateFilter
      DataName <- paste0("SummaryCount",ElectorateFilter,PartyFilter,Demographic)
      DFList[[DataName]] = DF
      j <- j+1
    }
    i <- i+1
  }
  

# FREE UP MEMORY
  rm(
    Aug28VoterHistory,
    DFMerge,
    DFVoterDetail2,
    DataFramesToAnalyze,
    DataFrame,
    DF
  )
  gc()
  
  
# CHARTS
  # Style rules
    StyleRules <- list()
    StyleRules$HedFont <- "Pragati Narrow"
    StyleRules$BarWidth <- 0.5
    StyleRules$Colors <- list()
    StyleRules$Colors$ChartBackground <- "#eeeeee"
    StyleRules$Colors$LineColor <- "#cccccc"
    StyleRules$Colors$Parties <- list()
    StyleRules$Colors$Parties$Democrats <- "#67a9cf"
    StyleRules$Colors$Parties$Republicans <- "#ef8a62"
    StyleRules$Colors$Parties$Neither <- "#bbbbbb"
    StyleRules$Caption <- "Chris Persaud / Datavizz.com\nSource: Florida Division of Elections, Sept. 2018 voter file"
    
  # Themes
    Themes <- list()
    Themes$Custom <- theme(
      plot.title = element_text(
        size = 20,
        family = StyleRules$HedFont,
        face = "bold"
      ),
      plot.subtitle = element_text(
        size = 13,
        margin = margin(
          b = unit(20, "pt")
        )
      ),
      plot.caption = element_text(
        size = 9,
        face = "italic",
        color = "#333333"
      ),
      axis.line.x = element_line(
        color = "#000000"
      ),
      axis.ticks = element_line(
        color = StyleRules$Colors$LineColor
      ),
      axis.title.x = element_text(
        margin = margin(
          t = 10
        )
      ),
      axis.text = element_text(
        size = 12
      ),
      axis.ticks.x = element_blank(),
      axis.title.y = element_text(
        margin = margin(
          r = 10
        )
      ),
      legend.title = element_blank(),
      legend.key = element_blank(),
      legend.key.height = unit(15,"pt"),
      legend.key.width = unit(30,"pt"),
      plot.background = element_rect(
        fill = StyleRules$Colors$ChartBackground
      ),
      panel.background = element_rect(
        fill = StyleRules$Colors$ChartBackground
      ),
      panel.grid.major = element_line(
        color = StyleRules$Colors$LineColor,
        size = 0.25
      ),
      panel.grid.minor = element_blank(),
      plot.margin = margin(
        t = 5,
        r = 5,
        b = 5,
        l = 5
      )
    )
    
    Themes$FlippedBar <- theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major.y = element_blank()
    )
    
  # Party affiliation by electorate
    ChartDataParties <- rbind(
      DFList[["SummaryCountAllVotersAllPartiesPartyFixed"]], 
      DFList[["SummaryCountAug28VotersAllPartiesPartyFixed"]]
    )
    ChartDataParties$PartyFixed <- factor(
      x = ChartDataParties$PartyFixed,
      levels = rev(c("Democrat","Republican","Neither"))
    )
    
    ChartPartyBreakdownStyle <- list()
    ChartPartyBreakdownStyle$HED <- list()
    ChartPartyBreakdownStyle$HED$x <- 5
    ChartPartyBreakdownStyle$HED$y <- -0.1
    ChartPartyBreakdown <- ggplot(
      data = ChartDataParties,
      aes(
        x = Electorate,
        y = PercentOfTotal
      )
    ) + 
      geom_bar(
        aes(fill=PartyFixed),
        stat = "identity",
        width = StyleRules$BarWidth
      ) +
      scale_fill_manual(
        values = rev(c(
          StyleRules$Colors$Parties$Democrats,
          StyleRules$Colors$Parties$Republicans,
          StyleRules$Colors$Parties$Neither
        ))
      ) +
      scale_x_discrete(
        labels = c("All\nFlorida\nvoters","Aug. 28\nvoters"),
        expand = c(StyleRules$BarWidth*4, 0)
      ) +
      scale_y_continuous(
        labels = func.percentFormatX, # Using this function because the chart will flip
        expand = c(0,0),
        limits = c(0,1.05)       
      ) +
      geom_text( # Add figures to bars
        aes(
          label = paste0(
            sprintf(fmt = "%.0f", (PercentOfTotal*100)),
            '%'
          ),
          group = PartyFixed
        ),
        position = position_stack(
          vjust = 0.5
        ),
        fontface = "bold",
        color = "#ffffff",
        size = 5
      ) +
      geom_text(
        label = "Florida primary voters vs. all registered voters",
        inherit.aes = F,
        x = ChartPartyBreakdownStyle$HED$x,
        y = ChartPartyBreakdownStyle$HED$y,
        check_overlap = T,
        hjust = 0,
        size = 8,
        family = StyleRules$HedFont,
        fontface = "bold"
      ) +
      geom_text(
        label = "Who is registered to vote and who voted Aug. 28, 2018",
        inherit.aes = F,
        x = ChartPartyBreakdownStyle$HED$x - 0.5,
        y = ChartPartyBreakdownStyle$HED$y,
        check_overlap = T,
        hjust = 0,
        size = 5
      ) +
      labs(
        caption = StyleRules$Caption
      ) +
      guides(
        fill = guide_legend(reverse = T)
      ) +
      coord_flip(
        clip = "off"
      ) +
      Themes$Custom +
      Themes$FlippedBar + 
      theme(
        legend.background = element_rect(
          fill = StyleRules$Colors$ChartBackground
        ),
        legend.position = c(0.2, 0.8),
        legend.direction = "vertical",
        legend.text = element_text(
          size = 10,
          colour = "#666666",
          margin = margin(l = 2.5)
        ),
        plot.margin = unit(
          x = c(5,1,1,1),
          units = "line"
        )
      )
    ChartPartyBreakdown
    ggsave(
      filename = "Parties.png",
      plot = ChartPartyBreakdown,
      device = "png",
      path = OutputDir,
      width = 200,
      height = 125,
      units = "mm",
      dpi = 144
    )
  
  
  
  
  
  
  
  
