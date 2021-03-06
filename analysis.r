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
    StyleRules$Colors$Parties$Democrats <- "#1f78b4"
    StyleRules$Colors$Parties$Republicans <- "#e31a1c"
    StyleRules$Colors$Parties$Neither <- "#bbbbbb"
    
    StyleRules$Colors$Race <- list()
    StyleRules$Colors$Race$Black <- "#66c2a5"
    StyleRules$Colors$Race$Hispanic <- "#fc8d62"
    StyleRules$Colors$Race$Other <- "#8da0cb"
    StyleRules$Colors$Race$Unknown <- "#bbbbbb"
    StyleRules$Colors$Race$White <- "#e78ac3"
    
    StyleRules$Colors$AgeGroup <- list()
    StyleRules$Colors$AgeGroup$X1829 <- "#fdd0a2"
    StyleRules$Colors$AgeGroup$X3039 <- "#fdae6b"
    StyleRules$Colors$AgeGroup$X4049 <- "#fd8d3c"
    StyleRules$Colors$AgeGroup$X5059 <- "#e6550d"
    StyleRules$Colors$AgeGroup$X60Plus <- "#a63603"
    StyleRules$Colors$AgeGroup$Unknown <- "#bbbbbb"
    
    StyleRules$Colors$Sex <- list()
    StyleRules$Colors$Sex$Female <- "#eaafe6"
    StyleRules$Colors$Sex$Male <- "#acceef"
    StyleRules$Colors$Sex$Unknown <- "#bbbbbb"
    
    StyleRules$YExpand <- 1.05
    StyleRules$Subtitle1 <- "Preliminary look at who could have voted and who did"
    StyleRules$Labels <- list()
    StyleRules$Labels$Aug28VsAll <- c("All\nFlorida\nvoters","Aug. 28\nvoters")
    StyleRules$Labels$Party <- c("Republican\nprimary","Democratic\nprimary")
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
  
  # BY VOTER GROUP - ALL VOTERS VS AUG 28 VOTERS
    # Party affiliation
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
          labels = c(StyleRules$Labels$Aug28VsAll),
          expand = c(StyleRules$BarWidth*4, 0)
        ) +
        scale_y_continuous(
          labels = func.percentFormatX, # Using this function because the chart will flip
          expand = c(0,0),
          limits = c(0,StyleRules$YExpand)       
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
          label = "Mostly major party voters in the 2018 Florida primary races",
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
          label = StyleRules$Subtitle1,
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
    
    # Race
      ChartDataRace <- rbind(
        DFList[["SummaryCountAllVotersAllPartiesRaceLabel2"]], 
        DFList[["SummaryCountAug28VotersAllPartiesRaceLabel2"]]
      )
      ChartDataRace$RaceLabel2 <- factor(
        x = ChartDataRace$RaceLabel2,
        levels = rev(c("White, Not Hispanic","Black, Not Hispanic","Hispanic","Other","Unknown"))
      )
  
      ChartRaceStyle <- list()
      ChartRaceStyle$HED <- list()
      ChartRaceStyle$HED$x <- 5.2
      ChartRaceStyle$HED$y <- -0.1
      ChartRace <- ggplot(
        data = ChartDataRace,
        aes(
          x = Electorate,
          y = PercentOfTotal
        )
      ) + 
        geom_bar(
          aes(fill=RaceLabel2),
          stat = "identity",
          width = StyleRules$BarWidth
        ) +
        scale_fill_manual(
          values = rev(c(
            StyleRules$Colors$Race$White,
            StyleRules$Colors$Race$Black,
            StyleRules$Colors$Race$Hispanic,
            StyleRules$Colors$Race$Other,
            StyleRules$Colors$Race$Unknown
          ))
        ) +
        scale_x_discrete(
          labels = c(StyleRules$Labels$Aug28VsAll),
          expand = c(StyleRules$BarWidth*4, 0)
        ) +
        scale_y_continuous(
          labels = func.percentFormatX, # Using this function because the chart will flip
          expand = c(0,0),
          limits = c(0,StyleRules$YExpand)       
        ) +
        geom_text( # Add figures to bars
          aes(
            label = ifelse(
              test = PercentOfTotal > 0.05,
              yes = paste0(
                sprintf(fmt = "%.0f", (PercentOfTotal*100)),
                '%'
              ),
              no = ''
            ),
            group = RaceLabel2
          ),
          position = position_stack(
            vjust = 0.5
          ),
          fontface = "bold",
          color = "#ffffff",
          size = 5
        ) +
        geom_text(
          label = "White voters likely dominated the 2018 Florida primary election",
          inherit.aes = F,
          x = ChartRaceStyle$HED$x,
          y = ChartRaceStyle$HED$y,
          check_overlap = T,
          hjust = 0,
          size = 8,
          family = StyleRules$HedFont,
          fontface = "bold"
        ) +
        geom_text(
          label = StyleRules$Subtitle1,
          inherit.aes = F,
          x = ChartRaceStyle$HED$x - 0.4,
          y = ChartRaceStyle$HED$y,
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
          legend.position = c(0.2, 0.9),
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
      ChartRace
      ggsave(
        filename = "Race.png",
        plot = ChartRace,
        device = "png",
        path = OutputDir,
        width = 200,
        height = 125,
        units = "mm",
        dpi = 144
      )
    
    # Age groups
      ChartDataAgeGroup <- rbind(
        DFList[["SummaryCountAllVotersAllPartiesAgeGroup"]], 
        DFList[["SummaryCountAug28VotersAllPartiesAgeGroup"]]
      )
      ChartDataAgeGroup$AgeGroup <- ifelse(
        test = is.na(ChartDataAgeGroup$AgeGroup),
        yes = "Unknown",
        no = ChartDataAgeGroup$AgeGroup
      )
      ChartDataAgeGroup$AgeGroup <- factor(
        x = ChartDataAgeGroup$AgeGroup,
        levels = rev(c("18-29","30-39","40-49","50-59","60+","Unknown"))
      )
      
      ChartAgeGroupStyle <- list()
      ChartAgeGroupStyle$HED <- list()
      ChartAgeGroupStyle$HED$x <- 5.2
      ChartAgeGroupStyle$HED$y <- -0.1
      ChartAgeGroup <- ggplot(
        data = ChartDataAgeGroup,
        aes(
          x = Electorate,
          y = PercentOfTotal
        )
      ) + 
        geom_bar(
          aes(fill=AgeGroup),
          stat = "identity",
          width = StyleRules$BarWidth
        ) +
        scale_fill_manual(
          values = rev(c(
            StyleRules$Colors$AgeGroup$X1829,
            StyleRules$Colors$AgeGroup$X3039,
            StyleRules$Colors$AgeGroup$X4049,
            StyleRules$Colors$AgeGroup$X5059,
            StyleRules$Colors$AgeGroup$X60Plus,
            StyleRules$Colors$AgeGroup$Unknown
          ))
        ) +
        scale_x_discrete(
          labels = c(StyleRules$Labels$Aug28VsAll),
          expand = c(StyleRules$BarWidth*4, 0)
        ) +
        scale_y_continuous(
          labels = func.percentFormatX, # Using this function because the chart will flip
          expand = c(0,0),
          limits = c(0,StyleRules$YExpand)       
        ) +
        geom_text( # Add figures to bars
          aes(
            label = ifelse(
              test = PercentOfTotal > 0.05,
              yes = paste0(
                sprintf(fmt = "%.0f", (PercentOfTotal*100)),
                '%'
              ),
              no = ''
            ),
            group = AgeGroup
          ),
          position = position_stack(
            vjust = 0.5
          ),
          fontface = "bold",
          color = "#ffffff",
          size = 5
        ) +
        geom_text(
          label = "Few young people voted in Florida's 2018 primary",
          inherit.aes = F,
          x = ChartAgeGroupStyle$HED$x,
          y = ChartAgeGroupStyle$HED$y,
          check_overlap = T,
          hjust = 0,
          size = 8,
          family = StyleRules$HedFont,
          fontface = "bold"
        ) +
        geom_text(
          label = StyleRules$Subtitle1,
          inherit.aes = F,
          x = ChartAgeGroupStyle$HED$x - 0.4,
          y = ChartAgeGroupStyle$HED$y,
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
          legend.position = c(0.2, 0.9),
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
      ChartAgeGroup
      ggsave(
        filename = "AgeGroups.png",
        plot = ChartAgeGroup,
        device = "png",
        path = OutputDir,
        width = 200,
        height = 130,
        units = "mm",
        dpi = 144
      )
  
      # Sex
      ChartDataSex <- rbind(
        DFList[["SummaryCountAllVotersAllPartiesSexLabel"]], 
        DFList[["SummaryCountAug28VotersAllPartiesSexLabel"]]
      )
      ChartDataSex$SexLabel <- factor(
        x = ChartDataSex$SexLabel,
        levels = rev(c("Female","Male","Unknown"))
      )
      
      ChartSexStyle <- list()
      ChartSexStyle$HED <- list()
      ChartSexStyle$HED$x <- 5.2
      ChartSexStyle$HED$y <- -0.1
      ChartSex <- ggplot(
        data = ChartDataSex,
        aes(
          x = Electorate,
          y = PercentOfTotal
        )
      ) + 
        geom_bar(
          aes(fill=SexLabel),
          stat = "identity",
          width = StyleRules$BarWidth
        ) +
        scale_fill_manual(
          values = rev(c(
            StyleRules$Colors$Sex$Female,
            StyleRules$Colors$Sex$Male,
            StyleRules$Colors$Sex$Unknown
          ))
        ) +
        scale_x_discrete(
          labels = c(StyleRules$Labels$Aug28VsAll),
          expand = c(StyleRules$BarWidth*4, 0)
        ) +
        scale_y_continuous(
          labels = func.percentFormatX, # Using this function because the chart will flip
          expand = c(0,0),
          limits = c(0,StyleRules$YExpand)       
        ) +
        geom_text( # Add figures to bars
          aes(
            label = ifelse(
              test = PercentOfTotal > 0.05,
              yes = paste0(
                sprintf(fmt = "%.0f", (PercentOfTotal*100)),
                '%'
              ),
              no = ''
            ),
            group = SexLabel
          ),
          position = position_stack(
            vjust = 0.5
          ),
          fontface = "bold",
          color = "#ffffff",
          size = 5
        ) +
        geom_text(
          label = "2018 Florida primary voters vs. all registered voters, by sex",
          inherit.aes = F,
          x = ChartSexStyle$HED$x,
          y = ChartSexStyle$HED$y,
          check_overlap = T,
          hjust = 0,
          size = 8,
          family = StyleRules$HedFont,
          fontface = "bold"
        ) +
        geom_text(
          label = StyleRules$Subtitle1,
          inherit.aes = F,
          x = ChartSexStyle$HED$x - 0.4,
          y = ChartSexStyle$HED$y,
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
          legend.position = c(0.2, 0.9),
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
      ChartSex
      ggsave(
        filename = "Sex.png",
        plot = ChartSex,
        device = "png",
        path = OutputDir,
        width = 200,
        height = 120,
        units = "mm",
        dpi = 144
      )
      
    # BY PARTY
      # Race
      DFList[["SummaryCountAug28VotersDemVotersRaceLabel2"]]$Electorate <- "Democrat"
      DFList[["SummaryCountAug28VotersRepVotersRaceLabel2"]]$Electorate <- "Republican"
      ChartDataRaceByParty <- rbind(
        DFList[["SummaryCountAug28VotersDemVotersRaceLabel2"]], 
        DFList[["SummaryCountAug28VotersRepVotersRaceLabel2"]]
      )
      ChartDataRaceByParty$RaceLabel2 <- factor(
        x = ChartDataRaceByParty$RaceLabel2,
        levels = rev(c("White, Not Hispanic","Black, Not Hispanic","Hispanic","Other","Unknown"))
      )
      
      ChartRaceStyle <- list()
      ChartRaceStyle$HED <- list()
      ChartRaceStyle$HED$x <- 5.2
      ChartRaceStyle$HED$y <- -0.15
      ChartRaceByParty <- ggplot(
        data = ChartDataRaceByParty,
        aes(
          x = Electorate,
          y = PercentOfTotal
        )
      ) + 
        geom_bar(
          aes(fill=RaceLabel2),
          stat = "identity",
          width = StyleRules$BarWidth
        ) +
        scale_fill_manual(
          values = rev(c(
            StyleRules$Colors$Race$White,
            StyleRules$Colors$Race$Black,
            StyleRules$Colors$Race$Hispanic,
            StyleRules$Colors$Race$Other,
            StyleRules$Colors$Race$Unknown
          ))
        ) +
        scale_x_discrete(
          limits= c("Republican","Democrat"),
          labels = c(StyleRules$Labels$Party),
          expand = c(StyleRules$BarWidth*4, 0)
        ) +
        scale_y_continuous(
          labels = func.percentFormatX, # Using this function because the chart will flip
          expand = c(0,0),
          limits = c(0,StyleRules$YExpand)       
        ) +
        geom_text( # Add figures to bars
          aes(
            label = ifelse(
              test = PercentOfTotal > 0.05,
              yes = paste0(
                sprintf(fmt = "%.0f", (PercentOfTotal*100)),
                '%'
              ),
              no = ''
            ),
            group = RaceLabel2
          ),
          position = position_stack(
            vjust = 0.5
          ),
          fontface = "bold",
          color = "#ffffff",
          size = 5
        ) +
        geom_text(
          label = "2018 Florida Dem primary less white than GOP",
          inherit.aes = F,
          x = ChartRaceStyle$HED$x,
          y = ChartRaceStyle$HED$y,
          check_overlap = T,
          hjust = 0,
          size = 8,
          family = StyleRules$HedFont,
          fontface = "bold"
        ) +
        geom_text(
          label = "Preliminary racial breakdown of Florida's Democratic and Republican primaries",
          inherit.aes = F,
          x = ChartRaceStyle$HED$x - 0.4,
          y = ChartRaceStyle$HED$y,
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
          legend.position = c(0.2, 0.9),
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
      ChartRaceByParty
      ggsave(
        filename = "Race-party.png",
        plot = ChartRaceByParty,
        device = "png",
        path = OutputDir,
        width = 200,
        height = 125,
        units = "mm",
        dpi = 144
      )
      
      # Age groups
      DFList[["SummaryCountAug28VotersDemVotersAgeGroup"]]$Electorate <- "Democrat"
      DFList[["SummaryCountAug28VotersRepVotersAgeGroup"]]$Electorate <- "Republican"
      ChartDataAgeGroupParty <- rbind(
        DFList[["SummaryCountAug28VotersDemVotersAgeGroup"]], 
        DFList[["SummaryCountAug28VotersRepVotersAgeGroup"]]
      )
      ChartDataAgeGroupParty$AgeGroup <- ifelse(
        test = is.na(ChartDataAgeGroupParty$AgeGroup),
        yes = "Unknown",
        no = ChartDataAgeGroupParty$AgeGroup
      )
      ChartDataAgeGroupParty$AgeGroup <- factor(
        x = ChartDataAgeGroupParty$AgeGroup,
        levels = rev(c("18-29","30-39","40-49","50-59","60+","Unknown"))
      )
      
      ChartAgeGroupStyle <- list()
      ChartAgeGroupStyle$HED <- list()
      ChartAgeGroupStyle$HED$x <- 5.2
      ChartAgeGroupStyle$HED$y <- -0.15
      ChartAgeGroupParty <- ggplot(
        data = ChartDataAgeGroupParty,
        aes(
          x = Electorate,
          y = PercentOfTotal
        )
      ) + 
        geom_bar(
          aes(fill=AgeGroup),
          stat = "identity",
          width = StyleRules$BarWidth
        ) +
        scale_fill_manual(
          values = rev(c(
            StyleRules$Colors$AgeGroup$X1829,
            StyleRules$Colors$AgeGroup$X3039,
            StyleRules$Colors$AgeGroup$X4049,
            StyleRules$Colors$AgeGroup$X5059,
            StyleRules$Colors$AgeGroup$X60Plus,
            StyleRules$Colors$AgeGroup$Unknown
          ))
        ) +
        scale_x_discrete(
          limits= c("Republican","Democrat"),
          labels = c(StyleRules$Labels$Party),
          expand = c(StyleRules$BarWidth*4, 0)
        ) +
        scale_y_continuous(
          labels = func.percentFormatX, # Using this function because the chart will flip
          expand = c(0,0),
          limits = c(0,StyleRules$YExpand)       
        ) +
        geom_text( # Add figures to bars
          aes(
            label = ifelse(
              test = PercentOfTotal > 0.05,
              yes = paste0(
                sprintf(fmt = "%.0f", (PercentOfTotal*100)),
                '%'
              ),
              no = ''
            ),
            group = AgeGroup
          ),
          position = position_stack(
            vjust = 0.5
          ),
          fontface = "bold",
          color = "#ffffff",
          size = 5
        ) +
        geom_text(
          label = "2018 FL primary: Elderly voters likely dominated both parties",
          inherit.aes = F,
          x = ChartAgeGroupStyle$HED$x,
          y = ChartAgeGroupStyle$HED$y,
          check_overlap = T,
          hjust = 0,
          size = 8,
          family = StyleRules$HedFont,
          fontface = "bold"
        ) +
        geom_text(
          label = "Preliminary breakdown of voter age in Florida's Democratic and Republican primaries",
          inherit.aes = F,
          x = ChartAgeGroupStyle$HED$x - 0.4,
          y = ChartAgeGroupStyle$HED$y,
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
          legend.position = c(0.2, 0.9),
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
      ChartAgeGroupParty
      ggsave(
        filename = "AgeGroups-party.png",
        plot = ChartAgeGroupParty,
        device = "png",
        path = OutputDir,
        width = 200,
        height = 130,
        units = "mm",
        dpi = 144
      )
      
      # Sex
      DFList[["SummaryCountAug28VotersDemVotersSexLabel"]]$Electorate <- "Democrat"
      DFList[["SummaryCountAug28VotersRepVotersSexLabel"]]$Electorate <- "Republican"
      ChartDataSexParty <- rbind(
        DFList[["SummaryCountAug28VotersDemVotersSexLabel"]], 
        DFList[["SummaryCountAug28VotersRepVotersSexLabel"]]
      )
      ChartDataSexParty$SexLabel <- factor(
        x = ChartDataSexParty$SexLabel,
        levels = rev(c("Female","Male","Unknown"))
      )
      
      ChartSexStyle <- list()
      ChartSexStyle$HED <- list()
      ChartSexStyle$HED$x <- 5.2
      ChartSexStyle$HED$y <- -0.15
      ChartSexParty <- ggplot(
        data = ChartDataSexParty,
        aes(
          x = Electorate,
          y = PercentOfTotal
        )
      ) + 
        geom_bar(
          aes(fill=SexLabel),
          stat = "identity",
          width = StyleRules$BarWidth
        ) +
        scale_fill_manual(
          values = rev(c(
            StyleRules$Colors$Sex$Female,
            StyleRules$Colors$Sex$Male,
            StyleRules$Colors$Sex$Unknown
          ))
        ) +
        scale_x_discrete(
          limits= c("Republican","Democrat"),
          labels = c(StyleRules$Labels$Party),
          expand = c(StyleRules$BarWidth*4, 0)
        ) +
        scale_y_continuous(
          labels = func.percentFormatX, # Using this function because the chart will flip
          expand = c(0,0),
          limits = c(0,StyleRules$YExpand)       
        ) +
        geom_text( # Add figures to bars
          aes(
            label = ifelse(
              test = PercentOfTotal > 0.05,
              yes = paste0(
                sprintf(fmt = "%.0f", (PercentOfTotal*100)),
                '%'
              ),
              no = ''
            ),
            group = SexLabel
          ),
          position = position_stack(
            vjust = 0.5
          ),
          fontface = "bold",
          color = "#ffffff",
          size = 5
        ) +
        geom_text(
          label = "Women big in 2018 Florida Dem primary",
          inherit.aes = F,
          x = ChartSexStyle$HED$x,
          y = ChartSexStyle$HED$y,
          check_overlap = T,
          hjust = 0,
          size = 8,
          family = StyleRules$HedFont,
          fontface = "bold"
        ) +
        geom_text(
          label = "Preliminary breakdown of sex in Florida's Democratic and Republican primaries",
          inherit.aes = F,
          x = ChartSexStyle$HED$x - 0.4,
          y = ChartSexStyle$HED$y,
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
          legend.position = c(0.2, 0.9),
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
      ChartSexParty
      ggsave(
        filename = "Sex-party.png",
        plot = ChartSexParty,
        device = "png",
        path = OutputDir,
        width = 200,
        height = 120,
        units = "mm",
        dpi = 144
      )