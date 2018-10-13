source("import-vote-data.r")
 
# GET AUG 28 2018 ELECTION VOTER HISTORY
  Aug28VoterHistory <- filter(
    .data = DFVoterHistory,
    `Election Date`=="08/28/2018"
  )
  gc()
  write.csv(
    x = Aug28VoterHistory,
    file = paste0(OutputDir,"Aug28VoterHistory.csv"),
    row.names = F
  )
  
  
  
  