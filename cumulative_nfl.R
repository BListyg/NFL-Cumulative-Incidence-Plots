nfl_data = read.csv("updated_nfl_data20180113.csv", header = T, stringsAsFactors = FALSE)

colnames(nfl_data) = gsub("nfl_data.","",colnames(nfl_data))

plot.nfl = function(team,date){
  
  library(reshape2)
  library(survminer)
  library(cmprsk)
  
  
  
  #Creates a subset of cleaned/processed NFL data with team who had possession & the date they played
  subset = nfl_data[which(as.character(nfl_data$posteam)==team & 
                          as.character(nfl_data$ï..Date) == date),]
  
  if(nrow(subset)<2){return("Team did not play on selected date.")}
  
  else if(nrow(subset)>2){
  
  #Creates a cumulative incidence fit based on a) when the play occured, b) the type of play, and c) the team playing (if multiple teams then multiple plots are made)
  fit <- cuminc(
    ftime = subset$TimeSecs,
    group = subset$posteam,
    fstatus = subset$PlayType)
  
  #Returns GGPLOT of the Cumulative incidence function graph
  return(ggcompetingrisks(fit) 
         + geom_line(size=2)
         + theme(axis.text.x = element_text(size = 13))
         + labs(title = paste(subset$AwayTeam[1],"@",subset$HomeTeam[1], "on", subset$ï..Date[1]))
  )
  #If team/date combo doesn't exist, then R will crash (at least on my computer)
}
}

plot.nfl(team = "ATL", date = "2016-12-24")

