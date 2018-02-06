#Reading in data in this repo
nfl_data = read.csv("updated_nfl_data20180113.csv", header = T, stringsAsFactors = FALSE)

#Cleaning column name text, columns have "nfl_data." before the variable names and I have to get rid of it for the function to run
colnames(nfl_data) = gsub("nfl_data.","",colnames(nfl_data))

#Initializing function
#Takes an NFL teams 3 word abbreviation 
#and a data they played
plot.nfl = function(team,date){
  
  library(reshape2) #Helps shape data apporpriately 
  library(survminer) #Biostats package for plotting cumulative incidence model objects
  library(cmprsk) #the package that does the plotting
  
  
  
  #Creates a subset of cleaned/processed NFL data with team who had possession & the date they played
  subset = nfl_data[which(as.character(nfl_data$posteam)==team & 
                          as.character(nfl_data$ï..Date) == date),]
  
  #If you enter a team / date combo that doesn't exist, this is returned
  #I've had R crash when this wasn't included
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

#Example of what plot looks like
#RiseUp
#GoFalcons
plot.nfl(team = "ATL", date = "2016-12-24")

