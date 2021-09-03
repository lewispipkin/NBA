rm(list=ls()) #clear environment
options(warn=-1) #suppress warnings
library(stringr)
library(rvest)
library(magrittr)

#season abbreviations
seasons=c('1999-00','2000-01','2001-02','2002-03','2003-04','2004-05','2005-06','2006-07','2007-08',
          '2008-09','2009-10','2010-11','2011-12','2012-13','2013-14','2014-15','2015-16','2016-17','2017-18',
          '2018-19','2019-20','2021-21') 

seasons_df=data.frame(year=1999:2018)
seasons_df$first_season=seasons[seasons_df$year-1998]
seasons_df$second_season=seasons[seasons_df$year-1997]
seasons_df$third_season=seasons[seasons_df$year-1996] #these are the first three seasons a player was eligible to play in the NBA


all_players_drafted = data.frame() #initialize the data frame that will hold all the data
for(i in 1999:2018){ #twenty years of data
  cat(paste0('Scraping the ',i,' draft \n')) #to monitor the for loop's progress
  url <- paste0('https://www.basketball-reference.com/draft/NBA_',i,'.html')

draft <- url %>%
  read_html() %>% 
  html_nodes('table') %>%
  html_table() %>%
  extract2(1) #extract the plain text of the table

names(draft) <- as.matrix(draft[1,]) 
draft <- draft[-1,]
draft = draft[-which(draft$Rk==''|draft$Rk=='Rk'),] #quickly cleaning the data a little bit

player_extensions <- url %>% 
  read_html() %>% 
  html_nodes(xpath="//table//a") %>% 
  html_attr('href') %>% 
  as.data.frame() %>% 
  select(link=1) %>% 
  mutate(link=as.character(link)) %>% 
  filter(str_detect(link,'player')) %>% 
  pull(link) #getting a vector of player abbreviations in a uniform way

draft$link = player_extensions
draft <- draft %>% select(2,3,4,23) #only grab necessary columns
draft$year <- i

all_players_drafted <- rbind(all_players_drafted,draft) #append that year's draft table to the all_players_drafted dataset 
}

all_players_drafted <- all_players_drafted %>% left_join(seasons_df) #join to show the first three eligible seasons of each player

all_players_drafted$player_stats_url <- paste0('https://www.basketball-reference.com',all_players_drafted$link) #create URL for each player's stats
adv_headers <- c("Season", "Age", "Tm", "Lg", "Pos", "G", "MP", "PER", "TS%", 
                 "3PAr", "FTr", "ORB%", "DRB%", "TRB%", "AST%", "STL%", "BLK%", 
                 "TOV%", "USG%", "", "OWS", "DWS", "WS", "WS/48", "", "OBPM", 
                 "DBPM", "BPM", "VORP") 
#these are the headers of the table we are looking for- depending on what a player's career looked like (playoff experience, etc.),
#the table we want will be in a different position of the list created below
                                            
for(i in 1:nrow(all_players_drafted)){
  cat(paste0('Scraping ',all_players_drafted$Player[i],'\'s advanced stats - Pick #',all_players_drafted$Pk[i],' in the ',all_players_drafted$year[i],' draft\n'))
                            #monitor progress of the for loop
  
  if(is.null(all_players_drafted$player_stats_url[i] %>% 
             read_html() %>%
             html_nodes('table') %>%
             html_table() %>% unlist)){
    all_players_drafted$first_year_WS[i] <- NA
    all_players_drafted$first_three_years_WS[i] <- NA  #if the player did not play in the NBA at all, their win shares will be marked as the default NA.
                                            #eventually, this will be changed to zero; the NA is just a placeholder to be used to create a future variable
    } else {
  
  all_players_drafted$player_stats_url[i] %>% 
    read_html() %>%
    html_nodes('table') %>%
    html_table() %>% 
    extract2(which(sapply(sapply(all_players_drafted$player_stats_url[i] %>% 
                                   read_html() %>%
                                   html_nodes('table') %>%
                                   html_table(),names),function(t) all(t %in% adv_headers)))[1]) -> data #pull the advanced stats table
  all_players_drafted$first_year_WS[i] <- NA         #initialize column
  all_players_drafted$first_three_years_WS[i] <- NA  #honestly not sure why this is necessary but it did not work without these two lines
  
  all_players_drafted$first_year_WS[i] <- ifelse(is_empty(data$WS[which(data$Season==all_players_drafted$first_season[i])]),
                                                 0,
                                                 data$WS[which(data$Season==all_players_drafted$first_season[i])]) #if they did not play in their first season, mark as zero
  all_players_drafted$first_three_years_WS[i] <- sum(data$WS[which(data$Season %in% c(all_players_drafted$first_season[i],
                                   all_players_drafted$second_season[i],
                                   all_players_drafted$third_season[i]))]) #sum all win shares from their first three available seasons
    }
}

all_players_drafted$did_not_play_in_NBA <- ifelse(is.na(all_players_drafted$first_year_WS)&
                                             is.na(all_players_drafted$first_three_years_WS),1,0)
all_players_drafted[is.na(all_players_drafted)] <- 0 #change NA to zero now that 'did not play' is established
all_players_drafted$Pk <- as.numeric(all_players_drafted$Pk)


summary = all_players_drafted %>% group_by(Pk) %>% summarize(avg_first_year_WS=mean(first_year_WS),
                                                   cv_first_year_WS=sd(first_year_WS)/mean(first_year_WS), #computing mean and CV for both time frames
                                                   avg_first_three_years_WS=mean(first_three_years_WS),
                                                   cv_first_three_years_WS=sd(first_three_years_WS)/mean(first_three_years_WS))

#what picks pack the most punch in the first year?
summary %>% arrange(desc(avg_first_year_WS)) %>% head(10) %>% select(1,2)

#what picks are the surest thing the first year?
summary %>% arrange(desc(cv_first_year_WS)) %>% head(10) %>% select(1:3)

#what picks pack the most punch for the first three years?
summary %>% arrange(desc(avg_first_three_years_WS)) %>% head(10) %>% select(1,4)

#what picks are the surest thing for the first three years?
summary %>% arrange(desc(cv_first_year_WS)) %>% head(10) %>% select(1,4,5)

