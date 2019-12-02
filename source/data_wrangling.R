## data wrangling
## WARNINGS: the specific year data refers to season (year-1):year

west_ind = data.frame(
  Tm = c("CHI","LAL","POR","WAS","CHO","MIL","BRK","DET","HOU","NOP","IND","MEM","DEN","LAC","ATL","NYK","MIA","ORL","DAL","PHO",
         "SAC","UTA","MIN","SAS","CLE","TOR","BOS","PHI","GSW","OKC"),
  western = c(0,1,1,0,0,0,0,0,1,1,0,1,1,1,0,0,0,0,1,1,1,1,1,1,0,0,0,0,1,1),
  TeamName = c("Chicago Bulls","Los Angeles Lakers","Portland Trail Blazers", "Washington Wizards","Charlotte Hornets",
               "Milwaukee Bucks","Brooklyn Nets" ,"Detroit Pistons","Houston Rockets" ,"New Orleans Pelicans",
               "Indiana Pacers","Memphis Grizzlies","Denver Nuggets","Los Angeles Clippers","Atlanta Hawks",
               "New York Knicks" ,"Miami Heat","Orlando Magic","Dallas Mavericks","Phoenix Suns" ,
               "Sacramento Kings","Utah Jazz", "Minnesota Timberwolves","San Antonio Spurs","Cleveland Cavaliers",
               "Toronto Raptors" ,"Boston Celtics","Philadelphia 76ers" ,"Golden State Warriors","Oklahoma City Thunder")
)

get_ws_data = function(year){
  ws_url = paste0("https://www.basketball-reference.com/play-index/psl_finder.cgi?request=1&match=single&type=totals&per_minute_base=36&per_poss_base=100&season_start=1&season_end=-1&lg_id=NBA&age_min=0&age_max=99&is_playoffs=N&height_min=0&height_max=99&year_min=",
                  year,"&year_max=",year,"&birth_country_is=Y&as_comp=gt&as_val=0&pos_is_g=Y&pos_is_gf=Y&pos_is_f=Y&pos_is_fg=Y&pos_is_fc=Y&pos_is_c=Y&pos_is_cf=Y&order_by=ws")
  ws_stats = ws_url %>% read_html %>%
    html_table() %>%
    .[[1]] 
  ws_stats = ws_stats[,c(2,5,7)]
  colnames(ws_stats)= ws_stats[1,]  
  ws_stats = ws_stats[-c(1,which(ws_stats$Player=='Player')),]
  ws_stats$WS = as.numeric(ws_stats$WS )
  ws_stats_summary = ws_stats %>%
    group_by(Tm) %>%
    summarise(WS = max(WS))
  return(ws_stats_summary)
}
WinShare = get_ws_data(2018)


get_team_year_feature = function(year){
  
  #team_stats = get_team_stats() # get the team stats for all seasons
  team_stats_year = team_stats %>%
    filter(grepl(as.character(year-1),Season)) # filter the specific year 
  team_stats_year[,-c(2,3)] = as.numeric(as.matrix(team_stats_year[,-c(2,3)])) # turn characters into numeric
  
  
  ## select Win, Loss, Win-Loss per, Team points, Opponent points
  team_stats_year_feature = team_stats_year %>%
    select(Tm, W, L, `W/L%`, TeamPTS, OpponentPTS) %>%
    inner_join(west_ind, by = 'Tm')
  
  ## assign the team rank
  team_stats_year_rank = team_stats_year_feature %>%
    group_by(western) %>%
    mutate(ConfRank = order(order(W, TeamPTS, decreasing = T)))
  
  ## add the winshare feature from previous season
  Winshare  = get_ws_data(year-1)
  team_stats_year_rank = team_stats_year_rank %>% left_join(WinShare, by = 'Tm')
  team_stats_year_rank$WS[is.na(team_stats_year_rank$WS)] = 0
  
  return(team_stats_year_rank)
  
}

team_stats_18to19 = get_team_year_feature(2019)


get_game_year_feature = function(year){
  game_stats_year = game_stats %>% 
    filter(grepl(as.character(year-1),Season))
  ## game stats 
  ## the results of every game in the season 
  game_stats_feature = game_stats_year[,c(1,5,6,3,4)] 
  game_stats_feature[,c(3,5)] = as.numeric(as.matrix(game_stats_feature[,c(3,5)]))
  colnames(game_stats_feature) = c("Date","Home","HomePTS","Away","AwayPTS")
  game_stats_feature = game_stats_feature %>%
    mutate(HomeWin = ifelse(HomePTS > AwayPTS,1,0))
  return(game_stats_feature)
}
game_stats_18to19 = get_game_year_feature(2019)