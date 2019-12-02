library(rvest)

########################################
#                                      #   
# Scrape the game schedule data        #
#                                      #  
########################################

get_schedule_data = function(year, month){
  game_url = paste0("https://www.basketball-reference.com/leagues/NBA_",
                    year,"_games-",month,".html")
  game_stats = game_url %>% 
    read_html %>%
    html_table() %>%
    .[[1]]
  
  # remove playoff games for april
  if (month=="april" & year!='2020'){
    playoff_pos = which(game_stats$Date=='Playoffs')
    game_stats = game_stats[1:(playoff_pos-1),]
  }
  game_stats = data.frame(game_stats) %>%
    mutate(Season = paste0(as.numeric(year)-1,'-',substr(year,3,4)))
  return(game_stats)
}

year_list = as.character(2016:2020)
month_list = c("october","november","december",
               "january","february","march","april")


combine_game_data = function(){
  game_stats  = NULL
  for(i in 1: length(year_list)){
    for(j in 1:length(month_list)){
      game_stats_tmp = get_schedule_data(year_list[i],month_list[j])
      game_stats = rbind(game_stats, game_stats_tmp)
    }
  }
  return(game_stats)
}

game_stats = combine_game_data()


###########################################
#                                         #   
# Scrape the first 20 game team stats     #
#                                         #  
###########################################

get_team_stats = function(year_min = 2016, year_max = 2020, game_min = 1, game_max = 20){
  team_stats = NULL
  page_num = ceiling((year_max-year_min+1)*30/100)
  for(i in 1:page_num){
    team_url = paste0("https://www.basketball-reference.com/play-index/tgl_finder.cgi?request=1&match=single&lg_id=NBA&is_playoffs=N&team_seed_cmp=eq&opp_seed_cmp=eq&year_min=2016&year_max=2020&is_range=Y&game_num_type=team&game_num_min=1&game_num_max=20&order_by=date_game&order_by_asc=&offset=",
                      (i-1)*100)
    team_stats_tmp = team_url %>% 
      read_html %>%
      html_table() %>%
      .[[1]]   
    team_stats = rbind(team_stats, team_stats_tmp)
  }
  # remove extra rows with texts, and organize the column names
  newColName = paste0(colnames(team_stats),team_stats[1,])
  colnames(team_stats) = newColName
  extra_line = which(team_stats$Rk=='Rk')
  team_stats = team_stats[-extra_line,]
  return(team_stats)
}
team_stats = get_team_stats()

readr::write_csv(game_stats,"./data/game_stats.csv")
readr::write_csv(team_stats,"./data/team_stats.csv")
