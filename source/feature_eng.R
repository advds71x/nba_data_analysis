####################################################
#                                                  #   
# Player Injury Data                               #
#                                                  # 
####################################################

library(rvest)
library(tidyverse)
get_injury_data = function(year){
  injury_url = paste0("https://www.spotrac.com/nba/injured-reserve/",
                    year-1)
  injury_stats = injury_url %>% 
    read_html %>%
    html_table() %>%
    .[[4]]
  
  colnames(injury_stats)[1] = "Player"
  
  injury_stats = injury_stats %>%
    separate(Player,sep="\n",into = c("Player","other")) %>%
    mutate(Games = as.numeric(Games),
           Salary = as.numeric(gsub('[$,]', '', injury_stats$`Cash EarnedWhile Injured`))) %>%
    select(Player,Team,Games,Salary) %>%
    filter(Games!='NA') %>%
    mutate(Season = paste0(year-1,'-',substr(year,3,4)))
  return(injury_stats)
}

injury_2020 = get_injury_data(2016)

combine_injury_data = function(){
  year_list = c(2017:2020)
  injury_stats = NULL
  for(i in 1:length(year_list)){
    injury_stats_tmp = get_injury_data(year_list[i])
    injury_stats = rbind(injury_stats, injury_stats_tmp)
  }
  return(injury_stats)
}


injury_stats = combine_injury_data()



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
    group_by(Tm,Player) %>%
    summarise(WS_max = max(WS), WS_num = n()) 
  
  return(ws_stats_summary)
}
Winshare = get_ws_data(2018)

# last year's winshare, this year's injury and transfer 
get_ws_injury_trans_data = function(year){
  Winshare_last_year = get_ws_data(year-1)
  Winshare_this_year = get_ws_data(year)
  injury_year = get_injury_data(year)
  
  winshare_injury_ind = Winshare_this_year %>%
    left_join(injury_year, by = c("Player","Tm"="Team")) %>%
    mutate(injury_ind = ifelse(is.na(Season),0,1))
  winshare_this_year_injury_trans = winshare_injury_ind %>%
    right_join(Winshare_last_year, by = c("Player")) %>%
    select(Tm.x,Player,Tm.y,WS_max.y,WS_num.y) %>% 
    ungroup() %>% na.omit() %>%
    group_by(Tm.x) %>%
    summarise(WS_max = max(WS_max.y), WS_num = n(),Player = Player[which.max(WS_max.y)]) 
  return(winshare_this_year_injury_trans)

}

a = get_ws_injury_trans_data(2020)
