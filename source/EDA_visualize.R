####################################################
#                                                  #   
# Team-level EDA                                   #
#                                                  # 
####################################################
library(tidyverse)

WL_record_cal = function(game_schedule,WL_record){
  for(i in 1:nrow(game_schedule)){
    if (game_schedule$HomeWin[i]==1){
      WL_record$W[which(WL_record$TeamName==game_schedule$Home[i])] = 
        WL_record$W[which(WL_record$TeamName==game_schedule$Home[i])] + 1
      WL_record$L[which(WL_record$TeamName==game_schedule$Away[i])] = 
        WL_record$L[which(WL_record$TeamName==game_schedule$Away[i])] + 1
    }
    
    if (game_schedule$HomeWin[i]==0){
      WL_record$L[which(WL_record$TeamName==game_schedule$Home[i])] = 
        WL_record$L[which(WL_record$TeamName==game_schedule$Home[i])] + 1
      WL_record$W[which(WL_record$TeamName==game_schedule$Away[i])] = 
        WL_record$W[which(WL_record$TeamName==game_schedule$Away[i])] + 1
    }
  }
  return(WL_record)
}



team_def_off_plot = function(year){
  team_stats_year = get_team_year_feature(year)
  p = team_stats_year %>% 
    ggplot(aes(x = TeamPTS/20, y = OpponentPTS/20))+
    geom_point(aes(size = -ConfRank, col = factor(western)))+
    geom_text(aes(label=Tm),vjust = 0, nudge_y = 0.5)+
    geom_abline(slope = 1,intercept = 0,linetype="dashed", color = "red",size = 2)+
    facet_grid(~western)+
    labs(title = paste0('Season ',year-1,'-',substr(year,3,4)))+
    theme(plot.title = element_text(hjust = 0.5))+
    coord_fixed()+
    theme(legend.position = "bottom")
  print(p)
}
team_def_off_plot(2016)

team_rank_plot = function(year, Nfirst = 300){
  team_stats_year = get_team_year_feature(year)
  game_stats_year = get_game_year_feature(year)
  colnames(team_stats_year)[c(2,3)] = c("W.pre","L.pre")
  
  WL_record = cbind(west_ind,W = 0, L=0)
  WL_record  = WL_record_cal(game_stats_year,WL_record)
  colnames(WL_record)[c(4,5)] = c("W.all","L.all")
  WL_record = WL_record %>%
    group_by(western) %>%
    mutate(Season_Rank = order(order(W.all, decreasing = T))) %>%
    inner_join(team_stats_year[,c(1,2,3,8,9)],by=c("Tm","TeamName"))
  p = WL_record %>%
    ggplot(aes(x = ConfRank, y = Season_Rank))+
    geom_point(aes(col = factor(western)),size = 2)+
    geom_text(aes(label=Tm),vjust = 0, nudge_y = 0.5)+
    geom_abline(slope = 1,intercept = 0,linetype="dashed", color = "red",size = 2)+
    geom_hline(yintercept=8, linetype="dotted", color = "blue",size = 2)+
    facet_grid(~western)+
    labs(title = paste0('Season ',year-1,'-',substr(year,3,4)))+
    theme(plot.title = element_text(hjust = 0.5))+
    coord_fixed()+
    theme(legend.position = "bottom")
  print(p)
  
  p2 = WL_record %>%
    ggplot(aes(x = W.pre, y =W.all))+
    geom_point(aes(col = factor(western)),size = 2)+
    geom_smooth(method = "lm")+ 
    geom_text(aes(label=Tm),vjust = 0, nudge_y = 0.5)+
    facet_grid(~western)+
    labs(title = paste0('Season ',year-1,'-',substr(year,3,4)))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position = "bottom")
  print(p2)
}

team_rank_plot(2019)

####################################################
#                                                  #   
# Home effect, western effect                      #
#                                                  # 
####################################################

home_away_plot = function(year){
  game_stats_year = get_game_year_feature(year)
  standing_url = paste0("https://www.basketball-reference.com/leagues/NBA_",
                        year,"_standings.html")
  
  standing_year = standing_url %>% 
    read_html %>%
    html_nodes(xpath = '//comment()') %>%    # select comment nodes
    html_text() %>%    # extract comment text
    paste(collapse = '') %>%    # collapse to a single string
    read_html() %>%    # reparse to HTML
    html_table() %>%
    .[[1]]  
  
  colnames(standing_year) = paste0(colnames(standing_year),standing_year[1,])
  standing_year = standing_year[-1,]
  
  standing_table = standing_year %>%
    select(Team,Overall,PlaceHome,PlaceRoad,ConferenceE,ConferenceW) %>%
    separate(PlaceHome,into = c("HomeW","HomeL"),sep = "-") %>%
    separate(PlaceRoad,into = c("AwayW","AwayL"),sep = "-") %>%
    separate(ConferenceE,into = c("EastW","EastL"),sep = "-") %>%
    separate(ConferenceW,into = c("WestW","WestL"),sep = "-") %>%
    separate(Overall,into = c("Wtot","Ltot"),sep = "-") %>%
    inner_join(west_ind,by=c("Team" = "TeamName"))
  
  standing_table_gather = standing_table %>%
    gather(stats,win,-c("Team","Wtot","Ltot","Tm","western"))
  standing_table_gather[,c(2,3,7)] = as.numeric(as.matrix(standing_table_gather[,c(2,3,7)]))
  p = standing_table_gather %>% 
    filter(stats %in% c("HomeW","AwayW")) %>%
    ggplot(aes(x = reorder(Tm,Wtot), y = win,fill= stats))+
    geom_bar(stat = "identity",position = "dodge")+
    facet_grid(~western,scales="free_x")+
    labs(title = paste0('Season ',year-1,'-',substr(year,3,4)))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position = "bottom")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  print(p)
  
  team_factor_levels= standing_table_gather %>% 
    filter(western==1 & stats %in% c('EastW') | western==0 & stats %in% c('WestW')) %>%
    arrange(win) %>%
    select(Tm) %>% as.matrix() %>% as.character()
  
  p2 = standing_table_gather %>%
    mutate(Tm = factor(Tm,levels = team_factor_levels)) %>%
    filter(western==1 & stats %in% c('EastW','EastL') | western==0 & stats %in% c('WestW','WestL')) %>%
    ggplot(aes(x =Tm, y = win,fill= stats))+
    geom_bar(stat = "identity")+
    labs(title = paste0('Season ',year-1,'-',substr(year,3,4)))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position = "bottom")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  print(p2)
  
  
}

home_away_plot(2019)


####################################################
#                                                  #   
# player winshare                                  #
#                                                  # 
####################################################

team_ws_plot = function(year){
  team_stats_year = get_team_year_feature(year)
  game_stats_year = get_game_year_feature(year)
  colnames(team_stats_year)[c(2,3)] = c("W.pre","L.pre")
  
  WL_record = cbind(west_ind,W = 0, L=0)
  WL_record  = WL_record_cal(game_stats_year,WL_record)
  colnames(WL_record)[c(4,5)] = c("W.all","L.all")
  WL_record = WL_record %>%
    group_by(western) %>%
    mutate(Season_Rank = order(order(W.all, decreasing = T))) %>%
    inner_join(team_stats_year[,c(1,2,3,8,9)],by=c("Tm","TeamName"))
  
  
  WinShare = get_ws_data(year)
  WL_record_ws = WL_record %>%
    inner_join(WinShare, by='Tm')
  
  p = WL_record_ws %>%
    ggplot(aes(x = WS, y = Season_Rank))+
    geom_point(aes(col = factor(western)),size = 2)+
    geom_text(aes(label=Tm),vjust = 0, nudge_y = 0.5)+
    facet_grid(~western)+
    labs(title = paste0('Season ',year-1,'-',substr(year,3,4)))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position = "bottom")
  print(p)
}


get_ws_raw_data = function(year){
  ws_url = paste0("https://www.basketball-reference.com/play-index/psl_finder.cgi?request=1&match=single&type=totals&per_minute_base=36&per_poss_base=100&season_start=1&season_end=-1&lg_id=NBA&age_min=0&age_max=99&is_playoffs=N&height_min=0&height_max=99&year_min=",
                  year,"&year_max=",year,"&birth_country_is=Y&as_comp=gt&as_val=0&pos_is_g=Y&pos_is_gf=Y&pos_is_f=Y&pos_is_fg=Y&pos_is_fc=Y&pos_is_c=Y&pos_is_cf=Y&order_by=ws")
  ws_stats = ws_url %>% read_html %>%
    html_table() %>%
    .[[1]] 
  ws_stats = ws_stats[,c(2,3,5,7)]
  colnames(ws_stats)= ws_stats[1,]  
  ws_stats = ws_stats[-c(1,which(ws_stats$Player=='Player')),]
  ws_stats$WS = as.numeric(ws_stats$WS )
  return(ws_stats)
}




player_ws_plot = function(){
  year_list = c(2015:2019)
  ws_stats  = NULL
  for(i in 1: length(year_list)){
      ws_tmp = get_ws_raw_data(year_list[i])
      ws_stats = rbind(ws_stats, ws_tmp)
  }
  ws_stats = ws_stats %>% arrange(-WS)
  player_rank_list = unique(ws_stats$Player)
  
  ws_stats_rank = ws_stats %>%
    group_by(Season) %>%
    mutate(WSRank = rank(-WS,ties.method = "first"))
  
  p = ws_stats_rank %>%
    filter(Player %in% player_rank_list[1:20]) %>%
    ggplot(aes(x = Player, y = WSRank,fill = Player))+
    geom_boxplot()+
    #labs(title = player_rank_list[player_rank])+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position = "none")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  print(p)
}
player_ws_plot()

