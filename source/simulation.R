####################################################
#                                                  #   
# Simulation based on Predicted Results            #
#                                                  # 
####################################################

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

## Remaining 60 games
set.seed(2019)
B = 50
game_simulation = function(pred_result, B, year, Nfirst = 300, IsWest = 1){
  game_stats_year = get_game_year_feature(year)
  pretrain_set = game_stats_year[1:Nfirst,]
  train_set = game_stats_year[-c(1:Nfirst),]
  WL_record = cbind(west_ind,W = 0, L=0)
  
  WL_record_pre  = WL_record_cal(pretrain_set,WL_record)
  WL_record_post = WL_record_cal(train_set,WL_record)
  WL_record_all  = WL_record_cal(train_set,WL_record_pre)
  WL_record_west = WL_record_all %>%
    filter(western==1) %>%
    arrange(-W)
  
  
  WL_simu_rep = NULL
  for(k in 1:B){
    simu_game = rep(0,nrow(pred_result))
    
    for(i in 1:nrow(pred_result)){
      simu_game[i] = rbinom(1,1, pred_result$pred_win_prob[i])
    }
    simu_result = pred_result %>%
      select(Home,Away,HomeWin) %>%
      mutate(HomeWin = simu_game)
    
    # WL_record = cbind(west_ind,W = 0, L= 0)
    WL_simu = WL_record_cal(simu_result,WL_record_pre)
    WL_simu_west = WL_simu %>%
      filter(western==IsWest) %>%
      mutate(TeamRank = rank(-W,ties.method = "random")) %>%
      mutate(playoff = ifelse(TeamRank<=8,1,0))
    WL_simu_rep = rbind(WL_simu_rep, WL_simu_west)
  }
  ans = list(simu = WL_simu_rep, result = WL_record_west)
  return(ans)
}
simu_result = game_simulation(win_prob_pred19_all, B, year = 2019)
train_set_simu = simu_result$simu
train_set_summary = train_set_simu %>%
  group_by(Tm) %>%
  summarise(W_avg = mean(W),
            L_avg = 82 - W_avg,
            W_med = median(W),
            L_med = 82 - W_med,
            playoff_prob = sum(playoff)/B) %>%
  arrange(-W_avg)

WL_record_west = simu_result$result
