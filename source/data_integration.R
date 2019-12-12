#######################################################
#                                                     #   
# Integrative Prediction using multiple seasons data  #
#                                                     # 
#######################################################

## combine season data
combine_season_data = function(){
  year_list = c(2016:2020)
  team_stats = NULL
  for(i in 1:length(year_list)){
    year = year_list[i]
    team_stats_year = get_team_year_feature(year) 
    team_stats_year = team_stats_year %>%
      mutate(Season = paste0(year-1,'-',substr(year,3,4)),
             year = year)
    team_stats = rbind(team_stats,team_stats_year)
  }
  return(team_stats)
}
team_stats_all = combine_season_data()

## team pc plot
team_pc_plot = function(){
  #team_stats_all = combine_season_data()
  team_stats_X = team_stats_all %>%
    select(-Tm, - TeamName, - Season, - year)
  team_stats_pc = prcomp(team_stats_X,scale. = T)$x[,1:2] # select first two PCs
  
  rownames(team_stats_X) = paste(team_stats_all$Tm,team_stats_all$Season)
  
  p1.pca = fviz_pca_ind( prcomp(team_stats_X,scale. = T),
                        #label = "none", # hide individual labels
                        habillage = team_stats_all$year, # color by groups
                        addEllipses = TRUE # Concentration ellipses
  )
  print(p1.pca)
  
  team_stats_pc = data.frame(team_stats_pc,team_stats_all[,c('Tm', 'TeamName','Season','western','year')])
  
 
  
  league_summary = team_stats_pc %>% 
    group_by(Season,year) %>%
    summarise(PC1_med = median(PC1),
              PC2_med = median(PC2)) %>%
    as.data.frame()
  rownames(league_summary) = league_summary$Season
  
  
 
  
  p1 = # team_stats_pc %>%
    ggplot(data = team_stats_pc, aes(x= PC1, y= PC2,col = factor(year)))+
    geom_point()+
    geom_text(aes(label=paste(Tm,Season)),size = 2, vjust = 0, nudge_y = 0.1)+
    labs(title = 'PC plots for NBA teams from 2015-16 to 2019-20')+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position = "bottom")+
    geom_point(aes(x=PC1_med,y=PC2_med),data = league_summary, size = 5, shape = 7)
  print(p1)
  
  
  p2 = ggplot(aes(x=PC1_med,y=PC2_med,col=year),data = league_summary)+
    geom_point()
  #print(p2)
  year_dist = dist(league_summary[,-1],diag = T,upper = T)
  team_dist = get_dist(team_stats_X)
  p3 = fviz_dist(team_dist, 
                 order=F,gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
  print(p3)
} 
team_pc_plot()

## kmeans clustering plot
library(factoextra)
team_cluster_plot = function(){
  team_stats_X = team_stats_all %>%
    select(-Tm, - TeamName, - Season, - year) %>%
    as.data.frame()
  rownames(team_stats_X) = paste(team_stats_all$Tm,team_stats_all$Season)
  k5 = kmeans(scale(team_stats_X), centers = 5, nstart = 15)
  p = fviz_cluster(k5, data = team_stats_X)
  print(p)
}
team_cluster_plot()





## end-to-end prediction function
win_prob_prediction = function(train_year = c(2016,2017,2018), test_year = 2019, firstN = 300,
                               year_effect = 1){
  ## ignore year effect
  if (year_effect == 0){
    train_set = NULL
    for(i in 1:length(train_year)){
      train_set_tmp = get_train_test_data(train_year[i],firstN)
      train_set = rbind(train_set, train_set_tmp)
    }
    test_set = get_train_test_data(test_year,firstN)
    
    ## logit model
    logit_model = glm(HomeWin~ team_per_diff,family=binomial(link='logit'),
                      data = train_set)
    print(summary(logit_model)) # model summary
    
    ## training accuracy
    print("training accuracy")
    print(confusionMatrix(factor(ifelse(logit_model$fitted.values>0.5,1,0)),
                          factor(train_set$HomeWin)))
    
    ## testing accuracy
    pred_win_prob = predict(logit_model,newdata = test_set, type = "response")
    pred_game = ifelse(pred_win_prob>0.5,1,0)
    
    test_set_pred = cbind(test_set,pred_win_prob,pred_game)
    print("testing accuracy")
    print(confusionMatrix(factor(test_set_pred$pred_game),factor(test_set_pred$HomeWin)))
    return(test_set_pred)
  }
  
  # consider year effect
  if (year_effect == 1){
    test_set = get_train_test_data(test_year,firstN)
    test_set_pred = test_set
    
    for(i in 1:length(train_year)){
      train_set= get_train_test_data(train_year[i],firstN)
      
      
      ## logit model
      logit_model = glm(HomeWin~ team_per_diff,family=binomial(link='logit'),
                        data = train_set)
      # print(summary(logit_model)) # model summary
      
      ## training accuracy
      # print("training accuracy")
      # print(confusionMatrix(factor(ifelse(logit_model$fitted.values>0.5,1,0)),
      #                      factor(train_set$HomeWin)))
      
      ## testing accuracy
      pred_win_prob = predict(logit_model,newdata = test_set, type = "response")
      pred_game = ifelse(pred_win_prob>0.5,1,0)
      test_set_pred = data.frame(test_set_pred,pred_win_prob,pred_game)
    }
    return(test_set_pred)
  }
}

win_prob_pred19_all = win_prob_prediction()
win_prob_pred18_all = win_prob_prediction(train_year = c(2016,2017),test_year = 2018,year_effect = 0)

win_prob_pred19_one = win_prob_prediction(train_year = 2018,year_effect = 0)
win_prob_pred19 = build_logit_model_year(train_year = 2019,test_year = 2019)
win_prob_pred19_weight = win_prob_prediction()



weighted_game_prediction = function(game){
  year = length(game)/2
  pred_game_vote = ifelse(sum(game[seq(2,2*year,by = 2)]) >= year/2,1,0)
  year_weight = (year:1)/sum(year:1)
  pred_win_prob_weight = sum(game[seq(1,2*year,by = 2)] * year_weight)
  pred_result = c(pred_game_vote,pred_win_prob_weight)
  return(pred_result)
}
a = data.frame(t(apply(win_prob_pred19_weight[,8:13],1,weighted_game_prediction)))
print(confusionMatrix(factor(a$X1),factor(win_prob_pred19_weight$HomeWin)))
print(confusionMatrix(factor(win_prob_pred19_all$pred_game),factor(win_prob_pred19_all$HomeWin)))

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

