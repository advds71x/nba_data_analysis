
####################################################
#                                                  #   
# Model fitting based on 2017-2018 season data     #
# test on 2018-2019 data                           #
# train_set_teamper is for model training          #
#                                                  #
####################################################

## train the model using train_year, test the model on test_year, use firstN games to capture the feature
get_train_test_data = function(year, firstN){
  team_stats_year = get_team_year_feature(year)
  game_stats_year = get_game_year_feature(year)
  
  train_set = game_stats_year[-c(1:firstN),]
  team_per_diff = data.frame(team_stats_year[match(train_set$Home,team_stats_year$TeamName),c(2,5,6,7,9,10)]
                             - team_stats_year[match(train_set$Away,team_stats_year$TeamName),c(2,5,6,7,9,10)])
  train_set_teamper = data.frame(train_set,
                                 team_per_diff = I(as.matrix(team_per_diff)))
  
  return(train_set_teamper)
}



build_logit_model_year = function(train_year = 2018, test_year = 2019, firstN = 300){
  
  train_set = get_train_test_data(train_year,firstN)
  test_set = get_train_test_data(test_year,firstN)
  
  ## logit model
  logit_model = glm(HomeWin~ team_per_diff,family=binomial(link='logit'),
                    data = train_set)
  print(summary(logit_model)) # model summary
  
  ## training accuracy
  
  print(confusionMatrix(factor(ifelse(logit_model$fitted.values>0.5,1,0)),
                               factor(train_set$HomeWin)))
  
  ## testing accuracy
  pred_win_prob = predict(logit_model,newdata = test_set, type = "response")
  pred_game = ifelse(pred_win_prob>0.5,1,0)
  
  test_set_pred = cbind(test_set,pred_win_prob,pred_game)
  print(confusionMatrix(factor(test_set_pred$pred_game),factor(test_set_pred$HomeWin)))
  return(test_set_pred)
}

test_set_pred = build_logit_model_year()
