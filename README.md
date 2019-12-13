# Predicting NBA standings  - Who will make the 2019-2020 NBA playoffs in the Western Conference?


## Overview
This is the final project for Advanced Data Science @ JHU. This project built a prediction model for winning percentage of the opposing teams in a basketball game based on the team-level and player-level statistics. After simulating the rest games in the 2019-2020 season, we present the playoffs in the Western Conference. 


## Data
The `data` folder contains our training and testing data scraped from BASKETBALL REFERENCE 
* `game_stats_all.csv` - the team statistics of the first 20 games from season 2015-16 to season 2019-20. 
* `team_stats_all.csv` - the results (W-L record and scores) for each of the remaining 62 games from season 2015-16 to 2018-19 
* `pre_20_games.rds` - the results for the first 20 games for each team during season 2019-20
* `post_62_games_pred.rds` - predictions for the rest of season 2019-2020

## Exploratory Data Analysis
The `plot` folder contains plots from our EDA. 
* `back2back.png` - boxplot: the proportions of games won stratified by whether it is a back-to-back game
* `home_away.png` - histagram: the total wins of each team stratified by home/away and conference during season 2018-2019
* `player_ws_plot.png` - boxplot: the players' win share rankings over season 2014-15 to season 2018-19 
* `team_cluster_plot.png` - cluster: five clusters of the teams from 2015-16 to 2019-20 using k-means algorithm
* `team_def_off_plot.png` - scatter plot: points scored of each team vs opponent in the first 20 games during season 2018-19
* `team_pc_plot.png` - principle component: two PCs of all the teams from 2015-16 to 2019-20
* `team_rank_plot.png` - scatter plot: final conference standing vs conference standing after 20 games during season 2018-19
* `team_win_plot.png` - scatter plot: final total wins vs total wins after 20 games during season 2018-19
* `team_ws_plot.png` - line segment: win shares of of the best players in each team 
* `west_east.png` - histogram: win/lose when the teams face opposing conference
* `year_dist.png` - heatmap: the distance between every pair of seasons 

## Source code
The `source` folder contains source code of the functions used in the analysis 
* `data_scraping.R` - functions to scrape team/player statistics, game results, injury information, and game schedule
* `data_wrangling.R` - functions to tidy data and extract information from raw data
* `EDA_visualize.R` - functions to generate the plots in the analysis 
* `data_integration.R` - functions to explore the yearly effect
* `feature_eng.R` - functions to create team features
* `model_build.R` - functions to build and test the prediction model 
* `simulation.R` - based on the predicted probability, simulate the games for B times


## Shinyapp

The [Shinyapp](https://kate-yueyi-li.shinyapps.io/nba_simulation_shiny/) presents one simulation based on the predicted probabilities from our model. User could see the the final simulation results as well as daily game predictions. 


## To get a quick overview of the project, checkout our [website](https://advds71x.github.io/NBAproj/) and [Youtube video](https://www.youtube.com/watch?v=nkDTnJeeuJs&feature=youtu.be). For the full analysis, read our [final report](https://advds71x.github.io/nba_data_analysis/description). 

## Contributors
Kate Li (yli324@jhu.edu) 

Runzhe Li (rli51@jhmi.edu)

Yifan Zhang (yzhan170@jhu.edu)

Linda Zhou (lzhou54@jhu.edu)

