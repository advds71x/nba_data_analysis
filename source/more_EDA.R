library(data.table)
library(rowr)
library(dplyr)
game_stats = readr::read_csv("./data/game_stats_all.csv")


teams <- unique(game_stats$Visitor.Neutral)
game_after_streak <- list()
game_consecutive <- list()
year <- 2019

for (team in teams) {
  ### wrangling team winning streak data
  team_game_stats <- game_stats %>% filter(grepl(as.character(year-1), Season))
  team_game_stats <- team_game_stats %>% filter(Visitor.Neutral == team | Home.Neutral == team)
  team_game_stats <- team_game_stats[complete.cases(team_game_stats[, "PTS"]),]
  # compute winning streaks
  team_game_stats$isWin <- 0
  team_game_stats[team_game_stats$Home.Neutral == team & (team_game_stats$PTS.1 > team_game_stats$PTS), "isWin"] = 1
  team_game_stats[team_game_stats$Visitor.Neutral == team & (team_game_stats$PTS.1 < team_game_stats$PTS), "isWin"] = 1
  team_game_stats$preGameWinStreak <- rowid(rleid(team_game_stats$isWin)) * team_game_stats$isWin
  na.omit(setDT(team_game_stats)[, preGameWinStreak:=c(NA, preGameWinStreak[-.N])])
  # compute losing streaks
  team_game_stats$isLose <- 1 - team_game_stats$isWin
  team_game_stats$preGameLoseStreak <- rowid(rleid(team_game_stats$isLose)) * team_game_stats$isLose
  na.omit(setDT(team_game_stats)[, preGameLoseStreak:=c(NA, preGameLoseStreak[-.N])])
  # overall streak
  team_game_stats$streak <- paste0("W", team_game_stats$preGameWinStreak)
  team_game_stats[team_game_stats$streak == "W0", "streak"] <- paste0("L", team_game_stats$preGameLoseStreak[team_game_stats$streak == "W0"])
  # game results after streaks
  game_after_streak[[team]] <- as.data.frame.matrix(table(team_game_stats$streak, team_game_stats$isWin))
  
  ### wrangling back-to-back game data
  team_game_stats$GameDate <- as.Date(substring(team_game_stats$Date, 6), "%b %d, %Y")
  team_game_stats$consecutive <- c(NA,diff(as.Date(team_game_stats$GameDate)) == 1)
  # game results after back-to-back game
  game_consecutive[[team]] <- as.data.frame.matrix(table(team_game_stats$consecutive, team_game_stats$isWin))
}



#paired t-test for the difference between conditional and marginal probability
condition_prob<-function(matrix){
  con_prob=matrix[,"1"]/(matrix[,"1"]+matrix[,"0"])
  return(con_prob)
}

marginal_prob<-function(matrix){
  colSums(matrix)[2]/(colSums(matrix)[2]+colSums(matrix)[1])
}

con<-lapply(game_after_streak,condition_prob)
mar<-lapply(game_after_streak, marginal_prob)

ite<-1
mar_rep<-NA
for (i in mar){
  mar_rep<-c(mar_rep,rep(i,length(con[[ite]])))
  ite<-ite+1
}


mar_rep<-na.omit(mar_rep)
con<-unlist(con)
t.test(mar_rep,con,paired = T)

#chi-square test for independence
tests<-lapply(game_after_streak, chisq.test)
pval<-NA
for (i in tests){
  pval<-c(pval,i$p.value)
}
pval<-na.omit(pval)
sum(pval<0.05)


# merge winning streak data of all teams
all_team_streaks <- bind_rows(game_after_streak, .id = "team")
all_team_streaks$Streak <- unlist(lapply(game_after_streak, rownames))
colnames(all_team_streaks)[2:3] <- c("lose","win")
all_team_streaks$winProb <- all_team_streaks$win/(all_team_streaks$win + all_team_streaks$lose)

# plot 
all_team_streaks_plot <- all_team_streaks  %>% filter(Streak %in% c("L3","L2","L1","W1","W2","W3"))
all_team_streaks_plot$Streak <- factor(all_team_streaks_plot$Streak,levels = c("L3","L2","L1","W1","W2","W3"))
ggplot(data=all_team_streaks_plot, aes(x=Streak, y=winProb)) +
  geom_boxplot(show.legend = F) +
  labs(title = "Team Performances After Winning or Losing Streak") +
  ylab("Winning percentage of game following a streak")
# geom_point(position=position_jitterdodge(jitter.width=5, dodge.width = 0), cex=1.5, aes(color=factor(team)), show.legend = F)


all_team_streaks_plot <- all_team_streaks  %>% filter(Streak %in% c("L3","L2","L1","W1","W2","W3"))
all_team_streaks_plot$Streak <- factor(all_team_streaks_plot$Streak,levels = c("L3","L2","L1","W1","W2","W3"))
p <- ggplot(data=all_team_streaks_plot, aes(x=Streak, y=winProb)) +
  geom_boxplot(show.legend = F) +
  labs(title = "Team Performances After Winning or Losing Streak") +
  ylab("Winning percentage of game following a streak") +
  geom_point(position=position_jitterdodge(jitter.width=5, dodge.width = 0), cex=1.5, aes(color=factor(team)), show.legend = F)
ggplotly(p)



# merge and plot back-to-back game data of all teams
all_team_consecutive <- bind_rows(game_consecutive, .id = "team")
all_team_consecutive$consecutive <- unlist(lapply(game_consecutive, rownames))
colnames(all_team_consecutive)[2:3] <- c("lose","win")
all_team_consecutive$winProb <- all_team_consecutive$win/(all_team_consecutive$win + all_team_consecutive$lose)
ggplot(data=all_team_consecutive, aes(x=consecutive, y=winProb)) +
  geom_boxplot(show.legend = F) 
# chi-square test for independence
lapply(game_consecutive, chisq.test)
pval<-NA
for (i in tests){
  pval<-c(pval,i$p.value)
}
pval<-na.omit(pval)
sum(pval<0.05)

