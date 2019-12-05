#PageRank algorithm
library(igraph)
library(tcltk)
library(dplyr)
dat<-read.csv("./data/game_stats.csv",stringsAsFactors = F)
#label conference
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



dat$raw_points<-abs(dat$PTS.1-dat$PTS)
dat$win<-ifelse(dat$PTS.1-dat$PTS>0,dat$Home.Neutral,dat$Visitor.Neutral)
dat$lose<-ifelse(dat$PTS.1-dat$PTS>0,dat$Visitor.Neutral,dat$Home.Neutral)
dat_igraph<-data.frame(A=dat$lose,B=dat$win,weight=dat$raw_points)
dat_igraph<-dat_igraph%>%group_by(A,B)%>%summarise(weight=mean(weight))
dat_graph<-graph.data.frame(d=dat_igraph,directed = TRUE)
is_weighted(dat_graph)
plot.igraph(dat_graph)
#tkplot(dat_graph)
adj_matrix<-get.adjacency(dat_graph,attr="weight")
rank <- sort(page.rank(dat_graph,directed=T)$vector,decreasing = T)

##first twenty
dat_igraph_20<-data.frame(A=dat$lose[1:300],B=dat$win[1:300],weight=dat$raw_points[1:300])
dat_igraph_20<-dat_igraph_20%>%group_by(A,B)%>%summarise(weight=mean(weight))
dat_graph_20<-graph.data.frame(d=dat_igraph_20,directed = TRUE)
is_weighted(dat_graph_20)
#tkplot(dat_graph)
adj_matrix<-get.adjacency(dat_graph_20,attr="weight")
rank <- sort(page.rank(dat_graph_20,directed=T)$vector,decreasing = T)

#plot
west_ind_sort<-west_ind[match(get.vertex.attribute(dat_graph_20,"name"), west_ind$TeamName),]
dat_graph_20<-set_vertex_attr(dat_graph_20,"western",value=west_ind_sort$western)
dat_graph_20<-set_vertex_attr(dat_graph_20,"Tm",value=as.character(west_ind_sort$Tm))
colrs<-c("gray50","tomato")
V(dat_graph_20)$color>-colrs[as.factor(V(dat_graph_20)$western)]
deg <- 2*degree(dat_graph_20, mode="in")
plot.igraph(dat_graph_20,
            edge.arrow.size=0.3,
            vertex.color=colrs[as.factor(V(dat_graph_20)$western)],
            vertex.label=V(dat_graph_20)$Tm,
            vertex.size=deg,
            edge.width=4*E(dat_graph_20)$weight/max(E(dat_graph_20)$weight))


