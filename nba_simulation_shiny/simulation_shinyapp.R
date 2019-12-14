library(shiny)
library(dplyr)
library(here)
library(DT)
library(shinyjs)
library(ggplot2)

#read in schdule and model prediction
pre_20_games<-readRDS("pre_20_games.rds")
post_62_games_pred<-readRDS("post_62_games_pred.rds")

#a function that turns a decimals into percentage display
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

brks <- seq(0.5,1,by=0.05)
r<-round(seq(250, 44, length.out = length(brks) + 1), 0)
g<-round(seq(250, 162, length.out = length(brks) + 1), 0)
b<-round(seq(250, 95, length.out = length(brks) + 1), 0)
clrs <- paste0("rgb(",r,",",g,",",b,")" )

ui<-navbarPage("NBA Simulation",
           tabPanel("Game Simulation",
                    column(1,imageOutput("home"),offset =1),
                    column(1,imageOutput("vs"),offset =3),
                    column(1,imageOutput("away"),offset=3),
                    column(1,imageOutput("win"),offset=4),
                    useShinyjs(),
                    column(2,
                           tags$button(
                             id = "web_button",
                             class = "btn action-button",
                             tags$img(
                               src = 'start_icon.jpg',
                                      height = "200px"))
                           )
                    ),
           
           tabPanel("Table Summary",
                    DTOutput("table")
                    ),
           
           tabPanel("Playoffs",
                    fluidRow(
                      br(),
                        column(1,imageOutput("first"),offset =2),
                        column(1,imageOutput("second"),offset =1),
                        column(1,imageOutput("third"),offset=1),
                        column(1,imageOutput("fourth"),offset=1)
                        ),
                    fluidRow(
                        column(1,imageOutput("fifth"),offset =2),
                        column(1,imageOutput("sixth"),offset =1),
                        column(1,imageOutput("seventh"),offset=1),
                        column(1,imageOutput("eigth"),offset=1)
                        )
                    ),
           
           tabPanel("Games",
                    dateInput("date","Date"),
                    DTOutput("dailyTB")
                    )
)




server <- function(input, output, session) {
  
  observeEvent(input$web_button,
               {
              hide("web_button")
              post_62_games_pred$win<-rbinom(nrow(post_62_games_pred),1,post_62_games_pred[,8])
               post_62_games_pred$winteam<-ifelse(post_62_games_pred$win==1,post_62_games_pred[,2],post_62_games_pred[,4])
               post_62_games_pred$loseteam<-ifelse(post_62_games_pred$win==1,post_62_games_pred[,4],post_62_games_pred[,2])
               west_ind = data.frame(
                 western = c(0,1,1,0,0,0,0,0,1,1,0,1,1,1,0,0,0,0,1,1,1,1,1,1,0,0,0,0,1,1),
                 Team = c("Chicago Bulls","Los Angeles Lakers","Portland Trail Blazers", "Washington Wizards","Charlotte Hornets",
                          "Milwaukee Bucks","Brooklyn Nets" ,"Detroit Pistons","Houston Rockets" ,"New Orleans Pelicans",
                          "Indiana Pacers","Memphis Grizzlies","Denver Nuggets","Los Angeles Clippers","Atlanta Hawks",
                          "New York Knicks" ,"Miami Heat","Orlando Magic","Dallas Mavericks","Phoenix Suns" ,
                          "Sacramento Kings","Utah Jazz", "Minnesota Timberwolves","San Antonio Spurs","Cleveland Cavaliers",
                          "Toronto Raptors" ,"Boston Celtics","Philadelphia 76ers" ,"Golden State Warriors","Oklahoma City Thunder")
               )%>%filter(western==1)%>%select(Team)%>%mutate(win=rep(0,15),lose=rep(0,15))
               
               ##calculate total win/lose 
               for (j in (1:nrow(post_62_games_pred))){
                 for (i in west_ind$Team){
                   if (post_62_games_pred[j,11]==i){
                     west_ind[west_ind$Team==i,"win"]=west_ind[west_ind$Team==i,"win"]+1
                   }
                   if (post_62_games_pred[j,12]==i){
                     west_ind[west_ind$Team==i,"lose"]=west_ind[west_ind$Team==i,"lose"]+1
                   }
                 }
               }
               
               
               #playoffs
               
               west_ind$win<-west_ind$win+pre_20_games$W
               west_ind$lose<-west_ind$lose+pre_20_games$L
               playoff<-west_ind[order(west_ind$win,decreasing =T),]$Team[1:8]

               rv <- reactiveValues(lines = character(0), n = 0, win=0,winteam=character(0),home=character(0),away=character(0))
               
               observe({
                 
                 invalidateLater(400, session)
                 
                 isolate({
                   if (rv$n >= nrow(post_62_games_pred)) {
                     return()
                   } else {
                     rv$n <- rv$n + 1
                     rv$home=post_62_games_pred[rv$n,2]
                     rv$away=post_62_games_pred[rv$n,4]
                     #rv$win<-rbinom(1,1,post_62_games_pred[rv$n,8])
                     rv$winteam<-post_62_games_pred[rv$n,11]
                     rv$loseteam<-post_62_games_pred[rv$n,12]
                     
                     
                   }
                   
                 })
                 
               })
               
               output$home<-renderImage({
                 filename <- normalizePath(file.path('./www',paste(rv$home, '.png', sep='')))
                 list(src = filename,width=200,height=200)
               },deleteFile = FALSE)
               
               output$vs<-renderImage({
                 filename <- normalizePath(file.path('./www',paste("vs", '.png', sep='')))
                 list(src = filename,width=150,height=150)
               },deleteFile = FALSE)
               
               output$away<-renderImage({
                 filename <- normalizePath(file.path('./www',paste(rv$away, '.png', sep='')))
                 list(src = filename,width=200,height=200)
               },deleteFile = FALSE)
               
               output$win<-renderImage({
                 filename <- normalizePath(file.path('./www',paste(rv$winteam, '.png', sep='')))
                 list(src = filename,width=270,height=270)
               },deleteFile = FALSE)
               
               output$table<-renderDT(
                 datatable(west_ind, options = list(pageLength = 20,dom = 't'))
                 
               )
               
               output$first<-renderImage({
                 filename <- normalizePath(file.path('./www',paste(playoff[1], '.png', sep='')))
                 list(src = filename,width=100,height=100)
               },deleteFile = FALSE)
               
               output$second<-renderImage({
                 filename <- normalizePath(file.path('./www',paste(playoff[2], '.png', sep='')))
                 list(src = filename,width=100,height=100)
               },deleteFile = FALSE)
               
               output$third<-renderImage({
                 filename <- normalizePath(file.path('./www',paste(playoff[3], '.png', sep='')))
                 list(src = filename,width=100,height=100)
               },deleteFile = FALSE)
               
               output$fourth<-renderImage({
                 filename <- normalizePath(file.path('./www',paste(playoff[4], '.png', sep='')))
                 list(src = filename,width=100,height=100)
               },deleteFile = FALSE)
               
               output$fifth<-renderImage({
                 filename <- normalizePath(file.path('./www',paste(playoff[5], '.png', sep='')))
                 list(src = filename,width=100,height=100)
               },deleteFile = FALSE)
               
               output$sixth<-renderImage({
                 filename <- normalizePath(file.path('./www',paste(playoff[6], '.png', sep='')))
                 list(src = filename,width=100,height=100)
               },deleteFile = FALSE)
               
               output$seventh<-renderImage({
                 filename <- normalizePath(file.path('./www',paste(playoff[7], '.png', sep='')))
                 list(src = filename,width=100,height=100)
               },deleteFile = FALSE)
               
               output$eigth<-renderImage({
                 filename <- normalizePath(file.path('./www',paste(playoff[8], '.png', sep='')))
                 list(src = filename,width=100,height=100)
               },deleteFile = FALSE)

               })
               
  
               
  
               output$dailyTB<-renderDT(
 
                #display daily game prediction
                datatable(data=post_62_games_pred[post_62_games_pred$Date==(as.character(format(input$date,"%a, %b %d, %Y"))),c(2,4,8)]%>%
                   mutate(Home_Win_Prob=percent(pred_win_prob,digits=0),pred_lose_prob=1-pred_win_prob,Away_Win_Prob=percent(1-pred_win_prob,digits=0))%>%
                     select(Home,Away,Home_Win_Prob,Away_Win_Prob,pred_win_prob,pred_lose_prob), options=list(
                       #searching = FALSE, 
                       pageLength = 20,
                       dom = 't',
                     columnDefs = list(list(visible=FALSE, targets=c(5,6)))
                     
                     )
                   )%>%
                   formatStyle(
                     columns = c("Home","Away"),
                     valueColumns = c("pred_win_prob","pred_lose_prob"),
                     backgroundColor = styleInterval(brks,
                                                    clrs )
                   )
                
                   
                 
               )
  

}

shinyApp(ui, server)
