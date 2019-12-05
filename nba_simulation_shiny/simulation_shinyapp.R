library(shiny)
library(dplyr)
library(here)
library(DT)

#path=getwd()
#test_set_pred<-readRDS(paste0(path,"/data/test_set_pred.rds"))
test_set_pred<-readRDS(("../data/test_set_pred.rds"))
test_set_pred$win<-rbinom(nrow(test_set_pred),1,test_set_pred[,8])
test_set_pred$winteam<-ifelse(test_set_pred$win==1,test_set_pred[,2],test_set_pred[,4])
test_set_pred$loseteam<-ifelse(test_set_pred$win==1,test_set_pred[,4],test_set_pred[,2])
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
for (j in (1:nrow(test_set_pred))){
  for (i in west_ind$Team){
    if (test_set_pred[j,11]==i){
      west_ind[west_ind$Team==i,"win"]=west_ind[west_ind$Team==i,"win"]+1
    }
    if (test_set_pred[j,12]==i){
      west_ind[west_ind$Team==i,"lose"]=west_ind[west_ind$Team==i,"lose"]+1
    }
  }
}

#playoffs
playoff<-west_ind[order(west_ind$win,decreasing =T),]$Team[1:8]




ui<-navbarPage("NBA Simulation",
           tabPanel("Game Simulation",
                    column(1,imageOutput("home"),offset =0),
                    column(1,imageOutput("vs"),offset =3),
                    column(1,imageOutput("away"),offset=3),
                    column(1,imageOutput("win"),offset=3),
                    ),
           tabPanel("Table Summary",
                    DTOutput("table")
                    ),
           tabPanel("Play Offs",
                    
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
              
                    
                    
                    )
)




server <- function(input, output, session) {
  
  rv <- reactiveValues(lines = character(0), n = 0, win=0,winteam=character(0),home=character(0),away=character(0))
  
  observe({
    
    invalidateLater(100, session)
    
    isolate({
      if (rv$n >= 600) {
        return()
      } else {
        rv$n <- rv$n + 1
        rv$home=test_set_pred[rv$n,2]
        rv$away=test_set_pred[rv$n,4]
        #rv$win<-rbinom(1,1,test_set_pred[rv$n,8])
        rv$winteam<-test_set_pred[rv$n,11]
        rv$loseteam<-test_set_pred[rv$n,12]

        
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
    list(src = filename,width=350,height=350)
  },deleteFile = FALSE)
  
  output$table<-renderDT(
    west_ind
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
  

}

shinyApp(ui, server)
