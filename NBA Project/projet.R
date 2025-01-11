

### Librairy ###

library(readxl)
library(readr)
library(dplyr)
library(plsgenomics)
library(mlbench)
library(e1071)
library(klaR)
library(MASS)
library(class)
library(glmnet)
library(ROCR)
library(randomForest)
library(readr)
library(ggplot2)
library(scales)
library(plotly)
library(lubridate)
library(tidyr)
library(gridExtra)
library(FactoMineR)
library(stringr)

library(forecast)
library(tseries)
library(MASS)
library(cowplot)
library(dendextend)
library(pscl)

# NBA Salary Team

NBA_Salary_History <- read_excel("NBA_Salary_History.xlsx", 
                                 sheet = "Team Salaries")

# NBA Salary Player

NBA_Salary_History_Players <- read_excel("NBA_Salary_History.xlsx", 
                            sheet = "Player Salaries")

# Importations du jeu de données des MVP

MVP <- read_excel("NBA Finals and MVP.xlsx")

# Season 

seasons <- read_csv("all_seasons.csv")

# Team

games <- read_csv("nba_data_gen/games.csv")
teams <- read_csv("nba_data_gen/teams.csv")

# Stats players

players<- read.csv("nba_data/Players.csv")
season_stats<- read.csv("nba_data/Seasons_Stats.csv")


# Team names update

initiale_to_name<-function(team){

  ind<-which(team=="IND")
  team[ind]<-"Indiana Pacers"
  
  ind<-which(team=="TOR")
  team[ind]<-"Toronto Raptors"
  
  ind<-which(team=="NJN")
  team[ind]<-"Brooklyn Nets"
  
  ind<-which(team=="POR")
  team[ind]<-"Portland Trail Blazers"
  
  ind<-which(team=="MIL")
  team[ind]<-"Milwaukee Bucks"
  
  ind<-which(team=="GSW")
  team[ind]<-"Golden State Warriors" 
  
  ind<-which(team=="CLE")
  team[ind]<-"Cleveland Cavaliers" 
  
  ind<-which(team=="MIN")
  team[ind]<-"Minnesota Timberwolves"
  
  ind<-which(team=="DET")
  team[ind]<-"Detroit Pistons"
  
  ind<-which(team=="SAS")
  team[ind]<-"San Antonio Spurs"
  
  ind<-which(team=="VAN")
  team[ind]<-"Memphis Grizzlies"
  
  ind<-which(team=="CHI")
  team[ind]<-"Chicago Bulls"
  
  ind<-which(team=="HOU")
  team[ind]<-"Houston Rockets"
  
  ind<-which(team=="NYK")
  team[ind]<-"New York Knicks"
  
  ind<-which(team=="UTA")
  team[ind]<-"Utah Jazz"
  
  ind<-which(team=="WAS")
  team[ind]<-"Washington Wizards"
  
  ind<-which(team=="LAL")
  team[ind]<-"Los Angeles Lakers"
  
  ind<-which(team=="PHI")
  team[ind]<-"Philadelphia 76ers"
  
  ind<-which(team=="SEA")
  team[ind]<-"Oklahoma City Thunder"
  
  ind<-which(team=="DAL")
  team[ind]<-"Dallas Mavericks"
  
  ind<-which(team=="DEN")
  team[ind]<-"Denver Nuggets"
  
  ind<-which(team=="LAC")
  team[ind]<-"Los Angeles Clippers"
  
  ind<-which(team=="MIA")
  team[ind]<-"Miami Heat"
  
  ind<-which(team=="ATL")
  team[ind]<-"Atlanta Hawks"
  
  ind<-which(team=="BOS")
  team[ind]<-"Boston Celtics"
  
  ind<-which(team=="SAC")
  team[ind]<-"Sacramento Kings"
  
  ind<-which(team=="ORL")
  team[ind]<-"Orlando Magic"
  
  ind<-which(team=="PHX")
  team[ind]<-"Phoenix Suns"
  
  ind<-which(team=="CHH")
  team[ind]<-"Charlotte Hornets"
  
  ind<-which(team=="MEM")
  team[ind]<-"Memphis Grizzlies"
  
  ind<-which(team=="NOH")
  team[ind]<-"New Orleans Pelicans"
  
  ind<-which(team=="CHA")
  team[ind]<-"Charlotte Hornets"
  
  ind<-which(team=="NOK")
  team[ind]<-"New Orleans Pelicans"
  
  ind<-which(team=="OKC")
  team[ind]<-"Oklahoma City Thunder"
  
  ind<-which(team=="BKN")
  team[ind]<-"Brooklyn Nets"
  
  ind<-which(team=="NOP")
  team[ind]<-"New Orleans Pelicans"

  return(team)
}

re_team<-function(team){
  
  ind<-which(team=="New Orleans Hornets")
  team[ind]<-"New Orleans Pelicans"
  
  ind<-which(team=="Charlotte Bobcats")
  team[ind]<-"Charlotte Hornets"
  
  ind<-which(team=="New Jersey Nets")
  team[ind]<-"Brooklyn Nets"
  
  ind<-which(team=="Washington Bullets")
  team[ind]<-"Washington Wizards"
  
  ind<-which(team=="Vancouver Grizzlies")
  team[ind]<-"Memphis Grizzlies"
  
  ind<-which(team=="Seattle SuperSonics")
  team[ind]<-"Oklahoma City Thunder"
  
  ind<-which(team=="New Orleans/Oklahoma City Hornets")
  team[ind]<-"New Orleans Pelicans"
  
  ind<-which(team=="Kansas City Kings")
  team[ind]<-"Sacramento Kings"
  
  ind<-which(team=="New Orleans Jazz")
  team[ind]<-"Utah Jazz"
  
  ind<-which(team=="Buffalo Braves")
  team[ind]<-"Los Angeles Clippers"
  
  ind<-which(team=="New York Nets")
  team[ind]<-"Brooklyn Nets"
  
  ind<-which(team=="San Diego Clippers")
  team[ind]<-"Los Angeles Clippers"
  
  ind<-which(team=="Baltimore Bullets")
  team[ind]<-"Washington Wizards"
  
  ind<-which(team=="Fort Wayne Pistons")
  team[ind]<-"Detroit Pistons"
  
  ind<-which(team=="St. Louis Hawks")
  team[ind]<-"Atlanta Hawks"
  
  ind<-which(team=="San Francisco Warriors")
  team[ind]<-"Golden State Warriors"
  
  ind<-which(team=="Syracuse Nationals")
  team[ind]<-"Philadelphia 76ers"
  
  ind<-which(team=="Rochester Royals")
  team[ind]<-"Sacramento Kings"
  
  ind<-which(team=="Philadelphia Warriors")
  team[ind]<-"Golden State Warriors"
  
  ind<-which(team=="Minneapolis Lakers")
  team[ind]<-"Los Angeles Lakers"
  
  return(team)
  
}

initiale<-function(team){
  
  indice<-which(team=="NJN")
  team[indice]<-"BKN"
  
  indice<-which(team=="SEA")
  team[indice]<-"OKC"
  
  indice<-which(team=="VAN")
  team[indice]<-"MEM"
  
  indice<-which(team=="CHH")
  team[indice]<-"CHA"
  
  indice<-which(team=="NOK")
  team[indice]<-"NOP"
  
  indice<-which(team=="NOH")
  team[indice]<-"NOP"
  
  indice<-which(team=="WSB")
  team[indice]<-"WAS"
  
  indice<-which(team=="CHO")
  team[indice]<-"CHA"
  
  indice<-which(team=="PHO")
  team[indice]<-"PHX"
  
  indice<-which(team=="TOT")
  team[indice]<-"TOR"
  
  indice<-which(team=="BRK")
  team[indice]<-"BKN"
  
  return(team)
}


NBA_Salary_History$Team<-re_team(NBA_Salary_History$Team)
#unique(NBA_Salary_History$Team)

# Merge Season + MVP

seasons$Year<-paste(substr(seasons$season,start=1,stop=2),substr(seasons$season,start=6,stop=7),sep="")
seasons$Year[which(seasons$Year==1900)]<-2000

season_statsMVP = merge(seasons, MVP, by.x=c("Year"),by.y=c("Year"))
season_statsMVP<-season_statsMVP[,1:30]

# Changement du format du nom et rajout colonne MVP binaire

season_statsMVP$MVP_number<-0
annee<-unique(season_statsMVP$Year)
tab<-strsplit(season_statsMVP$player_name," ")
vect<-c()
index<-0

for (i in 1:length(tab)){
  tab[[i]][1]<-paste0(substr(tab[[i]][1],start=1,stop=1),". ")
  vect<-c(vect,paste0(tab[[i]][1],tab[[i]][2]))
}
season_statsMVP$player_name<-vect

for (i in annee){
  ind_y<-which(season_statsMVP$Year==i)
  ind_p<-which(season_statsMVP$player_name[ind_y]==season_statsMVP$`MVP Name`[ind_y])
  season_statsMVP$MVP_number[ind_p+index]<-1
  index<-length(ind_y)+index
}
indice<-which(season_statsMVP$player_name=="S. Curry")
indice_2<-which(season_statsMVP$team_abbreviation[indice]=="PHX")
indice_3<-which(season_statsMVP$team_abbreviation[indice]=="SAC")
season_statsMVP$MVP_number[c(indice[indice_2],indice[indice_3])]<-0

#ind<-which(season_statsMVP$MVP_number==1)
#season_statsMVP[ind,]

season_statsMVP<-season_statsMVP[,-c(23:30)]

data_1<-season_statsMVP

# Deuxième jeu de données stats MVP

# Importation 

annee<-unique(MVP$Year)
dat<-read_csv(paste0("nba-models-master/season-stats-advanced/",1997,".csv"))
dat<-cbind(dat,rep(1997,nrow(dat)))
tot<-dat
#tot
colnames(tot)[30]<-"season"
#tot

annee<-as.character(annee[-c(1:48)])

for (i in annee){
  dat<-read_csv(paste0("nba-models-master/season-stats-advanced/",i,".csv"))
  dat<-cbind(dat,rep(i,nrow(dat)))
  colnames(dat)[30]<-"season"
  #dat
  tot<-rbind(tot,dat)
}
#tot
#unique(tot$season)


# Changement formatage du nom numéro 1

annee<-unique(tot$season)
tab<-strsplit(tot$Player,"\\",fixed=TRUE)
length(tab)
length(tot$Player)
vect<-c()
index<-0

for (i in 1:length(tab)){
  vect<-c(vect,tab[[i]][1])
}
#length(vect)

tot$Player<-vect

# Join sur la saison du jeu de données réponse MVP et jeu de données stats MVP

season_statsMVP = merge(tot, MVP, by.x=c("season"),by.y=c("Year"))
season_statsMVP<-season_statsMVP[,c(1:30,37)]

# Formatage du nom numéro 2

tab<-strsplit(season_statsMVP$Player," ")
vect<-c()
index<-0

for (i in 1:length(tab)){
  tab[[i]][1]<-paste0(substr(tab[[i]][1],start=1,stop=1),". ")
  vect<-c(vect,paste0(tab[[i]][1],tab[[i]][2]))
}
season_statsMVP$Player<-vect

season_statsMVP$Player<-gsub("\\*", "",season_statsMVP$Player)
#season_statsMVP

# Création MVP Number colonnnes

season_statsMVP$MVP_number<-0
for (i in annee){
  ind_y<-which(season_statsMVP$season==i)
  ind_p<-which(season_statsMVP$Player[ind_y]==season_statsMVP$`MVP Name`[ind_y])
  season_statsMVP$MVP_number[ind_p+index]<-1
  index<-length(ind_y)+index
}
indice<-which(season_statsMVP$Player=="S. Curry")
indice_2<-which(season_statsMVP$Tm[indice]=="PHO")
indice_3<-which(season_statsMVP$Tm[indice]=="SAC")
season_statsMVP$MVP_number[c(indice[indice_2],indice[indice_3])]<-0

#ind<-which(season_statsMVP$MVP_number==1)
#season_statsMVP[ind,]

season_statsMVP<-season_statsMVP[,-31]

data_2<-season_statsMVP

data_1$team_abbreviation<-initiale(data_1$team_abbreviation)
data_2$Tm<-initiale(data_2$Tm)

data_1$player_name<-str_to_lower(data_1$player_name)
data_2$Player<-str_to_lower(data_2$Player)

season_statsMVP <- merge(data_1, data_2, by.x=c("player_name","Year","team_abbreviation"),by.y=c("Player","season","Tm"))
season_statsMVP<-season_statsMVP[,-c(5,13,23)]
colnames(season_statsMVP)[48]<-"MVP_number"

ind<-which(season_statsMVP$G>50)
da<-season_statsMVP[ind,c(1,2,15,22,24)] %>% 
  group_by(Year, Pos) %>% 
  slice(which.max(net_rating/G))


### Season stats 2 ###

season_stats2<-season_stats
rowtokick<-c()
poste<-c("PG","SG","SF","PF","C")
for (i in seq(1,nrow(season_stats2))){
  if (!(season_stats2$Pos[i] %in% poste)){
    rowtokick<-c(rowtokick,i)
  }
}
season_stats2<-season_stats2[-rowtokick,]

### TOP Players : Avec restriction ###

season_stats4<-season_stats2[,c("Year","Pos","PTS","MP","G","BLK","AST","STL","BPM")]
season_stats4$MPPG<-season_stats4$MP/season_stats4$G
season_stats4<-season_stats4 %>%
  filter(MPPG>30) %>%
  filter(G>50)%>%
  group_by(Year, Pos) %>%
  summarise(sum_PTS=sum(PTS),
            Points=mean(PTS),
            sum_MP=sum(MP),
            Minutes=mean(MP),
            sum_BLK=sum(BLK),
            Bloquages=mean(BLK),
            Matches=mean(G),
            Passes_Decisives=mean(AST),
            Interceptions=mean(STL),
            BPM=mean(BPM))

### SEASON CLASSEMENT ###

rankings <- data.frame()
for (i in seq(1977,2019)){
  ranking<- read.csv(paste(paste("nba-models-master/season-standings/",i,sep=""),".csv",sep=""))
  if (i<1999){
    ranking<-ranking[,1:15]
    ranking<-ranking[,-c(8,9,10,11,12,13)]
  }
  else if (i==1999){
    ranking<-ranking[,1:13]
    ranking<-ranking[,-c(8,9,10,11)]
  }
  else if(i<2005){
    ranking<-ranking[,1:15]
    ranking<-ranking[,-c(8,9,10,11,12,13)]
  }
  else if(i>=2005){
    ranking<-ranking[,1:17]
    ranking<-ranking[,-c(8,9,10,11,12,13,14,15)]
  }
  ranking$Year<-i
  rankings<-rbind(rankings,ranking)
}

rankings<-rankings[,c(2,3,4,5,10)]
for (i in seq(1,nrow(rankings))){
  rankings$Overall[i]<-as.numeric(str_split(rankings$Overall[i], "-")[[1]][1])/(as.numeric(str_split(rankings$Overall[i], "-")[[1]][2])+as.numeric(str_split(rankings$Overall[i], "-")[[1]][1]))
  
  rankings$Home[i]<-as.numeric(str_split(rankings$Home[i], "-")[[1]][1])/(as.numeric(str_split(rankings$Home[i], "-")[[1]][2])+as.numeric(str_split(rankings$Home[i], "-")[[1]][1]))
  
  rankings$Road[i]<-as.numeric(str_split(rankings$Road[i], "-")[[1]][1])/(as.numeric(str_split(rankings$Road[i], "-")[[1]][2])+as.numeric(str_split(rankings$Road[i], "-")[[1]][1]))
}

rankings$team_name<-rankings$Team
rankings$team_name<-re_team(rankings$team_name)

### BILAN TOUTES LES ANNEES ###

rankings2<-rankings[,c("Overall","Home","Road","Year","team_name")]
rankings2<-rankings2 %>%
  group_by(team_name) %>%
  summarise(mean_O=mean(as.numeric(Overall)),
            mean_H=mean(as.numeric(Home)),
            mean_R=mean(as.numeric(Road)))


### BILAN POUR CHAQUE ANNEE ###

rankings3<-rankings[,c("Overall","Home","Road","Year","team_name")]
rankings3<-rankings3 %>%
  group_by(team_name,Year) %>%
  summarise(mean_O=mean(as.numeric(Overall)),
            mean_H=mean(as.numeric(Home)),
            mean_R=mean(as.numeric(Road)))

### Rename colonne ###
rankings2<-rename(rankings2,
                  c("team_name"="team_name",
                    "General"="mean_O",
                    "Domicile"="mean_H",
                    "Deplacement"="mean_R"
                  ))
rankings3<-rename(rankings3,
                  c("team_name"="team_name",
                    "Year"="Year",
                    "General"="mean_O",
                    "Domicile"="mean_H",
                    "Deplacement"="mean_R"
                  ))

#### VB style ####

VB_style <- function(msg = 'Hello', style="font-size: 100%;"){
  tags$p( msg , style = style )
}

#### Salaire quantile ####

sal_play<-aggregate(. ~ Season,NBA_Salary_History_Players[,-c(2:3)] , mean)
sal_play2<-aggregate(. ~ Season,NBA_Salary_History_Players[,-c(2:3)] , FUN=quantile,probs=0.1)
sal_play3<-aggregate(. ~ Season,NBA_Salary_History_Players[,-c(2:3)] , FUN=quantile,probs=0.25)
sal_play4<-aggregate(. ~ Season,NBA_Salary_History_Players[,-c(2:3)] , FUN=quantile,probs=0.75)
sal_play5<-aggregate(. ~ Season,NBA_Salary_History_Players[,-c(2:3)] , FUN=quantile,probs=0.90)

sal_play$Salary5<-round(sal_play2$Salary,0)
sal_play$Salary25<-round(sal_play3$Salary,0)
sal_play$Salary75<-round(sal_play4$Salary,0)
sal_play$Salary95<-round(sal_play5$Salary,0)

### Prédiction salaire ###

sal_play2<-sal_play$Salary/10**6
#package(tseries)
#kpss.test(time_ser, null = c( "Trend"), lshort = TRUE)
time_ser<-ts(sal_play2,frequency=1,start=c(1991))
kpss.test(diff(time_ser), null = c( "Trend"), lshort = TRUE)

fit<-auto.arima(
  time_ser,
  stationary = FALSE,
  seasonal = FALSE
)

### ####


### Fin Salaire quantile ###

### Stats TEAM : Tableau PALMARES ###

MVP$`NBA Champion`<-re_team(MVP$`NBA Champion`)
MVP$`Eastern Champion`<-re_team(MVP$`Eastern Champion`)
MVP$`Western Champion`<-re_team(MVP$`Western Champion`)

tab1<-as.data.frame(table(MVP$`NBA Champion`))
tab2<-as.data.frame(table(MVP$`Eastern Champion`))
tab3<-as.data.frame(table(MVP$`Western Champion`))
tab_tot<-merge(tab1, tab2, by.x=c("Var1"),by.y=c("Var1"),all.x = TRUE,all.y=TRUE)
tab_tot<-merge(tab_tot, tab3, by.x=c("Var1"),by.y=c("Var1"),all.x = TRUE,all.y=TRUE)
colnames(tab_tot)<-c("Team","Champion","Eastern","Weastern")
ind<-which(tab_tot$Eastern>0)
tab_tot$Weastern[ind]<-0
ind<-which(tab_tot$Weastern>0)
tab_tot$Eastern[ind]<-0
tab_tot[19:23,2]<-0

### RECORDS : SAISON ###

season_stats6<-season_stats2[,c("Year","Pos","PTS","MP","G","BLK","AST","STL","BPM","PER","Player","Tm")]
ind<-which(season_stats6$Player=="Eddie Johnson" & season_stats6$Pos=="SF")
season_stats6$Player[ind]<-"Eddie Johnson*"
ind<-which(season_stats6$Player=="Gerald Henderson" & season_stats6$Pos=="SG")
season_stats6$Player[ind]<-"Gerald Henderson*"
ind<-which(season_stats6$Player=="Mike Dunleavy" & season_stats6$Pos=="PG")
season_stats6$Player[ind]<-"Mike Dunleavy*"
ind<-which(season_stats6$Player=="George Johnson" & season_stats6$Pos=="PF")
season_stats6$Player[ind]<-"George Johnson*"
season_stats7<-season_stats6%>%
  group_by(Player)%>%
  filter(Tm!="TOT")%>%
  summarise(sum_PTS=sum(PTS),
            mean_PTS=mean(PTS),
            sum_BLK=sum(BLK),
            mean_BLK=mean(BLK),
            sum_STL=sum(STL),
            mean_STL=mean(STL),
            sum_AST=sum(AST),
            mean_AST=mean(AST),
            sum_G=sum(G),
            mean_G=mean(G))

### EVOLUTION DU BASKET TOUS LES JOUEURS ###

season_stats5<-season_stats2[,c("Year","Pos","PTS","MP","G","BLK","AST","STL","BPM")]
season_stats5<-season_stats5 %>%
  group_by(Year, Pos) %>%
  summarise(sum_PTS=sum(PTS),
            Points=mean(PTS),
            sum_MP=sum(MP),
            Minutes=mean(MP),
            sum_BLK=sum(BLK),
            Bloquages=mean(BLK),
            Matches=mean(G),
            Passes_Decisives=mean(AST),
            Interceptions=mean(STL),
            BPM=mean(BPM))


### ACP ###

### Construction array #####

build_array<-function(data,nb_cluster){
  
      clust_play<-data %>% 
        group_by(Tm) %>% 
        count(part)
      
      ligne<-unique(clust_play$Tm)
      colonne<-cbind("Team",t(as.character(unique(clust_play$part))))
      nodata <- as.data.frame(setNames(replicate((nb_cluster+1),numeric(0), simplify = F), colonne))
      
      for (i in 1:length(ligne)){
        ind<-which(clust_play$Tm==ligne[i])
        vect<-c(ligne[i])
        for(j in 2:length(colonne)){
          indice<-which(clust_play$part[ind]==colonne[j])
          indice
          if(length(indice)==0){
              elem<-0
          }else{
              elem<-as.integer(clust_play$n[ind[indice]])
          }
          vect<-cbind(vect,elem)
        }
        nodata<-rbind(nodata,vect)
      }
      colnames(nodata)<-colonne
      
      for(i in 2:(nb_cluster+1)){
        nodata[,i]<-as.integer(nodata[,i])
      }
      return(nodata)
}

### TEAM ###

teams<-teams[,c(2,5)]

ma_home<-aggregate(. ~ TEAM_ID_home + SEASON,games[,c(6,7:13)] , mean)
ec_home<-aggregate(. ~ TEAM_ID_home + SEASON,games[,c(6:7,15:20)] , mean)

colnames(ma_home)<-c("Team","Year","pts_ma","FG_PCT_ma","FT_PCT_ma","FG3_PCT_ma","AST_ma","REB_ma")
colnames(ec_home)<-c("Team","Year","pts_ec","FG_PCT_ec","FT_PCT_ec","FG3_PCT_ec","AST_ec","REB_ec")

ma_away<-aggregate(. ~ TEAM_ID_away + SEASON,games[,c(6,14:20)] , mean)
ec_away<-aggregate(. ~ TEAM_ID_away + SEASON,games[,c(6,8:14)] , mean)

colnames(ma_away)<-c("Team","Year","pts_ma","FG_PCT_ma","FT_PCT_ma","FG3_PCT_ma","AST_ma","REB_ma")
colnames(ec_away)<-c("Team","Year","pts_ec","FG_PCT_ec","FT_PCT_ec","FG3_PCT_ec","AST_ec","REB_ec")

ma<-rbind(ma_home,ma_away)
ec<-rbind(ec_home,ec_away)

ma<-aggregate(. ~ Team + Year,ma, mean)
ec<-aggregate(. ~ Team + Year,ec , mean)

team_pa<- merge(ma, ec, by.x=c("Team","Year"),by.y=c("Team","Year"))
team<-merge(team_pa, teams, by.x=c("Team"),by.y=c("TEAM_ID"))
team$Team<-team$ABBREVIATION
team<-team[,-15]

### AGE ###

season_stats_age<-season_statsMVP[which(season_statsMVP$Age<41),]

#table(season_stats_age$Age)
#View(season_stats_age[which(season_stats_age$Age==36),])
age<-aggregate(. ~ Age,season_stats_age[,c(12,13,14,24,25,23,42,46)] , mean)
library(plyr)
age<-rename(age,
          c("Age"="Age",
            "pts"="Points",
            "reb"="Rebonds",
            "ast"="Passes",
            "G"="Matches",
            "MP"="Minutes",
            "WS/48"="Impact Victoire",
            "BPM"="Note du joueur"
          )
)
detach("package:plyr", unload=TRUE)
age<-as.data.frame(age)



### DRAFT ###

d_draft<-season_statsMVP[,c(9,10,11,13,14,23,24,25,42,46,12)]

library(plyr)
d_draft<-rename(d_draft,
                       c("Age"="Age",
                         "pts"="Points",
                         "reb"="Rebonds",
                         "ast"="Passes",
                         "G"="Matches",
                         "MP"="Minutes",
                         "WS/48"="Impact Victoire",
                         "BPM"="Note du joueur"
                       )
)
detach("package:plyr", unload=TRUE)
d_draft<-as.data.frame(d_draft)
### 

d_draft$draft_round<-as.integer(d_draft$draft_round)
d_draft$draft_year<-as.integer(d_draft$draft_year)

d_draft<- d_draft %>%
  filter(!is.na(draft_year))

#draft<-aggregate(. ~ draft_year,season_statsMVP[,c(9,13,14,15,24,12)] , mean)
#draft<-as.data.frame(draft)


### Age feat Draft ###

#imp_perf<-as.data.frame(table(season_statsMVP$Age))
#colnames(imp_perf)<-c("Age","Nombre de joueurs")
#View(t(imp_perf))
#summary(season_statsMVP$Age)

data_age_draft<-season_statsMVP[which(season_statsMVP$Age<41),]

data_age_draft<- data_age_draft %>%
  filter(!is.na(draft_year))

data_age_draft$Nb_year_played<-(as.integer(data_age_draft$Year)-as.integer(data_age_draft$draft_year))
data_age_draft<-data_age_draft[which(data_age_draft$Nb_year_played<=(data_age_draft$Age-18+1)),]


library(plyr)
data_age_draft<-rename(data_age_draft,
            c("Age"="Age",
              "Nb_year_played"="Nombre d'annees en NBA",
              "pts"="Points",
              "reb"="Rebonds",
              "ast"="Passes",
              "G"="Matches",
              "MP"="Minutes",
              "WS/48"="Impact Victoire",
              "BPM"="Note du joueur",
              "NB"="Note du joueur"
            )
)

detach("package:plyr", unload=TRUE)
data_age_draft<-as.data.frame(data_age_draft)

data_age_draft2<-aggregate(. ~ Age + `Nombre d'annees en NBA`,data_age_draft[,c(12,13,14,24,25,23,42,46,49)] , mean)

### Quantile - Stats age ###

season_stats_age_quantile<-season_statsMVP[which(season_statsMVP$Age<41),]

t_age_2<-aggregate(. ~ Age,season_stats_age_quantile[,c(12,13,14,24,25,23,42,46)],FUN=quantile,probs=0.1)
t_age_2<-plyr::rename(t_age_2,
                      c("Age"="Age","pts"="Points","reb"="Rebonds","ast"="Passes","G"="Matches","MP"="Minutes",
                        "WS/48"="Impact Victoire","BPM"="Note du joueur"))
t_age_2<-as.data.frame(t_age_2)

t_age_3<-aggregate(. ~ Age,season_stats_age_quantile[,c(12,13,14,24,25,23,42,46)],FUN=quantile,probs=0.25)
t_age_3<-plyr::rename(t_age_3,
                      c("Age"="Age","pts"="Points","reb"="Rebonds","ast"="Passes","G"="Matches","MP"="Minutes",
                        "WS/48"="Impact Victoire","BPM"="Note du joueur"))
t_age_3<-as.data.frame(t_age_3)

t_age_4<-aggregate(. ~ Age,season_stats_age_quantile[,c(12,13,14,24,25,23,42,46)],FUN=quantile,probs=0.75)
t_age_4<-plyr::rename(t_age_4,
                      c("Age"="Age","pts"="Points","reb"="Rebonds","ast"="Passes","G"="Matches","MP"="Minutes",
                        "WS/48"="Impact Victoire","BPM"="Note du joueur"))
t_age_4<-as.data.frame(t_age_4)

t_age_5<-aggregate(. ~ Age,season_stats_age_quantile[,c(12,13,14,24,25,23,42,46)],FUN=quantile,probs=0.9)
t_age_5<-plyr::rename(t_age_5,
                      c("Age"="Age","pts"="Points","reb"="Rebonds","ast"="Passes","G"="Matches","MP"="Minutes",
                        "WS/48"="Impact Victoire","BPM"="Note du joueur"))
t_age_5<-as.data.frame(t_age_5)

### Fin Quantile ###


### Stats Players ###

library(plyr)
stats_players<-season_statsMVP[,c(1,2,3,5,6,12,13,14,22,24,25,42,46)]
stats_players<-rename(stats_players,
                      c("player_name"="Joueur",
                        "Year"="Annee",
                        "team_abbreviation"="Equipe",
                        "player_height"="Taille",
                        "player_weight"="Poids",
                        "pts"="Points",
                        "reb"="Rebonds",
                        "ast"="Passes",
                        "Pos"="Poste",
                        "G"="Matches",
                        "MP"="Minutes",
                        "WS/48"="Impact Victoire",
                        "BPM"="Note du joueur"
                      )
)
stats_players<-stats_players[,c("Joueur",
                                "Annee",
                                "Equipe",
                                "Poste",
                                "Taille",
                                "Poids",
                                "Points",
                                "Rebonds",
                                "Passes",
                                "Matches",
                                "Minutes",
                                "Impact Victoire",
                                "Note du joueur")]

stats_players$Taille<-round(stats_players$Taille,0)
stats_players$Poids<-round(stats_players$Poids,0)
detach("package:plyr", unload=TRUE)

### Stats country ###

df<-plyr::count(season_statsMVP$country)
df<-df[which(df$freq>50),1]
country_list<-as.vector(df)
ind<-which(season_statsMVP$country %in% country_list)
season_stats_country<-season_statsMVP[ind,]

season_stats_country2<-season_statsMVP
season_stats_country2$country[which(season_statsMVP$country!="USA")]<-"Other"

country_prop<-prop.table(table(season_stats_country2$Year,season_stats_country2$country),1)
country_prop<-as.data.frame(country_prop)
country_prop<-rename(country_prop,
                     c("Annee"="Var1",
                       "Pays"="Var2",
                       "Prop"="Freq"
                     )
)


country_prop2<-country_prop[which(country_prop$Pays=="Other"),c(3)]
time_ser2=ts(country_prop2,frequency=1,start=c(1997))

fit2<-auto.arima(
  time_ser2,
  stationary = FALSE,
  seasonal = FALSE
)


### mvp ####

MVPtot <- data.frame("MVP reel"=character(),
                     "Annee"=character(), 
                     "MVP predit"=character(),
                     "MVP_knn"=character(),
                     "MVP_LDA"=character(),
                     "MVP_NB"=character(),
                     "Same"=character(),
                     "Same2"=character(),
                     "Same3"=character(),
                     "Same4"=character()) 


line=1

#init
season_statsMVP2<-season_statsMVP[,-c(5,6,38,43)]
season_statsMVP2<-na.omit(season_statsMVP2)
Xkeep<-season_statsMVP2[,4]
season_statsMVP2<-season_statsMVP2[,-4]
for(i in unique(season_statsMVP$Year)){
  indice<-which(season_statsMVP2$Year!=i)
  Xkeeptest<-Xkeep[-indice]
  X<-season_statsMVP2[,-c(43)]
  Year<-X$Year[-indice]
  Player<-X$player_name[-indice]
  nums <- unlist(lapply(X, is.numeric))
  Y<-season_statsMVP2[,43]
  
  #RF
  Xtrain<-X[indice,]
  Xtest<-X[-indice,]
  Ytrain<-Y[indice]
  Ytest<-Y[-indice]
  
  #knn et lda
  X_knn<-X[ , nums]
  X_knn<-scale(X_knn, center = TRUE, scale = TRUE)
  Xtrain_knn<-X_knn[indice,]
  Xtest_knn<-X_knn[-indice,]
  
  #NB
  data<-cbind(X_knn,Y)
  data<-as.data.frame(data)
  datatrain<-data[indice,]
  Xtest_nb<-as.data.frame(Xtest_knn[-indice,])
  
  #### Processing ####
  #rf
  rf<-randomForest(ntree = 50,x=Xtrain,y=as.factor(Ytrain))
  pred<-predict(rf,Xtest,type="prob")
  MVPtest<-cbind(Xtest,pred)
  MVPtest<-cbind(MVPtest,Xkeeptest)
  
  #knn
  pred_test<-class::knn(Xtrain_knn, Xtest_knn, as.factor(Ytrain), k=51,prob=TRUE)
  R1<-as.numeric(attr(pred_test,"prob"))
  MVPtest_knn<-cbind(Player,Xtest_knn)
  MVPtest_knn<-cbind(MVPtest_knn,R1)
  MVPtest_knn<-cbind(MVPtest_knn,Year)
  MVPtest_knn<-cbind(MVPtest_knn,Xkeeptest)
  MVPtest_knn<-as.data.frame(MVPtest_knn)
  
  #LDA
  g_lda<-lda(Xtrain_knn,grouping = as.factor(Ytrain))
  pred_test_lda<-predict(g_lda,Xtest_knn)$posterior
  MVPtest_lda<-cbind(Player,Xtest_knn)
  MVPtest_lda<-cbind(MVPtest_lda,pred_test_lda)
  MVPtest_lda<-cbind(MVPtest_lda,Year)
  MVPtest_lda<-cbind(MVPtest_lda,Xkeeptest)
  MVPtest_lda<-as.data.frame(MVPtest_lda)
  
  #Naive Bayes
  g_naive <- naiveBayes(as.factor(Y) ~ ., data = datatrain)
  pred_test_nb<-predict(g_naive, Xtest_knn, type = "raw")
  score_bayes <-pred_test_nb[,2]
  MVPtest_nb<-cbind(Player,Xtest_knn)
  MVPtest_nb<-cbind(MVPtest_nb,pred_test_nb)
  MVPtest_nb<-cbind(MVPtest_nb,Year)
  MVPtest_nb<-cbind(MVPtest_nb,Xkeeptest)
  MVPtest_nb<-as.data.frame(MVPtest_nb)
  
  
  #### Prediction ####
  year<-unique(MVPtest$Year)
  ind<-c()
  
  #RF
  for (i in year){
    indyear<-MVPtest$X[which(MVPtest$Year==i)]
    maxi<-max(MVPtest$"1"[which(MVPtest$X %in% indyear & MVPtest$MP>100)])
    indexX<-MVPtest$X[which(MVPtest$"1"==maxi & MVPtest$Year==i)]
    ind<-cbind(ind,indexX)
  }
  MVPpred<-MVPtest[which(MVPtest$X %in% ind),c(1,2,44)]
  MVPpred<-MVPpred[order(MVPpred$Year),]
  
  #KNN
  ind_knn<-c()
  for (i in year){
    #indyear<-MVPtest_knn$Xkeeptest[which(MVPtest_knn$Year==i)]
    ###attention
    indyear<-MVPtest_knn$Xkeeptest[which(MVPtest_knn$Year==i & MVPtest$MP>100)]
    mini<-min(MVPtest_knn$R1[which(MVPtest_knn$Xkeeptest %in% indyear)])
    indexX<-MVPtest_knn$Xkeeptest[which(MVPtest_knn$R1==mini & MVPtest_knn$Year==i)]
    ind_knn<-cbind(ind_knn,indexX)
  }
  MVPpred_knn<-MVPtest_knn[which(MVPtest_knn$Xkeeptest %in% ind_knn),c(1,35,36)]
  MVPpred_knn<-MVPpred_knn[order(MVPpred_knn$Year),]
  
  #LDA
  ind_lda<-c()
  for (i in year){
    indyear<-MVPtest_lda$Xkeeptest[which(MVPtest_lda$Year==i & MVPtest$MP>100)]
    mini<-min(MVPtest_lda$"1"[which(MVPtest_lda$Xkeeptest %in% indyear)])
    indexX<-MVPtest_lda$Xkeeptest[which(MVPtest_lda$"1"==mini & MVPtest_lda$Year==i)]
    ind_lda<-cbind(ind_lda,indexX)
  }
  MVPpred_lda<-MVPtest_lda[which(MVPtest_lda$Xkeeptest %in% ind_lda),c(1,36,37)]
  MVPpred_lda<-MVPpred_lda[order(MVPpred_lda$Year),]
  
  #NB
  ind_nb<-c()
  for (i in year){
    indyear<-MVPtest_nb$Xkeeptest[which(MVPtest_nb$Year==i & MVPtest$MP>100)]
    maxi<-min(MVPtest_nb$"1"[which(MVPtest_nb$Xkeeptest %in% indyear)])
    indexX<-MVPtest_nb$Xkeeptest[which(MVPtest_nb$"1"==maxi & MVPtest_nb$Year==i)]
    ind_nb<-cbind(ind_nb,indexX)
  }
  MVPpred_nb<-MVPtest_nb[which(MVPtest_nb$Xkeeptest %in% ind_nb),c(1,36,37)]
  MVPpred_nb<-MVPpred_nb[order(MVPpred_nb$Year),]
  
  
  
  #### Vrai MVP ####
  ind<-c()
  for (i in year){
    indyear<-season_statsMVP$X1[which(season_statsMVP$Year==i)]
    maxi<-max(season_statsMVP$MVP_number[which(season_statsMVP$X1 %in% indyear)])
    indexX<-season_statsMVP$X1[which(season_statsMVP$MVP_number==maxi &season_statsMVP$Year==i)]
    ind<-cbind(ind,indexX)
  }
  MVPtrue<-season_statsMVP[which(season_statsMVP$X1 %in% ind),c(1,2)]
  MVPtrue<-MVPtrue[order(MVPtrue$Year),]
  
  
  #### Creation table a afficher ####
  MVPtoprint<-merge(MVPpred,MVPtrue,by.x="Year",by.y="Year")
  MVPtoprint<-plyr::rename(MVPtoprint,c("player_name.x"="MVP predit",
                                        "Year"="Annee","player_name.y"="MVP reel"
  ))
  MVPtoprint2<-merge(MVPtoprint,MVPpred_knn,by.x = "Annee",by.y="Year")
  MVPtoprint2<-plyr::rename(MVPtoprint2,c("MVP predit"="MVP predit",
                                          "Annee"="Annee","MVP reel"="MVP reel","Player"="MVP_knn"
  ))
  
  MVPtoprint3<-merge(MVPtoprint2,MVPpred_lda,by.x = "Annee",by.y="Year")
  MVPtoprint3<-plyr::rename(MVPtoprint3,c("MVP predit"="MVP predit",
                                          "Annee"="Annee","MVP reel"="MVP reel","MVP_knn"="MVP_knn","Player"="MVP_LDA"
  ))
  
  MVPtoprint4<-merge(MVPtoprint3,MVPpred_nb,by.x = "Annee",by.y="Year")
  MVPtoprint4<-plyr::rename(MVPtoprint4,c("MVP predit"="MVP predit",
                                          "Annee"="Annee","MVP reel"="MVP reel","MVP_knn"="MVP_knn","MVP_LDA"="MVP_LDA","Player"="MVP_NB"
  ))
  
  MVPtoprint4<-MVPtoprint4[,c("MVP reel", "Annee","MVP predit","MVP_knn","MVP_LDA","MVP_NB")]
  MVPtoprint4$Same<-as.numeric(MVPtoprint4$Annee)
  MVPtoprint4$Same[which(MVPtoprint4$`MVP reel`==MVPtoprint4$`MVP predit`)]<-1
  MVPtoprint4$Same[which(MVPtoprint4$`MVP reel`!=MVPtoprint4$`MVP predit`)]<-0
  MVPtoprint4$Same2<-as.numeric(MVPtoprint4$Annee)
  MVPtoprint4$Same2[which(MVPtoprint4$`MVP reel`==MVPtoprint4$`MVP_knn`)]<-1
  MVPtoprint4$Same2[which(MVPtoprint4$`MVP reel`!=MVPtoprint4$`MVP_knn`)]<-0
  MVPtoprint4$Same3<-as.numeric(MVPtoprint4$Annee)
  MVPtoprint4$Same3[which(MVPtoprint4$`MVP reel`==MVPtoprint4$`MVP_LDA`)]<-1
  MVPtoprint4$Same3[which(MVPtoprint4$`MVP reel`!=MVPtoprint4$`MVP_LDA`)]<-0
  MVPtoprint4$Same4<-as.numeric(MVPtoprint4$Annee)
  MVPtoprint4$Same4[which(MVPtoprint4$`MVP reel`==MVPtoprint4$`MVP_NB`)]<-1
  MVPtoprint4$Same4[which(MVPtoprint4$`MVP reel`!=MVPtoprint4$`MVP_NB`)]<-0
  MVPtot[line,]<-MVPtoprint4
  line=line+1
}

MVPtot<-MVPtot[order(MVPtot$Annee,decreasing = TRUE),]
MVPtot<-plyr::rename(MVPtot,c("MVP.predit"="MVP RF",
                              "Annee"="Annee","MVP.reel"="MVP reel","MVP_knn"="MVP knn","MVP_LDA"="MVP LDA","MVP_NB"="MVP NB"
))

#### FIN MVP ####

### ACP ###


indice<-which(season_stats2$Year>2000)
season_stats_acp<-season_stats2[indice,]

season_stats_acp<-season_stats_acp[,-c(1,8,14:20,22,27,32,33,35,36,38,39,42,43)]
season_stats_acp[,c(26:34)]<-round(season_stats_acp[,c(26:34)]/season_stats_acp$G,2)

indic_30G<-which(season_stats_acp$G>30)
season_stats_acp<-season_stats_acp[indic_30G,]

seasons <- read_csv("all_seasons.csv")
seasons$Year<-paste(substr(seasons$season,start=1,stop=2),substr(seasons$season,start=6,stop=7),sep="")
seasons$Year[which(seasons$Year==1900)]<-2000
seasons<-seasons[,c(2,5,6,23)]

seasons$player_name<-gsub("\\*", "",seasons$player_name)
season_stats_acp$Player<-gsub("\\*", "",season_stats_acp$Player)

season_stats_acp_indic<-season_stats_acp[,c(2,13:20)]
season_stats_acp_indic

season_stats_PCA<-merge(season_stats_acp, seasons, by.x=c("Year","Player"),by.y=c("Year","player_name"),all.x=TRUE)
season_stats_PCA$Tm<-initiale(season_stats_PCA$Tm)

### Champion ###

rankingschamp <- data.frame()
for (i in seq(1977,2019)){
  ranking<- read.csv(paste(paste("nba-models-master/season-standings/",i,sep=""),".csv",sep=""))
  if (i<1999){
    ranking<-ranking[,1:15]
    ranking<-ranking[,-c(8,9,10,11,12,13)]
  }
  else if (i==1999){
    ranking<-ranking[,1:13]
    ranking<-ranking[,-c(8,9,10,11)]
  }
  else if(i<2005){
    ranking<-ranking[,1:15]
    ranking<-ranking[,-c(8,9,10,11,12,13)]
  }
  else if(i>=2005){
    ranking<-ranking[,1:17]
    ranking<-ranking[,-c(8,9,10,11,12,13,14,15)]
  }
  ranking$Year<-i
  rankingschamp<-rbind(rankingschamp,ranking)
}

for (i in seq(1,nrow(rankingschamp))){
  rankingschamp$Overall[i]<-as.numeric(str_split(rankingschamp$Overall[i], "-")[[1]][1])/(as.numeric(str_split(rankingschamp$Overall[i], "-")[[1]][2])+as.numeric(str_split(rankingschamp$Overall[i], "-")[[1]][1]))
  
  rankingschamp$Home[i]<-as.numeric(str_split(rankingschamp$Home[i], "-")[[1]][1])/(as.numeric(str_split(rankingschamp$Home[i], "-")[[1]][2])+as.numeric(str_split(rankingschamp$Home[i], "-")[[1]][1]))
  
  
  rankingschamp$Road[i]<-as.numeric(str_split(rankingschamp$Road[i], "-")[[1]][1])/(as.numeric(str_split(rankingschamp$Road[i], "-")[[1]][2])+as.numeric(str_split(rankingschamp$Road[i], "-")[[1]][1]))
  
  
  rankingschamp$X3[i]<-as.numeric(str_split(rankingschamp$X3[i], "-")[[1]][1])/(as.numeric(str_split(rankingschamp$X3[i], "-")[[1]][2])+as.numeric(str_split(rankingschamp$X3[i], "-")[[1]][1]))
  
  
  rankingschamp$X10[i]<-as.numeric(str_split(rankingschamp$X10[i], "-")[[1]][1])/(as.numeric(str_split(rankingschamp$X10[i], "-")[[1]][2])+as.numeric(str_split(rankingschamp$X10[i], "-")[[1]][1]))
  
  
  rankingschamp$E[i]<-as.numeric(str_split(rankingschamp$E[i], "-")[[1]][1])/(as.numeric(str_split(rankingschamp$E[i], "-")[[1]][2])+as.numeric(str_split(rankingschamp$E[i], "-")[[1]][1]))
  
  
  rankingschamp$W[i]<-as.numeric(str_split(rankingschamp$W[i], "-")[[1]][1])/(as.numeric(str_split(rankingschamp$W[i], "-")[[1]][2])+as.numeric(str_split(rankingschamp$W[i], "-")[[1]][1]))
}

champions <- read_excel("NBA Finals and MVP.xlsx")
champions<-champions[,c(1,5)]
rankingschamp<-merge(rankingschamp,champions,by.x=c("Year"),by.y=c("Year"))
rankingschamp$Champ<-0

for (i in seq(1,nrow(rankingschamp))){
  if(rankingschamp$Team[i]==rankingschamp$`NBA Champion`[i]){
    rankingschamp$Champ[i]<-1
  }
}

Champtot <- data.frame("Champion reel"=character(),
                       "Annee"=character(), 
                       "Champion predit"=character(),
                       "Same"=character()) 

line=1

### Predictions ###

rankingschamp<-rankingschamp[which(rankingschamp$Year>1980),]
for(i in unique(rankingschamp$Year)){
  indice<-which(rankingschamp$Year!=i)
  X<-rankingschamp[,-c(11,12)]
  Y<-rankingschamp[,12]
  Xtrain<-X[indice,]
  Xtest<-X[-indice,]
  Ytrain<-Y[indice]
  Ytest<-Y[-indice]
  #Prediction
  rf<-randomForest(x=Xtrain,y=as.factor(Ytrain))
  pred<-predict(rf,Xtest,type="prob")
  champtest<-cbind(Xtest,pred)
  year<-unique(champtest$Year)
  ind<-c()
  index=0
  for (i in year){
    indyear<-which(champtest$Year==i)
    ind<-cbind(ind,index+which.max(champtest$"1"[indyear]))
    index=index+length(indyear)
  }
  #### VRAi champ ####
  Champtrue<-rankingschamp[which(rankingschamp$Year==i & rankingschamp$Champ==1),c(1,3)]
  Champtrue
  Champtoprint<-merge(Champtrue,champtest[ind,c(1,3)],by.x="Year",by.y="Year")
  Champtoprint
  Champtoprint$Same<-as.numeric(Champtoprint$Year)
  Champtoprint
  Champtoprint$Same[which(Champtoprint$`Team.x`==Champtoprint$`Team.y`)]<-1
  Champtoprint$Same[which(Champtoprint$`Team.x`!=Champtoprint$`Team.y`)]<-0
  Champtot[line,]<-Champtoprint
  line<-line+1
}
Champtot<-Champtot[,c("Annee","Champion.reel","Champion.predit","Same")]
Champtot<-plyr::rename(Champtot,c("Annee"="Champion reel","Champion.reel"="Annee","Champion.predit"="Champion predit","Same"="Same"))
Champtot<-Champtot[order(Champtot$Annee,decreasing = TRUE),]


### Table Var Métrique ###

table_met<-data.frame(Variables = c("BPM","OBPM","DBPM","WS","OWS","DWS","WS/48","PER"), 
                      Signification = c("Box Plus-Minus","Offensive BPM","Defensive BPM","Win Share","Offensive WS","Defensive WS","Win Share /48 Minutes","Player Efficiency Rating"),
                      Explication = c("Metrique avancee de la qualite","Metrique avancee de la qualite offensive","Metrique avancee de la qualite defensive","Participation a la victoire","Participation a la victoire offensive","Participation a la victoire defensive","Participation a la victoire sur 48 minutes","Efficacite du joueur"),
                      Type=c("Les deux","Offensive","Defensive","Les deux","Offensive","Defensive","Les deux","Les deux"))

### Table Var Players ###

table_play<-data.frame(Variables=colnames(season_stats_PCA)[-c(1:5,8,13:20)],
                       Signification = c("Games","Minutes Played","True Shooting Percentage","X3PAr","Ftr","Usage Percentage","Field Goal Percentage","3-Point Field Goal %","2-Point Field Goals %","Effective Field Goal Percentage",
                                         "Free Throws Percentage","Offensive Rebounds","Defensive Rebounds","Total Rebounds","Assists","Steels","Blocks","Turnovers","Personal Fouls","Points","Height","Weight"),
                       Explication = c("Matchs joues","Minutes jouees","Pourcentage reussite tirs","Pourcentage 3-points tentes","Pourcentage lancer franc tentes","Pourcentage de ballons touches","Pourcentage de tirs marques","Pourcentage 3-points marques","Pourcentage 2-points marques","Pourcentage de tirs effectifs marques",
                                       "Pourcentage lancer franc marques","Rebonds offensifs","Rebonds defensifs","Rebonds totaux","Passes decisives","Interceptions","Blocages","Pertes de balle","Fautes commises","Points marques","Taille","Poids")
)


### Table Var Team ###

#table_team<-data.frame(Variables = c("BPM","OBPM","DBPM","OWS","DWS","WS","WS/48","PER","VORP"), 
#                      Signification = c("Basketball Box Score-Based Metric","Offensive BPM","Defensive BPM","Offensive WS","Defensive WS","Win Share","Win Share /48","Player Efficiency Rating","Value Over Replacement Player"),
#                      Explication = c("nul","nul","nul","nul","nul","nul","nul","nul","nul"),
#                      Type=c("All","Offensive","Defensive","Offensive","Defensive","All","All","All","All"))

table_team<-data.frame(Variables = colnames(strong_team)[-c(1,2)], 
                       Signification = c("Points marques","Pourcentage tirs marques","Pourcentage lancer franc marques","Pourcentage 3-Points marques","Passes decisives faites","Rebonds pris","Points encaisses","Pourcentage tirs encaisses","Pourcentage lancer franc encaisses","Pourcentage 3-points encaisses","Passes decisives encaisses","Rebonds encaisses","Pourcentage Victoires","Pourcentage Victoires Domicile","Pourcentage Victoires Deplacement"),
                       Type=c("Offensive","Offensive","Offensive","Offensive","Offensive","Offensive","Defensive","Defensive","Defensive","Defensive","Defensive","Defensive","Les deux","Les deux","Les deux"))



# Team traitement #
team_st<-team
team_st$Team<-initiale_to_name(team_st$Team)

ind_t_st<-which(team_st$Year<2020)
team_st<-team_st[ind_t_st,]

ind_r4<-which(rankings3$Year>2002)
rankings4<-rankings3[ind_r4,]

strong_team<-merge(team_st,rankings4,by.x=c("Year","Team"),by.y=c("Year","team_name"))


years_clust<-c(unique(as.character(sort(team$Year))))


### Initialisation ### 

season_stats_PCA_S<-season_stats_PCA

### Selection Postes ###

#if(input$poste!="toutes les postes" && input$st_year!="toutes les saisons"){
  
 # indic_pos<-which(season_stats_PCA_S$Pos==input$poste)
 # season_stats_PCA_S<-season_stats_PCA_S[indic_pos,]
  
 # indic_year<-which(season_stats_PCA_S$Year==input$st_year)
#  season_stats_PCA_S<-season_stats_PCA_S[indic_year,]
#}

### Selection Saison ###

#elif(input$st_year!="toutes les saisons"){
  
#  season_stats_PCA_S<-season_stats_PCA
#  indi_year<-which(season_stats_PCA_S$Year==as.integer(input$st_year))
#  season_stats_PCA_S<-season_stats_PCA_S[indi_year,]
  
#}

#season_stats_PCA_S<-season_stats_PCA
#indi_year<-which(season_stats_PCA_S$Year==as.integer(2017))
#season_stats_PCA_S<-season_stats_PCA_S[indi_year,]

#elif(input$poste!="toutes les postes"){
  
#  season_stats_PCA_S<-season_stats_PCA
#  indic_pos<-which(season_stats_PCA_S$Pos==input$poste)
#  season_stats_PCA_S<-season_stats_PCA_S[indic_pos,]
  
#}

### Choix métrique ###

#print(season_stats_PCA_S[,-c(13:20)])
#jeu<-cbind(season_stats_PCA_S[,-c(13:20)],season_stats_PCA_S[,as.character(input$st)])
#colnames(jeu)[29]<-as.character(input$st)

#jeu<-cbind(season_stats_PCA_S[,-c(13:20)],season_stats_PCA_S[,"DBPM"])
#colnames(jeu)[29]<-as.character("DBPM")


#fit.lm <- lm(DBPM~.,data=jeu[-c(1:5,7)]) #attention il faudrait standardiser
#Aic.lin <- MASS::stepAIC(fit.lm, trace = FALSE ,direction = c("backward"))
### ACP ###
#library(FactoMineR)
#season_stats_PCA_S$Pos<-as.factor(season_stats_PCA_S$Pos)
#p <- MCA(season_stats_PCA_S[,-c(1:2,5)])
#jeu<-jeu[which(jeu$Pos=="PG"),]
#pca<-PCA(jeu[,-c(1,5:7)],quali.sup=c(1,2),quanti.sup=c(25),scale.unit=TRUE,graph=TRUE)
#fviz_pca_var(pca,"quali.var",col.var="contrib")
#plotellipses(pca)
#plot(pca,habillage=2,invisible="quali",title="",label="all")
#plot(pca,axes=c(1,2),choix="var", invisible = "ind")
#fviz_pca_var(pca,axes=c(1,2),col.var="contrib")
#library(factoextra)
#fviz_pca_ind(pca, label="none", habillage=2,
#                 addEllipses=TRUE, ellipse.level=0.95)
#fviz_pca_ind(pca, select.ind = list(name = c("Draymond Green")))
#pca$call
#fviz_pca_ind(pca, label=2, habillage=2)
#fviz_pca_ind(pca, habillage=2,mean.point=TRUE,label="none")

#fviz_pca_ind(pca, select.ind = list(cos2 = 50),habillage=2,mean.point=TRUE)
#jeu[6271,]
#fviz(pca, "col") 
#season_stats_PCA<-season_stats_PCA %>% drop_na()
### Afdm ###
#library(missMDA)
#estim_ncpFAMD(season_stats_PCA_S[,-c(1:2,5)])
#imputeFAMD(season_stats_PCA_S[,-c(1:2,5)])

#p<-imputeFAMD(season_stats_PCA_S)
### with missing values
#require(missMDA)

#res.impute <- imputeFAMD(season_stats_PCA_S, ncp=3) 
#res.afdm <- FAMD(season_stats_PCA_S,tab.disj=res.impute$tab.disj)
