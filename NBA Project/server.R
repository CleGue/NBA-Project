#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    #### BOX #####
    
    output$Champion <- renderValueBox({
        if(input$season_team!="toutes les saisons"){
            indice=which(MVP$Year==input$season_team)
            valueBox(
                VB_style( paste0( 'Champion: ',as.character(MVP$`NBA Champion`[indice])),  "font-size: 60%;"  ),
                VB_style(paste0("Results : ",as.character(MVP$Result[indice]))),
                icon = icon('export', lib = 'glyphicon'), #icon("sign-in"),
                color = "yellow"
            )
            
        }else{
        valueBox(
            VB_style( paste0( 'Champion: ',"Boston Celtics", "17" ),  "font-size: 60%;"  ),
            VB_style( paste0("Last Title : " ," 2008" )), 
            icon = icon('export', lib = 'glyphicon'), #icon("sign-in"),
            color = "yellow"
        )
        }
    })
    
    output$Eastern <- renderValueBox({
        if(input$season_team!="toutes les saisons"){
            indice=which(MVP$Year==input$season_team)
            valueBox(
                VB_style( paste0( 'Eastern Champion : ',as.character(MVP$`Eastern Champion`[indice])),  "font-size: 60%;"  ),
                VB_style(paste0("Saison :",as.character(input$season_team))), 
                icon = icon('export', lib = 'glyphicon'), #icon("sign-in"),
                color = "blue"
            )
        }else{
        valueBox(
            VB_style( paste0( 'Eastern Champion: ',"Boston Celtics", " 21" ),  "font-size: 60%;"  ),
            VB_style( paste0("Last Title : " ,"2008" )), 
            icon = icon('export', lib = 'glyphicon'), #icon("sign-in"),
            color = "blue"
        )
        }
    })
    
    output$Weastern <- renderValueBox({
        
        if(input$season_team!="toutes les saisons"){
            indice=which(MVP$Year==input$season_team)
            valueBox(
                VB_style( paste0( 'Weastern Champion : ',as.character(MVP$`Western Champion`[indice])),  "font-size: 60%;"  ),
                VB_style(paste0("Saison : ",as.character(input$season_team))), 
                icon = icon('export', lib = 'glyphicon'), #icon("sign-in"),
                color = "red"
            )
        }else{
        valueBox(
            VB_style( paste0( 'Weastern Champion: ',"Los Angeles Lakers", " 30" ),  "font-size: 60%;"  ),
            VB_style( paste0("Last Title : " ," 2010" )), 
            icon = icon('export', lib = 'glyphicon'), #icon("sign-in"),
            color = "red"
        )
        }
    })
    
    

    #### PLOT ###
    
    output$distPlot <- renderPlot({
        
        ### PLAYER ####
        
        if(input$type == "Player"){
            
            if(input$Choix_team!="toutes les équipes" & input$Choix_season!="toutes les saisons"){
                ind<-which(NBA_Salary_History_Players$Team==input$Choix_team & NBA_Salary_History_Players$Season==input$Choix_season)
            }
            else if(input$Choix_team=="toutes les équipes" & input$Choix_season=="toutes les saisons"){
                ind<-which(NBA_Salary_History_Players$Player!="NA")
            }
            else if(input$Choix_team=="toutes les équipes" & input$Choix_season!="toutes les saisons"){
                ind<-which(NBA_Salary_History_Players$Season==input$Choix_season)
            }
            else if(input$Choix_team!="toutes les équipes" & input$Choix_season=="toutes les saisons"){
                ind<-which(NBA_Salary_History_Players$Team==input$Choix_team)
            }
            if(input$Choix_player!="tous les joueurs"){
                ind<-which(NBA_Salary_History_Players$Player==input$Choix_player)
                #NBA_Salary_History_Players
                sal_m_play<-NBA_Salary_History_Players[ind,]
                
                # Barplot basique
                p<-ggplot(data=sal_m_play, aes(x=Season, y=Salary/10**6,fill=Salary/10**6)) +
                    geom_bar(stat="identity") + ylab("Salaire ") + xlab("Saison") + ggtitle(paste0("Evolution du salaire de : ",input$Choix_player))
                
                # Barplot horizontal
                p<-p + coord_flip()
                p
            }
            
            else{
                #NBA_Salary_History_Players
                sal_m_play<-aggregate(. ~ Player,NBA_Salary_History_Players[ind,-c(1:2)] , mean)
                
                #sal_m_play
                sal_m_play<-sal_m_play[order(-sal_m_play$Salary),]
                sal_m_play<-sal_m_play[1:input$`Number of Team/Player`,]
                
                # Barplot basique
                p<-ggplot(data=sal_m_play, aes(x=reorder(Player,Salary), y=Salary/10**6,fill=Salary/10**6)) +
                    geom_bar(stat="identity") + ylab("Salaire") + xlab("Joueur") + ggtitle("Classement des joueurs par salaire moyen en millions de dollars")
                
                # Barplot horizontal
                p<-p + coord_flip()
                p
            }
        }
        
        
        ### TEAM ###
        
        else if(input$type == "team"){
            if(input$Choix_team2!="toutes les équipes"){
                ind<-which(NBA_Salary_History$Team==input$Choix_team2)
                sal_m2<-NBA_Salary_History[ind,]
                # Barplot basique
                p<-ggplot(data=sal_m2, aes(x=Season, y=`Total Salary`/10**6,fill=`Total Salary`/10**6)) +
                    geom_bar(stat="identity") + ylab("Somme des salaires de l'équipe") + xlab("Equipe") + ggtitle("Classement des salaires par équipe en millions de dollars")
                
                # Barplot horizontal
                p<-p + coord_flip()
                p
            }
            else{
                ind<-which(NBA_Salary_History$`Salary Cap`!="NA")
                sal_m2<-aggregate(. ~ Team,NBA_Salary_History[ind,-1] , mean)
                
                sal_m2<-sal_m2[order(-sal_m2$`Total Salary`),]
                sal_m2<-sal_m2[1:input$`Number of Team/Player`,]
                
                # Barplot basique
                p<-ggplot(data=sal_m2, aes(x=reorder(Team, `Total Salary`), y=`Total Salary`/10**6,fill=`Total Salary`/10**6)) +
                    geom_bar(stat="identity") + ylab("Somme des salaires de l'équipe") + xlab("Equipe") + ggtitle("Classement des salaires par équipe en millions de dollars")
                
                # Barplot horizontal
                p<-p + coord_flip()
                p
            }
        }
        
    })
    
    output$distPlot_pred_sal <- renderPlot({
        
        if(input$choice_sal_pred=="evol"){
        
        colors_2 <- c("Salaire moyen" = "red", "Quantile d'ordre 0.10" = "lightblue", "Quantile d'ordre 0.25" = "blue", "Quantile d'ordre 0.75" = "blue", "Quantile d'ordre 0.90" = "lightblue")
        
        ggplot(data=sal_play,aes(x=Season,group=1))+
            geom_line(aes(y=Salary/10**6,color="Salaire moyen"))+
            geom_line(aes(y=Salary5/10**6,color="Quantile d'ordre 0.10"),linetype="dashed")+
            geom_line(aes(y=Salary25/10**6,color="Quantile d'ordre 0.25"),linetype="dashed")+
            geom_line(aes(y=Salary75/10**6,color="Quantile d'ordre 0.75"),linetype="dashed")+
            geom_line(aes(y=Salary95/10**6,color="Quantile d'ordre 0.90"),linetype="dashed")+
            xlab("Saison")+
            ylab("Salaire en millions de dollars")+
            ggtitle("Evolution du salaire en millions de dollars")+
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
            #labs(color = "Légende")+
            scale_color_manual(values = colors_2)
        }
        
        else{
            plot(forecast(fit,h=12),ylab="Salaire moyen NBA (millions de dollars)",
                 main="Prédictions du salaire moyen NBA ARIMA(0,1,0)")
            
        }
    })
    
    ### Evolution du basket ###
    
    output$distPlotly <- renderPlotly({
        if(input$Choix_Top=="TOP"){
            if(input$evol!="Matches" & input$evol!="BPM"){
                pts_par_poste<-ggplot(data=season_stats4,aes(x=Year,y=(!!input$evol/Matches),colour=Pos,group=Pos))+geom_line()+xlab("Saison")+ylab(paste0("Moyenne de ", input$evol))+ggtitle(paste0(paste0("Moyenne de ", input$evol),  " par matches par poste"))
                ggplotly(pts_par_poste)
            }
            else{
                pts_par_poste<-ggplot(data=season_stats4,aes(x=Year,y=(!!input$evol),colour=Pos,group=Pos))+geom_line()+xlab("Saison")+ylab(paste0("", input$evol))+ggtitle(paste0(input$evol,  " par poste"))
                ggplotly(pts_par_poste)
            }
        }
        else{
            if(input$evol!="Matches" & input$evol!="BPM"){
                pts_par_poste<-ggplot(data=season_stats5,aes(x=Year,y=(!!input$evol/Matches),colour=Pos,group=Pos))+geom_line()+xlab("Saison")+ylab(paste0("Moyenne de ", input$evol))+ggtitle(paste0(paste0("Moyenne de ", input$evol),  " par matches par poste"))
                ggplotly(pts_par_poste)
            }
            else{
                pts_par_poste<-ggplot(data=season_stats5,aes(x=Year,y=(!!input$evol),colour=Pos,group=Pos))+geom_line()+xlab("Saison")+ylab(paste0("", input$evol))+ggtitle(paste0(input$evol,  " par poste"))
                ggplotly(pts_par_poste)
            }
        }
    })
    
    output$distPlotly2 <- renderPlotly({
        
        if(input$season_team!="toutes les saisons"){
            ind<-which(rankings3$Year==input$season_team)
            victory<-ggplot(data=rankings3[ind,],aes(x=reorder(team_name,!!input$stat_team),y=round(!!input$stat_team*100,2),fill=!!input$stat_team))+geom_bar(stat="identity")+xlab("Team")+ylab("% Victoire")+ggtitle("Pourcentage de Victoire")
            victory<-victory + coord_flip()
            ggplotly(victory)
        }
        else{
            victory<-ggplot(data=rankings2,aes(x=reorder(team_name,!!input$stat_team),y=round(!!input$stat_team*100,2),fill=!!input$stat_team))+geom_bar(stat="identity")+xlab("Team")+ylab("% Victoire")+ggtitle("Pourcentage de Victoire")
            victory<-victory +coord_flip()
            ggplotly(victory)
        }
    })
    
    ### STATS COUNTRY ###
    
    output$distPlotly3 <- renderPlotly({
        pays<-ggplot(data=season_stats_country,aes(x=Year,color=country,group=country))+geom_line(stat="count")+xlab("Saison")+ylab("Number of Player")+ggtitle("Nombre de joueur par an par nationalité")
        ggplotly(pays)
    })
    
    ### PRédictions Country ###
    
    output$distPlot_stranger <- renderPlot({

        if(input$choice_country_pred=="evol"){
            
            ggplot(data=country_prop,aes(x=Annee,y=Prop,color=Pays,group=Pays))+
                geom_line(stat="identity")+xlab("Saison")+ylab("Proportion de joueurs")+
                ggtitle("Evolution de l'origine des joueurs en NBA")+
                scale_y_continuous(labels = scales::percent)+
                theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
        }
        
        else{
            
            plot(forecast(fit2,h=12),ylab="proportion de joueurs non américain en NBA",
                 main="Prédiction joueurs étrangers en NBA ARIMA(0,1,0)")
            
        }
    })
    
    
    
    ### records ###
    
    output$distPlot_Records <- renderPlot({
        
        if(input$records=="Points"){
            pts<-season_stats7[order(-season_stats7$sum_PTS),]
            pts<-pts[1:input$Numbertop,]
            p<-ggplot(data=pts, aes(x=reorder(Player,sum_PTS), y=sum_PTS,label=sum_PTS)) +
                geom_bar(stat="identity",fill="green") +
                #,fill = "red"
                geom_text(size = 5, position = position_stack(vjust = 0.5))+ ylab("Points Marqués") + xlab("Joueurs") + ggtitle(paste0(paste0("Classement des joueurs par ",input$records)," depuis 1950"))
        }
        else if(input$records=="Bloquages"){
            pts<-season_stats7[order(-season_stats7$sum_BLK),]
            pts<-pts[1:input$Numbertop,]
            p<-ggplot(data=pts, aes(x=reorder(Player,sum_BLK), y=sum_BLK,label=sum_BLK)) +
                geom_bar(stat="identity") + ylab("Bloquages Effectués") + xlab("Joueurs") + ggtitle(paste0(paste0("Classement des joueurs par ",input$records)," depuis 1974"))+
                geom_text(size = 5, position = position_stack(vjust = 0.5))
            
        }
        else if(input$records=="Interceptions"){
            pts<-season_stats7[order(-season_stats7$sum_STL),]
            pts<-pts[1:input$Numbertop,]
            p<-ggplot(data=pts, aes(x=reorder(Player,sum_STL), y=sum_STL,label=sum_STL)) +
                geom_bar(stat="identity") + ylab("Interceptions Effectués") + xlab("Joueurs") + ggtitle(paste0(paste0("Classement des joueurs par ",input$records)," depuis 1974"))+
                geom_text(size = 5, position = position_stack(vjust = 0.5))
            
        }
        else if(input$records=="Assists"){
            pts<-season_stats7[order(-season_stats7$sum_AST),]
            pts<-pts[1:input$Numbertop,]
            p<-ggplot(data=pts, aes(x=reorder(Player,sum_AST), y=sum_AST,label=sum_AST)) +
                geom_bar(stat="identity") + ylab("Passes Décisives Effectués") + xlab("Joueurs") + ggtitle(paste0(paste0("Classement des joueurs par ",input$records)," depuis 1950"))+
                geom_text(size = 5, position = position_stack(vjust = 0.5))
            
        }
        else if(input$records=="Matches"){
            pts<-season_stats7[order(-season_stats7$sum_G),]
            pts<-pts[1:input$Numbertop,]
            p<-ggplot(data=pts, aes(x=reorder(Player,sum_G), y=sum_G,label=sum_G)) +
                geom_bar(stat="identity") + ylab("Matches Joués") + xlab("Joueurs") + ggtitle(paste0(paste0("Classement des joueurs par ",input$records)," depuis 1950"))+
                geom_text(size = 5, position = position_stack(vjust = 0.5))
            
        }
        else if(input$records=="Saisons"){
            season_stats6$MPPG<-season_stats6$MP/season_stats6$G
            season_stats8<-season_stats6 %>%
                filter(MPPG>30) %>%
                filter(G>50)
            season_stats8$PYear<-paste0(paste0(season_stats8$Player,' - '),season_stats8$Year)
            pts<-season_stats8[order(-season_stats8$BPM),]
            pts<-pts[1:input$Numbertop,]
            p<-ggplot(data=pts, aes(x=reorder(PYear,BPM), y=BPM,label=BPM)) +
                geom_bar(stat="identity") + ylab("BPM") + xlab("Joueurs") + ggtitle(paste0(paste0("Classement des meilleures ",input$records)," réalisées depuis 1950"))+
                geom_text(size = 5, position = position_stack(vjust = 0.5))
            
        }
        p<-p + coord_flip()
        p
    })
    
    #### AGE ###
    
    output$distPlot5 <- renderPlot({
        
        if(input$choice_age=="tous les ages"){
            
            if(input$nb_play_check!="yes" || length(input$nb_play_check)==0){
                dat_age<-data_age_draft
            }
            
            else{
                indice_a<-which(data_age_draft$`Nombre d'annees en NBA`<=input$NOYP)
                dat_age<-data_age_draft[indice_a,]
                
            }
            dat_age<-aggregate(. ~ Age, dat_age[,c(12,13,14,24,25,23,42,46,49)], mean)
            
            ggplot(data=dat_age,aes(x=Age,y=(!!input$stat_age)))+geom_line(stat="identity")+
                xlab("Age")+ylab(paste0(input$stat_age,""))+
                ggtitle(paste0(input$stat_age," en fonction de l'age"))+scale_x_continuous(breaks=seq(18,40,1))+
                geom_point()
        }
        
        else{
            
            ind<-which(data_age_draft2$Age==input$choice_age)
            dat_age<-data_age_draft2[ind,]
            
            if(input$nb_play_check=="yes" && length(input$nb_play_check)!=0){
                
                indice_a<-which(dat_age$`Nombre d'annees en NBA`<=input$NOYP)
                dat_age<-dat_age[indice_a,]
            }
            
            ggplot(data=dat_age,aes(x=`Nombre d'annees en NBA`,y=(!!input$stat_age)))+geom_bar(stat="identity", width=0.5, fill="darkred")+
                xlab("Nombre d'annees en NBA")+ylab(paste0(input$stat_age,""))+
                ggtitle(paste0(input$stat_age," en fonction du nombre d'années passé en NBA pour les joueurs de ",input$choice_age," ans"))+
                scale_x_continuous(breaks=seq(1,max(dat_age$`Nombre d'annees en NBA`),1))
        }
    })
    
    output$distPlot_age_quantile<- renderPlot({
        
        colors <- c("Moyenne" = "red", "Quantile d'ordre 0.10" = "lightblue", "Quantile d'ordre 0.25" = "blue", "Quantile d'ordre 0.75" = "blue", "Quantile d'ordre 0.90" = "lightblue")
        ggplot(NULL)+
            geom_line(data=age,aes(x=Age,y=(!!input$stat_age),color="Moyenne"))+
            geom_line(data=t_age_2,aes(x=Age,y=(!!input$stat_age),color="Quantile d'ordre 0.10"),linetype="dashed")+
            geom_line(data=t_age_3,aes(x=Age,y=(!!input$stat_age),color="Quantile d'ordre 0.25"),linetype="dashed")+
            geom_line(data=t_age_4,aes(x=Age,y=(!!input$stat_age),color="Quantile d'ordre 0.75"),linetype="dashed")+
            geom_line(data=t_age_5,aes(x=Age,y=(!!input$stat_age),color="Quantile d'ordre 0.90"),linetype="dashed")+
            xlab("Age")+
            ylab(paste0(input$stat_age,""))+
            ggtitle(paste0(input$stat_age," en fonction de l'age"))+
            scale_color_manual(values = colors)
    })
    
    #### DRAFT ###
    
    output$distPlot6 <- renderPlot({
        if(input$choice_draft=="toutes les drafts"){
            
            if(input$type_draft!="Draft"){
                
                draft2<-d_draft
                
                draft2$draft_number<-as.integer(draft2$draft_number)
                
                draft2<- draft2 %>%
                    filter(!is.na(draft_number))
                
                if(input$type_draft=="Round 1"){
                    
                    indice_r<-which(as.integer(draft2$draft_round)==1)
                    draft2<-draft2[indice_r,]
                    
                    indice_nb<-which(draft2$draft_number<=input$NumberDraft)
                    draft2<-draft2[indice_nb,]
                    
                    draft2<-aggregate(. ~ draft_year + draft_round, draft2 , mean)
                }
                if(input$type_draft=="Round 2"){
                    
                    indice_r<-which(as.integer(draft2$draft_round)==2)
                    draft2<-draft2[indice_r,]
                    
                    indice_nb<-which(draft2$draft_number<=input$NumberDraft)
                    draft2<-draft2[indice_nb,]
                    
                    draft2<-aggregate(. ~ draft_year + draft_round, draft2 , mean)
                }
            }
            else{
                
                draft2<-as.data.frame(aggregate(. ~ draft_year,d_draft[,-c(2:3)] , mean))
                
            }
            
            
            ggplot(data=draft2,aes(x=reorder(draft_year,draft2[,as.character(input$stat_draft)]),y=draft2[,as.character(input$stat_draft)]))+geom_bar(stat="identity")+
                xlab("Year")+ylab("Stats")+
                ggtitle("Statistiques en fonction de l'années de draft")
        }
        else{
            draft2<-d_draft
            
            draft2<-aggregate(. ~ draft_year + draft_round,d_draft[,-3] , mean)
            
            indice_draft<-which(draft2$draft_year==input$choice_draft)
            draft2<-draft2[indice_draft,]
            
            ggplot(data=draft2,aes(x=draft_round,y=draft2[,as.character(input$stat_draft)]))+geom_bar(stat="identity")+
                xlab("Draft")+ylab("Stats")+
                ggtitle(paste0("Statistiques de la draft ",input$choice_draft))
        }
    })
     
    ### ACP Strong Player ###
    
    output$distPlot_ACP <- renderPlot({
        
        ### Initialisation ### 
        
        season_stats_PCA_S<-season_stats_PCA
        
        ### Selection Postes ###
        
        if(input$poste!="toutes les postes" && input$st_year!="toutes les saisons"){
            
            indic_pos<-which(season_stats_PCA_S$Pos==input$poste)
            season_stats_PCA_S<-season_stats_PCA_S[indic_pos,]
            
            indic_year<-which(season_stats_PCA_S$Year==input$st_year)
            season_stats_PCA_S<-season_stats_PCA_S[indic_year,]
        }
        
        ### Selection Saison ###
        
        else if(input$st_year!="toutes les saisons"){

            season_stats_PCA_S<-season_stats_PCA
            indi_year<-which(season_stats_PCA_S$Year==as.integer(input$st_year))
            season_stats_PCA_S<-season_stats_PCA_S[indi_year,]
            
        }
        
        else if(input$poste!="toutes les postes"){
            
            season_stats_PCA_S<-season_stats_PCA
            indic_pos<-which(season_stats_PCA_S$Pos==input$poste)
            season_stats_PCA_S<-season_stats_PCA_S[indic_pos,]
            
        }
        
        ### Choix métrique ###
        
        #print(season_stats_PCA_S[,-c(13:20)])
        
        jeu<-cbind(season_stats_PCA_S[,-c(13:20)],season_stats_PCA_S[,as.character(input$st)])
        colnames(jeu)[29]<-as.character(input$st)
        
        ### ACP ###
        
        pca<-PCA(jeu,quali.sup=c(1:8),quanti.sup=c(29),scale.unit=TRUE,graph=FALSE)
        fviz_pca_var(pca,axes=c(1,2),col.var="contrib")
        
    })
    
    output$distPlot_ACP_pf <- renderPlot({
        
        ### Initialisation ### 
        
        season_stats_PCA_S<-season_stats_PCA
        
        ### Selection Postes ###
        
        if(input$poste!="toutes les postes" && input$st_year!="toutes les saisons"){
            
            indic_pos<-which(season_stats_PCA_S$Pos==input$poste)
            season_stats_PCA_S<-season_stats_PCA_S[indic_pos,]
            
            indic_year<-which(season_stats_PCA_S$Year==input$st_year)
            season_stats_PCA_S<-season_stats_PCA_S[indic_year,]
        }
        
        ### Selection Saison ###
        
        if(input$st_year!="toutes les saisons"){
            
            season_stats_PCA_S<-season_stats_PCA
            indi_year<-which(season_stats_PCA_S$Year==as.integer(input$st_year))
            season_stats_PCA_S<-season_stats_PCA_S[indi_year,]
            
        }
        
        else if(input$poste!="toutes les postes"){
            
            season_stats_PCA_S<-season_stats_PCA
            indic_pos<-which(season_stats_PCA_S$Pos==input$poste)
            season_stats_PCA_S<-season_stats_PCA_S[indic_pos,]
            
        }
        
        ### Choix métrique ###
        
        #print(season_stats_PCA_S[,-c(13:20)])
        
        jeu<-cbind(season_stats_PCA_S[,-c(13:20)],season_stats_PCA_S[,as.character(input$st)])
        colnames(jeu)[29]<-as.character(input$st)
        
        ### ACP ###
        
        pca<-PCA(jeu,quali.sup=c(1:8),quanti.sup=c(29),scale.unit=TRUE,graph=FALSE)
        fviz_pca_ind(pca, label="none", habillage=3,
                                      addEllipses=TRUE, ellipse.level=0.95)
        
    })
    ### ACP Strong Team ###
    
    output$distPlot_ACP_team <- renderPlot({
        
        ### Initialisation ### 
        
        st_team<-strong_team
        
        ### Selection Postes ###
        
        if(input$st_year_team!="toutes les saisons"){
            
            indic_year<-which(st_team$Year==input$st_year_team)
            st_team<-st_team[indic_year,]
        }
        
        ### Choix métrique ###
        
        
        jeu<-cbind(st_team[,-c(15:17)],st_team[,as.character(input$str_team)])
        colnames(jeu)[15]<-as.character(input$str_team)
        
        ### ACP ###
        
        pca<-PCA(jeu,quali.sup=c(1:2),quanti.sup=c(15),scale.unit=TRUE,graph=FALSE)
        fviz_pca_var(pca,axes=c(1,2),col.var="contrib")
        
    })
    
    ### FIN ACP Strong Team
    
    ####  LIEN  ####
    
    output$distPlot_lien <- renderPlot({
        
        team_l1<-team
        team_l1<-aggregate(. ~ Team,team_l1[,-2] , mean)
        
        if(input$seas!="toutes les années"){
            ind<-which(team$Year==input$seas)
            team_l1<-team[ind,]
        }  
        
        n_l1 <- nrow(team_l1)
        Z_lien <- scale(team_l1[,-c(1:2)], center=TRUE,scale=TRUE)*sqrt(n_l1/(n_l1-1))
        
        d_l1 <- dist(Z_lien)
        tree_lien <- hclust(d_l1^2/(2*n_l1), method="ward.D") #les individus sont pondérés par 1/n
        
        #plot(tree_lien, hang=-1, main="CAH de Ward", sub="", xlab="",labels=team_l1$Team)
        #rect.hclust(tree_lien, k=4)
        
        tete<-as.dendrogram(tree_lien)
        labels(tete)<-team_l1$Team[tree_lien$order]
        tete %>% set("labels_col", value = c("green", "blue","red","brown"), k=4) %>% 
            plot(main = "CAH de Ward")
        rect.dendrogram(tete,k=4)
    })
    output$text_cluster_team <- renderUI({ 
        team_l1<-team
        team_l1<-aggregate(. ~ Team,team_l1[,-2] , mean)
        
        if(input$seas!="toutes les années"){
            ind<-which(team$Year==input$seas)
            team_l1<-team[ind,]
        }  
        
        n_l1 <- nrow(team_l1)
        Z_lien <- scale(team_l1[,-c(1:2)], center=TRUE,scale=TRUE)*sqrt(n_l1/(n_l1-1))
        
        d_l1 <- dist(Z_lien)
        tree_lien <- hclust(d_l1^2/(2*n_l1), method="ward.D") #les individus sont pondérés par 1/n
        
        plot(tree_lien, hang=-1, main="CAH de Ward", sub="", xlab="",labels=team_l1$Team)
        rect.hclust(tree_lien, k=4)
        part_lien_text <- cutree(tree_lien,k=4)
        ind_c1<-part_lien_text==1
        c<-team_l1[ind_c1,1]
        str1<-"Cluster 1 :"
        str2<-c[1]
        if(length(c)>1){
            for (j in c[2:length(c)]){
                str2<-paste(str2,j,sep=", ")
            }}
        ind_c2<-part_lien_text==2
        c2<-team_l1[ind_c2,1]
        str3<-'Cluster 2 :'
        str4<-c2[1]
        if(length(c2)>1){
            for (j in c2[2:length(c2)]){
                str4<-paste(str4,j,sep=", ")
            }}
        ind_c3<-part_lien_text==3
        c3<-team_l1[ind_c3,1]
        str5<-'Cluster 3 :'
        str6<-c3[1]
        if(length(c3)>1){
            for (j in c3[2:length(c3)]){
                str6<-paste(str6,j,sep=", ")
            }}
        ind_c4<-part_lien_text==4
        c4<-team_l1[ind_c4,1]
        str7<-'Cluster 4 :'
        str8<-c4[1]
        if(length(c4)>1){
            for (j in c4[2:length(c4)]){
                str8<-paste(str8,j,sep=", ")
            }}
        HTML(paste("</br>",paste0("<div style='height: 20","'>",paste(paste(paste(paste(paste(paste(paste(str1, str2, sep = ' '),str3,sep='<br/>'),str4,sep=" "),str5,sep='<br/>'),str6,sep=" "),str7,sep='<br/>'),str8,sep=" "))))
    })
    
    output$distPlot_lien2 <- renderPlot({
        
        team_lien2<-team
        team_lien2<-aggregate(. ~ Team,team_lien2[,-2] , mean)
        
        if(input$seas!="toutes les années"){
            ind<-which(team$Year==input$seas)
            team_lien2<-team[ind,]
        }  
        
        n_l2 <- nrow(team_lien2)
        Z_lien2 <- scale(team_lien2[,-c(1:2)], center=TRUE,scale=TRUE)*sqrt(n_l2/(n_l2-1))
        
        d_l2 <- dist(Z_lien2)
        tree_lien2 <- hclust(d_l2^2/(2*n_l2), method="ward.D") #les individus sont pondérés par 1/n
        
        K_lien2 <- 4
        part_lien2 <- cutree(tree_lien2,k=K_lien2)
        part_lien2 <- as.factor(part_lien2)
        levels(part_lien2) <- paste("cluster",1:K_lien2,sep="")
        desc_lien2 <- catdes(data.frame(part_lien2,team_lien2[,-1]),
                             num.var=1)
        plot(desc_lien2,barplot = TRUE)
        
    })
    
    output$distPlot_lien3 <- renderPlot({
        
        s7_l3_tab<-season_stats_PCA[,-c(6,7,10)]
        
        if(input$seas_play!="toutes les années"){
            ind<-which(s7_l3_tab$Year==as.integer(input$seas_play))
            s7_l3_tab<-s7_l3_tab[ind,]
        }
        
        n3 <- nrow(s7_l3_tab)
        Z3 <- scale(s7_l3_tab[,-c(1:3,5)], center=TRUE,scale=TRUE)*sqrt(n3/(n3-1))
        
        d3 <- dist(Z3)
        tree3 <- hclust(d3^2/(2*n3), method="ward.D") #les individus sont pondérés par 1/n
        
        K3 <- 4
        part3 <- cutree(tree3,k=K3)
        part3 <- as.factor(part3)
        levels(part3) <- paste("cluster",1:K3,sep="")
        s7_l3_tab2<-cbind(part3,s7_l3_tab)
        
        desc_l3 <- catdes(data.frame(part3,s7_l3_tab[,-c(1:3,5)]),
                          num.var=1)
        plot(desc_l3,barplot=TRUE)
        
    })
    
    output$distPlot_lien_effect2 <- renderPlot({
        
        s7_l3_tab2<-season_stats_PCA[,-c(6,7,10)]
        
        if(input$seas_play!="toutes les années"){
            ind<-which(s7_l3_tab2$Year==as.integer(input$seas_play))
            s7_l3_tab2<-s7_l3_tab2[ind,]
        }
        
        n_le2 <- nrow(s7_l3_tab2)
        Z_le2 <- scale(s7_l3_tab2[,-c(1:3,5)], center=TRUE,scale=TRUE)*sqrt(n_le2/(n_le2-1))
        
        d_le2 <- dist(Z_le2)
        tree_le2 <- hclust(d_le2^2/(2*n_le2), method="ward.D") 
        
        K_le2 <- 4
        part <- cutree(tree_le2,k=K_le2)
        part <- as.factor(part)
        levels(part) <- paste("cluster",1:K_le2,sep="")
        s7_l3_all<-cbind(part,s7_l3_tab2)
        
        
        nodata_le2<-build_array(s7_l3_all,4)
        
        n_le2_bis <- nrow(nodata_le2)
        Z_le2_bis <- scale(nodata_le2[,-1], center=TRUE,scale=TRUE)*sqrt(n_le2_bis/(n_le2_bis-1))
        
        d_le2_bis <- dist(Z_le2_bis)
        tree_le2_bis <- hclust(d_le2_bis^2/(2*n_le2_bis), method="ward.D")
        
        K_l3_bis <- 4
        part_l3_bis <- cutree(tree_le2_bis,k=K_l3_bis)
        part_l3_bis <- as.factor(part_l3_bis)
        levels(part_l3_bis) <- paste("cluster",1:K_l3_bis,sep="")
        desc_le2_bis<- catdes(data.frame(part_l3_bis,nodata_le2[,-1]),
                              num.var=1)
        plot(desc_le2_bis,barplot=TRUE)
        
    })
    
    output$distPlot_lien_effect <- renderPlot({
        
        s_le<-season_stats_PCA[,-c(6,7,10)]
        
        if(input$seas_play!="toutes les années"){
            ind<-which(s_le$Year==as.integer(input$seas_play) & s_le$Tm!="TOR")
            s_le<-s_le[ind,]
            
        }
        else{
            ind<-which(s_le$Tm!="TOR") 
            s_le<-s_le[ind,]
        }
        
        nle <- nrow(s_le)
        Zle <- scale(s_le[,-c(1:3,5)], center=TRUE,scale=TRUE)*sqrt(nle/(nle-1))
        
        dle <- dist(Zle)
        treele <- hclust(dle^2/(2*nle), method="ward.D") 
        
        K3 <- 4
        part <- cutree(treele,k=K3)
        part <- as.factor(part)
        levels(part) <- paste("cluster",1:K3,sep="")
        s_all<-cbind(part,s_le)
        
        nodatale<-build_array(s_all,4)
        
        nle_bis <- nrow(nodatale)
        Zle_bis <- scale(nodatale[,-1], center=TRUE,scale=TRUE)*sqrt(nle_bis/(nle_bis-1))
        
        dle_bis <- dist(Zle_bis)
        treele_bis <- hclust(dle_bis^2/(2*nle_bis), method="ward.D") 
        
        plot(treele_bis, hang=-1, main="CAH de Ward", sub="", xlab="",labels=nodatale[,1])
        rect.hclust(treele_bis, k=4)
        
    })
    output$text_cluster_player <- renderUI({ 
        s_le<-season_stats_PCA[,-c(6,7,10)]
        
        if(input$seas_play!="toutes les années"){
            
            ind<-which(s_le$Year==as.integer(input$seas_play) & s_le$Tm!="TOR")
            s_le<-s_le[ind,]
            
        }
        else{
            ind<-which(s_le$Tm!="TOR")
            s_le<-s_le[ind,] 
        }
        
        nle <- nrow(s_le)
        Zle <- scale(s_le[,-c(1:3,5)], center=TRUE,scale=TRUE)*sqrt(nle/(nle-1))
        
        dle <- dist(Zle)
        treele <- hclust(dle^2/(2*nle), method="ward.D") 
        
        K3 <- 4
        part <- cutree(treele,k=K3)
        part <- as.factor(part)
        levels(part) <- paste("cluster",1:K3,sep="")
        s_all<-cbind(part,s_le)
        
        nodatale<-build_array(s_all,4)
        
        nle_bis <- nrow(nodatale)
        Zle_bis <- scale(nodatale[,-1], center=TRUE,scale=TRUE)*sqrt(nle_bis/(nle_bis-1))
        
        dle_bis <- dist(Zle_bis)
        treele_bis <- hclust(dle_bis^2/(2*nle_bis), method="ward.D") 
        
        plot(treele_bis, hang=-1, main="CAH de Ward", sub="", xlab="",labels=nodatale[,1])
        rect.hclust(treele_bis, k=4)
        
        
        part_lien_text <- cutree(treele_bis,k=4)
        ind_c1<-part_lien_text==1
        c<-nodatale[ind_c1,1]
        str1<-"Cluster 1 :"
        str2<-c[1]
        if(length(c)>1){
            for (j in c[2:length(c)]){
                str2<-paste(str2,j,sep=", ")
            }}
        ind_c2<-part_lien_text==2
        c2<-nodatale[ind_c2,1]
        str3<-'Cluster 2 :'
        str4<-c2[1]
        if(length(c2)>1){
            for (j in c2[2:length(c2)]){
                str4<-paste(str4,j,sep=", ")
            }}
        ind_c3<-part_lien_text==3
        c3<-nodatale[ind_c3,1]
        str5<-'Cluster 3 :'
        str6<-c3[1]
        if(length(c3)>1){
            for (j in c3[2:length(c3)]){
                str6<-paste(str6,j,sep=", ")
            }}
        ind_c4<-part_lien_text==4
        c4<-nodatale[ind_c4,1]
        str7<-'Cluster 4 :'
        str8<-c4[1]
        if(length(c4)>1){
            for (j in c4[2:length(c4)]){
                str8<-paste(str8,j,sep=", ")
            }}
        HTML(paste("</br>",paste0("<div style='height: 20","'>",paste(paste(paste(paste(paste(paste(paste(str1, str2, sep = ' '),str3,sep='<br/>'),str4,sep=" "),str5,sep='<br/>'),str6,sep=" "),str7,sep='<br/>'),str8,sep=" "))))
    })
    
    
    
    output$distPlot_lien4 <- renderPlot({
        
        s_tp<-season_stats_PCA[,-c(6,7,10)]
        
        if(input$seas_tp!="toutes les années"){
            
            ind<-which(s_tp$Year==as.integer(input$seas_tp) & s_tp$Tm!="TOR")
            s_tp<-s_tp[ind,]
        }
        else{
            ind<-which(s_tp$Tm!="TOR")
            s_tp<-s_tp[ind,]
        }
        
        n_tp <- nrow(s_tp)
        Z_tp <- scale(s_tp[,-c(1:3,5)], center=TRUE,scale=TRUE)*sqrt(n_tp/(n_tp-1))
        
        d_tp <- dist(Z_tp)
        tree_tp <- hclust(d_tp^2/(2*n_tp), method="ward.D") #les individus sont pondérés par 1/n
        
        K_l3 <- 4
        part <- cutree(tree_tp,k=K_l3)
        part <- as.factor(part)
        levels(part) <- paste("cluster",1:K_l3,sep="")
        s_tp_all<-cbind(part,s_tp)
        
        nodata_tp<-build_array(s_tp_all,4)
        
        team_lien2<-team
        team_lien2<-aggregate(. ~ Team,team_lien2[,-2] , mean)
        
        if(input$seas_tp!="toutes les années"){
            ind<-which(team$Year==input$seas_tp & team$Team!="TOR")
            team_lien2<-team[ind,]
        }
        else{
            ind<-which(team$Team!="TOR")
            team_lien2<-team[ind,]
        }
        
        team_2<-merge(nodata_tp, team_lien2, by.x=c("Team"),by.y=c("Team"))
        
        n4 <- nrow(team_2)
        Z4 <- scale(team_2[,-1], center=TRUE,scale=TRUE)*sqrt(n4/(n4-1))
        
        d4 <- dist(Z4)
        tree_l4 <- hclust(d4^2/(2*n4), method="ward.D") #les individus sont pondérés par 1/n
        
        plot(tree_l4, hang=-1, main="CAH de Ward", sub="", xlab="",labels=team_2$Team)
        rect.hclust(tree_l4, k=4)
        
    }) 
    output$text_cluster_player_team <- renderUI({ 
        s_tp<-season_stats_PCA[,-c(6,7,10)]
        
        if(input$seas_tp!="toutes les années"){
            
            ind<-which(s_tp$Year==as.integer(input$seas_tp) & s_tp$Tm!="TOR")
            s_tp<-s_tp[ind,]
        }
        else{
            ind<-which(s_tp$Tm!="TOR")
            s_tp<-s_tp[ind,]
        }
        
        n_tp <- nrow(s_tp)
        Z_tp <- scale(s_tp[,-c(1:3,5)], center=TRUE,scale=TRUE)*sqrt(n_tp/(n_tp-1))
        
        d_tp <- dist(Z_tp)
        tree_tp <- hclust(d_tp^2/(2*n_tp), method="ward.D") #les individus sont pondérés par 1/n
        
        K_l3 <- 4
        part <- cutree(tree_tp,k=K_l3)
        part <- as.factor(part)
        levels(part) <- paste("cluster",1:K_l3,sep="")
        s_tp_all<-cbind(part,s_tp)
        
        nodata_tp<-build_array(s_tp_all,4)
        
        team_lien2<-team
        team_lien2<-aggregate(. ~ Team,team_lien2[,-2] , mean)
        
        if(input$seas_tp!="toutes les années"){
            ind<-which(team$Year==input$seas_tp & team$Team!="TOR")
            team_lien2<-team[ind,]
        }
        else{
            ind<-which(team$Team!="TOR")
            team_lien2<-team[ind,]
        }
        
        team_2<-merge(nodata_tp, team_lien2, by.x=c("Team"),by.y=c("Team"))
        
        n4 <- nrow(team_2)
        Z4 <- scale(team_2[,-1], center=TRUE,scale=TRUE)*sqrt(n4/(n4-1))
        
        d4 <- dist(Z4)
        tree_l4 <- hclust(d4^2/(2*n4), method="ward.D") #les individus sont pondérés par 1/n
        
        plot(tree_l4, hang=-1, main="CAH de Ward", sub="", xlab="",labels=team_2$Team)
        rect.hclust(tree_l4, k=4)
        
        
        part_lien_text <- cutree(tree_l4,k=4)
        ind_c1<-part_lien_text==1
        c<-team_2$Team[ind_c1]
        #c<-nodatale[ind_c1,1]
        str1<-"Cluster 1 :"
        str2<-c[1]
        if(length(c)>1){
            for (j in c[2:length(c)]){
                str2<-paste(str2,j,sep=", ")
            }}
        ind_c2<-part_lien_text==2
        c2<-team_2$Team[ind_c2]
        str3<-'Cluster 2 :'
        str4<-c2[1]
        if(length(c2)>1){
            for (j in c2[2:length(c2)]){
                str4<-paste(str4,j,sep=", ")
            }}
        ind_c3<-part_lien_text==3
        c3<-team_2$Team[ind_c3]
        str5<-'Cluster 3 :'
        str6<-c3[1]
        if(length(c3)>1){
            for (j in c3[2:length(c3)]){
                str6<-paste(str6,j,sep=", ")
            }}
        ind_c4<-part_lien_text==4
        c4<-team_2$Team[ind_c4]
        str7<-'Cluster 4 :'
        str8<-c4[1]
        if(length(c4)>1){
            for (j in c4[2:length(c4)]){
                str8<-paste(str8,j,sep=", ")
            }}
        HTML(paste("</br>",paste0("<div style='height: 20","'>",paste(paste(paste(paste(paste(paste(paste(str1, str2, sep = ' '),str3,sep='<br/>'),str4,sep=" "),str5,sep='<br/>'),str6,sep=" "),str7,sep='<br/>'),str8,sep=" "))))
    })
    
    
    
    
    
    
    
    
    
    
    output$distPlot_lien5 <- renderPlot({
        
        s_tp2<-season_stats_PCA[,-c(6,7,10)]
        
        if(input$seas_tp!="toutes les années"){
            
            ind<-which(s_tp2$Year==as.integer(input$seas_tp))
            s_tp2<-s_tp2[ind,]
        }
        
        n_tp2 <- nrow(s_tp2)
        Z_tp2 <- scale(s_tp2[,-c(1:3,5)], center=TRUE,scale=TRUE)*sqrt(n_tp2/(n_tp2-1))
        
        d_tp2 <- dist(Z_tp2)
        tree_tp2 <- hclust(d_tp2^2/(2*n_tp2), method="ward.D") #les individus sont pondérés par 1/n
        
        K_l3 <- 4
        part <- cutree(tree_tp2,k=K_l3)
        part <- as.factor(part)
        levels(part) <- paste("cluster",1:K_l3,sep="")
        s_tp2_all<-cbind(part,s_tp2)
        
        nodata_tp2<-build_array(s_tp2_all,4)
        
        team_lien2<-team
        team_lien2<-aggregate(. ~ Team,team_lien2[,-2] , mean)
        
        if(input$seas_tp!="toutes les années"){
            ind<-which(team$Year==input$seas_tp)
            team_lien2<-team[ind,]
        }  
        
        team_2_bis<-merge(nodata_tp2, team_lien2, by.x=c("Team"),by.y=c("Team"))
        
        n4_bis <- nrow(team_2_bis)
        Z4_bis <- scale(team_2_bis[,-1], center=TRUE,scale=TRUE)*sqrt(n4_bis/(n4_bis-1))
        
        d4_bis <- dist(Z4_bis)
        tree_tp2_bis <- hclust(d4_bis^2/(2*n4_bis), method="ward.D") #les individus sont pondérés par 1/n
        
        K_l5 <- 4
        part_l5 <- cutree(tree_tp2_bis,k=K_l5)
        part_l5 <- as.factor(part_l5)
        levels(part_l5) <- paste("cluster",1:K_l5,sep="")
        desc_l5 <- catdes(data.frame(part_l5,team_2_bis[,-1]),
                          num.var=1)
        plot(desc_l5,barplot=TRUE)
        
    }) 
    
    output$distPlot2 <- renderPlot({
        
        autres_lim<-60
        tab<-table(season_statsMVP$country)
        
        if(input$season_c != "toutes les années"){
            ind<-which(season_statsMVP$Year==input$season_c)
            tab<-table(season_statsMVP$country[ind])
            autres_lim<-10
        }
        tab<-sort(tab,decreasing = TRUE)
        tab<-as.data.frame(tab)
        ind<-which(tab$Freq<autres_lim)
        tab$Var1<-as.character(tab$Var1)
        tab$Var1[ind]<-"Autres"
        tab<-aggregate(. ~ Var1,tab , sum)
        tab$Freq<-round((tab$Freq/sum(tab$Freq))*100,2)
        
        pie(tab$Freq,col=c("#AAFFAA","#FFEE44","#FFAAAA","blue","red"),labels=tab$Var1,main="Diagramme de la représentation des pays ",cex=1.1)
        
    })
    
    output$table2 <- DT::renderDataTable({
        
        if(input$season_c!="toutes les années"){
            ind<-which(season_statsMVP$Year==input$season_c)
            tb<-as.data.frame(table(season_statsMVP$country[ind]))
        }
        else{
            tb<-as.data.frame(table(season_statsMVP$country))
        }
        
        nb_country<-count(unique(tb))
        nb_tot_play<-sum(tb$Freq)
        ind<-which(tb$Var1=="USA")
        nb_tot_strange_player<-nb_tot_play-tb$Freq[ind]
        
        contab <- data.frame(
            Tot = c(nb_tot_play[1],nb_tot_strange_player[1],nb_country[1])
        )
        colnames(contab) <- c("Number of Player", "Number of Player Strange", "Number of Country")
        DT::datatable(contab)
        
    })
    
    output$table_lien <- DT::renderDataTable({
        
        s7_l3_tab<-season_stats_PCA[,-c(6,7,10)]
        
        if(input$seas_play!="toutes les années"){
            ind<-which(s7_l3_tab$Year==as.integer(input$seas_play))
            s7_l3_tab<-s7_l3_tab[ind,]
        }
        
        n3 <- nrow(s7_l3_tab)
        Z3 <- scale(s7_l3_tab[,-c(1:3,5)], center=TRUE,scale=TRUE)*sqrt(n3/(n3-1))
        
        d3 <- dist(Z3)
        tree3 <- hclust(d3^2/(2*n3), method="ward.D") #les individus sont pondérés par 1/n
        
        K3 <- 4
        part3 <- cutree(tree3,k=K3)
        part3 <- as.factor(part3)
        levels(part3) <- paste("cluster",1:K3,sep="")
        s7_l3_tab<-cbind(part3,s7_l3_tab)
        DT::datatable(s7_l3_tab,options = list(
            lengthMenu = c(3, 5, 10)))
        
    })
    
    output$table_tot <- DT::renderDataTable({
        DT::datatable(tab_tot)
    })
    
    #### Stat player ####
    
    output$table_stat <- DT::renderDataTable({
        if(input$year_bf!="toutes les années"){
            ind<-which(stats_players$Annee==input$year_bf & stats_players$Minutes>1500)
            DT::datatable(stats_players[ind,])
        }
        else{
            ind<-which(stats_players$Minutes>1500)
            DT::datatable(stats_players[ind,])
        }
    })
    
    #### Predictions ####
    
    output$table_MVP <- DT::renderDataTable({
        #DT::datatable(MVPtot,options=list(columnDefs=list(list(targets=4,visible=FALSE))))%>%formatStyle("MVP predit","Same",target="row",backgroundColor=styleEqual(c(0,1),c("red","#36ff33")))
        DT::datatable(MVPtot,options=list(columnDefs=list(list(targets=c(7,8,9,10),visible=FALSE))))%>%formatStyle(c("MVP RF","MVP knn","MVP LDA","MVP NB"),c("Same","Same2","Same3","Same4"),backgroundColor=styleEqual(c(0,1),c("red","#36ff33")))
    })
    
    output$table_champ <- DT::renderDataTable({
        DT::datatable(Champtot,options=list(columnDefs=list(list(targets=4,visible=FALSE))))%>%formatStyle("Champion predit","Same",backgroundColor=styleEqual(c(0,1),c("red","#36ff33")))
    })
    
    ### Palmares ###
    
    output$table_tot <- DT::renderDataTable({
        DT::datatable(tab_tot)
    })
    
    ### Table _ s ###
    
    output$table_s <- DT::renderDataTable({
        
        season_stats_PCA2<-season_stats_PCA
        
        if(input$st_year!="toutes les saisons"){
            indic_year<-which(season_stats_PCA2$Year==input$st_year)
            season_stats_PCA2<-season_stats_PCA2[indic_year,]
        }
        
        
        vect<-unique(season_stats_PCA2$Pos)
        pos<-c()
        
        for(i in 1:length(vect)){
            indice<-which(season_stats_PCA2$Pos==vect[i])
            X<-cor(season_stats_PCA2[indice,-c(1:8,13:20)],season_stats_PCA2[indice,as.character(input$st)],use = "complete.obs")
            pos<-cbind(pos,X)
            colnames(pos)[i]<-vect[i]
        }
        pos<-as.data.frame(pos)
        DT::datatable(t(round(pos,2)))
        
    })
    
    ### Table_str_team ###
    
    output$table_str_team <- DT::renderDataTable({
        
        st_team_2<-strong_team
        
        if(input$st_year_team!="toutes les saisons"){
            indic_year<-which(st_team_2$Year==input$st_year_team)
            st_team_2<-st_team_2[indic_year,]
        }
        
        X<-cor(st_team_2[,-c(1:2,15:17)],st_team_2[,as.character(input$str_team)],use = "complete.obs")
        pos_str<-as.data.frame(X)
        colnames(pos_str)<-as.character(input$str_team)
        
        DT::datatable(t(round(pos_str,2)))
        
    })
    
    ### Variables players ###
    
    output$table_var_players <- DT::renderDataTable({
        DT::datatable(table_play)
    })
    
    ### Variables teams ###
    
    output$table_var_team <- DT::renderDataTable({
        DT::datatable(table_team)
    })
    
    ### Variables métriques ###
    
    output$table_var_met <- DT::renderDataTable({
        DT::datatable(table_met)
    })
    
    #### Big Five ####
    
    output$table_big <- DT::renderDataTable({
        if(input$year_bf2!="toutes les années"){
            Points3<-input$big_five2
            ind_PG<-which(stats_players$Annee==input$year_bf2 & stats_players$Minutes>1500 & stats_players$Poste=="PG")
            Points2<-stats_players[ind_PG,Points3]
            PG<-stats_players[ind_PG,]%>%
                filter(rank(desc(Points2), ties.method = "min") <= 1)
            ind_SG<-which(stats_players$Annee==input$year_bf2 & stats_players$Minutes>1500 & stats_players$Poste=="SG")
            Points2<-stats_players[ind_SG,Points3]
            SG<-stats_players[ind_SG,]%>%
                filter(rank(desc(Points2), ties.method = "min") <= 1)
            ind_SF<-which(stats_players$Annee==input$year_bf2 & stats_players$Minutes>1500 & stats_players$Poste=="SF")
            Points2<-stats_players[ind_SF,Points3]
            SF<-stats_players[ind_SF,]%>%
                filter(rank(desc(Points2), ties.method = "min") <= 1)
            ind_PF<-which(stats_players$Annee==input$year_bf2 & stats_players$Minutes>1500 & stats_players$Poste=="PF")
            Points2<-stats_players[ind_PF,Points3]
            PF<-stats_players[ind_PF,]%>%
                filter(rank(desc(Points2), ties.method = "min") <= 1)
            ind_C<-which(stats_players$Annee==input$year_bf2 & stats_players$Minutes>1500 & stats_players$Poste=="C")
            Points2<-stats_players[ind_C,Points3]
            C<-stats_players[ind_C,]%>%
                filter(rank(desc(Points2), ties.method = "min") <= 1)
            big<-rbind(rbind(rbind(rbind(PG,SG),SF),PF),C)
            index<-grep(Points3, colnames(big))
            index2<-c(1,2,3,4,index)
            big2<-big[,index2]
            DT::datatable(big2)
        }
        else{
            Points3<-input$big_five2
            ind_PG<-which(stats_players$Minutes>1500 & stats_players$Poste=="PG")
            Points2<-stats_players[ind_PG,Points3]
            PG<-stats_players[ind_PG,]%>%
                filter(rank(desc(Points2), ties.method = "min") <= 1)
            ind_SG<-which(stats_players$Minutes>1500 & stats_players$Poste=="SG")
            Points2<-stats_players[ind_SG,Points3]
            SG<-stats_players[ind_SG,]%>%
                filter(rank(desc(Points2), ties.method = "min") <= 1)
            ind_SF<-which(stats_players$Minutes>1500 & stats_players$Poste=="SF")
            Points2<-stats_players[ind_SF,Points3]
            SF<-stats_players[ind_SF,]%>%
                filter(rank(desc(Points2), ties.method = "min") <= 1)
            ind_PF<-which(stats_players$Minutes>1500 & stats_players$Poste=="PF")
            Points2<-stats_players[ind_PF,Points3]
            PF<-stats_players[ind_PF,]%>%
                filter(rank(desc(Points2), ties.method = "min") <= 1)
            ind_C<-which(stats_players$Minutes>1500 & stats_players$Poste=="C")
            Points2<-stats_players[ind_C,Points3]
            C<-stats_players[ind_C,]%>%
                filter(rank(desc(Points2), ties.method = "min") <= 1)
            big<-rbind(rbind(rbind(rbind(PG,SG),SF),PF),C)
            index<-grep(Points3, colnames(big))
            index2<-c(1,2,3,4,index)
            big2<-big[,index2]
            DT::datatable(big2)
        }
    })
})
