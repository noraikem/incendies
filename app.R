#### Chargement des packages ----




##### Chargement de la BDD ------

Victimes <- read_excel("data/Victimes.xlsx")

## VictimesB : contient toutes les colonnes victimes, blessés, .. 

VictimesB = Victimes[,-1]
VictimesB = VictimesB[,-(2:4)]
VictimesB = VictimesB[,-(3:5)]


VictimesBbis = VictimesB[,-(0:2)]

## VictimesC : contient uniquement les colonnes victimes
VictimesC = VictimesB[,-(3:6)]
VictimesC = VictimesC[,-(4:9)]
VictimesC = VictimesC[,-(5:10)]
VictimesC = VictimesC[,-(6:14)]


VictimesCbis = VictimesC[,-2]

##### Donnnees carto ------


# regions <- read_sf(dsn = "/cloud/project/Alternance/app4/data/regions_2015_metropole_region.shp", 
#     layer = "regions_2015_metropole_region.shp") %>% 
#     mutate(region = str_conv(region, "utf-8"))
#   regions

 c1=leaflet()%>%setView(lat =47,lng=3,zoom=5.5)%>%addTiles()
 c1 


##### DF pour Information sur toute la France ------
Victimes_longue = VictimesCbis %>% gather(-Année, key = "var", value = "Valeur")
df1 = Victimes_longue %>% group_by(Année, var) %>% 
  summarise(
    Somme = sum(Valeur, na.rm = T),
    Max = max(Valeur),
    Mediane = round(median(Valeur, na.rm=T),2),
    Ecart_type = round(sd(Valeur, na.rm = T), 2)
  )


### DEFINE UI ----

ui = dashboardPage(
  dashboardHeader(
    title = h3("Victimes des incendies en France "),
    titleWidth = 370
  ),
  dashboardSidebar(
    sidebarMenu(
                menuItem("Description des données", tabName = "desc"),  
                menuItem("Outil de comparaison", tabName = "comp"),
                menuItem("Informations sur toute la France", tabName = "fr"),
                menuItem("Cartographie",tabName = "carto"),
                menuItem("Jeu de données",tabName = "donnees")
    ),
    width=300
  ),
  dashboardBody(
    tabItems(
      
      ##### Description des données ------
      
      tabItem("desc", 
              
              box(
                title = "Distribution des Victimes par département",
                plotOutput("evolution"),
                width = 10
              ),
              fluidRow(
                column(2,
                       selectInput("choixD","Département:", c("All", unique(as.character(Victimes$Département))))
                       
                ),
                
                column(2,
                       selectInput("choixFEU1","Quel type de feu ? ",
                                   list("Tous" = 1, 
                                        "Feux d'habitation" = 2,
                                        "Feux d'ERP avec local à sommeil"=3,
                                        "Feux d'ERP sans local à sommeil"=4
                                   )
                       )
                ),
                column(2,
                       selectInput("choixVIC1","Quelles victimes ? ",
                                   list("Toutes" = 11,
                                        "Décédés" = 12,
                                        "Blessés graves" = 13,
                                        "Blessés légers" = 14,
                                        "Impliqués" = 15,
                                        "Sauvés" = 16,
                                        "Mis en sécurité" = 17
                                        
                                   )
                       )
                )
                
              ),
              
              DT::dataTableOutput("evoltable")
      ),
      
      ##### Comparaison entre deux département -----    
      
      tabItem("comp", 
              valueBox(
                value = "Comparaison entre deux départements",
                subtitle = "",
                color = "black",
                width = 12
              ),
              fluidRow(
                column(5,
                       selectInput("choixD1","Département n°1:", c("All", unique(as.character(Victimes$Département))))
                       
                ),
                column(5,
                       selectInput("choixD2","Département n°2:",c("All",  unique(as.character(Victimes$Département))))
                )
              ),
              # box(
              #   title = "Distribution des Victimes",
              #   plotOutput("evolution1"),
              #   width = 5
              # ),box(
              #   title = "Distribution des Victimes",
              #   plotOutput("evolution2"),
              #   width = 5
              # ),
              box(
                title = "Distribution des Victimes",
                plotOutput("evolution3"),
                width = 10
              ),
              fluidRow(
                column(2,
                       selectInput("choixFEU","Quel type de feu ? ",
                                   list("Tous" = 1, 
                                        "Feux d'habitation" = 2,
                                        "Feux d'ERP avec local à sommeil"=3,
                                        "Feux d'ERP sans local à sommeil"=4
                                   )
                       )
                ),
                column(2,
                       selectInput("choixVIC","Quelles victimes ? ",
                                   list("Toutes" = 11,
                                        "Décédés" = 12,
                                        "Blessés graves" = 13,
                                        "Blessés légers" = 14,
                                        "Impliqués" = 15,
                                        "Sauvés" = 16,
                                        "Mis en sécurité" = 17
                                        
                                   )
                       )
                )
                
              ),
              fluidRow(infoBoxOutput("info1"),
                       infoBoxOutput("info2"))
              
              
      ),  
      
      ##### Informations sur toute la France -----                    
      tabItem("fr",
              tabBox(
                title = "Victimes ",
                width = 10,
                tabPanel(title = "Feux d'habitation",
                         dataTableOutput("vfh")
                ),
                tabPanel(title = "Feux ERP avec local à sommeil", 
                         dataTableOutput("vferpalas")
                ),
                tabPanel(title = "Feux ERP sans local à sommeil", 
                         dataTableOutput("vferpslas")
                )
              )
      ),
      
      ##### Cartographie ----    
      
      tabItem("carto",


                        fluidPage(
                          leafletOutput("carto1"),
                          fluidRow(
                            column(2,
                                   selectInput("choixAcarto","Année:", c("All", unique(as.character(Victimes$Année))))

                            ),
                            column(2,
                                   selectInput("choixRcarto","Région:", c("All", unique(as.character(Victimes$Région))))

                            ),
                            column(2,
                                   selectInput("choixDcarto","Département:", c("All", unique(as.character(Victimes$Département))))

                            ),

                            column(2,
                                   selectInput("choixFEUcarto","Quel type de feu ? ",
                                               list("Tous" = 1,
                                                    "Feux d'habitation" = 2,
                                                    "Feux d'ERP avec local à sommeil"=3,
                                                    "Feux d'ERP sans local à sommeil"=4
                                               )
                                   )
                            ),
                            column(2,
                                   selectInput("choixVICcarto","Quelles victimes ? ",
                                               list("Toutes" = 11,
                                                    "Décédés" = 12,
                                                    "Blessés graves" = 13,
                                                    "Blessés légers" = 14,
                                                    "Impliqués" = 15,
                                                    "Sauvés" = 16,
                                                    "Mis en sécurité" = 17

                                               )
                                   )
                            )

                          )
                  )


              ),
      
      ##### Jeu de données ---- 
      
      tabItem("donnees",
              
              fluidRow(
                column(2,
                       selectInput("annee","Année:", c("All", unique(as.character(Victimes$Année)))
                       )
                ),
                column(2,
                       selectInput("zone","Zone:",c("All",  unique(as.character(Victimes$Zone)))
                       )
                ),
                column(2,
                       selectInput("reg","Région:",c("All",unique(as.character(Victimes$Région)))
                       )
                ),
                
                column(2,
                       selectInput("dep", "Département:", c("All",unique(as.character(Victimes$Département)))
                       )
                ),
                column(2,
                       selectInput("categ","Catégorie:",c("All", unique(as.character(Victimes$Catégorie)))
                       )
                )
              ),
              # Create a new row for the table.
              
              DT::dataTableOutput("table")
      )
    ) 
  ),
  title = "Informations sur les Victimes" ,
  skin = "red"
)


### DEFINE SERVER LOGIC ----

server = function(input, output) {
  
  
  
  ##### exploitation 1 département -----  
  ###### filtre -----
  
  variable_a_afficher1 = reactive({
    
    if(input$choixFEU1 == 1){ 
      
      if(input$choixVIC1 == 11) var = "Victimes"
      if(input$choixVIC1 == 12) var = "Décédés"
      if(input$choixVIC1 == 13) var = "`Blessés graves`"
      if(input$choixVIC1 == 14) var = "`Blessés légers`"
      if(input$choixVIC1 == 15) var = "Impliqués"  
      if(input$choixVIC1 == 16) var = "Sauvés"
      if(input$choixVIC1 == 17) var = "`Mis en sécurité`"
      
      var
      
    }
    
    
    if(input$choixFEU1 == 2){ 
      
      if(input$choixVIC1 == 11) var = "`Victimes (feux d'habitation)`"
      if(input$choixVIC1 == 12) var = "`Décédés (feux d'habitation)`"
      if(input$choixVIC1 == 13) var = "`Blessés graves (feux d'habitation)`"
      if(input$choixVIC1 == 14) var = "`Blessés légers (feux d'habitation)`"
      if(input$choixVIC1 == 15) var = "`Impliqués (feux d'habitation)`"  
      if(input$choixVIC1 == 16) var = "`Sauvés (feux d'habitation)`"
      if(input$choixVIC1 == 17) var = "`Mis en sécurité (feux d'habitation)`"
      
      var
      
    }
    if(input$choixFEU1 == 3){ 
      
      if(input$choixVIC1 == 11) var = "`Victimes (feux ERP avec local à sommeil)`"
      if(input$choixVIC1 == 12) var = "`Décédés (feux ERP avec local à sommeil)`"
      if(input$choixVIC1 == 13) var = "`Blessés graves (feux ERP avec local à sommeil)`"
      if(input$choixVIC1 == 14) var = "`Blessés légers (feux ERP avec local à sommeil)`"
      if(input$choixVIC1 == 15) var = "`Impliqués (feux ERP avec local à sommeil)`"  
      if(input$choixVIC1 == 16) var = "`Sauvés (feux ERP avec local à sommeil)`"
      if(input$choixVIC1 == 17) var = "`Mis en sécurité (feux ERP avec local à sommeil)`"
      
      var
      
    }    
    if(input$choixFEU1 == 4){ 
      
      if(input$choixVIC1 == 11) var = "`Victimes (feux ERP sans local à sommeil)`"
      if(input$choixVIC1 == 12) var = "`Décédés (feux ERP sans local à sommeil)`"
      if(input$choixVIC1 == 13) var = "`Blessés graves (feux ERP sans local à sommeil)`"
      if(input$choixVIC1 == 14) var = "`Blessés légers (feux ERP sans local à sommeil)`"
      if(input$choixVIC1 == 15) var = "`Impliqués (feux ERP sans local à sommeil)`"  
      if(input$choixVIC1 == 16) var = "`Sauvés (feux ERP sans local à sommeil)`"
      if(input$choixVIC1 == 17) var = "`Mis en sécurité (feux ERP sans local à sommeil)`"
      
      var
      
    }
    
    var
  })  
  
  ###### courbe -----
  
  output$evolution = renderPlot({
    
    if (input$choixFEU1>0) { 
      ggplot( subset(Victimes, Département == input$choixD),aes_string(y=variable_a_afficher1())) +
        geom_line(aes(x=Année),  colour = "red") +
        geom_point(aes(x=Année))
    }
    
  })
  
  ###### donnees -----
  
  output$evoltable <- DT::renderDataTable({DT::datatable(
    {
      data <- Victimes
      
      if (input$choixD != "All") {
        data <- data[data$Département == input$choixD,]
      }
      data[,c("Année","Département",variable_a_afficher1())]
      
      
    },options = list(scrollX=T))
  })
  
  
  
  ##### comparaison  -----  
  
  ###### filtre ------  
  
  variable_a_afficher = reactive({
    
    if(input$choixFEU == 1){ 
      
      if(input$choixVIC == 11) var = "Victimes"
      if(input$choixVIC == 12) var = "Décédés"
      if(input$choixVIC == 13) var = "`Blessés graves`"
      if(input$choixVIC == 14) var = "`Blessés légers`"
      if(input$choixVIC == 15) var = "Impliqués"  
      if(input$choixVIC == 16) var = "Sauvés"
      if(input$choixVIC == 17) var = "`Mis en sécurité`"
      
      var
      
    }
    
    
    if(input$choixFEU == 2){ 
      
      if(input$choixVIC == 11) var = "`Victimes (feux d'habitation)`"
      if(input$choixVIC == 12) var = "`Décédés (feux d'habitation)`"
      if(input$choixVIC == 13) var = "`Blessés graves (feux d'habitation)`"
      if(input$choixVIC == 14) var = "`Blessés légers (feux d'habitation)`"
      if(input$choixVIC == 15) var = "`Impliqués (feux d'habitation)`"  
      if(input$choixVIC == 16) var = "`Sauvés (feux d'habitation)`"
      if(input$choixVIC == 17) var = "`Mis en sécurité (feux d'habitation)`"
      
      var
      
    }
    if(input$choixFEU == 3){ 
      
      if(input$choixVIC == 11) var = "`Victimes (feux ERP avec local à sommeil)`"
      if(input$choixVIC == 12) var = "`Décédés (feux ERP avec local à sommeil)`"
      if(input$choixVIC == 13) var = "`Blessés graves (feux ERP avec local à sommeil)`"
      if(input$choixVIC == 14) var = "`Blessés légers (feux ERP avec local à sommeil)`"
      if(input$choixVIC == 15) var = "`Impliqués (feux ERP avec local à sommeil)`"  
      if(input$choixVIC == 16) var = "`Sauvés (feux ERP avec local à sommeil)`"
      if(input$choixVIC == 17) var = "`Mis en sécurité (feux ERP avec local à sommeil)`"
      
      var
      
    }    
    if(input$choixFEU == 4){ 
      
      if(input$choixVIC == 11) var = "`Victimes (feux ERP sans local à sommeil)`"
      if(input$choixVIC == 12) var = "`Décédés (feux ERP sans local à sommeil)`"
      if(input$choixVIC == 13) var = "`Blessés graves (feux ERP sans local à sommeil)`"
      if(input$choixVIC == 14) var = "`Blessés légers (feux ERP sans local à sommeil)`"
      if(input$choixVIC == 15) var = "`Impliqués (feux ERP sans local à sommeil)`"  
      if(input$choixVIC == 16) var = "`Sauvés (feux ERP sans local à sommeil)`"
      if(input$choixVIC == 17) var = "`Mis en sécurité (feux ERP sans local à sommeil)`"
      
      var
      
    }
    
    var
  })             
  
  
  ###### courbe -----
  
  
  # output$evolution1 = renderPlot({
  # 
  # 
  #     
  #   if (input$choixFEU>0) { 
  #     ggplot( subset(Victimes, Département == input$choixD1),aes_string(y=variable_a_afficher())) +
  #       geom_line(aes(x=Année),  colour = "red") +
  #       geom_point(aes(x=Année,))
  #   }
  #                      
  #  })           
  # 
  # output$evolution2 = renderPlot({
  #   if (input$choixFEU>0) { 
  #     ggplot( subset(Victimes, Département == input$choixD2),aes_string(y=variable_a_afficher())) +
  #       geom_line(aes(x=Année),  colour = "blue") +
  #       geom_point(aes(x=Année,))
  #   }
  # }) 
  
  
  
  output$evolution3 = renderPlot({
    if (input$choixFEU>0) { 
      ggplot(subset(Victimes, Département == input$choixD2 | Département == input$choixD1),aes_string(y=variable_a_afficher())) +
        geom_line( aes(x=Année,color=Département)) + geom_point(aes(x=Année))
    }
  })
  
  
  ##### stat toute la france -----    
  
  output$vfh = renderDataTable({
    datatable(
      data.frame(
        df1 %>%
          filter(var == "Victimes (feux d'habitation)") %>%
          select(-var)
      ),
      colnames = c(
        'Annee'= 'Année' ,
        'Ecart type' = 'Ecart_type'
      ),
      options = list(pageLength = 50, dom = 'tip')
    )
    
  })
  
  output$vferpalas = renderDataTable({
    datatable(
      data.frame(
        df1 %>%
          filter(var == "Victimes (feux ERP avec local à sommeil)") %>%
          select(-var)
      ),
      colnames = c(
        'Annee'= 'Année' ,
        'Ecart type' = 'Ecart_type'
      ),
      options = list(pageLength = 50, dom = 'tip')
    )
    
  })
  
  output$vferpslas = renderDataTable({
    datatable(
      data.frame(
        df1 %>%
          filter(var == "Victimes (feux ERP sans local à sommeil)") %>%
          select(-var)
      ),
      colnames = c(
        'Annee'= 'Année' ,
        'Ecart type' = 'Ecart_type'
      ),
      options = list(pageLength = 50, dom = 'tip')
    )
    
  })
  
  
  
  
  
  
  
  
  
  ##### cartographie -----
  
  
   output$carto1=renderLeaflet(c1)
  
  ##### donnees -----
  
  output$table <- DT::renderDataTable({DT::datatable(
    # options = list( lengthMenu= c(30,50,100,200),pageLength = 30, scrollX=T)
    {
      data <- Victimes
      
      
      if (input$annee != "All") {
        data <- data[data$Année == input$annee,]
      }
      if (input$zone != "All") {
        data <- data[data$Zone == input$zone,]
      }
      if (input$reg != "All") {
        data <- data[data$Région == input$reg,]
      }
      
      if (input$dep != "All") {
        data <- data[data$Département == input$dep,]
      }
      if (input$categ != "All") {
        data <- data[data$Catégorie == input$categ,]
      }
      
      data
      
      
    },extensions = 'Buttons',
    options = list(scrollX=T,pageLength= 20,buttons = c('copy', 'csv', 'excel', 'pdf','print'),dom="lftiprB"))
  })    
  
  
  
}   




### RUN THE APP ----
shinyApp(ui = ui, server = server)  

