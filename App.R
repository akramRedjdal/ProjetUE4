library(data.table)
library(spdep)
library(sp)
library(stringr)
library(rgdal)
library(DCluster)
library(epitools)
library(sf)
library(leaflet)
library(dplyr)
library(stats)
library(cartography)
library(tidyverse)
library(geojsonio)
library(reshape2)
library(shape)
library(shiny)
library(ggplot2)
library(ggplot2)
library(plotly)
library(DT)
library(shinydashboard)
library(knitr)
library(kableExtra)
#IMPORTATION DE DONNEES 
polluantParEtablissement <- read_delim("data/etablissement.csv" , "," , escape_double = FALSE , trim_ws = TRUE)
localisationParEtablissement <- read_delim("data/registre-francais-des-emission-polluantes-etablissements.csv" , ";" , escape_double = FALSE , trim_ws = TRUE)
radon <- read_delim("data/radon.csv" , ";" , escape_double = FALSE , trim_ws = TRUE)
#Creation de la base de données
EtablissementPolluants <- polluantParEtablissement %>% dplyr::select( nom ) %>% 
  setnames(old=c("nom"), new=c("Nom Etablissement")) %>% 
  merge(localisationParEtablissement,by="Nom Etablissement")
Etablissement <-  setDT(EtablissementPolluants)[, paste0("type", 1:2) := tstrsplit(coordonnees, ",", type.convert = TRUE, fixed = TRUE)] %>% 
  setnames(old=c("type1","type2"), new=c("lng","lat"))
Etablissements <- Etablissement %>% filter(!is.na(Etablissement$lat))
#Normalisation des colonnes departement et communes pour merger a partir des 2 et eviter les NA
Etablissement$Departement <- gsub("[[:punct:]]", "", as.character(Etablissement$Departement))
radon$nom_dept <-  gsub("[[:punct:]]", "", as.character(radon$nom_dept))
Etablissement$Departement <- tolower(Etablissement$Departement)
radon$nom_dept <- tolower(radon$nom_dept)
Etablissement <- Etablissement %>% filter(!is.na(Departement))%>% group_by(Departement)
radon <- radon %>% filter(!is.na(nom_dept)) %>% group_by(nom_dept)
Etablissement$Commune <- gsub("[[:punct:]]", "", as.character(Etablissement$Commune))
radon$nom_comm <-  gsub("[[:punct:]]", "", as.character(radon$nom_comm))
Etablissement$Commune <- tolower(Etablissement$Commune)
radon$nom_comm <- tolower(radon$nom_comm)
Etablissement <- Etablissement %>% filter(!is.na(Commune))%>% group_by(Commune)
radon <- radon %>% filter(!is.na(nom_comm)) %>% group_by(nom_comm)
EtablissementRadon <- merge(radon,Etablissement,by.x=c("nom_dept","nom_comm"),by.y=c("Departement","Commune"),all.x=F,all.y=T)
EtablissementRadon <- EtablissementRadon %>% dplyr::filter(!is.na(EtablissementRadon$lng))
#creation data frame spatial
coordinates(EtablissementRadon) <- ~ lng + lat
EtablissementRadon@coords <- EtablissementRadon@coords[,c(2,1)]
#Carte Polygone Departement France obtenu a partir d'un github qui trvaille sur les données INSSEE et normallisation
departements <- geojsonio::geojson_read("data/departements.geojson", what = "sp")
departements$nom <- gsub("[[:punct:]]", "", as.character(departements$nom ))
departements$nom <- tolower(departements$nom )

#donnée cours 
effectif_france <- read_delim("data/effectif.france.csv", ";", escape_double = FALSE, trim_ws = TRUE)
evenements <- read_delim("data/evenements.csv", ";", escape_double = FALSE, trim_ws = TRUE)
effectif_departement <- read_delim("data/effectif.departement.csv", ";", escape_double = FALSE, trim_ws = TRUE)
#standardisation!
ratio.vector <- vector()
for (i in colnames(evenements[,-1])) {
  ratio.vector <- append(ratio.vector, round(ageadjust.direct(count = evenements[,i], 
                                                              pop = effectif_departement[,i], 
                                                              stdpop = effectif_france[,2])["adj.rate"] *10^5, 2))
}
ratioEvenement <- tibble(dep = colnames(evenements[,-1]),
                          
                          ratio = ratio.vector )
#normalisation Ratio
ratioEvenement$dep <- gsub("[[:punct:]]", "", as.character(ratioEvenement$dep))
ratioEvenement$dep <- tolower(ratioEvenement$dep )
#Merge fichier Spatial data Polygon avec ratio ET radon 
radon <- radon %>% group_by(nom_dept)
ratioRadon <- left_join(ratioEvenement,radon,by=c("dep"="nom_dept"),all.x=T,all.y=F)
departements@data = data.frame(departements@data, ratioEvenement[match(departements@data$nom, ratioEvenement$dep),])
departements@data = data.frame(departements@data, radon[match(departements@data$nom, radon$nom_dept),])
##carte potentiel Radon par Departement
bins <- c(1, 2,3)
pal <-colorFactor(c("gray","yellow","red"), domain = c(1,2,3) , na.color ="white" , alpha = TRUE )

carte  <- leaflet(departements) %>%  addProviderTiles(providers$Esri.WorldStreetMap) %>% 
  setView(lat = 47.256, lng = 2.35, zoom = 6) %>% 
  addPolygons( fillColor = ~pal(classe_potentiel),
                       weight = 2,
                       opacity = 1,
                       color = "white",
                       dashArray = "3",
                       fillOpacity = 0.7)           %>% addLegend(pal = pal, 
                                                                  values = ~classe_potentiel, 
                                                                  opacity = 0.7, 
                                                                  title = "Potentiel Radon",
                                                                  position = "topright") 
#carte ratio de l'evenement de la maladie x par Region
bins2 <- c(0,1, 2,3,4,5,6,7,8,Inf)
pal2 <- colorBin("Blues", domain = departements$ratio, bins = bins2)

pal3 <- colorBin("Blues", domain = EtablissementRadon$ratio, bins = bins2)

carte2  <- leaflet(departements) %>%  addProviderTiles(providers$Esri.WorldStreetMap) %>% 
  setView(lat = 47.256, lng = 2.35, zoom = 6) %>% 
  addPolygons( fillColor = ~pal2(ratio),
               weight = 2,
               opacity = 1,
               color = "white",
               dashArray = "3",
               fillOpacity = 0.7)           %>% addLegend(pal = pal2, 
                                                          values = ~ratio, 
                                                          opacity = 0.7, 
                                                          title = "Ratio incidence maladie X",
                                                          position = "bottomright") 
#carte france par departement avec Marquers vers tout les etablissements : 
col <- colorFactor(c("gray","yellow","red"), domain = c(1,2,3) , na.color ="white" , alpha = TRUE )
carte3 <- leaflet(departements) %>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>%setView(lat = 47.256, lng = 2.35, zoom = 6) %>% 
  addMarkers(data = EtablissementRadon@coords, 
             clusterOptions = markerClusterOptions() ,
             label = EtablissementRadon@data$`Nom Etablissement`,
             popup = str_c("<br/>Commune :<br/>",EtablissementRadon@data$nom_comm ,"<br/> num siret :<br/> " , 
                           EtablissementRadon@data$`Numéro Siret`, "<br/> APE : <br/>" , 
                           EtablissementRadon@data$`Libellé APE`  ) ) 
#Fonction Histogramme Selon libellé APE .
histDepartement <- function(Etablissements, choice,ligne) {
  plot <- Etablissements %>% filter(Departement == choice) %>%  group_by(`Libellé APE`) %>% filter(!is.na(`Libellé APE`))
  plot2 <- plot[1:ligne,]
  p <-  ggplot(data = plot2 , aes (x=plot2$`Libellé APE`,y=length(`Nom Etablissement`)/ligne)) + 
    geom_bar(aes(fill=Commune), stat="identity" , position = position_stack(reverse = TRUE)) + coord_flip() +
    theme(legend.position = "bottom") + 
    xlab("Libellé APE") + ylab("Nombre Etablissements ") + ggtitle(choice) 
  ggplotly(p ) 
}
histDepartementNom <- function(Etablissements, choice,ligne) {
  plot <- Etablissements %>% filter(Departement == choice) %>%  group_by(`Nom Etablissement`) %>% filter(!is.na(`Nom Etablissement`))
  plot2 <- plot[1:ligne,]
  p <-  ggplot(data = plot2 , aes (x=plot2$`Nom Etablissement`,y=length(`Nom Etablissement`)/ligne)) + 
    geom_bar(aes(fill=Commune), stat="identity" , position = position_stack(reverse = TRUE)) + coord_flip() +
    theme(legend.position = "bottom") + 
    xlab("Nom Etablissement") + ylab("Nombre Etablissements ") + ggtitle(choice) 
  ggplotly(p ) 
}
histDepartementLibelleEprtr <- function(Etablissements, choice,ligne) {
  plot <- Etablissements %>% filter(Departement == choice) %>%  group_by(`Libelle Eprtr`) %>% filter(!is.na(`Libellé Eprtr`))
  plot2 <- plot[1:ligne,]
  p <-  ggplot(data = plot2 , aes (x=plot2$`Libelle Eprtr`,y=length(`Nom Etablissement`)/ligne)) + 
    geom_bar(aes(fill=Commune), stat="identity" )  +
    theme(legend.position = "bottom") + 
    xlab("Libelle Eprtr") + ylab("Nombre Etablissements ") + ggtitle(choice) 
  ggplotly(p ) 
}
histDepartementCodeEprtr <- function(Etablissements, choice,ligne) {
  plot <- Etablissements %>% filter(Departement == choice) %>%  group_by(`Code Eprtr`) %>% filter(!is.na(`Code Eprtr`))
  plot2 <- plot[1:ligne,]
  p <-  ggplot(data = plot2 , aes (x=plot2$`Code Eprtr`,y=length(`Nom Etablissement`)/ligne)) + 
    geom_bar(aes(fill=Commune), stat="identity" , position = position_stack(reverse = TRUE)) + coord_flip() +
    theme(legend.position = "bottom") + 
    xlab("Code Eprtr ") + ylab("Nombre Etablissements ") + ggtitle(choice) 
  ggplotly(p ) 
}
EtablissementRadon@data= data.frame(EtablissementRadon@data, ratioRadon[match(EtablissementRadon@data$classe_potentiel, ratioRadon$classe_potentiel),])
APP <-  shinyApp(
  ui <- fluidPage(
    
    sidebarPanel(
      selectInput("axis","Choix X :" , choices = c("Libelle APE","Nom Etablissement","Code Eprtr"),selected = "Libelle APE"),
      selectInput("Departement",  "Departement :",
                  choices = Etablissements$Departement , selected = ""),
      checkboxInput("table","montrer le tableau de données",value = FALSE),
      sliderInput(inputId ="size", label="Nombre De Lignes", value = 40,
                  min = 5, max = 150, step = 5, animate =T , width = '400px' ),
      sliderInput(inputId ="opacity", label="Opacitée Incidence de la maladie X", value = 1,
                  min = 0, max = 2, step = 0.05, animate =T , width = '400px' ),
      sliderInput(inputId ="opacityRadon", label="Opacitée Potentiel Radon Par Region", value = 0,
                  min = 0, max = 2, step = 0.05, animate =T , width = '400px' ),
      radioButtons(inputId="points", label="Ploter des points",
                   choices = c("Incidence Maladie Par Commune",
                               "Potentiel Radon par Commune",
                               "Pas de Points"), 
                   selected = "Pas de Points",
                   width = "400px" )
      
      
     ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Carte Interactive",box( leafletOutput("map", width = "100%", height = "700px"),title = "Carte Interavtive pour les EtablissementsPolluants,PotentielRadon,IncidenceMaladieX",width = "100%", height = "100%")),
        tabPanel("Analyse de Données",plotlyOutput("Plot"), 
                 DT::dataTableOutput("tableau"))
        ))),
  
  
  server <- function(input, output) {
    output$tableau <- DT::renderDataTable({
    if (input$table){
      DT::datatable(data = Etablissements %>% filter(Departement == input$Departement)  %>% dplyr::select(`Nom Etablissement`,
                                                                                                  `Libellé APE`,
                                                                                                  `Code APE` ,
                                                                                                  `Libelle Eprtr`,Departement,Commune),
                    options = list(pageLength=10),rownames = F)
    }})
    
    output$Plot  <- renderPlotly({  if(input$axis=="Libelle APE"){
      histDepartement(Etablissements,input$Departement,input$size)}
      else if (input$axis=="Nom Etablissement"){ histDepartementNom(Etablissements,input$Departement,input$size)}
      else if (input$axis=="Code Eprtr"){histDepartementCodeEprtr(Etablissements,input$Departement,input$size)
      }})
   
   
    output$map <- renderLeaflet({
if(input$points=="Incidence Maladie Par Commune"){
  
carte3 %>% 
    addPolygons( fillColor = ~pal2(ratio),
                 weight = 2,
                 opacity = 1,
                 color = "white",
                 dashArray = "3",
                 fillOpacity = input$opacity)%>% addLegend(pal = pal2, 
                                                           values = ~ratio, 
                                                           opacity = 0.7, 
                                                           title = "Ratio incidence maladie X",
                                                           position = "bottomright") %>% 
    addPolygons( fillColor = ~pal(classe_potentiel),
                 weight = 2,
                 opacity = 1,
                 color = "White",
                 dashArray = "3",
                 fillOpacity = input$opacityRadon) %>% 
    addLegend(pal = pal, 
              values = ~classe_potentiel, 
              opacity = 0.7, 
              title = "Potentiel Radon",
              position = "topright")   %>%  
addCircles(data = EtablissementRadon@coords, 
                 color= pal3(EtablissementRadon$ratio) ,
                 label = EtablissementRadon$nom_dept,
                  popup = str_c("<br/>Ratio :<br/>",EtablissementRadon@data$ratio,
                                                  "<br/>Commune :<br/>",EtablissementRadon@data$nom_comm ) )
}
else if(input$points=="Potentiel Radon par Commune"){
  carte3 %>%     addPolygons( fillColor = ~pal2(ratio),
                 weight = 2,
                 opacity = 1,
                 color = "white",
                 dashArray = "3",
                 fillOpacity = input$opacity)%>% addLegend(pal = pal2, 
                                                           values = ~ratio, 
                                                           opacity = 0.7, 
                                                           title = "Ratio incidence maladie X",
                                                           position = "bottomright") %>% 
    addPolygons( fillColor = ~pal(classe_potentiel),
                 weight = 2,
                 opacity = 1,
                 color = "White",
                 dashArray = "3",
                 fillOpacity = input$opacityRadon)   %>% 
    addLegend(pal = pal, 
              values = ~classe_potentiel, 
              opacity = 0.7, 
              title = "Potentiel Radon",
              position = "topright")   %>%  
    addCircles(data = EtablissementRadon@coords, 
                     color= pal(EtablissementRadon@data$classe_potentiel) ,
                     label = EtablissementRadon$nom_dept,
                     popup = str_c("<br/>Ratio :<br/>",EtablissementRadon@data$classe_potentiel,
                                   "<br/>Commune :<br/>",EtablissementRadon@data$nom_comm ) )
}
else if(input$points=="Pas de Points"){carte3 %>% 
    addPolygons( fillColor = ~pal2(ratio),
                 weight = 2,
                 opacity = 1,
                 color = "white",
                 dashArray = "3",
                 fillOpacity = input$opacity)%>% addLegend(pal = pal2, 
                                                           values = ~ratio, 
                                                           opacity = input$opacity, 
                                                           title = "Ratio incidence maladie X",
                                                           position = "bottomright") %>% 
    addPolygons( fillColor = ~pal(classe_potentiel),
                 weight = 2,
                 opacity = 1,
                 color = "White",
                 dashArray = "3",
                 fillOpacity = input$opacityRadon) %>% 
    addLegend(pal = pal, 
              values = ~classe_potentiel, 
              opacity = 0.6, 
              title = "Potentiel Radon",
              position = "topright")  
    
    
}
      

      
    })
  }
)
#Impressions de tableau
my_df_print <- function(df) {
  df <- as.data.frame(df)
  df <- format(df, width = NULL, justify = "centre", big.mark = " ")
  kable(df, format = "html") %>% 
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = F,
                  font_size = 11, position = "left")
}
#Voisinage pour autocorrelation
departements@data$ratio <- replace(departements@data$ratio, is.na(departements@data$ratio), 0) 
voisinQ=poly2nb(departements)
voisinR=poly2nb(departements,	queen=FALSE)		
coords = coordinates(departements)
#Matrice conguite
matrice_conguiteQ = nb2listw(voisinQ,zero.policy=TRUE)	
#Bootstrap
bootstrap=moran.mc(departements$ratio,listw=matrice_conguiteQ,	nsim=999)		
#autocorrelogram
cor=sp.correlogram(voisinQ,	departements$ratio,	order=8,	method="I",	style="W",zero.policy=TRUE )	
#stone test 
Coordonees <-  Etablissements %>% dplyr::select(Departement,lng,lat)
doublons <- which(duplicated(Coordonees$Departement)) 
Coordonees<-Coordonees[-doublons,]
Coordonees <- Coordonees %>% filter(!is.na(Coordonees$lat))
Coordonees$Departement <- gsub("[[:punct:]]", "", as.character(Coordonees$Departement))
Coordonees$Departement <- tolower(Coordonees$Departement)
stoneTest <- merge(ratioEvenement,Coordonees,by.x="dep" ,by.y="Departement")
colnames(stoneTest) <- c("dep","Observed","x","y")
stoneTest <-  merge(stoneTest,ratioRadon)
stoneTest <- stoneTest %>% filter(!is.na(stoneTest$ratio))
stoneTest <- stoneTest %>% filter(!is.na(stoneTest$classe_potentiel))
stoneTest<-cbind(stoneTest, Expected=stoneTest$classe_potentiel*stoneTest$ratio/stoneTest$classe_potentiel)
stoneTest <- stoneTest %>% dplyr::select(dep,Observed,Expected,x,y)

