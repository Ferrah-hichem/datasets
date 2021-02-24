library(shiny)
library(Hmisc)
library(UsingR)
library(reshape2)
library(ggpubr)
library(shinydashboard)
ui <- dashboardPage(
  dashboardHeader(
    title="G.F.E"
  ),
  
  dashboardSidebar(
    
    
    #chargement du fichier
    fileInput(inputId = "file1", label = "Veuillez choisir votre fichier CSV",
              accept = c("text/plain", ".csv")
    ),
    
    
    sidebarMenu(
      
      #Donnees quantitatives discrÃ¨tes:
      
      menuItem("Accueil", tabName = "accueill", icon = icon("home")),
      menuItem("Le dataset", tabName = "readData", icon = icon("refresh")),
      menuItem("Univariee", tabName = "quant", icon=icon("list-alt")),
      menuItem("Bivariee", tabName = "qual", icon=icon("list-alt")),
      menuItem("Qualitative VS Quantitative", tabName = "vs", icon=icon("list-alt"))
      
    )
  ),
  dashboardBody(
    
    tags$head(tags$style(HTML('
      .content-wrapper {
        
        background-color: #fcfffff5;
      }
      .main-sidebar{
        background-color: #595f5e !important;
      } 

      .navbar  {
        background-color: #595f5e !important;
      }
      .col-sm-8{
      width :100% !important;
      }
                              .sidebar-toggle:hover{
                              background-color: #595f5e !important;
                              }
                              .logo{
                              
                              background-color: #595f5e !important;
                              }
                              '
    ))),
    
    tabItems(
      
      tabItem(tabName = "readData",
              h3("Les données du dataset IRIS",align = "center"),
              tableOutput(outputId = "contents")
              
      ),
      tabItem(tabName = "stat",
              h3("Statistiques  ",align = "center"),
              tableOutput(outputId = "statistique")
              
      ),
      tabItem(tabName = "qual",
              
              
              mainPanel(
                fluidRow(
                  column(6,
                         selectInput("choix", "Le choix de la variable",
                                     c("sepal_length" = "V2", "sepal_width" = "V3", "petal_length" = "V4", "petal_width" = "V5"),selected = 1),
                         
                  ),
                  column(6,
                         selectInput("choixx", "Le choix de la variable",
                                     c("sepal_length" = "V2", "sepal_width" = "V3", "petal_length" = "V4", "petal_width" = "V5"),selected = 1),
                         
                  )
                  
                  
                ),
                
                tabsetPanel(
                  tabPanel("Nuage de points", 
                           fluidRow(
                             h3("Nuage de point avec la regression linéaire", align="center"),
                             column(6, plotOutput("nuagePoints")),
                             
                             column(6, textOutput("correlation"))
                             
                           )
                  ),
                  tabPanel("Histogrammes dos à dos", 
                           fluidRow(
                             column(8, offset = 1, plotOutput("histbackback"))
                           ), style="padding-left: 150px; margin-top: 10px; padding-right: 350px;"
                           
                  ),
                  tabPanel("Nuage de points et Histogrammes", 
                           fluidRow(
                             column(8, offset = 1, plotOutput("nuagePointshist"))
                           ), style="padding-left: 150px; margin-top: 10px; padding-right: 350px;"
                           
                  ),
                  tabPanel("Caractéristiques", tableOutput("caract")),
                  
                  tabPanel("Boîtes parallèles", 
                           fluidRow(
                             column(6, plotOutput("boxplotBasic")),
                             column(6, plotOutput("boxplotGgplot"))
                           ),
                           fluidRow(
                             column(4, offset = 4, textOutput("cor"))
                           )
                           
                  )
                  
                  
                )
              )),
      
      tabItem(tabName = "quant",
              
              
              mainPanel(
                selectInput("radio", "Le choix de la variable",
                            c("sepal_length" = "V2", "sepal_width" = "V3", "petal_length" = "V4", "petal_width" = "V5"),selected = 1),
                
                
                tabsetPanel(
                  tabPanel("Le SUMMARY", 
                           fluidRow(
                             h3("Le summary de la variable choisie", align = "center"),
                             
                             # Affichage d'un summary
                             verbatimTextOutput(outputId = "summary")
                           )
                           
                           
                           
                           
                  ),
                  
                  tabPanel("Graphes + boxplot", 
                           fluidRow(
                             column(4, 
                                    # Zone d'affichage du diagramme en bÃÂÃÂ¢tons des effectifs
                                    plotOutput(outputId = "effectifsDiag")),
                             column(4, 
                                    # Zone d'affichage du diagramme en bÃÂÃÂ¢tons des effectifs cumulÃÂÃÂ©s
                                    plotOutput(outputId = "effectifsCumDiag")),
                             column(4, 
                                    # Zone d'affichage de la boÃÂÃÂ®te ÃÂÃÂ  moustaches
                                    plotOutput(outputId = "boiteMoustaches"))
                           )
                           
                           
                           
                  ),
                  
                  tabPanel("Histogrammes et courbes ", 
                           h3("Visualisation des graphes ", align = "center"),
                           fluidRow(
                             
                             
                             column(4,
                                    h5("Histogramme des effectifs", align = "center"),
                                    plotOutput(outputId = "effectifsHist")),
                             column(4,
                                    h5("Histogramme des densitÃ©s de frequences", align = "center"),
                                    plotOutput(outputId = "effectifsHistFreqDens")),
                             column(4,
                                    h5("Courbe cumulative", align ="center"),
                                    plotOutput(outputId = "effectifsCumCurve"))
                             
                           ),
                           
                           
                  ),
                  tabPanel("Statistique qualitatives ", 
                           
                           fluidRow(
                             
                             
                             column(6,
                                    h5("Histogramme des effectifs", align = "center"),
                                    tableOutput(outputId = "statqq")),
                             column(6,
                                    h5("Courbe cumulative", align ="center"),
                                    plotOutput(outputId = "effectifsDiagq"))
                             
                           ),
                           
                           
                  )
                  
                  
                  
                  
                )
              )),
      
      tabItem(tabName = "vs",
              h3("Quantitative VS Qualitative  ",align = "center"),
              mainPanel(
                tabsetPanel(
                  tabPanel("Diag. Barres (1 var.)", 
                           fluidRow(
                             column(6, plotOutput("barplotUni")),
                             column(6, plotOutput("barplotOrderedUni"))
                           )
                  ),
                  tabPanel("Diag. Barres (2 var.)", 
                           fluidRow(
                             column(6, plotOutput("barplotBi")),
                             column(6, plotOutput("barplotDodgeBi"))
                           )
                  ),
                  tabPanel("Diag. Profils", 
                           fluidRow(
                             column(6, plotOutput("barplotProfils")),
                             column(6, tableOutput("contingency"))
                           )
                  ),
                  tabPanel("Indices", 
                           fluidRow(
                             column(6, offset = 2, tableOutput("force"))
                           )
                  )
                  
                )
                , style = "font-size: 75%")),
      
      tabItem(tabName = "accueill",
              img(src="iris.jpg", style=" width:27%",align = "center"),
              h3(" A propos du tp ",align = "center"),
              br(),
              br(),
              br(),
              strong("Problèmatique", style="font-size :15pt"),
              br(),
              br(),
              p("Le travail présenté dans cette application, rentre dans le cadre d’un TP du module Data Science 2 pour les étudiants MLDS de l’université de Paris pour l’année universitaire 2020/2021.
Le travail consiste de faire une analyse uni-variée, sur l’ensemble des variables d’un data-set choisi, Cette analyse comporte la mise en place d’un tableau de fréquences, diagramme en barre, graphe de la fonction de répartition empirique, Histogramme et boite à moustache de chaque variable.
L’application interagit d’une manière réactive à la demande de l’utilisateur.
L’utilisateur à la possibilité d’explorer l’analyse uni-variée, d’une variable choisie parmi celles qui sont disponibles.
", style=" font-size :13pt"),
              br(),
              br(),
              strong("Données utilisées :", style="font-size :15pt"),
              p("Le Data-set utilisé dans ce travail est un data-set classique intitulé « Edgar Anderson's Iris Data » qui regroupe 150 lignes d’observations de données d'Iris (type de plantes) qui sont identifiées par 5 variables (4 quantitatives et 1 qualitative) :
", style=" font-size :13pt"),
              p("Sepal.Length : Longueur des sépales."),
              p("Sepal.Width : Largeur des sépales."),
              p("Petal.Length : Longueur des pétales. "),
              p("Petal.Width : Largeur des pétales."),
              p("Species : espèces. "),
              br(),
              br(),
              p(""),
              h3("Réalisé Par :  GHARBI Mohamed, FERRAH Hichem – ELKEFIF NASSIM ",align = "center")
      )
      
      
      
      
      
      
      
    )
  )
)

server <- function(input, output) {
  data <- reactive({
    # Initialement, class(input$file1) = NULL
    # AprÃÂÃÂ¨s chargement, class(input$file1) = data.frame
    # avec les colonnes 'size', 'type', and 'datapath' columns. 
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    
    read.csv(inFile$datapath, header = FALSE,nrows=150, sep=';')
    
    
  })
  
  tabStats <- reactive({
    dt = data()
    dt2 =dt[2:150,input$radio]
    # Calculer les effectifs et les effectifs cumulÃÂ©s
    table.tmp <- as.data.frame(table(dt2))
    table.tmp <- cbind(table.tmp, cumsum(table.tmp[[2]]))
    # Calculer les frÃÂ©quences et les frÃÂ©quences cumulÃÂ©s
    table.tmp <- cbind(table.tmp, 
                       table.tmp[[2]]/nrow(data())*100,
                       table.tmp[[3]]/nrow(data())*100)
    # Ajouter des noms de colonnes
    colnames(table.tmp) <- c(dt[1,input$radio], "Effectifs", "Effectifs Cum.",
                             "Frequences", "Frequences Cum.")
    # Renvoyer le tableau statistique
    return(table.tmp)
  })
  tabStat <- reactive({
    dt = data()
    dt2 =dt[2:151,"V6"]
    # Calculer les effectifs et les effectifs cumulÃÂ©s
    table.tmp <- as.data.frame(table(dt2))
    table.tmp <- cbind(table.tmp, cumsum(table.tmp[[2]]))
    # Calculer les frÃÂ©quences et les frÃÂ©quences cumulÃÂ©s
    table.tmp <- cbind(table.tmp, 
                       table.tmp[[2]]/nrow(data())*100,
                       table.tmp[[3]]/nrow(data())*100)
    # Ajouter des noms de colonnes
    colnames(table.tmp) <- c(dt[1,"V6"], "Effectifs", "Effectifs Cum.",
                             "Frequences", "Frequences Cum.")
    # Renvoyer le tableau statistique
    return(table.tmp)
  })
  
  
  # Commande pour le calcul du summary
  
  output$contents <- renderTable({ 
    data()
  })
  
  
  
  # Boîtes parallèles
  # ----
  output$boxplotBasic <- renderPlot({
    
    d <- data()[2:150,]
    d.stack <- melt(d, measure.vars = c("V2","V3","V4","V5"))
    # Boxplot basique
    d.stack$value <- as.numeric(d.stack$value)
    boxplot(d.stack$value ~ d.stack$variable , col="grey",
            xlab = "Modalités", ylab = "Mesures")
  })
  
  output$boxplotGgplot <- renderPlot({
    d <- data()[2:150,]
    d.stack <- melt(d, measure.vars = c("V2","V3","V4","V5"))
    d.stack$value <- as.numeric(d.stack$value)
    # Boxplot élaborée
    qplot(x = d.stack[,2], y = d.stack[,1], 
          xlab = "Modalités", ylab = "Mesures",
          geom=c("boxplot", "jitter"), fill=d.stack[,2]) +
      theme(legend.title=element_blank())
  })
  
  
  
  
  output$histbackback <- renderPlot({
    options(digits=1)
    x.var = input$choix ; y.var = input$choixx;
    dt = data()
    dt2 =dt[2:149,input$choix]
    dt2 = as.numeric(dt2)
    dt = data()
    dt3 =dt[2:149,input$choixx]
    dt3 = as.numeric(dt3)
    histbackback(x = dt2, y = dt3,
                 xlab = c(x.var, y.var), main = paste(x.var, "and", y.var), 
                 las = 2)
  })
  output$nuagePoints <- renderPlot({
    # Simple nuage de point 
    options(scipen=999)
    x.var = input$choix ; y.var = input$choixx;
    plot(x = data()[2:150, x.var], y = data()[2:150, y.var], col = "blue",
         las = 2, cex.axis = 0.7,
         main = paste(y.var, "en fonction de", x.var),
         xlab = x.var, ylab = y.var, cex.lab = 1.2
    )
    # Droite de régression linéaire (y~x) 
    abline(lm(data()[2:150, y.var]~data()[2:150, x.var]), col="red", lwd = 2)
    options(scipen=0)
    
    
  })
  output$correlation <- renderText({
    dt = data()
    dt2 =dt[2:149,input$choix]
    dt2 = as.numeric(dt2)
    dt = data()
    dt3 =dt[2:149,input$choixx]
    dt3 = as.numeric(dt3)
    #x.var = input$choix ; y.var = input$choixx;
    coeff.tmp <- cov(dt2, dt3)/(sqrt(var(dt2)*var(dt3)))
    paste("Coefficient de corrélation linéaire =", round(coeff.tmp,digits = 2))
  })
  output$summary <- renderPrint({
    dt = data()
    dt2 =dt[2:149,input$radio]
    #print(dt2)
    t(summary.default(as.numeric(as.character(dt2)))) })
  
  output$statistique <- renderTable({ 
    tabStats() })
  output$statqq <- renderTable({ 
    tabStat() })
  # Nuage de points et histogrammes
  # ----
  output$nuagePointshist <- renderPlot({
    options(digits=1)
    dt = data()
    dt2 =dt[2:149,input$choix]
    dt2 = as.numeric(dt2)
    dt = data()
    dt3 =dt[2:149,input$choixx]
    dt3 = as.numeric(dt3)
    EF = dt2; 
    CA = dt3;
    scatter.with.hist( EF, CA)
  })
  
  output$caract <- renderTable({
    # Définition des colonnes choisies 
    var.names <- c("V6", "V5", "V4","V3","V2")
    # Initialisation de la table
    caract.df <- data.frame()
    
    # Pour chaque colonne, calcul de min, max, mean et ecart-type
    for(strCol in var.names){
      caract.vect <- c(min(data()[2:150, strCol]), max(data()[2:150,strCol]), 
                       mean(var(data()[2:150,strCol])), sqrt(var(data()[2:150,strCol])))
      caract.df <- rbind.data.frame(caract.df, caract.vect)
    }
    
    # Définition des row/colnames
    rownames(caract.df) <- var.names
    colnames(caract.df) <- c("Minimum", "Maximum", "Moyenne", "Ecart-type")
    # Renvoyer la table
    caract.df
  }, rownames = TRUE, digits = 0)
  # Commande pour l'affichage du plot des effectifs
  output$effectifsDiag <- renderPlot({ 
    dt = data()
    plot(table(data.frame(dt[2:151,input$radio])), col ="blue", xlab =dt[1,input$radio], ylab ="Effectifs", 
         main ="Distribution des effectifs")
  })
  output$effectifsDiagq <- renderPlot({ 
    dt = data()
    plot(table(data.frame(dt[2:150,"V6"])), col ="blue", xlab =dt[1,"V6"], ylab ="Effectifs", 
         main ="Distribution des effectifs")
  })
  # Commande pour l'affichage du plot des frÃÂ©quences cumulÃÂ©es
  output$effectifsCumDiag <- renderPlot({ 
    plot(ecdf(as.numeric(as.character(tabStats()[,1]))), 
         col ="blue", xlab = "La variable" , ylab ="Frequences cumulees", 
         main ="Frequences cumulees ")
  })
  
  # Commande pour l'affichage de la boÃÂ®te ÃÂ  moustaches
  output$boiteMoustaches <- renderPlot({
    # BoÃÂ®te ÃÂ  moustaches
    dt = data()
    boxplot( data.frame(as.numeric(as.character(dt[2:150,input$radio]))), col = grey(0.8), 
             main = "Taille des petales",
             ylab = "taille", las = 1)
    # Affichage complÃÂ©mentaires en Y des diffÃÂ©rents ÃÂ¢ges
    rug(data()[,1], side = 2)
  })
  # RÃ©cupÃ©ration des valeurs fecondite
  fecondite <- reactive({
    if(!"Sepal.Length" %in% colnames(data())) return(NULL)
    data()$Sepal.Length
    
  })
  # Histogrammes
  # ----
  output$effectifsHist <- renderPlot({
    dt = data()
    
    # Histogramme des effectifs
    hist(as.numeric(as.character(dt[2:149,input$radio])) , freq = TRUE, cex.axis = 1.5, cex.main = 1.5,
         main = "Histogramme", col = "blue",
         xlab = dt[1,input$radio], ylab = "Effectifs", las = 1,
         right = FALSE, cex.lab = 1.5)
  })
  
  output$effectifsCumCurve <- renderPlot({
    dt = data()
    # RÃ©cupÃ©ration des infos Ã  partir de l'histogramme
    tmp.hist <- hist(as.numeric(as.character(dt[2:149,input$radio])) , plot = FALSE,
                     
                     right = FALSE)
    # Courbe cumulative (effectifs)
    plot(x = tmp.hist$breaks[-1], y = cumsum(tmp.hist$counts),
         xlab = dt[1,input$radio],
         ylab = "Effectifs cumulés", cex.axis = 1.5, cex.lab = 1.5,
         main = "Courbe cumulative ",
         type = "o", col = "blue", lwd = 2, cex.main = 1.5)
    
  })
  
  
  
  output$effectifsHistFreqDens <- renderPlot({
    dt = data()
    # Histogramme des densitÃ©s de frÃ©quences
    hist( as.numeric(as.character(dt[2:149,input$radio])), freq = FALSE, cex.axis = 1.5, cex.main = 1.5,
          main = "Histogramme de la variable", col = "green",
          xlab = dt[1,input$radio] , ylab = "Densité de fréquences", las = 1,
          right = FALSE, cex.lab = 1.5)
  })
  
  
  # Force de la liaison entre 'especes' et 'couleur'
  # ----
  output$force <- renderTable({
    force.df <- as.data.frame(matrix(NA, nrow = 3, ncol = 1))
    rownames(force.df) = c("X2", "Phi2", "Cramer")
    
    # La table de contingence des profils observés
    tab = with(data()[2:150,], table(V6, V7))
    # La table de contigence s'il y a indépendence
    tab.indep = tab
    n = sum(tab)
    tab.rowSum = apply(tab, 2, sum)
    tab.colSum = apply(tab, 1, sum)
    print(tab.colSum)
    print(tab.rowSum)
    print(c)
    for(i in c(1:length(tab.colSum))){
      for(j in c(1:length(tab.rowSum))){
        tab.indep[i,j] = tab.colSum[i]*tab.rowSum[j]/n
      }
    }
    
    # Calcul du X²
    force.df[1,1] = sum((tab-tab.indep)^2/tab.indep)
    # Calcul du Phi²
    force.df[2,1] = force.df[1,1]/n
    # Calcul du Cramer
    force.df[3,1] = sqrt(force.df[2,1]/(min(nrow(tab), ncol(tab))-1))
    
    force.df
    
  }, rownames=TRUE, colnames=FALSE)
  
  # Unidimensionnel
  output$barplotUni <- renderPlot({
    # Diagramme en barres de la variable 'Level' avec ggplot
    
    ggplot(data()[2:150,], aes(x = V6)) + geom_bar()
  })
  
  
  
  output$barplotProfils <- renderPlot({
    # Diagramme de profils entre les variables 'V2' et 'V6'
    ggplot(data()[2:150,], aes(x = V7, fill = V6)) + geom_bar(position = "fill")
  })
  
  # Bidimensionnel
  output$barplotBi <- renderPlot({
    # Diagramme en barres entre les variables 'V2' et 'V6'
    ggplot(data()[2:150,], aes(x = V7, fill = V6)) + geom_bar()
  })
  
  
  output$barplotProfils <- renderPlot({
    # Diagramme de profils entre les variables 'V2' et 'V6'
    ggplot(data()[2:150,], aes(x = V7, fill = V6)) + geom_bar(position = "fill")
  })
  
  
  # Table de contingence entre 'Sex' et 'Level'
  # ----
  output$contingency <- renderTable({
    tab = with(data()[2:150,], table(V6, V7))
    round(tab/sum(tab), 3)
    tab
  })
}

shinyApp(ui, server)