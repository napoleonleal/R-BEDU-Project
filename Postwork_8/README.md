# 📂 POSTWORK 8

Link para la aplicacion Shiny:
https://omar-magaa.shinyapps.io/Postwork8/

Debido a la identación y anidación que se genera al hacer el código se
presenta por bloques comentado


## 📋 Creamos el logo para la aplicación Shiny

``` r
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinythemes)
library(plotly)

customLogo <- shinyDashboardLogoDIY(
    boldText = tags$a("Equipo 10",href="https://github.com/napoleonleal/R-BEDU-Project")
    , mainText = ""
    , textSize = 16
    , badgeText = "BEDU"
    , badgeTextColor = "white"
    , badgeTextSize = 3
    , badgeBackColor = "#000000"
    , badgeBorderRadius = 5
)
```

## 📋 Definimos la UI para la aplicacion

``` r
ui <- fluidPage(
    # Creamos la pagina con un dashboard
    dashboardPage( title = "Equipo 10",
                   # Definimos el header de la pagina
                   dashboardHeader(title = customLogo
                   ),
                   # Creamos una SideBar y definimos los elementos que contendra
                   dashboardSidebar(
                       sidebarMenu(
                           menuItem(  "Inicio"
                                    , tabName = "home"
                                    , icon = icon("home")),
                           menuItem(  "Graficas de barras"
                                    , tabName = "graficas"
                                    , icon = icon("bar-chart")),
                           menuItem(  "Goles casa - visitante"
                                    , tabName = "post3"
                                    , icon = icon("area-chart")),
                           menuItem(  "Data Table"
                                    , tabName = "data_table"
                                    , icon = icon("table")),
                           menuItem(  "Factores de Ganacias"
                                    , tabName = "ganacias"
                                    , icon = icon("money")),
                           menuItem(  "Repositorios"
                                    , tabName = "gh"
                                    , icon = icon("github"))
                       )
                   ),
```

``` r
                   # Definimos el body del dashboar
                   dashboardBody(
                       shinyDashboardThemes(
                          # Especificamos el tema que vamos a utlizar para la aplicacion
                           theme = "purple_gradient"
                       ),
                       # Definimos el cuerpo para cada tab del menu
                       tabItems(
                           # Inicio
                           tabItem(tabName = "home",
                                   fluidRow(
                                       column(8, align="center", offset = 2,
                                              strong( h1("BIENVENIDO AL SHINY DEL EQUIPO 10"),
                                                      h2("AQUI PODRÁS INTERACTUAR CON NUESTROS RESULTADOS"),
                                                      tags$br(),
                                                      h2("INTEGRANTES DEL EQUIPO: "),
                                                      h3("María Magdalena Castro Sam"), 
                                                      h3("Sergio Napoleón Leal"), 
                                                      h3("Jesús Omar Magaña Medina"),
                                                      h3("Fernando Itzama Novales Campos"), 
                                                      h3("Adrián Ramírez Cortés"), 
                                                      h3("Efraín Soto Olmos")
                                                    ),
                                              img(src = "blob.png", height = 250, width = 250),
                                              tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")
                                       )
                                   )
                           ),
```

``` r
                           # Grafica de barras de los goles
                           tabItem(tabName = "graficas",
                                   fluidRow(
                                     column(8, align="center", 
                                            offset = 2, 
                                            h1("Goles a favor y en contra por equipo")),
                                     selectInput("x", "Selecciona el valor de x",
                                                 choices = c("Goles Locales", 
                                                             "Goles Visitantes")),
                                     plotlyOutput("grafica8",height = 800)
                                   ),
                           ),
```

``` r
                           # Graficas Postwork 3
                           tabItem(tabName = "post3", 
                                   fluidRow(
                                       column(8, align="center", offset = 2,
                                              strong( h1("Gráficas del PostWork 3"))),
                                       box(title = "P(x) Marginal Equipo Local meta Gol",
                                           plotlyOutput("plotPW1")),
                                       box(title = "P(y) Marginal Equipo Visitante meta Gol",
                                           plotlyOutput("plotPW2")),
                                       box(title = "Figura 3.3 P(x∩y) conjunta",
                                           plotlyOutput("plotPW3"))
                                   )
                           ),
```

``` r
                           # Data table del archivo match.data.csv
                           tabItem(tabName = "data_table",
                                   fluidRow(        
                                     column(8, align="center", 
                                            offset = 2,  
                                            titlePanel(h3("Data Table de la Liga Española"))),
                                       dataTableOutput ("data_table")
                                   )
                           ), 
```

``` r
                           # Grafica de Momios
                           tabItem(tabName = "ganacias",
                                   fluidRow(
                                       column(8, align="center", offset = 2,
                                              strong( 
                                                h1("Gráficas de los factores de ganancia mínimo y máximo"))),
                                       column(9, align="right", offset = 2,
                                              radioButtons("picture", "Tipo de momios:",
                                                          c("Escenario con momios máximos",
                                                            "Escenario con momios promedio"))
                                              ),imageOutput("imagenMomios")
                                   )
                           ),
```

``` r
                           # Repositorios
                           tabItem(tabName = "gh",
                                   fluidRow(
                                      box(title= "Repositorio version GitHub", 
                                          tags$a(img(src= "git.png", 
                                                     height = 320, width = 580), 
                                                     href= "https://github.com/napoleonleal/R-BEDU-Project")),
                                      box(title="Repositorio version HTML",  
                                          tags$a(img(src= "page.png", height = 320, width = 580), 
                                                    href= "https://itzamango.github.io/postwork-equipo-10/"))
                                   )
                           )
                       )
                   )
    )
)
```

## 📋 Definimos las funciones del servidor

``` r
server <- function(input, output) {

    library(ggplot2)
    library(dplyr)
    library(viridis)
    library(viridisLite)
    
    # Leemos el archivo de los resultados de los partidos de la liga española desde 2010
    # hasta 2020
    df.resultado = read.csv("https://raw.githubusercontent.com/omar17md/ejemplo_1/main/df_resultado.csv")
```

``` r
    #Gráfico de goles
    output$grafica8 <- renderPlotly({
      if(input$x == "Goles Locales"){
        p =  ggplot(df.resultado, aes( x = home.score ) ) +
          geom_bar(aes(fill = Result)) +
          facet_wrap( ~ away.team ) +
          labs(y = "Goles")+
          scale_fill_discrete(name = "Resultados", 
                              labels = c("Gano Visitante", 
                                         "Empate", "Gano Local"))
        #  +scale_y_continuous(limits = c(0,50))
        
      }else{
        p =  ggplot(df.resultado, aes( x = away.score ) ) +
          geom_bar(aes(fill = Result)) +
          facet_wrap( ~ away.team ) +
          labs(y = "Goles")+
          scale_fill_discrete(name = "Resultados", 
                              labels = c("Gano Visitante", 
                                         "Empate", "Gano Local"))
        #  +scale_y_continuous(limits = c(0,50))
      }
      
      
      ggplotly(p)
        
    })
```

``` r
    # Función para obtener la frecuencia y probabilidad
    get.prob.df<- function(data, team, name.gol, name.prob){ 
        team      <- enquo(team)
        data %>% 
            pull(!!team) %>% 
            table(., dnn = (name.gol)) %>% 
            as.data.frame %>% 
            mutate(!!name.prob := Freq/sum(Freq)) 
    }
    
    # Función para redondear 3 digitos
    get.round <- function(data, digits){
        data %>% mutate_if(is.numeric, round, digits=digits)
    }
    
    # Función para obtener la probabilidad conjunta en objeto tabla
    get.prob.joint.tbl <- function(data, team.h, team.a, name.h, name.a){
      data %>%
        {table( team.h,  team.a, dnn = c(name.h, name.a)) } %>% 
        prop.table
    }
```

``` r
    # Gráficas del postwork3
    # Leemos el archivo que se genero en el postwork2
    data <- "https://raw.githubusercontent.com/napoleonleal/R-BEDU-Project/main/Postwork_2/Postwork_02.csv" %>%
        read.csv %>% 
        mutate(Date = as.Date(Date, "%Y-%m-%d"))
    
    #  calculamos la probabilidad marginal de los goles metidos por locales:
    prob.m.local <- 
        get.prob.df(data, FTHG, "Gol.Home", "P.Marginal") 
    
    prob.m.local %>% get.round(3)
    
    # Creamos una función para gráficar los datos
    plot.bar <- function(data, x.lab, y.lab, f.lab, title){
        Goles         <- data[1] %>% unlist()
        Porcentaje    <- (data[3]*100 ) %>% unlist %>% round(., digits=2)
        Prob.Marginal <- data[3] %>% unlist() %>% round(., digits=4)
        data %>%
            ggplot() +
            geom_bar(stat = 'identity') +
            aes(   x = Goles
                   , y = Porcentaje
                   , fill = Porcentaje
                   , text = paste("Prob Marginal", Prob.Marginal,
                                  group = interaction(Goles, Porcentaje))
            ) +
            labs(  x     = x.lab
                   , y     = y.lab
                   , fill  = f.lab
                   , title = title
            ) +
            theme_minimal() +
            theme( text = element_text(size=15)
                   ,legend.title = element_text(size=10) 
            ) +
            scale_fill_viridis(name=f.lab, direction = 1) 
    }
```

``` r
   #Primera grafica del postwork3
   output$plotPW1 <- renderPlotly({ 
       
       plot.local = plot.bar(prob.m.local
                              , "Goles [n]"
                              , "Probabilidad de ocurrencia [%]"
                              , "%"
                              , "Probabilidad de anotación del equipo local" )
       ggplotly(plot.local)
   })
   
   
   
    prob.m.visit <- 
      get.prob.df(data, FTAG, "Gol.Away", "P.Marginal") 
    
    
    prob.m.visit %>% get.round(3)
    
    #Segunda grafica del postwork 3
    output$plotPW2 <- renderPlotly({
      plot.visit <- plot.bar(prob.m.visit
                             , "Goles [n]"
                             , "Probabilidad de ocurrencia [%]"
                             , "%"
                             , "Probabilidad de anotación del equipo visitante" )
      
         ggplotly(plot.visit) 
    })
```

``` r
    # Calculamos la probabilidad conjunta de que el equipo que juega en casa anote *'x'*
    # goles y el equipo que juega como visitante anote '*y'* goles:
    prob.joint <- 
      get.prob.joint.tbl(data, data$FTHG, data$FTAG, "Local", "Visitante") 
    
    prob.joint  %<>% "*"(100) %>% round(2)
    
    prob.joint %<>% as.data.frame() %>% rename(Probabilidad = Freq)
    
    # Creamos la fucion para graficar el mapa de calor
    plot.heat <- function(data, x.lab, y.lab, f.lab, title){
      ggplot(data) +
        aes(   Local
               , Visitante
               , fill = Probabilidad
        ) + #gráficamos
        geom_raster() +
        labs(  x     = x.lab
               , y     = y.lab
               , fill  = f.lab
               , title = title
        ) +
        theme(  text = element_text(size=18)
                , legend.title = element_text(size=15) 
        ) +
        scale_fill_viridis(  name=f.lab
                             , direction = 1 #, option = "H"
        ) 
    }
```

``` r
    # Tercera grafica del postwork 3
    output$plotPW3 <- renderPlotly({
      plot.mapa.calor <-
        plot.heat(  prob.joint
                    , "Local [goles]"
                    , "Visitante [goles]"
                    , "Probabilidad [%]"
                    , "Probabilidad Conjunta de anotación"
        )
      
      ggplotly(plot.mapa.calor)    #versión interactiva 
    })
```

``` r
    #Data Table
    df = read.csv("https://raw.githubusercontent.com/napoleonleal/R-BEDU-Project/main/Postwork_8/match.data.csv")
    output$data_table <- renderDataTable( {df}, 
                                          options = list(aLengthMenu = c(5,25,50),
                                                         iDisplayLength = 50)
    )
    
    # Imagenes momios
    output$imagenMomios <- renderImage({   
        if(input$picture == "Escenario con momios máximos"){            
            list(src = "www/momios1.png", height = 520, width = 1200)
        }                                        
        else if(input$picture == "Escenario con momios promedio"){
            list(src = "www/momios2.png", height = 520, width = 1200)
        }
    }, deleteFile = FALSE)
}

# Corremos la aplicacion
shinyApp(ui = ui, server = server)
```
