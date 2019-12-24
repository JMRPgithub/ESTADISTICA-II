library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = h3("TLC")),
  
  
  dashboardSidebar(
    sidebarMenu(
      br(),
      br(),
      
      menuItem("CARATULA", tabName = "caratula"),
     
      menuItem("DEFINICION", tabName = "definicion"),
      
      menuItem("SIMULACION", tabName = "simulacion")
    ),
    
   
    hr(),
    
    
    tags$button(class = "btn btn-primary btn-lg btn-block", "data-toggle"="collapse", "data-target"="#simulacion", "PARAMETROS"),
    
    tags$div(class = "collapse", id = "simulacion",
             
             selectInput("distribucion","distribucion",
                         c("Moneda",
                           "Dados",
                           "Normal",
                           "Gamma",
                           "Poisson"
                           
                         ),
                         selected = "Gamma"
             ),
             
             uiOutput("parametros"),
             
             sliderInput("muestra", "Tamano de Muestra n: ",
                         min = 1, max = 1000, value = 500),
             sliderInput("simulacion", "Numero de Simulaciones",
                         min = 1, max = 1000, value = 255)
             
             )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", href = "css/bootstrap.css"),
      tags$link(rel = "stylesheet", href = "css/styles.css"),
      tags$script(src = "js/bootstrap.js")
      
    ),
    
    tabItems(
      tabItem("caratula",
        tags$div(class = "fondo container-fluid", 
                 h2(class = "text-center", "UNIVESIDAD NACIONAL SAN CRISTOBAL DE HUAMANGA"),
                 h3(class = "text-center", "FACULTAD DE INGENIERIA DE MINAS GEOLOGIA Y CIVIL"),
                 tags$div(class = "container text-center",
                          img(src = "img/unsch.png", class = "img-fluid", height ="300", width ="200")
                          ),
                 h2(class = "text-center" ,"TRABAJO FINAL DE ESTADISTICA II"),
                 h3(class = "text-center", "PROFESOR"),
                 h4(class = "text-center texto", "Jackson Romero Plasencia"),
                 h3(class = "text-center", "INTEGRANTES"), 
                 h4(class = "texto text-center" ,"CONTRERAS MORENO, Luis Vidal"),
                 h4(class = "texto text-center" ,"BONILLA OCHOA, Edward"),
                 h4(class = "texto text-center" ,"HUARACA CCAHUIN, Rudy"),
                 h4(class = "texto text-center" ,"HUARACA HUARHUACHI, Marco"),
                 h4(class = "texto text-center" ,"RUA SULCA, Kevin")
                 )
          
        ),
      
      
      tabItem("definicion",
          tags$div(class = "container", 
                   h1(class = "texto text-center","Definicion del Teorema Central del Limite"),
                   br(),
                   h4(class = "texto","el teorema central del limite es una teoria estadistica que,
            dada una muestra suficientemenete grande de la poblacion, 
            la distribucion de las medias muestrales seguira una distribucion normal."),
                   
                   h4(class = "texto", "Ademas, el TCL afirma que a medida que el tamano 
                      de la muestra se incrementa, la media muestral se acercara a la media de
                      la poblacion. Por tanto, mediante el TCL podemos definir la distribucion
                      de la media muestral de una determinada poblacion con una varianza conocida.
                      De manera que la distribucion seguira una distribucion normal si el tamano 
                      de la muestra es lo suficientemente grande."),
                   br(),
                   br(),
                   
                   tags$div(class = "text-center",
                            img(src = "img/tcl.png", class = "img-fluid"))
                   
                   )
      ),
      
      
      tabItem("simulacion",
              
              tabsetPanel(
                type = "tabs",
                
                
                tabPanel(
                  title = "grafica a normal",
                  h1(class = " text-danger texto", tags$strong("SIMULACION")),
                  column(6, plotOutput("simulacion", width="900px", height="550px"),
                  textOutput("prueba")),
                  column(6, box(
                    title = "NOTA", status = "danger", collapsible = TRUE, solidHeader = TRUE,
                    h5("mientras en numero de simulaciones del ejercicio aumente, la grafica 
                       se acercara mas a tomar forma de una campana de gaus.")
                  ))
                  ),
                
                tabPanel(
                  title = "grafica QQ",
                  h1(class = "text-danger texto",strong("SIMULACION QQ")),
                  column(6, plotOutput("simulacion2", width="900px", height="550px")),
                  column(6, box(
                    title = "NOTA", status = "danger", collapsible = TRUE, solidHeader = TRUE,
                    h5("La linea recta esta representando una distribucion normal
                       con la cual compararemos las medias de la simulacion."),
                    h5("cada punto representa la media de cada uno de los lanzamiento o simulacion que hicimos."),
                    h5("si los puntos y la linea recta coinciden o tratan de coincidir significa que 
                       las medias tienden a la normalizacion.")
                  ))
                  )
              
              )
              
      
      )
    )
  )
)



server <- function(input, output) { 
  
  output$parametros = renderUI({
    switch (input$distribucion,
            Normal = list(numericInput("media",HTML("media &mu; "),0),
                          numericInput("desviacion",HTML("Desviacion &sigma;"),1,min = 0)),
            Gamma = list(numericInput("forma", "Parametro forma",
                                      min = 0.01,
                                      value = 1,
                                      step = 0.1),
                         numericInput("escala", "Parametro escala: ", 
                                      min = 1,
                                      value = 1,
                                      step = 0.0001))
            
    )
  })
  
  
  output$prueba <- renderText({
    paste("es: ", input$media)
  })
  
  output$simulacion <- renderPlot({ 
    
    #m <- input$lanzamiento
    #n <- input$ejercicio
    
    #sam <- vector("numeric", length = m)
    
    #for( i in 1:m){
    #  sam[i] <- mean(sample(1:2, n, replace = TRUE))
    #}
    
    n <- input$muestra
    m <- input$simulacion
    
    
    
    
    if(input$distribucion == "Normal"){
      set.seed(input$media * input$desviacion)
      muestras <- matrix(rnorm(m*n, input$media, input$desviacion), nrow = m)
      media <- input$media
      varia <- input$desviacion ^ 2
    }
    
    
    
    
    
    else if(input$distribucion == "Gamma"){
      
      set.seed(input$forma * input$escala)
      muestras <- matrix(rgamma(m*n, input$forma, input$escala), nrow = m)
      media <- input$forma * input$escala
      varia <- input$forma * input$escala ^ 2
    }
    
    else if(input$distribucion == "Poisson"){
      
      set.seed(123)
      muestras <- matrix(rpois(m*n, lambda = 1), nrow = m)
      media <- 0
      varia <- 0
    }
    
    else if(input$distribucion == "Moneda"){
      set.seed(1234)
      muestras <- matrix(sample(1:2, m*n, replace = TRUE), nrow = m)
      media <- 0
      varia <- 0
      
    }
    
    
    else if(input$distribucion == "Dados"){
      set.seed(1234)
      muestras <- matrix(sample(1:6, m*n, replace = TRUE), nrow = m)
      media <- 0
      varia <- 0
      
    }
    
    
    
    
    
    
    medias <- rowMeans(muestras)
    
    
    hist(medias, col = "grey", breaks = 20, freq = F)
    #lines(density(medias), lwd = 5, col = 'dodgerblue3')
    curve(dnorm(x, mean=media, sd=sqrt(varia/n)),
          add=T, lwd=5, col='dodgerblue3')
    
    
    
    
  })

  
  #-----------------------------------------------------
  
  
  output$simulacion2 <- renderPlot({ 
    
    #m <- input$lanzamiento
    #n <- input$ejercicio
    
    #sam <- vector("numeric", length = m)
    
    #for( i in 1:m){
    #  sam[i] <- mean(sample(1:2, n, replace = TRUE))
    #}
    
    
    
    n <- input$muestra
    m <- input$simulacion
    
    
    
    
    if(input$distribucion == "Normal"){
      set.seed(input$media * input$desviacion)
      muestras <- matrix(rnorm(m*n, input$media, input$desviacion), nrow = m)
      media <- input$media
      varia <- input$desviacion ^ 2
    }
    
    else if(input$distribucion == "Gamma"){
      
      set.seed(input$forma * input$escala)
      muestras <- matrix(rgamma(m*n, input$forma, input$escala), nrow = m)
      media <- input$forma * input$escala
      varia <- input$forma * input$escala ^ 2
    }
    
    
    else if(input$distribucion == "Poisson"){
      
      set.seed(123)
      muestras <- matrix(rpois(m*n, lambda = 1), nrow = m)
      media <- 0
      varia <- 0
    }
    
    if(input$distribucion == "Moneda"){
      set.seed(1234)
      muestras <- matrix(sample(1:2, m*n, replace = TRUE) == 1, nrow = m)
      media <- 1
      varia <- 1
      
    }
    
    if(input$distribucion == "Dados"){
      set.seed(1234)
      muestras <- matrix(sample(1:6, m*n, replace = TRUE), nrow = m)
      media <- 1
      varia <- 1
      
    }
    
    
    
    
    
    medias <- rowMeans(muestras)
    
    
    #hist(medias, breaks = 30, freq = F)
    #lines(density(medias), lwd = 5, col = 'dodgerblue3')
    #curve(dnorm(x, mean=media, sd=sqrt(varia/n)),
    #      add=T, lwd=5, col='dodgerblue3')
    
    qqnorm(medias, las=1, pch=1, col='dodgerblue3')
    qqline(medias)
    
    
  })
  
  
  }

shinyApp(ui, server)



# getwd()
# setwd("D:/programacion/r")
# runApp("trabajo_final")
# runApp("trabajo_final", display.mode = "showcase")