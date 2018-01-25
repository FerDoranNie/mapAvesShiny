####################################
#Creado por Fernando Dorantes Nieto <(°) 
#                                     ( >)"
#                                      /|
####################################

library(shiny)
library(leaflet)

shinyUI(fluidPage(
  theme="./bootstrap.css",
  # tags$head(
  #   tags$link(href="./bootstrap.css")
  # ),
  # Titulo de la app
  titlePanel(title= div(img(src="./bird3.png", width=50, height= 50),
                        "App de busqueda de información de aves")),
  
  tags$h3("Usando Ebird, GBIF e  información del AOU"),
  
  tags$b("Usa el nombre cientifico de cualquier ave de Norteamérica y América Central"),
  tags$br(),
  tags$b("VAMOS, SON MÁS DE  2000 ESPECIES DIFERENTES"),
  tags$div(
    tags$b("Por ejemplo: "),
    tags$b("algunas especies son:"),
    em("Pharomachrus mocinno,"), 
    em("Columbina inca,"),
    em("Pyrocephalus rubinus,"), 
    em("Accipiter striatus,"), 
    tags$b("etc, etc")
    
  ),
  tags$div(
    id="Git",
    tags$a(href="https://www.github.com",
           icon("github-square", "fa-5x"),
           icon("github-alt", "fa-5x"),
           tags$br(),
           "¿Quiere saber como se hizo esta app?",
           tags$br(),
           "Da click en el gatito de Github"
           
    )
  ),
  
  navbarPage(
    "Buscando aves",
    # inverse = T,
    tabPanel(
    "App",
    # includeHTML("./www/extras.html"),
    # htmlOutput("test", click="testillo"),
    # htmlOutput("test"),
    
    # Sidebar with a slider input for number of bins 
    textInput("inserta", "Nombre ave", "Escribe el nombre de un ave"),
    verbatimTextOutput("valor"),
    htmlOutput("imagencilla"),

    
    h3("Las 100 aves más cercanas a ti de la especie que buscaste."),
    leafletOutput("mapita"),
    plotOutput("tmapMapita"),
    h3("Lo que se dice en Wikipedia de tu especie"),
    h4("Las palabras más grandes son las más frecuentes y las más pequeñas las que menos se mencionan"),
    tags$br(),
    
    tabsetPanel(
      tabPanel("Nube de palabras ingles", plotOutput("nubePalabrosa")),
      tabPanel("Nube de palabras español", plotOutput("nubePalabrosaEspaniol"))
      
    ),
    tags$br(),
    
    verbatimTextOutput("testWeb"),
    verbatimTextOutput("testWebEspaniol"),
    
    h3("Tabla de información de Ebird y general"),
    mainPanel(
      tabsetPanel(
        tabPanel("Ebird Información",tableOutput("tablaTest")),
        tabPanel("AOU Información", tableOutput("tablaEspecie")),
        # tabPanel("AOU Información", DT::dataTableOutput("tablaEspecie")), 
        tabPanel("GBIF Información",tableOutput("tablaGbif")) 
      )
      # dataTableOutput("tablaTest")  
    )
    # mainPanel(
    #   tabsetPanel(
    #     tabPanel("Mapa dinámico",   leafletOutput("mapita")),
    #     tabPanel("Mapa estático y con tipo de tierra",   plotOutput("tmapMapita"))
    #   )
    # ),
    # 
    # sidebarPanel(
    #     position="left",
    #     sidebarPanel(),
    #     mainPanel(
    #       plotOutput("nubePalabrosa"),
    #       tableOutput("tablaTest")  
    #     )
    #     
    #     # sidebarPanel(),
    #  
    #   
    # )
      
    ),
    tabPanel("Información de esta APP",
             includeMarkdown("./www/acercade.md")),
    tabPanel("Autor",
             includeMarkdown("./www/autor.md"))
    
  )

))
