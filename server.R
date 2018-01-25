####################################
#Creado por Fernando Dorantes Nieto <(°) 
#                                     ( >)"
#                                      /|
####################################

# Esta aplicación tiene como objetivo
# dar a conocer las posibilidades de crear un buscador de datos de aves 
# mediante la api de EBIRD


########################### LIBRERIAS

library(shiny) ## para la app
library(magrittr) # magia pokemon %>%
library(data.table) # el todopoderoso data table 
library(rebird) # conexión con la API de Ebird
library(rvest) ## Web Scraping
library(httr) # Web Scraping
library(XML) # Leer archivos XML y propiedades de los mismos
library(xml2) # Web scraping y lo mismo que XML
library(tm) # minería y manipulación de texto
library(wordcloud) # nubes de palabras
library(palettetown) # colores de los pokemon
library(leaflet) # mapas dinámicos
library(sp) # datos geográficos y manipulación
library(leaflet.extras) # funciones extras para leaflet
library(tmap) # mapas temáticos
library(rgdal) # data espacial
library(maptools) # herramientas extras
library(rgbif) # Conexión a GBIF

########################### FUNCIONES

"%!in%" <- function(x,y)!("%in%"(x,y)) # función para definir que cosas no están dentro de un objeto

tmMaker <- function(X){
  texto <- gsub("[[:punct:]]", "", X)
  texto <- iconv(texto, to="ASCII//TRANSLIT")
  texto <- iconv(texto, to="utf-8")
  texto <- VectorSource(texto)
  texto <- VCorpus(texto)
  texto <- texto %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwords("sp")) %>%
    tm_map(removeWords, stopwords("en")) %>%
    tm_map(removeWords, stopwords("ge")) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace) %>%
    tm_map(PlainTextDocument) %>%
    tm_map(removeNumbers)
  return(texto)
}




# ^ Es la función contraria a %in% de r Base

## ejemplos
"a" %in% c("a", "b", "c") # se traduce como ¿a está en el grupo de letras a,b y c?, la respuesta es si o TRUE 
"z" %!in% c("a", "b", "c")# se traduce como ¿z NO está en el grupo de letras a,b y c?, la respuesta es si o TRUE


########################### DATOS GENERALES Y OBJETOS GLOBALES

coloresArticuno <- pokepal("articuno", spread = 12) # colores de !! Articuno
coloresBulbasaur <- pokepal("bulbasaur", spread = 12) # colores de !! Bulbasaur
# paleta <- c("darkgreen", "brown", "green", "lightblue", "darkgreen", "gray50", 
#             "white", "steelblue")
paleta <- c("#008744", "#904141", "#3b852f", "#214c57", "#73CCCA", "gray50",
            "white", "#559DEE")


dataAOU <- read.csv("./www/NACC_list_species.csv", header = T, 
                    fileEncoding = "utf-8")


imagen1 <- "./www/imagenes/bird3.png"

icono <- makeIcon("./www/imagenes/bird3.png",
                  10, 10)


data("wrld_simpl")
data("land")
data("World")

dataAOU <- dataAOU %>% 
  data.table %>% 
  .[, nombres := tolower(species)]

dataAves <- dataAOU %>% 
  .[, c("avibase.id", "rank", "common_name", "order", "family", 
        "subfamily", "genus", "species", "nombres")] 

names(dataAves) <- c("id_avibase", "tipo", "Nombre_ingles", "Orden", "Familia",
                     "SubFamilia", "Genero", "Especie", "nombre")

webPage <- "https://en.wikipedia.org/wiki/"
webPageSpanish <- "https://es.wikipedia.org/wiki/"

img <- "https://upload.wikimedia.org/wikipedia/commons/6/6e/Passer_domesticus_male_%2815%29.jpg"


# test <-  nearestobs("Passer domesticus", max = 100)
# leaflet(data=test) %>% 
#   addTiles() %>%
#   # addProviderTiles(provider = "OpenTopoMap") %>% 
#   addMarkers(~lng, ~lat, icon = icono)
  


# test2 <- test  %>%  
#   data.table %>% 
#   .[, c("lng", "lat")] %>% 
#   data.frame


# test2 <- sf::st_as_sf(test2, coords=c("lng", "lat"), crs=4326)
# limites <- bbox(SpatialPoints(cbind(test$lng, test$lat)))
# 
# 
# tm_shape(land, bbox=limites)+
#   tm_raster("cover_cls", title = "Tipo de suelo", palette = paleta,
#             labels = c("Bosque", "Otro tipo de vegetacion natural", 
#                        "Tierras de cultivo", "Humedal", "Vegetación dispersa", 
#                        "Ciudad", "Hielo/nieve/tundra", "Oceanos/rios/lagos"))+
#   tm_legend(position=c("right", "bottom"), frame=T, bg.color="transparent", 
#             aes.color= c(lines="black" ))+
#   tm_layout(inner.margins = c(0.3, 0.4, 0.2, 0.1 ), legend.frame = "red",
#             legend.hist.width = 18, legend.outside = T, 
#             legend.outside.position = c("left", "bottom"))+
#   tm_shape(wrld_simpl ) +
#   tm_borders(col="steelblue")+
#   tm_shape(test2)+
#   tm_dots(shape = 20, size = 0.2)

# X <- data.frame(name_lookup(query="Ectopistes", rank= "species", return="data" , 
#                             status = "ACCEPTED"))
# 
# X <- X %>% 
#   data.table %>% 
#   .[class=="Aves"] %>% 
#   .[, c("species",  "genus", "family", 
#         "order", "class", "taxonomicStatus")] %>% 
#   # .[, extinct :=  ifelse(is.na(extinct), F, extinct)] %>% 
#   .[order(species)] %>%  
#   .[!is.na(genus)] %>% 
#   .[!is.na(order)]  %>% 
#   unique


########################### APLICACION


shinyServer(function(input, output) {
  X <- reactiveValues()
  X$data <- NULL
  observe({
    if(tolower(input$inserta) %in% dataAves$nombre){
      X$data <- data.frame(nearestobs(species = input$inserta, 
                                      max = 100, locale="es_MX"))
    }
    
    
  }
    
  )
  
  Scraping <- reactiveValues()
  
  # general <- eventReactive(input$inserta, {
  #   data <- data.frame(nearestobs(species = "#inserta", 
  #                         max = 100, locale="es_MX"))
  #   
  #   
  # }
  # )
  
  #  output$imagencilla <- renderImage({
  #   if(input$inserta== "Escribe el nombre de un ave"){
  #     salida <- imagen1
  #   }else{
  #     if(tolower(input$inserta) %in% dataAves$nombre){
  #       webPage <- paste0(webPage,
  #                         tolower(paste(unlist(strsplit(input$inserta, "[[:space:]]","")),
  #                                       collapse="_")))
  #       pagina <- read_html(webPage)
  #       pagina <- htmlParse(pagina, asText = T)
  #       imagen <- xpathSApply(pagina, path="//*[@property='og:image']", xmlAttrs)
  #       salida <- as.character(imagen[2])
  #       print("gatitos")
  #       print(salida)
  #       }else{
  #          salida <- imagen1
  #     }
  #   }
  #   list(src=salida,
  #        width= 50,
  #        height=50)
  #   }, deleteFile = F
  # )
   output$imagencilla <- renderText(
     {
       tist <- NULL
       if(tolower(input$inserta) %in% dataAves$nombre){
         pagina <- paste0(webPage,
                           tolower(paste(unlist(strsplit(input$inserta, "[[:space:]]","")),
                                         collapse="_")))
         pagina <- read_html(pagina)
         pagina <- htmlParse(pagina, asText = T)
         imagen <- xpathSApply(pagina, path="//*[@property='og:image']", xmlAttrs)
         salida <- as.character(imagen[2])
         tist <- c('<img src="', salida,  '" >')
         # c('<img src=', img, '">')
         # tist <- c('<img src=', img, '">')
         # print(paste0('<img src= "',salida,  ' height="50"', ' width=50', '">'))
       }else{
         tist="Aquí va la imágen del ave que buscaste. Si no es correcto, no aparecerá"
       }
       tist
     }
   )
   
  output$test <- renderText({
     c('<img src=', img, '">')
     # '<img src="https://upload.wikimedia.org/wikipedia/commons/6/6e/Passer_domesticus_male_%2815%29.jpg" height="50px" width=50px">'
     })

  # observeEvent(input$testillo, {print("batman")})
  
  output$valor <- renderText(
      if(input$inserta !="Escribe el nombre de un ave"){
        input$inserta
      }
    )

  output$tablaTest<-  renderTable(
    if(input$inserta== "Escribe el nombre de un ave"){
      data.frame(x="no has puesto datos", y= "pon el nombre de un ave")    
    }else{
      if(tolower(input$inserta) %in% dataAves$nombre){
        # data <- data.frame(nearestobs(species =  input$inserta, 
        #                               max = 100, locale="es_MX"))   
        data <- X$data %>% 
          data.table %>% 
          .[, !c("locId")]
        names(data)<- c("longitud", "localidad_ave_vista", "cuantas_aves", 
                        "nombre_cientifico", "obsValida",
                        "sitio_avistamiento_privado", 
                        "fecha_observacion", "observacion_Revisada", 
                        "Nombre comun ingles", "latitud", "idLocalidad")
        data.frame(data)
      }else{
        data.frame(x="Nombre de ave", y= "Incorrecto")   
      }
    }
    

  )

  output$tablaEspecie<-  renderTable(
    if(input$inserta== "Escribe el nombre de un ave"){
      data.frame(x="no has puesto datos", y= "pon el nombre de un ave")    
    }else{
      if(tolower(input$inserta) %in% dataAves$nombre){
        # data <- data.frame(nearestobs(species =  input$inserta, 
        #                               max = 100, locale="es_MX"))   
        dataG_ave <- dataAves %>% 
          data.table %>% 
          .[nombre== tolower(input$inserta)] %>% 
          .[, !c("nombre")]
        
        data.frame(dataG_ave)
      }else{
        data.frame(x="Nombre de ave", y= "Incorrecto")   
      }
    }
    

  )
  
  output$tablaGbif <-  renderTable(
    if(input$inserta== "Escribe el nombre de un ave"){
      data.frame(x="no has puesto datos", y= "pon el nombre de un ave")    
    }else{
      if(tolower(input$inserta) %in% dataAves$nombre){
        print("llega")
        genero <- strsplit(input$inserta, "[[:space:]]")
        genero <- unlist(genero)
        genero <- genero[1]
        genero <- as.character(genero)
        X <- data.frame(name_lookup(query=tolower(genero), rank= "species", return="data" , 
                                    status = "ACCEPTED"))
        
        X <- X %>% 
          data.table %>% 
          .[class=="Aves"] %>% 
          .[, c("species",  "genus", "family", 
                "order", "class", "taxonomicStatus")] %>% 
          # .[, extinct :=  ifelse(is.na(extinct), F, extinct)] %>% 
          .[order(species)] %>%  
          .[!is.na(genus)] %>% 
          .[!is.na(order)]  %>% 
          unique
        print(genero)
        print(dim(X))
        data.frame(X)
      }else{
        data.frame(x="Nombre de ave", y= "Incorrecto")   
      }
    }
    
    
  )
  

  output$testWeb <- renderText({
    if(input$inserta !="Escribe el nombre de un ave"){
      webPage <- paste0(webPage,
                        paste(unlist(strsplit(input$inserta, "[[:space:]]","")),collapse="_")
      )
      webPage
    }
  
    })

  output$testWebEspaniol <- renderText({
    if(input$inserta !="Escribe el nombre de un ave"){
      webPageSpanish <- paste0(webPageSpanish,
                        paste(unlist(strsplit(input$inserta, "[[:space:]]","")),collapse="_")
      )
      webPageSpanish
    }
  
    })
  

  
  output$nubePalabrosa <- renderPlot({
    webPage <- paste0(webPage,
                      tolower(paste(unlist(strsplit(input$inserta, "[[:space:]]","")),
                                    collapse="_")))
    
    if(input$inserta!= "Escribe el nombre de un ave" ){
      # if("try_error" %!in% class(try(read_html(webPage)))){
      if(tolower(input$inserta) %in% dataAves$nombre){
        pagina <- read_html(webPage)
        pagina <- htmlParse(pagina, asText = T)
        print("Me quiero volver mono")
        print(webPage)
        imagen <- xpathSApply(pagina, path="//*[@property='og:image']", xmlAttrs)
        imagen <- as.character(imagen[2])
        
        texto <- xpathApply(pagina, path="//p", xmlValue)
        texto <- as.character(unlist(texto)) %>%
          paste(collapse = " ")

        # texto <- gsub("[[:punct:]]", "", texto)
        # texto <- iconv(texto, to="ASCII//TRANSLIT")
        # texto <- iconv(texto, to="utf-8")
        # texto <- VectorSource(texto)
        # texto <- VCorpus(texto)
        # texto <- texto %>%
        #   tm_map(content_transformer(tolower)) %>%
        #   tm_map(removeWords, stopwords("sp")) %>%
        #   tm_map(removeWords, stopwords("en")) %>%
        #   tm_map(removeWords, stopwords("ge")) %>%
        #   tm_map(removePunctuation) %>%
        #   tm_map(stripWhitespace) %>%
        #   tm_map(PlainTextDocument) %>%
        #   tm_map(removeNumbers)
        texto <- tmMaker(texto)
        # par(mar= c(0,0,0,0))
        # wordcloud(texto, max.words = 50, colors = coloresArticuno, scale=c(5,1),
        #           min.freq = 0.1)
        matrizPrueba <- texto %>% TermDocumentMatrix %>%  matrix 
        if(dim(matrizPrueba)[1]>50){
          par(mar= c(0,0,0,0))
          
          wordcloud(texto, max.words = 50, colors = coloresArticuno, scale=c(5,1),
                    min.freq = 0.1)
        }else{
          par(mar= c(0,0,0,0))
          
          wordcloud(texto, max.words = 50, colors = "black", scale=c(5,1),
                    min.freq = 0.1)
        }
        # text(x = 0.5, y = 1, labels = paste("Lo que se dice en wikipedia de: \n", 
        #                                     input$inserta, sep=""))
      }else{
        print("Mono mono mono") ## print de testeo del app 
        texto <- "Este no es el nombre de un ave de Norteamérica.
                \n Por favor usa un nombre científico de alguna ave.
                \n This is not a valid species from North America.
                \n Please use scientific name of a bird.
                 "
        plot(x= 1, y= 1, type="n", xaxt="n", yaxt="n", xlab="", ylab="")
        text(x=1, y=1, labels = texto)
        # wordcloud(texto, max.words = 100, colors = coloresArticuno, scale=c(3,1),
        #           min.freq = 0.1)
      }
    
    }else{
      print("Me volvi mono")
      texto <- "Escribe el nombre de un ave de Norteamérica."
      plot(x= 1, y= 1, type="p", xaxt="n", yaxt="n", xlab="", 
           ylab="", col="steelblue", pch=20)
      text(x=1, y=1, labels = texto)
      
    } 
    
  } )
  
  output$mapita <- renderLeaflet(
    if(input$inserta!= "Escribe el nombre de un ave" ){
      if(tolower(input$inserta) %in% dataAves$nombre){
        # data <- data.frame(nearestobs(species =  input$inserta, 
        #                               max = 100, locale="es_MX"))
        leaflet(data= X$data) %>% 
          addTiles %>% 
          addMarkers(~lng, ~lat, icon = icono)
        
      }else{
        leaflet() %>% 
          addTiles() %>% 
          addMarkers(lng = -99.141546, lat= 19.429065) %>% 
          addPopups(lng = -99.141546, lat= 19.429065, 
                    popup="Este mapa permanererá estático mientras
                    no se ponga un nombre correcto de ave",
                    options = popupOptions(closeButton = FALSE)) 
      }
      
    }else{
      leaflet() %>% 
        addTiles() %>% 
        addMarkers(lng = -99.141546, lat= 19.429065) %>% 
        addPopups(lng = -99.141546, lat= 19.429065, 
                  popup="Este mapa permanererá estático mientras
                    no se ponga un nombre de un ave",
                  options = popupOptions(closeButton = FALSE))
      
    }
  )

  output$nubePalabrosaEspaniol <- renderPlot({
    webPageSpanish <- paste0(webPageSpanish,
                      tolower(paste(unlist(strsplit(input$inserta, "[[:space:]]","")),
                                    collapse="_")))
    
    if(input$inserta!= "Escribe el nombre de un ave" ){
      # if("try_error" %!in% class(try(read_html(webPage)))){
      if(tolower(input$inserta) %in% dataAves$nombre){
        pagina <- read_html(webPageSpanish)
        pagina <- htmlParse(pagina, asText = T)
        print("Me quiero volver mono")
        print(webPage)
        imagen <- xpathSApply(pagina, path="//*[@property='og:image']", xmlAttrs)
        imagen <- as.character(imagen[2])
        
        texto <- xpathApply(pagina, path="//p", xmlValue)
        texto <- as.character(unlist(texto)) %>%
          paste(collapse = " ")
  
        # texto <- gsub("[[:punct:]]", "", texto)
        # texto <- iconv(texto, to="ASCII//TRANSLIT")
        # texto <- iconv(texto, to="utf-8")
        # texto <- VectorSource(texto)
        # texto <- VCorpus(texto)
        # texto <- texto %>%
        #   tm_map(content_transformer(tolower)) %>%
        #   tm_map(removeWords, stopwords("sp")) %>%
        #   tm_map(removeWords, stopwords("en")) %>%
        #   tm_map(removeWords, stopwords("ge")) %>%
        #   tm_map(removePunctuation) %>%
        #   tm_map(stripWhitespace) %>%
        #   tm_map(PlainTextDocument) %>%
        #   tm_map(removeNumbers)
        
        texto <- tmMaker(texto)
        matrizPrueba <- texto %>% TermDocumentMatrix %>%  matrix 
        if(dim(matrizPrueba)[1]>50){
          par(mar= c(0,0,0,0))
          
          wordcloud(texto, max.words = 50, colors = coloresArticuno, scale=c(5,1),
                    min.freq = 0.1)
        }else{
          par(mar= c(0,0,0,0))
          
          wordcloud(texto, max.words = 50, colors = "black", scale=c(5,1),
                    min.freq = 0.1)
        }
        # text(x = 0.5, y = 1, labels = paste("Lo que se dice en wikipedia de: \n", 
        #                                     input$inserta, sep=""))
      }else{
        print("Mono mono mono") ## print de testeo del app 
        texto <- "Este no es el nombre de un ave de Norteamérica.
                \n Por favor usa un nombre científico de alguna ave.
                \n This is not a valid species from North America.
                \n Please use scientific name of a bird.
                 "
        plot(x= 1, y= 1, type="n", xaxt="n", yaxt="n", xlab="", ylab="")
        text(x=1, y=1, labels = texto)
        # wordcloud(texto, max.words = 100, colors = coloresArticuno, scale=c(3,1),
        #           min.freq = 0.1)
      }
      
    }else{
      print("Me volvi mono")
      texto <- "Escribe el nombre de un ave de Norteamérica."
      plot(x= 1, y= 1, type="p", xaxt="n", yaxt="n", xlab="", 
           ylab="", col="steelblue", pch=20)
      text(x=1, y=1, labels = texto)
      
    } 
    
  } )
  
  
  output$tmapMapita <- renderPlot(
    if(input$inserta!= "Escribe el nombre de un ave" ){
      if(tolower(input$inserta) %in% dataAves$nombre){
        # data <- data.frame(nearestobs(species =  input$inserta, 
        #                               max = 100, locale="es_MX"))
        test <- X$data
        test2 <- X$data  %>%
          data.table %>%
          .[, c("lng", "lat")] %>%
          data.frame
        
        
        test2 <- sf::st_as_sf(test2, coords=c("lng", "lat"), crs=4326)
        limites <- bbox(SpatialPoints(cbind(test$lng, test$lat)))
        
        
        tm_shape(land, bbox=limites)+
          tm_raster("cover_cls", title = "Tipo de suelo", palette = paleta,
                    labels = c("Bosque", "Otro tipo de vegetacion natural", 
                               "Tierras de cultivo", "Humedal", "Vegetación dispersa", 
                               "Ciudad", "Hielo/nieve/tundra", "Oceanos/rios/lagos"))+
          tm_legend(position=c("right", "bottom"), frame=T, bg.color="transparent", 
                    aes.color= c(lines="black" ))+
          tm_layout(inner.margins = c(0.3, 0.4, 0.2, 0.1 ), legend.frame = "red",
                    legend.hist.width = 18, legend.outside = T, 
                    legend.outside.position = c("left", "bottom"))+
          tm_shape(wrld_simpl ) +
          tm_borders(col="steelblue")+
          tm_shape(test2)+
          tm_dots(shape = 20, size = 0.2)
        
        
      }else{
        tm_shape(World)+
          tm_borders(col = "steelblue")+
          tm_layout(title="Este mapa \n permanecerá \n estático \n mientras \n no se ponga \n un nombre \n correcto de ave",
                    title.size = 1, title.col="red")
      }
      
    }else{
      tm_shape(World)+
        tm_borders(col = "steelblue")+
        tm_layout(title="Este mapa \n permanecerá \n estático \n mientras \n no se ponga \n un nombre \n  de ave",
                  title.size = 1, title.col="red")      
    }
  )
  
  
})
