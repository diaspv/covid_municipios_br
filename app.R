# Script para a elaboração do mapa de COVID19 por municípios
#      Elaborado por Patrick Dias <github.com/diaspv/>
#              Fonte dos dados brasil.IO


# Carregar as bibliotecas necessárias
pcts = c("shiny", "tidyverse", "leaflet", "shinythemes", "shinycssloaders")
for(i in pcts){
    if(!require(i, character.only = TRUE)) 
        install.packages(i)
    library(i, character.only = TRUE)
}


# Carregamento dos dados necessários
# Dados de coordenadas dos municípios
coordenadas <- read.csv("./DADOS/Municipios_Lat-Long.csv")  
coordenadas[1] <- NULL
# Dados do COVID19 no Brasil. Fonte: brasil.io
dados <- read.csv("https://brasil.io/dataset/covid19/caso?format=csv", stringsAsFactors = FALSE, encoding = "UTF-8") 
atualizado <- as.character(format(max(as.Date(dados$date)), "%d/%m/%Y"))
dados <- dados %>% 
    filter(is_last == "True") %>% 
    filter(place_type == "city") %>%
    select(city, absolutos = confirmed, relativos = confirmed_per_100k_inhabitants, obitos = deaths, letalidade = death_rate, city_ibge_code) %>% 
    merge(coordenadas, by = "city_ibge_code", all.x = T) %>% 
    mutate(relativos = replace_na(relativos, 0)) %>% 
    arrange(desc(absolutos))
rm(coordenadas)
# Popup do mapa
dados$popup <- paste0(
    "<b>", dados$city, "</b>", ":", "<br>",
    "Nº Casos Confirmados: ", dados$absolutos, "<br>",
    "Casos por 100mil hab: ", sub(".", ",", format(round(dados$relativos, 2), nsmall = 2), fixed = TRUE), "<br>",
    "Nº Óbitos: ", dados$obitos, "<br>",
    "Letalidade: ", as.integer(dados$letalidade*100), "%",
    sep = ""
)


## Painel do usuário
ui <- fluidPage(
    # layout
    theme = shinytheme("cosmo"),
    navbarPage(
        title = "COVID19 no Brasil",
        tabPanel("Mapa por municípios",
                 div(class="outer",
                     # Exibir o mapa
                     leafletOutput("mapa", width="100%", height=550) %>% withSpinner(color="black"),
                     # Textos
                     tagList(tags$b("FONTE:"),
                             "Secretarias Estaduais de Saúde. Compilado por: ",
                             tags$a(href="https://brasil.io/dataset/covid19/caso_full/", 
                                    "brasil.IO",
                                    target="_blank"), 
                             "<Atualizado em: ",
                             atualizado,
                             ">",
                             tags$br(),
                             "Mapa elaborado por ",
                             tags$a(href="https://github.com/diaspv/covid_municipios_br/blob/master/app.R", 
                                    "Patrick Dias"))
                 )
        )
    )
)


## Código do servidor
server <- function(input, output) {
# Elaboração do mapa
    # Paleta de cores
    pal1 <- colorQuantile(
        palette = "YlGnBu", 
        domain = dados$relativos,
        n = 4)
    pal2 <- colorQuantile(
        palette = "YlGnBu",
        domain = dados$absolutos,
        n = 4)
    # Renderizar o mapa 
    output$mapa <- renderLeaflet({
        leaflet(dados) %>% 
            addTiles(attribution = "Brasil.io") %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addCircles(lng = ~long, 
                       lat = ~lat, 
                       weight = 1,
                       radius = ~relativos*50, 
                       label = ~city,
                       popup = ~popup,
                       color = "black",
                       fillColor = ~pal1(relativos),
                       fillOpacity = 0.5, 
                       group = "Casos p/ 100mil hab") %>% 
            addCircles(lng = ~long, 
                       lat = ~lat, 
                       weight = 1,
                       radius = ~absolutos*5, 
                       label = ~city,
                       popup = ~popup,
                       color = "black",
                       fillColor = ~pal2(absolutos),
                       fillOpacity = 0.5, 
                       group = "Nº Casos Confirmados") %>%
            addCircles(lng = ~long, 
                       lat = ~lat, 
                       weight = 1,
                       radius = ~letalidade*10000, 
                       label = ~city,
                       popup = ~popup,
                       color = "black",
                       fillColor = "gray",
                       fillOpacity = 0.5, 
                       group = "Taxa de Letalidade") %>% 
            addCircles(lng = ~long, 
                       lat = ~lat, 
                       weight = 1,
                       radius = ~obitos*50, 
                       label = ~city,
                       popup = ~popup,
                       color = "black",
                       fillColor = "gray",
                       fillOpacity = 0.5, 
                       group = "Nº Óbitos Confirmados") %>%
            addLayersControl(
                position = "bottomright",
                overlayGroups = c("Nº Casos Confirmados", "Casos p/ 100mil hab", "Nº Óbitos Confirmados", "Taxa de Letalidade"),
                options = layersControlOptions(collapsed = FALSE)) %>% 
            hideGroup(c("Casos p/ 100mil hab", "Taxa de Letalidade", "Nº Óbitos Confirmados")) %>% 
            addLegend(pal = pal1,
                      values  = dados$relativos,
                      title = "Quartis")
        })
}

shinyApp(ui = ui, server = server)
