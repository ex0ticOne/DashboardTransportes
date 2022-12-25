library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(prophet)
library(readr)
library(dygraphs)

source('ProphetPersonalizado.R')
lista_companhias <- read_rds('lista_companhias.rds')

cabecalho <- dashboardHeader(title = "Dashboard de Transportes", 
                             titleWidth = "350")

#Lateral
lateral <- dashboardSidebar(sidebarMenu(uiOutput("selecao_companhia"),
                                        uiOutput("selecao_UF"),
                                        uiOutput("selecao_ramo"),
                                        uiOutput("anos_futuros")), width = 200)

#Corpo
corpo <- dashboardBody(box(dygraphOutput("plot", height = "700"), 
                           width = "100%",  
                           title = "TESTE"))

ui <- dashboardPage(cabecalho, lateral, corpo, skin = "black", 
                    title = "Dashboard de Transportes")

server <- function(input, output) {
  
  output$selecao_companhia <- renderUI(selectInput("companhia_selecionada", 
                                                   label = "Selecione a companhia", choices = lista_companhias$Noenti))
  
  output$selecao_UF <- renderUI(selectInput("UF_selecionada", label = "Selecione a UF", choices = c("AC", "AL", "AM", "BA", "CE",
                                                                                                    "DF", "ES", "GO", "MA", "MG", 
                                                                                                    "MS", "MT", "PA", "PB", "PE",
                                                                                                    "PI", "PR", "RJ", "RN", "RO",
                                                                                                    "RR","RS", "SC", "SE", "SP", 
                                                                                                    "TO")))
  
  output$selecao_ramo <- renderUI(selectInput("ramo_selecionado", label = "Selecione o ramo", choices = c("Transporte Nacional" = 621, 
                                                                                                          "Transporte Internacional" = 622)))
  
  output$anos_futuros <- renderUI(noUiSliderInput("anos_selecionado", label = "Selecione os anos futuros", 
                                                  min = 1, max = 30, value = 10, step = 1, orientation = "vertical", 
                                                  direction = "rtl", height = 300))
  
  projecao_sinistralidade <- reactive({
    
    modelo <- read_rds(paste0(getwd(), "/modelos/", lista_companhias$coenti[lista_companhias$Noenti == input$companhia_selecionada], 
                              "-", 
                              input$UF_selecionada, 
                              "-", 
                              input$ramo_selecionado, 
                              ".rds"))
    
    periodo_futuro <- make_future_dataframe(modelo, periods = input$anos_selecionado * 12, freq = "month",
                                            include_history = TRUE)
    
    projecao <- predict(modelo, periodo_futuro)
      
    plot_projecao <- dyplot.prophet(modelo, projecao, 
                                    main = paste0(lista_companhias$Noenti[lista_companhias$Noenti == input$companhia_selecionada], " - ", 
                                                  input$UF_selecionada),
                                    xlab = "Linha Temporal",
                                    ylab = "Escala de Sinistralidade")
    
    return(plot_projecao)
    
  })
  
  output$plot <- renderDygraph(projecao_sinistralidade())
  
}

shinyApp(ui, server)