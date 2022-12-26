library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(prophet)
library(readr)
library(dygraphs)

#Source do Prophet e lista de companhias
source('ProphetPersonalizado.R')
lista_companhias <- read_rds('lista_companhias.rds')

cabecalho <- dashboardHeader(title = "Dashboard de Sinistros de Transportes", 
                             titleWidth = "400")

#Lateral
lateral <- dashboardSidebar(sidebarMenu(uiOutput("selecao_companhia"),
                                        uiOutput("selecao_UF"),
                                        uiOutput("selecao_ramo"),
                                        uiOutput("anos_futuros")), width = "250")

#Corpo
corpo <- dashboardBody(tags$head(
                       tags$style(HTML(".shiny-output-error-validation {color: black; 
                                                                        font-size: 30px;}"))),
                       box(dygraphOutput("plot_projecao", height = "700"),
                           width = "100%", 
                           title = "Projeção de Sinistros (em escala logarítmica)",
                           status = "primary", 
                           solidHeader = TRUE))

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
                                                  min = 1, max = 50, value = 5, step = 1, orientation = "vertical", 
                                                  direction = "rtl", height = 500, 
                                                  format = wNumbFormat(decimals = 0)))
  
  projecao_plot <- reactive({
    
   validate(
     need(file.exists(paste0(getwd(), "/modelos/", lista_companhias$coenti[lista_companhias$Noenti == input$companhia_selecionada], 
                             "-", 
                             input$UF_selecionada, 
                             "-", 
                             input$ramo_selecionado, 
                             ".rds")),
          message = "Não há projeção disponível para essa combinação de parâmetros, em razão da falta de dados históricos. Tente outra companhia, UF e/ou ramo. "))
    
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
                                                  input$UF_selecionada, " - ", 
                                                  input$ramo_selecionado),
                                    xlab = "Linha Temporal",
                                    ylab = "Escala Logarítmica de Sinistralidade")
    
    return(plot_projecao)
    
  })
  
  output$plot_projecao <- renderDygraph(projecao_plot())
  
}

shinyApp(ui, server)