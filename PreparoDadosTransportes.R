library(prophet)
library(dplyr)
library(htmltools)
library(tidyr)
library(readr)
library(WriteXLS)

argumentos <- commandArgs(trailingOnly = TRUE)

#download.file('http://www2.susep.gov.br/redarq.asp?arq=BaseCompleta%2ezip', 
             # destfile = 'BaseCompleta.zip')

#unzip('BaseCompleta.zip', files = c('SES_UF2.csv', 'Ses_cias.csv'))

dataset <- read.csv('SES_UF2.csv', sep = ";", dec = ",")
dataset <- dataset[order(dataset$damesano, decreasing = FALSE), ]

dataset$UF <- toupper(dataset$UF)
dataset$damesano <- as.Date(paste0(substring(dataset$damesano, 0, 4), "-", 
                                   substring(dataset$damesano, 5, 7), "-01"))

companhias <- read_delim("Ses_cias.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Coenti = col_integer()), 
                         locale = locale(encoding = "ISO-8859-1"), 
                         trim_ws = TRUE)

dataset_transportes <- dataset %>% 
  group_by(damesano, coenti, UF) %>%
  summarise(PremiosDiretos = sum(premio_dir), 
            SinistrosDiretos = sum(sin_dir)) %>%
  mutate(Sinistralidade = SinistrosDiretos / PremiosDiretos)

dataset_transportes <- left_join(dataset_transportes, companhias[, c(1:2)], by = c('coenti' = 'Coenti'))

lista_seguradoras <- data.frame(table(dataset_transportes$Noenti))
lista_seguradoras <- lista_seguradoras$fr

lista_UF <- as.factor(dataset_transportes$UF)

for (seguradora in lista_seguradoras) {
    
    for (uf in lista_UF) {
    
    if (length(dataset_transportes$y[dataset_transportes$Noenti == seguradora &
                                   dataset_transportes$ramos == ramo_transporte &
                                   dataset_transportes$UF == uf]) < 2) {
                                     
      next
      
    } else {  
    
    dataset_treino <- dataset_transportes[dataset_transportes$Noenti == seguradora &
                                          dataset_transportes$ramos == ramo_transporte &
                                          dataset_transportes$UF == uf, c("ds", "y")]  
    
    modelo <- prophet(dataset_treino, 
                      growth = "linear", 
                      yearly.seasonality = TRUE, 
                      weekly.seasonality = FALSE, 
                      daily.seasonality = FALSE, 
                      seasonality.mode = "multiplicative")
    
    periodos_futuros <- make_future_dataframe(modelo, 48, 
                                              freq = "month", include_history = TRUE)
    
    projecao <- predict(modelo, periodos_futuros)
    
    plot <- dyplot.prophet(modelo, projecao, 
                           main = paste0("Projeção Sinistralidade - ", seguradora, " - ", ramo_transporte, " - ", uf), 
                           xlab = "Linha Temporal", 
                           ylab = "Sinistralidade")
    
    save_html(plot, paste0("PROJECOES/Projeção Sinistralidade - ", seguradora, " - ", ramo_transporte, " - ", uf, ".html"), background = "antiquewhite")
      
  }
    }
  }
