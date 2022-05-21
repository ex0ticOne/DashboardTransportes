library(prophet)
library(dplyr)
library(htmltools)
library(tidyr)
library(readr)
library(WriteXLS)

argumentos <- commandArgs(trailingOnly = TRUE)

ano_escolhido <- as.integer(argumentos[1])
ramo_escolhido <- case_when(argumentos[2] == 1 ~ 621,
                            argumentos[2] == 2 ~ 622)

seguradoras_escolhidas <- read.csv('seguradoras_escolhidas.csv')
companhias_escolhida <- seguradoras_escolhidas$Noenti[seguradoras_escolhidas$escolhida == 1]

#download.file('http://www2.susep.gov.br/redarq.asp?arq=BaseCompleta%2ezip', 
         #     destfile = 'BaseCompleta.zip')

unzip('BaseCompleta.zip', files = c('SES_UF2.csv', 'Ses_cias.csv'))

dataset <- read.csv('SES_UF2.csv', sep = ";", dec = ",")
dataset <- dataset[order(dataset$damesano, decreasing = FALSE), ]

dataset <- dataset[dataset$damesano >= paste0(ano_escolhido, "01"), ]

ramos_transporte <- c("621", "622")

dataset$UF <- toupper(dataset$UF)

companhias <- read_delim("Ses_cias.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Coenti = col_integer()), 
                         locale = locale(encoding = "ISO-8859-1"), 
                         trim_ws = TRUE)

dataset_transportes <- dataset %>%
  mutate(ds = paste0(substring(dataset$damesano, 0, 4), "-", substring(dataset$damesano, 5, 7), "-01")) %>%
  filter(ramos %in% ramos_transporte) %>%
  filter(premio_dir > 0) %>%
  group_by(ds, coenti, ramos, UF) %>%
  summarise(PremiosDiretos = sum(premio_dir), 
            SinistrosDiretos = sum(sin_dir)) %>%
  mutate(y = round(SinistrosDiretos / PremiosDiretos, digits = 2)) %>%
  filter(y > 0) %>%
  left_join(y = companhias[, c(1:2)], by = c('coenti' = 'Coenti'))

lista_seguradoras <- data.frame(table(dataset_transportes$Noenti))
lista_seguradoras <- as.factor(lista_seguradoras$Var1[lista_seguradoras$Freq != ""])

lista_UF <- as.factor(dataset_transportes$UF)

for (seguradora in companhias_escolhida) {
  
    for (uf in lista_UF) {
    
    if (length(dataset_transportes$y[dataset_transportes$Noenti == seguradora &
                                   dataset_transportes$ramos == ramo_escolhido &
                                   dataset_transportes$UF == uf]) < 2) {
                                     
      next
      
    } else {  
    
    dataset_treino <- dataset_transportes[dataset_transportes$Noenti == seguradora &
                                          dataset_transportes$ramos == ramo_escolhido &
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
                           main = paste0("Projeção Sinistralidade - ", seguradora, " - ", ramo_escolhido, " - ", uf), 
                           xlab = "Linha Temporal", 
                           ylab = "Sinistralidade")
    
    save_html(plot, paste0("PROJECOES/Projeção Sinistralidade - ", seguradora, " - ", ramo_escolhido, " - ", uf, ".html"), 
              background = "antiquewhite")
      
  }
    }
  }
