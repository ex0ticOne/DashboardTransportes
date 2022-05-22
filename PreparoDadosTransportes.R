library(prophet)
library(dplyr)
library(htmltools)
library(tidyr)
library(readr)
library(WriteXLS)

argumentos <- commandArgs(trailingOnly = TRUE)

#ano_escolhido <- as.integer(argumentos[1])
ano_escolhido <- 2010

companhias <- read_delim("lista_seguradoras.csv", 
                         delim = ",", escape_double = FALSE, col_types = cols(Coenti = col_integer()), 
                         locale = locale(encoding = "ISO-8859-1"), 
                         trim_ws = TRUE)

companhias_escolhidas <- as.vector(companhias$Coenti[companhias$escolhida == 1])

#download.file('http://www2.susep.gov.br/redarq.asp?arq=BaseCompleta%2ezip', 
         #     destfile = 'BaseCompleta.zip')

unzip('BaseCompleta.zip', files = c('SES_UF2.csv', 'Ses_cias.csv'))

dataset <- read.csv('SES_UF2.csv', sep = ";", dec = ",")

dataset <- dataset[dataset$damesano >= paste0(ano_escolhido, "01"), ]

ramos_transporte <- c("621", "622")

dataset$UF <- toupper(dataset$UF)

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

lista_UF <- data.frame(table(dataset_transportes$UF))
lista_UF <- as.factor(lista_UF$Var1[lista_UF$Freq != 0])

lista_seguradoras <- data.frame(table(dataset_transportes$coenti))
lista_seguradoras <- as.factor(lista_seguradoras$Var1[lista_seguradoras$Freq != 0])

for (seguradora in lista_seguradoras) {
  
  for (ramo in ramos_transporte) {
  
    for (uf in lista_UF) {
      
      if (length(dataset_transportes$Noenti[dataset_transportes$coenti == seguradora &
                                     dataset_transportes$ramos == ramo &
                                     dataset_transportes$UF == uf]) < 2) {
        
        next
        
      } else {
    
    dataset_treino <- dataset_transportes[dataset_transportes$coenti == seguradora &
                                          dataset_transportes$ramos == ramo &
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
                           main = paste0("Projeção Sinistralidade - ", companhias$Noenti[companhias$Coenti == seguradora], " - ", ramo, " - ", uf), 
                           xlab = "Linha Temporal", 
                           ylab = "Sinistralidade")
    
    save_html(plot, paste0("PROJECOES/Projeção Sinistralidade - ", companhias$Noenti[companhias$Coenti == seguradora], " - ", ramo, " - ", uf, ".html"), 
              background = "antiquewhite")
      
  }
}
  }
}
