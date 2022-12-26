library(prophet)
library(dplyr)
library(htmltools)
library(tidyr)
library(readr)

options(timeout = 300)

argumentos <- commandArgs(trailingOnly = TRUE)

ano_escolhido <- as.integer(argumentos[1])

#Baixa e descompacta o dataset
download.file('http://www2.susep.gov.br/redarq.asp?arq=BaseCompleta%2ezip', 
              destfile = 'BaseCompleta.zip')

unzip('BaseCompleta.zip', files = c('SES_UF2.csv', 'Ses_cias.csv'))

dataset <- read.csv('SES_UF2.csv', sep = ";", dec = ",")

#Subset dos dados a partir do ano escolhido no argumento
dataset <- dataset[dataset$damesano >= paste0(ano_escolhido, "01"), ]

#Códigos dos ramos escolhidos
ramos_transporte <- c("621", "622")

dataset$UF <- toupper(dataset$UF)

#Criação do dataset compilado com a sinistralidade por ramo, companhia, mês e UF
#Conversão para logaritmo de base 10 para colocar diferentes montantes de indenização na mesma base
dataset_transportes <- dataset %>%
  mutate(ds = paste0(substring(dataset$damesano, 0, 4), "-", substring(dataset$damesano, 5, 7), "-01")) %>%
  filter(ramos %in% ramos_transporte) %>%
  filter(premio_dir > 0) %>%
  group_by(ds, coenti, ramos, UF) %>%
  summarise(SinistrosDiretos = sum(sin_dir)) %>%
  filter(SinistrosDiretos >= 0) %>%
  mutate(y = log10(SinistrosDiretos + 1))

#Listas de UFs e companhias
lista_UF <- unique(dataset_transportes$UF)
lista_UF <- lista_UF[order(lista_UF)]

lista_seguradoras <- unique(dataset_transportes$coenti)
lista_seguradoras <- lista_seguradoras[order(lista_seguradoras)]

#Loop para criação de cada modelo
for (seguradora in lista_seguradoras) {
  
  for (uf in lista_UF) {
  
    for (ramo in ramos_transporte) {
      
      #Evita a criação de modelos para companhias com poucos dados, pois são muito imprecisos. 
      #Considera apenas as combinações com pelo menos 12 dados de sinistralidade
      if (length(dataset_transportes$coenti[dataset_transportes$coenti == seguradora &
                                     dataset_transportes$ramos == ramo &
                                     dataset_transportes$UF == uf]) < 12) {
        
        next
        
      } else {
    
        #Subset do loop, treino do modelo e salva o objeto em RDS na pasta
    dataset_treino <- dataset_transportes[dataset_transportes$coenti == seguradora &
                                          dataset_transportes$ramos == ramo &
                                          dataset_transportes$UF == uf, c("ds", "y")]  
    
    modelo <- prophet(dataset_treino, 
                      growth = "linear", 
                      yearly.seasonality = TRUE, 
                      weekly.seasonality = FALSE, 
                      daily.seasonality = FALSE, 
                      seasonality.mode = "multiplicative", uncertainty.samples = FALSE)
      
    write_rds(modelo, paste0("modelos/", seguradora, "-", uf, "-", ramo , ".rds"))
  }
}
  }
}
