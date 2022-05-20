library(prophet)
library(dplyr)
library(htmltools)
library(tidyr)
library(WriteXLS)

argumentos <- commandArgs(trailingOnly = TRUE)

download.file('http://www2.susep.gov.br/redarq.asp?arq=BaseCompleta%2ezip', 
              destfile = 'BaseCompleta.zip')

unzip('BaseCompleta.zip', files = c('SES_UF2.csv', 'Ses_cias.csv'))

dataset <- read.csv('SES_UF2.csv', sep = ";", dec = ",")
companhias <- read.csv('Ses_cias.csv', sep = ";", dec = ",")
dataset$ramos <- as.integer(dataset$ramos)

ramos_transportes <- c("621", "622")

dataset_transportes <- dataset[dataset$ramos %in% ramos_transportes, ]

#Remoção das companhias que não trabalham com transporte
dataset_transportes <- dataset_transportes[dataset_transportes$sin_dir > 0, ]

#Join com a tabela das companhias
dataset_transportes <- left_join(dataset_transportes, companhias[, c(1:2)], by = c('coenti' = 'Coenti'), )

#
dataset_transportes <- dataset_transportes[, c(1:5, 7, 12)]

#Índice de sinistralidade
dataset_transportes$sinistralidade <- round(as.double(dataset_transportes$sin_dir / dataset_transportes$premio_dir), digits = 2)
