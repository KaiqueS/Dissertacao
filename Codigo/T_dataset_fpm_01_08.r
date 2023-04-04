library("readODS")
library("tidyverse")
library( dplyr )
library("readODS")
library("readxl")
library("foreign")
library( "httr" )
library( "xml2" )
library( "rvest" )
library( ff )
#library( ffbase )
library( plyr )
library( DescTools )
library( readr )

setwd("G:/Trabalho/Dissertacao/Datasets/TesouroN/")
setwd( "/media/kaique/Arquivos/Trabalho/Dissertacao/Datasets/TesouroN/" )

fpm <- read.csv( "fpm_2001_2008.csv", sep = ";", fileEncoding = "latin1" )

length( unique( fpm$Município ) )

colnames( fpm )[ colnames( fpm ) == "Código.SIAFI" ] = "id_city_siafi"
colnames( fpm )[ colnames( fpm ) == "Código.IBGE" ] = "id_city_ibge"
colnames( fpm )[ colnames( fpm ) == "Ano" ] = "term"
colnames( fpm )[ colnames( fpm ) == "Município" ] = "municipio"

teste_fpm <- as.data.frame( gsub( "[^0-9,]", "", fpm$Valor.Consolidado ) )
teste_fpm$`gsub("[^0-9,]", "", fpm$Valor.Consolidado)` <- parse_number( teste_fpm$`gsub("[^0-9,]", "", fpm$Valor.Consolidado)`, locale = readr::locale( decimal_mark = "," ) )

#fpm$Valor.Consolidado <- as.numeric( gsub( "[^0-9]", "", fpm$Valor.Consolidado, fixed = TRUE ) )
fpm$Valor.Consolidado <- parse_number( fpm$Valor.Consolidado, locale = readr::locale( decimal_mark = "," ) )
fpm$Valor.Consolidado <- sub( "[^0-9]", ".", fpm$Valor.Consolidado )
fpm$Valor.Consolidado <- as.numeric( fpm$Valor.Consolidado )

fpm$Valor.Consolidado <- gsub( "[^0-9]", ".", fpm$Valor.Consolidado )
fpm$Valor.Consolidado <- as.numeric( fpm$Valor.Consolidado )
fpm$Valor.Consolidado <- fpm$Valor.Consolidado / 100

options( digits = 10 )

fpm[ 1, "Valor.Consolidado" ]

fpm <- fpm[ !duplicated( fpm$id_city_ibge ), ]

fpm <- merge( fpm, banco_ibge_populacao[ c( "id_city_ibge", "sigla_uf"  ) ], by = "id_city_ibge" )

match_fpm_ibge <- match( fpm$id_city_ibge, banco_ibge_populacao$id_city_ibge )

fpm$uf <- banco_ibge_populacao[ match_fpm_ibge, c( "sigla_uf" ) ]
