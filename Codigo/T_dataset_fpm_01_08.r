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

setwd("G:/Trabalho/Dissertacao/Datasets/TesouroN/")
setwd( "/media/kaique/Arquivos/Trabalho/Dissertacao/Datasets/TesouroN/" )

fpm <- read.csv( "fpm_2001_2008.csv", sep = ";", fileEncoding = "latin1" )

length( unique( fpm$Município ) )

colnames( fpm )[ colnames( fpm ) == "Código.SIAFI" ] = "id_city_siafi"
colnames( fpm )[ colnames( fpm ) == "Código.IBGE" ] = "id_city_ibge"
colnames( fpm )[ colnames( fpm ) == "Ano" ] = "term"
colnames( fpm )[ colnames( fpm ) == "Município" ] = "municipio"
fpm <- fpm[ !duplicated( fpm$id_city_ibge ), ]

fpm <- merge( fpm, banco_ibge_populacao[ c( "id_city_ibge", "sigla_uf"  ) ], by = "id_city_ibge" )

match_fpm_ibge <- match( fpm$id_city_ibge, banco_ibge_populacao$id_city_ibge )

fpm$uf <- banco_ibge_populacao[ match_fpm_ibge, c( "sigla_uf" ) ]
