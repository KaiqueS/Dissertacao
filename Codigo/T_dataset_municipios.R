library("readODS")
library("tidyverse")
library("readxl")
library("foreign")
library( "httr" )
library( "xml2" )
library( "rvest" )
library( ff )
#library( ffbase )
library( dplyr )
library( plyr )
library( ggplot2 )
library( basedosdados )
library( DescTools )
library( purrr )
library( stringi )

setwd( "G:/Trabalho/Dissertacao/Datasets/Dir_mun_bdd" )

set_billing_id( "graphite-argon-368423" )

municipios_query <- bdplyr( "graphite-argon-368423.diretorio_mun_br_bdd.diretorio_municipios" )

df_municipios <- bd_collect( municipios_query )

write.csv( df_municipios, "municipios_bdd.csv" )

municipios <- read.csv( "municipios_bdd.csv" )
