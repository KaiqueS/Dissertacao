library("readxl")
library("foreign")
library( "httr" )
library( "xml2" )
library( "rvest" )
library( ff )
#library( ffbase )
library( dplyr )
library( plyr )

Sys.setlocale( "LC_ALL", "Portuguese_Brazil.1252" )

setwd("G:/Trabalho/Dissertacao/Datasets/MUNIC/")

munic_2020 <- read.csv( "MUNIC_2020.csv", encoding = "UTF-8" )
munic_2020 <- pivot_wider( data = munic_2020, id_cols = "Temas disponíveis" )
munic_2020 <- reshape( munic_2020, direction = "wide", idvar = "Temas disponíveis", timevar =   )

munic_2020$
