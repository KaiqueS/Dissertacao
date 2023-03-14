#replicar ferraz e finan, mas com dados de brollo

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

setwd("G:/Trabalho/Dissertacao/Datasets")

# Load Brollo

base_brollo <- as.data.frame( read.dta("Brollo_Dataset_Original.dta")  )

# Load TSE

base_tse <- read.csv( "TSE/tse_combinado_16-20.csv", sep = "," )

# Load CENSO 2010

url_ftp_censo_2010 <- "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_do_Universo/xls/Brasil/"

links_dos_bancos <- as.data.frame( read_html( url_ftp_censo_2010 ) %>%
                                   html_nodes( "a" ) %>%
                                   html_attr( "href" ) )

colnames( links_dos_bancos ) <- "Links"

filenames <- as.data.frame( links_dos_bancos[ grep( ".zip", links_dos_bancos$Links ), ] )

for(  correct_name in filenames ){
  
  correct_name <- paste( "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_do_Universo/xls/Brasil/", correct_name, sep = '' )
}

correct_url <- as.data.frame( correct_name )

for( url in correct_url$correct_name ){
  
  download.file( url, basename(url) )
}

files_to_unzip <- list.files( pattern = "*.zip", full.names = TRUE )

ldply( .data = files_to_unzip, .fun = unzip )

ibge_bdd <- read.csv( "IBGE/ibge_basedosdados.csv", sep = "," )

# Load MUNIC 2018

base_munic <- as.data.frame( read.csv2("Base_MUNIC_Coms_2019.csv") )
