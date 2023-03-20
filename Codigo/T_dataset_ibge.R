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
library( basedosdados )
library( DescTools )
library( plyr )
library( haven )

setwd( "G:/Trabalho/Dissertacao/Datasets/IBGE/" )
setwd( "G:/Trabalho/Dissertacao/Datasets/IBGE/ibge_source/" )
setwd( "G:/Trabalho/Dissertacao/Datasets/IBGE/ibge_pop_censo_source/" )

# IBGE - Censo 2000 Source #

url_ftp_censo_2000 <- "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2000/Dados_do_Universo/Municipios/"

links_dos_bancos <- as.data.frame( read_html( url_ftp_censo_2000 ) %>%
                                   html_nodes( "a" ) %>%
                                   html_attr( "href" ) )

links_dos_bancos <- as.data.frame( links_dos_bancos[ -c( 1, 2, 3, 4, 5 ), ] )

colnames( links_dos_bancos ) <- "Links"

filenames <- as.data.frame( links_dos_bancos[ grep( ".zip", links_dos_bancos$Links ), ] )

for(  correct_name in filenames ){
  
  correct_name <- paste( "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2000/Dados_do_Universo/Municipios/", correct_name, sep = '' )
}

correct_url <- as.data.frame( correct_name )

for( url in correct_url$correct_name ){
  
  download.file( url, basename(url) )
}

files_to_unzip <- list.files( pattern = "*.zip", full.names = FALSE )

ldply( .data = files_to_unzip, .fun = unzip )

file.rename( list.files( pattern = "uni   " ), str_replace( list.files( pattern = "uni   " ), pattern = "uni   ", "" ) )

ibge_files <- list.files( pattern = "*.xls" )

for( i in seq_along( ibge_files ) ){ 

  corrected_name <- tools::file_path_sans_ext( ibge_files[ i ] )
  assign( corrected_name, read_xls( ibge_files[ i ] ) )
}

list_datasets <- lapply( ibge_files, as.data.frame( read_xls ) )
list_datasets <- lapply( list_datasets, as.data.frame( function( x ) x[ -c( 1:6 ), ] ) )
list_datasets <- lapply( list_datasets, as.data.frame( function( x ) x[ , -c( 3:10 ) ] ) )
list_datasets <- lapply( list_datasets, setNames, c( "municipio", "populacao", "id_city_ibge" ) )

ibge_populacao_censo <- bind_rows( list_datasets )
ibge_populacao_censo$populacao <- as.numeric( ibge_populacao_censo$populacao )
ibge_populacao_censo$id_city_ibge <- as.numeric( ibge_populacao_censo$id_city_ibge )

write.csv( ibge_populacao_censo, "ibge_populacao_censo_2000.csv" )

banco_ibge_source <- read.csv( "ibge_populacao_censo_2000.csv", sep = "," )

# IBGE - População Source #

# ATENÇÃO: os anos de 2000 e 2004 apresentam municípios que foram modificados nos anos subsequentes( 2001 e 2005 )
lista <- list.files( pattern = "*.xls" )

for( i in lista ){
  
  corrected_name <- tools::file_path_sans_ext( i )
  
  assign( corrected_name, read_xls( i ) )
}


# Corrigindo irregularidades de formatação nos datasets: removendo linhas e colunas vazias
#                                                        renomeando colunas para padronização

# IBGE Primeiro Ciclo -> 2000, 2001, 2002
ibge_populacao_2000.xls <- ibge_populacao_2000.xls[ -c( 1, 2 ), ]
ibge_populacao_2000.xls <- ibge_populacao_2000.xls[ , -c( 6 ) ]
colnames( ibge_populacao_2000.xls ) <- c( "uf", "id_uf", "id_municipio", "municipio", "populacao" )
ibge_populacao_2000.xls$ano <- 2000
ibge_populacao_2000.xls <- ibge_populacao_2000.xls[ -c( 5508 : 5511 ), ]

ibge_populacao_2001.xls <- ibge_populacao_2001.xls[ -c( 1, 2, 3, 4 ), ]
colnames( ibge_populacao_2001.xls ) <- c( "uf", "id_uf", "id_municipio", "municipio", "populacao" )
ibge_populacao_2001.xls$ano <- 2001

ibge_populacao_2002.xls <- ibge_populacao_2002.xls[ -c( 1, 2, 3, 4, 5 ), ]
colnames( ibge_populacao_2002.xls ) <- c( "uf", "id_uf", "id_municipio", "municipio", "populacao" )
ibge_populacao_2002.xls$ano <- 2002
ibge_populacao_2002.xls <- ibge_populacao_2002.xls[ -c( 5561 ), ]

# IBGE Segundo Ciclo -> 2004, 2005, 2006
ibge_populacao_2004.xls <- ibge_populacao_2004.xls[ -c( 1, 2, 3, 4 ), ]
colnames( ibge_populacao_2004.xls ) <- c( "uf", "id_uf", "id_municipio", "municipio", "populacao" )
ibge_populacao_2004.xls$ano <- 2004
ibge_populacao_2004.xls <- ibge_populacao_2004.xls[ -c( 5565 : 5570 ), ]

ibge_populacao_2005.xls <- ibge_populacao_2005.xls[ -c( 1, 2, 3, 4 ), ]
colnames( ibge_populacao_2005.xls ) <- c( "uf", "id_uf", "id_municipio", "municipio", "populacao" )
ibge_populacao_2005.xls$ano <- 2005
ibge_populacao_2005.xls <- ibge_populacao_2005.xls[ -c( 5565 ), ]

ibge_populacao_2006.xls <- ibge_populacao_2006.xls[ -c( 1, 2, 3, 4 ), ]
colnames( ibge_populacao_2006.xls ) <- c( "uf", "id_uf", "id_municipio", "municipio", "populacao" )
ibge_populacao_2006.xls$ano <- 2006
ibge_populacao_2006.xls <- ibge_populacao_2006.xls[ -c( 5565 : 5567 ), ]

# Juntar bancos do primeiro ciclo

5507 + 5560 + 5560

lista_ciclo1 <- list( ibge_populacao_2000.xls, ibge_populacao_2001.xls, ibge_populacao_2002.xls )

ibge_0104 <- Reduce( function( x, y ) merge( x, y, all = TRUE ), lista_ciclo1 )
ibge_0104$populacao <- as.numeric( ibge_0104$populacao )
ibge_0104$log_natural_pop <- log( ibge_0104$populacao )

write.csv( ibge_0104, "ibge_0104.csv" )

ibge_0104 <- read.csv( "ibge_0104.csv", sep = "," )
ibge_0104 <- ibge_0104[ , -c( 1 ) ]

ibge_0104 <- left_join( ibge_0104, fpm[ , c( "municipio", "id_city_ibge", "uf" ) ], by = c( "municipio", "uf" ) )

write.csv( ibge_0104, "ibge_0104.csv" )

ibge_0104 <- read.csv( "ibge_0104.csv", sep = "," )

sum( is.na( ibge_0104$id_city_ibge ) )

# Juntar bancos do segundo ciclo

5564 * 3

lista_ciclo2 <- list( ibge_populacao_2004.xls, ibge_populacao_2005.xls, ibge_populacao_2006.xls )

ibge_0508 <- Reduce( function( x, y ) merge( x, y, all = TRUE ), lista_ciclo2 )
ibge_0508$populacao <- as.numeric( ibge_0508$populacao )
ibge_0508$log_natural_pop <- log( ibge_0508$populacao )

write.csv( ibge_0508, "ibge_0508.csv" )

ibge_0508 <- read.csv( "ibge_0508.csv", sep = "," )
ibge_0508 <- ibge_0508[ , -c( 1 ) ]

ibge_0508 <- left_join( ibge_0508, fpm[ , c( "municipio", "id_city_ibge", "uf" ) ], by = c( "municipio", "uf" ) )

write.csv( ibge_0508, "ibge_0508.csv" )

ibge_0508 <- read.csv( "ibge_0508.csv", sep = "," )

# Juntar bancos de ambos os ciclos
  
ibge_0108 <- merge( ibge_0104, ibge_0508, all = TRUE )

write.csv( ibge_0108, "ibge_0108.csv" )

ibge_0108 <- read.csv( "ibge_0108.csv", sep = "," )

# IBGE BASE DOS DADOS #

#setwd( dirname( rstudioapi::getActiveDocumentContext()$path ) )
#setwd( "/media/kaique/Arquivos/Trabalho/Dissertacao/Datasets/IBGE/" )

#teste <- read.csv( "ibge_basedosdados.csv", sep = "," )

set_billing_id( "graphite-argon-368423" )

query <- bdplyr( "graphite-argon-368423.ibge_basedosdados.ibge" )

df <- bd_collect( query )

write.csv( df, "ibge_basedosdados.csv" )

banco_ibge_censo <- read.csv( "ibge_basedosdados.csv", sep = "," )

query <- bdplyr( "graphite-argon-368423.ibge_populacao.ibge_populacao" )

df <- bd_collect( query )

write.csv( df, "ibge_populacao_bdd.csv" )

banco_ibge_populacao <- read.csv( "ibge_populacao_bdd.csv", sep = "," )
banco_ibge_populacao <- subset( banco_ibge_populacao, ano == 2000 )
colnames( banco_ibge_populacao )[ colnames( banco_ibge_populacao ) == "id_municipio" ] <- "id_city_ibge"
banco_ibge_populacao$log_natural_pop <- log( banco_ibge_populacao$populacao )
banco_ibge_populacao <- banco_ibge_populacao[ , -c( 1 ) ]

# TODO: média populacional para anos 2001 = 2000, 2001, 2002
#                                    2005 = 2004, 2005, 2006

# Agora, limpar o banco da seguinte forma: o objetivo é deixar o banco com um
# número de observações igual ao do banco da Brollo. Então, vou realizar uma
# amostragem estratificada. Talvez eu faça duas amostragens: uma respeitando
# as proporções com que os municípios aparecem no banco da Brollo, e outra
# respeitando as proporções dos municípios observadas no banco do TSE.