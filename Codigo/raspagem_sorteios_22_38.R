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
library( magrittr )
library( tm )
library( stringi )

setwd( "G:/Trabalho/Dissertacao/Datasets/Sorteios/" )

# RASPAR OS SITES DA CGU PARA PEGAR CIDADES SORTEADAS NAS LOTERIAS 22-38

link_cgu_loterias <- "https://www.gov.br/cgu/pt-br/assuntos/auditoria-e-fiscalizacao/programa-de-fiscalizacao-em-entes-federativos/edicoes-anteriores/municipios"

links_sorteios <- read_html( link_cgu_loterias ) %>% 
                             html_nodes( "a" ) %>%
                             html_attr( "href" ) %>%
                             str_subset( "https://www.gov.br/cgu/pt-br/assuntos/auditoria-e-fiscalizacao/programa-de-fiscalizacao-em-entes-federativos/edicoes-anteriores/municipios/" )

paginas_sorteios <- lapply( links_sorteios, read_html )

#teste <- read_html( links_sorteios[ 1 ] )
for( link in links_sorteios ){
  
  num_sorteio <- gsub( "https://www.gov.br/cgu/pt-br/assuntos/auditoria-e-fiscalizacao/programa-de-fiscalizacao-em-entes-federativos/edicoes-anteriores/municipios/" , " " , link )
  
  #assign( num_sorteio, link )
  
  #assign( num_sorteio, as.data.frame( as.data.frame( read_html( link ) %>% html_nodes( xpath = "//p" ) %>% html_text( ) )[ -c( 3:8 ), ] ) )
  #assign( num_sorteio, as.data.frame( as.data.frame( read_html( link ) %>% html_nodes( xpath = "//p" ) %>% html_text( ) )[ grepl( "1º", colnames( num_sorteio ) ), ] ) )
  ##assign( num_sorteio, as.data.frame( read_html( link ) %>% html_nodes( xpath = "//p" ) %>% html_text( ) ) )
  
  data <- as.data.frame( read_html( link ) %>% html_nodes( xpath = "//p" ) %>% html_text( ) )
  data <- as.data.frame( data[ grepl( "1º|31º", data$`read_html(link) %>% html_nodes(xpath = "//p") %>% html_text()` ), ] )
  colnames( data ) <- "valores"
  
  cities_to_rows <- str_replace_all( data$valores, "\\d(.*?)\\d[[º]]", "," )
  cities_to_rows <- as.data.frame( cities_to_rows )
  cities_to_rows <- cities_to_rows %>% 
                    mutate( cities_to_rows = strsplit( as.String( cities_to_rows ), "," ) ) %>%
                    unnest( cities_to_rows )
  cities_to_rows <- cities_to_rows[ !apply( cities_to_rows == "", 1, all ), ]
  cities_to_rows$cities_to_rows <- substring( cities_to_rows$cities_to_rows, 3 )
  colnames( cities_to_rows )[ colnames( cities_to_rows ) == "cities_to_rows" ] <- "municipio"
  cities_to_rows$uf <- str_extract( cities_to_rows$municipio, '\\b\\w+$' )
  
  #assign( num_sorteio, data )
  assign( num_sorteio, cities_to_rows )
}

#data <- data %>% mutate( municipios = str_extract( valores, '\\D+(?=\\d+)' ) )
data <- data %>% str_replace_all( data$valores, "\\d(.*?)\\d[[º]]", "," ) %>% strsplit( as.String( valores ), "," ) %>% unnest( valores )
#teste <- str_replace_all( data$valores, "[[º]]", "" )
teste <- str_replace_all( data$valores, "\\d(.*?)\\d[[º]]", "," )
teste <- as.data.frame( teste )
teste <- teste %>% 
         mutate( teste = strsplit( as.String( teste ), "," ) ) %>%
         unnest( teste )
teste <- teste[ !apply( teste == "", 1, all ), ]
teste$teste <- substring( teste$teste, 3 )
teste$estado <- str_extract( teste$teste, '\\b\\w+$' )

#teste <- str_replace_all( data$valores, "[[:punct:][:digit:][:cntrl:][º]]", "" )
teste

#teste <- lapply( links_sorteios, as.data.frame( function( x ) { read_html( x ) %>% html_nodes( xpath = "//p" ) %>% html_text( ) } ) )
teste <- lapply( links_sorteios, function( x ) { read_html( x ) %>% html_nodes( xpath = "//p" ) %>% html_text( ) } )
teste <- lapply( teste, grepl( "1º|31º", colnames ) )

teste[[1]]

heads <- as.data.frame(teste %>% html_nodes( xpath = "//p" ) %>%
         html_text( ))
