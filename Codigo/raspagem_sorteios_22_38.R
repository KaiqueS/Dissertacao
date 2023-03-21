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
  
  assign( num_sorteio, data )
}

#teste <- lapply( links_sorteios, as.data.frame( function( x ) { read_html( x ) %>% html_nodes( xpath = "//p" ) %>% html_text( ) } ) )
teste <- lapply( links_sorteios, function( x ) { read_html( x ) %>% html_nodes( xpath = "//p" ) %>% html_text( ) } )
teste <- lapply( teste, grepl( "1º|31º", colnames ) )

teste[[1]]

heads <- as.data.frame(teste %>% html_nodes( xpath = "//p" ) %>%
         html_text( ))
