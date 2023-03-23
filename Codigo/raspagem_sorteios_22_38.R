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

#paginas_sorteios <- lapply( links_sorteios, read_html )

#teste <- read_html( links_sorteios[ 1 ] )
for( link in links_sorteios ){
  
  num_sorteio <- gsub( "https://www.gov.br/cgu/pt-br/assuntos/auditoria-e-fiscalizacao/programa-de-fiscalizacao-em-entes-federativos/edicoes-anteriores/municipios/" , "" , link )
  
  #assign( num_sorteio, link )
  
  #assign( num_sorteio, as.data.frame( as.data.frame( read_html( link ) %>% html_nodes( xpath = "//p" ) %>% html_text( ) )[ -c( 3:8 ), ] ) )
  #assign( num_sorteio, as.data.frame( as.data.frame( read_html( link ) %>% html_nodes( xpath = "//p" ) %>% html_text( ) )[ grepl( "1º", colnames( num_sorteio ) ), ] ) )
  ##assign( num_sorteio, as.data.frame( read_html( link ) %>% html_nodes( xpath = "//p" ) %>% html_text( ) ) )
  
  data <- as.data.frame( read_html( link ) %>% html_nodes( xpath = "//p" ) %>% html_text( ) )
  data <- as.data.frame( data[ grepl( "1º|31º", data$`read_html(link) %>% html_nodes(xpath = "//p") %>% html_text()` ), ] )
  colnames( data ) <- "valores"
  
  #cities_to_rows <- str_replace_all( data$valores, "\\d(.*?)\\d[[º]]", "," ) # Erro aqui: removendo primeira cidade
  #cities_to_rows <- str_replace_all( data$valores, "\\d[º]", "," )
  cities_to_rows <- str_replace_all( data$valores, "\\d[º]|\\d [º]", "," )
  cities_to_rows <- as.data.frame( cities_to_rows )
  cities_to_rows <- cities_to_rows %>% 
                    mutate( cities_to_rows = strsplit( as.String( cities_to_rows ), "," ) ) %>%
                    unnest( cities_to_rows ) %>%
                    unique( )
  
  cities_to_rows <- cities_to_rows[ !apply( cities_to_rows == "", 1, all ), ]
  #cities_to_rows$cities_to_rows <- substring( cities_to_rows$cities_to_rows, 4 )
  cities_to_rows$cities_to_rows <- str_replace_all( cities_to_rows$cities_to_rows, "[^[:alnum:][:space:]]", "" )
  cities_to_rows$cities_to_rows <- str_replace_all( cities_to_rows$cities_to_rows, "[[:digit:]]", "" )
  colnames( cities_to_rows )[ colnames( cities_to_rows ) == "cities_to_rows" ] <- "municipio"
  #cities_to_rows$uf <- str_extract( cities_to_rows$municipio, '\\b\\w+$' )
  cities_to_rows$uf <- str_replace_all( cities_to_rows$municipio, ".*\\b([A-Z]{2})", "\\1" )
  cities_to_rows$sorteio <- gsub( "[^0-9]", "", num_sorteio )
  #cities_to_rows$municipio <- sub( "\\s+[^ ]+$", "", cities_to_rows$municipio )
  #cities_to_rows$municipio <- sub( "\\s+[^ ]+$", "", cities_to_rows$municipio )
  cities_to_rows$municipio <- gsub( "([A-Z]{2}).*", "", cities_to_rows$municipio )
  cities_to_rows$municipio <- trimws( cities_to_rows$municipio, which = "both" )
  #cities_to_rows$municipio <- gsub( "[^[:alnum:][:space:]]", "", cities_to_rows$municipio )
  
  #assign( num_sorteio, data )
  assign( num_sorteio, cities_to_rows )
}

rm( `21o-sorteio`, `40o-sorteio`, `copy_of_38-sorteio`, cities_to_rows, data, link, link_cgu_loterias, links_sorteios, num_sorteio )

# Sorteios com problemas: 22[2,42,50,], 23[42], 26[14, 52], 27[39], 28[30, 52], 29[11, 28, 31], 30[32, 34, 52], 33[26], 36[42], 38[56]
`22o-sorteio`[ c( 2, 42, 50 ), "municipio" ] <- c( "Sítio d'Abadia", "Olho d'Água do Casado", "Olho-d'Água do Borges" )
`23o-sorteio`[ c( 42 ), "municipio" ] <- c( "Igarapé-Açu" )
`26o-sorteio`[ c( 14, 52 ), "municipio" ] <- c( "Entre-Ijuís", "Apicum-Açu" )
`27o-sorteio`[ c( 39 ), "municipio" ] <- c( "Peixe-Boi" )
`28o-sorteio`[ c( 30, 52 ), "municipio" ] <- c( "Machadinho D'Oeste", "Olho d'Água das Cunhãs" )
`29o-sorteio`[ c( 11, 28, 31 ), "municipio" ] <- c( "Não-Me-Toque", "Itaporanga d'Ajuda", "Olho d'Água Grande" )
`30o-sorteio`[ c( 32, 34, 52 ), "municipio" ] <- c( "Olho d'Água das Flores", "Pau D'Arco", "Barra D'Alcântara" )
`33o-sorteio`[ c( 26 ), "municipio" ] <- c( "São Felipe D'Oeste" )
`36-sorteio`[ c( 42 ), "municipio" ] <- c( "Olho-d'Água do Borges" )
`38-sorteio`[ c( 56 ), "municipio" ] <- c( "São João d'Aliança" )

todos_bancos <- Filter( function( x ) is( x, "data.frame" ), mget( ls( ) ) )

bancos_mergidos <- Reduce( function( x, y ) merge( x, y, all = TRUE ), todos_bancos )

write.csv( bancos_mergidos, "municipios_sorteados_22-38.csv" )
