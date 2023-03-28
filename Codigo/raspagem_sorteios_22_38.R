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
#library( plyr )
library( basedosdados )
library( DescTools )
library( magrittr )
library( tm )
library( stringi )
library( gtools )
library( ff )
#library( ffbase )
library( haven )
library( stringr )

setwd( "G:/Trabalho/Dissertacao/Datasets/Sorteios/" )

### RASPAR OS SITES DA CGU PARA PEGAR CIDADES SORTEADAS NAS LOTERIAS 1-20 ###

# Site da CGU contendo links para as páginas das cidades sorteadas
link_cgu_loterias_01_20 <- "https://www.gov.br/cgu/pt-br/assuntos/auditoria-e-fiscalizacao/programa-de-fiscalizacao-em-entes-federativos/edicoes-anteriores/municipios?b_start:int=20"

# Extraindo somente as páginas dos municípios sorteados
links_sorteios_01_20 <- read_html( link_cgu_loterias_01_20 ) %>% 
                        html_nodes( "a" ) %>%
                        html_attr( "href" ) %>%
                        str_subset( "https://www.gov.br/cgu/pt-br/assuntos/auditoria-e-fiscalizacao/programa-de-fiscalizacao-em-entes-federativos/edicoes-anteriores/municipios/" )

# Tratando as páginas: removendo informações desnecessárias e mantendo apenas nomes das cidades e os estados das quais fazem parte
#                      após a limpeza, carregar cidades, estados e respectivo número do sorteio em dataframes para cada sorteio.
for( link in links_sorteios_01_20 ){
  
  num_sorteio <- gsub( "https://www.gov.br/cgu/pt-br/assuntos/auditoria-e-fiscalizacao/programa-de-fiscalizacao-em-entes-federativos/edicoes-anteriores/municipios/" , "" , link )
  
  data <- as.data.frame( read_html( link ) %>% html_nodes( xpath = "//p" ) %>% html_text( ) )
  data <- as.data.frame( data[ grepl( "1º|31º", data$`read_html(link) %>% html_nodes(xpath = "//p") %>% html_text()` ), ] )
  
  epoca <- read_html( link ) %>% html_nodes( "div" ) %>% html_text( )
  epoca <- unique( str_extract_all( epoca, "[0-9]{2}/[0-9]{2}/[0-9]{4}", "%d/%m/%Y" ) )
  epoca <- as.numeric( str_extract_all( epoca[ 2, 1 ], "[0-9]{4}", "\\1" ) )
  
  colnames( data ) <- "valores"
  
  cities_to_rows <- str_replace_all( data$valores, "\\d[º]|\\d [º]", "," )
  cities_to_rows <- as.data.frame( cities_to_rows )
  cities_to_rows <- cities_to_rows %>% 
                    mutate( cities_to_rows = strsplit( as.String( cities_to_rows ), "," ) ) %>%
                    unnest( cities_to_rows ) %>%
                    unique( )
  
  cities_to_rows <- cities_to_rows[ !apply( cities_to_rows == "", 1, all ), ]
  
  cities_to_rows$cities_to_rows <- str_replace_all( cities_to_rows$cities_to_rows, "[^[:alnum:][:space:]]", "" )
  cities_to_rows$cities_to_rows <- str_replace_all( cities_to_rows$cities_to_rows, "[[:digit:]]", "" )
  
  colnames( cities_to_rows )[ colnames( cities_to_rows ) == "cities_to_rows" ] <- "municipio"
  
  cities_to_rows$uf <- str_replace_all( cities_to_rows$municipio, ".*\\b([A-Z]{2})", "\\1" )
  
  cities_to_rows$sorteio <- gsub( "[^0-9]", "", num_sorteio )
  
  cities_to_rows$municipio <- gsub( "([A-Z]{2}).*", "", cities_to_rows$municipio )
  cities_to_rows$municipio <- trimws( cities_to_rows$municipio, which = "both" )
  
  cities_to_rows$year <- epoca
  
  assign( num_sorteio, cities_to_rows )
}

rm( epoca, link_cgu_loterias_01_20, links_sorteios_01_20, num_sorteio, data, cities_to_rows )

# TRATAMENTO sorteios: 
#   1(tudo), 
`1deg-sorteio`[ , "municipio" ] <- c( "Balneário Arroio do Silva", "Ribeirão Corrente", "Castelândia", "Colônia do Piauí", "Rio Preto da Eva" )

#   10[] - linhas vazias 4, 5, [18] pontuação
`10deg-sorteio` <- `10deg-sorteio`[ -c( 4, 5 ), ]
`10deg-sorteio`[ 18, "municipio" ] <- "Lambari d'Oeste"

#   11[] - linhas vazias 4, 5
`11deg-sorteio` <- `11deg-sorteio`[ -c( 4, 5 ), ]

#   12[] - linhas vazias 4, 5
`12deg-sorteio`<- `12deg-sorteio`[ -c( 4, 5 ), ]
`12deg-sorteio`[ 3, "municipio" ] <- "Ji-Paraná"

# Montando lista com todos os dataframes relevantes
todos_bancos_01_20 <- Filter( function( x ) is( x, "data.frame" ), mget( ls( ) ) )

# Mergindo os bancos com municípios sorteados de cada loteria em um único banco
bancos_mergidos_01_20 <- Reduce( function( x, y ) merge( x, y, all = TRUE ), todos_bancos_01_20 )

# TRATAMENTO: removendo carácteres irregulares das siglas UF dos estados
bancos_mergidos_01_20$uf <- str_replace_all( bancos_mergidos_01_20$uf, "[^[A-Z]]", "" )

# DO: ordernar siglas dos estados alfabeticamente e atribuir a cada sigla um valor de 1 a 26
#lista_estados <- setNames( data.frame( mixedsort( unique( bancos_mergidos$uf ) ) ), "uf" )
#lista_estados$uf_id <- seq.int( nrow( lista_estados ) )

#bancos_mergidos <- left_join( bancos_mergidos, lista_estados[ , c( "uf", "uf_id" ) ], by = "uf" )

# Banco final: municípios sorteados nas loterias 22 a 38
write.csv( bancos_mergidos_01_20, "municipios_sorteados_01-20.csv" )

### TÉRMINO RASPAGEM 1-20 ###

# Limpando ambiente para iniciar segunda raspagem
rm( list = ls( ) )

### RASPAR OS SITES DA CGU PARA PEGAR CIDADES SORTEADAS NAS LOTERIAS 21-38 ###

# Site da CGU contendo links para as páginas das cidades sorteadas
link_cgu_loterias <- "https://www.gov.br/cgu/pt-br/assuntos/auditoria-e-fiscalizacao/programa-de-fiscalizacao-em-entes-federativos/edicoes-anteriores/municipios"

# Extraindo somente as páginas dos municípios sorteados
links_sorteios <- read_html( link_cgu_loterias ) %>% 
                             html_nodes( "a" ) %>%
                             html_attr( "href" ) %>%
                             str_subset( "https://www.gov.br/cgu/pt-br/assuntos/auditoria-e-fiscalizacao/programa-de-fiscalizacao-em-entes-federativos/edicoes-anteriores/municipios/" )

# Tratando as páginas: removendo informações desnecessárias e mantendo apenas nomes das cidades e os estados das quais fazem parte
#                      após a limpeza, carregar cidades, estados e respectivo número do sorteio em dataframes para cada sorteio.
for( link in links_sorteios ){
  
  num_sorteio <- gsub( "https://www.gov.br/cgu/pt-br/assuntos/auditoria-e-fiscalizacao/programa-de-fiscalizacao-em-entes-federativos/edicoes-anteriores/municipios/" , "" , link )
  
  data <- as.data.frame( read_html( link ) %>% html_nodes( xpath = "//p" ) %>% html_text( ) )
  data <- as.data.frame( data[ grepl( "1º|31º", data$`read_html(link) %>% html_nodes(xpath = "//p") %>% html_text()` ), ] )
  
  epoca <- read_html( link ) %>% html_nodes( "div" ) %>% html_text( )
  epoca <- unique( str_extract_all( epoca, "[0-9]{2}/[0-9]{2}/[0-9]{4}", "%d/%m/%Y" ) )
  epoca <- as.numeric( str_extract_all( epoca[ 2, 1 ], "[0-9]{4}", "\\1" ) )
  
  colnames( data ) <- "valores"
  
  cities_to_rows <- str_replace_all( data$valores, "\\d[º]|\\d [º]", "," )
  cities_to_rows <- as.data.frame( cities_to_rows )
  cities_to_rows <- cities_to_rows %>% 
                    mutate( cities_to_rows = strsplit( as.String( cities_to_rows ), "," ) ) %>%
                    unnest( cities_to_rows ) %>%
                    unique( )
  
  cities_to_rows <- cities_to_rows[ !apply( cities_to_rows == "", 1, all ), ]
  
  cities_to_rows$cities_to_rows <- str_replace_all( cities_to_rows$cities_to_rows, "[^[:alnum:][:space:]]", "" )
  cities_to_rows$cities_to_rows <- str_replace_all( cities_to_rows$cities_to_rows, "[[:digit:]]", "" )
  
  colnames( cities_to_rows )[ colnames( cities_to_rows ) == "cities_to_rows" ] <- "municipio"
  
  cities_to_rows$uf <- str_replace_all( cities_to_rows$municipio, ".*\\b([A-Z]{2})", "\\1" )
  
  cities_to_rows$sorteio <- gsub( "[^0-9]", "", num_sorteio )
  
  cities_to_rows$municipio <- gsub( "([A-Z]{2}).*", "", cities_to_rows$municipio )
  cities_to_rows$municipio <- trimws( cities_to_rows$municipio, which = "both" )
  
  cities_to_rows$year <- epoca
  
  assign( num_sorteio, cities_to_rows )
}

# Removendo items irrelevantes do Global Environment. Faço isso pois uso os itens relevantes na construção da lista
# que contém as bases a serem usadas na versão final do banco de municípios sorteados.
# rm( `21o-sorteio`, `40o-sorteio`, `copy_of_38-sorteio`, cities_to_rows, data, link, link_cgu_loterias, links_sorteios, num_sorteio )
rm( `40o-sorteio`, `copy_of_38-sorteio`, cities_to_rows, data, link, link_cgu_loterias, links_sorteios, num_sorteio )

# Sorteios com problemas: 22[2,42,50,], 23[42], 26[14, 52], 27[39], 28[30, 52], 29[11, 28, 31], 30[32, 34, 52], 33[26], 36[42] e 36[21, uf], 38[56]
# PROBLEMAS: Nomes das cidades incorretos por acentos e pontuação. Como são poucas, corrijo manualmente
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

`36-sorteio`[ c( 21 ), "uf" ] <- c( "MG" )

# Montando lista com todos os dataframes relevantes
todos_bancos <- Filter( function( x ) is( x, "data.frame" ), mget( ls( ) ) )

# Mergindo os bancos com municípios sorteados de cada loteria em um único banco
bancos_mergidos <- Reduce( function( x, y ) merge( x, y, all = TRUE ), todos_bancos )

# TRATAMENTO: removendo carácteres irregulares das siglas UF dos estados
bancos_mergidos$uf <- str_replace_all( bancos_mergidos$uf, "[^[A-Z]]", "" )

# DO: ordernar siglas dos estados alfabeticamente e atribuir a cada sigla um valor de 1 a 26
# lista_estados <- setNames( data.frame( mixedsort( unique( bancos_mergidos$uf ) ) ), "uf" )
# lista_estados$uf_id <- seq.int( nrow( lista_estados ) )

# bancos_mergidos <- left_join( bancos_mergidos, lista_estados[ , c( "uf", "uf_id" ) ], by = "uf" )

# Banco final: municípios sorteados nas loterias 21 a 38
write.csv( bancos_mergidos, "municipios_sorteados_21-38.csv" )

### TÉRMINO RASPAGEM 21-38 ###

municipios_sorteados_01_20 <- read.csv( "municipios_sorteados_01-20.csv", sep = "," )
municipios_sorteados_01_20 <- municipios_sorteados_01_20[ , -c( 1 ) ]
municipios_sorteados_21_38 <- read.csv( "municipios_sorteados_21-38.csv", sep = "," )
municipios_sorteados_21_38 <- municipios_sorteados_21_38[ , -c( 1 ) ]

municipios_sorteados_01_38 <- merge( municipios_sorteados_01_20, municipios_sorteados_21_38, all = TRUE )
write.csv( municipios_sorteados_01_38, "municipios_sorteados_01_38.csv" )

rm( list = ls( ) )

municipios_sorteados <- read.csv( "municipios_sorteados_01_38.csv", sep = "," )

### ZONA DE TESTES - INICIO ###

# TRATAMENTO: removendo colunas inseridas por causa do formato csv
municipios_sorteados <- municipios_sorteados[ , -c( 1 ) ]

# TESTE - BROLLO + IBGE + SORTEADOS #

municipios_sorteados <- subset( municipios_sorteados, sorteio <= 29 )

# TESTE - IBGE + MUNICIPIOS SORTEADOS #

setwd( "G:/Trabalho/Dissertacao/Datasets/IBGE/" )
setwd( "G:/Trabalho/Dissertacao/Datasets/IBGE/ibge_source/" )
setwd( "G:/Trabalho/Dissertacao/Datasets/IBGE/ibge_pop_censo_source/" )

teste <- left_join( municipios_sorteados, banco_ibge_source[ , c( "municipio", "populacao", "id_city_ibge" ) ], by = "municipio" )
teste <- left_join( municipios_sorteados, banco_ibge_source[ , c( "municipio", "id_city_ibge" ) ], by = "municipio" )
teste <- left_join( banco_ibge_populacao, teste[ , c( "municipio", "id_city_ibge", "sorteio", "uf" ) ], by = c( "uf", "id_city_ibge" ) )
teste <- teste[ complete.cases( teste ), ]
sum( is.na( teste$municipio ) )

table2 <- read_dta( "table2.dta" )
table2$populacao <- round( exp( table2$lpop ) )

rm(teste_ferraz)

teste_ferraz <- left_join( table2, teste[ , c( "sorteio", "municipio", "id_city_ibge", "populacao", "uf" ) ], by = c( "sorteio", "populacao" ) )
teste_ferraz <- teste_ferraz[ complete.cases( teste_ferraz ), ]
sum( is.na( teste_ferraz$id_city_ibge ) )
