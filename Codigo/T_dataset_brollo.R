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
library( lfe )
library( fixest )
library( sandwich )
library( plm )

setwd("G:/Trabalho/Dissertacao/Datasets/Brollo/")
#setwd( "/media/kaique/Arquivos/Trabalho/Dissertacao/Datasets/Brollo/" )

options( digits = 22 )

large_banco_brollo <- as.data.frame( read.dta( "AER_largesample.dta" ) )
small_banco_brollo <- as.data.frame( read.dta( "AER_smallsample.dta" ) )

colnames( large_banco_brollo )[ colnames( large_banco_brollo ) == "id_city" ] = "id_city_siafi"
colnames( small_banco_brollo )[ colnames( small_banco_brollo ) == "id_city" ] = "id_city_siafi"

# test_cluster_city <- feols( opp_college ~ fpm + fpm_hat + pop + pop_2 + pop_3 + term + regions + id_city, large_banco_brollo )
# summary( test_cluster_city )
# 
# test_clust <- se( vcovCL( test_cluster_city, cluster = ~id_city, type = "HC1" ) )[ "id_city" ]
# test_clust
# 
# print( 9.170594e-06 )

#small_banco_brollo[ , "id_city" ] <- small_banco_brollo[ , "id_city" ] * 2646.17443938753
#small_banco_brollo[ , "uf" ] <- small_banco_brollo[ , "uf" ] * 19.8330458389463
#small_banco_brollo[ , "regions" ] <- small_banco_brollo[ , "regions" ] * 24.3891209928529

#small_banco_brollo$id_city <- round( small_banco_brollo$id_city )
#small_banco_brollo$uf <- round( small_banco_brollo$uf )
#small_banco_brollo$regions <- round( small_banco_brollo$regions )

large_banco_brollo[ 1675, ]
small_banco_brollo[ 758, ]

large_banco_brollo[ 2677, ]
small_banco_brollo[ 944, ]

write.csv( small_banco_brollo, "small_banco_brollo.csv" )
write.csv( large_banco_brollo, "large_banco_brollo.csv" )

intersection <- data.frame( intersect( names(large_banco_brollo), names(small_banco_brollo) ) )
#intersection <- data.frame( intersect( names( small_banco_brollo ), names( large_banco_brollo ) ) )
intersection <- intersection[ -c( 1, 2, 3 ), ]
#intersection <- intersection[ -c( 71, 72, 73 ), ]

intersection

small_banco_brollo[ , -which( names( large_banco_brollo ) %in% intersection ) ]
small_banco_brollo[ , which( names( large_banco_brollo ) %in% intersection ) ]
small_banco_brollo[ , !names( large_banco_brollo ) %in% intersection ]
small_banco_brollo[ , !names( large_banco_brollo ) %in% names( small_banco_brollo ) ]

# Usar colunas em comum entre large e small para repor id_city em small
#intersec_large_small <- small_banco_brollo[ , which( names( large_banco_brollo ) %in% intersection ) ]
intersec_large_small <- small_banco_brollo[ , intersection ]
#intersec_large_small <- small_banco_brollo[ , which( names( large_banco_brollo ) %in% intersect( names( large_banco_brollo ), names( small_banco_brollo ) ) ) ]
sum( is.na( intersec_large_small$id_city ) )

names( small_banco_brollo )
names( large_banco_brollo )
intersect( names( large_banco_brollo ), names( small_banco_brollo ) )[ -c( 1, 2, 3 ) ]
intersect( names( small_banco_brollo ), names( large_banco_brollo ) )
names( intersec_large_small )

# To many NA's
intersec_large_small <- left_join( intersec_large_small, large_banco_brollo[ , c( "id_city", "uf", "regions", "term", "pop", "pop_2", "pop_3" ) ], by = c( "term", "pop", "pop_2", "pop_3" ) )
sum( is.na( intersec_large_small$id_city ) )

intersec_large_small <- left_join( intersec_large_small, small_banco_brollo[ , c( "broad", "narrow", "term", "pop", "pop_2", "pop_3" ) ], by = c( "term", "pop", "pop_2", "pop_3" ) )
sum( is.na( intersec_large_small$id_city ) )

length( unique( large_banco_brollo$id_city ) )
length( unique( small_banco_brollo$id_city ) )

# O número de NA's ainda é alto, mas vou trabalhar com isso por enquanto, 500obs ainda é bom
# TODO: entender o porquê de tanto NA -> PROVAVELMENTE por causa do "term", ou por causa dos números DECIMAIS em pop, pop_2 e pop_3!!!!
# ATENÇÃO: esse banco é potencialmente problemático
banco_teste <- merge( large_banco_brollo, small_banco_brollo[ , c( intersection, "broad", "narrow" ) ], by = intersection ) # id_city tá vindo de qual banco? Large ou Small?
banco_teste <- right_join( large_banco_brollo, small_banco_brollo[ , c( intersection, "broad", "narrow" ) ], by = intersection ) # id_city tá vindo de qual banco? Large ou Small?
#banco_teste_v2 <- merge( large_banco_brollo, small_banco_brollo[ , c( intersection, "broad", "narrow" ) ], by = intersection )
sum( is.na( banco_teste$broad ) )
sum( is.na( banco_teste$narrow ) )
sum( is.na( banco_teste$id_city ) ) 

names( banco_teste )

write.csv( banco_teste, "banco_teste.csv" )

# TODO: talvez criar bancos para cada ano? 2001 e 2005

small_brollo_2001 <- subset( small_banco_brollo, term == "2001" )
small_brollo_2005 <- subset( small_banco_brollo, term == "2005" )
small_intersect_2001 <- subset( intersec_large_small, term == "2001" )
small_intersect_2005 <- subset( intersec_large_small, term == "2005" )

small_intersect_2001 <- left_join( small_intersect_2001, large_banco_brollo[ , c( "id_city", "pop", "pop_2", "pop_3" ) ], by = c( "pop", "pop_2", "pop_3" ) )
sum( is.na( small_intersect_2001$id_city ) )

small_intersect_2005 <- left_join( small_intersect_2005, large_banco_brollo[ , c( "id_city", "pop", "pop_2", "pop_3" ) ], by = c( "pop", "pop_2", "pop_3" ) )
sum( is.na( small_intersect_2001$id_city ) )

sprintf( "%.10f", small_intersect_2001[ 1, 2 ] )

which( small_intersect_2001 == 10507.6669921875, arr.ind = TRUE )
which( small_intersect_2001 == 16450, arr.ind = TRUE )
which( intersec_large_small == 10507.667, arr.ind = TRUE )
which( small_banco_brollo == 10507.6669921875, arr.ind = TRUE )
which( large_banco_brollo == 10507, arr.ind = TRUE )
which( large_banco_brollo == 10710.000, arr.ind = TRUE )

large_banco_brollo[ 2814, c( 4, 5, 38, 73 ) ]
small_banco_brollo[ 1, c( 1, 2, 35, 70 ) ]

test <- data.frame( table( large_banco_brollo$id_city ) )
test <- subset( test, Freq > 1 )


# Assumindo que ID_CITY == Codigo SIAFI dos municípios
  # adicionar nomes dos municípios ao banco de brollo
  # depois usar nomes dos municípios para adicionar candidatos e partidos ao banco de brollo
  # adicionar ideologia ao banco de brollo

copia_large_brollo <- left_join( x = large_banco_brollo, y = fpm[ , c( "Município", "id_city", "term" ) ], by = c( "id_city", "term" ) )
copia_large_brollo <- subset( copia_large_brollo, pop <= 50940 )
copia_large_brollo$pop

copia_small_brollo <- left_join( x = banco_teste, y = fpm[ , c( "Município", "id_city", "term" ) ], by = c( "id_city", "term" ) )
copia_small_brollo <- copia_small_brollo[ complete.cases( copia_small_brollo ), ]
sum( is.na( copia_small_brollo$Município ) )
sum( is.na( copia_small_brollo$broad ) )
sum( is.na( copia_small_brollo$narrow ) )

write.csv( copia_small_brollo, "small_brollo_municipio.csv" )

small_brollo_municipio <- read.csv( "small_brollo_municipio.csv", sep = "," )

# Agora pegar os dados do TSE - Partido e Ideologia, e colocar no banco

test <- data.frame( table( small_banco_brollo$id_city ) )
test

#broad narrow fraction_broad fraction_narrow before m_yschool dumm0     id_city         uf    regions
#broad narrow fraction_broad fraction_narrow before m_yschool dumm0
# Codificando cada faixa populacional de acordo com as divisões que a autora faz
# large_banco_brollo <- large_banco_brollo %>% mutate( pop_group = case_when( pop < 10189 ~ 1,
#                                                                             pop >= 10189 & pop <= 13584 ~ 2,
#                                                                             pop >= 13585 & pop <= 16980 ~ 3,
#                                                                             pop >= 16981 & pop <= 23772 ~ 4,
#                                                                             pop >= 23773 & pop <= 30564 ~ 5,
#                                                                             pop >= 30565 & pop <= 37356 ~ 6,
#                                                                             pop >= 37357 & pop <= 44148 ~ 7,
#                                                                             pop >= 44149 & pop <= 50940 ~ 8,
#                                                                             pop > 50940 ~ 9 ) )

# Desnecessário: pop_cat == pop_group
# small_banco_brollo <- small_banco_brollo %>% mutate( pop_group = case_when( pop < 10189 ~ 1,
#                                                                             pop >= 10189 & pop <= 13584 ~ 2,
#                                                                             pop >= 13585 & pop <= 16980 ~ 3,
#                                                                             pop >= 16981 & pop <= 23772 ~ 4,
#                                                                             pop >= 23773 & pop <= 30564 ~ 5,
#                                                                             pop >= 30565 & pop <= 37356 ~ 6,
#                                                                             pop >= 37357 & pop <= 44148 ~ 7,
#                                                                             pop >= 44149 & pop <= 50940 ~ 8,
#                                                                             pop > 50940 ~ 9 ) )


# Recriando a tabela 2, p.1772
fpm_large <- aggregate( large_banco_brollo$fpm, list( large_banco_brollo$pop_cat ), mean )
fpm_small <- aggregate( small_banco_brollo$fpm, list( small_banco_brollo$pop_cat ), mean )

small_banco_brollo[ , c( "pop_cat", "pop_group" ) ]

# Usar o small_banco_brollo pois é o único com medidas de corrupção
# As variáveis binárias indicando corrupção são: narrow e broad

