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
library( haven )

setwd( "G:/Trabalho/Dissertacao/Datasets/Ferraz/Stata/" )

#path <- "G:/Trabalho/Dissertacao/Datasets/Ferraz/Stata/"

# Ferraz usa dados das eleições de 2004, 2008 e 2012

names <- list.files( pattern = "*.dta" )

for( i in names ){

  corrected_name <- tools::file_path_sans_ext( i )
    
  assign( corrected_name, read_dta( i ) )
}

length( unique( `table4-1`$cod_munic ) )

# Trabalharemos com a Table2. Usaremos um arredondamento do exp( 1 ) para reverter o logaritmo natural da lpop
#options( digits = 5 )

table2$populacao <- round( exp( table2$lpop ) )

match_ferraz_ibge <- match( table2$populacao, banco_ibge_source$populacao )
match_ferraz_ibge <- match( banco_ibge_source$populacao, table2$populacao )
match_ferraz_ibge <- match_ferraz_ibge[ complete.cases( match_ferraz_ibge ) ]

# PREFERÍVEL -> #

match_ferraz_ibge <- match( ibge_0108$populacao, table2$populacao )
match_ferraz_ibge <- match( table2$populacao, ibge_0108$populacao )
match_ferraz_ibge <- match_ferraz_ibge[ complete.cases( match_ferraz_ibge ) ]

table2 <- left_join( table2, ibge_0508[ , c( "populacao", "id_city_ibge" ) ], by = "populacao" )

table2$id_city_ibge <- ibge_0108[ match_ferraz_ibge, "id_city_ibge" ]

# PREFERÍVEL <- #

match_ferraz_ibge <- match( table2$populacao, banco_ibge_populacao$populacao )
match_ferraz_ibge <- match( banco_ibge_populacao$populacao, table2$populacao )
match_ferraz_ibge <- match_ferraz_ibge[ complete.cases( match_ferraz_ibge ) ]

table2$id_city_ibge <- banco_ibge_populacao[ match_ferraz_ibge, ]

# ----------- #

data_table1$lpop <- log( data_table1$pop, base = exp( 1 ) )
data_table3$pop <- round( exp( data_table3$lpop ) )

compare_pop <- as.data.frame( cbind( data_table1$pop, data_table3$pop ) )
compare_pop$V3 <- round( compare_pop$V2 )

match_compare_pop <- match( compare_pop$V1, compare_pop$V3 )
match_compare_pop <- match_compare_pop[ complete.cases( match_compare_pop ) ]
length( unique( match_compare_pop ) )
unmatch_compare_pop <- intersect( compare_pop$V1, compare_pop$V3 )
compare_pop[ unmatch_compare_pop, ]

placeholder <- as.data.frame( compare_pop[ match_compare_pop, ] )

test_table1_lpop <- as.data.frame( table1$lpop )
test_table1_lpop <- as.data.frame( test_table1_lpop[ order( test_table1_lpop[ , 1 ], decreasing = FALSE ), ] )
test_table1_lpop <- as.data.frame( test_table1_lpop[ -c( 1 ), ] )
colnames( test_table1_lpop ) <- "lpop"

test_table2_lpop <- as.data.frame( table2$lpop )
test_table2_lpop <- as.data.frame( test_table2_lpop[ order( test_table2_lpop[ , 1 ], decreasing = FALSE ), ] )
colnames( test_table2_lpop ) <- "lpop"

test_table3_lpop <- as.data.frame( table3$lpop )
test_table3_lpop <- as.data.frame( test_table3_lpop[ order( test_table3_lpop[ , 1 ], decreasing = FALSE ), ] )
colnames( test_table3_lpop ) <- "lpop"

rm(log_test_df )
log_test_df <- as.data.frame( cbind( test_table1_lpop$lpop, test_table2_lpop$lpop, test_table3_lpop$lpop ) )
# log_test_df <- data.frame( )
# log_test_df$V1 <- test_table1_lpop$
# log_test_df$V2 <- test_table2_lpop$lpop
# log_test_df$V3 <- test_table3_lpop$lpop
log_test_df$diff <- log_test_df$V1 - log_test_df$V2
log_test_df$less1 <- log_test_df$diff < 1

print( typeof( table2$lpop[ 5 ] ) )
print( typeof( table1$pop[ 5 ] ) )

table1$lpop[ 2 ]

match_ferraz_ferraz <- match( table1$lpop, table2$lpop )
match_ferraz_ferraz <- match( table2$lpop, table1$lpop )

which( data_table1 == 12708, arr.ind = TRUE )

match_ferraz_ibge_0104 <- match( ibge_0108$populacao, data_table1$pop )
match_ferraz_ibge_0104 <- match_ferraz_ibge_0104[ complete.cases( match_ferraz_ibge_0104 ) ]

match_ferraz_ibge_0508 <- match( data_table2$lpop, ibge_0508$log_natural_pop )
#match_ferraz_ibge_0108 <- match( data_table2$lpop, ibge_0108$log_natural_pop )

match_ferraz_ibge_0108 <- match( data_table1$pop, ibge_0108$populacao )
match_ferraz_ibge_0108 <- match_ferraz_ibge_0108[ complete.cases( match_ferraz_ibge_0108 ) ]
match_ferraz_ibge_0108_ibge <- match( ibge_0108$populacao, data_table1$pop )
match_ferraz_ibge_0108_ibge <- match_ferraz_ibge_0108_ibge[ complete.cases( match_ferraz_ibge_0108_ibge ) ]

match_ferraz_ibge_censo <- match( data_table1$pop, ibge_populacao_censo$populacao )
match_ferraz_ibge_censo <- match_ferraz_ibge_censo[ complete.cases( match_ferraz_ibge_censo ) ]
match_ferraz_ibge_censo_2 <- match( ibge_populacao_censo$populacao, data_table1$pop )
match_ferraz_ibge_censo_2 <- match_ferraz_ibge_censo_2[ complete.cases( match_ferraz_ibge_censo_2 ) ]

test_ferraz_ibge <- data_table1
test_ferraz_ibge <- merge( test_ferraz_ibge, ibge_0108[ , c( "id_city_ibge", "populacao" ) ], by.x = "pop", by.y = "populacao" )
sum( is.na( test_ferraz_ibge ) )
test_ferraz_ibge <- test_ferraz_ibge[ complete.cases( test_ferraz_ibge ), ]
test_ferraz_ibge$lpop <- log( test_ferraz_ibge$pop )

test_log_ferraz <- data.frame( log( data_table1$pop ) )
colnames( test_log_ferraz )[ colnames( test_log_ferraz ) == "log.data_table1.pop." ] <- "log_pop"
match_log_ferraz <- match( data_table2$lpop, test_log_ferraz$log_pop )
match_log_ferraz <- match_log_ferraz[ complete.cases( match_log_ferraz ) ]

sprintf( "%.50f", test_ferraz_ibge[ 2, "lpop" ] )
sprintf( "%.50f", data_table2[ 884, "lpop" ] )

# Os logaritmos gerados a partir da Table1 não são compatíveis com os logaritmos na Table2
match_teste_ferraz_log <- match( data_table2$lpop, test_ferraz_ibge$lpop )
match_teste_ferraz_log <- match_teste_ferraz_log[ complete.cases( match_teste_ferraz_log ) ]
