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

setwd("G:/Trabalho/Dissertacao/Datasets/Ferraz/Stata/")

#path <- "G:/Trabalho/Dissertacao/Datasets/Ferraz/Stata/"

names <- list.files( pattern = "*.dta" )

for( i in names ){
  
  assign( i, read_dta( i ) )
}

length( unique( `table4-1.dta`$cod_munic ) )

#data_list <- lapply( names, read_dta )

# data_fig3 <- as.data.frame( read_dta( "figure3.dta" ) )
# data_table41 <- as.data.frame( read_dta( "table4-1.dta" ) )
data_table2 <- as.data.frame( read_dta( "table2.dta" ) )
data_table1 <- as.data.frame( read_dta( "table1.dta" ) )

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
