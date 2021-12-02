##################################################################
#' Claudinei Oliveira dos Santos
#' Biologo | Msc. Ecologia | Dr. Ciências Ambientais
#' LAPIG - UFG
#' claudineisan@pastoepixel.com
##################################################################

#' Rodar o modelo century
#' Rodar o equilíbrio

##
#' Packages, functions and configurations
options(scipen = 9999)

#install.packages('tidyverse')
library(tidyverse)

source('r_function_century_simulations.r')

###
#' input data
dir_proj <- getwd()
dir_ptos <- 'pontos'
dir_cent <- 'century'
dir_rest <- 'resultados'

###
sch_eq <- Sys.glob(file.path(dir_ptos, 'e*.sch'))
cem_eq <- Sys.glob(file.path(dir_ptos, 'e*.100'))

sch_lu <- Sys.glob(file.path(dir_ptos, "l*.sch"))
cem_lu <- Sys.glob(file.path(dir_ptos, "l*.100"))
wth_lu <- Sys.glob(file.path(dir_ptos, "l*.wth"))

list_names <- substr(cem_eq, 11, 14) 
nloop <- length(list_names)

for(i in 1:nloop){
  cat('ponto =', i , list_names[i], '\n')
  
  list_names_i <- list_names[i]
  sch_eq_i <- grep(list_names_i, sch_eq, value = TRUE)
  cem_eq_i <- grep(list_names_i, cem_eq, value = TRUE)
  sch_lu_i <- grep(list_names_i, sch_lu, value = TRUE)
  cem_lu_i <- grep(list_names_i, cem_lu, value = TRUE)
  wth_lu_i <- grep(list_names_i, wth_lu, value = TRUE)
  
  runCentury(sch_eq_i = sch_eq_i,
             cem_eq_i = cem_eq_i,
             sch_lu_i = sch_lu_i,
             cem_lu_i = cem_lu_i,
             wth_lu_i = wth_lu_i,
             dir_proj = dir_proj,
             dir_ptos = dir_ptos,
             dir_cent = dir_cent,
             dir_rest = dir_rest,
             run_what = 'lu_cr')
  }

##################################################################
