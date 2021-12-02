##################################################################
#' Claudinei Oliveira dos Santos
#' Biologo | Msc. Ecologia | Dr. Ciências Ambientais
#' LAPIG - UFG
#' claudineisan@pastoepixel.com
##################################################################

#' Ler os arquivos *.lis

###
#' Packages, functions and configurations
options(scipen = 9999)
library(tidyverse)

###
#' input data
lis_files <- Sys.glob(file.path("resultados", "lu*.lis"))

###
# lis_files_id <- substr(lis_files, 15, 18)
lis_files_id <- substr(lis_files, 12, nchar(lis_files)-4)
nloop <- length(lis_files_id)

df_results <- NULL
for (i in 1:nloop){

  id_i <- lis_files_id[i]
  lis_file_i <- grep(id_i, lis_files, value = TRUE)
  
  df_results_i <- read.table(lis_file_i, h = T)
  sub_df_lis_i <- df_results_i[between(df_results_i$time, 1980, 2020), ]
  sub_df_lis_i$ponto <- str_replace(id_i, '/', '_')
  sub_df_lis_i <- sub_df_lis_i[,c(ncol(sub_df_lis_i), 1:(ncol(sub_df_lis_i)-1))]
  sub_df_lis_i$time
  
  df_results <- rbind(df_results, sub_df_lis_i)
}

###
#' output file
write.csv(df_results, file = 'resultados/resultados_century_simulations.csv', row.names = FALSE)

##################################################################
