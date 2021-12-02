##################################################################
#' Claudinei Oliveira dos Santos
#' Biologo | Msc. Ecologia | Dr. Ciências Ambientais
#' LAPIG - UFG
#' claudineisan@pastoepixel.com
##################################################################

#' Gerar aquivo wth

##
#' Packages, functions and configurations
options(scipen = 9999)

library(tidyverse)
library(lubridate)
library(gtools)

source('r_function_century_simulations.r')

###
#' input data
clima_1991_2005 <- read.csv("dados/climate_data_1991_2005.csv")
clima_2006_2100 <- read.csv("dados/climate_data_2006_2100.csv")

###
#' input data
names(clima_1991_2005) <- c('ano', 'mes', "prec", "tmin", "tmax")
names(clima_2006_2100) <- c('ano', 'mes', "prec", "tmin", "tmax")

wth_clima_1991_2005 <- createCenturyWth(clima_1991_2005)
wth_clima_2006_2100 <- createCenturyWth(clima_2006_2100)

write.table(wth_clima_1991_2005,
            file = "dados/clima_1991_2005.wth",
            row.names = FALSE,
            col.names = FALSE,
            sep = "  ",
            quote = FALSE)

write.table(wth_clima_2006_2100,
            file = "dados/clima_2006_2100.wth",
            row.names = FALSE,
            col.names = FALSE,
            sep = "  ",
            quote = FALSE)

####################################################
