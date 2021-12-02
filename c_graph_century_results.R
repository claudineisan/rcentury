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

library(ggplot2)
library(tidyverse)
library(doBy)

###
#' input data
result_century <- read_csv('resultados/resultados_century_simulations.csv')

result_century$ponto_id <- substr(result_century$ponto,
                      nchar(result_century$ponto)-3,
                      nchar(result_century$ponto))

summary_result_century <- summaryBy(somsc ~ ponto_id, 
                                    data = result_century,
                                    FUN = c(mean)) 
names(summary_result_century) <- c('ponto_id', 'somsc')
summary_result_century$somsc <- summary_result_century$somsc/100
summary_result_century

###
###
#' plot

#' medias
data <- summary_result_century
ggplot_i <- ggplot(data, mapping = aes(ponto_id, somsc, color = ponto_id)) +
  geom_point(size = 5) +
  xlab('Tempo (anos)') +
  ylab('Estoque de Carbon (MgC.ha-1)') +
  ggtitle('Estoque de Carbono na Matéria Orgânica') +
  theme(axis.text = element_text(colour = '#666666', size=12, face = 'plain', angle = 0),
        axis.text.x=element_text(colour = '#666666', size=12, face = 'plain', angle = 30, vjust = 0.6, hjust = 0.7),
        axis.title=element_text(colour = '#000000', size=15, face = "plain"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.line.y = element_line(colour = '#666666', size = 0.8),
        axis.line.x = element_line(colour = '#666666', size = 0.8),
        legend.title = element_blank(),
        legend.text = element_text(colour = '#666666', size = 12, face = 'plain'),
        legend.position = 'right',
        legend.direction = 'vertical',
        plot.title = element_text(colour = '#000000', size=15, face = 'bold', angle = 0, hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(colour = '#666666', size=20, face = 'plain'),
  )
ggplot_i


###
#' media anual
result_century$ano <- round(result_century$time, 0)
summary_result_century <- summaryBy(somsc + aglivc ~ ponto_id + ano, 
                                    data = result_century,
                                    FUN = mean) 

names(summary_result_century) <- str_remove(names(summary_result_century), '.mean')
summary_result_century$somsc <- summary_result_century$somsc/100

data <- summary_result_century
ggplot_i <- ggplot(data, mapping = aes(ano, somsc, color = ponto_id)) +
  geom_line(size = 1) +
  geom_point(size = 2, shape = 16) +
  xlab('Tempo (anos)') +
  ylab('Estoque de Carbon (MgC.ha-1)') +
  ggtitle('Estoque de Carbono na Matéria Orgânica') +
  theme(axis.text = element_text(colour = '#666666', size=12, face = 'plain', angle = 0),
        axis.text.x=element_text(colour = '#666666', size=12, face = 'plain', angle = 30, vjust = 0.6, hjust = 0.7),
        axis.title=element_text(colour = '#000000', size=15, face = "plain"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.line.y = element_line(colour = '#666666', size = 0.8),
        axis.line.x = element_line(colour = '#666666', size = 0.8),
        legend.title = element_blank(),
        legend.text = element_text(colour = '#666666', size = 12, face = 'plain'),
        legend.position = 'right',
        legend.direction = 'vertical',
        plot.title = element_text(colour = '#000000', size=15, face = 'bold', angle = 0, hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(colour = '#666666', size=20, face = 'plain'),
  )
ggplot_i


#' serie temporal
data <- result_century
data$time <- as.numeric(data$time)
  ggplot_i <- ggplot(data, mapping = aes(time, somsc/100, color = ponto)) +
  geom_line(size = 0.4) +
  geom_point(size = 0.8) +
  xlab('Tempo (anos)') +
  ylab('Estoque de Carbon (MgC.ha-1)') +
  ggtitle('Estoque de Carbono na Matéria Orgânica') +
  theme(axis.text = element_text(colour = '#666666', size=12, face = 'plain', angle = 0),
        axis.text.x=element_text(colour = '#666666', size=12, face = 'plain', angle = 30, vjust = 0.6, hjust = 0.7),
        axis.title=element_text(colour = '#000000', size=15, face = "plain"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.line.y = element_line(colour = '#666666', size = 0.8),
        axis.line.x = element_line(colour = '#666666', size = 0.8),
        legend.title = element_blank(),
        legend.text = element_text(colour = '#666666', size = 12, face = 'plain'),
        legend.position = 'right',
        legend.direction = 'vertical',
        plot.title = element_text(colour = '#000000', size=15, face = 'bold', angle = 0, hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(colour = '#666666', size=20, face = 'plain'),
  )
ggplot_i

#####################################################################
#####################################################################