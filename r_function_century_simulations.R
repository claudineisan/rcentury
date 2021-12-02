##################################################################
#' Claudinei Oliveira dos Santos
#' Biologo | Msc. Ecologia | Dr. Ciências Ambientais
#' LAPIG - UFG
#' claudineisan@pastoepixel.com
##################################################################

##
#' Packages, functions and configurations
options(scipen = 9999)

###
#' function century simulation
#' por padrão os argumentos opcionais são os últimos;
runCentury <- function(sch_eq_i = NULL,
                       cem_eq_i = NULL,
                       sch_lu_i = NULL,
                       cem_lu_i = NULL,
                       wth_lu_i = NULL,
                       dir_proj = NULL,
                       dir_ptos = NULL,
                       dir_cent = NULL,
                       dir_rest = NULL,
                       run_what = c("eq_cm", 
                                   "lu_cm", 
                                   "lu_cr"
                                   )
                       ){

  path_proj <- dir_proj
    
    if (run_what == "eq_cm") {
      
      print("Rodar century para o equilibrio")

      file.copy(from = sch_eq_i, 
                to = dir_cent
                )
      file.copy(from = cem_eq_i, 
                to = dir_cent
                )
      
      rename_eq_sch <- paste0(dir_cent, 
                              '/eq_site.sch'
                              )
      rename_eq_100 <- paste0(dir_cent, 
                              '/eq_site.100'
                              )
      
      file.rename(
        str_replace(sch_eq_i, 
                    dir_ptos, 
                    dir_cent
                    ), 
        rename_eq_sch
        )
      file.rename(
        str_replace(cem_eq_i, 
                    dir_ptos, 
                    dir_cent
                    ), 
        rename_eq_100
        )

      setwd(dir_cent)
      system('century -s eq_site -n eq_site')
      system('list100 eq_site eq_site output.txt')

      setwd(path_proj)
      
      bin_eq_site_i <- str_replace(rename_eq_sch, 
                                   '.sch', 
                                   '.bin'
                                   )
      lis_eq_site_i <- str_replace(rename_eq_sch, 
                                   '.sch', 
                                   '.lis'
                                   )

      bin_eq_i <- gsub("100", 
                       "bin", 
                       gsub(dir_ptos, 
                            dir_rest, 
                            cem_eq_i)
                       )
      lis_eq_i <- gsub("100", 
                       "lis", 
                       gsub(dir_ptos, 
                            dir_rest, 
                            cem_eq_i)
                       )
      
      file.copy(from = bin_eq_site_i, 
                to = bin_eq_i, 
                overwrite = FALSE
                )
      file.copy(from = lis_eq_site_i, 
                to = lis_eq_i, 
                overwrite = FALSE
                )
      
      file.remove(bin_eq_site_i,
                  lis_eq_site_i,
                  rename_eq_sch,
                  rename_eq_100
                  )

    } else if (run_what == "lu_cm") {
      
      print("Rodar century para o land use com clima medio")
      
      file.copy(from = sch_eq_i, 
                to = dir_cent
                )
      file.copy(from = cem_eq_i, 
                to = dir_cent
                )
      file.copy(from = sch_lu_i, 
                to = dir_cent
                )
      file.copy(from = cem_lu_i, 
                to = dir_cent
                )
      
      rename_eq_sch <- paste0(dir_cent,
                              '/eq_site.sch'
                              )
      rename_eq_100 <- paste0(dir_cent, 
                              '/eq_site.100'
                              )
      rename_lu_sch <- paste0(dir_cent,
                              '/lu_site.sch'
                              )
      rename_lu_100 <- paste0(dir_cent, 
                              '/lu_site.100'
                              )
      
      file.rename(str_replace(sch_eq_i,
                              dir_ptos,
                              dir_cent
                              ), 
                  rename_eq_sch
                  )
      file.rename(str_replace(cem_eq_i,
                              dir_ptos,
                              dir_cent
                              ),
                  rename_eq_100
                  )
      file.rename(str_replace(sch_lu_i,
                              dir_ptos,
                              dir_cent
                              ), 
                  rename_lu_sch
                  )
      file.rename(str_replace(cem_lu_i,
                              dir_ptos,
                              dir_cent
                              ),
                  rename_lu_100
                  )

      setwd(dir_cent)
      system('century -s eq_site -n eq_site')
      system('list100 eq_site eq_site output.txt')
      system('century -s lu_site -n lu_site -e eq_site')
      system('list100 lu_site lu_site output.txt')
      
      setwd(path_proj)
      
      bin_eq_site_i <- str_replace(rename_eq_sch, 
                                   '.sch', 
                                   '.bin'
                                   )
      lis_eq_site_i <- str_replace(rename_eq_sch, 
                                   '.sch', 
                                   '.lis'
                                   )
      bin_lu_site_i <- str_replace(rename_lu_sch, 
                                   '.sch', 
                                   '.bin'
                                   )
      lis_lu_site_i <- str_replace(rename_lu_sch,
                                   '.sch', 
                                   '.lis'
                                   )
      
      bin_eq_i <- gsub("100", 
                       "bin", 
                       gsub(dir_ptos, 
                            dir_rest, 
                            cem_eq_i)
                       )
      lis_eq_i <- gsub("100", 
                       "lis", 
                       gsub(dir_ptos, 
                            dir_rest, 
                            cem_eq_i)
                       )
      bin_lu_i <- gsub("100", 
                       "bin", 
                       gsub(dir_ptos, 
                            dir_rest,
                            cem_lu_i)
                       )
      lis_lu_i <- gsub("100",
                       "lis", 
                       gsub(dir_ptos,
                            dir_rest,
                            cem_lu_i)
                       )
      
      # file.copy(from = bin_eq_site_i, 
      #           to = bin_eq_i,
      #           overwrite = FALSE
      #           )
      # file.copy(from = lis_eq_site_i,
      #           to = lis_eq_i,
      #           overwrite = FALSE
      #           )
      file.copy(from = bin_lu_site_i,
                to = bin_lu_i, 
                overwrite = FALSE
                )
      file.copy(from = lis_lu_site_i,
                to = lis_lu_i, 
                overwrite = FALSE
                )

      file.remove(bin_eq_site_i,
                  lis_eq_site_i,
                  bin_lu_site_i,
                  lis_lu_site_i,
                  rename_eq_sch,
                  rename_eq_100,
                  rename_lu_sch,
                  rename_lu_100
                  )
      
    } else if (run_what == "lu_cr") {

      print("Rodar century para o land use com clima real")

      file.copy(from = sch_eq_i,
                to = dir_cent
                )
      file.copy(from = cem_eq_i, 
                to = dir_cent
                )
      file.copy(from = sch_lu_i,
                to = dir_cent
                )
      file.copy(from = cem_lu_i,
                to = dir_cent
                )
      file.copy(from = wth_lu_i,
                to = dir_cent
                )
      
      rename_eq_sch <- paste0(dir_cent,
                              '/eq_site.sch'
                              )
      rename_eq_100 <- paste0(dir_cent, 
                              '/eq_site.100'
                              )
      rename_lu_sch <- paste0(dir_cent,
                              '/lu_site.sch'
                              )
      rename_lu_100 <- paste0(dir_cent, 
                              '/lu_site.100'
                              )
      rename_lu_wth <- paste0(dir_cent,
                              '/lu_site.wth'
                              )
      
      file.rename(str_replace(sch_eq_i,
                              dir_ptos,
                              dir_cent
                              ), 
                  rename_eq_sch
                  )
      file.rename(str_replace(cem_eq_i,
                              dir_ptos,
                              dir_cent
                              ),
                  rename_eq_100
                  )
      file.rename(str_replace(sch_lu_i,
                              dir_ptos,
                              dir_cent
                              ), 
                  rename_lu_sch
                  )
      file.rename(str_replace(cem_lu_i,
                              dir_ptos,
                              dir_cent
                              ),
                  rename_lu_100
                  )
      file.rename(str_replace(wth_lu_i,
                              dir_ptos,
                              dir_cent
                              ),
                  rename_lu_wth
                  )
      
      setwd(dir_cent)
      system('century -s eq_site -n eq_site')
      system('list100 eq_site eq_site output.txt')
      system('century -s lu_site -n lu_site -e eq_site')
      system('list100 lu_site lu_site output.txt')
      
      setwd(dir_proj)
      
      bin_eq_site_i <- str_replace(rename_eq_sch,
                                   '.sch',
                                   '.bin'
                                   )
      lis_eq_site_i <- str_replace(rename_eq_sch,
                                   '.sch',
                                   '.lis'
                                   )
      bin_lu_site_i <- str_replace(rename_lu_sch,
                                   '.sch',
                                   '.bin'
                                   )
      lis_lu_site_i <- str_replace(rename_lu_sch,
                                   '.sch', 
                                   '.lis'
                                   )
      
      
      bin_eq_i <- gsub("100",
                       "bin",
                       gsub(dir_ptos, 
                            dir_rest,
                            cem_eq_i
                            )
                       )
      lis_eq_i <- gsub("100",
                       "lis", 
                       gsub(dir_ptos,
                            dir_rest,
                            cem_eq_i
                            )
                       )
      bin_lu_i <- gsub("100",
                       "bin",
                       gsub(dir_ptos,
                            dir_rest,
                            cem_lu_i
                            )
                       )
      lis_lu_i <- gsub("100",
                       "lis", 
                       gsub(dir_ptos,
                            dir_rest,
                            cem_lu_i
                            )
                       )
      
      # file.copy(from = bin_eq_site_i,
      #           to = bin_eq_i, 
      #           overwrite = FALSE
      #           )
      # file.copy(from = lis_eq_site_i,
      #           to = lis_eq_i, 
      #           overwrite = FALSE
      #           )
      file.copy(from = bin_lu_site_i,
                to = bin_lu_i,
                overwrite = FALSE
                )
      file.copy(from = lis_lu_site_i, 
                to = lis_lu_i,
                overwrite = FALSE
                )
      
      file.remove(bin_eq_site_i,
                  lis_eq_site_i,
                  bin_lu_site_i,
                  lis_lu_site_i,
                  rename_eq_sch,
                  rename_eq_100,
                  rename_lu_sch,
                  rename_lu_100,
                  rename_lu_wth
                  )
    } else {
      print("choose a option to century simulation: eq_cm, lu_cm or lu_cr")
    }
  }

###
#' change files
changFILES <- function(changeFrom, changeTo, l1, l2){
  newFile <- readLines(changeFrom)
  newFile[l1:l2] <- changeTo
  write.table(newFile, changeFrom, quote = FALSE, row.names = FALSE, col.names = FALSE)
}

###
#'alterar parâmetros nos arquivos 100 por linhas
changFILES_linhaI <- function(changeFrom, changeTo, linha){
  newFile <- readLines(changeFrom)
  newFile[linha] <- changeTo
  write.table(newFile, changeFrom, quote = FALSE, row.names = FALSE, col.names = FALSE)
}

###
#' gerar o wth
createCenturyWth <- function(x) {
  if(!ncol(x) == 5){
    print("The data frame must have five columns")
  } else{
    climateData = x
    #reshape data (long to wide)
    reshapeClimateData = as.data.frame(
      t(
        stats::reshape(climateData,
                       idvar = c("mes"),
                       v.names = c( "prec", "tmin", "tmax"),
                       timevar = "ano", direction = "wide")
      )
    )
    names(reshapeClimateData) = 1:12
    reshapeClimateData$Var = substr(rownames(reshapeClimateData), 1, 4)
    reshapeClimateData$ano = substr(rownames(reshapeClimateData), 6, 9)
    reshapeClimateData = reshapeClimateData[-1, c(13:14,1:12)]
    # Format rows output to the same number of character
    rowsClimateData = list()
    for (i in 1:nrow(reshapeClimateData))
    {
      linha = reshapeClimateData[i,]
      v_linha = as.vector(linha)
      v_linha[,3:14] = sprintf("%.2f", round(v_linha[,3:14], 2))
      v_linha = as.vector(as.character(v_linha))
      for (j in 3:14) {
        v_linha[j] = ifelse(nchar(v_linha[j]) < 5, paste0(" ", v_linha[j]), v_linha[j] )
      }
      v_linha = paste(v_linha, collapse = "  ")
      rowsClimateData[[i]] = v_linha
    }
    dfClimateData = do.call("rbind", rowsClimateData)
    return(dfClimateData)
  }
}
####################################################
#####################################################################
