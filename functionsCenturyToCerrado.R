#####################################################################
#####################################################################
#'funcoes consolidadas para processamento no grid por blocos
#'

getReferenceRaster <- function(inputDir, pattern) {
  
  imageFiles = Sys.glob(file.path(inputDir, pattern))

  rasterFile <- raster(imageFiles[1])
  
  rasterFile
}

getBlocksOffset <- function(colOffset, rowOffset, nBlock, nColBlock = NULL, nRowBlock = NULL) {
  if(!is.null(nColBlock)){
    print("blocos por colunas")
    #blocks by col
    blocksOffset <- data.frame(iBlock = 1, colOffset, rowOffset)
    for(iBlock in 2:nBlock){
      blockOffset_i <- c(iBlock, (colOffset + ((iBlock-1) * nColBlock)), rowOffset)
      blocksOffset <-rbind(blocksOffset, blockOffset_i)
    }
    return(blocksOffset)
  } else if(!is.null(nRowBlock)){
    print("blocos por linhas")
    #blocks by col
    blocksOffset <- data.frame(iBlock = 1, colOffset, rowOffset)
    for(iBlock in 2:nBlock){
      blockOffset_i <- c(iBlock, colOffset, (rowOffset + ((iBlock-1) * nRowBlock)))
      blocksOffset <-rbind(blocksOffset, blockOffset_i)
    }
    return(blocksOffset)
  } else {
    print("O numero de linhas ou colunas por blocks nao informado")
    
  }
}

getOutputExtent <- function(referenceRaster, blockOffset, blockSize) {
  extentBlock <- extent(referenceRaster, blockOffset[2], c((blockOffset[2]-1) + blockSize[2]),
                        blockOffset[1], c((blockOffset[1]-1) + blockSize[1]))
  return(extentBlock)
}

readBlockImages = function(inputDir, outputExtent, pattern) {
  
  imageFiles <- mixedsort(Sys.glob(file.path(inputDir, pattern)))
#   print(imageFiles)
  
  rasterBlock_img1 <- (crop(raster(imageFiles[1]), outputExtent))
  
  imageDataList <- data.frame(cellNumber = 1:ncell(rasterBlock_img1))
  imageDataList[,2:(length(imageFiles)+1)] <- NA
  names(imageDataList)[2:ncol(imageDataList)] <- paste0('band',1:(ncol(imageDataList)-1))
  
  imageDataList[,2:ncol(imageDataList)] <- lapply(imageFiles, function(x){crop(raster(x), outputExtent)[]})
  
  return(imageDataList[,-1]) #nao gravar cellNumber
}

saveImage <- function(outputData, outputfile, blockOffset, outputExtent, referenceRaster) {
  
  outputfile = paste0(outputfile, 'rowcolOffset_', blockOffset[2], '_', blockOffset[1], '.tif') ###
  
  outputRaster <- crop(referenceRaster, outputExtent)
  outputRaster[] <- outputData
  
  writeRaster(outputRaster, filename=outputfile, overwrite=TRUE)
}


#####################################################################
#####################################################################
#'CENTURY Soil Organic Matter Model Environment
#'funções par rodar o modelo century para o bioma cerrado
runCenturyCerrado <- function(pixel){
  
  pixel <- as.numeric(pixel)
  
  if(is.na(pixel[2])){
    OUT <- as.numeric(rep(NA, 960))
    
  } else if(!length(pixel) == 1447){
    print("o comprimento do pixel deve ser 1375")
  } else{
    STi <- Sys.time()    
    
    DTym <- seq.Date(from = ymd('1980-01-01'), 
                     to = ymd('2019-12-01'), 
                     by = 'month')
    
    cellNumber <- pixel[1]
    
    
    ###
    ###
    #'criar arquivo wth
    climateData <- data.frame(
      t(
        rbind(pixel[8:487]/10,
              pixel[488:967],
              pixel[968:1447])
      )
    )
    names(climateData) <- c('prec', 'tmin', 'tmax')
    climateData$ano = year(DTym)
    climateData$mes = month(DTym)
    
    #reshape data (long to wide)
    reshapeClimateData = as.data.frame(
      t(
        stats::reshape(climateData,
                       idvar = c("mes"),
                       v.names = c("prec", "tmin", "tmax"),
                       timevar = "ano", 
                       direction = "wide")
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
        v_linha[j] = ifelse(nchar(v_linha[j]) < 5, 
                            paste0(" ", v_linha[j]), 
                            v_linha[j] )
      }
      v_linha = paste(v_linha, collapse = "  ")
      rowsClimateData[[i]] = v_linha
    }
    dfClimateData = do.call("rbind", rowsClimateData)
    write.table(dfClimateData, 
                file = paste0("lu_site_", cellNumber, ".wth"), 
                row.names = FALSE, 
                col.names = FALSE, 
                sep = "  ", 
                quote = FALSE)
    ###
    ###
    #'criar arquivo sch e 100
    meanCD <- doBy::summaryBy(prec + tmin + tmax ~ mes, data = climateData)
    meanCD[,2:4] <- round(meanCD[,2:4] + 0.00001, 5)
    names(meanCD) <- c('mes', 'prec', 'tmin', 'tmax')
    
    ###
    ###
    #' prepare file land use site.100
    lu_site_100 <- readLines('template/Lu_site.100')
    lu_site_100[1:15]
    
    #' ppt
    lu_site_100[3] <- gsub('0.000000', meanCD[1, 'prec'], lu_site_100[3])
    lu_site_100[4] <- gsub('0.000000', meanCD[2, 'prec'], lu_site_100[4])
    lu_site_100[5] <- gsub('0.000000', meanCD[3, 'prec'], lu_site_100[5])
    lu_site_100[6] <- gsub('0.000000', meanCD[4, 'prec'], lu_site_100[6])
    lu_site_100[7] <- gsub('0.000000', meanCD[5, 'prec'], lu_site_100[7])
    lu_site_100[8] <- gsub('0.000000', meanCD[6, 'prec'], lu_site_100[8])
    lu_site_100[9] <- gsub('0.000000', meanCD[7, 'prec'], lu_site_100[9])
    lu_site_100[10] <- gsub('0.000000', meanCD[8, 'prec'], lu_site_100[10])
    lu_site_100[11] <- gsub('0.000000', meanCD[9, 'prec'], lu_site_100[11])
    lu_site_100[12] <- gsub('0.000000', meanCD[10, 'prec'], lu_site_100[12])
    lu_site_100[13] <- gsub('0.000000', meanCD[11, 'prec'], lu_site_100[13])
    lu_site_100[14] <- gsub('0.000000', meanCD[12, 'prec'], lu_site_100[14])
    
    #' tmmn
    lu_site_100[39] <- gsub('0.000000', meanCD[1, 'tmin'], lu_site_100[39])
    lu_site_100[40] <- gsub('0.000000', meanCD[2, 'tmin'], lu_site_100[40])
    lu_site_100[41] <- gsub('0.000000', meanCD[3, 'tmin'], lu_site_100[41])
    lu_site_100[42] <- gsub('0.000000', meanCD[4, 'tmin'], lu_site_100[42])
    lu_site_100[43] <- gsub('0.000000', meanCD[5, 'tmin'], lu_site_100[43])
    lu_site_100[44] <- gsub('0.000000', meanCD[6, 'tmin'], lu_site_100[44])
    lu_site_100[45] <- gsub('0.000000', meanCD[7, 'tmin'], lu_site_100[45])
    lu_site_100[46] <- gsub('0.000000', meanCD[8, 'tmin'], lu_site_100[46])
    lu_site_100[47] <- gsub('0.000000', meanCD[9, 'tmin'], lu_site_100[47])
    lu_site_100[48] <- gsub('0.000000', meanCD[10, 'tmin'], lu_site_100[48])
    lu_site_100[49] <- gsub('0.000000', meanCD[11, 'tmin'], lu_site_100[49])
    lu_site_100[50] <- gsub('0.000000', meanCD[12, 'tmin'], lu_site_100[50])
    
    #' tmmx
    lu_site_100[51] <- gsub('0.000000', meanCD[1, 'tmax'], lu_site_100[51])
    lu_site_100[52] <- gsub('0.000000', meanCD[2, 'tmax'], lu_site_100[52])
    lu_site_100[53] <- gsub('0.000000', meanCD[3, 'tmax'], lu_site_100[53])
    lu_site_100[54] <- gsub('0.000000', meanCD[4, 'tmax'], lu_site_100[54])
    lu_site_100[55] <- gsub('0.000000', meanCD[5, 'tmax'], lu_site_100[55])
    lu_site_100[56] <- gsub('0.000000', meanCD[6, 'tmax'], lu_site_100[56])
    lu_site_100[57] <- gsub('0.000000', meanCD[7, 'tmax'], lu_site_100[57])
    lu_site_100[58] <- gsub('0.000000', meanCD[8, 'tmax'], lu_site_100[58])
    lu_site_100[59] <- gsub('0.000000', meanCD[9, 'tmax'], lu_site_100[59])
    lu_site_100[60] <- gsub('0.000000', meanCD[10, 'tmax'], lu_site_100[60])
    lu_site_100[61] <- gsub('0.000000', meanCD[11, 'tmax'], lu_site_100[61])
    lu_site_100[62] <- gsub('0.000000', meanCD[12, 'tmax'], lu_site_100[62])
    
    #' soil
    soilData <- pixel[3:7]
    soilData[1:3] <- round((soilData[1:3]/100) + 0.00001, 5)
    soilData[4] <- round((soilData[4]/1000) + 0.00001, 5)
    soilData[5] <- round((soilData[5]/10) + 0.00001, 5)
    
    lu_site_100[68] <- gsub('0.00000', soilData[1], lu_site_100[68])
    lu_site_100[69] <- gsub('0.00000', soilData[2], lu_site_100[69])
    lu_site_100[70] <- gsub('0.00000', soilData[3], lu_site_100[70])
    lu_site_100[72] <- gsub('1.00000', soilData[4], lu_site_100[72])
    lu_site_100[101] <- gsub('0.00000', soilData[5], lu_site_100[101])
    
    lu_site_100_name <- paste0("lu_site_", cellNumber, ".100")
    write.table(lu_site_100,  
                file = lu_site_100_name, 
                quote = FALSE, 
                row.names = FALSE, 
                col.names = FALSE)
    
    #'prepare file land use site.sch
    lu_site_sch <-  readLines('template/Lu_site.sch')
    lu_site_sch_name <- paste0("lu_site_", cellNumber, ".sch")
    lu_site_sch[3] <- gsub('lu_site.100', lu_site_100_name, lu_site_sch[3])
    
    # ano_conv <- 1984 + pixel[2]  # ano de convers?o para pastagem
    ano_conv <- pixel[2]  # ano de convers?o para pastagem
    lu_site_sch[1] <- gsub('1982', ano_conv - 3, lu_site_sch[1])
    lu_site_sch[19] <- gsub('1983', ano_conv - 2, lu_site_sch[19])
    lu_site_sch[21] <- gsub('1982', ano_conv - 3, lu_site_sch[21])
    lu_site_sch[49] <- gsub('1984', ano_conv - 1, lu_site_sch[49])
    lu_site_sch[51] <- gsub('1984', ano_conv - 1, lu_site_sch[51])
    lu_site_sch[65] <- gsub('1985', ano_conv, lu_site_sch[65])
    
    lu_site_sch[69] <- gsub('lu_site.wth', 
                            paste0("lu_site_", cellNumber, ".wth"), 
                            lu_site_sch[69])
    
    write.table(lu_site_sch,  
                file = lu_site_sch_name, 
                quote = FALSE, 
                row.names = FALSE, 
                col.names = FALSE)
    
    ###
    ###
    #'prepare file equilibrio site.100 e sch
    eq_site_100 <- lu_site_100
    eq_site_100_name <- paste0("eq_site_", cellNumber, ".100")
    
    write.table(eq_site_100,  
                file = eq_site_100_name, 
                quote = FALSE, 
                row.names = FALSE, 
                col.names = FALSE)
    
    #'prepare file equilibrio site.100 e sch
    eq_site_sch <-  readLines('template/Eq_site.sch')
    eq_site_sch[3] <- gsub('eq_site.100', eq_site_100_name, eq_site_sch[3])
    eq_site_sch_name <- paste0("eq_site_", cellNumber, ".sch")
    
    write.table(eq_site_sch,  
                file = eq_site_sch_name, 
                quote = FALSE, 
                row.names = FALSE, 
                col.names = FALSE)
    
    ####
    ####
    ##'rodar o modelo century
    
    equilibrio <- paste0('century -s eq_site_', 
                         cellNumber, 
                         ' -n eq_site_', 
                         cellNumber)
    system(equilibrio)
    
    land_use <- paste0('century -s lu_site_', 
                       cellNumber, 
                       ' -n result/lu_site_', 
                       cellNumber, 
                       ' -e eq_site_', 
                       cellNumber)
    system(land_use)
    
    lis_file <- paste0('list100 result/lu_site_', 
                       cellNumber, 
                       ' result/lu_site_', 
                       cellNumber, ' output.txt')
    system(lis_file)
    
    ###
    ###
    #' vari?veis a ser utilizadas
    #' checar se o arquivo tem 240 linhas, caso n?o, acresntar NA no in?cio'
    lis_lu <- read.table(paste0("result/lu_site_", cellNumber, ".lis"), h = T)
    lis_lu <- lis_lu[lis_lu$time > 2000.01 & lis_lu$time < 2020.01, ]
    
    if(nrow(lis_lu) < 240){
      VecNA <- as.numeric(rep(NA, (240 - nrow(lis_lu))))
      aglivc <- as.numeric(c(VecNA, lis_lu$aglivc))
      bglivc <- as.numeric(c(VecNA, lis_lu$bglivc))
      somsc <- as.numeric(c(VecNA, lis_lu$somsc))
      stdedc <- as.numeric(c(VecNA, lis_lu$stdedc))    
    } else {
      aglivc <- as.numeric(lis_lu$aglivc)
      bglivc <- as.numeric(lis_lu$bglivc)
      somsc <- as.numeric(lis_lu$somsc)
      stdedc <- as.numeric(lis_lu$stdedc)
    }
    
    OUT <- as.numeric(c(aglivc, bglivc, somsc, stdedc))
    
    ###
    ###
    #' remove files
    Sys.sleep(1)
    file.remove(paste0("eq_site_", cellNumber, ".100"),
                paste0("eq_site_", cellNumber, ".sch"),
                paste0("eq_site_", cellNumber, ".bin"),
                paste0("lu_site_", cellNumber, ".sch"),
                paste0("lu_site_", cellNumber, ".100"),
                # paste0("result/lu_site_", cellNumber, ".bin"),
                # paste0("result/lu_site_", cellNumber, ".lis"),
                paste0("lu_site_", cellNumber, ".wth")
    )
    # print(Sys.time() - STi)
  }
  
  return(OUT)
}


#####################################################################
#####################################################################

procRasterBlocks <- function(inputDir, outputDir, fileToExtent, pattern, nColBlock = NULL, nRowBlock = NULL, ncores = NULL) {
    STTot <- Sys.time()
    
    DTym <- seq.Date(from = ymd('1980-01-01'), to = ymd('2019-12-01'), by = 'month') # col names
    
    ncores <- ifelse(is.null(ncores), detectCores(), ncores)
    print(ncores)
    
    clusterPool <- makeCluster(ncores)
    clusterEvalQ(clusterPool, {
        require(doBy)
        require(dplyr)
        require(lubridate)
    })
    
    #preparing blocks
    referenceRaster <- getReferenceRaster(inputDir, pattern)
    
    rowMin <- rowFromY(referenceRaster, ymax(fileToExtent))
    rowMax <- rowFromY(referenceRaster, ymin(fileToExtent))
    colMin <- colFromX(referenceRaster, xmin(fileToExtent))
    colMax <- colFromX(referenceRaster, xmax(fileToExtent))
    
    nRow <- rowMax - rowMin
    nCol <- colMax - colMin
    
    colOffset <- colMin
    rowOffset <- rowMin
    nRowBlock <- ifelse(is.null(nRowBlock), nRow, nRowBlock)
    nColBlock <- ifelse(is.null(nColBlock), nCol, nColBlock)
    
    nBlock <- ifelse(is.null(nRowBlock), 
                     ceiling(nCol / nColBlock), 
                     ceiling(nRow / nRowBlock) )
    print(paste0("Numero de blocos = ", nBlock))
    
    iBlockSize <- c(nColBlock, nRowBlock)
    blocksOffset <- getBlocksOffset(colOffset, rowOffset, nBlock, nColBlock = NULL, nRowBlock)
    
    #process data by blocks
    for (bloco in blocFirst:blocEnd){
        print(paste0("executing block = ", bloco))
        
        iBlockOffset <- as.numeric(blocksOffset[bloco, 2:3])
        print(iBlockOffset)
        
        STBloco <- Sys.time()
        outputExtent <- getOutputExtent(referenceRaster, iBlockOffset, iBlockSize)
        
        ###
        ###
        #'ler as cinco bases de dados (mapa de pastagem, solo, precipitacao, temp. maxima e temp. minima)
        #'read data of the block
        STRead <- Sys.time()
        
        #'mapa de pastagem
        blockData_pasture = readBlockImages(paste0(inputDir,"pasture_map"), outputExtent, pattern)
        names(blockData_pasture) <- c('cellNumber', 'pastMask')
        
        #'solos
        blockData_soil = readBlockImages(paste0(inputDir,"soil_grid_br_1km"), outputExtent, pattern)
        names(blockData_soil) <- c('sand', 'silte', 'clay', 'bkrd', 'ph')
        
        #'precipitacao
        blockData_prec = readBlockImages(paste0(inputDir,"pr_1km"), outputExtent, pattern)
        names(blockData_prec) <- DTym
        
        #'temperatura minima
        blockData_tmmn = readBlockImages(paste0(inputDir,"tmmn_1km"), outputExtent, pattern)
        names(blockData_tmmn) <- DTym
        
        #'temperatura maxima
        blockData_tmmx = readBlockImages(paste0(inputDir,"tmmx_1km"), outputExtent, pattern)
        names(blockData_tmmx) <- DTym
        
        #'juntar blocos de ados
        blockData <- cbind(blockData_pasture, blockData_soil, blockData_prec, blockData_tmmn, blockData_tmmx)
        
        #'gravar o bloco de dados
        #write.csv(blockData, file = paste0(inputDir, '/blockdata_', bloco,'.csv'), row.names = F)
        
        totSTRead <- paste0("time to read block ", bloco, " = ", Sys.time() - STRead)
        print(totSTRead)
        
        #run function
        DTym <- seq.Date(from = ymd('2000-01-01'),
                 to = ymd('2019-12-01'), 
                 by = 'month')
        colNames <- c(paste0("aglivc_", DTym),
              paste0("bglivs_", DTym),
              paste0("somsc_", DTym),
              paste0("stdeadc_", DTym)
              )
        STRun <- Sys.time()
        centuryCerradoResult <- as.data.frame(t(parApply(cl = clusterPool, blockData, 1, runCenturyCerrado)))
        names(centuryCerradoResult) <- colNames 
        totSTRun <- paste0("time to run function for block i = ", Sys.time() - STRun)
        print(totSTRun)
        
        #Write results
        for (i in 1:ncol(centuryCerradoResult)) {
            outputfile <- paste0(outputDir, "/", names(centuryCerradoResult)[i], '_block_', bloco, '_')
            outputData <- centuryCerradoResult[, i]
            saveImage(outputData, outputfile, iBlockOffset, outputExtent, referenceRaster)
        }
        totSTBloco <- paste0("time to execute block = ", bloco, " = ", Sys.time() - STBloco)
        print(totSTBloco)
        
        rm(centuryCerradoResult, blockData)
        gc(reset = TRUE)
    }
    print(paste0("time to execute all blocks = ", Sys.time() - STTot))
    stopCluster(clusterPool)
}

#####################################################################
#####################################################################
#'pacotes necessários
suppressWarnings(suppressMessages(library(gtools)))
suppressWarnings(suppressMessages(library(raster)))
suppressWarnings(suppressMessages(library(parallel)))
suppressWarnings(suppressMessages(library(lubridate)))
suppressWarnings(suppressMessages(library(stats)))
suppressWarnings(suppressMessages(library(doBy)))

#' definir pasta century como diretório
setwd("H:\\run_century_goiasvelho/century")
inputDir <- "H:\\run_century_goiasvelho/dados/"
outputDir <- "H:\\run_century_goiasvelho/mu_goias_century_2000_2020_1km"
fileToExtent <- raster("H:\\run_century_goiasvelho/dados/2_mu_goias_first_year_pasture_col5_2017_lapig_1km_mask17.tif")

pattern <- "*.tif"

nColBlock <- NULL
nRowBlock <- 5
ncores <- 5

blocFirst <- as.numeric(1)
blocEnd <- as.numeric(14)

procRasterBlocks(inputDir, outputDir, fileToExtent, pattern, nColBlock = NULL, nRowBlock, ncores = ncores)

#####################################################################
#####################################################################