
library(stringr)
library(parallel)
library(BiocParallel)
library(seqinr)
# https://services.healthtech.dtu.dk/download/2ff9ce14-4376-453f-a994-ec54ebb8b247/netMHCpan-4.0a.Linux.tar.gz

.netMHCpan.file <- NULL
#  installNetMHCPan
#'
#' Painless instalation tool for netMHCpan in linux and R
#'
#'
#' @param file NULL (default) or character with the netMHCpanXX.Linux.tar.gz file. If NULL, a file selection dialog window will be opened.
#' @param data NULL(default) or character with the path of the data.Linux.tar.gz data file. If NULL it will be automatically downloaded from the netMHCpan server (it may take time)
#' @param dir  character  direrctory of dir the software will be installed
#'
#' @details install the netMHCpan software in your computer and allows suing it trhough R.
#'  as a results it will cread an RDS file named which is used to run the rest of the function
#'  configIniRAPInetMHCpan.RDS PLEASE DO NOT DELET IT
#'
#' In this way, it may be run in parallel to speed up peptides sercehing trhopugh the whole HLA set.
#' Is succesfull instalation , an output displaying the following information shoule be seen on screen.
#'
#' @export
#'
#' @return
#' if instalation succeed, the following message is printed "netMHCpan Instaltion OK
#'

installNetMHCPan <- function(file = NULL , data = NULL, dir){
  if(is.null(file)){
    file <- rstudioapi::selectFile()

  }
  if(str_detect(file, "MHCII")==TRUE){
    stop("\nAre you trying to install netMHCIIpan ?? please verify the imput file")
  }
  if(file.exists(file)==FALSE){
    stop("File not found")
  }
  if(!str_detect(file,"Linux")){
    stop("Only linux instalation")
  }

  .netMHCpan.file <- basename(file)

  files.in.tar <- untar(file, list = TRUE)
  cat(paste("\nUncompressing files at:", dir))
  untar(file, exdir = dir)

  oldp <- getwd()
  setwd(file.path(dir,files.in.tar[1]))
  if(missing(data) | is.null(data)){
    cat(paste("\nDownloading file in:", getwd(),"\n.... This may take time!!!...\n"))
    url.dire.net <- str_replace(files.in.tar[1],"net","Net")
    url.dir  <- paste("http://www.cbs.dtu.dk/services/", url.dire.net,"data.Linux.tar.gz",sep="")

    download.file(url.dir, destfile = basename(url.dir), method = "wget")
    if(file.exists("data.Linux.tar.gz")){
      untar("data.Linux.tar.gz")
    }else {
      stop("Fail download")
    }
  }else{
    cat(paste("\nUncompressing data in :", getwd(),"\n"))
    if(str_detect(data, "data.Linux")==FALSE) stop("error in data file name")
    untar(data)
  }
  cat(paste("\nEditing ", "netMHCpan"))
  .EditTCSHfile(file = file.path(dir,paste(files.in.tar[1],"netMHCpan",sep="")))
  .TestCode(dir = file.path(dir,files.in.tar[1]))

  saveRDS(file.path(dir,files.in.tar[1]), file = file.path(.libPaths()[1],"configIniRAPInetMHCpan.RDS"))
}

# Internal function for config edition
.EditTCSHfile <- function(file){
  rl <- readLines(file)

  id<- which(str_detect(rl, "setenv\tNMHOME"))
  rl[id] <- paste("setenv\tNMHOME\t",dirname(file),sep="")
  id<- which(str_detect(rl, "scratch"))
  if(length(id)==0) {
    id <- which(str_detect(rl, "\tsetenv  TMPDIR  /tmp/"  ))
    rl[id] <- str_replace(rl[id],"/tmp/", file.path(dirname(file), "tmp"))
  }else{
    rl[id] <- str_replace(rl[id],"/scratch", file.path(dirname(file), "tmp"))
  }
  dir.create(file.path(dirname(file), "tmp"))

  writeLines(text=rl, con = file)
}
# Internal function for testing instalation
.TestCode <- function(dir){
  if(str_detect(dir,"MHCII")==TRUE){
    res <- system2(paste(dir,"/netMHCIIpan",sep=""), args = "-f test/example.pep -inptype 1 -a DRB1_0101 -v",
                   stdout = TRUE)
    if(any(res == "   1     DRB1_0101        AAAGAEAGKATTE    1   AAGAEAGKA     0.547        Sequence      0.001552    67.50    0.000       ")){
      cat("\nnetMHCIIpan Instalation OK")
    }else{
      stop("Error, instalation went wrong")
    }
  }else{
    res <- system2(paste(dir,"/netMHCpan",sep=""), args = "-p test/test.pep", stdout = TRUE)
    if(any(res == "    1  HLA-A*02:01       AAAWYLWEV  AAAWYLWEV  0  0  0  0  0    AAAWYLWEV         PEPLIST 0.4024040  0.7289  0.7426 <= WB")){
      cat("\n netMHCpan Instalation OK\n")
    }else{
      stop("Error, instalation went wrong\n")
    }
  }

}
.GetPath <- function(mchI = TRUE){
  if(mchI){
    if(file.exists(file.path(.libPaths()[1],"configIniRAPInetMHCpan.RDS"))){
      return(readRDS(file.path(.libPaths()[1],"configIniRAPInetMHCpan.RDS")))
    }else{
      stop("ERROR: RAPInetMHCpan conif file no found, probably not installed software")
    }

  }else{
    if(file.exists(file.path(.libPaths()[1],"configIniRAPInetMHCIIpan.RDS"))){
      return(readRDS(file.path(.libPaths()[1],"configIniRAPInetMHCIIpan.RDS")))
    }else{
      stop("ERROR: RAPInetMHCIIpan conif file no found, probably not installed software")
    }

  }
}

#  installNetMHCIIPan
#'
#' Painless instalation tool for netMHCIIpan in linux and R
#'
#'
#' @param file NULL (default) or character with the netMHCIIpanXX.Linux.tar.gz file. If NULL,
#' a file selection dialog window will be opened.
#' @param data NULL(default) or character with the path of the data.Linux.tar.gz data file.
#' If NULL it will be automatically downloaded from the netMHCIIpan server (it may take time)
#' @param dir  character. dir the woftware will be installed
#'
#' @details install the netMHCIIpan software in your computer and allows suing it trhough R
#'  as a results it will cread an RDS file named which is used to run the rest of the function
#'  configIniRAPInetMHCIIpan.RDS PLEASE DO NOT DELET IT
#'
#' In this way, it may be run in parallel to speed up peptides sercehing trhopugh the whole HLA set.
#' Is succesfull instalation , an output displaying the following information shoule be seen on screen.
#' .....
#' @export
#'
#' @return
#' None.
#'
#' @examples
#' ## see...

installNetMHCIIPan <- function(file = NULL , data = NULL, dir ){
  if(is.null(file)){
    file <- rstudioapi::selectFile()

  }
  if(str_detect(file, "MHCII")==FALSE){
    stop("The file name should have MHCII")
  }
  if(file.exists(file)==FALSE){
    stop("File not found")
  }
  if(!str_detect(file,"Linux")){
    stop("Only linux instalation")
  }

  .netMHCpan.file <- basename(file)

  files.in.tar <- untar(file, list = TRUE)
  cat(paste("\nUncompressing files at:", dir))
  untar(file, exdir = dir)

  oldp <- getwd()
  setwd(file.path(dir,files.in.tar[1]))
  if(missing(data) | is.null(data)){
    cat(paste("\nDownloading file in:", getwd(),"\n.... This may take time!!!...\n"))
    url.dire.net <- str_replace(files.in.tar[1],"net","Net")
    url.dir  <- paste("http://www.cbs.dtu.dk/services/", url.dire.net,"data.tar.gz",sep="")

    download.file(url.dir, destfile = basename(url.dir), method = "wget")
    if(file.exists("data.tar.gz")){
      untar("data.tar.gz")
    }else {
      stop("Fail download")
    }
  }else{
    cat(paste("\nUncompressing data in :", getwd(),"\n"))
    untar(data)
  }
  cat(paste("\nEditing ", "netMHCIIpan"))
  .EditTCSHfile(file = file.path(dir,files.in.tar[1],"netMHCIIpan"))
  .TestCode(dir = file.path(dir,files.in.tar[1]))

  saveRDS(file.path(dir,files.in.tar[1]), file = file.path(.libPaths()[1],"configIniRAPInetMHCIIpan.RDS"))
}

