installNetMHCPan4.1 <- function(file = NULL , data = NULL, dir = ".", version = "4.1"){
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
    stop("Only linux installation")
  }

  # file <- "/media/elmer/141f3650-b135-4c9d-a497-9f1ee77c14f5/home/Elmer/Inmuno/netMHCpan-4.1b.Linux.tar.gz"
  # dir <- "/media/elmer/141f3650-b135-4c9d-a497-9f1ee77c14f5/home/Elmer/Inmuno"
  .netMHCpan.file <- basename(file)

  files.in.tar <- untar(file, list = TRUE)
  readme.file <- files.in.tar[stringr::str_detect(files.in.tar,"readme")]
  software <- files.in.tar[1]
  sf.info <- .SoftwareInfo(name = "netMHCpan", masterDirectory = dir, softwareDirectory = software,version = "4.1")

  cat(paste("\nUncompressing files at:", sf.info$masterDirectory))
  untar(file, exdir = sf.info$masterDirectory)

  #reading readme file to look for data
  readme.f <- readLines(file.path(dir,readme.file))
  data.url <- stringr::str_remove_all(readme.f[which(stringr::str_detect(readme.f,"https"))]," ")
  if(length(data.url)<1){
    cat("data URL not found, pls check the readme file and install data file")
  }

  oldp <- getwd()
  setwd(sf.info$softwareDirectory)

  cat(paste("\nDownloading", basename(data.url)," file in:", getwd(),"\n.... This may take time!!!...\n"))

  download.file(data.url, destfile = basename(data.url), method = "wget")
  if(file.exists(basename(data.url))){
      untar(basename(data.url))
    }else{
      stop("Fail download")
    }

  cat(paste("\nEditing ", "netMHCpan"))
  .EditTCSHfile.v4.1(file = file.path(dir,paste(files.in.tar[1],"netMHCpan",sep="")))
  .TestCode(dir = file.path(dir,files.in.tar[1]))

  saveRDS(file.path(dir,files.in.tar[1]), file = file.path(.libPaths()[1],"configIniRAPInetMHCpan.RDS"))
}



# Internal function for config edition
.EditTCSHfile.v4.1 <- function(file.info){
  if(file.exists(file.info$fullPath)==FALSE){
    stop(paste0("ERROR: ejectution file", file.info$Name, "not found")
  }

  # file <- file.path(dir,paste(files.in.tar[1],"netMHCpan",sep=""))
  rl <- readLines(file.info$softwareDirectory)

  id<- which(stringr::str_detect(rl, "setenv\tNMHOME"))
  rl[id] <- paste("setenv\tNMHOME\t",file.info$softwareDirectory,sep="")
  id<- which(stringr::str_detect(rl, "scratch"))
  if(length(id)==0) {
    id <- which(stringr::str_detect(rl, "\tsetenv  TMPDIR  /tmp"  ))
    rl[id] <- paste0("\tsetenv  TMPDIR  ",file.info$softwareDirectory, "tmp"))
  }else{
    rl[id] <- str_replace(rl[id],"/scratch", file.path(dirname(file), "tmp"))
  }
  dir.create(file.path(file.info$softwareDirectory, "tmp"))
  writeLines(text=rl, con = file.info$fullPath)
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
# Internal function for testing installation
.TestCode <- function(dir, software){
  if(str_detect(dir,"MHCII")==TRUE){
    res <- system2(paste(dir,"/netMHCIIpan",sep=""), args = "-f test/example.pep -inptype 1 -a DRB1_0101 -v",
                   stdout = TRUE)
    if(any(res == "   1     DRB1_0101        AAAGAEAGKATTE    1   AAGAEAGKA     0.547        Sequence      0.001552    67.50    0.000       ")){
      cat("\nnetMHCIIpan Installation OK")
    }else{
      stop("Error, installation went wrong")
    }
  }else{
    res <- paste0(dir,"/",software,"netMHCpan",sep=""), args = "-p test/test.pep", stdout = TRUE)
    if(any(res == "   1 HLA-A*02:01      AAAWYLWEV AAAWYLWEV  0  0  0  0  0    AAAWYLWEV         PEPLIST 0.4403830    0.472  0.74258 <= SB")){
      cat("\n netMHCpan Installation OK\n")
    }else{
      stop("Error, installation went wrong\n")
    }
  }

}


#' .SoftwareInfo
#' internal function that build the diretcory information about installed software
#'
.SoftwareInfo <- function(name, masterDirectory, softwareDirectory,version){
  name <- stringr::str_remove_all(name,"/")
  ret <- list(Name=name,
              masterDirectory = normalizePath(masterDirectory),
              softwareDirectory = normalizePath(file.path(masterDirectory,softwareDirectory)),
              fullPath = normalizePath(file.path(masterDirectory,softwareDirectory,name)),
              version = version)

  class(ret) <- c("SoftwareInfo",class(ret))
  return(ret)
}
