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
    stop(paste0("ERROR: ejectution file", file.info$Name, "not found"))
  }

  # file <- file.path(dir,paste(files.in.tar[1],"netMHCpan",sep=""))
  rl <- readLines(file.info$softwareDirectory)

  id<- which(stringr::str_detect(rl, "setenv\tNMHOME"))
  rl[id] <- paste("setenv\tNMHOME\t",file.info$softwareDirectory,sep="")
  id<- which(stringr::str_detect(rl, "scratch"))
  if(length(id)==0) {
    id <- which(stringr::str_detect(rl, "\tsetenv  TMPDIR  /tmp"  ))
    rl[id] <- paste0("\tsetenv  TMPDIR  ",file.info$softwareDirectory, "tmp")
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
    res <- system2(paste0(dir,"/",software,"netMHCpan",sep=""), args = "-p test/test.pep", stdout = TRUE)
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


.RunNetMHCPan <- function(seqfile, allele, rthParam = 0.50, rltParam= 2.0, tParam = -99.9002, pLength, fileInfo){
  hla <- allele
  softwarePath <- .GetPath()
  if(is.null(softwarePath) | !dir.exists(softwarePath)){
    stop("ERROR: missing netMHCpan path")
  }
  if(!.CheckAllele(allele)){
    stop(paste(allele, "NOT Found, Please check allele name"))
  }
  if(stringr::str_detect(basename(seqfile),".fasta")){
    datafile <- seqfile
  }else{
    if(stringr::str_detect(basename(seqfile),".pep")){
      datafile <- paste("-p ",seqfile,sep="")
    }else{
      stop("ERROR file should end in fasta or pep")
    }
  }
  if(missing(pLength)){
    long.pep <- paste(8:11,collapse = ",")
  }else{
    long.pep <- pLength
  }

  command <- fileInfo$fullPath

  # command <- str_replace_all(command,"//","/")
  arguments <- c(file = datafile,
                 syn = paste("-syn ",file.path(fileInfo$softwareDirectory,"Linux_x86_64/data/synlist.bin"),sep = ""),
                 tdir = paste("-tdir ", file.path(fileInfo$softwareDirectory,"tmp/netMHCpanXXXXXX"),sep = ""),
                 rdir = paste("-rdir ", file.path(fileInfo$softwareDirectory,"Linux_x86_64"),sep = ""),
                 hlapseudo = paste("-hlapseudo ", file.path(fileInfo$softwareDirectory,"Linux_x86_64/data/MHC_pseudo.dat"),sep = ""),
                 thrfmt = paste("-thrfmt ", file.path(fileInfo$softwareDirectory,"Linux_x86_64/data/threshold/%s.thr.%s"),sep = ""),
                 version = paste("-version ", file.path(fileInfo$softwareDirectory,"Linux_x86_64/data/version"),sep = ""),
                 allname = paste("-allname ", file.path(fileInfo$softwareDirectory,"Linux_x86_64/data/allelenames"),sep = ""),
                 rth = paste("-rth ",rthParam, sep = ""),
                 rlt = paste("-rlt ",rltParam, sep = ""),
                 t = paste("-t ",tParam,sep = ""),
                 v = "-v",
                 BA= "-BA",
                 a = paste("-a", NULL),
                 l= paste("-l ",long.pep))
  nm <- names(arguments)
  arguments <- str_replace_all(arguments,"//","/")
  names(arguments) <- nm
  # res <- unlist(lapply(hla,function(al){
  al <- stringr::str_remove_all(hla,"\\*")
    arguments["a"] <- paste("-a",al)
    # print(arguments["a"])
    s1 <- system2(command = command, stdout = TRUE, args = arguments)
    # print(paste("Longitud seq:",length(s1)))
    # binders <- c(which(str_detect(s1,"SB")),which(str_detect(s1,"WB")))
    # class(s1) <- "RAPIMHC"
    # if(length(binders)>0){
    #   return(s1[binders])
    # }else return(NA)
  # }))
}

Format.Out <- function(resRAPIMHC){
  id.ini <- which(stringr::str_detect(resRAPIMHC, " Pos         MHC        Peptide      Core Of Gp Gl Ip Il") ) + 2
  id.fin <- rev(which(stringr::str_detect(resRAPIMHC, "---------------------------------------------------------------------------------------------------------------------------"  )))[1]-1

  ret <- data.frame(do.call(rbind, lapply(id.ini:id.fin, function(i){

      re <- unlist(stringr::str_split(resRAPIMHC[i]," "))
    re <- re[re != ""]
    re <- re[ 1:16]
  }    )))
  colnames(ret) <- c("Pos","MHC","Peptide","Core","Of","Gp","Gl","Ip","Il","Icore","Identity","Score_EL","%Rank_EL","Score_BA","%Rank_BA","Aff(nM)")
  return(ret)
}

