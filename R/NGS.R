### Configuration files ####

#  .OpenConfigFile
#'
#' internal function to load configuration info.
#'
#'
#' @details It will create a RAPI_Confing.RDS file with software location paths and version
#' for NGS data processing. FastQC, TrimGalore,
#'  PLEASE DO NOT DELET this file
#'
#' @return
#' the configuration list. Each slot holds one software
#'
.OpenConfigFile <- function(){
 if( file.exists(file.path(.libPaths()[1],"RAPI_Config.RDS")) == TRUE ){
   config.list <- readRDS(file.path(.libPaths()[1],"RAPI_Config.RDS"))
 }else{
   config.list <- list()
   config.list$main$path <- file.path(dirname("~/.local/bin"),"bin")
   if(dir.exists(config.list$main$path) == FALSE){
     dir.create(config.list$main$path)
     if(dir.exists(config.list$main$path) == FALSE){
       stop(paste("ERROR: Please verify dir creation permitions at",dirname(config.list$main$path)))
     }
   }

   saveRDS(config.list,file.path(.libPaths()[1],"RAPI_Config.RDS"))
   if( file.exists(file.path(.libPaths()[1],"RAPI_Config.RDS")) == FALSE ){
     stop("ERROR can create ConfigFile")
   }
 }
  return(config.list)
}
#  .SaveConfigFile
#'
#' internal function to save configuration info.
#'
#'
#' @details It will save the RAPI_Confing.RDS file after installation of TrimGalore, FASTQC
#'
.SaveConfigFile <- function(conf){
  if( file.exists(file.path(.libPaths()[1],"RAPI_Config.RDS")) == TRUE ){
    saveRDS(conf,file.path(.libPaths()[1],"RAPI_Config.RDS"))
  }else{
    stop("ERROR CONFIG FILE does NOT exists", )
  }
}

### SOFTWARE Installation ####
#  InstallTrimGalore
#'
#' painless trim_galore installation.
#' please provide the url from https://github.com/FelixKrueger/TrimGalore
#'
#' @param trimTarBall the url of trimgalore, like "https://github.com/FelixKrueger/TrimGalore/archive/0.6.5.tar.gz"
#' @param where (ptional) directory to where the software will be installed. if missing, it will use ~/.local hidden directory
#' @param cutadaptPath the diretory where cutadapt was installed, if missing it assumes ~/.local/bin/cutadapt
#'
#' @export
#' @author Elmer A. Fernández
#'
#' @details It will install TrimGalore, the RAPI_Config.RDS file will be updated in accordance
#'
#' @return
#' if success, the TrimGalore run without inputs and cutadapt location
#'

InstallTrimGalore <- function(trimTarBall , where, cutadaptPath ){
  config.list <- .OpenConfigFile()
  if(length(config.list) == 0){
    if(missing(cutadaptPath)){
      config.list$cutadapt$path <- file.path(dirname("~/.local/bin/cutadapt"),"cutadapt")

      if( any(stringr::str_detect(list.files(dirname(config.list$cutadapt$path)),"cutadapt")) == FALSE){
        stop("ERROR: cutadapt not found, pls verify instalation")
      }
      config.list$cutadapt$version <- as.numeric(system2(command = config.list$cutadapt$path, args = "--version", wait = TRUE, stdout = TRUE))
      print(config.list)
    }else{
      config.list$cutadapt <- file.path(dirname(cutadaptPath),"cutadapt")
      if(any(stringr::str_detect(list.files(config.list$cutadapt),"cutadapt"))==FALSE){
        stop("ERROR: cutadapt not found, pls verify instalation")
      }
    }
  }
  ##lISTO HASTA ACA CUTADAPT
  if(missing(where)){
    config.list$trimgalore$path <- dirname(config.list$cutadapt$path)
  }else{
    config.list$trimgalore$path <- where
  }
  if(stringr::str_detect(trimTarBall, "https")){##URL, download
    download.file(url = trimTarBall, method = "wget",
                  destfile = file.path(config.list$trimgalore$path,"trim_galore.tar.gz"))

    untar(file.path(config.list$trimgalore$path,"trim_galore.tar.gz"), exdir = config.list$trimgalore$path)
    dlist <- list.dirs(config.list$trimgalore$path, recursive = FALSE)
    id<- which(stringr::str_detect(toupper(dlist),"TRIMGALORE"))
    config.list$trimgalore$path <- dlist[[id]]
    config.list$trimgalore$version <- stringr::str_remove(toupper(basename(dlist[[2]])),"TRIMGALORE")
    print(config.list$trimgalore)
    ##Test trimgalore
    system2(command = file.path(config.list$trimgalore$path,"trim_galore"), args = paste("--path_to_cutadapt", config.list$cutadapt$path) )
  }
  .SaveConfigFile(conf = config.list)
}

#  InstallFastQC
#'
#' painless FastQC installation.
#' please provide the url from https://www.bioinformatics.babraham.ac.uk/projects/download.html#fastqc
#'
#' @param fqurl the url of fastqc
#'
#' @export
#' @author Elmer A. Fernández
#'
#' @details It will install FastQC, the RAPI_Config.RDS file will be updated in accordance
#'
#' @return
#' if success, the configuration list file will be printed and the approrpiate fastqc version will appears
#'
InstallFastQC <- function(fqurl){
  # https://www.bioinformatics.babraham.ac.uk/projects/fastqc/fastqc_v0.11.9.zip
  config.list <- .OpenConfigFile()
  download.file(fqurl, method = "wget",
                destfile = file.path(config.list$main$path,"fasqc.zip"))
  unzip(file.path(config.list$main$path,"fasqc.zip"), exdir = config.list$main$path)
  config.list$fastqc$path <- file.path(config.list$main$path,"FastQC")
  system2(command = "ln", arg = c(config.list$fastqc$path, "/usr/local/bin/fastqc"))
  system2(command = "chmod", args = c("755", file.path(config.list$fastqc$path,"fastqc")))
  config.list$fastqc$version <- system2(command = file.path(config.list$fastqc$path,"fastqc"), args = "--version", stdout = TRUE)
  print(config.list$fastqc$version)
  .SaveConfigFile(conf = config.list)
}

###RUN  SOFTWARE ####
#  RunTrimGalore
#'
#' R interface to TrimGalore
#' please see TrimGalore web site at https://github.com/FelixKrueger/TrimGalore/blob/master/Docs/Trim_Galore_User_Guide.md
#' @param file a character vector with the file names or the files names to be processed. The fasts should end in .fastq
#' @param pared (TRUE default) if paired == TRUE, the it will look for the second fastq file for each file. The files are expected to be
#' .fastq and identified as XXXX_1.fastq and their corresponding pair XXXX_2.fastq.
#' @param quality (integer>0) see --quality at TrimGalore web site
#' @param phred33 (logical) if TRUE, then --phred33 ON, else --phred33 OFF (see at TrimGalore web site)
#' @param phred64 (logical) (see --phred64 at TrimGalore web site)
#' @param fasqc (logical) if TRUE will run fastqc in its default version
#' @param max_length (integer) (see --max_length at TrimGalore web site)
#' @param error_rate (float) default 0.1 (see -e option at TrimGalore web site)
#' @param gzip (logical) if TRUE (default) the output files will be zipped (see--gzip and --dont_zip at TrimGalore web site)
#' @param length (integer) default 20 (see --length at TrimaGalore web site)
#' @param maxn (integer) (see --max_n at TrimGalore web site)
#' @param trimn (integer) (see --trim-n at TrimGalore web site)
#' @param outdir (character) if missing the output will be stored at the same directory of the (first) input file. (see --output_dir at TrimGalore web site)
#' @param report_file (logical) (default TRUE) (see --repot_file at TrimGalore web site)
#' @param clip_R1 (integer) (see --clip_R1 at TrimGalore web site)
#' @param clip_R2 (integer) (see --clip_R2 at TrimGalore web site)
#' @param three_prime_clip_R1 (see --three_prime_clip_R1 at TrimGalore web site)
#' @param three_prime_clip_R2 (see --three_prime_clip_R2 at TrimGalore web site)
#' @param retain_unpaired (see --retain_unpaired at TrimGalore web site)
#' @param length_1 (integer) default 35)  (see --length_1 at TrimGalore web site)
#' @param length_2 (integer) default 35) (see --length_2 at TrimGalore web site)
#' @param trim1 (integer) (see --trim1 at TrimGalore web site)
#' @param cores ( integer) (default 4) number of cores or threads to use (see --cores at TrimGalore web site)
#' @param remove_intermediate remove intermediate trimmed files (only fif paired == TRUE)
#'
#' @export
#' @author Elmer A. Fernández
#'
#' @details
#' see output files definitions at TrimGalore web site
#'
#' @return
#' none
#'
RunTrimGalore <- function(file, paired = TRUE, quality = 20, phred33 = TRUE, phred64 = FALSE,
                          fastqc = FALSE, max_length, error_rate = 0.1, gzip=TRUE,
                          length = 20, maxn, trimn, outdir, report_file = TRUE,
                          clip_R1, clip_R2, three_prime_clip_R1, three_prime_clip_R2,
                          retain_unpaired, length_1 = 35, length_2 = 35,
                          trim1, cores =4 , remove_intermediate = paired){
#   --path_to_cutadapt
  config.list <- .OpenConfigFile()
  if(is.null(config.list$cutadapt$pat)){
    stop("\nERROR: cutadapt not installed\n")
  }
  if(is.null(config.list$trimgalore$path)){
    stop("\nERROR: trimgalore not installed\n")
  }

  trim.arguments <- c(paired = ifelse(paired,"--paired", NULL))
  if(quality>0){
    trim.arguments <- c(trim.arguments, quality = paste("--quality",quality))
  }else{
    stop("ERROR, quality > 0")
  }
 if(phred33){
   trim.arguments <- c(trim.arguments, phred33 = "--phred33")
 }

  if(phred64){
    trim.arguments <- c(trim.arguments, phred64 = "--phred64")
  }
  if(fastqc){
    trim.arguments <- c(trim.arguments, fastqc = "--fastqc")
  }
  if(!missing(max_length)){
    trim.arguments <- c(trim.arguments, max_length = paste("--max_length",max_length))
  }
  trim.arguments <- c(trim.arguments, error_rate = paste("-e",error_rate))
  if(gzip){
    trim.arguments <- c(trim.arguments, gzip = "--gzip")
  }else{
    trim.arguments <- c(trim.arguments, dont_gzip = "--dont_gzip")
  }
  trim.arguments <- c(trim.arguments, length = paste("--length",length))
  if(!missing(maxn)){
    trim.arguments <- c(trim.arguments, maxn = paste("--max_n",maxn))
  }
  if(!missing(trimn)){
    trim.arguments <- c(trim.arguments, trimn = paste("--trim-n",trimn))
  }

  if(missing(outdir)==FALSE){
    dir.create(outdir)
    if( dir.exists(outdir) == FALSE){
      stop(paste("ERROR:", outdir, "could not be created" ))
    }else{
      trim.arguments <- c(trim.arguments, output_dir = paste("--output_dir",outdir))
    }
  }else{
    trim.arguments <- c(trim.arguments, output_dir = paste("--output_dir",dirname(file[1])))
  }

  # if(!report_file){
  #   trim.arguments <- c(trim.arguments, "--no_report_file")
  # }
  trimgalore <- file.path(config.list$trimgalore$path,"trim_galore")

  trim.arguments <- c(trim.arguments, paste("--path_to_cutadapt", config.list$cutadapt$path))
  trim.arguments <- c(trim.arguments, cores = paste("--cores", cores))

  if(paired){
    file2 <- file
    file2 <- stringr::str_replace_all(file2,"_1.fastq","_2.fastq")
  }else{
    file2=""
  }
  fex <- file.exists(c(file,file2))
  if(all(fex) == FALSE){
    cat(paste("\n",c(file,file2)[!fex],"\n"))

    stop(paste("\nERROR ----\n",paste(c(file,file2)[!fex],collapse = "\n"), "\nthese files do not exists",sep=""))
  }
  # arguments <- c("--paired",sbj$files, "--cores 2 " ,"--path_to_cutadap ~/.local/bin/cutadapt ", paste("-o ",sbj$subjectDir,sep=""))
  # print(c(trimgalore,c(file, file2, trim.arguments)))
  system2(command = trimgalore, args = c(trim.arguments, file, file2))

  # return(list(trimgalore,c(trim.arguments, file, file2)))

  # system2(command = "/home/elmer/.local/bin/TrimGalore-0.6.5/trim_galore" , args = )
   # if(all(c(remove_intermediate,paired))==TRUE){
   #   f1 <- stringr::str_remove(file,".fasta",_trimmed.fq
   #   file.remove()
   # }
}

#  RunFastQC
#'
#' R interface to FastQC. It requires that FastQC be installed by means if InstallFastQC function
#' path location should be saved in the ConfigFile. Try first CheckFastQC().
#' @param file a character vector with the file names or the files names to be processed. The files should end in .fastq. If paired-ends, (see paired)
#' Only the firs file should be indicated (XXX_1.fastq). The second file (XXX_2.fastq) will be automatically searched.
#' @param paired (TRUE default) if paired == TRUE, the it will look for the second fastq file for each file. The files are expected to be
#' .fastq and identified as XXXX_1.fastq and their corresponding pair XXXX_2.fastq.
#' @param outdir (character) if missing the output will be stored in a directory named FASQC_RAPI
#' @param java (character to  java) Provides the full path to the java binary you want to use to launch fastqc.
#' If not supplied then java is assumed to be in your path.
#' @param min_length Sets an artificial lower limit on the length of the sequence to be shown in the report.
#' @param threads ( integer) (default 4) number of cores or threads to use (see --cores at TrimGalore web site)
#' @param contaminants Specifies a non-default file which contains the list of contaminants to screen overrepresented sequences against.
#' The file must contain sets of named contaminants in the form name[tab]sequence.  Lines prefixed with a hash will be ignored.
#' @param adapters Specifies a non-default file which contains the list of adapter sequences which will be explicity searched against
#' the library. The file must contain sets of named adapters in the form name[tab]sequence.  Lines prefixed with a hash will be ignored
#' @param limits Specifies a non-default file which contains a set of criteria which will be used to determine the warn/error limits for the
#' various modules.  This file can also be used to selectively remove some modules from the output all together.  The format
#' needs to mirror the default limits.txt file found in the Configuration folder.
#' @param kmers (integer) default 7.  Specifies the length of Kmer to look for in the Kmer content module. Specified Kmer length must be between 2 and 10.
#' @export
#' @author Elmer A. Fernández
#'
#' @details
#' the zip and html files for further exploration
#'
#' @return
#' none
#'
RunFastQC <- function(file, paired = TRUE, outdir, java, min_length, threads,
                      contaminants, adapters, limits, kmers ){
  config.list <- .OpenConfigFile()

  if(is.null(config.list$fastqc$path)){
    stop("\nERROR: FASTQC not installed\n")
  }
  PE <- paired
  argument.vector <- NULL
  # argument.vector <- c(outdir = "--outdir ", java = "--java ",
                       # extract = "--extract", min_length = "--min_length",
                       # threads = "--threads", contaminants = "--contaminants",
                       # adapters = "--adapters ", limits = "--limits",
                       # kmers = "--kmers")

  if(missing(outdir)==FALSE){
    dir.create(outdir)
    if( dir.exists(outdir) == FALSE){
      stop(paste("ERROR:", outdir, "could not be created" ))
    }else{
      argument.vector <- c(argument.vector, outdir = paste("--outdir",outdir))
    }
  }else{
    dir.create(file.path(dirname(file[1]),"FastQC_RAPI"))
    if(!dir.exists(file.path(dirname(file[1]),"FastQC_RAPI"))){
      stop("Problems creating the FASTQC directory")
    }
    argument.vector <- c(argument.vector, outdir = paste("--outdir",file.path(dirname(file[1]),"FastQC_RAPI")))
  }

  if(!missing(java)){
    argument.vector <- c(argument.vector, java = paste("--java",java))
  }

  if(!missing(min_length)){
    argument.vector <- c(argument.vector, min_length = paste("--min_length",min_length))
  }

  if(!missing(threads)){
    nc <- parallel::detectCores()
    if(threads > length(file)*ifelse(PE,2,1)){
      threads <- length(file)*ifelse(PE,2,1)
    }
    if(threads > nc){
      threads <- nc
    }
    argument.vector <- c(argument.vector, threads = paste("--threads",threads))
  }else{
    argument.vector <- c(argument.vector, threads = paste("--threads",1))
  }
  if(PE){
    file2 <- file
    file2 <- stringr::str_replace_all(file2,"_1.fastq","_2.fastq")
  }else{
    file2=""
  }
  if(!missing(kmers)){
    if(2 <= kmers & kmers <=10){
      argument.vector <- c(argument.vector, kmers = paste("--kmers",kmers))
    }else{

      stop(paste("ERROR: 2 <= kmears <=10, you set:", kmers))
    }
  }else{
    argument.vector <- c(argument.vector, kmers = paste("--kmers",7))
  }
  fex <- file.exists(c(file,file2))
  if(all(fex) == FALSE){
    cat(paste("\n",c(file,file2)[!fex],"\n"))

    stop(paste("\nERROR ----\n",paste(c(file,file2)[!fex],collapse = "\n"), "\nthese files do not exists",sep=""))
  }
  # print(file)
  # print(file2)
  # print(argument.vector)
  system2(command = file.path(config.list$fastqc$path,"fastqc"), args = c(file, file2,argument.vector))
}


#  CheckFastQC
#'
#' verify if FASTQC is accessible by the library
#' path location should be saved in the ConfigFile. Try first CheckFastQC().
#' @export
#' @author Elmer A. Fernández
#'
#' @details
#' it should output the help page of FASTQC
#'
#' @return
#' see details
#'
CheckFastQC <- function(){
  config.list <- .OpenConfigFile()
  system2(command = file.path(config.list$fastqc$path,"fastqc"), args = "--help")
}

#  CheckTrimGalore
#'
#' verify if TrimGalores is accessible by the library
#' path location should be saved in the ConfigFile. Try first CheckFastQC().
#' @export
#' @author Elmer A. Fernández
#'
#' @details
#' it should output the help page of FASTQC
#'
#' @return
#' see details
#'
CheckTrimGalore <- function(){
  config.list <- .OpenConfigFile()
  trimgalore <- file.path(config.list$trimgalore$path,"trim_galore")
  system2(command = trimgalore, args = paste("--path_to_cutadapt", config.list$cutadapt$path))
  system2(command = trimgalore, args = "--help")
}


