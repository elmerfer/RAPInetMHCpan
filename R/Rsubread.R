#' RsubBuildIndex
#'
#' Creates the reference index file and update the Configuration file
#' @description The index file will be stored in the same reference file and the path and name
#' will be masked into the configuration file
#' @export
#' @examples
#' \dontrun{
#'
#'  RsubBuildIndex(hgFile = "/.../MyHumanReferencelocation/hg19.fa)
#' }
#'
RsubBuildIndex <- function(hgFile){

  if(!file.exists(hgFile)){
    stop(paste(hgFile, "NOT FOUND"))
  }

  config.list <- .OpenConfigFile()

  nf <- unlist(stringr::str_split(basename(hgFile),"\\."))[1]
  config.list$Rsubread$index$path <- dirname(hgFile)
  config.list$Rsubread$index$file <- file.path(config.list$Rsubread$index$path,paste("RSubread.",nf,".index",sep=""))


  buildindex(basename = config.list$Rsubread$index$file, reference = hgFile)
  if(!file.exists(paste(config.list$Rsubread$index$file,".reads",sep=""))){
    stop("The index file could not be created")
  }
  .SaveConfigFile(config.list)
}
#' RsubSubjunc
#'
#' A wrapper function to Rsubread::subjunc thats facilitate its use. Only for hg19 human genome reference
#' @description A wrapper function to run Rsubread subjunc function for human genome hg19 only.
#' by generating the genome index by RsubBuildIndex function, it genome indexes stored
#' in the RAPInetMHCpan configuration file. see Rsubread subjunt function for more details.
#' @export
#' @examples
#' \dontrun{
#'
#' }
#'
RsubSubjunc <- function(file,paired = TRUE, ...){
  config.list <- .OpenConfigFile()
  if(!file.exists(config.list$Rsubread$index.file)){
    cat("\n----------")
    cat("\nRSubread index file not created, pls run RsubBuildIndex")
    stop()
  }
  if(paired){
    if(any(stringr::str_detect(file,"_val_1.fq"))){
      cat("\nAssuming trimmed file by TrimGalore, building _val_2.fq")
      file2 <- stringr::str_replace_all(file, "1.fq","2.fq")
    }else{
      file2 <- stringr::str_replace_all(file, "1.fastq","2.fastq")
    }

  }else{
    file2 <- NULL
  }
  f1.ok <- file.exists(file)
  f2.ok <- file.exists(file2)
  if(all(f1.ok)==FALSE){
    stop(paste("The following files where not found", file[!f1.ok]))
  }
  if(all(f2.ok)==FALSE){
    stop(paste("The following files where not found", file2[!f2.ok]))
  }

  input.args <- unlist(match.call())[-1]
  if(any(names(input.args)=="annot.inbuilt")){
    if(input.args["annot.inbuilt"] != "hg19"){
      stop("annot.intbuilt should be hg19")
    }
  }else{
    stop("please set annot.intbuilt = hg19 ")
  }
  subjunc( index= config.list$Rsubread$index.file,
           readfile1 = file1,
           readfile2 =  file2,
           ...)
}


RsubFeatureCounts <- function(filebam,...){

}
