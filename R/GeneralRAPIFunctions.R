### GENERAL FUNCTIONS ####
#'  FormatOut
#'
#' Internal function, it format the output. This function should not be used
#'
#'
#' @param resRAPIMHC an object results
#'
#' @return
#'
#'
#' @examples
#' \dontrun{
#' FormatOut(eDB = result)
#' }
FormatOut <- function(resRAPIMHC){
  if(is.na(resRAPIMHC)[1]){
    return()
  }
  if(class(resRAPIMHC)=="RAPIMHC"){
    sal <- data.frame(do.call(rbind, lapply(resRAPIMHC, function(x) {

      if(any(c(stringr::str_detect(x,"<= SB"),stringr::str_detect(x,"<= WB")),na.rm=TRUE)==FALSE) {
        # n <- 1
        return()
      }

      aux <- unlist(stringr::str_split(x," "))
      aux <- aux[aux != ""]
      aux <- aux[aux != "<="]
    })))
  }else{
    sal <- data.frame(do.call(rbind, lapply(resRAPIMHC, function(x) {

      if(any(c(stringr::str_detect(x,"<=SB"),stringr::str_detect(x,"<=WB")), na.rm = TRUE)==FALSE) {
        # n <- 1
        return()
      }

      aux <- unlist(stringr::str_split(x," "))
      aux <- aux[aux != ""]
      aux <- aux[aux != "<="]
    })))
  }

  if(class(resRAPIMHC)=="RAPIMHC"){
    colnames(sal) <- c("Pos","Allele", "Peptide", "Core", "Offset","Dpos","Dlength","Ipos","Ilength","InterCore","SeqName","RawScore","Affy","PercRank","Type")
    return(sal[order(sal$Type, sal$PercRank, decreasing = c(FALSE,FALSE)),])
  }else{
    colnames(sal) <- c("Pos","Allele", "Peptide", "StartPos", "Core", "Reliability","SeqName","ELscore","PercRank","Exp_bind","BAscore","Affy","BARank","Type")
    sal$Type <- stringr::str_remove_all(sal$Type,"<=")
    return(sal[order(sal$Type, sal$PercRank, decreasing = c(FALSE,FALSE)),])
  }
}



#'  .GetMHCgeneList
#'
#' generate a ggplot2, where in the y axis the amount of alleles bindig to the sequence starting at x position is displayed
#' internal
#'
#' @param segmentsLength number of segments
#' @param alleleGroup allele group
#'
#' @return
#'
#'
#' @examples
#' \dontrun{
#' FormatOut(eDB = result)
#' }
.GetMHCgeneList <- function(segmentsLength = 10,
                           alleleGroup = c("HLA","BoLA", "Gogo","H","H2", "Mamu", "Patr","SLA", "All")){
  softwarePath <- .GetPath()
  allele.names.file <- stringr::str_replace_all( file.path(softwarePath, "data/allelenames" ),"//","/")
  if(file.exists(allele.names.file)==FALSE){
    stop("error: File not found")
  }
  HLA.table <- read.table(allele.names.file)

  HLA.group <- match.arg( alleleGroup, c("HLA","BoLA", "Gogo","H","H2", "Mamu", "Patr","SLA", "All"))
  if(HLA.group != "ALL") {
    HLA.table <- HLA.table[stringr::str_detect(HLA.table$V1,HLA.group),]
  }

  id <- c(seq(1,nrow(HLA.table),length.out = segmentsLength),nrow(HLA.table)+1)

  allele.list <- lapply(1:(length(id)-1), function(x) HLA.table$V1[id[x]:(id[x+1]-1)])
  return(allele.list)

}
#'  GetMHCgeneList
#'
#' returns an list of Allele types (length nSegments)
#'
#'
#' @param nSegments integer, number of segments
#' @param MHC.group MHC allele group "HLA","BoLA", "Gogo","H","H2", "Mamu", "Patr","SLA",
#'        "DRB", "DQ","DP","Mouse","All"
#' @param MHCII (logical) if MHCII = TRUE, then only MHC.group in "DRB", "DQ","DP","Mouse","All" are evaluated
#'                        if MHCII = FALSE, only "HLA","BoLA", "Gogo","H","H2", "Mamu", "Patr","SLA", "All" are evaluated
#' @export
#'
#' @return
#' a list of alleles with length = nSegments (ussualy set to the number of cores or CPU workers)
#'
#' @examples
#' \dontrun{
#' FormatOut(eDB = result)
#' }
GetAlleleList <- function(nSegments = 10,
                          MHC.group = c("HLA","BoLA", "Gogo","H","H2", "Mamu", "Patr","SLA",
                                        "DRB", "DQ","DP","Mouse","All"),
                          MHCII =FALSE){
  if(MHCII == TRUE){
    softwarePath <- .GetPath(!MHCII)
    MHC.group <- try(match.arg(MHC.group, c("DRB", "DQ","DP","Mouse","All")))
    if(class(MHC.group) == "try-error"){
      stop("ERROR: bad argument in MHC.group")
    }
    HLA.table <- read.table(file.path(softwarePath,"data/allelelist.txt"),h=F)
    if(MHC.group != "All"){
      HLA.table <- HLA.table[which(stringr::str_detect(HLA.table$V1, MHC.group)),]
    }

  }else{
    return(.GetMHCgeneList(nSegmens, MHC.group))
  }

  id <- c(seq(1,nrow(HLA.table),length.out = nSegments),nrow(HLA.table)+1)

  allele.list <- lapply(1:(length(id)-1), function(x) HLA.table$V1[id[x]:(id[x+1]-1)])
  return(allele.list)

}

.CheckAllele <- function(allele){
  softwarePath <- RAPInetMHCpan:::.GetPath()
  allele.names.file <- stringr::str_replace_all( file.path(softwarePath, "data/allelenames" ),"//","/")
  if(file.exists(allele.names.file)==FALSE){
    stop("error: File not found")
  }
  HLA.table <- read.table(allele.names.file)
  return(any(stringr::str_detect(HLA.table$V1, allele)))
}

BuildPeptideFile <- function(peptide){
  fname <- tempfile(pattern = "RAPInetMHCpan", tmpdir = tempdir(), fileext = ".pep")
  writeLines(peptide, con = fname)
  if(file.exists(fname)){
    return(fname)
  }else{
    stop("ERROR: BuildPeptideFile")
  }
}

#'  summary
#'
#' a summary general function to display RAPIMHC and RAPIMHCII results
#'
#'
#' @param eDB RAPIMHC or RAPIMHCII object
#' @export
#'
#' @return
#'
#'
#' @examples
#' \dontrun{
#' summary(eDB)
#' }
summary.RAPIMHCII <- function(eDB){
  cat("\n-------------------\n")
  table(eDB$Type)

  sb.p <- length(unique(subset(eDB,Type == "SB")$Core))
  wb.p <- length(unique(subset(eDB,Type == "WB")$Core))
  cat(paste("\nNumber of unique SB (",sb.p,") and WB (",wb.p, ") Peptides"))


}

#'  VerifySeq
#'
#' generate a ggplot2, where in the y axis the amount of alleles bindig to the sequence starting at x position is displayed
#'
#'
#' @param seqFile fasta file name (default missing, it will open an file open dialog box)
#' @param eDB RAPIMHC or RAPIMHCII object
#' @export
#'
#' @return
#' a list with the slot "Peptide" for RAPIMHC y RAPIMHCII and with "InterCore" in the RAPIMHC case
#'
#' @examples
#' \dontrun{
#' VerifySeq(eDB = result)
#' }
VerifySeq <- function(seqFile , eDB){
  if( missing( seqFile ) ){
    seqFile <- file.choose()
  }
  if(file.exists(seqFile) == FALSE){
    stop("File not found")
  }
  seq <- seqinr::read.fasta(file = seqFile, seqtype = "AA")
  if(length(seq) > 1){
    stop("only one sequence available, multisequence under development")
  }
  seq.name <- seqinr::getName(seq)
  seq <- paste(unlist(seq),collapse = "")
  ##looking Peptides
  found.pep <- unlist(lapply(unique(eDB$Peptide), function(x){
    if(stringr::str_detect(seq,as.character(x) )) {
      return(as.character(x))
    }else{
      return(NA)
    }
  } ))
  if( "RAPIMHC" %in% class(eDB)){
    found.icore <- unlist(lapply(unique(eDB$InterCore), function(x){
      if(stringr::str_detect(seq,as.character(x) )) {
        return(as.character(x))
      }else{
        return(NA)
      }
    } ))
    ret <- list(Peptide = found.pep[!is.na(found.pep)], InterCore = found.icore[!is.na(found.icore)])
  }else{
    ret <- list(Peptide = found.pep[!is.na(found.pep)])
  }
  # names(ret) <- seq.name
  return(ret)
}

##
#'  SaveExcel
#'
#' generate a ggplot2, where in the y axis the amount of alleles bindig to the sequence starting at x position is displayed
#'
#'
#' @param eDB RAPIMHC or RAPIMHCII object
#' @param file character string with the file name
#' @export
#'
#' @return
#'
#'
#' @examples
#' \dontrun{
#' VerifySeq(eDB = result)
#' }
SaveExcel <- function(eDB, file){
  tosave <- list(StrongBinders = subset(eDB, Type == "SB"),
       WeakBinders = subset(eDB, Type == "WB"))
  if( !stringr::str_detect(file, "xlsx")){
    file <- paste(file,".xlsx",sep="")
  }
  openxlsx::write.xlsx(tosave, file = file, colNames = TRUE)
}



  }
}
