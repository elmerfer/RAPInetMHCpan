### MHC I SECTION ####
#' RunNetMHCPan
#'
#' Run a peptides between 8 to 14 mers along a fasta sequence (from file)
#'
#' @param seqfile character with the file path of the fasta file (it should be .fasta and contain onle 1 sequence)
#' @param allele a character vector with the HLA sequences  c("HLA-A01:01") or c("HLA-A01:01","HLA-A02:01") and so on. If NULL it will be automatically downloaded from the netMHCpan server (it may take time)
#' @param rthParam float (default 0.5) upper limit for strong binder (SB -> peptide percent rank < rhtParam)
#' @param rlhParam float (default 2.0) upper limit for weak binder (WB ->  rhtParam <= peptide percent rank < rltParam )
#' @param tParam tparam
#'
#' @details run netMHCpan trhough the sequence fasta file for each HLA
#'
#' @export
#'
#' @return a character
#'
RunNetMHCPan <- function(seqfile, allele, rthParam = 0.50, rltParam= 2.0, tParam = -99.9002){
  res <- .RunNetMHCPan(seqfile, allele, rthParam , rltParam , tParam )
  class(res) <- "RAPIMHC"
  res<-FormatOut(res)
  class(res) <- c("RAPIMHC", class(res))
  return(invisible(res))
}
.RunNetMHCPan <- function(seqfile, allele, rthParam = 0.50, rltParam= 2.0, tParam = -99.9002){
  hla <- allele
  softwarePath <- .GetPath()
  if(is.null(softwarePath) | !dir.exists(softwarePath)){
    stop("ERROR: missing netMHCpan path")
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
  long.pep <- paste(8:14,collapse = ",")
  command <- file.path(softwarePath,"netMHCpan")
  command <- str_replace_all(command,"//","/")
  arguments <- c(file = datafile,
                 syn = paste("-syn ",file.path(softwarePath,"Linux_x86_64/data/synlist.bin"),sep = ""),
                 tdir = paste("-tdir ", file.path(softwarePath,"tmp/TMPXXXXXX"),sep = ""),
                 rdir = paste("-rdir ", file.path(softwarePath,"Linux_x86_64"),sep = ""),
                 hlapseudo = paste("-hlapseudo ", file.path(softwarePath,"Linux_x86_64/data/MHC_pseudo.dat"),sep = ""),
                 thrfmt = paste("-thrfmt ", file.path(softwarePath,"Linux_x86_64/data/threshold/%s.thr.%s"),sep = ""),
                 version = paste("-version ", file.path(softwarePath,"Linux_x86_64/data/version"),sep = ""),
                 allname = paste("-allname ", file.path(softwarePath,"Linux_x86_64/data/allelenames"),sep = ""),
                 rth = paste("-rth ",rthParam, sep = ""),
                 rlt = paste("-rlt ",rltParam, sep = ""),
                 t = paste("-t ",tParam,sep = ""),
                 v = "-v","-BA",
                 a = paste("-a", NULL),
                 l= paste("-l ",long.pep))
  arguments <- str_replace_all(arguments,"//","/")

  res <- unlist(lapply(hla,function(al){
    arguments["a"] <- paste("-a",al)
    # print(arguments["a"])
    s1 <- system2(command = command, stdout = TRUE, args = arguments)
    print(paste("Longitud seq:",length(s1)))
    binders <- c(which(str_detect(s1,"SB")),which(str_detect(s1,"WB")))
    class(s1) <- "RAPIMHC"
    if(length(binders)>0){
      return(s1[binders])
    }else return(NA)
  }))


}
#' RunAllMHC
#'
#' Run a ALL the available MHCI peptides or by specie
#'
#' @param seqfile character with the file path of the fasta file (it should be .fasta and contain onle 1 sequence, Multifasta files in progress)
#' @param alleleGroup a character vector indicating whi allele family, possible values are "HLA","BoLA", "Gogo","H","H2", "Mamu", "Patr","SLA", "All"
#' @param rthParam float (default 0.5) upper limit for strong binder (SB -> peptide percent rank < rhtParam)
#' @param rlhParam float (default 2.0) upper limit for weak binder (WB ->  rhtParam <= peptide percent rank < rltParam )
#' @param tParam tparam
#' @param nCores integer indicating the number of used CPUs or workers (see BiocParallele)
#'
#' @details run netMHCIIpan trhough the sequence fasta file for each MHCII Allele
#'
#' @export
#'
#' @return a character
#'

RunAllMHC <- function(seqfile, alleleGroup = c("HLA","BoLA", "Gogo","H","H2", "Mamu", "Patr","SLA", "All"),
                      rthParam = 0.50, rltParam= 2.0, tParam = -99.9002,  nCores ){
  if(missing(nCores)){
    nCores <- parallel::detectCores()-1
    cat(paste("\nSetting number of cores to", nCores," (since nCores args is missing)\n"))
  }

  HLA.lista <- .GetMHCgeneList(segmentsLength = nCores,
                              alleleGroup = alleleGroup)
  res <- RunMHCAlleles(seqfile=seqfile, alleleList = HLA.lista, rthParam=rthParam,
                       rltParam = rltParam, tParam=tParam,  nCores=nCores)
  return(res)
}
#' RunMHCAlleles
#'
#' Run a ALL the available MHCI peptides or by specie (This function is called by RunAllMHC)
#'
#'
#' @param seqfile character with the file path of the fasta file (it should be .fasta and contain onle 1 sequence, Multifasta files in progress)
#' @param alleleList a list of characters containing alleles list(l1 = c("HLA-A01:01","HLA-A02:01"), l2 = c("HLA-A26:01","HLA-B08:01"))
#' @param rthParam float (default 0.5) upper limit for strong binder (SB -> peptide percentage rank < rhtParam)
#' @param rlhParam float (default 2.0) upper limit for weak binder WB ->  rhtParam <= peptide percentrank < rltParam
#' @param tParam tparam
#' @param nCores integer indicating the number of used CPUs or workers from BiocParallele
#'
#' @details run netMHCIIpan trhough the sequence fasta file for each MHCII Allele
#'
#' @export
#'
#' @return a character
#'
RunMHCAlleles <- function(seqfile, alleleList , rthParam = 0.50, rltParam= 2.0, tParam = -99.9002,  nCores ){
  softwarePath <- .GetPath()

  if(missing(nCores)){
    nCores <- parallel::detectCores()-1
    cat(paste("\nSetting number of cores to", nCores," (since nCores args is missing)\n"))
  }
  if(nCores > length(alleleList)){
    nCores <- length(alleleList)
    cat(paste("\nSetting number of cores to", nCores," as long of the HLA list input\n"))
  }


  ret.list <- bplapply(alleleList, function(x, sqf){
    return(.RunNetMHCPan(seqfile=sqf, allele = x, rltParam = rltParam, rthParam = rthParam, tParam = tParam))
  }, sqf = seqfile, BPPARAM= MulticoreParam(workers =  nCores))
  ret.list <- unlist(ret.list)
  class(ret.list) <- "RAPIMHC"
  ret <- FormatOut(ret.list)
  class(ret) <- c("RAPIMCH", class(ret))
  return(ret)
}
##............................................................................................
#---- MHC II SECTION ----

#' RunNetMHCIIPan
#'
#' Run a peptides between 8 to 14 mers along a fasta sequence (from file)
#'
#'
#' @param seqfile character with the file path of the fasta file (it should be .fasta and contain onle 1 sequence)
#' @param alleles a character vector with the MHCII allele sequences  c("DRB1_0101") or c("DRB1_0101","DRB1_0111") and so on. If NULL it will be automatically downloaded from the netMHCIIpan server (it may take time)
#' @param rankS float (default 0.5) upper limit for strong binder
#' @param rankW float (default 10.0) upper limit for weak binder
#' @param rankF float (default 10.0) filter to show peptide binders
#' @param pepLength integer (default 15) length of the tested peptide
#'
#' @details run netMHCIIpan trhough the sequence fasta file for each MHCII Allele
#'
#' @export
#'
#' @return a character vector with Strong Binders (SB) and Weak Binders (WB)

RunNetMHCIIPan <- function(seqfile, alleles, rankS = 0.5, rankW = 10, rankF = 10, pepLength = 15){
  ret <- .RunNetMHCIIPan(seqfile, alleles, rankS , rankW, rankF, pepLength)
  ret <- FormatOut(ret)
  class(ret) <- c("RAPIMHCII", class(ret))
  return(ret)
}
.RunNetMHCIIPan <- function(seqfile, alleles, rankS = 0.5, rankW = 10, rankF = 10, pepLength = 15){
  softwarePath <- .GetPath(FALSE)
  if(is.null(softwarePath) | !dir.exists(softwarePath)){
    stop("ERROR: missing netMHCIIpan path")
  }

  if(str_detect(basename(seqfile),".fasta")){
    datafile <- seqfile
    fasta <- TRUE
  }else{
    if(str_detect(basename(seqfile),".pep")){
      fasta <- FALSE
      datafile <- seqfile
    }else{
      stop("ERROR file should end in fasta or pep")
    }
  }
  if(is.null(alleles) | missing(alleles)){
    hlaseq <- TRUE
  }else{
    hlaseq <- FALSE
  }

  long.pep <- paste(8:14,collapse = ",")
  command <- file.path(softwarePath,"netMHCIIpan")
  command <- str_replace_all(command,"//","/")

  arguments <- c(seqtype    = paste("-inptype ", ifelse(fasta,"0","1"),sep=""),
                 seqfile    =  paste("-f", seqfile),
                 allele     = paste("-a", alleles, collapse = ","),
                 pepL       = paste("-length", pepLength),
                 filter     = paste("-filter ", ifelse(hlaseq == FALSE,1,0)),
                 rankf      = paste("-rankF", rankF),
                 ranks      = paste("-rankS", rankS),
                 rank       = paste("-rankW", rankW),
                 BA         = "-BA",
                 context    = "-context")

  arguments <- str_replace_all(arguments,"//","/")
  seq_type='aa'
  res <- unlist(lapply(alleles,function(al){
    arguments["allele"] <- paste("-a",al)
    s1 <- system2(command = command, stdout = TRUE, args = arguments)
    binders <- c(which(str_detect(s1,"SB")),which(str_detect(s1,"WB")))

    if(length(binders)>0){
      return(s1[binders])
    }else return(NA)
  }))
  class(res) <- "RAPIMHCII"
  return(res)
}

#' RunMHCIIAlleles
#'
#' Run a peptides between 8 to 14 mers along a fasta sequence (from file)
#'
#' @param seqfile character with the file path of the fasta file (it should be .fasta and contain onle 1 sequence)
#' @param alleleList a character vector with the MHCII allele sequences  c("DRB1_0101") or c("DRB1_0101","DRB1_0111") and so on. If NULL it will be automatically downloaded from the netMHCIIpan server (it may take time)
#' @param rankS float (default 0.5) upper limit for strong binder
#' @param rankW float (default 10.0) upper limit for weak binder
#' @param rankF float (default 10.0) filter to show peptide binders
#' @param pepLength integer (default 15) length of the tested peptide
#' @param nCores number of used CPS or worker for BiocParallel
#'
#' @details run netMHCIIpan trhough the sequence fasta file for each MHCII Allele
#'
#' @export
#'
#' @return a data.frame
RunMHCIIAlleles <- function(seqfile, alleleList ,rankS = 0.5, rankW = 10, rankF = rankW, pepLength = 15, nCores ){
  softwarePath <- .GetPath(FALSE)


  if( missing( nCores) ) {
    nCores <- parallel::detectCores() - 1
    cat(paste("\nSetting number of cores to", nCores," (since nCores args is missing)\n"))
  }
  if( nCores > length( alleleList ) ) {
    nCores <- length( alleleList )
    cat(paste("\nSetting number of cores to", nCores," as long of the HLA list input\n"))
  }
  cat(paste("\nUsing ", nCores," workers\n"))


  ret.list <- bplapply(alleleList, function(x, sqf, wh){
    return(.RunNetMHCIIPan(seqfile = sqf, alleles = x))
  }, sqf = seqfile,
  BPPARAM = MulticoreParam(workers =  nCores))

  ret.list <- unlist(ret.list)
  class(ret.list) <- "RAPIMHCII"
  ret.list <- FormatOut(ret.list)
  class(ret.list) <- c("RAPIMHCII",class(ret.list))
  return( ret.list )
}
#' RunMHCIIAlleles
#'
#' Run a peptides between 8 to 14 mers along a fasta sequence (from file)
#'
#' @param seqfile character with the file path of the fasta file (it should be .fasta and contain onle 1 sequence)
#' @param alleleGroup see GetAlleleList
#' @param rankS float (default 0.5) upper limit for strong binder
#' @param rankW float (default 10.0) upper limit for weak binder
#' @param rankF float (default 10.0) filter to show peptide binders
#' @param pepLength integer (default 15) length of the tested peptide
#' @param nCores number of used CPS or worker for BiocParallel
#'
#' @details run ALL MHCII alleles agains the sequenfile
#'
#' @export
#'
#' @return an RAPIMHCII data.frame like obsejt
RunAllMHCII <- function(seqfile, alleleGroup ,rankS = 0.5, rankW = 10, rankF = rankW, pepLength = 15, nCores ){
  if(missing(nCores)){
    nCores <- parallel::detectCores()-1
    cat(paste("\nSetting number of cores to", nCores," (since nCores args is missing)\n"))
  }

  HLA.lista <- GetAlleleList(nSegments = nCores, MHC.group = alleleGroup, MHCII = TRUE)
  ret <- RunMHCIIAlleles(seqfile = seqfile,
                         alleleList = HLA.lista,
                         rankS = rankS,
                         rankW = rankW, rankF = rankF,
                         pepLength = pepLenth, nCores = nCores )
  return(ret) #seqfile, alleleList ,rankS = 0.5, rankW = 10, rankF = rankW, pepLength = 15, nCores
}
