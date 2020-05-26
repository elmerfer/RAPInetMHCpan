### PLOT FUNCTION ####
#'  PlotPeptideLengthDistribution
#'
#' generate a ggplot2 bar plot of the distribution counts of predicted peptides of different length
#'
#' @param resDF a data frame or RAPIMHCI or RAPIMHCII class object
#' @param main a cg¿haracter with a plot title
#' @export
#'
#' @return
#' a ggplot2 object
#'
#' @examples
#' \dontrun{
#' RepresentedRegions(resDF)
#' }
PlotPeptideLengthDistribution <- function(resDF, main){
  # if(class(resDF)!="RAPI"){stop("is not a RAPI results df)}
  if("RAPIMHC" %in% class(resDF) ){
    if(all(c("Peptide","InterCore") %in% colnames(resDF)  ) == FALSE){
      stop("Error: is not an appropriate data frame imput")
    }
  }
  t1 <- data.frame(Length=unlist(str_length(resDF$Peptide)),PeptideType = "Peptide")
  # t2 <- data.frame(Length=unlist(str_length(resDF$Core)),PeptideType = "Core")
  t3 <- data.frame(Length=unlist(str_length(resDF$InterCore)),PeptideType = "Interaction")

  df <- data.frame(rbind(t1,t3))

  df$PeptideType <- factor(df$PeptideType, levels= c("Peptide","Interaction"))

  pl <- ggplot2::ggplot(df, ggplot2::aes(Length)) + ggplot2::geom_bar() + ggplot2::facet_grid(cols = ggplot2::vars(PeptideType))
  if(missing(main)) {
    print(pl)
  }else print(pl + ggplot2::ggtitle(main))
  return(invisible(pl))
}

#'  PlotBindingPeptideDistribution
#'
#' generate a ggplot2, displays bars indicating the amount of predicted peptides of different lengths
#'
#'
#' @param resDF a data frame or RAPIMHCI or RAPIMHCII class object
#' @param pepLevel peptide level filter ¡?
#' @param main a character with a plot title
#' @export
#'
#' @return
#' a ggplot2 object
#'
#' @examples
#' \dontrun{
#' RepresentedRegions(resDF)
#' }
PlotBindingPeptideDistribution <- function(resDF, pepLevel = 0, main){

  top.p <- data.frame(N=c(sort(table(resDF$Peptide),decreasing = TRUE)), PeptideType = "Peptide")
  top.p$X <- 1:nrow(top.p)
  if("RAPIMHC" %in% class(resDF)){
    top.ic <- data.frame(N=c(sort(table(resDF$InterCore),decreasing = TRUE)), PeptideType = "Interaction")
    top.ic$X <- 1:nrow(top.ic)
    df <- data.frame(rbind(top.p, top.ic))
    df$PeptideType <- factor(df$PeptideType, levels = c("Peptide","Interaction") )
  }else{
    df <- top.p
    df$PeptideType < "Peptide"
  }


  pl <- ggplot(subset(df, N > pepLevel), aes(y=N,x=X)) +  geom_point() + facet_grid(rows = vars(PeptideType))
  if(missing(main)) {
    print(pl)
  }else print(pl + ggtitle(main))
  return(invisible(pl))
}
#'  PlotPeptideLengthDistribution
#'
#' generate a ggplot2, where in the y axis the amount of alleles bindig to the sequence starting at x position is displayed
#' they are also known as "immunodominsnt regions" as in Contact https://doi.org/10.1016/j.chom.2020.03.002
#'
#' @param resDF a data frame or RAPIMHCI or RAPIMHCII class object
#' @param main a cg¿haracter with a plot title
#' @param nbk number of x axis breaks
#' @export
#'
#' @return
#' a ggplot2 object
#'
#' @examples
#' \dontrun{
#' RepresentedRegions(resDF)
#' }
RepresentedRegions <- function(resDF, byGroup = FALSE,
                               main= "Number of candidate epitopes per position",nbk=100){

  # resDF$Pos <- as.numeric(as.character(resDF$Pos))
  resDF$Pos <- as.numeric(as.character(resDF$Pos))
  rl <- range(resDF$Pos)
  if(byGroup == TRUE){

    resDF$HLAgroup <- do.call(rbind, str_split(resDF$HLAgene,"\\*"))[,1]
    pl <- ggplot(resDF, aes(Pos)) + geom_bar( aes(fill=HLAgroup)) +
      scale_x_continuous(breaks=seq(rl[1],rl[2],nbk))
  }else{
    pl <- ggplot(resDF, aes(Pos)) + geom_bar() + scale_x_continuous(breaks=seq(rl[1],rl[2],nbk))
  }
  pl <- pl + ggtitle(main) + xlab("Sequence prosition")
  print(pl)
  return(invisible(pl))
}

#'  PeptideBindersLogo
#'
#' generate a ggplot2 logo of all peptides binder of lenth in size.
#' if eDB is an RAPIMHCII object only one plot will be generated
#'
#'
#' @param eDB a data frame or RAPIMHCI or RAPIMHCII class object
#' @param size a integer or integer verctor with values between 8 and 14
#' @param type character Peptide or InterCore for RAPIMHC or Peptide for RAPIMHCII
#' @export
#'
#' @return
#' a ggplot2 object
#'
#' @examples
#' \dontrun{
#' RepresentedRegions(resDF)
#' }
PeptideBindersLogo <- function(eDB, size = c(8:14), type = c("Peptide","InterCore")){


  if("RAPIMHC" %in% class(eDB) ){
    type = match.arg(type,c("Peptide","InterCore"))
    if( min(size)>= 8 & max(size) <=14 ){
      seqs.aa <- lapply(size, function(x){
        sqs <- unique(as.character(eDB[which(stringr::str_length(eDB[,type])==x),type]))
        if(length(sqs)>0){
          return(sqs)
        }else{
          return()
        }
      })
      names(seqs.aa) <- size
      seqs.aa <- seqs.aa[-which(sapply(seqs.aa, is.null))]
      gl <- ggseqlogo::ggseqlogo(seqs.aa, seq_type='aa', ncol=2, nrow = round(max(size)/2,0), method = 'prob' )
    }else{
      stop("ERROR: size range between 8 and 14")
    }
  }else{
    type = match.arg(type,c("Peptide"))
    seqs.aa <- list(unique(as.character(eDB$Peptide)))
    names(seqs.aa) <- "15"

    gl<-ggseqlogo::ggseqlogo(seqs.aa, seq_type='aa',method = 'prob' )
  }

  print(gl)
  return(invisible(gl))
}

#'  PeptideBindersLogo
#'
#' generate a ggplot2 logo of all cores binders associated to such peptide.
#' The cores is the sequence predicted as binder inside the peptide, it can be the same sequence or
#' a shorter one (a deletion/loop in the tested sequence) or longer one with an insertion.
#'
#'
#' @param eDB a data frame or RAPIMHCI or RAPIMHCII class object
#' @param peptide a string sequence peptide
#' @export
#'
#' @return
#' a ggplot2 object
#'
#' @examples
#' \dontrun{
#' PeptideBindersLogo(eDB = res, peptide == "MSRIGMEVTPSGTW")
#' }
PeptideCoreLogo <- function(eDB, peptide){

  if(!any(class(outII) %in% c("RAPIMHC", "RAPIMHCII"))){
    stop("Error: PeptideCoreLogo(eDB, peptide) eDB is not RAPIMHC class")
  }
  if(missing(peptide)){
    stop("Error: PeptideCoreLogo(eDB, peptide) missing peptide")
  }
  if(any(eDB$Peptide == peptide)){
    seqs.aa <- as.character(subset(eDB, Peptide == peptide)$Core)
    gl <- ggseqlogo::ggseqlogo(seqs.aa, seq_type='aa', method = 'prob' ) +
      ggplot2::ggtitle(paste("Peptide:", peptide))
  }


  print(gl)
  return(invisible(gl))
}

#'  PlotPeptideCore
#'
#' generate a ggplot2 logo of the evaluated papetide and its binding regions (Core) allowing
#' the identification of the Core with its deleted (bulging out) regions and predicted insertion sites
#'
#'
#' @param eDB a data frame or RAPIMHCI or RAPIMHCII class object
#' @param index (integer) indicating which row of the RAPIMHC and RAPIMHCII objects
#'
#' @import ggseqlogo
#' @import stringr
#' @export
#'
#' @return
#' a ggplot2 object
#'
#' @examples
#' \dontrun{
#' PlotPeptideCore(eDB = res, index = 1)
#' }
PlotPeptideCore <- function(eDB, index){
  if(any(class(eDB) %in% c("RAPIMHC","RAPIMHCII"))==FALSE){
    stop("ERROR: PlotPeptideCore, eDB must be RAPIMHC or RAPIMHCIIclass")
  }
  if("RAPIMHCII" %in% class(eDB)){

    start <- as.numeric(as.character(eDB[index,"StartPos"]))
    lcseq <- stringr::str_length(eDB[index,"Core"])
    pseq <- as.character(eDB[index, "Peptide"])
    p1 <- ggplot2::ggplot() +
      ggplot2::annotate('rect', xmin = 0.5+start, xmax = start+lcseq+0.5, ymin = -0.25, ymax = 1.25, alpha = .1, col='black', fill='yellow') +
      ggseqlogo::geom_logo(as.character(pseq), stack_width = 0.90, method = "prob") + ggplot2::ggtitle(pseq)+
      ggseqlogo::theme_logo()
    print(p1)
    return(invisible(p1))
  }
  eDB$Ilength <- as.numeric(as.character(eDB$Ilength))
  eDB$Dlength <- as.numeric(as.character(eDB$Dlength))
  ## check if insertion
  if(eDB[index,]$Ilength>0){
    p1 <- ggseqlogo::ggseqlogo(as.character(eDB[index,"Core"]),method = "prob") + ggplot2::ggtitle(as.character(eDB[index,"Peptide"]))
    plot(p1)
    return(invisible(p1))
  }else{##deletion or normal
    if(eDB[index,]$Dlength==0){
      p1 <- ggseqlogo::ggseqlogo(as.character(eDB[index,"Core"]),method = "prob") + ggplot2::ggtitle(as.character(eDB[index,"Peptide"]))
      plot(p1)
      return(invisible(p1))
    }else{
      seq.ins <- eDB[index,]$Peptide
      dpo <- as.numeric(as.character(eDB[index,]$Dpos))
      ile <- as.numeric(as.character(eDB[index,]$Dlength))+1
      off <- as.numeric(as.character(eDB[index,]$Offset))

      if(off>0){
        bl <- ifelse(off+dpo+ile+dpo>= str_length(seq.ins), str_length(seq.ins) + 0.5, off+dpo+ile+dpo+0.5)
        p1 <- ggplot2::ggplot() +
          ggplot2::annotate('rect', xmin = 0.5+off, xmax = off+dpo+0.5, ymin = -0.25, ymax = 1.25, alpha = .1, col='black', fill='yellow') +
          ggplot2::annotate('rect', xmin = off+dpo+ile-0.5, xmax = bl, ymin = -0.25, ymax = 1.25, alpha = .1, col='black', fill='yellow') +
          ggseqlogo::geom_logo(as.character(seq.ins), stack_width = 0.90, method = "prob") + ggplot2::ggtitle(seq.ins)+
          ggseqlogo::theme_logo()
        print(p1)
        return(invisible(p1))
      }else{

        bl <- ifelse(2+off+dpo+ile+dpo>= str_length(seq.ins), str_length(seq.ins) + 0.5, 1+off+dpo+ile+dpo+0.5)
        p1 <- ggplot2::ggplot() +
          ggplot2::annotate('rect', xmin = 0.25, xmax = off+dpo+0.5, ymin = -0.25, ymax = 1.25, alpha = .1, col='black', fill='yellow') +
          ggplot2::annotate('rect', xmin = off+dpo+ile-0.5, xmax = bl, ymin = -0.25, ymax = 1.25, alpha = .1, col='black', fill='yellow') +
          ggseqlogo::geom_logo(as.character(seq.ins), stack_width = 0.90, method = "prob") + ggplot2::ggtitle(seq.ins)+
          ggseqlogo::theme_logo()
        print(p1)
        return(invisible(p1))
      }

    }
  }
}

