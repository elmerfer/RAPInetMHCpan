

library(openxlsx)
pep <- read.xlsx("/media/elmer/141f3650-b135-4c9d-a497-9f1ee77c14f5/home/Elmer/Base de datos.xlsx")
pep$seq <- unlist(lapply(pep$Sequence, function(x) unlist(stringr::str_split(x," "))[1]))
pep$seq[stringr::str_detect(pep$seq, "\\*")] <- NA
pep$Category <- rowSums(apply(pep[,c(3:5)],2,as.numeric))
table(pep$Category)
summary(pep$TAP)
pep1<-pep[,c("seq","WT","Category","HLA","TAP","TAP.WT", "DAI")]
pep1 <- na.omit(pep1)
dim(pep1)

pep1$Cleave <- unlist(lapply(cleave(pep1$seq), length))
pep1$CleaveWt <- unlist(lapply(cleave(pep1$WT), length))

pep1$a <- log(abs(as.numeric(pep1$DAI))+1)
summary(as.numeric(pep1$DAI))
summary(pep1$a)
t.test(a~Category,pep1)
table()
summary(pep1$TAP.WT)
ggplot(pep1, aes(x=factor(Category),y=as.numeric(DAI), colour = factor(Category))) + geom_boxplot() + geom_jitter(height = 0)
ggplot(pep1, aes(x=TAP,y= TAP.WT, colour = factor(Category))) + geom_point()
ggplot(pep1, aes(x=log(as.numeric(DAI)),y= TAP.WT, colour = factor(Category))) + geom_point()
plot(pep1$TAP.WT, pep1$TAP)
abline(a=0, b=1)
table(pep1$Category, pep1$Cleave)

library(BiocParallel)

hla.types <- unique(pep1$HLA)
length(hla.types)
lr <- bplapply(hla.types, function(al){

  subpep <- subset(pep1, HLA == al)

  pep.file <- tempfile(fileext = ".pep")
  writeLines(subpep$seq, pep.file)
  wt.file <- tempfile(fileext = ".pep")
  writeLines(subpep$WT, wt.file)
  mut <- system2(command = command, args = c("-BA",
                                             stringr::str_remove_all(paste0("-a HLA-",al),"\\*"),
                                             paste0("-p ",pep.file )),stdout = TRUE)
  mut <- Format.Out(mut)
  file.remove(pep.file)
  wt <- system2(command = command, args = c("-BA",
                                            stringr::str_remove_all(paste0("-a HLA-",al),"\\*"),
                                            paste0("-p ",wt.file )),stdout = TRUE)
  wt <- Format.Out(wt)
  file.remove(wt.file)
  return(list(M=mut,W=wt))
}, BPPARAM = MulticoreParam(workers = length(hla.types)))


dai <- do.call(rbind,lapply(lr, function(x) {
  colnames(x$M) <- paste0(colnames(x$M),".Mut")
  colnames(x$W) <- paste0(colnames(x$W),".Wt")
  cbind(x$M,x$W)
  }
  ))
dim(dai)

dai <- merge(dai, pep1[,c("seq","Category")], by.x = "Peptide.Mut",by.y = "seq",sort=F)
table(dai$Category)
dai$dai <-  -as.numeric(dai$`Aff(nM).Mut`) + as.numeric(dai$`Aff(nM).Wt`)
dai$dai <-  as.numeric(dai$`Aff(nM).Mut`)/(as.numeric(dai$`Aff(nM).Mut`) + as.numeric(dai$`Aff(nM).Wt`))
dai$dai <-  as.numeric(dai$`Aff(nM).Mut`)/(as.numeric(dai$`Aff(nM).Wt`))
dai$dai <-  -as.numeric(dai$`Aff(nM).Mut`) + as.numeric(dai$`Aff(nM).Wt`)
dai$dai <- -as.numeric(dai$`%Rank_EL.Mut`) + as.numeric(dai$`%Rank_EL.Wt`)
dai$dai <- as.numeric(dai$`%Rank_BA.Mut`) / (as.numeric(dai$`%Rank_BA.Wt`)+as.numeric(dai$`%Rank_BA.Mut`))
dai$Category <- factor(dai$Category)
library(ggplot2)

ggplot(dai, aes(y=log(dai),x=Category)) + geom_boxplot() + geom_jitter(width = 0.2)
summary(dai$dai)
unlist(lapply(lr, function(x) nrow(x$M)))

merge(lr[[1]]$M,lr[[1]]$W, by = "MHC", sort=F, suffixes = c(".Mut",".Wt") )
