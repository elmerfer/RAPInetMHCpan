library(seqinr)
library(stringr)
library(BiocParallel)
library(openxlsx)
library(RAPInetMHCpan)

#this sequence segment belongs to NA H5N1 .[(GenBank: AAC32089.1)
myseq <- s2c("MNPNQKIITIGSICMVVGIISLMLQIGNIISVWVSHIIQTWHPNQPEPCNQSINFYTEQAAASVTLAGNSSLCPISGWAIYSKDNSIRIGSKGDVF")

seqfile <- file.path(.libPaths()[1],"filetest.fasta")
write.fasta(myseq, file.out = seqfile,
            name = "NA H5N1 .[(GenBank: AAC32089.1) short sequence]")
if(!file.exists(seqfile)){
  stop("ERROR: pls check path")
}
##Run for a unique allele for MHC I
t <- Sys.time()
ret <- RunNetMHCPan(seqfile = seqfile, allele = "HLA-A01:01")
print(Sys.time()-t)
View(ret)
PlotPeptideCore(eDB = ret, index = 1)
## Run list of alleles (it will run on two cores, if you whant to run just one core, provide a vector instead of a list)
t <- Sys.time()
ret <- RunMHCAlleles(seqfile = seqfile, alleleList = list(c("HLA-A01:01","HLA-A02:01"),c("HLA-A03:01","HLA-A24:02")))
print(Sys.time()-t)
## run for a unique allele for MHC II
ret <- RunNetMHCIIPan(seqfile = seqfile, alleles = "DRB1_0101")
## Run list of alleles (it will run on two cores, if you whant to run just one core, provide a vector instead of a list)
al.list <- list(L1=c("DRB1_0101","DRB1_0102"), L2=c("DRB1_0103","DRB1_0104"))
t <- Sys.time()
ret <- RunMHCIIAlleles(seqfile = seqfile, alleleList = al.list)
print(Sys.time()-t)
View(ret)
##how many Strong and Weak Binders?
table(ret$Type)

file.remove(seqfile)
