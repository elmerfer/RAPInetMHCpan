% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RAPInetMHCpan.R
\name{installNetMHCIIPan}
\alias{installNetMHCIIPan}
\title{Painless instalation tool for netMHCIIpan in linux and R}
\usage{
installNetMHCIIPan(file = NULL, data = NULL, dir)
}
\arguments{
\item{file}{NULL (default) or character with the netMHCIIpanXX.Linux.tar.gz file. If NULL,
a file selection dialog window will be opened.}

\item{data}{NULL(default) or character with the path of the data.Linux.tar.gz data file.
If NULL it will be automatically downloaded from the netMHCIIpan server (it may take time)}

\item{dir}{character. dir the woftware will be installed}
}
\value{
None.
}
\description{
Painless instalation tool for netMHCIIpan in linux and R
}
\details{
install the netMHCIIpan software in your computer and allows suing it trhough R
 as a results it will cread an RDS file named which is used to run the rest of the function
 configIniRAPInetMHCIIpan.RDS PLEASE DO NOT DELET IT

In this way, it may be run in parallel to speed up peptides sercehing trhopugh the whole HLA set.
Is succesfull instalation , an output displaying the following information shoule be seen on screen.
.....
}
\examples{
## see...
}
