# RAPInetMHCpan
RAPInetMHCpan: a library to facilitate the instaltion and used of both netHCpan and netMHCIIpan neural netowrks for prediction of peptide binders (up to now only Linux. Windows & Mac in progress)

## Installation of RAPInetMHCpan
download the precompiled version of [RAPInetMHCpan](https://github.com/elmerfer/RAPInetMHCpan/blob/master/RAPInetMHCpan_0.1.0.tar.gz), then use 
```R
install.packages("...../RAPInetMHCpan_0.1.0.tar.gz", repos = NULL, type = "source")
```
### Install from GitHub

First, you need to install the devtools package. You can do this from CRAN. Invoke R and then type
```R
install.packages("devtools")
```
Load the devtools package.
```R
library(devtools)
install_github("elmerfer/RAPInetMHCpan")
```

### Prerequisites
You will need to install from CRAN
* [openxlsx](https://cran.r-project.org/web/packages/openxlsx/index.html)
* [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html)
* [stringr](https://cran.r-project.org/web/packages/stringr/index.html)
* [parallel](https://cran.r-project.org/web/packages/parallel/index.html)

You will need to install from [Bioconductor](http://www.bioconductor.org)
* [BiocParallel](https://bioconductor.org/packages/release/bioc/html/BiocParallel.html)
### Before the instalation of netMHCpan and netMHCIIpan
Please be sure that you have the right shell to run netMHCpan and netMHCIIpan. They both use the "tcsh", a Unix based shell compatible with cshel (a shel to run c code).

To verify if you have it in your machine, please type from a console terminal the following command 
'tsch --version'
if succeed you will see something like:
![tsch output](https://github.com/elmerfer/RAPInetMHCpan/blob/master/tsch.shell.png)
if not installed try 'sudo apt-get install tcsh' and veryfy. 

If it is done, you may continue installing netMHCpan and netMHCIIpan through RAPInetMHCpan
### Installation of netMHCpan and netMHCIIpan with RAPInetMHCpan
Follow the instructions and fill the form to receive the rights to download [netMHCpan](https://services.healthtech.dtu.dk/service.php?NetMHCpan-4.0) and [netMHCIIpan](https://services.healthtech.dtu.dk/service.php?NetMHCIIpan-3.2) and save them to your favorite directory.
Onpen an R session or RStudio and type:
```R
installNetMHCPan(file = "/home/.../myfavoritedir/netMHCpan-4.0a.Linux.tar.gz" , data = NULL, dir = "/where i whant/dir")
installNetMHCIIPan(file = "/home/.../myfavoritedir/netMHCIIpan-4.a.Linux.tar.gz" , data = NULL, dir = "/where i whant/dir")
```
### Testing the library
Please download and run the following R script file [test.rapiNetMHCpan](https://github.com/elmerfer/RAPInetMHCpan/blob/master/test.rapiNetMHCpan.R)
## Authors

* **Elmer A. Fernández** - *Idea and Initial work* - [elmerfer](https://github.com/elmerfer)

## License

This project is licensed under the GPL 3-0 License 



