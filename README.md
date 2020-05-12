# RAPInetMHCpan
RAPInetMHCpan: a library to facilitate the instaltion and used of both netHCpan and netMHCIIpan neural netowrks for prediction of peptide binders (up to now only Linux. Windows & Mac in progress)

## Instalation
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
Please be sure that you have the right shell to run netMHCpan and netMHCIIpan. They use the "tcsh" a Unix based shell compatible with cshel (a shel to run c code)
To verify if you have it in your machine from a console terminal type 
'''tsch --version'''
if succeed you will see something like:
![tsch output](https://github.com/elmerfer/RAPInetMHCpan/blob/master/tsch.shell.png)
### Testing the library
Please download and run the following R script file [test.rapiNetMHCpan](https://github.com/elmerfer/RAPInetMHCpan/blob/master/test.rapiNetMHCpan.R)
## Authors

* **Elmer A. Fernández** - *Idea and Initial work* - [elmerfer](https://github.com/elmerfer)

## License

This project is licensed under the GPL 3-0 License 



