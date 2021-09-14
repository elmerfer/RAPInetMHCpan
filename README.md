# RAPInetMHCpan
RAPInetMHCpan: a library to facilitate the installation and use of both [netMHCpan](https://services.healthtech.dtu.dk/service.php?NetMHCpan-4.0) and [netMHCIIpan](https://services.healthtech.dtu.dk/service.php?NetMHCIIpan-3.2)  neural networks for prediction of peptides binding to MHC molecules (up to now only available for Linux,  Mac in progress)

## Installation of the library RAPInetMHCpan 
You may choose one of the following choices (please check the prerequisites)
### Download the precomplied version and install from R console or RStudio
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
* [seqinr](https://cran.r-project.org/web/packages/seqinr/index.html)
* [ggseqlogo](https://cran.r-project.org/web/packages/ggseqlogo/index.html)

You will need to install from [Bioconductor](http://www.bioconductor.org)
* [BiocParallel](https://bioconductor.org/packages/release/bioc/html/BiocParallel.html)
### Before the installation of netMHCpan and netMHCIIpan
Please be sure that you have the right shell to run netMHCpan and netMHCIIpan. They both use the "tcsh", a Unix based shell compatible with cshel (a shel to run c code).

## Installing the netMHCpan and netMHCIIpan softwares
### Check if the appropriate C shell is installed in your machine
To verify if you have it in your machine, please type from a console terminal the following command 
'tsch --version'
if succeed you will see something like this:
![tsch output](https://github.com/elmerfer/RAPInetMHCpan/blob/master/tsch.shell.png)
if not installed try 'sudo apt-get install tcsh' and verify. 

If it is done, you may continue installing netMHCpan and netMHCIIpan through RAPInetMHCpan
### Installation of netMHCpan and netMHCIIpan with RAPInetMHCpan into R
Follow the instructions and fill the form to receive the rights to download [netMHCpan](https://services.healthtech.dtu.dk/service.php?NetMHCpan-4.0) and [netMHCIIpan](https://services.healthtech.dtu.dk/service.php?NetMHCIIpan-3.2) and save them to your favorite directory.
Onpen an R session or RStudio and type:
```R
library(RAPInetMHCpan)
installNetMHCPan(file = "/home/.../myfavoritedir/netMHCpan-VERSION.Linux.tar.gz" , data = NULL, dir = "/where i whant/dir")
installNetMHCIIPan(file = "/home/.../myfavoritedir/netMHCIIpan-VERSION.Linux.tar.gz" , data = NULL, dir = "/where i whant/dir")
```
It will print on console:

`netMHCpan Installation OK`

or

`netMHCIIpan Installation OK`

### Testing the library
Please download and run the following R script file [test.rapiNetMHCpan](https://github.com/elmerfer/RAPInetMHCpan/blob/master/test.rapiNetMHCpan.R)
## Authors

* **Elmer A. Fernández** - *Idea and Initial work* - Centro de Investigación y Desarrollo en Inmunología y Enfermedades Infecciosas (CIDIE) - Univ. Católica de Córdoba - CONICET, Argentina 
* **Macarena Rodriguez Walker** - *Sequence analysis* - Centro de Investigación y Desarrollo en Inmunología y Enfermedades Infecciosas (CIDIE) - Univ. Católica de Córdoba - CONICET, Argentina 
## License

This project is licensed under the GPL 3-0 License 

## Sotware citation
NetMHCpan-4.0: Improved Peptide–MHC Class I Interaction Predictions Integrating Eluted Ligand and Peptide Binding Affinity Data. Vanessa Jurtz 1, Sinu Paul 2, Massimo Andreatta, Paolo Marcatili, Bjoern Peters, and Morten Nielsen. The Journal of Immunology (2017) ji1700893; DOI: 10.4049/jimmunol.1700893 

Improved prediction of MHC II antigen presentation through integration and motif deconvolution of mass spectrometry MHC eluted ligand data.
Reynisson B, Barra C, Kaabinejadian S, Hildebrand WH, Peters B, Nielsen M. J Proteome Res 2020 Apr 30. doi: 10.1021/acs.jproteome.9b00874.




