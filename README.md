# RAPInetMHCpan
RAPInetMHCpan: a library to facilitate the instaltion and used of both netHCpan and netMHCIIpan neural netowrks for prediction of peptide binders

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
[openxlsx](https://cran.r-project.org/web/packages/openxlsx/index.html)
[ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html)
[stringr](https://cran.r-project.org/web/packages/stringr/index.html)
[parallel](https://cran.r-project.org/web/packages/parallel/index.html)
You will need to install from [Bioconductor](http://www.bioconductor.org)
[BiocParallel](https://bioconductor.org/packages/release/bioc/html/BiocParallel.html)

## Authors

* **Elmer A. Fernández** - *Idea and Initial work* - [elmerfer](https://github.com/elmerfer)

## License

This project is licensed under the GPL 3-0 License 



