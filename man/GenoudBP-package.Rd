\name{GenoudBP-package}
\alias{GenoudBP-package}
\alias{GenoudBP}
\docType{package}
\title{
building ANN with BP and Genoud
}
\description{
this package is ANN toolbox ,where the early publised Genoud Algorithm and the rgenoud package is employed to Optimize BP process
}
\details{
\tabular{ll}{
Package: \tab GenoudBP\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-01-02\cr
License: \tab GPL\cr
}

    data(pima)
    GenoudBPModeler(pima,hid_num =9)
}
\author{
      Zhiyong Yang

Maintainer: Zhiyong Yang <1079637988@qq.com>

}
\references{
    Walter Mebane, Jr. and Jasjeet S. Sekhon. 2011.Genetic Optimization Using Derivatives: The rgenoud package for R.Journal of Statistical Software, 42(11): 1-26.
    
  Xavier Robin, Natacha Turck, Alexandre Hainard, Natalia Tiberti, Frédérique Lisacek,Jean-Charles Sanchez and Markus Müller (2011). pROC: an open-source package for R and S+ to analyze and compare ROC curves. BMC Bioinformatics, 12, p. 77.  DOI: 10.1186/1471-2105-12-77
}

\examples{
   ##for multicore version
   require(parallel)
   data(pima)
   cl  <- makeCluster(getOption("cl.cores",detectCores(logical =FALSE)))  
   f  <- GenoudBPModeler(pima,hid_num =9,cluster =cl )
}
