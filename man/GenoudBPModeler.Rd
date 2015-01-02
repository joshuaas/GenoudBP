\name{GenoudBPModeler}
\alias{GenoudBPModeler}

\title{
  the main funcion for modeling
}
\description{
  this function will build an ANN model based on  the given data using genoud algorithm to
optimize BP neuron network 
}
\usage{
GenoudBPModeler(df, separated = F, cluster = F, ratio = 0.85, lambda = 0.2, hid_num, maxiter = 100, BFGS = F, ...)
}

\arguments{
  \item{df}{
 the dataset used to build the model
}
  \item{separated}{
     whether the dataset df is separated
}
  \item{cluster}{
%%     ~~Describe \code{cluster} here~~
}
  \item{ratio}{
 the ratio for training data
}
  \item{lambda}{
see genoudBP
}
  \item{hid_num}{
see genoudBP}
  \item{maxiter}{
see genoudBP}
  \item{BFGS}{
see genoudBP
}

}

\value{

  \item{model }{the genoudBP object generated based on traing data}
  \item{roc_train }{roc performance on traning data}
  \item{roc_test }{roc performance on test    data}
  

}

\author{
Zhiyong Yang}



\seealso{
GenoudBP}
