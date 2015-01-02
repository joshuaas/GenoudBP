\name{genoudBp}
\alias{genoudBp}
\title{
 genoudBP training function
}
\description{
this function is used to build an ANN model based on genoudBP algrithm and returning a model of
a S3 class genoudBP}
\usage{
genoudBp(train_x, train_y, multi, hid_num, lambda = 0.2, maxiter = 100, cluster = F, center, scale, n_out, BFGS = BFGS, ...)
}
\arguments{
  \item{train_x}{
  the training input used to build the returning model
}
  \item{train_y}{
  the test input used to build the returning model

}
  \item{multi}{
  see definition of dataSeparation
}
  \item{hid_num}{
  number of hidden layer nodes
}
  \item{lambda}{
   coefficient for tuning regularization
}
  \item{maxiter}{
  maximum number of Genoud iteration
}
  \item{cluster}{
  the cluster parameter for Genoud
}
  \item{center}{
  see dataSeparation
}
  \item{scale}{
  see dataSeparation
}
  \item{n_out}{
  see dataSeparation
}
  \item{BFGS}{
  a logical value to indicate whether to use BFGS algorithm during genoud
}
}
 
\value{
  \item{n_f  }{the number of input dimensions}
  \item{w1_fit }{ W^{(1)} i.e. the weight matrix between input layer and hidden layer}
  \item{w2_fit }{W^(2) i.e. the weight matrix between hidden layer and output layer}
  \item{op }{the genoudBP result}

}

\author{
    Zhiyong Yang
}
