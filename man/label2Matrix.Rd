\name{label2Matrix}
\alias{label2Matrix}
\title{
  convert multiclass label to dummy matrix
}
\description{
this function is used to convert a multiclass label vector into a dummy variable matrix}
\usage{
label2Matrix(label)
}
\arguments{
  \item{label}{
   classlabel
}
}
\details{
the unique class labels in the label vector will be first evaliated ,which is used generate the binary matrix y of 0 or 1. y_ij is exactly the indicator of whether the ith sample belongs to the
    jth class }
