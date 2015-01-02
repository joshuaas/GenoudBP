\name{predict.GenoudBP}
\alias{predict.GenoudBP}
\title{
 predict for GenoudBP
}
\description{
a predict implementation for genoudBP object(recall that,in R, predict function is a 
general function for different types of objects)
}
\usage{
predict.GenoudBP(object, test, scaled = F)
}
\arguments{
  \item{object}{
 genoudBP object to be used}
  \item{test}{
 test data}
  \item{scaled}{
a logical value for  whether the test dataset is scaled.If scaled = F then the test will
    be scaled according to the training data used to build the object}
}

