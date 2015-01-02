\name{rocDisplay}
\alias{rocDisplay}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
  display performance with ROC 
}
\description{
this function is used to display the roc curve for a given roc object in the input }
\usage{
rocDisplay(roc, classLabel, dataLabel)
}
\arguments{
  \item{roc}{
a roc object to be displayed}
  \item{classLabel}{
the classlabel or the class name of which the roc includes}
  \item{dataLabel}{
used to indicate whether a training data or a test data is used in the given roc object}
}
