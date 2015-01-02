\name{dataSeparation}
\alias{dataSeparation}

\title{
 data separation function 
}
\description{
  dataSeparation NOTE THAT THE OUTPUT VARIABLE SHOULD BE LOCATED AT THE LAST COLUMN !!!
   this function is used to separate the input dataset into a training set with ratio % 
   of the whole dataset and test set with (1 - ratio) % of the dataset 
}
\usage{
dataSeparation(dataset, ratio = 0.85)
}

\arguments{
  \item{dataset}{
     the dataset to be separated
}
  \item{ratio}{
     ratio of the training set size
}
}

\value{
   this function returns a DataSep object, which includes both the input(train_x) and 
    desired output for training and test set respectively
  \item{train_x }{the input of training data}
  \item{train_y }{the desired output for training data}
  \item{test_x }{the input of test data}
  \item{test_y }{the desired output for test data}
  \item{n_out }{he number of dimensions for the output, n_out = 1 if binary classification is executed,n_out >1 otherwise.}
  \item{multi }{indicator for whether the dataset is a multiclass classification problem }
  \item{center }{generated during data scaling ,center is a vector with each element of which being 
the mean value for the coresponding variable(column)}
  \item{std }{generated during data scaling, std is a vector with each element of which being the stadard deviation of the corresponding variable(column)}

}

\author{
    Zhiyong Yang
}
\note{
  NOTE THAT THE OUTPUT VARIABLE SHOULD BE LOCATED AT THE LAST COLUMN !!!
}



\keyword{ ~data }
\keyword{ ~Separation }% __ONLY ONE__ keyword per line
