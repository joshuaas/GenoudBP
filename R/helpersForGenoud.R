#! /usr/bin/Rscript
  #'@description this is the general function for performance evaluation 
    performanceEval <- function(object,...) UseMethod("performanceEval")
      
  
   
    #'@description dataSeparation NOTE THAT THE OUTPUT VARIABLE SHOULD BE LOCATED AT THE LAST COLUMN !!!
    #'@description this function is used to separate the input dataset into a training set with ratio % 
    #'of the whole dataset and test set with (1 - ratio) % of the dataset 
    #'@param dataset:  the dataset to be separated
    #'@param ratio  :  ratio of the training set size
    #'@return this function returns a DataSep object, which includes both the input(train_x) and 
    #'desired output for training and test set respectively
    #'@return train_x : the input of training data
    #'@return train_y : the desired output for training data
    #'@return test_x : the input of test data
    #'@return test_y : the desired output for test data
    #'@return n_out  : the number of dimensions for the output, n_out = 1 if binary classification is executed,
    #'n_out >1 otherwise.
    #'@return multi : indicator for whether the dataset is a multiclass classification problem 
    #'@return center : generated during data scaling ,center is a vector with each element of which being 
    #'the mean value for the coresponding variable(column)
    #'@return std : generated during data scaling, std is a vector with each element of which being the stadard deviation
    #'of the corresponding variable(column)
    
    dataSeparation  <- function(dataset,ratio = 0.85){
        multi <- judgeMulti(dataset)
        n_out <- 1
        dataset <- na.omit(dataset)
        data <- scale(dataset[,-ncol(dataset)])
        center <- attr(data,"scaled:center")
        std <- attr(data,"scaled:scale") 
        if(multi){
          tar <- label2Matrix(dataset[,ncol(dataset)])
          n_out <- ncol(tar)
          }else{
          tar <- as.matrix(dataset[,ncol(dataset)])
        } 
        
        data <- cbind(rep(1,nrow(dataset)),data)
        force(ratio)
        train_num  <- round(nrow(data) * 0.85)
        index <- sample(nrow(dataset),replace=F);
        train_x <- as.matrix(data[index[1:train_num],]);
        train_y <- tar[index[1:train_num],];
        test_x <- as.matrix(data[index[(train_num+1):nrow(dataset)],]);
        test_y <- tar[index[(train_num+1):nrow(dataset)],];
        
        
        structure(list(train_x =  train_x,train_y = train_y,test_x =test_x,test_y =test_y ,
                       n_out =n_out,multi =multi,center = center,std =std ),class = "SepData")
    }
    
    #'@description judge whether a given dataset is a multiclass dataset
    judgeMulti  <- function(df){
        uniq <- unique(df[, ncol(df) ])
          if( length(uniq) == 2 && ( ( sum(uniq ==c(1,0)) ==2 ) || (sum(uniq ==c(0,1)) ==2) ) ) {
            multi  <- F
            n_out  <- 1
          }else if(length(uniq) == 2){
            stop('turn binary class into sequence of 0,1')
            
          }else{
            multi  <- T
            n_out  <- length(unique(df[,ncol(df)]))
          }
          multi
    }
    
    # roc display function ---'----------------------------------------------------
    #'@description : this function is used to display the roc curve for a given roc object in the input 
    #'@param roc :a roc object to be displayed
    #'@param classLabel: the classlabel or the class name of which the roc includes
    #'@param dataLabel :used to indicate whether a training data or a test data is used in the given roc object
    
    rocDisplay <- function(roc,classLabel,dataLabel){
      plot(roc,print.auc=T,auc.polygon=TRUE, 
           max.auc.polygon=TRUE,
           auc.polygon.col="skyblue", print.thres=TRUE, main = paste(" ROC Curve for",classLabel,dataLabel))
    }

    #'@description : this function uses a genoudBP object to evaluate the output of a test data
    #'@param model : a genoudBP object to be used
    #'@param test  : a test input 
    pred.GenoudBp <- function(model,test){
          multi = model$multi
          test_num <- nrow(test)
          n_out    <- model$n_out
          n_f      <- model$n_f
          w1_fit   <- model$w1_fit
          w2_fit   <- model$w2_fit
          
        #initialize output h  multi is the indicator for whether a multiclass classification
        #should be executed
        
        if(multi){
            h  =matrix(0,nrow  = test_num,ncol = n_out)   
          }else{
            h=rep(0,test_num)
            
          }
          
          a1  <- rep(0,n_f)
          lapply(1:test_num,function(i){
            a1 <<- test[i,]
            if(multi){
              h[i,] <<- sigm(w2_fit%*%c(1,sigm(w1_fit%*%a1)))
              
            }else{
              h[i] <<- sigm(w2_fit%*%c(1,sigm(w1_fit%*%a1)))
              
            }
            
          }
          )
          h
    }
    
    # label to matrix-----------------------------------------------------------------------------
    #'@description this function is used to convert a multiclass label vector into a dummy variable matrix
    #'@details the unique class labels in the label vector will be first evaliated ,which is used generate 
    #'the binary matrix y of 0 or 1. y_ij is exactly the indicator of whether the ith sample belongs to the
    #'jth class 
    #'@param label: the label column for your multiclassification dataset
            
    label2Matrix  <- function(label){
      #calculate the number of different labels 
      type  <-  length(unique(label))
      
      #generate dummy matrix y
      n <- length(label)
      y <- matrix(0,nrow = n,ncol = type)
      lapply(1:n,function(i){
        y[i,] <<-as.numeric(label[i] == unique(label) )
      })
      
      colnames(y) = (unique(label))
            
      y
    }
    
    
    #'@description : an implementaion for preformanceSEval with GenoudBP object
    #'@param model : a genoudBP object
    #'@param x     : input data
    #'@param y     : output data
    #'@seealso rocDisplay , predict.GenoudBP for more help
    performanceEval.GenoudBP <- function(model,x,y,scaled =F,dataLabel){
         # prediction --------------------------------------------------------------
       
        res   <- predict(model,x,scaled)
        multi <- model$multi
        n_out <- model$n_out
        
    ##remember to modify the roc evaluation for multiclass problem
    if(multi){
       
       res_roc <- NULL
       
       lapply(1:n_out,function(i){
         r  <-   roc(response = y[,i],predictor =res[,i])
         
         if(identical(res_roc,NULL)){
             res_roc <<-r
           
            
        
          }else{
            res_roc <<- list(res_roc,r)

          }
         rocDisplay(r,colnames(y)[i],dataLabel)
         
       })
       
       roc_total <- roc(response =y,predictor = res )
       rocDisplay(roc_total,"overall roc",dataLabel)

    }else{
        res_roc=roc(predictor=res,response=y)
        rocDisplay(res_roc,"",dataLabel)
        
    }
        res_roc
    }
    
    #'@description : normalize the test data with mean center and standard deviation scale
    normalizeData <- function(center,scale,test){
        test1 <- sweep(x = test,MARGIN = 2,FUN = '-',center)
        test1 <- sweep(x =test1,FUN = '/',scale)
    }   
    
    #'@description : a predict implementation for genoudBP object(recall that,in R, predict function is a 
    #'general function for different types of objects)
    #'@param object : a genoudBP object to be used
    #'@param test   : a test data
    #'@param scaled :a logical value for  whether the test dataset is scaled.If scaled = F then the test will
    #'be scaled according to the training data used to build the object
    predict.GenoudBP <- function(object,test,scaled =F){
        if(!scaled){
            test <- normalizeData(object$center,object$scale,test)
        }

        pred.GenoudBp(object,test)
    }
    
    