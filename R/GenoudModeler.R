#'@description main function for genoud modeling
#'@description : this function will build an ANN model based on  the given data using genoud algorithm to
#'optimize BP neuron network 
#'@param df : the dataset used to build the model
#'@param separated: whether the dataset df is separated
#'@param ratio : the ratio for training data
#'@param lambda : see genoudBP
#'@param hid_num :see genoudBP
#'@param maxiter : see genoudBP
#'@param BFGS see genoudBP
#'@return model the genoudBP object generated based on traing data
#'@return roc_train : roc performance on traning data
#'@return roc_test  : roc performance on test    data

GenoudBPModeler <- function(df,separated =F,cluster =F,ratio = 0.85,lambda =0.2,hid_num,maxiter =100,BFGS=F,pop.size=40,...){

        if(!separated){
            df <- dataSeparation(df,ratio)
        
        }
        
        model <- genoudBp(train_x = df$train_x, train_y = df$train_y, multi = df$multi,
                          n_out = df$n_out, center = df$center, scale = df$std
                          ,hid_num = hid_num ,maxiter = maxiter, lambda =lambda, cluster = cluster,BFGS =BFGS,pop.size = pop.size)
        
        roc_train <- performanceEval(model, df$train_x , df$train_y , scaled = T, "training data")
        roc_test <- performanceEval(model, df$test_x, df$test_y, scaled = T, "test data")
        
        list(model =model, roc_train = roc_train, roc_test = roc_test, dataset =df, center = df$center, scale  = df$scale)

}