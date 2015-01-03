
  sigm=function(z){
      1  / ( 1 + exp ( - z ) )
    }
########################################
#'@description genoudBP
#'@description this function is used to build an ANN model based on genoudBP algrithm and returning a model of
#'a S3 class genoudBP
#'@param trian_x : the training input used to build the returning model
#'@param train_y : the training output used to build the returning model
#'@param multi   : see definition of dataSeparation
#'@param hid_num : number of hidden layer nodes
#'@param lambda   : coefficient for tuning regularization
#'@param maxiter : maximum number of Genoud iteration
#'@param cluster : the cluster parameter for Genoud
#'@param center  : see dataSeparation
#'@param scale   : see dataSeparation
#'@param n_out   : see dataSeparation
#'@param BFGS    : a logical value to indicate whether to use BFGS algorithm during genoud
#'@return this function returns a ANN model with S3 class GenoudBP we have implemented two general functons
#'i.e. predict & performaceEval for that class
#'@return n_f    : the number of input dimensions
#'@return w1_fit : W^(1) i.e. the weight matrix between input layer and hidden layer
#'@return w2_fit : W^(2) i.e. the weight matrix between hidden layer and output layer 
#'@return op     : the genoudBP result

genoudBp <- function(train_x,train_y,multi,hid_num,lambda=0.2,maxiter=100,cluster = F,center,scale,n_out,BFGS,pop.size,...){
 

    # sigmoid function ---'----------------------------------------------------
    #'@description the sigmoid is used as the transformation function through out the whole network
    #'
    sigm=function(z){
      1  / ( 1 + exp ( - z ) );
    }
    
    #'@description the loss function
    fn <- function(x){
          
          w1_fun=matrix(x[1:(hid_num*n_f)],nrow = hid_num,byrow = T)
          w2_fun= matrix(x[(hid_num*n_f+1):(hid_num*n_f+ n_out *(hid_num+1) )],nrow=n_out,byrow=T)  #needa change
          if(multi){
             h  <- matrix(0,nrow =train_num,ncol = n_out)
             multi <- T
          }else{
            h=rep(0,train_num)
            
          }
          
          a2 <- rep(0,hid_num)
          a1 <- rep(0,n_f)
          a3  <- rep(0,n_out)
          lapply(1:train_num,function(i){
            a1 <<- train_x[i,]
            a2 <<- sigm(w1_fun%*%a1)
            a2 <<- c(1,a2)
            a3 <<- sigm(w2_fun%*%a2)
            if(multi){
               h[i,] <<- a3
            }else{
              h[i] <<- a3
              
            }
          }
          )
          
          if (multi){
            h  <- as.matrix(h)
          }else{
            h <<- as.vector(h)            
          }
          
          if( multi ){
              J <- -sum(train_y* log(h) + (1-train_y) *log(1-h),c(1,2)) /  (2 *train_num)    
            
          }else{
           
            J <- - ( train_y %*% log(h) + (1-train_y) %*% log(1 - h))  / (2 * train_num )
            
          }
          
          reg= lambda * ( sum( w1_fun[,-1]^2 , c(1,2) ) + sum( w2_fun[,-1]^2 , c(1,2) ) )  / ( 2 * train_num )
          J = J + reg
          J
          
          
        }

    #'@description the gradient function
    grad <- function(x){
          
          w1_fun <- matrix(x[1:(hid_num*n_f)],nrow=hid_num,byrow=T)
          w2_fun= matrix(x[(hid_num*n_f+1):(hid_num*n_f+ n_out *(hid_num+1) )],nrow=n_out,byrow=T)  #needa change
          
          Delta1 <- matrix(0,hid_num,n_f)
          Delta2 <- matrix(0,n_out,hid_num+1)   #needa change
          delta3 <- rep(0,n_out)
          delta2 <- rep(0,hid_num+1)
          #         D2=Delta2;
          #         D1=Delta1;   
          a1 <- as.numeric(rep(0,n_f))
          a2 <- as.numeric(rep(0,hid_num))
          
          lapply(1:train_num,function(x){
            a2 <<- c(1,sigm(w1_fun %*% train_x[x,])) 
            if(multi){
              delta3 <<-  sigm(
                w2_fun %*% a2    
              )  - train_y[x,]  
            }else{
              delta3 <<-  sigm(
                w2_fun %*% a2    
              )  - train_y[x]  
            }
            
                  
            delta2 <<- t(w2_fun)%*%delta3*a2*(1-a2)  
            Delta2 <<- Delta2+delta3%*%t(a2)
            Delta1 <<- Delta1+(delta2%*%t(train_x[x,]))[-1,]
            
          })
                    
              
          
          
          Delta2[,-1] <- Delta2[,-1]/(train_num)+lambda*w2_fun[,-1]
          Delta1[,-1] <- Delta1[,-1]/(train_num)+lambda*w1_fun[,-1]
          Delta2[,1]  <- Delta2[,1]/(train_num)
          Delta1[,1]  <- Delta1[,1]/(train_num)
          
          
          c(as.vector(t(Delta1)),as.vector(t(Delta2)))        
          
          
        }
    

    
        n_f   <- ncol(train_x)
        train_num  <- nrow(train_x)
###########generate sample###############        
             
        nvar=hid_num*n_f+(hid_num+1) * n_out;
         
# initialize --------------------------------------------------------------

        

        w1=runif(hid_num * (n_f),-1,1)
        w2=runif(n_out * (hid_num+1),-1,1)  #need change
        inpar=c(w1,w2);



# optimize ----------------------------------------------------------------
        ##change for multiclass classification
        domains=rep(c(-1,1),nvar);
        domains=matrix(domains,byrow=T,nrow=nvar);

        op=genoud(fn=fn,gr=grad,nvars=nvar,starting.values=inpar,print.level=0,
                  Domains=domains,boundary.enforcement=1,max.generations=maxiter,...,
                  debug=T,gradient.check=F,pop.size=pop.size,cluster = cluster,BFGS =BFGS);

        w1_fit=matrix(op$par[1:(hid_num*(n_f))],byrow=T,nrow=hid_num);
        w2_fit=matrix(op$par[(hid_num*n_f+1):(hid_num*n_f+ n_out *(hid_num+1) )],
                      nrow=n_out,byrow=T);
        

 

    # return value ------------------------------------------------------------
    structure(list(n_f=n_f,hid_num=hid_num,n_out = n_out,w1_fit=w1_fit,w2_fit=w2_fit,likelihood=op$value,
         op = op,multi =multi,center =center,scale =scale),class ="GenoudBP")
  
}