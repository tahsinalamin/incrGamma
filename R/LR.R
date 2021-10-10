### Author: Sikder Tahsin Al-Amin
### Description: Computes Linear Regression with Gamma Matrix.

library(pracma) #to plot

gamma.test <- function(filename, sparse="Y") {
  library(data.table)
  library(chunkR)
  gamma_cpp <- function(X, Flag1) {
    .Call(`_incrGamma_gamma_cpp`, X, Flag1)
  }
  
  ### LR ####
  LR_with_gamma <-function(gammaMatrix){
    d = length(gammaMatrix[1,])
    Q = gammaMatrix[1:(d-1), 1:(d-1)]
    XYT = gammaMatrix[1:(d-1), d]
    Beta = solve(Q) %*% XYT
    return(Beta)
  }
  
  
  print("**gamma started**")
  print(sparse)
  
  #read the dataset
  partial_gamma <- matrix() #gamma for each chunk
  #ChunkSize = 2000
  ChunkSize = as.integer(sqrt(nrow(X_Matrix)))
  
  ### LR in R ###
  R_beta = lm(X_Matrix$X90 ~ .,data=X_Matrix)$coefficients
  
  #declare the lists to store the relative errors: needed when computing errors in each iteration
  #error_list_LR = list()

  
  t0= proc.time() #time starts
  t_Start = Sys.time()
  time_list = list()
  
  tmp_path <- file.path(getwd(),filename) #construct the path to a file
  chunker_object <- chunker(tmp_path,chunksize = ChunkSize, sep=",", has_colnames = T, has_rownames = F) #create chunk object
  iter = 1
  
  while(next_chunk(chunker_object)){ #reading the chunk
    chunk_table = get_table(chunker_object) #get the chunk
    partial_gamma = gamma_cpp(t(as.matrix(chunk_table)), sparse) #pass the chunk to rcpp
    if(iter==1){
      NumDim = ncol(chunk_table)
      full_gamma <- matrix(0,NumDim+1, NumDim+1) ##full gamma matrix initialized as zero
    }
    full_gamma = full_gamma + partial_gamma  #add partial gamma with the previous gamma
    
    
    ###### Model computation ######
    #LR_beta = LR_with_gamma(full_gamma)
 
    
    #calculate relative error for LR
    relative_error = abs((R_beta-LR_beta)/R_beta)
    error_list_LR[[iter]]=max(relative_error)
    
    
    ## getting the time
    time_list[iter]= (Sys.time()-t_Start)
    
    ##needed when want to finish the execution early
    # if (iter>2){
    #   val1 = abs(error_list_LR[[iter]]-error_list_LR[[iter-1]])
    #   val2 = abs(error_list_LR[[iter]]-error_list_LR[[iter-2]])
    #   if (val1<0.01 & val2<0.01){
    #     break
    #   }
    # }
    
    iter = iter + 1
  }
  
  ## print and check the output
  #print(full_gamma)
  #print(LR_beta)
  #print(error_list_LR)
  
  t1=proc.time()
  elapsed=t1-t0
  print(elapsed) ##prints the total time
  print(paste0("Total blocks: ",iter))
  
  #par(mfrow=c(1,1)) ##to plot multiple plots
  
  #### plot LR relative errors ####
  # j = 1
  # new_error_list_LR = list()
  # for (val in error_list_LR){
  #   if (val<100){  ##if error is greater than 100, discard
  #     new_error_list_LR[j]=val
  #     j = j+1
  #   }
  # }
  
  # x_ticks=1:length(new_error_list_LR)
  # plot((x_ticks/max(x_ticks))*100,c(do.call("rbind",lapply(new_error_list_LR,'[[',1))),xlab="% of Total Data Set",ylab="Relative Error (%)",main="Relative error of coefficients as data set size grows")
 }
