### Author: Sikder Tahsin Al-Amin
### Description: Computes Linear Regression and PCA incrementally with Gamma Matrix.

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
  
  ### PCA ###
  PCA_with_gamma = function(gammaMatrix){
    d_plus2 = length(gammaMatrix[1,])
    d = d_plus2 -1
    L = gammaMatrix[2:d,1]
    Q = gammaMatrix[2:d, 2:d]
    n = gammaMatrix[1,1]
    corr.matrix = matrix(nrow = length(Q[1,]), ncol = length(Q[1,]),0)
    for (a in 1:length(Q[1,])) {
      denom_1 = sqrt((n*Q[a,a]) - (L[a]*L[a]))
      for (b in 1:length(Q[1,])) {
        numerator = ((n*Q[a,b]) - (L[a]*L[b]))
        denom_2 = sqrt((n*Q[b,b]) - (L[b]*L[b]))
        corr.matrix[a,b] = numerator/(denom_1*denom_2)
      }
    }
    return(svd(corr.matrix))
  }
  
  print("**gamma started**")
  print(sparse)
  
  #read the dataset
  #X_Matrix = read.csv(filename, header =TRUE)
  partial_gamma <- matrix() #gamma for each chunk
  #ChunkSize = 2000
  ChunkSize = as.integer(sqrt(nrow(X_Matrix)))
  
  ### LR in R ###
  #R_beta = lm(X_Matrix$X90 ~ .,data=X_Matrix)$coefficients
  
  ### PCA in R ###
  #svd_R = svd(cor(X_Matrix[,1:ncol(X_Matrix)-1]))
  
  #declare the lists to store the relative errors: needed when computing errors in each iteration
  #error_list_LR = list()
  #error_list_PCA_d = list()
  #error_list_PCA_u = list()
  #error_list_PCA_v = list()
  
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
    PCA_gamma = PCA_with_gamma(full_gamma)
    
    #calculate relative error for LR
    # relative_error = abs((R_beta-LR_beta)/R_beta)
    # error_list_LR[[iter]]=max(relative_error)
    
    #calculate relative error for PCA
    # relative_error_d = abs((max(svd_R$d) - max(PCA_gamma$d))/max(svd_R$d))
    # relative_error_u = abs((svd_R$u - PCA_gamma$u)/svd_R$u)
    # relative_error_v = abs((max(svd_R$v) - max(PCA_gamma$v))/max(svd_R$v))
    # error_list_PCA_d[iter] = relative_error_d
    # error_list_PCA_u[iter] = max(relative_error_u)
    # error_list_PCA_v[iter] = relative_error_v
    
    ## getting the time
    
    #time_list[iter]= (Sys.time()-t_Start)
    
    ##needed when want to finish the execution early
    # if (iter>2){
    #   val1 = abs(error_list_PCA_d[[iter]]-error_list_PCA_d[[iter-1]])
    #   val2 = abs(error_list_PCA_d[[iter]]-error_list_PCA_d[[iter-2]])
    #   if (val1<0.01 & val2<0.01){
    #     break
    #   }
    # }
    
    iter = iter + 1
  }
  
  #print(full_gamma)
  
  #gammaR = gamma_cpp(t(as.matrix(X_Matrix)),sparse) #compute gamma as old method
  
  # print(LR_beta)
  # print("beta with full gamma..")
  # print(LR_with_gamma(gammaR)) #to prove both method gives same result
  
  # print(PCA_gamma)
  # print("SVD with full gamma..")
  # print(PCA_with_gamma(gammaR)) #to prove both method gives same result
  
  t1=proc.time()
  elapsed=t1-t0
  print(elapsed) ##prints the total time
  #print(error_list_LR)
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
  
  #### plot PCA relative errors ####
  #x_ticks=1:length(error_list_PCA_d)
  #plot((x_ticks/max(x_ticks))*100,c(do.call("rbind",lapply(error_list_PCA_d,'[[',1))),xlab="% of Total Data Set",ylab="Relative Error (%)",main="Relative error of eigen values as data set size grows")
 }
