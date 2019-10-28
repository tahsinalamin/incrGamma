### Author: Sikder Tahsin Al Amin
### Description: Compute Naive Bayes model with Gamma matrix.


NB.gamma <- function(filename,class="-1"){
  library(data.table)
  library(chunkR)
  library(e1071)
  k_gamma_cpp <- function(X, k, class_col) {
    .Call(`_incrGamma_k_gamma_cpp`, X, k, class_col)
  }
  
  k_gamma_diagonal <- function(X, k, class_col) {
    .Call(`_incrGamma_k_gamma_diagonal`, X, k, class_col)
  }
  
  ##### Naive Bayes #####
  
 NB_modelFromGamma = function(final.gamma, Nglobal, NumDimen){
    N = list()# contain the number of records correspoding to a certain class label at each index
    L = list()# contain the sum of each column correspoding to a certain class label at each index
    Q = list()# contain the sum of squares of each column correspoding to a certain class label at each index
    prior = list()# contain the probability of number of records correspoding to a certain class label at each index
    mu = list()# contain the means of each column correspoding to a certain class label at each index
    sigma = list()#contain the variance of each column correspoding to a certain class label at each index
    
    #### populate the N, L, Q, prior, mu, sigma lists to construct the Naive Bayes model
    for(index in 1:2){
      N[[index]] =final.gamma[1,(index-1)*2+1]
      L[[index]] = final.gamma[2:nrow(final.gamma),(index-1)*2+1]
      Q[[index]] = final.gamma[2:nrow(final.gamma),(index-1)*2+2]
      
      prior[[index]] = N[[index]]/Nglobal #W
      mu[[index]] = L[[index]][1:NumDimen-1]/N[[index]] #C NumDimen-1: without the class columns
      sigma[[index]] = Q[[index]][1:NumDimen-1]/N[[index]] - ((L[[index]][1:NumDimen-1]/N[[index]])^2) #R
    }
    returnList = list()
    returnList[[1]] = prior
    returnList[[2]] = mu
    returnList[[3]] = sigma
    
    return(returnList)
  }
  
  model.prediction <- function(filename, model, model_name){
    naiveBayesPrediction <- function(X_Test, mu, sigma, prior, class_labels) {
      .Call(`_incrGamma_naiveBayesPrediction`, X_Test, mu, sigma, prior, class_labels)
    }
    
    X_Test_data = read.csv(filename, header = FALSE)
    X_Test = X_Test_data[,-ncol(X_Test_data)] #without class label column
    
    if(model_name == "NB")
    {
      prior = model[[1]] #W
      mu = model[[2]] #C
      sigma = model[[3]] #R
      class_labels = model[[4]]
      
      prob = data.frame(naiveBayesPrediction(as.matrix(X_Test), mu, sigma, prior, class_labels[[1]]))
      x <- c('i','g',colnames(X_Test))
      colnames(prob) <- x
      
      prob = transform(prob, prod = Reduce('*', prob[,3:ncol(prob)]))# add a column prod which contains the product of the independent variables in each row
      result = do.call(rbind,lapply(split(prob,prob$i),function(chunk) chunk[which.max(chunk$prod),]))[,1:2]# to give the predicted class label for each row in test dataset    
    }
  }
  
  print("**k_gamma started**")
  ChunkSize = as.integer(sqrt(nrow(X_Matrix)))
  
  ##reading the csv file
  #X_Matrix = read.csv(filename, header =FALSE)
  # X_Matrix<-na.omit(X_Matrix) ## discard None values  
  
  #get the class column number. if no class column provided, then last column is class column
  # if(class != "-1"){
  #   class_col = which(colnames(X_Matrix)==class)
  # }
  # else
  #   class_col = NumDim
  # print(paste0("Class column number =",class_col))
  
  #set number of unique classes (k) from the class_col
  k=2
  
  #declare the gammas
  partial_gamma <- matrix()
  
  ### NB in R ###
  # NB_R= naiveBayes(X_Matrix$V31~ .,data = X_Matrix)
  # NB_prior_R = max(NB_R$apriori/nrow(X_Matrix))
  # NB_musigma_R = do.call(rbind.data.frame, NB_R$tables)
  # NB_mu_R = max(NB_musigma_R$V1)
  
  
  #declare the lists to store the relative errors
  NB_error_prior_list=list()
  NB_error_mu_list = list()
  
  t0= proc.time() #starting time
  t_Start = Sys.time()
  time_list = list()
  
  #read the dataset by chunk
  tmp_path <- file.path(getwd(),filename)
  chunker_object <- chunker(tmp_path,chunksize = ChunkSize, sep=",", has_colnames = T, has_rownames = F) #create chunk object
  #get_completed(chunker_object) #get the number of lines already read
  iter = 1
  
  #print("loop starts")
  while(next_chunk(chunker_object)){ #reading the chunk
    chunk_table = get_table(chunker_object) #get the chunk
    if(iter==1){
      NumDim = ncol(chunk_table)
      full_gamma <- matrix(0,NumDim+1,k*2) ##full gamma matrix initialized as zero
      class_col = NumDim
    }
    #k_list_gamma[[iter]] = k_gamma_diagonal(t(as.matrix(chunk_table)),as.integer(k), as.integer(class_col)) #pass the chunk to rcpp
    partial_gamma = k_gamma_diagonal(t(as.matrix(chunk_table)),as.integer(k), as.integer(class_col)) #pass the chunk to rcpp
    full_gamma = full_gamma + partial_gamma
    Nglobal = get_completed(chunker_object)
    
    ### compute models ###
    NB_gamma = NB_modelFromGamma(full_gamma, Nglobal, NumDim)
    #NB_gamma[[4]] = class_labels
    
    ##calculate relative error of NB
    # prior_gamma = max(sapply(NB_gamma[[1]], max))
    # NB_prior_error = abs((NB_prior_R-prior_gamma)/NB_prior_R)
    # NB_error_prior_list[iter]= NB_prior_error
    # #
    # mu_gamma = max(sapply(NB_gamma[[2]], max))
    # NB_mu_error=abs((NB_mu_R - mu_gamma)/NB_mu_R)
    # NB_error_mu_list[iter]=NB_mu_error

    #### to finish execution early
    #time_list[iter]= (Sys.time()-t_Start)
    # if (iter>2){
    #   if (abs((NB_error_mu_list[[iter]]-NB_error_mu_list[[iter-1]]))<0.0001 &
    #       (abs(NB_error_mu_list[[iter]]-NB_error_mu_list[[iter-2]]))<0.0001){
    #     break
    #   }
    # }
    
    iter = iter + 1
  }
  
  t1=proc.time()
  elapsed=t1-t0
  print(elapsed)
  print(paste0("total blocks = ",iter))
  
  #####prediction of models #######
  #print(NB_gamma)
  # print(NB_error_prior_list[1])
  # NB_prediction = model.prediction("creditcardTest.csv",NB_gamma,"NB")
 
 # ## plot NB errors ##
  #par(mfrow=c(1,1)) ##to plot multiple plots
  #x_ticks=1:length(NB_error_mu_list)
  #plot((x_ticks/max(x_ticks))*100,c(do.call("rbind",lapply(NB_error_prior_list,'[[',1))),main="NB -> prior ")
  #plot((x_ticks/max(x_ticks))*100,c(do.call("rbind",lapply(NB_error_prior_list,'[[',1))),xlab="% of Total Data Set",ylab="Relative Error (%)",main="Relative error of prior as data set size grows")
  #plot((x_ticks/max(x_ticks))*100,c(do.call("rbind",lapply(NB_error_mu_list,'[[',1))),xlab="% of Total Data Set",ylab="Relative Error (%)",main="Relative error of mean as data set size grows")
  
 }
