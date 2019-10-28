### Author: Sikder Tahsin Al Amin
### Description: Compute Kmeans clustering algo exploiting Gamma matrix.


kmeans.gamma <- function(filename, k){
  library(data.table)
  library(chunkR)
  assignCluster <- function(X, cluster_centroids, k) {
    .Call(`_incrGamma_assignCluster`, X, cluster_centroids, k)
  }
  
  #### kmeans ####
  kmeansModelFromGamma = function(final.gamma, Nglobal, NumDimen,k){
    N = list()# contain the number of records correspoding to a certain class label at each index
    L = list()# contain the sum of each column correspoding to a certain class label at each index
    Q = list()# contain the sum of squares of each column correspoding to a certain class label at each index
    W = list()# contain the probability of number of records correspoding to a certain class label at each index
    C = list()# contain the means of each column correspoding to a certain class label at each index
    R = list()#contain the variance of each column correspoding to a certain class label at each index
    
    #### populate the N, L, Q, prior, mu, sigma lists to construct the Naive Bayes model
    for(index in 1:k){
      N[[index]] =final.gamma[1,(index-1)*2+1]
      L[[index]] = final.gamma[2:nrow(final.gamma),(index-1)*2+1]
      Q[[index]] = final.gamma[2:nrow(final.gamma),(index-1)*2+2]
      
      C[[index]] = L[[index]]/N[[index]]
      R[[index]] = Q[[index]]/N[[index]] - L[[index]]*L[[index]]/(N[[index]]^2)
      W[[index]] = N[[index]]/Nglobal
    }
    returnList = list()
    returnList[[1]] = C
    returnList[[2]] = R
    returnList[[3]] = W
    
    return(returnList)
  }
  
  print("**kmeans_gamma started**")
  ChunkSize = as.integer(sqrt(nrow(X_Matrix)))
  
  ##reading the csv file
  X_Matrix = read.csv(filename, header =TRUE)
  
  ### k-means() in R ###
  km_R = kmeans(X_Matrix[-ncol(X_Matrix)],k)
  c_km_R = max(km_R$centers)
  
  ##declare the gammas
  partial_gamma <- matrix()
  
  ##error list
  #km_c_error_list = list()
  
  t0= proc.time() #starting time
  t_Start = Sys.time()
  time_list = list()
  
  #read the dataset by chunk
  tmp_path <- file.path(getwd(),filename)
  chunker_object <- chunker(tmp_path,chunksize = ChunkSize, sep=",", has_colnames = T, has_rownames = F) #create chunk object
  iter = 1
  
  while(next_chunk(chunker_object)){ #reading the chunk
    chunk_table = get_table(chunker_object) #get the chunk
    X = chunk_table[,-ncol(chunk_table)] #remove the class label column
    if(iter==1){
      NumDim = ncol(X)
      full_gamma <- matrix(0,NumDim+2,k*2)
      to_chunk_gamma <- matrix(0,NumDim+2,k*2)
      class_col = NumDim
    }
    
    ### process the chunk to get cluster numbers ###  
    
    #declare the lists
    sum_of_error = 0.0
    cluster_centroids=list()
    cluster_id = list()
    pass=1
    prev_sum_of_error = 0.0
    
    while(TRUE){
      #compute cluster centroids randomly only for the first time and for first chunk
      if (pass==1){
        random_numbers = sample(1:nrow(X), k) #generate random numbers for first iter cluster centroids
        for(clusters in 1:k){
          cluster_centroids[[clusters]] = unlist(X[random_numbers[clusters],])
        }
      }
      
      #get cluster numbers
      ret = assignCluster((as.matrix(X)), cluster_centroids, length(cluster_centroids))#assign the cluster number to each row in dataset
      cluster_id[[iter]] = (ret[[1]])#get the cluster number list for all rows in the dataset
      sum_of_error =  ret[[2]]#get sum of error value to break out of loop when it reaches a constant value
      
      X$Cluster = cluster_id[[iter]]#append cluster_ids to the original data in order to send it to construct gamma matrix
      
      ### compute k-gamma on new X and get the model ###
      partial_gamma = k_gamma_diagonal(t(as.matrix(X)),as.integer(k), as.integer(class_col+1)) #pass the chunk to rcpp
      to_chunk_gamma = full_gamma + partial_gamma
      Nglobal = get_completed(chunker_object)
      
      #get the model
      CRW = kmeansModelFromGamma(to_chunk_gamma, Nglobal, NumDim,k)
      C = CRW[[1]]
      
      if(sqrt(abs(prev_sum_of_error - sum_of_error))< 1)
        break;
        
      prev_sum_of_error = sum_of_error
      cluster_centroids = C
      pass = pass + 1
    }
    full_gamma = full_gamma + partial_gamma
    
    #get the errors
    CRW_gamma = kmeansModelFromGamma(full_gamma, Nglobal, NumDim,k)
    c_gamma = max(sapply(CRW_gamma[[1]], max))
    km_c_error = abs((c_km_R-c_gamma)/c_km_R)
    km_c_error_list[[iter]] = km_c_error
    
    ##to finish execution early.
    #time_list[iter]= (Sys.time()-t_Start)
    # if (iter>2){
    #   if (abs((km_c_error_list[[iter]]-km_c_error_list[[iter-1]]))<0.0001 &
    #       (abs(km_c_error_list[[iter]]-km_c_error_list[[iter-2]]))<0.0001){
    #     break
    #   }
    # }
    
    iter = iter + 1
  }
  
  t1=proc.time()
  elapsed=t1-t0
  print(elapsed) ##prints total time
  print(paste0("Total blocks = ",iter))
 
  # par(mfrow=c(1,1)) ##to plot multiple plots
  #x_ticks=1:length(km_c_error_list)
  # plot((x_ticks/max(x_ticks))*100,c(do.call("rbind",lapply(km_c_error_list,'[[',1))),xlab="% of Total Data Set",ylab="Relative Error (%)",main="Relative error of mean as data set size grows")
 }
