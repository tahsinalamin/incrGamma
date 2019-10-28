// Author: Sikder Tahsin Al Amin
// Description: Computes the gamma and k-gamma matrices and other c++ functions in Rcpp.

#include <Rcpp.h>
using namespace Rcpp;

// This is a simple function using Rcpp that creates an R list
// containing a character vector and a numeric vector.
//
// Learn more about how to use Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//
// and browse examples of code using Rcpp at:
//
//   http://gallery.rcpp.org/
//



// [[Rcpp::export]]
NumericMatrix gamma_cpp_dense(NumericMatrix X) {
  /* int a,b,i; */
  int n = X.ncol();
  int d = X.nrow();
  if(n<d) { Rprintf("Error: gamma() n<d "); return 0; }
  //NumericVector x_i(d+1);
  double *x= new double[d+1];
  NumericMatrix G(d+1,d+1);
  
  for(int i = 0; i < n; i++) {
    //for(int a=0;a<d;a++) x_i(a)= X(a,i);
    x[0]=1;
    for(int a=0;a<d;a++) x[a+1]= X(a,i);
    
    //x_i(d)= 1;R
    for(int a = 0; a <= d; a++) {
      for (int b = 0; b <= a; b++) {
        //G(a,b) += x_i(a)*x_i(b); // slow, why?
        G(a,b) += x[a]*x[b];   // fastest
        // G(a,b) += X(a,i)*X(b,i);   // fast
      }
    }
    
  }
  for(int a = 0; a <= d; a++)  // copy lower triangle
    for (int b = a+1; b <= d; b++) G(a,b)= G(b,a);
  
  
  
  return G;
}


// [[Rcpp::export]]
NumericMatrix gamma_cpp_sparse(NumericMatrix X){
  /* int a,b,i; */
  int n = X.ncol();
  int d = X.nrow();
  if(n<d) { printf("Error: gamma() n<d "); return 0; }
  //NumericVector x_i(d+1);
  double *x= new double[d+1];
  double *v = new double[d+1];
  int *j = new int[d+1];
  NumericMatrix G(d+1,d+1);
  
  for(int i = 0; i < n; i++) {
    //for(int a=0;a<d;a++) x_i(a)= X(a,i);
    int k = 0;
    x[0]=1;
    for(int a=0;a<d;a++) x[a+1]= X(a,i);
    
    for(int a=0;a<=d;a++){
      if(x[a]!=0){
        v[k]= x[a];
        j[k] = a;
        k += 1;
        
      }
    }
    //x_i(d)= 1;
    //Rcpp::Rcout<<"\n V end \n"<<G<<std::endl;
    for(int a = 0; a < k; a++) {
      for (int b = 0; b <= a; b++) {
        //Rcpp::Rcout<<a<<"\t"<<b<<std::endl;
        //G(a,b) += x_i(a)*x_i(b); // slow, why?
        G(j[a],j[b]) += v[a]*v[b];   // fastest
        //G(a,b) += X(a,i)*X(b,i);   // fast
      }
    }
  }
  for(int a = 0; a <= d; a++)  // copy lower triangle
    for (int b = a+1; b <= d; b++) G(a,b)= G(b,a);
  
  return G;
}


// [[Rcpp::export]]
NumericMatrix gamma_cpp_diagonal(NumericMatrix X) {
  /* int a,b,i; */
  //X = transpose(X);
  int n = X.ncol();
  int d = X.nrow();
  //if(n<d) { printf("Error: gamma() n<d "); return 0; }
  //NumericVector x_i(d+1);
  double *x= new double[d+1];
  NumericMatrix G(d+1,d+1);
  
  for(int i = 0; i < n; i++) {
    //for(int a=0;a<d;a++) x_i(a)= X(a,i);
    x[0]=1;
    for(int a=0;a<d;a++) x[a+1]= X(a,i);
    
    //x_i(d)= 1;
    for(int a = 0; a <= d; a++) {
      G(a,0) += x[a]*x[0]; //computing L
      if(a!=0)
        G(a,a) += x[a]*x[a]; //computing Q
    }
  }
  for(int a = 0; a <= d; a++)  // copy lower triangle
    for (int b = a+1; b <= d; b++) G(a,b)= G(b,a);
  
  return G;
}


// [[Rcpp::export]]
List k_gamma_cpp(NumericMatrix X, int k, int class_col) {
  int n = X.ncol();
  int d = X.nrow();
  int j1=0; //subscripts
  int j2=0;
  //return List::create(Named("1")=n,Named("2")=d); //for efficiency by Dirk eddelbuttel
  Rprintf("number of rows=%d, and columns=%d\n",d,n);
  
  NumericMatrix x1(d,n); // size of x1? inefficient
  NumericMatrix x2(d,n); //size of x2? inefficient
  
  NumericMatrix G1(d+1,d+1); //gamma1 from x1
  NumericMatrix G2(d+1,d+1); //gamma2 from x2
  
  for (int i=0;i <X.nrow();i++){
    if(X(i,class_col-1)==0){
      x1(j1,_)=X(i,_); //row is copied
      j1 = j1+1;
    }
    else{
      x2(j2,_)=X(i,_);
      j2=j2+1;
    }
  }
  
  //get k gammas
  G1 = gamma_cpp_diagonal(x1); //x1 has extra rows with 0
  G2 = gamma_cpp_diagonal(x2);
  
  //list of gammas
  //ret['1']=G1;
  //ret['2']=G2;
  
  //return ret;
  return List::create(Named("1")=G1,Named("2")=G2); //for efficiency by Dirk eddelbuttel
}



// [[Rcpp::export]]
NumericMatrix k_gamma_diagonal(NumericMatrix X, int k, int class_col) {
  int n = X.ncol();
  int d = X.nrow();
  
  double *x= new double[d+1];
  NumericMatrix G(d+1,k*2); //gamma
  
  for(int i = 0; i < n; i++) {
    int c = X(class_col-1,i); //getting class number
    x[0]=1;
    for(int a=0;a<d;a++) x[a+1]= X(a,i); //X with extra 1s.
    
    for(int a=0;a<=d;a++){
      G(a,c*2) += x[a]*x[0]; //computing L
      if(a!=0)
        G(a,c*2+1) += x[a]*x[a]; //computing Q
    }
  }
  return G;
}


// [[Rcpp::export]]
NumericMatrix  naiveBayesPrediction(NumericMatrix X_Test, List mu, List sigma, List prior,IntegerVector class_labels){
  Rcpp::Rcout<<"prob size is :"<<X_Test.nrow()*class_labels.length()<<std::endl;
  //IntegerVector class_labels(class_lbls[1]);
  NumericMatrix prob(X_Test.nrow()*class_labels.length(), X_Test.ncol()+2);
  NumericVector temp(X_Test.ncol()+2);
  int ind = 0;
  int probIndex = 0;
  for(int xi = 0;xi < X_Test.nrow(); xi++){
    //Rcpp::Rcout<<"inside xi loop"<<xi<<std::endl;
    temp[ind++] = xi + 1;
    for(int g = 0; g < class_labels.length(); g++){
      //Rcpp::Rcout<<"inside g loop"<<g<<std::endl;
      temp[ind++] = class_labels[g];
      SEXP mu_list = mu[g];
      SEXP sigma_list = sigma[g];
      SEXP prior_list = prior[g];
      NumericVector mu_vect(mu_list), sigma_vect(sigma_list), prior_vect(prior_list);
      for(int col = 0; col < X_Test.ncol(); col++){
        //Rcpp::Rcout<<"inside col loop"<<col<<std::endl;
        if(sigma_vect[col]==0){
          temp[ind++] = 1;
        }
        else{
          double first = 1/sqrt(((2*3.141592)*sigma_vect[col]));
          double second = exp(-0.5*pow((X_Test(xi,col))-(mu_vect[col]),2)/sigma_vect[col]);
          temp[ind++] = first * second;
        }
      }
      prob(probIndex++,_ ) = temp;
      ind = 1;
      //Rcpp::Rcout<<"\n \n prob is : "<<prob<<std::endl;
    }
    ind = 0;
  }
  
  return prob;
}

// [[Rcpp::export]]

List assignCluster(NumericMatrix X, List cluster_centroids, int k){
  NumericVector cluster_id(X.nrow());
  List ret;
  double sum_of_error = 0.0;
  for(int i = 0; i<X.nrow(); i++){
    NumericVector sq_diff_sum(k);
    for (int j =0; j<k;j++){
      SEXP clu_cent_vect = cluster_centroids[j];
      NumericVector cluster_centroid_j = as<NumericVector>(clu_cent_vect);
      sq_diff_sum[j] = sum(pow((X(i, _) - cluster_centroid_j),2));
    }
    
    cluster_id[i] = which_min(sq_diff_sum);//argmin
    //Rcpp::Rcout<<"min "<<min(sq_diff_sum)<<std::endl;
    sum_of_error = sum_of_error + min(sq_diff_sum);
    //Rcpp::Rcout<<"sum_of_error : "<<sum_of_error<<std::endl;
  }
  ret["cluster_id"] = cluster_id;
  ret["sum_of_error"] = sum_of_error;
  return ret;
}

// [[Rcpp::export]]
NumericMatrix gamma_cpp(NumericMatrix X, std::string Flag1) {
  int d = X.nrow();
  //int s = Flag[0];
  NumericMatrix G(d+1,d+1);
  if(Flag1 == "Y" || Flag1 =="Yes" || Flag1 =="y" || Flag1 =="yes"){
    //Rcout<<"Sparse is selected"<<std::endl;
    G = gamma_cpp_sparse(X);
  }
  else{
    //Rcout<<"Dense is selected"<<std::endl;
    G = gamma_cpp_dense(X);
  }
  return G;
}
