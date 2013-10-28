Panjer<-function(N=1,n=c(3,0.5),X=c(0,0.5,0.5),s=1){
#N is the distribution of claim frequency
#the value of N can be choosed by the set {1,2,3}
#and {1,2,3} corresponds to {Poisson,Negative binomial,binomial}
#n is the parameters of the distribution of N
#X is the distribution(just consider discrete distribution)
#of single claim amount
#require package pracma
library(pracma);
 if(!is.numeric(n)){
   stop("please input the parameter!")
 }
 X<-abs(X);
  if(sum(X)!=1){
    stop("X is not a pdf!")
  }
  if(N!=1&&N!=2&&N!=3){
    stop("N is wrong!")
  }
  s<-as.integer(s);
  if(s<1){
    stop("s must be larger than 1!")
  }   
R=zeros(1,(s+1));
r<-length(X);
#Poisson distribution
   if(N==1){
     lambda=n[1];
     N0=dpois(0,lambda);
     #moment generating function
      g1<-function(t){
        exp(lambda*(ext(t)-1));
      }
     a=0;
     b=lambda;
   }
#Negative binomial distribution
   else if(N==2){
    if(length(n)<2){
      stop("parameters are wrong!")
    }
     r=n[1];
     p=n[2];
     N0=dnbinom(0,r,p);
      g1<-function(t){
        (p/(1-(1-p)*exp(t)))^r;
      }
     a=1-p;  
     b=a*(r-1);
   }
#Binomial distribution
   else if(N==3){
    if(length(n)<2){
      stop("parameters are wrong!")
    }

     n1=n[1];
     p=n[2];
     N0=dbinom(0,n1,p);
      g1<-function(t){
        (1-p+p*exp(t))^n1;
      }
     a=p/(p-1);
     b=-a*(n1+1);
   }
#results
 if(X[1]==0){
    R[1]=N0;
   }else{
    R[1]=g1(log(X[1]));
 }
#results 
 for(i in 2:(s+1)){
   t<-min(i-1,r-1);
   S<-zeros(1,t);
   for(h in 1:t){
      S[h]=(a+b*h/(i-1))*X[h+1]*R[i-h];
   }
   R[i]=(1/(1-a*X[1]))*sum(S);
 }
#output
   P<-sum(R);
   output<-list(Pi=R[s+1],P=P);
}