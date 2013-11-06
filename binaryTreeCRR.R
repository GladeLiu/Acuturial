binaryTreeCRR<-function(type=c("ce","ca","pe","pa"),n=20,a=0.01,b=0.01,r=0.05,delta=0.05,K=53,S0=50){
 type=type[1]; 
 q<-(exp(r*delta)-(1-b))/(a+b);
 p<-1-q;
  s1<-function(t){
    S0*(1+a)^(i-t)*(1-b)^(t-1);  
  }
  "%-+%"<-function(X,Y){
     if(X<Y){
        0;
     }else{
       X-Y;
      }
  }
  if(type=="ce"||type=="ca"){
    s2<-function(x){
      x%-+%K;
    }
  }else if(type=="pe"||type=="pa"){
    s2<-function(x){
      K%-+%x;
    }
  }else{
    stop("type is wrong!")
  }
  i<-n+1;
    W<-as.matrix(1:i);
    G<-apply(W,1,s1);
    P<-apply(as.matrix(G),1,s2);
    price0<-P;
  if(!type=="pa"){
    for(k in (i-1):1){
      for(j in 1:k){
         price0[j]<-exp(-r*delta)*(q*price0[j]+p*price0[j+1]);
      }
    }
   }else{
     for(k in (i-1):1){
      for(j in 1:k){
         price0[j]<-max(exp(-r*delta)*(q*price0[j]+p*price0[j+1]),
                    K-S0*(1+a)^(k-j)*(1-b)^(j-1));
      }
    }
   }
   exp(-delta*r)*price0[1];
}
