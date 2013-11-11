GBrownplot<-function(mu=0,sigma=1,N=100,T=1,x0=1,seed=NULL,n=0,...){
#�ú������ڼ��β����˶�ģ��ɼ�
#���Ϊģ��Ǽ�����ͼ
#1/NΪÿ��ʱ������NȱʡֵΪ100�������0.01
 library(pracma);
   if(N<=0|T<=0){
     stop("parameters are wrong!");
   }
   Delta<-1/N;
   W<-numeric(N+1);
   t<-seq(0,T, 1/N);
   S<-0:(T*N);
   R<-rnorm(T*N,0,1)*sqrt(Delta);
   W[2:(T*N+1)]<-cumsum(R);
   S<-x0*exp((mu-sigma^2/2)*t + sigma*W);
   plot(t,S,type="l",...);
}