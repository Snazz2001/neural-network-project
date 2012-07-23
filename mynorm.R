mynorm<-function(x,results){
  center<-results$center;
  spread<-results$spread;
  xnorm<-(x-center)/spread;
  return(xnorm);
}