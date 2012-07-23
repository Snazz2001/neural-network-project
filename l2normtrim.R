l2normtrim<-function(x,n=3){
  sdev<-sd(x);
  avg<-mean(x);
  notoutlier<-x[which(x<=avg+n*sdev&x>=avg-n*sdev)];
  l2norm<-sqrt(sum(notoutlier^2));
  result<-x/l2norm;
  return(result)
}