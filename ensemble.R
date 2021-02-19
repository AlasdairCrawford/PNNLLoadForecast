EnsembleRamp<-function(x){
  a<-2.9/6.8
  b<-9
  return((a+(1-a)*(x-1)/(b-1))*(x<b)+(x>=b))
}