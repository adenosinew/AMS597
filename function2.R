# Function 2 #
# Yicong Zhu #

corrected_z<-function(x,y,alternative="two.sided",mu=0){
  paired.x<-x[!is.na(x)==!is.na(y)]
  paired.y<-y[!is.na(x)==!is.na(y)]
  tumor<-x[!is.na(x) & is.na(y)]
  normal<-y[is.na(x) & !is.na(y)]
  n1<-length(paired.x)
  n2<-length(tumor)
  n3<-length(normal)
  t0<-c(paired.x,tumor)
  n0<-c(paired.y,normal)
  if(n1==0){
    s<-0
  }else{
    s<-cov(paired.x,paired.y)
  }
  z.stat<-(mean(t0)-mean(n0)-mu)/sqrt(sd(t0)^2/(n1+n2)+sd(n0)^2/(n1+n3)-2*n1*s/(n1+n2)/(n1+n3))
  if(alternative=="two.sided"){
    p.value=2*pnorm(abs(z.stat),lower.tail=F)
  }
  if(alternative=="greater"){
    p.value=pnorm(z.stat,lower.tail=F)
  }
  if(alternative=="less"){
    p.value=pnorm(z.stat,lower.tail=T)
  }
  return(c(z.stat=z.stat,p.value=p.value))
}