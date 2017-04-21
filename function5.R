#Function5
#Yuanjiao Wang

weighted.z.test<-function(x,y,alternative){
	paired.x<-x[!is.na(x)==!is.na(y)]
  	paired.y<-y[!is.na(x)==!is.na(y)]
  	tumor<-x[!is.na(x) & is.na(y)]
  	normal<-y[is.na(x) & !is.na(y)]
  	n1<-length(paired.x)+length(paired.y)
  	n2<-length(tumor)
  	n3<-length(normal)
	w1<-sqrt(n1)
	w2<-sqrt(n2+n3)
	t1<-t.test(paired.x,paired.y,alternative,paired = TRUE)
	p1<-t1$p.value
	t2<-t.test(tumor,normal,alternative,paired = FALSE)
	p2<-t2$p.value
	z1<-qnorm(1-p1)
	z2<-qnorm(1-p2)
	pc<-(w1*z1+w2*z2)/sqrt((w1^2)+(w2^2))
		if(alternative=="less"){
			pv<-pnorm(pc)
		}
		if (alternative=="greater"){
			pv<-1-pnorm(pc)
		}
		if(alternative=="two.sided"){
			pv<-2*min(pnorm(pc),1-pnorm(pc))
		}
	return(list(pc=pc,p.value=pv))
}
	