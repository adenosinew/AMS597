##function 3 
##Xining Zhang


Zstar <- function(x, tumor, alternative) {
paired_sample<-na.omit(x)
tumor<-x[!is.na(x[,1])&is.na(x[,2]),1]
normal<-x[is.na(x[,1])&!is.na(x[,2]),2]
n1<-dim(paired_sample)[1]
n2<-length(tumor)  
n3<-length(normal)
T_bar<-mean(tumor)
N_bar<-mean(normal)
sd_T <- sd(tumor)
sd_N <- sd(normal)
T1_bar<-mean(paired[,1])
N1_bar<-mean(paired[,2])
sd_T1<-sd(paired[,1])
sd_N1<-sd(paired[,2])
sd_TN1<-cov(paired[,1],paired[,2])
r<-sd_TN1/(sd_N1*sd_T1)

fstar<-n1*(n1+n3+n2*r)*((n1+n2)*(n1+n3)-n2*n3*r^2)^(-1)
gstar<-n1*(n1+n2+n3*r)*((n1+n2)*(n1+n3)-n2*n3*r^2)^(-1)

sigma2 <- (sd_T1^2 * (n1  - 1) + sd_N1^2*(n1 - 1) + (1 + r^2)*(sd_T^2*(n2 - 1) + sd_N^2*(n3 - 1)))/(2*(n1 - 1) + (1 + r^2)*(n2 + n3 - 2))
V1_star <- sigma2*( (2*n1*(1 - r) + (n2 +n3)*(1 - r^2))/((n1 + n2)*(n1 + n3) - n2*n3*r^2))

if(tumor==1){
 Z.star = (fstar*(T1_bar-T_bar)-gstar*(N1_bar-N_bar)+T_bar-N_bar)/sqrt(V1_star)
}
else if(tumor==2){
Z.star=(-fstar *(T1_bar-T_bar)+gstar*(N1_bar-N_bar)-T_bar+N_bar)/sqrt(V1_star)
}
if (alternative=="greater"){
P.value=pt(Z.star,lower.tail = F)
}
else if (alternative=="less"){
P.value=pt(Z.star,lower.tail = T)
}
else if (alternative=="two.sided"){
P.value=2*pt(abs(Z.star),lower.tail = F)
}

return(list(Z.star= Z.star,P.value=P.value))
}

    
