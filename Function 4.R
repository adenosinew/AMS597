Z.ls=function(x,tumor=1,alternative="two.sided"){
  if(dim(x)[2]!=2){
    cat("The input data frame is not correct. Please clean it to n by 2.")
  }
  else{
    paired_sample=na.omit(x)
    tumor_sample=x[!is.na(x[,1])&is.na(x[,2]),1]
    normal_sample=x[is.na(x[,1])&!is.na(x[,2]),2]
    n1=dim(paired_sample)[1]
    n2=length(tumor_sample)
    n3=length(normal_sample)
    T_bar=mean(tumor_sample)
    N_bar=mean(normal_sample)
    T1_bar=mean(paired_sample[,1])
    N1_bar=mean(paired_sample[,2])
    sd_T1=sd(paired_sample[,1])
    sd_N1=sd(paired_sample[,2])
    sd_TN1=cov(paired_sample[,1],paired_sample[,2])
    r=sd_TN1/(sd_N1*sd_T1)
    f=n1*(n1+n3+n2*sd_TN1/(sd_T1^2))*((n1+n2)*(n1+n3)-n2*n3*r^2)^(-1)
    g=n1*(n1+n2+n3*sd_TN1/(sd_N1^2))*((n1+n2)*(n1+n3)-n2*n3*r^2)^(-1)
    v1_1=((f^2)/n1+((1-f)^2)/n2)*(sd_T1^2)*(n1-1)
    v1_2=((g^2)/n1+((1-g)^2)/n3)*(sd_N1^2)*(n1-1)
    v1_3=2*f*g*sd_TN1*(n1-1)/n1
    v1=(v1_1+v1_2-v1_3)/(n1-1)
    denominator=sqrt(v1)
    if(tumor==1){
      Z.ls=(f*(T1_bar-T_bar)-g*(N1_bar-N_bar)+T_bar-N_bar)/denominator
    }
    else if(tumor==2){
      Z.ls=(-f*(T1_bar-T_bar)+g*(N1_bar-N_bar)-T_bar+N_bar)/denominator
    }
    else{
      cat("The input for parameter \"tumor\" is not correct. It can only be 1 or 2 for tumor data is in the 1st column or 2nd column respectively.")
    }
    if (alternative=="greater"){
      P.value=pt(Z.ls,df=n1,lower.tail = F)
    }
    else if (alternative=="less"){
      P.value=pt(Z.ls,df=n1,lower.tail = T)
    }
    else if (alternative=="two.sided"){
      P.value=2*pt(abs(Z.ls),df=n1,lower.tail = F)
    }
    else{    
      cat("The input for parameter \"alternative\" is not correct. It can only be \"greater\", \"less\" or \"two.sided\".")
    }
  }
  return(list(Z.ls=Z.ls,P.value=P.value))
}






set.seed(123)
rho <- 0.5
s1 = s2 <- sqrt(2)
mu1 <- 2
mu2 <- -2

U <- rnorm(50)
V <- rnorm(50)
X <- mu1 + s1*U
Y <- mu2 + s2*(rho*U + sqrt(1 - rho^2)*V)

data3=data.frame(X,Y)
data3[1,1]=NA
data3[2,2]=NA
data3[3,1]=NA
data3[4,2]=NA
Z.ls(data3,alternative="greater")
