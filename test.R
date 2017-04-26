# 1 situation
x<-c(3,2,5,13,54,63,22,11,NA,NA,NA)
y<-c(8,3,10,NA,NA,NA,NA,NA,43,96,14)
t.test(x,y) # t = -0.43795, df = 8.2215, p-value = 0.6727
corrected_Z(x,y) # $statistic[1] -0.4384378 $p.value[1] 0.661069
weighted.z.test(x,y) #$pc [1] 0.557924 $p.value [1] 0.5768963
# 2 situation
x.1<-c(3,4,22,54,63,77)
y.1<-c(34,2,14,55,32,14)
t.test(x.1,y.1) # t = 0.79497, df = 8.1711, p-value = 0.4491
corrected_Z(x.1,y.1) # $statistic[1] 0.9207191 $p.value [1] 0.3571971
weighted.z.test() #not enought 'x' observations
# 3 situation
x.2<-c(13,54,63,22,11,NA,NA,NA,NA)
y.2<-c(NA,NA,NA,NA,NA,43,96,14,45)
t.test(x.2,y.2) # t = -0.83701, df = 5.2676, p-value = 0.4389
corrected_Z(x.2,y.2) # $statistic[1] -0.8370064 $p.value [1] 0.402589
weighted.z.test() #not enough 'x' observations
