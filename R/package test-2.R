# 1 situation
x<-c(3,2,5,13,54,63,22,11,NA,NA,NA)
y<-c(8,3,10,NA,NA,NA,NA,NA,43,96,14)
t.test(x,y) # t = -0.43795, df = 8.2215, p-value = 0.6727

# Our Group
# 1: modified t-statistic:
modttest(x,y) #$statistic[1] -0.809376 $p.value [1] 0.4182989
# $statistic
# [1] -0.809376
#
# $p.value
# [1] 0.4182989
#
# [[3]]
# [1] "Modified T test"

# 2: corrected-Z
corrected_Z(x,y)
# $statistic
# [1] -0.4384378
#
# $p.value
# [1] 0.661069
#
# [[3]]
# [1] "Corrected Z test"

# 3: MLE based test, homoscedasticity, I think we are OK, should use t-test
homoMLE(x,y) # $Z.star [1] -0.7433454 $P.value [1] 0.5111933
# $statistic
# [1] -0.7433454
#
# $df
# [1] 3
#
# $p.value
# [1] 0.5111933
#
# [[4]]
# [1] "MLE based test of Ekbohm under homoscedasticity"


# 4: MLE based test, heteroscedasticity
heteMLE(x,y) # $Z.ls [1] -28.62885 $P.value [1] 9.357389e-05
# $statistic
# [1] -28.62885
#
# $df
# [1] 3
#
# $p.value
# [1] 9.357389e-05
#
# $METHOD
# [1] "MLE based test of Lin and Stivers under heteroscedasticity"

# 5: weighted Z-test
weighted.z.test(x,y)
# $p.value
# [1] 0.1310176
#
# [[2]]
# [1] "Weighted Z combination"
