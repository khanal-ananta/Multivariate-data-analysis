# A large international air carrier has collected data on employees in
# three different job classifications; 1) customer service personnel, 2) mechanics and 3) 
#dispatchers. The director of Human Resources wants to
# know if these three job classifications appeal to different personality
# types. Each employee is administered a battery of psychological test
# which include measures of interest in outdoor activity, sociability and
# conservativeness.

#--------------------------------------------------------------------------------------
# commnet for ctrl+shift+c

#--------------------------------------------------------------------------------------
# (a) Perform a descriptive analysis.

getwd()

df = read.csv("C:/Users/khanal/Desktop/multivariate data analysis/7-emplyee data-da/discrim.csv",
              sep = ";",dec = ".",header = TRUE)

head(df)
names(df)
tail(df)
df
summary(df)


dim(df) # dimension
class(df) # data struture type
str(df) # data struct

#--------------------------------------------------------------------------------------
# (b) Perform a correlation analysis for the explanatory variables.
cor(df)

ggpairs(df)

#--------------------------------------------------------------------------------------
# (c) Perform a linear discriminant analysis with response variable JOB.
# Assume equal variances. Interpret the results.

library(MASS)

fit1 = lda(df$JOB ~ df$OUTDOOR+df$SOCIAL+df$CONSERVATIVE, data=df)
attributes(fit1)
fit1

fit2 = lda(df$JOB ~ df$OUTDOOR+df$SOCIAL+df$CONSERVATIVE, data=df,na.action="na.omit", CV=TRUE)
attributes(fit2)
fit2

# predictioin classes
fit2$class

# Assess the accuracy of the prediction
# percent correct for each category of JOB

ct2 = table(acutual = df$JOB, predicted = fit2$class )
ct2

#           predicted
# acutual  1  2  3
# 1       67 14  4
# 2       16 67 10
# 3        3 14 49


# accuracy of the model
# sum of diagonal elements divided by total 

accuracy = sum(diag(ct2))/sum(ct2)
accuracy  # 0.75


# percent correct for each category of Crop
diag(prop.table(ct2, 1))
# 1         2         3 
# 0.7882353 0.7204301 0.7424242


# total percent correct
sum(diag(prop.table(ct2)))
# 0.75



#--------------------------------------------------------------------------------------
# (d) Perform a quadratic discriminant analysis with response variable
# JOB. Assume non-equal variances. Interpret the results.


library(MASS)

fit3 = qda(df$JOB ~ df$OUTDOOR+df$SOCIAL+df$CONSERVATIVE, data=df)
fit3

fit4 = qda(df$JOB ~ df$OUTDOOR+df$SOCIAL+df$CONSERVATIVE, data=df,na.action="na.omit", CV=TRUE)
fit4


# Assess the accuracy of the prediction
# percent correct for each category of JOB

ct4 = table(actual = df$JOB, predcited = fit4$class)
ct4

# 1  2  3
# 1 66 15  4
# 2 17 66 10
# 3  3 14 49


# percent correct for each category of JOb
diag(prop.table(ct4, 1))
# 1         2         3 
# 0.7764706 0.7096774 0.7424242 

# this is like sensitivity which is also called as recall
ct4[1,1]/(ct4[1,1]+ct4[1,2]+ct4[1,3]) # 0.7764706
ct4[2,2]/(ct4[2,1]+ct4[2,2]+ct4[2,3]) # 0.7096774
ct4[3,3]/(ct4[3,1]+ct4[3,2]+ct4[3,3]) # 0.7424242


# total percent correct
# accuracy
sum(diag(ct4))/sum(ct4)
# 0.7418033

sum(diag(prop.table(ct4)))
# 0.7418033
