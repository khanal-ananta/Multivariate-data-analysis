#In this example, the remote-sensing data are used. In this data set,
#crops, the observations are grouped into five crops: clover, corn, cotton,
#soybeans, and sugar beets. Four measures called x1 through x4 make
#up the descriptive variables.

#--------------------------------------------------------------------------------------
# commnet for ctrl+shift+c

#--------------------------------------------------------------------------------------
#(a) Perform a linear discriminant analysis with response variable Crop.
# Assume equal variances. Interpret the results.

getwd()


df = read.csv("C:/Users/khanal/Desktop/multivariate data analysis/6-crop-da/crops.csv",
              sep = ";",dec = ".",header = TRUE)

head(df)
tail(df)
names(df)
df
summary(df)


dim(df) # dimension
class(df) # data struture type
str(df) # data struct

df1 = df[,-c(1)]
head(df1)
head(df)

library(MASS)

fit1 = lda(Crop ~ x1+x2+x3+x4, data=df)
fit1
#proportion of trace
# LD1 LD2 LD3 LD4
# 0.7364 0.1985 0.0576 0.0075

fit2 = lda(Crop ~ x1+x2+x3+x4, data=df, CV=TRUE)
fit2

plot(fit1)
plot(fit2) # gives error


# this gives the prediction of every observations.
p1 = predict(fit1,df[,-c(1)])
p1
p1$class



# Assess the accuracy of the prediction
# percent correct for each category of Crop

ct1 = table(predicted = p1$class, actual = df$Crop)
ct1
               # actual      
# predicted    Clover Corn Cotton Soybeans Sugarbeets
# Clover          6    0      3        0          2
# Corn            0    6      0        1          0
# Cotton          3    0      1        2          0
# Soybeans        0    1      1        3          1
# Sugarbeets      1    1      0        2          2



ct2 = table(predicted = fit2$class, acutual = df$Crop)
ct2

# acutual
# predicted    Clover Corn Cotton Soybeans Sugarbeets
# Clover          4    3      1        0          3
# Corn            0    4      1        2          0
# Cotton          3    0      0        2          1
# Soybeans        0    1      1        3          1
# Sugarbeets      2    1      0        2          1

# accurecy percentage for each class
diag(prop.table(ct2, 1))
# Clover       Corn     Cotton   Soybeans Sugarbeets 
# 0.4444444  0.4444444  0.0000000  0.3333333  0.1666667 


# total percent correct
sum(diag(prop.table(ct2)))
# 0.3333333




#--------------------------------------------------------------------------------------
# (b) Perform a quadratic discriminant analysis with response variable
# Crop. Assume non-equal variances. Interpret the results.

fit3 <- qda(Crop ~ x1 + x2 + x3 + x4, data=na.omit(df))
fit3

fit4 <- qda(Crop ~ x1 + x2 + x3 + x4, data=na.omit(df),CV=TRUE)
fit4

p3 = predict(fit3,df[,-c(1)])
p3


ct3 = table(df$Crop, p3$class)


# correct precentage of each class
diag(prop.table(ct3, 1))
# Clover       Corn     Cotton   Soybeans Sugarbeets 
# 0.8181818  1.0000000  1.0000000  1.0000000  0.6666667 

# total percent correct
sum(diag(prop.table(ct3)))
# 0.8888889



ct4 = table(df$Crop, fit4$class)

# correct precentage of each class
diag(prop.table(ct4, 1))
# Clover       Corn     Cotton   Soybeans Sugarbeets 
# 0.8181818  0.2857143  0.3333333  0.3333333  0.1666667 

# total percent correct
sum(diag(prop.table(ct4)))
# 0.4444444

