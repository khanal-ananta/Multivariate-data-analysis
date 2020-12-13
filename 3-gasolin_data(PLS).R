# Gasoline is a data set with NIR spectra and octane numbers of 60
# gasoline samples. The NIR spectra were measured using diffuse reflec-
#   tance as log(1/R) from 900 nm to 1700 nm in 2 nm intervals, giving
# 401 wavelengths.

#---------------------------------------------------------------------------------------
#(a) Which variables are contained in the data set?
df = read.delim("C:/Users/khanal/Desktop/multivariate data analysis/3-gasolin-pls/gasoline.txt",
                sep = ",",dec = ".",header = TRUE)
head(df)
names(df)
class(df)
dim(df)
summary(df)
attributes(df)
  
#---------------------------------------------------------------------------------------
#(b) Perform a standard PLS! Use Octane as response variable and theN IR-variables as 
# explanatory variables.
require(pls)
head(df)
names(df)
x = df[-c(1,2)]
head(x)
colnames(x)
y = df[2]
head(y)
typeof(y)
y2 = as.data.frame.array(y)
class(y2) # data frame
typeof(y2) # list
Z = as.data.frame(cbind(y,x))

head(Z)
head(df)

gas1 <- plsr(Z$octane ~ ., ncomp = 10, data = Z)

summary(gas1)

gas2 <- plsr(df$octane ~ ., ncomp = 10, data = df)
summary(gas2)

#----------------------------------------------------------------------------------------
# (c) How much predictor and response variation is explained by each PLS factor?
# TRAINING: % variance explained
#             1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps  9 comps  10 comps
# X           70.97    78.56    86.15    95.40    96.12    96.97    97.32    98.10    98.32     98.71
# Z$octane    31.90    94.66    97.71    98.01    98.68    98.93    99.06    99.11    99.20     99.24

  
#---------------------------------------------------------------------------------------
# (d) Concentrate on the first two extracted factors: How much predic-tor and response variation 
# is explained?
#             1 comps  2 comps
# X           70.97    78.56 
# Z$octane    31.90    94.66

 
#---------------------------------------------------------------------------------------
# (e) Create a correlation loading plot for the first two factors.
par(mfrow = c(1,1))
plot(gas1, plottype = "correlation")
# shows the correlations between each variable and the selected components


biplot(gas1, scale = 0, cex = .5)

#---------------------------------------------------------------------------------------
# (f) Interpret the correlation loading plot.  
