# 1. A researcher has collected data on three psychological variables, four
# academic variables (standardized test scores) and gender for 600 college freshman. 
# She is interested in how the set of psychological variables
# relates to the academic variables and gender. In particular, the researcher is 
# interested in how many dimensions (canonical variables) are
# necessary to understand the association between the two sets of variables.
# We have a data file, mmreg, with 600 observations on eight variables.
# The psychological variables are locus of control, self concept and motivation. 
# The academic variables are standardized tests in reading (read),
# writing (write), math (math) and science (science). Additionally, the
# variable female is a zero-one indicator variable with the one indicating
# a female student.

#--------------------------------------------------------------------------------------
# comment for ctrl+shift+c

#--------------------------------------------------------------------------------------
# (a) Confirm the dimensions of mmreg.

getwd()


df = read.csv("C:/Users/khanal/Desktop/multivariate data analysis/8-cca/mmreg.csv",
              sep = ";",dec = ".",header = TRUE)

head(df)
tail(df)
df
summary(df)


dim(df) # dimension =  600   9


class(df) # data struture type
str(df) # data structure

#----------------------------------------------------------------------------------------
# (b) The psychological variables are locus of control, self-concept and
# motivation. The academic variables are standardized tests in reading, writing, math and 
# science. Additionally, the variable female
# is a zero-one indicator variable with the one indicating a female
# student.
# install.packages("ggplot2")
# install.packages("GGally")
require(ggplot2)
require(GGally)

psych <- df[, 2:4]
head(psych)


acad <- df[, 5:9]
head(acad)

cor(psych, method = "pearson", use = "complete.obs")
cor(acad, method = "pearson", use = "complete.obs")


ggpairs(acad)
ggpairs(acad)

# install.packages("matcor")
corr = matcor(psych, acad)
corr$Xcor
corr$Ycor
corr$XYcor


#----------------------------------------------------------------------------------------
# (c) Apply some descriptive statistics to the data.

summary(df)

#----------------------------------------------------------------------------------------
# (d) Perform a canonical correlation analysis. Display the canonical correlations.

# correaltion between canonical function that is like components in PCA.
# canonical function also known as canonical factors.

# require(CCA)
library(CCA)
cc1 <- cc(psych, acad)
cc1
attributes(cc1)

cc1$cor # 0.4640861 0.1675091 0.1039911

#----------------------------------------------------------------------------------------
# (e) Display the raw canonical correlations.

# also known as raw cononical coefficients.


# coefficient are used to find linear combinations for the canonical function.
# these are also called as canonical loadings.

cc1$xcoef

# $xcoef
# [,1] [,2] [,3]
# Control -1.2538339 -0.6214775 -0.6616896
# Concept 0.3513499 -1.1876867 0.8267209
# Motivation -1.2624203 2.0272641 2.0002284

cc1$ycoef

# [,1]         [,2]        [,3]
# READ    -0.044620596 -0.004910018  0.02138056
# WRITE   -0.035877112  0.042071471  0.09130733
# MATH    -0.023417185  0.004229472  0.00939821
# SCIENCE -0.005025157 -0.085162175 -0.10983502
# FEMALE  -0.632119239  1.084642482 -1.79464692


#----------------------------------------------------------------------------------------
# (f) Interpret the raw canonical cofficients.

# The raw canonical coefficients are interpreted in a manner analogous to interpreting 
# regression coefficients i.e., for the variable
# read, a one unit increase in reading leads to a .0446 decrease in
# the first canonical variate of set 2 when all of the other variables
# are held constant. Here is another example: being female leads to
# a .6321 decrease in the dimension 1 for the academic set with the
# other predictors held constant.



#----------------------------------------------------------------------------------------
# (g) Display the loadings of the variables on the canonical dimensions
# (variates).

cc2 <- comput(psych, acad, cc1)

cc2$corr.X.xscores # canonical loadings
cc2$corr.Y.xscores # cross loadings
cc2$corr.X.yscores
cc2$corr.Y.yscores

# These loadings are correlations between variables and the canonical variates.
# The above correlations are between observed variables and canonical variables 
# which are known as the canonical loadings. These
# canonical variates are actually a type of latent variable.


#----------------------------------------------------------------------------------------
# (h) In general, the number of canonical dimensions is equal to the
# number of variables in the smaller set; however, the number of
# significant dimensions may be even smaller. Find the number of
# significant dimensions!

# tests of canonical dimensions
# install.packages("CCP")
library(CCP)
rho <- cc1$cor
rho
## Define number of observations, number of variables in first set, and number 
# of variables in the second set.
n <- dim(psych)[1]
n
p <- length(psych)
p
q <- length(acad)
q


# Calculate p-values using the F-approximations of different test statistics:
p.asym(rho, n, p, q, tstat = "Wilks")
p.asym(rho, n, p, q, tstat = "Hotelling")
p.asym(rho, n, p, q, tstat = "Pillai")


# professor method
ev <- (1 - cc1$cor^2)
w <- rev(cumprod(rev(ev)))
d1 = d2 = f = k = min(p,q)
m <- n - 3/2 - (p + q)/2

for (i in 1:k) {
  s <- sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
   si <- 1/s
   d1[i] <- p * q
   d2[i] <- m * s - p * q/2 + 1
   r <- (1 - w[i]^si)/w[i]^si
   f[i] <- r * d2[i]/d1[i]
   p <- p - 1
   q <- q - 1
   }


pv <- pf(f, d1, d2, lower.tail = FALSE)
dmat <- cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv)
dmat

# WilksL         F df1      df2            p
# [1,] 0.7543611 11.715733  15 1634.653 7.497602e-28
# [2,] 0.9614300  2.944459   8 1186.000 2.905057e-03
# [3,] 0.9891858  2.164612   3  594.000 9.109217e-02


# from above we can see that first one is highly significant
# second is acceptable .
# third is not signicant.
# so taking 2 would be best suited for analysis.




#----------------------------------------------------------------------------------------
# standardized psych canonical coefficients diagonal matrix of psych sd's
s1 <- diag(sqrt(diag(cov(psych))))
s1 %*% cc1$xcoef

# standardized acad canonical coefficients diagonal matrix of acad sd's
s2 <- diag(sqrt(diag(cov(acad))))
s2 %*% cc1$ycoef


# The standardized canonical coefficients are interpreted in a manner analogous to
# interpreting standardized regression coefficients. For example, consider the variable 
# read, a one standard deviation increase in reading leads to a 0.45 standard deviation 
# decrease in the score on the first canonical variate for 
# set 2 when the other variables in the model are held constant.