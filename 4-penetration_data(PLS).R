
# This example, from Umetrics (1995), demonstrates different ways toexamine a PLS model. The data come 
# from the field of drug discovery. New drugs are developed from chemicals that are biologically active.
# Testing a compound for biological activity is an expensive procedure, so it is useful to be able to predict
# biological activity from cheaper chemi-cal measurements. In fact, computational chemistry makes it possible
# to calculate certain chemical measurements without even making the compound. These measurements include size, 
# lipophilicity, and polari-ty at various sites on the molecule.The data set named pentaTrain,contains 
#these data.
#---------------------------------------------------------------------------------------

# Samples with close scores along the same PC are similar (they have close values for the corresponding variables).
# Conversely, samples for which the scores differ greatly are quite different from each other with respect to 
# those variables.

# For each PC, look for variables with high loadings (i.e. close to +1 or -1); this indicates that the loading 
# is interpretable. To study variable correlations, one studies the relative location of variables in the loadings space. 
# Variables that lie close together are highly correlated. For instance, if two variables have high loadings along the 
# same PC, it means that their angle is small, which in turn means that the two variables are highly correlated. 
# If both loadings have the same sign, the correlation is positive (when one variable increases, so does the other). 
# If the loadings have opposite signs, the correlation is negative (when one variable increases, the other decreases).
#---------------------------------------------------------------------------------------
# (a) Which variables are contained in the data set?
df = read.csv("C:\\Users\\khanal\\Desktop\\multivariate data analysis\\4-pentatrain-pls\\pentatrain.csv",
              dec = ".", sep = ";")

head(df)
class(df)
dim(df)
summary(df)
attributes(df)
#---------------------------------------------------------------------------------------
# (b) Perform a standard PLS! Use log RAI as response variable and
# S1 − S5, L1 − L5, P 1 − P 5 as explanatory variables.
# install.packages("pls")
library(pls)

pls1 = plsr(df$log_RAI~df$S1+df$S2+df$S3+df$S4+df$S5+df$L1+df$L2+df$L3+df$L4+df$L5+df$P1+
              df$P2+df$P3+df$P4+df$P5, data = df)


#---------------------------------------------------------------------------------------
# (c) How much predictor and response variation is explained by each PLS factor?
summary(pls1)

#---------------------------------------------------------------------------------------
# (d) Concentrate on the first two extracted factors: How much predic-tor and response variation is explained?
#              1 comps  2 comps
# X             39.38    52.70
# df$log_RAI    84.04    96.15 
#---------------------------------------------------------------------------------------
# (e) Create a correlation loading plot for the first two factors.
# note inner circle is 50% and outer circle is 100%

#par(mfrow = c(1,1))
plot(pls1, plottype = "correlation")

#---------------------------------------------------------------------------------------
# (f) Interpret the correlation loading plot.

biplot(pls1,scale = 0, cex =1)
# from this plot except s3 l3 and s1 all other observation almost co-related to each other.

#---------------------------------------------------------------------------------------
