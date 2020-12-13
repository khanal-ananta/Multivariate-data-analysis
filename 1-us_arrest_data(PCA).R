
# The data set USArrests contains statistics, in arrests per 100,000 re-sidents for assault, murder, and rape  
# in each of the 50 US states in1973. Also given is the percent of the population living in urban areas.
# USArrests data sets comes with basic dataset with 4 variables. UsingPCA, we are going to find linear 
# combinations of the variables that maxmimal variance and mutually uncorrelated.
#--------------------------------------------------------------------------------------

# score = observations = latent variable(principle components)
# variable = loading = influence in variation
# loading is the cosine of angle of the pc with the variable axis(relation to the original data)

#--------------------------------------------------------------------------------------
# a)Which variables are contained in the data set?
getwd()

df = read.csv("C:/Users/khanal/Desktop/multivariate data analysis/1-us_arrest-pca/USArrests.csv",
              sep = ",",dec = ".",header = TRUE)
df
head(df)
summary(df)

dim(df) # dimension
class(df) # data struture type
str(df) # data structure


head(df)
tail(df)

df1 = df[,c(-1)]
head(df1)

cor(df1) # gives the correlation between the variables

cov(df1) # gives the co variance between the variables
#----------------------------------------------------------------------------------------
# b)Create scatterplots of the different variable combinations.

require(graphics)
pairs(df1, panel = panel.smooth, main = "USArrests data")

#---------------------------------------------------------------------------------------
#c) Perform a standard PCA!
# professor has done with prcomp
pca1 <- prcomp(df1,center = TRUE,scale = TRUE)
pca1

pca2 <- princomp(df1,scores = TRUE, cor = TRUE)
pca2


#---------------------------------------------------------------------------------------
# d) How many principal components are returned by the standard PCA?
# since we have four variable there is 4 principle  component.

#---------------------------------------------------------------------------------------
# (e) How much of the total variance is explaned by the different principal components?
summary(pca1)
summary(pca2)

attributes(pca1)
attributes(pca2)

#---------------------------------------------------------------------------------------
# (f) Take a look at the scree plot and interpret the results.
# this gives bar charts

# a scree plot is a line plot of the eigenvalues of factors or principal components in an analysis
# Use a scree plot to select the principal components to keep. An ideal curve should be steep, then 
# bends at an "elbow" - this is your cutting-off point.

# if the graph is not ideal to find elbow point then use:

# Kaiser rule: pick PCs with eigenvalues of at least 1.
# Proportion of variance plot: the selected PCs should be able to describe at least 80% of the variance.

plot(pca1)
plot(pca2)

screeplot(pca1,type = "line",main = "scree plot")
screeplot(pca2, type = "line", main = "scree plot")

var_pro = (pca2$sdev)^2/sum((pca2$sdev)^2)
var_pro

plot(var_pro,type = "b",ylab = "proportion of variance explained", xlab = "Principle component")

c_var = cumsum(var_pro)
c_var

plot(c_var,type = "b",ylab = "proportion of variance explained", xlab = "Principle component")


screeplot((pca2$sdev)^2/sum((pca2$sdev)^2), type = "line", main = "scree plot")

#---------------------------------------------------------------------------------------
# G ) Produce a plot of the first two principal components


biplot(pca1,scale=0, cex=.7)

biplot(pca2,scale=0, cex=.7)


#---------------------------------------------------------------------------------------
# H )Interpret the loadings for the first two principal components.

# From the plot as wells from the above loadings what we can un-derstand is, first component 
# places approximately equal weight on Assault, Murder and Rape, with much less weight on urban-
# pop. Hence this component roughly corresponds to a measure of overall rates of serious crimes.
# The second component places most of it weight on Urbanpop and much less weight on the other 
# 3 features. Hence, this compo-nent roughly corresponds to the level of urbanization of the state.
# Overall, we see that the crime-related varaibales are located close to each other, and that the 
# urbanpop variable is far from other three. This indicates that the crime related variables are correlated
# with each other-States with high murder rates tend to had highassault and rape rates. Urabnpop variable 
# is less correlated with the other three.

#---------------------------------------------------------------------------------------
# loading and score of the pca
pca1$rotation
loadings(pca2)

pca1$x
pca2$scores

#---------------------------------------------------------------------------------------
# loading and score plot
# loadings are the co-variance between the original variables and the unit scale component.
# loadings = eigenvectors. sqrt(eigenvalues).
# Eigenvectors are unit-scaled loadings.

# A loading plot shows how strongly each characteristic influences a principal component.
# Another nice thing about loading plots: the angles between the vectors tell us 
# how characteristics correlate with one another.
# When two vectors are close, forming a small angle, the two variables they represent
# are positively correlated.
# If they meet each other at 90°, they are not likely to be correlated.
# When they diverge and form a large angle (close to 180°), they are negative correlated.

plot(pca1$x)
plot(pca2$loadings)




# score is the position of the measured points along the direction of the principle component.
# score helps in clustering of the data.

plot(pca1$rotation)
plot(pca2$scores)

# observation having high  score will have high loadings.
