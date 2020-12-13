
# The famous (Fisher’s or Anderson’s) iris data set gives the measure-ments in centimeters of the variables 
# sepal length and width and petallength and width, respectively, for 50 flowers from each of 3 species of
# iris. The species are Iris setosa, versicolor, and virginica.

#--------------------------------------------------------------------------------------
#(a) Which variables are contained in the data set?
df = read.delim("C:/Users/khanal/Desktop/multivariate data analysis/2-fisher's iris data-pca/iris.txt",
                sep = ",",dec = ".",header = TRUE)
head(df)
  
#--------------------------------------------------------------------------------------
#(b) How many observations are contained in the data set?
  
class(df) # data frame
dim(df) # 150

#--------------------------------------------------------------------------------------  
#(c) Create scatterplots of the different variable combinations.
require(graphics)
df1 = df[,c(-6)]
head(df1)
pairs(df1,panel = panel.smooth,main= "iris dataset")

#--------------------------------------------------------------------------------------
#(d) Perform a standard PCA for the quantitative variables in the dataset!
# there is no parameter for scaling and centering may be they are done atuomatically
pca = princomp(~df$Sepal.Length+df$Sepal.Width+df$Petal.Length+df$Petal.Width,cor = TRUE, scores = TRUE)
pca
summary(pca)
  
#--------------------------------------------------------------------------------------  
#(e) How many principal components are returned by the standard PCA?
# there are four since there are four variables 

#--------------------------------------------------------------------------------------
#(f) How much of the total variance is explaned by the different prin-cipal components?
summary(pca)
# from this table we can see all the proprotion of variance expalined by all components.


#--------------------------------------------------------------------------------------
# (g) Take a look at the scree plot and interpret the results.
plot(pca)
screeplot(pca, type = "l",main = "screeplot")


#--------------------------------------------------------------------------------------
# (h) Produce a plot of the first two principal components
biplot(pca, scale = 0, cex =.7)

#--------------------------------------------------------------------------------------
# (i) Interpret the loadings for the first two principal components.
# petal length, sepal length and petal width has similar weights or influence on component 1 and slight 
# diffferent influence in componet component 2 but sepal width has totally different influence in comp1 and
# compnent 2.


#--------------------------------------------------------------------------------------
# pca loadings and scores
pca$loadings
# loading of petal width and length for component 2 is  not shown may be becuase its near to 0.
pca$scores

plot(pca$loadings)
plot(pca$scores)

