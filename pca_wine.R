states =row.names(USArrests)
states
names ( USArrests )
apply ( USArrests , 2, mean )
apply ( USArrests , 2, var )
pr.out =prcomp ( USArrests , scale =TRUE)
scale(USArrests)
```
By default, the `prcomp()` function centers the variables to have mean zero. By using the option `scale=TRUE`, we scale the variables to have standard deviation one. The output from `prcomp()` contains a number of useful quantities.
```{r} 
names(pr.out)
```
The `center` and `scale` components correspond to the means and standard deviations of the variables that were used for scaling prior to implementing PCA.
```{r}
pr.out$center
pr.out$scale
```
The `rotation` matrix provides the principal component loadings; each column of `pr.out$rotation` contains the corresponding principal component loading vector.
```{r}
pr.out$rotation
```
We see that there are four distinct principal components. This is to be expected because there are in general min(n - 1, p) informative principal components in a data set with n observations and p variables.

Using the `prcomp()` function, we do not need to explicitly multiply the data by the principal component loading vectors in order to obtain the principal component score vectors. 
Rather the 50 ? 4 matrix x has as its columns the principal component score vectors. That is, the kth column is the kth principal component score vector.
```{r}
pr.out$x #we got 50 values corresponding to each row
nrow(USArrests)
pr.out$sdev #new std dev of principal components
sd(pr.out$x[,1]) 

#multiply loading vector by 1st row of scaled USArrests to verify the x values
z<-scale(USArrests) #scaling each value in each row and col of USArrests
sum(z[1,]*pr.out$rotation[,1])
sum(z[1,]*pr.out$rotation[,2])

```
We can plot the first two principal components as follows:
  ```{r}
biplot (pr.out , scale =0)
```
The `scale=0` argument to `biplot()` ensures that the arrows are scaled to represent the loadings; other values for scale give slightly different biplots with different interpretations. This scale has nothing to do with scaling...

Urban prop - look at the top axis - is coming to -.27. So PCA1 loading for urban prop is. -.27 and for pca2 loading look horizontally towards right around -.83.

If I multiply the all the loadings by -ve, then, we'll get the mirror-image of bi-plot which will also represent the same PCAs correctly. 

The `prcomp()` function also outputs the standard deviation of each principal component. 
For instance, on the `USArrests`  data set, we can access these standard deviations as follows:

```{r}
pr.out$sdev
```
The variance explained by each principal component is obtained by squaring these:
```{r}
pr.var = pr.out$sdev^2
pr.var
```
To compute the proportion of variance explained by each principal component, we simply divide the variance explained by each principal component by the total variance explained by all four principal components:
```{r}
pve = pr.var/sum(pr.var)
pve
```
We see that the first principal component explains `62.0%` of the variance in the data, the next principal component explains `24.7%` of the variance, and so forth. 
We can plot the PVE explained by each component, as well as the cumulative `PVE`, as follows:

```{r}
#Drawing the scree-plot
plot(pve, xlab ="Principal Component", ylab ="Proportion of Variance Explained", ylim=c(0 ,1) ,type="b")
#This graph shows that 60% of var. explained by PC1, 20% explained by PC2, 10% explained by PC3. We have to select PCs only until the elbow pattern appears i.e the difference in variance explained by the principal components is not 

plot(cumsum (pve), xlab ="Principal Component", ylab =" Cumulative Proportion of Variance Explained ", ylim=c(0 ,1), type="b") 
```
Note cumsum will always be 1

## Code 2

```{r}
wine = read.csv("https://storage.googleapis.com/dimensionless/Analytics/wine.csv")
```
Applying PCA on relevant predictors
```{r}
pca<-prcomp(wine[,3:7],scale=TRUE)
```
Creating biplot
```{r}
biplot(pca,scale=0)
```
Age and France Prop are exactly opposite of each other, in the same direction. As age increases France Prop decreases. 
AGST and WinterRain are exactly opposite. 


Analyzing components of the output
```{r}
#Std Dev
pca$sdev
# Loadings
pca$rotation #z1, z2, z3 etc. ....
# Principal Components
pca$x
```


PC5 is high in coeff in Age and FrancePop and opp signed
PC4 is high in coeff in WinterRain and AGST and HarvestRain
PC3 is high in coeff in AGST and HarvestRain and opp. signed (as expected thru bi-plot)
PC2 is high in coeff in WinterRain and HarvestRain and opposite signed (as expected looking at 
bi-plot); AGST also there; Age and FrancePop cancel each other
PC1 is high in coeff and opposite in Age and France Pop (Opp was expected through bi-plot), cancelling each other; so mostly AGST

Calculating proportion of variance
```{r}
pr.var<-pca$sdev^2
pve<-pr.var/sum(pr.var)
```
Creating scree plot and cumulative plots
```{r}
plot(pve, xlab ="Principal Component", ylab ="Proportion of Variance Explained", ylim=c(0 ,1) ,type="b")
plot(cumsum (pve), xlab ="Principal Component", ylab =" Cumulative Proportion of Variance Explained ", ylim=c(0 ,1), type="b")
```

Building linear regression model using PC1 to PC4
```{r}
predictor<-pca$x[,1:4] #we r using only upto 4 principal components
wine<-cbind(wine,predictor)
model<-lm(Price~PC1+PC2+PC3+PC4,data=wine)
summary(model)
```

So PC1, PC3 are coming most significant. 

#### Making Predictions  
We cannot convert test data into principal components, by applying pca. Instead we have to apply same transformations on test data as we did for train data
```{r}
wineTest = read.csv("https://storage.googleapis.com/dimensionless/Analytics/wine_test.csv")
wineTest

#Converting the test data vars into principal components using the same loadings of Principal Components of train data
pca_test<-predict(pca,wineTest[,3:7])
#it will scale the test data (using center and scale)
#then it will multiply the loadings of train data and convert the test data into principal components and convert the test data into pca_test type

class(pca_test)#output of predict fn is matrix. V need to convert into df.
pca_test
# Converting to data frame
pca_test<-as.data.frame(pca_test)
pca_test
```
Making predictions
```{r}
pred_pca<-predict(object = model, newdata=pca_test)
pred_pca
wineTest$Price
```
