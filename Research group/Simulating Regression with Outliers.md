
Our simulation generates $n$ values of the following model:
$$Y = \alpha+\beta X+\epsilon$$
by:
- generating n $X$'s
- generating n $\epsilon$'s $\sim NID(0, \sigma^2)$
- randomly modifying set number of $\epsilon$ by adding deviation $\sim U(-\delta,+\delta)$ 
- and finally generating $Y$'s by adding the required values together.
Here's our plots:
![[Pasted image 20250201030823.png]]
![[Pasted image 20250201032239.png]]
```r
rm(list=ls())
library(ggplot2)
set.seed(79)
alpha = 5
beta = 10
sigma = 1

n = 1000 #sample size

simulation = function(nOutlier, outlierDeviation){
  X = runif(n, 10,50)
  epsilon = rnorm(n, 0, sigma)
  
  epsilon[sample(1:n,nOutlier)] = rnorm(nOutlier,0,sigma) +
    runif( nOutlier, -outlierDeviation, outlierDeviation) #OUTLIER CREATION
  
  Y = alpha + X*beta + epsilon
  fit = lm(Y~X)
  
  return(summary(fit)$sigma)
}
x = 1:n
y = c()
#1. MSE VS % of outliers
for (i in x){
  y[i] = simulation(i, sigma*100)
}
df = data.frame(x,y)
p = ggplot(data= df, aes(x=x/n*100,y=y)) +
  geom_point() +
  labs(title="1. MSE vs Number of outliers, constant outlier deviation",
                      x= "% of outliers",
                      y= "Mean squares error")+
  stat_smooth(method = NULL, col = "red")
print(p)

#2. MSE vs Outlier Deviation


for (i in x){
  y[i] = simulation(round(n*2/10), sigma*i*100)
}
df = data.frame(x,y)
p = ggplot(data= df, aes(x=x*100,y=y)) +
  geom_point() +
  labs(title="2. MSE vs Outlier deviation, 20% outliers",
       x= "Outlier deviation (delta)",
       y= "Mean squares error")+
  stat_smooth(method = NULL, col = "red")
print(p)
```
## By:
Khalid Muntasir Sawad - 2210424179
Ashik E Elahi - 2210724113
Akib Iqbal - 2210824151
Jamilor Rahman Refat - 2210424116