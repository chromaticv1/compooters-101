![[Pasted image 20250201013645.png]]

We have simulated a simple linear model of the following:
$$Y = \alpha+\beta X+\epsilon$$
added outliers and the rest is history 🤷
```r
rm(list=ls())
library(ggplot2)
set.seed(79)
n = 100
alpha = 5
beta = 10

simulation = function(n_outlier, outlierDeviation){
  X = runif(n, 10,50)
  epsilon = rnorm(n, 0, 10)
  epsilon[floor(runif(n_outlier, 1, n+1))] = runif(n_outlier, -outlierDeviation, outlierDeviation)
  
  Y = alpha + X*beta + epsilon
  fit = lm(Y~X)
  #print(fit$coefficients)
  #distance = norm(betaVector-fit$coefficients, type="2")/norm(fit$coefficients, type="2")
  #
  return(summary(fit)$sigma)
}
x = 1:30
y = c()
for (i in x){
  #y[i] = mean(c(simulation(i),simulation(i), simulation(i), simulation(i),simulation(i)))
  
  y[i] = mean(sapply(i, simulation, 100))
}
df = data.frame(x,y)
p = ggplot(data= df, aes(x=x,y=y)) +
  geom_point() +
  labs(title="MSE vs Number of outliers",
                      x= "Number of outliers",
                      y= "Mean squares error")+
  stat_smooth(method = lm, col = "red")
print(p)
```