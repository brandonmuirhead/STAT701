# Problem 2 in homework 1

###### 2.a

x <- seq(0, 1, length.out = 40)
z <- rnorm(n = 40, mean = 0, sd = 2)    #creates the errors with sd = 2 and mean = 0
y <- 1 + 1.2 * x + z

df <- data.frame(x, y)    #puts the data into a data frame

linear <- lm(y~x, data = df)
lm_coef <- round(coef(linear), 3) # extract coefficients 
plot(x = x, y = y, pch=16, xlab="X Values", 
      ylab="Y Values")  #plot the points

abline(linear, col="red", lwd=4)         # plot best fit line 

mtext(bquote(y == .(lm_coef[2])*x + .(lm_coef[1])), 
      adj = 1, padj = 0) # display equation in top right

summary(linear)   #True B1 should be 1.2 also used to find RMSE

ci <- confint(linear)
ci[2,]            #calculates the CI for B1



###### 2.b Estimating the slope

b1 <- numeric(0)
ci <- data.frame(NULL, nrow = 100, ncol = 2)
x <- seq(0, 1, length.out = 40)
for (i in 1:100) {
      e <- 2 * rnorm(length(x))
      y <- 1 + 1.2*x + e
      l2 <- lm(y~x)
      conf <- confint(l2)
      b1[i] <- l2$coe[2]
      ci[i,] <- conf[2,]
}

hist(b1, breaks=10, col="blue")

