#Creating random walk

z <- rnorm(500)
c <- cumsum(z)

data.frame(z,c)[1:10,]
plot(c, type = 'l')

# Creating moving average functions
n <- length(z)

y.ma1 <- y.ma2 <- rep(0,n)

for(i in 2:n) {
  y.ma1[i] <- z[i] - 0.8*z[i-1]
}
#vectorized: 
# y.ma1 <- z[2:n] - 0.8*z[1:(n-1)]

for(i in 3:n) {
  y.ma2[i]<- z[i] + 0.7*z[i-1] - 0.5*z[i-2]
}
#vectorized: 
# y.ma2 <- z[3:n] + 0.7*z[2:(n-1)] - 0.5*z[1:(n-2)]

library("astsa")
acf1(y.ma1)
acf1(y.ma2)

#Creating Auto-regressive(1) Example
y.ar1.1 <- rep(0,n)
y.ar1.1[1] <- z[1]

for(i in 2:n) {
  y.ar1.1[i] <- 0.3*y.ar1.1[i-1] + z[i] 
}

acf1(y.ar1.1)
acf2(y.ar1.1)