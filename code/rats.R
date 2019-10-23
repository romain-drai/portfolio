
library(pander)
data = read.table('https://raw.githubusercontent.com/wiki/stan-dev/rstan/rats.txt', header = TRUE)
pander(head(data))
x <- c(8, 15, 22, 29, 36)


matplot(x, t(data), type = "b" ,pch=1,col = 1:30, 
        xlab = 'Time (in days)', ylab = 'Weigtht', 
        main = 'Growth of Rats') #plot

# Input data
y <- as.matrix(read.table('https://raw.githubusercontent.com/wiki/stan-dev/rstan/rats.txt', header = TRUE))
x <- c(8, 15, 22, 29, 36)

rats_dat = list(y = y, 
                x = x,
                xbar = mean(x),
                N <- nrow(y),
                T <- ncol(y))

# Fit model using STAN
library(rstan)
options(mc.cores = parallel::detectCores())
rats_fit <- stan('rats.stan', data = rats_dat, 
                 chains = 4, 
                 iter = 2000, 
                 warmup = 1000, 
                 thin = 1)

print(get_elapsed_time(rats_fit))

print(rats_fit, pars = c("alpha", 'beta'), 
      include = FALSE,
      probs = c(.1, 0.5, 0.9))

plot(rats_fit, pars = c("alpha") )
plot(rats_fit, pars = c("beta"))


traceplot(rats_fit, pars = c('mu_beta'), 
          inc_warmup = TRUE )

traceplot(rats_fit, pars = c('alpha0'), 
          inc_warmup = TRUE )

# Extracting Alpha0
alpha0 <- extract(rats_fit, 'alpha0')
alpha0 <- unlist(alpha0, use.names=FALSE)

plot(density(alpha0))

d = density(alpha0)
alpha0.map = d$x[which.max(d$y)]


# Predictive y1
y1_pred <- extract(rats_fit, 'y1_pred')
y1_pred2 <- matrix(unlist(y1_pred), ncol = 5, byrow = FALSE)

med = apply(y1_pred2, 2, median)
upp = apply(y1_pred2, 2, quantile, probs = 0.9)
low = apply(y1_pred2, 2, quantile, probs = 0.1)


library(plotrix)
plotCI(x, med, ui=upp, li=low, col = 'blue', 
       main = 'Predicted Growth of Rat 1')
points(x, med, type = 'b',  col = 'blue')
points(x, data[1,], col = 1, pch=19)



