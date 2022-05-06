
set.seed(123)

# make fake data with two predictors
# x1 has a "small" range across y, like temperature
# x2 has a "large" range across y, like precipitation
d <- data.frame(x1=seq(1, 30, length.out=100), x2=rnorm(100, 0, 1000))
d$y <- d$x1 + d$x2 + 0.001 * 0.1 * d$x1 * d$x2 + rnorm(100, 0, 20) # fake response

# copy data and center/scale predictors
dscaled <- d
dscaled[ , c('x1', 'x2')] <- scale(dscaled[ , c('x1', 'x2')])

# regressions
r1 <- lm(y ~ x1 * x2, data=d)
r2 <- lm(y ~ x1 * x2, data=dscaled)

# compare... note similarities and differences in t and p values
summary(r1)
summary(r2)

# back-transform slope
c1 <- coefficients(r1)
c2 <- coefficients(r2)

# back-transform slopes... they match those from r1 (but very slightly different)
c2['x1'] / sd(d$x1)
c2['x2'] / sd(d$x2)
c2['x1:x2'] / (sd(d$x1) * sd(d$x2))
summary(r1)

# back-transform SEs...
# different from the unscaled model's SEs for x2 because it has a very different magnitude of change across the range of y
summary(r2)$coefficients['x1', 'Std. Error'] / sd(d$x1)
summary(r2)$coefficients['x2', 'Std. Error'] / sd(d$x2)
summary(r2)$coefficients['x1:x2', 'Std. Error'] / (sd(d$x1) * sd(d$x2))

summary(r1)
