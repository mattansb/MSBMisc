X <-  c(1, 2, 3, 4, 5, 6)
Y <- c(15, 37, 52, 59, 83, 92)

m1 <- lm(Y ~ X)
m2 <- lm(Y ~ 0 + X)
m3 <- lm(log(Y) ~ X)

# Table 2
data.frame(
  mod1 = sapply(1:8, R2, pred = predict(m1), obs = Y),
  mod2 = sapply(1:8, R2, pred = predict(m2), obs = Y),
  mod3 = sapply(1:8, R2, pred = exp(predict(m3)), obs = Y)
)
