###
### Atelier 1
###
#
###
### Premier exemple general, p.9
###
A <- c(0, 100, 400, 1000, 2000)
f <- c(0.3, 0.15, 0.4, 0.1, 0.05)

(E <- sum(A * f))

(E2 <- sum(f * A^2))

(Var <- E2 - E^2)

F <- function(x) sapply(x, function(x) sum(f[A <= x]))
F(c(500, 30, 1343, 764))

zeta <- function(x) sapply(x, function(x) sum(f[A > x]))
zeta(c(800, E, 1.5 * E, 1400))

nu <- function(d) sapply(d, function(d) sum(pmin(A, d) * f))
nu(c(800, 500, 1400))

SL <- function(d) sapply(d, function(d) sum(pmax(A - d, 0) * f))
SL(c(800, 500, 1400))

M <- function(t) sapply(t, function(t) sum(exp(t * A) * f))
ent <- function(p) log(M(p)) / p
ent(c(0.001, 0.0005, 0.002))

rm(list = ls())
###
### Deuxieme exemple general, p.20
###
a1 <- 0.2
a2 <- 3
A <- 0:2000
f <- c(1 - a1, a1 * ((1:2000 / 2000)^a2 - (0:1999 / 2000)^a2))
sum(f) # verification

(E <- sum(A * f))

(E2 <- sum(f * A^2))

(Var <- E2 - E^2)

F <- function(x) sapply(x, function(x) sum(f[A <= x]))
F(c(500, 30, 1343, 764))

zeta <- function(x) sapply(x, function(x) sum(f[A > x]))
zeta(c(800, E, 1.5 * E, 1400))

nu <- function(d) sapply(d, function(d) sum(pmin(A, d) * f))
nu(c(800, 500, 1400))

SL <- function(d) sapply(d, function(d) sum(pmax(A - d, 0) * f))
SL(c(800, 500, 1400))

M <- function(t) sapply(t, function(t) sum(exp(t * A) * f))
ent <- function(p) log(M(p)) / p
ent(c(0.001, 0.0005, 0.002))

rm(list = ls())
###
### Ex.1, p.22
###
k <- 0:100
f <- 0.8 * dbinom(k, 100, 0.1) + 0.2 * dbinom(k, 100, 0.6)

sum(f)
sum(f * k)

rm(list = ls())
###
### Ex. 2-4, p.26-27
###
SL <- function(f, k, d) sapply(d, function(d) sum(pmax(k - d, 0) * f))

d <- 0:100
k <- 0:10000

f2 <- dpois(k, 5)
sl2 <- SL(f2, k, d)
sl2[1]
barplot(sl2[1:31], col = "lightcoral")

f3 <- dnbinom(k, 1/2, 1/11)
sl3 <- SL(f3, k, d)
sl3[1]
barplot(sl3[1:31], col = "lightcoral")

k <- 0:100

f4 <- 0.8 * dbinom(k, 100, 0.0125) + 0.2 * dbinom(k, 100, 0.2)
sl4 <- SL(f4, k, d)
sl4[1]
barplot(sl4[1:31], col = "lightcoral")

rm(list = ls())
###
### Ex. 5-6, p.28
###
d <- 0:100

sl_gam <- function(d, al, be) al/be * pgamma(d, al + 1, be, lower = FALSE) -
                                d * pgamma(d, al, be, lower = FALSE)
sl_gam(0, 5, 1) 

matplot(d, cbind(sl_gam(d, 5, 1), sl_gam(d, 1/2, 1/10)), type = c("l", "l"), lwd = c(3, 3), col = c("orange", "lightcoral"), lty = rep(1, 2))


sl_lnorm <- function(d, mu, sig) exp(mu + sig^2 / 2) * pnorm((log(d) - mu - sig^2) / sig, lower = FALSE) -
                                    d * pnorm((log(d) - mu) / sig, lower = FALSE)
sl_lnorm(0, log(5) - 0.125, 0.5)

matplot(d, cbind(sl_lnorm(d, log(5) - 0.125, 0.5), sl_lnorm(d, log(5) - 0.5, 1)), type = c("l", "l"), lwd = c(3, 3), col = c("orange", "lightcoral"), lty = rep(1, 2))

rm(list = ls())
###
### Troisieme exemple general, p.31
###
i <- 1:1000
q <- exp(-6 + 0.0045 * i) / (1 + exp(-6 + 0.0045 * i))
summary(q)

(ES <- sum(q))

(VarS <- sum(q * (1 - q)))
sqrt(VarS)

psi <- function(p) sum(log(1 - q + q * exp(p))) / p
psi(0.4)
