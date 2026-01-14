################################################################################
#### Introduction à l'actuariat II - Hiver 2026 ################################
#### École d'actuariat - Université Laval ######################################
#### Atelier 1 #################################################################
#### Auteur : Philippe Leblanc #################################################
################################################################################

###
### Premier exemple général
###

rm(list = ls())

# Il est important d'être en mesure de déduire le support ainsi que
# les probabilités associées à une variable aléatoire discrète à partir
# de sa fonction génératrice des probabilités.

x <- c(0, 100, 400, 1000, 2000)
fx <- c(0.3, 0.15, 0.4, 0.1, 0.05)

sum(fx)     # Vérification

## p.11
EspX <- sum(x * fx)
EspX

## p.12
EspX2 <- sum(x^2 * fx)
EspX2

## p.13
VarX <- EspX2 - EspX^2
VarX

## p.14
Fx <- function(k) sum(fx * I(x <= k))
sapply(c(30, 764, 1343), Fx)

## p.15
zeta <- function(k) sum(fx * I(x > k))
sapply(c(800, EspX, 1.5 * EspX, 1400), zeta)

## p.16
EspMin <- function(d) sum(pmin(x, d) * fx)
sapply(c(500, 800, 1400), EspMin)

## p.17
SL <- function(d) sum(pmax(x - d, 0) * fx)
sapply(c(500, 800, 1400), SL)

## p.18
FGM <- function(t) sum(exp(t * x) * fx)
ENTR <- function(rho) log(FGM(rho))/rho
sapply(c(0.0005, 0.001, 0.002), ENTR)


###
### Deuxième exemple général
###

rm(list = ls())

a1 <- 0.2; a2 <- 3
x <- 0:2000; k <- 1:2000

fx <- c(1 - a1, a1 * ((k/2000)^a2 - ((k - 1)/2000)^a2))
sum(fx)     # Vérification

## Espérance
EspX <- sum(x * fx)
EspX

## Seconde moment
EspX2 <- sum(x^2 * fx)
EspX2

## Variance
VarX <- EspX2 - EspX^2
VarX

## Fonction de répartition
Fx <- function(k) sum(fx * I(x <= k))
sapply(c(30, 764, 1343), Fx)

## Espérance limitée
EspLim <- function(d) sum(pmin(x, d) * fx)
sapply(c(500, 800, 1400), EspLim)

## Fonction stop-loss
SL <- function(d) sum(pmax(x - d, 0) * fx)
sapply(c(500, 800, 1400), SL)

## Mesure entropique
FGM <- function(t) sum(exp(t * x) * fx)
ENTR <- function(rho) log(FGM(rho))/rho
sapply(c(0.0005, 0.001, 0.002), ENTR)


###
### Espérance d’une v.a. positive
###

rm(list = ls())

# Nous observons que la variable aléatoire X obéit à un mélange de deux
# lois binomiales de paramètres (n = 100, q = 0.1) et (n = 100, q = 0.6)
# avec des poids respectifs de 0.8 et 0.2.

n <- 100
w <- c(0.8, 0.2)
q <- c(0.1, 0.6)

# Option 1
fx <- w[1] * dbinom(0:n, n, q[1]) + w[2] * dbinom(0:n, n, q[2])

# Option 2
fx <- sapply(0:n, function(x) sum(w * dbinom(x, n, q)))

## Vérification
sum(fx)

## Espérance
EspX <- sum(0:n * fx)
EspX


###
### Fonction stop-loss pour une loi de Poisson
###

rm(list = ls())

lam <- 5
xmax <- 100

x <- 0:xmax
fx <- dpois(x, lam)

SL <- function(d) sum(pmax(x - d, 0) * fx)
barplot(sapply(0:10, SL), names.arg = 0:10, col = 'lightcoral')


###
### Fonction stop-loss pour une loi binomiale négative
###

rm(list = ls())

r <- 1/2
q <- 1/11
xmax <- 100

x <- 0:xmax
fx <- dnbinom(x, r, q)

SL <- function(d, fx) sum(pmax(x - d, 0) * fx)
barplot(sapply(0:20, SL, fx = fx), names.arg = 0:20, col = 'lightcoral')


###
### Fonction stop-loss pour un mélange de lois binomiales
###

rm(list = ls())

n <- 100
x <- 0:n
w <- c(0.8, 0.2)
q <- c(0.0125, 0.2)

# Option 1
fx <- w[1] * dbinom(x, n, q[1]) + w[2] * dbinom(x, n, q[2])

# Option 2
fx <- sapply(0:n, function(x) sum(w * dbinom(x, n, q)))

SL <- function(d) sum(pmax(x - d, 0) * fx)

barplot(sapply(0:30, SL), names.arg = 0:30, col = 'lightcoral')


###
### Fonction stop-loss pour une loi gamma
###

rm(list = ls())

al <- 5
be <- 1

SL <- function(d) al/be * pgamma(d, al + 1, be, lower = FALSE) -
                  d * pgamma(d, al, be, lower = FALSE)

curve(SL(x), from = 0, to = 30, col = 'lightcoral', lwd = 3)


###
### Fonction stop-loss pour une loi log-normale
###

rm(list = ls())

mu <- log(5) - 0.125
sig <- 0.5

SL <- function(d) exp(mu + sig^2/2) * pnorm((log(d) - mu - sig ^2)/sig,
    lower = FALSE) - d * pnorm((log(d) - mu)/sig, lower = FALSE)

curve(SL(x), from = 0, to = 30, col = 'lightcoral', lwd = 3)


###
### Troisième exemple général
###

rm(list = ls())

i <- 1:1000
q <- exp(-6 + 0.0045 * i)/(1 + exp(-6 + 0.0045 * i))

## 1)
range(q)

## 2)
EspS <- sum(q)
EspS

## 3)
VarS <- sum(q * (1 - q))
sqrt(VarS)

## 4)
entr <- function(rho) 1/rho * log(prod(1 - q + q * exp(rho)))
entr(0.4)
