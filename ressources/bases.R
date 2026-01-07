################################################################################
#### Introduction à l'actuariat II - Hiver 2026 ################################
#### École d'actuariat - Université Laval ######################################
#### Auteur: Philippe Leblanc ##################################################
################################################################################

################################################################################
### Partie I - Lois discrètes ##################################################
################################################################################

# Fonction de masse de probabilité
x <- 0:10
fx <- c(0.1, 0.05, 0.11, 0.15, 0.17, 0.08, 0.1, 0.05, 0.09, 0.05, 0.05)

# Espérance
EspX <- sum(x * fx)
EspX

# Variance
VarX <- sum((x - EspX)^2 * fx)
VarX

# Fonction de répartition
Fx <- cumsum(fx)
Fx

# Espérance limitée
EspLim <- function(d) sum(pmin(x, d) * fx)
EspLim(5)

# Espérance tronquée
EspTr <- function(d) sum(x * fx * I(x > d))
EspTr(5)

# Fonction stop-loss
SL <- function(d) sum(pmax(x - d, 0) * fx)
SL(5)

# Fonction génératrice des moments
FGM <- function(t) sum(exp(t * x) * fx)
FGM(0.1)

# Fonction génératrice des probabilités
FGP <- function(t) sum(t^x * fx)
FGP(0.1)

# Value at Risk
VaR <- function(u) x[min(which(Fx >= u))]
VaR(0.95)

# Tail Value at Risk
TVaR <- function(u) SL(VaR(u))/(1 - u) + VaR(u)
TVaR(0.95)

# Left Tail Value at Risk
LTVaR <- function(u) (EspX - (1 - u) * TVaR(u))/u
LTVaR(0.95)

# Range Value at Risk
RVaR <- function(u1, u2) ((1 - u1) * TVaR(u1) - (1 - u2) * TVaR(u2))/(u2 - u1)
RVaR(0.6, 0.95)

