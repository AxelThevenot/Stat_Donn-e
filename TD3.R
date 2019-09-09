# Exercice  1

F_minus_1 <- function(y) asin(2 * y - 1)

# Soit u une variable aléatoire uniforme sur [0, 1] alors arcsin(2 * u -1) a la même loi que x
# On peut alors l'utiliser pour échantillonner selon la loi de x

runif(1,0,1)
n_max = 10000
x <- c()
for(i in 1:n_max) 
{
  x <- c(x, F_minus_1(runif(1,0,1)))
}

hist(x, breaks = "FD", col='#8800FF', freq=FALSE)


# Exercice  2
lambda <- 1
F_minus_1 <- function(y) - log(1 - y)/lambda

# Soit u une variable aléatoire uniforme sur [0, 1] alors arcsin(2 * u -1) a la même loi que x
# On peut alors l'utiliser pour échantillonner selon la loi de x

runif(1,0,1)
n_max = 10000
x <- c()
for(i in 1:n_max) 
{
  x <- c(x, F_minus_1(runif(1,0,1)))
}

hist(x, breaks = "FD", col='#8800FF', freq=FALSE)


F_minus_1 <- function(y) tan((y -1/2)*pi)

# Soit u une variable aléatoire uniforme sur [0, 1] alors arcsin(2 * u -1) a la même loi que x
# On peut alors l'utiliser pour échantillonner selon la loi de x

n_max = 10000
x <- c()
for(i in 1:n_max) 
{
  x <- c(x, F_minus_1(runif(1,0,1)))
}

hist(x, breaks = "FD", col='#8800FF', freq=FALSE, xlim=range(-10, 10))


# Exercice  3

p <- c(0.4982, 0.2103, 0.127, 0.073, 0.0418, 0.0241, 0.0132, 0.0069, 0.0035, 0.0015, 0.0005)
P <- cumsum(p)

F_minus_1 <- function(y) length(which(P < y)) 

n_max <- 10000
x <- c()
for(i in 1:n_max) 
{
  x <- c(x, F_minus_1(runif(1,0,1)))
}
tab <- table(x)/ n_max

#hist(x, breaks = "FD", col='#8800FF', freq =FALSE)
barplot(tab, space=0, ylim = c(0, 0.6), col='#2F9599') 

# Exercice 5

u <- runif(1, -2, 2)
v <- runif(1, 0, 0.4)
while(v > sqrt(4 - u^2)/2*pi){
  u <- runif(1, -2, 2)
  v <- runif(1, 0, 0.4)
}
lines(u,v,lwd=2)
# HUGO LAROCHELLE 




