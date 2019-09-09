library(MASS)
library(ggplot2)
library(plot3D)
library(rgl)
library(factoextra)
library(FactoMineR)
library(car)
library(gridExtra)
library(cowplot)


# Question 1

data(survey)
help(survey)
summary(survey)


# Question 2

base.0 = survey[, c(1,2,3,10)]
head(base.0)

# Question 3

base.1 = na.omit(base.0)  # na : not available

str(base.1)

# Question 4

sexe = base.1$Sex
couleur = ifelse(base.1$Sex == "Male", "blue", "red")

# Question 5

X = base.1[, c(2,3,4)]
head(X)
# plot(X, col=couleur, pch=16)


# Question 6 

#plot3d(X, type = 's', col = coleur, size = 2)



# Question 7 


#------------ Question n°10 à 12 ------------ 
# On appli l'ACP sur la base.1 car base.0 individus pas tous actifs

help(PCA)
# PCA(X, scale.unit = TRUE, ncp = 5, ind.sup = NULL, 
#     quanti.sup = NULL, quali.sup = NULL, row.w = NULL, 
#     col.w = NULL, graph = TRUE, axes = c(1,2))

res.pca = PCA(base.1, quali.sup = c(1), graph=FALSE)

#------------ Question n°13 ------------ 

# res.pca
res.pca$call$row.w
res.pca$call$col.w
summary(res.pca)


#------------ Question n°14 ------------ 

res.pca$eig
plot(res.pca$eig[1:3])


#------------ Question n°15 ------------ 

fviz_pca_ind(res.pca,axes=c(1,2),habillage=sexe) +  xlim(-4, 4) + ylim (-4, 4)
fviz_pca_ind(res.pca,axes=c(1,3),habillage=sexe) +  xlim(-4, 4) + ylim (-4, 4)
fviz_pca_ind(res.pca,axes=c(2,3),habillage=sexe) +  xlim(-4, 4) + ylim (-4, 4)


#------------ Question n°16 ------------ 

fviz_pca_var(res.pca,axes=c(1,2))
res.pca$var

res.pca$var$cos2
res.pca$var$cos2[,c(1,2)]



#------------ Question n°17 ------------ 

fviz_pca_biplot(res.pca, geom.ind = "point", repel = TRUE, habillage = sexe)


