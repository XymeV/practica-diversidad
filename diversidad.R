### DIVERSIDAD ALFA

#Indice de Shannon
shannon<- function (abundancias){
  prob<- abundancias/sum(abundancias)
  return(-sum(prob*log(prob)))
}
#Indice de Pielou
pielou<- function(abundancias){
  shannon(abundancias)/log(length(abundancias))
}
#Indice de Simpson
simpson<- function(abundancias){
  prob<- abundancias/sum(abundancias)
  sum (prob*prob)
}
#Indice de Simpson inverso
simpson2<- function(abundancias){
  prob<- abundancias/sum(abundancias)
  1/ sum (prob*prob)
}
#Indice de Gini
gini<- function(abundancias){
  prob<- abundancias/sum(abundancias)
  simp<- sum(prob*prob)
  return(1-simp)
}
#Indice de Chao1
chao1<- function(x){
  s_obs<-length(x[x>0]) 
  f1<-length(x[which(x==1)]) 
  f2<-length(x[which(x==2)])
  return(s_obs+f1^2/(2*f2) )
}


alfa<- function(abundancias){
  lista_indices<-list(
    shannon= shannon(abundancias),
    pielou= pielou(abundancias),
    simpson= simpson(abundancias),
    simpson2= simpson2(abundancias),
    gini= gini(abundancias),
    chao1= chao1(abundancias)
  )
  return(lista_indices)
}


alfa(c(9, 7, 5, 5, 5, 4, 4, 3, 3, 3, 2))
alfa(c(18, 15, 10, 5, 4, 4, 3, 3, 2, 2, 1, 1))
alfa(c(67))
alfa(c(9, 8, 7, 6, 6, 2,1, 1))
alfa(c(88))
alfa(c(13, 12, 12, 12, 11, 7, 4, 3, 1))
alfa(c(14, 10, 3, 3, 2, 2, 2, 1, 1))


### DIVERSIDAD BETA
## Para las matrices de distancia usando vegdist

#Matriz Jaccard
presencia2 <- read.csv(file= "datos/presencia2.csv")
rownames(presencia2)<- presencia2$especie
presencia2 <- presencia2 [,-1]
presencia2

library(vegan)
jac_matrix <- vegdist (presencia2, method="jaccard")
jac_matrix


#Matriz Bray-Curtis
abundancia2 <- read.csv(file = "datos/abundancias2.csv")
rownames(abundancia2)<- abundancia2$bolsa
abundancia2 <- abundancia2 [,-1]
abundancia2

bray_matrix <- vegdist (abundancia2, method="bray")
bray_matrix

#Indice de Jaccard 
jaccard <- function(x,y) {  
  intersection = length(which(x == 1 & y == 1))
  union = (length(which(x == 1)) + length(which(y == 1))) - intersection
  return(intersection/union)
}

#Bolsa 1
jaccard(presencia$bolsa_1, presencia$bolsa_2)
jaccard(presencia$bolsa_1, presencia$bolsa_3)
jaccard(presencia$bolsa_1, presencia$bolsa_4)
jaccard(presencia$bolsa_1, presencia$bolsa_5)
jaccard(presencia$bolsa_1, presencia$bolsa_6)
jaccard(presencia$bolsa_1, presencia$bolsa_7)

#Bolsa 2 
jaccard(presencia$bolsa_2, presencia$bolsa_2)
jaccard(presencia$bolsa_2, presencia$bolsa_3)
jaccard(presencia$bolsa_2, presencia$bolsa_4)
jaccard(presencia$bolsa_2, presencia$bolsa_5)
jaccard(presencia$bolsa_2, presencia$bolsa_6)
jaccard(presencia$bolsa_2, presencia$bolsa_7)

#Bolsa 3 
jaccard(presencia$bolsa_3, presencia$bolsa_3)
jaccard(presencia$bolsa_3, presencia$bolsa_4)
jaccard(presencia$bolsa_3, presencia$bolsa_5)
jaccard(presencia$bolsa_3, presencia$bolsa_6)
jaccard(presencia$bolsa_3, presencia$bolsa_7)

#Bolsa 4 
jaccard(presencia$bolsa_4, presencia$bolsa_4)
jaccard(presencia$bolsa_4, presencia$bolsa_5)
jaccard(presencia$bolsa_4, presencia$bolsa_6)
jaccard(presencia$bolsa_4, presencia$bolsa_7)

#Bolsa 5
jaccard(presencia$bolsa_5, presencia$bolsa_5)
jaccard(presencia$bolsa_5, presencia$bolsa_6)
jaccard(presencia$bolsa_5, presencia$bolsa_7)

#Bolsa 6 
jaccard(presencia$bolsa_6, presencia$bolsa_6)
jaccard(presencia$bolsa_6, presencia$bolsa_7)

#Bolsa 7 
jaccard(presencia$bolsa_7, presencia$bolsa_7)


#Indice de Bray - Curtis
bray <- function(x,y){
  muestras = pmin(x,y)
  minimos = sum(muestras)
  abun = sum(x) + sum(y)
  return(minimos / abun)
}

#Bolsa 1 
bray(abundancia$bolsa_1, abundancia$bolsa_1)
bray(abundancia$bolsa_1, abundancia$bolsa_3)
bray(abundancia$bolsa_1, abundancia$bolsa_4)
bray(abundancia$bolsa_1, abundancia$bolsa_5)
bray(abundancia$bolsa_1, abundancia$bolsa_6)
bray(abundancia$bolsa_1, abundancia$bolsa_7)

#Bolsa 2 
bray(abundancia$bolsa_2, abundancia$bolsa_2)
bray(abundancia$bolsa_2, abundancia$bolsa_3)
bray(abundancia$bolsa_2, abundancia$bolsa_4)
bray(abundancia$bolsa_2, abundancia$bolsa_5)
bray(abundancia$bolsa_2, abundancia$bolsa_6)
bray(abundancia$bolsa_2, abundancia$bolsa_7)

#Bolsa 3 
bray(abundancia$bolsa_3, abundancia$bolsa_3)
bray(abundancia$bolsa_3, abundancia$bolsa_4)
bray(abundancia$bolsa_3, abundancia$bolsa_5)
bray(abundancia$bolsa_3, abundancia$bolsa_6)
bray(abundancia$bolsa_3, abundancia$bolsa_7)

#Bolsa 4 
bray(abundancia$bolsa_4, abundancia$bolsa_4)
bray(abundancia$bolsa_4, abundancia$bolsa_5)
bray(abundancia$bolsa_4, abundancia$bolsa_6)
bray(abundancia$bolsa_4, abundancia$bolsa_7)

#Bolsa 5 
bray(abundancia$bolsa_5, abundancia$bolsa_5)
bray(abundancia$bolsa_5, abundancia$bolsa_6)
bray(abundancia$bolsa_5, abundancia$bolsa_7)

#Bolsa 6 
bray(abundancia$bolsa_6, abundancia$bolsa_6)
bray(abundancia$bolsa_6, abundancia$bolsa_7)

#Bolsa 7 
bray(abundancia$bolsa_7, abundancia$bolsa_7)
