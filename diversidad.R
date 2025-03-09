# ## DIVERSIDAD ALFA

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




## matrices de distancia usando vegdist

# indice jaccard

presencia <- read.csv(file = "datos/presencia.csv")

rownames(presencia)<- presencia$especie
presencia <- presencia [,-1]
presencia

library(vegan)

jac_matrix <- vegdist (presencia, method="jaccard")
jac_matrix


# indice Bray-Curtis

abundancia <- read.csv(file = "datos/abundancias.csv")

rownames(abundancia)<- abundancia$especie
abundancia <- abundancia [,-1]
abundancia

bray_matrix <- vegdist (abundancia, method="bray")
bray_matrix


