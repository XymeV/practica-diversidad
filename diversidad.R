#Diversidad alfa 

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


#Indice de Chao1
chao1<- function(x){
  s_obs<-length(x[x>0]) 
  f1<-length(x[which(x==1)]) 
  f2<-length(x[which(x==2)])
  return(s_obs+f1^2/(2*f2) )
  
}

#Bolsa 1
shannon(c(9, 7, 5, 5, 5, 4, 4, 3, 3, 3, 2))
pielou(c(9, 7, 5, 5, 5, 4, 4, 3, 3, 3, 2))
simpson(c(9, 7, 5, 5, 5, 4, 4, 3, 3, 3, 2))
simpson2(c(9, 7, 5, 5, 5, 4, 4, 3, 3, 3, 2))
gini(c(9, 7, 5, 5, 5, 4, 4, 3, 3, 3, 2))
chao1(c(9, 7, 5, 5, 5, 4, 4, 3, 3, 3, 2))

#Bolsa 2
shannon(c(18, 15, 10, 5, 4, 4, 3, 3, 2, 2, 1, 1))
pielou(c(18, 15, 10, 5, 4, 4, 3, 3, 2, 2, 1, 1))
simpson(c(18, 15, 10, 5, 4, 4, 3, 3, 2, 2, 1, 1))
simpson2(c(18, 15, 10, 5, 4, 4, 3, 3, 2, 2, 1, 1))
gini(c(18, 15, 10, 5, 4, 4, 3, 3, 2, 2, 1, 1))
chao1(c(18, 15, 10, 5, 4, 4, 3, 3, 2, 2, 1, 1))

#Bolsa 3
shannon(c(67))
pielou(c(67))
simpson(c(67))
simpson2(c(67))
gini(c(67))
chao1(c(67))

#Bolsa 4
shannon(c(9, 8, 7, 6, 6, 2,1, 1))
pielou(c(9, 8, 7, 6, 6, 2,1, 1))
simpson(c(9, 8, 7, 6, 6, 2,1, 1))
simpson2(c(9, 8, 7, 6, 6, 2,1, 1))
gini(c(9, 8, 7, 6, 6, 2,1, 1))
chao1(c(9, 8, 7, 6, 6, 2,1, 1))

#Bolsa 5
shannon(c(88))
pielou(c(88))
simpson(c(88))
simpson2(c(88))
gini(c(88))
chao1(c(88))

#Bolsa 6
shannon(c(13, 12, 12, 12, 11, 7, 4, 3, 1))
pielou(c(13, 12, 12, 12, 11, 7, 4, 3, 1))
simpson(c(13, 12, 12, 12, 11, 7, 4, 3, 1))
simpson2(c(13, 12, 12, 12, 11, 7, 4, 3, 1))
gini(c(13, 12, 12, 12, 11, 7, 4, 3, 1))
chao1(c(13, 12, 12, 12, 11, 7, 4, 3, 1))

#Bolsa 7
shannon(c(14, 10, 3, 3, 2, 2, 2, 1, 1))
pielou(c(14, 10, 3, 3, 2, 2, 2, 1, 1))
simpson(c(14, 10, 3, 3, 2, 2, 2, 1, 1))
simpson2(c(14, 10, 3, 3, 2, 2, 2, 1, 1))
gini(c(14, 10, 3, 3, 2, 2, 2, 1, 1))
chao1(c(14, 10, 3, 3, 2, 2, 2, 1, 1))



