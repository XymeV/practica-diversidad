
#### curva de rarefaccion

library(ggplot2)


sumas <- read.csv(file = "datos/sumas.csv")
sumas

#bolsa 1
b1 <- ggplot(sumas, aes( x=muestra, y=bolsa_1))+
  geom_point(color="red") + geom_line(color="red")+
  xlab ("MUESTRAS") + ylab ("ESPECIES NUEVAS")
b1

#bolsa 2
b2 <- ggplot(sumas, aes( x=muestra, y=bolsa_2))+
  geom_point(color="blue") + geom_line(color="blue")+
  xlab ("MUESTRAS") + ylab ("ESPECIES NUEVAS")
b2

#bolsa 3
b3 <- ggplot(sumas, aes( x=muestra, y=bolsa_3))+
  geom_point(color="purple") + geom_line(color="purple")+
  xlab ("MUESTRAS") + ylab ("ESPECIES NUEVAS")
b3

#bolsa 4
b4 <- ggplot(sumas, aes( x=muestra, y=bolsa_4))+
  geom_point(color="darkgreen") + geom_line(color="darkgreen")+
  xlab ("MUESTRAS") + ylab ("ESPECIES NUEVAS")
b4

#bolsa 5
b5 <- ggplot(sumas, aes( x=muestra, y=bolsa_5))+
  geom_point(color="hotpink") + geom_line(color="hotpink")+
  xlab ("MUESTRAS") + ylab ("ESPECIES NUEVAS")
b5

#bolsa 6
b6 <- ggplot(sumas, aes( x=muestra, y=bolsa_6))+
  geom_point(color="orange") + geom_line(color="orange")+
  xlab ("MUESTRAS") + ylab ("ESPECIES NUEVAS")
b6

#bolsa 7
b7 <- ggplot(sumas, aes( x=muestra, y=bolsa_7))+
  geom_point(color="brown") + geom_line(color="brown")+
  xlab ("MUESTRAS") + ylab ("ESPECIES NUEVAS")
b7



## RANK ABUNDANCE

abundance <- read.csv (file = "datos/abundancias.csv")
abundance

library (ggplot2)

#bolsa 1
rank1 <- ggplot(abundance, aes (x=especie, y=bolsa_1))+
  geom_bar (stat = "identity", fill = (c("purple", "pink", "lightblue", "hotpink2",
                                         "yellow", "lightgreen", "red", "orange",
                                         "blue", "darkblue", "darkgreen", "hotpink")))+
  xlab ("ABUNDANCIAS") + ylab ("ESPECIES")
  
rank1

#bolsa 2
rank2 <- ggplot(abundance, aes (x=especie, y=bolsa_2))+
  geom_bar (stat = "identity", fill = (c("purple", "pink", "lightblue", "hotpink2",
                                         "yellow", "lightgreen", "red", "orange",
                                         "blue", "darkblue", "darkgreen", "hotpink")))+
  xlab ("ABUNDANCIAS") + ylab ("ESPECIES")

rank2

#bolsa 3
rank3 <- ggplot(abundance, aes (x=especie, y=bolsa_3))+
  geom_bar (stat = "identity", fill = (c("purple", "pink", "lightblue", "hotpink2",
                                         "yellow", "lightgreen", "red", "orange",
                                         "blue", "darkblue", "darkgreen", "hotpink")))+
  xlab ("ABUNDANCIAS") + ylab ("ESPECIES")

rank3

#bolsa 4
rank4 <- ggplot(abundance, aes (x=especie, y=bolsa_4))+
  geom_bar (stat = "identity", fill = (c("purple", "pink", "lightblue", "hotpink2",
                                         "yellow", "lightgreen", "red", "orange",
                                         "blue", "darkblue", "darkgreen", "hotpink")))+
  xlab ("ABUNDANCIAS") + ylab ("ESPECIES")

rank4

#bolsa 5
rank5 <- ggplot(abundance, aes (x=especie, y=bolsa_5))+
  geom_bar (stat = "identity", fill = (c("purple", "pink", "lightblue", "hotpink2",
                                         "yellow", "lightgreen", "red", "orange",
                                         "blue", "darkblue", "darkgreen", "hotpink")))+
  xlab ("ABUNDANCIAS") + ylab ("ESPECIES")

rank5

#bolsa 6
rank6 <- ggplot(abundance, aes (x=especie, y=bolsa_6))+
  geom_bar (stat = "identity", fill = (c("purple", "pink", "lightblue", "hotpink2",
                                         "yellow", "lightgreen", "red", "orange",
                                         "blue", "darkblue", "darkgreen", "hotpink")))+
  xlab ("ABUNDANCIAS") + ylab ("ESPECIES")

rank6

#bolsa 7
rank7 <- ggplot(abundance, aes (x=especie, y=bolsa_7))+
  geom_bar (stat = "identity", fill = (c("purple", "pink", "lightblue", "hotpink2",
                                         "yellow", "lightgreen", "red", "orange",
                                         "blue", "darkblue", "darkgreen", "hotpink")))+
  xlab ("ABUNDANCIAS") + ylab ("ESPECIES")

rank7



