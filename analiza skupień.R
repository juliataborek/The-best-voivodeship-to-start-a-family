dane_as <- dane
dane_as <- subset(dane_as, select = -c(x1,x10,x12,x15,x16,x17,x18,x20,x24,x27))
row.names(dane_as) <- as.matrix(wojewodztwa)

######################### ODLEGŁOŚCI ###########################################

dist(x=dane_as, method = "euclidean")^2       # kwadratowa odległość euklidesowa
dist(x=dane_as, method = "euclidean")         # odległość euklidesowa
dist(x=dane_as, method = "manhattan")         # odległość miejsca (manhattan)
dist(x=dane_as, method = "maximum")  
dist(x=dane_as, method = "canberra")  
dist(x=dane_as, method = "minkowski")         # minkowski = odl. euklidesowa

# metryka euklidesowa
distmatrix <- dist(x=dane_as, method = "euclidean")
x <- as.matrix(distmatrix) [1:16, 1:16]
x<- round(x, digits = 3)  # zaokrąglenie do 3 miejsca po przecinku
min(dist(x=dane_as, method = "euclidean"))
max(x)

# metryka euklidesowa^2
distmatrix <- dist(x=dane_as, method = "euclidean")^2
x <- as.matrix(distmatrix) [1:16, 1:16]
x<- round(x, digits = 3)  # zaokrąglenie do 3 miejsca po przecinku
min(dist(x=dane_as, method = "euclidean")^2)
max(x)

# metryka manhattan
distmatrix <- dist(x=dane_as, method = "manhattan")
x <- as.matrix(distmatrix) [1:16, 1:16]
x<- round(x, digits = 3)  # zaokrąglenie do 3 miejsca po przecinku
min(dist(x=dane_as, method = "manhattan"))
max(x)

# metryka maximum
distmatrix <- dist(x=dane_as, method = "maximum")
x <- as.matrix(distmatrix) [1:16, 1:16]
x<- round(x, digits = 3)  # zaokrąglenie do 3 miejsca po przecinku
min(dist(x=dane_as, method = "maximum"))
max(x)

# metryka canberra
distmatrix <- dist(x=dane_as, method = "canberra")
x <- as.matrix(distmatrix) [1:16, 1:16]
x<- round(x, digits = 3)  # zaokrąglenie do 3 miejsca po przecinku
min(dist(x=dane_as, method = "canberra"))
max(x)

######################## WSTĘP - POJĘCIA #######################################

#install.packages("cluster")
library(cluster)
#install.packages("dendextend")
library(dendextend)

odl <- dist(dane_as, method = "euclidean") #zmienna odległość
dend1 <- hclust(odl, method = "complete") #metoda najdalszego sąsiada
#View(dend1)
dend1$height #wysokości
plot(dend1)
plot(dend1, hang = -1)
grupy <- cutree(dend1, k=4) #k - liczba klas
grupy #podział na grupy
rect.hclust(dend1, k=4, border = "red") #na wykresie podiał na klasy

dend2 <- hclust(dist(x = dane_as, method = "euclidean")^2, method = "ward.D")
#View(dend2)
dend2$height
plot(dend2, hang = -1)
grupy <- cutree(dend2, k=3)
grupy
rect.hclust(dend2, k=3, border="red")

####################### PORÓWNANIE METOD #######################################

######### euklides2
odl <- dist(x=dane_as, method = "euclidean")^2
w_dend_e <- hclust(odl, method = "ward.D")
s_dend_e <- hclust(odl, method = "single")
c_dend_e <- hclust(odl, method = "complete")
a_dend_e <- hclust(odl, method = "average")

par(mfrow=c(2,2))
plot(w_dend_e, hang=-1)
plot(s_dend_e, hang=-1)
plot(c_dend_e, hang=-1)
plot(a_dend_e, hang=-1)
par(mfrow=c(1,1))

c_w_e <- cophenetic(w_dend_e)  # metoda Warda
kof_w_e <- cor(c_w_e, odl) #korelacja miedzy odlegloscia cofenetyczna a odl eukl kwadratowa

c_s_e <- cophenetic(s_dend_e)  # metoda najbliższego sąsiada
kof_s_e <- cor(c_s_e, odl)

c_c_e <- cophenetic(c_dend_e)  # metoda najdalszego sąsiada
kof_c_e <- cor(c_c_e, odl)

c_a_e <- cophenetic(a_dend_e)  # metoda średniej grupowe
kof_a_e <- cor(c_a_e, odl)

kofenetyczna_e <- cbind(kof_w_e, kof_s_e, kof_c_e, kof_a_e)
kofenetyczna_e

colnames(kofenetyczna_e) <- c("Ward", "single", "complete", "average")
kofenetyczna_e <- round(kofenetyczna_e,3)
kofenetyczna_e

############ manhattan

odl <- dist(x=dane_as, method = "manhattan")
s_dend_mh <- hclust(odl, method = "single")
c_dend_mh <- hclust(odl, method = "complete")
a_dend_mh <- hclust(odl, method = "average")

c_s_mh <- cophenetic(s_dend_mh)  # metoda najbliższego sąsiada
kof_s_mh <- cor(c_s_mh, odl)

c_c_mh <- cophenetic(c_dend_mh)  # metoda najdalszego sąsiada
kof_c_mh <- cor(c_c_mh, odl)

c_a_mh <- cophenetic(a_dend_mh)  # metoda średniej grupowe
kof_a_mh <- cor(c_a_mh, odl)

kofenetyczna_mh <- cbind(kof_s_mh, kof_c_mh, kof_a_mh)
kofenetyczna_mh

colnames(kofenetyczna_mh) <- c( "single", "complete", "average")
kofenetyczna_mh <- round(kofenetyczna_mh,3)
kofenetyczna_mh

############ maximum

odl <- dist(x=dane_as, method = "maximum")
s_dend_mx <- hclust(odl, method = "single")
c_dend_mx <- hclust(odl, method = "complete")
a_dend_mx <- hclust(odl, method = "average")


c_s_mx <- cophenetic(s_dend_mx)  # metoda najbliższego sąsiada
kof_s_mx <- cor(c_s_mx, odl)

c_c_mx <- cophenetic(c_dend_mx)  # metoda najdalszego sąsiada
kof_c_mx <- cor(c_c_mx, odl)

c_a_mx <- cophenetic(a_dend_mx)  # metoda średniej grupowe
kof_a_mx <- cor(c_a_mx, odl)

kofenetyczna_mx <- cbind(kof_s_mx, kof_c_mx, kof_a_mx)
kofenetyczna_mx

colnames(kofenetyczna_mx) <- c( "single", "complete", "average")
kofenetyczna_mx <- round(kofenetyczna_mx,3)
kofenetyczna_mx

############ canberra

odl <- dist(x=dane_as, method = "canberra")
s_dend_c <- hclust(odl, method = "single")
c_dend_c <- hclust(odl, method = "complete")
a_dend_c <- hclust(odl, method = "average")

c_s_c <- cophenetic(s_dend_c)  # metoda najbliższego sąsiada
kof_s_c <- cor(c_s_c, odl)

c_c_c <- cophenetic(c_dend_c)  # metoda najdalszego sąsiada
kof_c_c <- cor(c_c_c, odl)

c_a_c <- cophenetic(a_dend_c)  # metoda średniej grupowe
kof_a_c <- cor(c_a_c, odl)

kofenetyczna_c <- cbind(kof_s_c, kof_c_c, kof_a_c)
kofenetyczna_c

colnames(kofenetyczna_c) <- c( "single", "complete", "average")
kofenetyczna_c <- round(kofenetyczna_c,3)
kofenetyczna_c

############ euklides

odl <- dist(x=dane_as, method = "euclidean")
s_dend_e1 <- hclust(odl, method = "single")
c_dend_e1 <- hclust(odl, method = "complete")
a_dend_e1 <- hclust(odl, method = "average")

c_s_e1 <- cophenetic(s_dend_e1)  # metoda najbliższego sąsiada
kof_s_e1 <- cor(c_s_e1, odl)

c_c_e1 <- cophenetic(c_dend_e1)  # metoda najdalszego sąsiada
kof_c_e1 <- cor(c_c_e1, odl)

c_a_e1 <- cophenetic(a_dend_e1)  # metoda średniej grupowe
kof_a_e1 <- cor(c_a_e1, odl)

kofenetyczna_e1 <- cbind(kof_s_e1, kof_c_e1, kof_a_e1)
kofenetyczna_e1

colnames(kofenetyczna_e1) <- c( "single", "complete", "average")
kofenetyczna_e1 <- round(kofenetyczna_e1,3)
kofenetyczna_e1

##### porównanie metod
porównanie <- t(rbind(kofenetyczna_e1, kofenetyczna_mh, kofenetyczna_mx,kofenetyczna_c))
colnames(porównanie) <- c("euklides", "manhattan", "maximum", "canberra")
porównanie
kofenetyczna_e

odl_e2 <- dist(x=dane_as, method = "euclidean")^2
w_dend_e <- hclust(odl_e2, method = "ward.D")
odl_e <- dist(x=dane_as, method = "euclidean")
a_dend_e <- hclust(odl_e, method = "average")
odl_mx <- dist(x=dane_as, method = "maximum")
a_dend_mx <- hclust(odl_mx, method = "average")
c_dend_mx <- hclust(odl_mx, method = "complete")


par(mfrow=c(2,2))
plot(c_dend_mx, hang=-1)
plot(a_dend_mx, hang=-1)
plot(a_dend_e, hang=-1)
plot(w_dend_e, hang=-1)

par(mfrow=c(1,1))

odl_e2 <- dist(x=dane_as, method = "euclidean")^2
a_dend_e <- hclust(odl_e2, method = "average")
plot(a_dend_e, hang=-1)

odl_c <- dist(x=dane_as, method = "canberra")
a_dend_c <- hclust(odl_c, method = "average")
plot(a_dend_c, hang=-1)

odl_mx <- dist(x=dane_as, method = "maximum")
s_dend_mx <- hclust(odl_mx, method = "single")
plot(s_dend_mx, hang=-1)

odl_e <- dist(x=dane_as, method = "euclidean")
s_dend_e<- hclust(odl_e, method = "single")
plot(s_dend_e, hang=-1)

par(mfrow=c(2,2))
plot(s_dend_mx, hang=-1)
plot(a_dend_e, hang=-1)
plot(a_dend_c, hang=-1)
plot(s_dend_e, hang=-1)
par(mfrow=c(1,1))

################ Kryterium Mojeny ##############################################


### Najlepszy wynik
odl_e2 <- dist(x=dane_as, method = "euclidean")^2
w_dend_e2 <- hclust(odl_e2, method = "ward.D")

w_dend_e2$height
wys <- c(0, w_dend_e2$height)
a = 0.7 # bardziej restrykcyjny
Mojena <- mean(wys) + a * sd(wys)
Mojena

Mojena2 <- mean(wys) + 1.25 *sd(wys)
Mojena2

################## METODA ŚREDNIEJ GRUPOWEJ DLA MAXIMUM i PROFILE ##########################

par(mfrow=c(2,1))
# a= 0,7 wiecej klas
plot(w_dend_e2, hang = -1)
abline(h=Mojena, col = "red")
rect.hclust(w_dend_e2, k=4, border="green")

# a = 1,25 mniej klas
plot(w_dend_e2, hang = -1)
abline(h=Mojena2, col = "red")
rect.hclust(w_dend_e2, k=3, border="green")




grupy <- cutree(w_dend_e2, k=3)
grupy
dane_as_gr2 <- dane_as
dane_as_gr2$grupy = grupy #dopisujemy do naszych danych kolumnę z grupą
grupy


profile = dane_as_gr2 %>%
  group_by(grupy) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
profile
#View(profile)

profile_2 = t(profile)
#View(profile_2)
colnames(profile_2) = c("skupienie_1", "skupienie_2", "skupienie_3")

profile_2 = profile_2[-1,]
l = nrow(profile_2)  
profile <- round(profile_2, digits = 3) 


profile_2 <- as.data.frame(profile_2)  
dt2 = as.data.frame(list(średnia_wartość = c(profile_2$skupienie_1, profile_2$skupienie_2, profile_2$skupienie_3),
                         grupy = c(rep("SKUPIENIE 1", l), rep("SKUPIENIE 2", l), rep("SKUPIENIE 3", l))))

dt2 = as.data.frame(list(średnia_wartość = c(profile_2$skupienie_1, profile_2$skupienie_2, profile_2$skupienie_3),
                         grupy = c(rep("SKUPIENIE 1", l), rep("SKUPIENIE 2", l), rep("SKUPIENIE 3", l)),
                         zmienne = c("X2", "X3", "X4", "X5","X6", "X7", "X8", "X9", 
                                     "X11", "X13", "X14", "X19", 
                                     "X21", "X22", "X23","X25", "X26")))
df2 = dt2

ggplot(data = df2, aes(x = zmienne, y = średnia_wartość, group = grupy, color = grupy)) +
  geom_line(size = 2) + geom_point()+
  scale_color_brewer(palette = "Greens") + 
  theme_minimal() + ggtitle("Profile skupień") + labs(fill = "Grupy")

ggplot(data = df2, aes(x = zmienne, y = średnia_wartość, group = grupy, color = grupy)) +
  geom_line(size = 2) + geom_point()+
  scale_color_brewer(palette = "Set1") + 
  theme_light()

ggplot(data = df2, aes(x = zmienne, y = średnia_wartość, group = grupy, color = grupy)) +
  geom_line(size = 2) + geom_point()+
  scale_color_brewer(palette = "Set1") + 
  theme_grey() + theme (legend.position = "top",
                        panel.grid.major.x = element_line (colour = "black" , linetype = "dashed" ) ,
                        panel.grid.major.y = element_blank(),
                        panel.grid.minor = element_blank() )

############## PO USUNIĘCIU x5,x6,x19,x25####################
dane_as2 <- dane_as
dane_as2 <- subset(dane_as2, select = -c(x5,x6,x19,x25))
row.names(dane_as2) <- as.matrix(wojewodztwa)

####################### PORÓWNANIE METOD #######################################

######### euklides2
odl <- dist(x=dane_as2, method = "euclidean")^2
w_dend_e <- hclust(odl, method = "ward.D")
s_dend_e <- hclust(odl, method = "single")
c_dend_e <- hclust(odl, method = "complete")
a_dend_e <- hclust(odl, method = "average")


c_w_e <- cophenetic(w_dend_e)  # metoda Warda
kof_w_e <- cor(c_w_e, odl) #korelacja miedzy odlegloscia cofenetyczna a odl eukl kwadratowa

c_s_e <- cophenetic(s_dend_e)  # metoda najbliższego sąsiada
kof_s_e <- cor(c_s_e, odl)

c_c_e <- cophenetic(c_dend_e)  # metoda najdalszego sąsiada
kof_c_e <- cor(c_c_e, odl)

c_a_e <- cophenetic(a_dend_e)  # metoda średniej grupowe
kof_a_e <- cor(c_a_e, odl)

kofenetyczna_e <- cbind(kof_w_e, kof_s_e, kof_c_e, kof_a_e)
kofenetyczna_e

colnames(kofenetyczna_e) <- c("Ward", "single", "complete", "average")
kofenetyczna_e <- round(kofenetyczna_e,3)
kofenetyczna_e

############ manhattan

odl <- dist(x=dane_as2, method = "manhattan")
s_dend_mh <- hclust(odl, method = "single")
c_dend_mh <- hclust(odl, method = "complete")
a_dend_mh <- hclust(odl, method = "average")

c_s_mh <- cophenetic(s_dend_mh)  # metoda najbliższego sąsiada
kof_s_mh <- cor(c_s_mh, odl)

c_c_mh <- cophenetic(c_dend_mh)  # metoda najdalszego sąsiada
kof_c_mh <- cor(c_c_mh, odl)

c_a_mh <- cophenetic(a_dend_mh)  # metoda średniej grupowe
kof_a_mh <- cor(c_a_mh, odl)

kofenetyczna_mh <- cbind(kof_s_mh, kof_c_mh, kof_a_mh)
kofenetyczna_mh

colnames(kofenetyczna_mh) <- c( "single", "complete", "average")
kofenetyczna_mh <- round(kofenetyczna_mh,3)
kofenetyczna_mh

############ maximum

odl <- dist(x=dane_as2, method = "maximum")
s_dend_mx <- hclust(odl, method = "single")
c_dend_mx <- hclust(odl, method = "complete")
a_dend_mx <- hclust(odl, method = "average")


c_s_mx <- cophenetic(s_dend_mx)  # metoda najbliższego sąsiada
kof_s_mx <- cor(c_s_mx, odl)

c_c_mx <- cophenetic(c_dend_mx)  # metoda najdalszego sąsiada
kof_c_mx <- cor(c_c_mx, odl)

c_a_mx <- cophenetic(a_dend_mx)  # metoda średniej grupowe
kof_a_mx <- cor(c_a_mx, odl)

kofenetyczna_mx <- cbind(kof_s_mx, kof_c_mx, kof_a_mx)
kofenetyczna_mx

colnames(kofenetyczna_mx) <- c( "single", "complete", "average")
kofenetyczna_mx <- round(kofenetyczna_mx,3)
kofenetyczna_mx

############ canberra

odl <- dist(x=dane_as2, method = "canberra")
s_dend_c <- hclust(odl, method = "single")
c_dend_c <- hclust(odl, method = "complete")
a_dend_c <- hclust(odl, method = "average")

c_s_c <- cophenetic(s_dend_c)  # metoda najbliższego sąsiada
kof_s_c <- cor(c_s_c, odl)

c_c_c <- cophenetic(c_dend_c)  # metoda najdalszego sąsiada
kof_c_c <- cor(c_c_c, odl)

c_a_c <- cophenetic(a_dend_c)  # metoda średniej grupowe
kof_a_c <- cor(c_a_c, odl)

kofenetyczna_c <- cbind(kof_s_c, kof_c_c, kof_a_c)
kofenetyczna_c

colnames(kofenetyczna_c) <- c( "single", "complete", "average")
kofenetyczna_c <- round(kofenetyczna_c,3)
kofenetyczna_c

############ euklides

odl <- dist(x=dane_as2, method = "euclidean")
s_dend_e1 <- hclust(odl, method = "single")
c_dend_e1 <- hclust(odl, method = "complete")
a_dend_e1 <- hclust(odl, method = "average")

c_s_e1 <- cophenetic(s_dend_e1)  # metoda najbliższego sąsiada
kof_s_e1 <- cor(c_s_e1, odl)

c_c_e1 <- cophenetic(c_dend_e1)  # metoda najdalszego sąsiada
kof_c_e1 <- cor(c_c_e1, odl)

c_a_e1 <- cophenetic(a_dend_e1)  # metoda średniej grupowe
kof_a_e1 <- cor(c_a_e1, odl)

kofenetyczna_e1 <- cbind(kof_s_e1, kof_c_e1, kof_a_e1)
kofenetyczna_e1

colnames(kofenetyczna_e1) <- c( "single", "complete", "average")
kofenetyczna_e1 <- round(kofenetyczna_e1,3)
kofenetyczna_e1

##### porównanie metod
porównanie <- t(rbind(kofenetyczna_e1, kofenetyczna_mh, kofenetyczna_mx,kofenetyczna_c))
colnames(porównanie) <- c("euklides", "manhattan", "maximum", "canberra")
porównanie
kofenetyczna_e


# 3 najlepsze + ward
odl_mx <- dist(x=dane_as2, method = "maximum")
a_dend_mx <- hclust(odl_mx, method = "average")
#plot(a_dend_mx, hang=-1)
c_dend_mx <- hclust(odl_mx, method = "complete")

odl_e <- dist(x=dane_as2, method = "euclidean")
a_dend_e <- hclust(odl_e, method = "average")

odl_e2 <- dist(x=dane_as2, method = "euclidean")^2
w_dend_e2<- hclust(odl_e2, method = "ward.D")

par(mfrow=c(2,2))
plot(a_dend_mx, hang=-1)
plot(c_dend_mx, hang=-1)
plot(a_dend_e, hang=-1)
plot(w_dend_e2, hang=-1)
par(mfrow=c(1,1))


# kolejne 4 najlepsze
s_dend_mx <- hclust(odl_mx, method = "single")

odl_c <- dist(x=dane_as2, method = "canberra")
a_dend_c <- hclust(odl_c, method = "average")

a_dend_e2<- hclust(odl_e2, method = "average")

odl_mh <- dist(x=dane_as2, method = "manhattan")
a_dend_mh <- hclust(odl_mh, method = "average")

par(mfrow=c(2,2))
plot(s_dend_mx, hang=-1)
plot(a_dend_c, hang=-1)
plot(a_dend_e2, hang=-1)
plot(a_dend_mh, hang=-1)
par(mfrow=c(1,1))


################ Kryterium Mojeny ##############################################


### Najlepszy wynik
odl_e2 <- dist(x=dane_as2, method = "euclidean")^2
w_dend_e2 <- hclust(odl_e2, method = "ward.D")

w_dend_e2$height
wys <- c(0, w_dend_e2$height)
a = 0.7 # bardziej restrykcyjny
Mojena <- mean(wys) + a * sd(wys)
Mojena

Mojena2 <- mean(wys) + 1.25 *sd(wys)
Mojena2

################## METODA WARDA i PROFILE ##########################

par(mfrow=c(1,2))
# a= 0,7 wiecej klas
plot(w_dend_e2, hang = -1)
abline(h=Mojena, col = "red")
grupy <- cutree(w_dend_e2, k=4)
grupy
rect.hclust(w_dend_e2, k=4, border="green")
# a = 1,25 mniej klas
plot(w_dend_e2, hang = -1)
abline(h=Mojena2, col = "red")
rect.hclust(w_dend_e2, k=3, border="green")



grupy <- cutree(w_dend_e2, k=3)
grupy
dane_as_gr <- dane_as2
dane_as_gr$grupy = grupy #dopisujemy do naszych danych kolumnę z grupą
grupy


profile = dane_as_gr %>%
  group_by(grupy) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
profile
#View(profile)

profile_2 = t(profile)
#View(profile_2)
colnames(profile_2) = c("skupienie_1", "skupienie_2", "skupienie_3")

profile_2 = profile_2[-1,]
l = nrow(profile_2)  
profile <- round(profile_2, digits = 3) 


profile_2 <- as.data.frame(profile_2)  
dt2 = as.data.frame(list(średnia_wartość = c(profile_2$skupienie_1, profile_2$skupienie_2, profile_2$skupienie_3),
                         grupy = c(rep("SKUPIENIE 1", l), rep("SKUPIENIE 2", l), rep("SKUPIENIE 3", l))))

dt2 = as.data.frame(list(średnia_wartość = c(profile_2$skupienie_1, profile_2$skupienie_2, profile_2$skupienie_3),
                         grupy = c(rep("SKUPIENIE 1", l), rep("SKUPIENIE 2", l), rep("SKUPIENIE 3", l)),
                         zmienne = c("X2", "X3", "X4",  "X7", "X8", "X9", 
                                     "X11", "X13", "X14", 
                                     "X21", "X22", "X23",  "X26")))
df2 = dt2

ggplot(data = df2, aes(x = zmienne, y = średnia_wartość, group = grupy, color = grupy)) +
  geom_line(size = 2) + geom_point()+
  scale_color_brewer(palette = "Greens") + 
  theme_minimal() + ggtitle("Profile skupień") + labs(fill = "Grupy")

profile_2 <- round(profile_2, digits = 3)
profile_2$średnie <- c(39.000 ,58.802, 26.776,  7.438, 59.034, 38.241, 12.831, 11.688, 21.438,
                       30.606, 15.137, 15.531, 54.794)

### ciekawsze wykresy dendrogramow
library(igraph)

fviz_dend(w_dend_e2, cex = 1.05, lwd = 2, k = 3, rect = TRUE, 
          k_colors = "jco", rect_border = "jco", rect_fill = TRUE, type = "rectangle") + ggtitle("Podział na grupy",) +
  theme(plot.title = element_text(size = 40, face = "bold"))
ggsave("mtcars.png", width = 60, height = 40, units = "cm")
fviz_dend(w_dend_e2, cex = 1.2, lwd = 2, k = 3, rect = TRUE, 
          k_colors = "jco", rect_border = "jco", rect_fill = TRUE, type = "circular")
fviz_dend(w_dend_e2, cex = 1.2, lwd = 2, k = 3, rect = TRUE, 
          k_colors = "jco", rect_border = "jco", rect_fill = TRUE, type = "phylogenic", 
          repel = TRUE, phylo_layout = "layout_as_tree")
