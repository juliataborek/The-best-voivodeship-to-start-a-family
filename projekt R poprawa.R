######## Projekt #######
#### WCZYTANIE DANYCH ####  
library(readxl)
dane <- read_excel("dane gotowe — kopia.xlsx")
View(dane)
wojewodztwa <- dane[, 2]
dane <- dane[,c(3:28)] #tylko cechy ilościowe
row.names(dane) <- as.matrix(wojewodztwa) #przypisanie nazw wierszy

#### STANDARYZACJA ####
scd <- scale (dane, center = T)
scd
scd = round(scd, digits = 3) #zaokrąglenie
View(scd)

#### STATYSTYKI ####
summary(dane)

library(psych)
describe(dane) #trimmed - ucięta średnia
# range - rozstęp, czyli max - min

##### KORELACJE #####
library(corrplot)
library(ppcor)

cor(dane)                    # korealacja całkowita Pearson
kor=round(cor(dane), 3)
print(kor)
pcor(dane)                   # korealacja cząstkowa Pearson, 1 tabela wazna tylko - estimate
kor_cz <- pcor(dane)$estimate
kor_cz <- round(kor_cz, 3)
View(kor_cz)
spcor(dane)                  # korealcja semicząstkowa Pearson
#write.table(kor_cz, "kor_cz.csv")


# heatmapa
heatmap(kor)

# inny wykres korelacji
library(corrplot)
par(mfrow = c(1,2))
corrplot(kor, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

corrplot(kor_cz, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

par(mfrow = c(1,2))
corrplot(kor, type = "upper", 
         tl.col = "black", tl.srt = 45)

corrplot(kor_cz, type = "upper", 
         tl.col = "black", tl.srt = 45)

# Trójkątna macierz korelacji
get_upper_tri <- function(kor){
  kor[lower.tri(kor)]<- NA
  return(kor)
}

library(reshape2)
upper_tri <- get_upper_tri(kor)
View(upper_tri)
#write.table(upper_tri, "kor_trojk.csv", sep = ' ')


melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot2::ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

View(get_upper_tri)

# GINI
library(EnvStats)
library(laeken)
library(tidyverse)


gini_c=c()

for(i in 1:ncol(dane))
{
  value_gini = apply(dane, 2, gini)
  search_value=value_gini[[i]]$value
  gini_c=c(gini_c, search_value)
}

library(magrittr)
library(dplyr)

result=dane %>%
  reframe(zmienne = colnames(dane),
          średnia = (colMeans(dane)),
          odchylenie = apply(dane, 2, sd),
          skośność = apply(dane, 2, skewness),
          współczynnik_zmienności = odchylenie / średnia *100)


result$gini = gini_c
for(i in 2:ncol(result))
{
  result[,i] = round(as.numeric(unlist(result[,i])), 2)
}

result

# ZM. KATEGORYCZNA
dane$x27 <- c("średnie",
              "wysokie",
              "średnie",
              "niskie",
              "średnie",
              "średnie",
              "wysokie",
              "wysokie",
              "średnie",
              "średnie",
              "niskie",
              "wysokie",
              "średnie",
              "niskie",
              "wysokie",
              "niskie"
)

W1 <- ggplot(data=dane, aes(x=as.factor(x27), y = x8)) + geom_boxplot(col="#69b3a2") +
  labs(x = "Poziom wydatków")

W2 <- ggplot(data=dane, aes(x=as.factor(x27), y = x9)) + geom_boxplot(col="#69b3a2") +
  labs(x = 'Poziom wydatków')

W3 <- ggplot(data=dane, aes(x=as.factor(x27), y = x10)) + geom_boxplot(col="#69b3a2") + 
  labs(x = 'Poziom wydatków')

library(gridExtra)

grid.arrange(W1, W2, W3, top = "Liczba szkół na 100 tys. ludności ")

ggplot(data=dane, aes(x=as.factor(x27), y = x11)) + geom_boxplot()

boxplot(dane$x2~dane$x27,
        main = "Zgony niemowląt",
        xlab = "x2",
        ylab = "WYDATKI",
        col = "#69b3a2",
        border = "#69b3e2",
        horizontal = TRUE,
        notch = FALSE)

#WYKRESY
ggplot(dane, aes(x = x17, y = after_stat(density))) +
  geom_histogram() +
  geom_density(color = "green", linewidth = 2)

ggplot(dane, aes(x17)) +
  geom_histogram(bins=6) 

hist(dane$x17) 
plot(density(dane$x17))

######### PCA #########
library(tidyverse)
danepca <- dane
danepca <- danepca %>% select(2,4,8,9,13,14,19,20,21,22,23)

library(corrr)
library(ggcorrplot)
library("FactoMineR")
library(factoextra)
library(ggrepel)
library(ggplot2)
library(ggfortify)


dane_pca <- prcomp(danepca, scale = TRUE)

# wartości własne
eig.val <- get_eigenvalue(dane_pca)
eig.val 

# ładunki składowe - dobre znaki
print(dane_pca)                               
#View(dane_pca)

# współrzędne przypadków - chyba dobre znaki
dane_pca$x                          
round(dane_pca$x[,1],3)                         
round(dane_pca$x[,2],3)
round(dane_pca$x[,3],3)

# korelacje zmiennych ze składową
pc.dane <- princomp(danepca, cor = TRUE)     
str(pc.dane)
#View(pc.dane)
round(cor(danepca, pc.dane$scores), 3)  #tu jest to co pod ładunkami w nawiasie (korelacje), ale złe znaki!!

pc.dane$loadings              # ładunki składowe - złe znaki
pc.dane$sdev                  # odchylenie standardowe składowych głównych
pc.dane$center                # średnie zmiennych
pc.dane$scale                 # odchylenie standardowe zmiennych
pc.dane$scores                # współrzędne przypadków?

################################################################################

# wykres osypiska - liniowy + odcięcie na poziomie wartości własnej równej 1
screeplot(dane_pca, type = "lines", main = "Wykres osypiska")
abline(1,0, col = 'red', lty = 2)   

# wykres osypiska - słupkowy + odcięcie na poziomie wartości własnej równej 1
screeplot(dane_pca, type = "barplot", col = "lightblue", main = "")                 
abline(1,0, col = 'red', lty = 2)

fviz_eig(dane_pca, addlabels = TRUE, 
         ylim = c(0, 60),
         main="Scree Plot")

library(GGally)

row.names(danepca) <- as.matrix(wojewodztwa)
ggplot(pc.dane$scores,  aes(Comp.1, Comp.2))+
  geom_point() + # Show dots
  geom_text(
    label=rownames(danepca), 
    colour = "red",
    nudge_x = 0.2, nudge_y = 0.2, 
    check_overlap = T)  + geom_hline(yintercept=0, alpha =0.3, linetype="dashed") +
  geom_vline(xintercept=0, alpha =0.3, linetype="dashed") +
  ggtitle("Współrzędne przypadków") + labs(x = "1 składowa", y= "2 składowa")

autoplot(dane_pca, data=dane, colour = "x27", loadings = TRUE, 
         loadings.colour = "blue",
         loadings.label=TRUE, 
         loadings.label.size=4) + ggtitle("Bioplot") + geom_text(
           label=row.names(danepca))

biplot(dane_pca)

################################################################################
fviz_pca_biplot(dane_pca, 
                repel = TRUE,
                col.var = "deepskyblue",
                title = "Biplot", geom="point")

fviz_pca_biplot(dane_pca, repel = FALSE, col.var = "blue", col.ind = "black") + geom_text(
  label=row.names(danepca))

fviz_pca_var(dane_pca, repel = TRUE,
             col.var = "#69b3a2",
             col.ind = "black", title = "Biplot", 
             xlab = "1 składowa (36,9%)",
             ylab = "2 składowa (23%)")

fviz_pca_var(dane_pca, repel = TRUE,
             col.var = "#69b3a2",
             col.ind = "black", axes=c(2,3),
             col.ind = "black", title = "Biplot", 
             xlab = "2 składowa (23%)",
             ylab = "3 składowa (13%)")

# Select the top 3 contributing variables?
fviz_pca_var(dane_pca, select.var = list(contrib = 3)) 

# Color individuals by groups
fviz_pca_ind(dane_pca, label="none", habillage=grupa)

fviz_pca_ind(dane_pca, label="none", habillage=grupa, legend.title = "Wydatki") + geom_text(
  label=row.names(danepca)) +ggtitle("Współrzędne przypadków") +
  labs(x = "1 składowa", y = "2 składowa")

fviz_pca_ind(dane_pca, label="none", habillage=grupa, legend.title = "Wydatki", axes = c(2,3)) + geom_text(
  label=row.names(danepca)) +ggtitle("Współrzędne przypadków") +
  labs(x = "2 składowa", y = "3 składowa")

# Add ellipses
p <- fviz_pca_ind(dane_pca, label="none", habillage=grupa,
                  addEllipses=TRUE, ellipse.level=0.95)
print(p)

# cos2 = the quality of the individuals on the factor map
fviz_pca_var(dane_pca, col.var = "cos2", 
             gradient.cols = c("blue", "black", "red"), repel = TRUE)
#################################################
#https://f0nzie.github.io/machine_learning_compilation/detailed-study-of-principal-component-analysis.html
var <- get_pca_var(dane_pca)
head(var$coord, 4) # korelacje
corrplot(var$cos2[,1:3], is.corr=FALSE)
corrplot(var$coord[,1:3], is.corr=FALSE)

# Color by cos2 values: quality on the factor map
fviz_pca_var(dane_pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

# Udział zmiennych w rozliczaniu zmienności głównych składowych są wyrażone w procentach.
round(var$contrib[,1:3], 3)
corrplot(var$contrib[,1:3], is.corr=FALSE)  

#Wykresy jaki % wpływu mają 
# For a given dimension, any row/column with a contribution above the reference 
# line could be considered as important in contributing to the dimension.
# czerwona linia na wysokości - 1/11 (ilosc zmiennych w PCA)
# Contributions of variables to PC1
fviz_contrib(dane_pca, choice = "var", axes = 1, top = 11, fill = "#69b3a2") +
  labs(y = "Udział w składowej (%)") + ggtitle("Wkład zmiennych w 1. składową")
# Contributions of variables to PC2
fviz_contrib(dane_pca, choice = "var", axes = 2, top = 11, fill = "#69b3a2") +
  labs(y = "Udział w składowej (%)") + ggtitle("Wkład zmiennych w 2. składową")
# Contributions of variables to PC3
fviz_contrib(dane_pca, choice = "var", axes = 3, top = 11, fill = "#69b3a2") +
  labs(y = "Udział w składowej (%)") + ggtitle("Wkład zmiennych w 3. składową")


# na budowanie 2 pierwszych składowych, które zmienne mają największy wpływ
fviz_contrib(dane_pca, choice = "var", axes = c(1,2), top = 11)

fviz_pca_var(dane_pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)


#INDIVIDUALS - przypadki
row.names(dane_pca) <- as.matrix(wojewodztwa)
ind <- get_pca_ind(dane_pca)
head(ind$coord) # wspólrzedne przypadków
fviz_pca_ind(dane_pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)
# Zawsze ta sama kolejność? w każdej składowej - alfabetyczna
fviz_cos2(dane_pca, choice = "ind", label=as.matrix(wojewodztwa))
fviz_cos2(dane_pca, choice = "ind", label=as.matrix(wojewodztwa), axes=2)
fviz_cos2(dane_pca, choice = "ind", label=as.matrix(wojewodztwa), axes=3)


###########################################

pca <- prcomp(danepca, scale.=TRUE, center = TRUE)
pca$x #wsp. przypadkow
pca.data <- data.frame(pca$x)
View(pca.data)
pca.data$plotx <- pca.data[,1]
pca.data$plotx #wsp. 1 składowej dla przypadków
pca.data$ploty <- pca.data[,2]
pca.data$ploty #wsp. 2 składowej dla przypadków
wydatki = as.character(dane$x27)
View(wydatki)
pca.data$grupa <- wydatki
pca.data$wojewodztwa <- wojewodztwa
View(pca.data)


#################################################
install.packages("ellipse")
library(ellipse)

#danepca2 <- danepca
#danepca2$x27<- dane$x27
#View(danepca)
PCA(danepca, graph = FALSE)
razem <- PCA(danepca, graph = FALSE)
View(razem)
#woj_dane <- wojewodztwa
# grupa2 <- podział na 2 poziomy
grupa2 <- c("niskie",
            "wysokie",
            "wysokie",
            "niskie",
            "niskie",
            "niskie",
            "wysokie",
            "wysokie",
            "niskie",
            "niskie",
            "niskie",
            "wysokie",
            "niskie",
            "niskie",
            "wysokie",
            "niskie"
)

grupa <-  pca.data$grupa

fviz_pca_biplot(razem, geom = "point", axes = c(1,2), col.var = "black", ggtheme = theme_minimal()) + 
  geom_text_repel(data = data.frame(razem$ind$coord, wojewodztwa), aes(x = Dim.1, y = Dim.2, label = as.matrix(wojewodztwa)), 
                  size = 3, color = "red")+ stat_ellipse(geom = "polygon",
                                                         aes(fill = grupa2), 
                                                         alpha = 0.2)

fviz_pca_biplot(razem, geom = "point", axes = c(1,2), col.var = "black", ggtheme = theme_minimal(),  
                xlab = "Miara fiansowo-edukacyjna", ylab = "Miara problemów finansowych", legend.title = "Wydatki") +
  geom_text_repel(data = data.frame(razem$ind$coord, wojewodztwa), aes(x = Dim.1, y = Dim.2, label = as.matrix(wojewodztwa)), 
                  size = 3, color = "red") + stat_ellipse(geom = "polygon",
                                                          aes(fill = grupa), 
                                                          alpha = 0.2)

fviz_pca_biplot(razem, geom = "point", axes = c(2,3), col.var = "black", ggtheme = theme_minimal(),  
                xlab = "Miara problemów finansowych", ylab = "Miara opieki zdrowotnej", legend.title = "Wydatki") +
  geom_text_repel(data = data.frame(razem$ind$coord, wojewodztwa), aes(x = Dim.2, y = Dim.3, label = as.matrix(wojewodztwa)), 
                  size = 3, color = "red") + stat_ellipse(geom = "polygon",aes(fill = grupa), 
                                                          alpha = 0.2)
# top 5 contributing individuals and variable
fviz_pca_biplot(dane_pca, select.ind = list(contrib = 5), 
                select.var = list(contrib = 5),
                ggtheme = theme_minimal())
                                                      
# Convex hull(wypukły kadłub) - inny kształt
fviz_pca_ind(dane_pca, geom.ind = "point",
             col.ind = dane$x27, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "convex",
             legend.title = "Groups"
)

# 1 i 2 składowa z podziałem na grupy zm i wydatki
fviz_pca_biplot(dane_pca, 
                # Fill individuals by groups
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2.5,
                fill.ind = dane$x27,
                col.ind = "black",
                # Color variable by groups
                col.var = factor(c("zdrowie", "zdrowie", "edukacja", "edukacja",
                                   "edukacja","edukacja", "finanse", "finanse",
                                   "finanse", "finanse","finanse")),
                
                legend.title = list(fill = "Wydatki", color = "Grupa"),
                repel = TRUE        # Avoid label overplotting
)+
  ggpubr::fill_palette("npg")+      # Indiviual fill color
  ggpubr::color_palette("dose")+      # Variable colors
  labs(x="1. składowa (36.9%)", y = "2. składowa (23%)") +
  ggtitle("Biplot z podziałem zmiennych na grupy")

# 2 i 3 składowa z podziałem na wydatki i grupy zm
fviz_pca_biplot(dane_pca, 
                # Fill individuals by groups
                geom.ind = "point",
                axes=c(2:3),
                pointshape = 21,
                pointsize = 2.5,
                fill.ind = dane$x27,
                col.ind = "black",
                # Color variable by groups
                col.var = factor(c("zdrowie", "zdrowie", "edukacja", "edukacja",
                                   "edukacja","edukacja", "finanse", "finanse",
                                   "finanse", "finanse","finanse")),
                
                legend.title = list(fill = "Wydatki", color = "Grupa"),
                repel = TRUE        # Avoid label overplotting
)+
  ggpubr::fill_palette("npg")+      # Indiviual fill color
  ggpubr::color_palette("dose")+      # Variable colors
  labs(x="2. składowa (23%)", y = "3. składowa (13%)") +
  ggtitle("Biplot z podziałem zmiennych na grupy")
################################################################################
