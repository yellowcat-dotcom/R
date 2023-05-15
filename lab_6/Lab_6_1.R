table.01 <- read.table("eda.csv", header = TRUE,";", dec=',', fileEncoding="cp1251");table.01
table.02 <- scale(table.01[,3:9], center = TRUE, scale = TRUE);table.02
table.02<-table.01[,-1];table.02
table.02<-table.02[,-1];table.02
df3<-table.01; df3
maxs <- apply(table.02, 2, max);maxs
mins <- apply(table.02, 2, min);mins

table.02 <- scale(table.02, center = mins, scale = maxs - mins); table.02

n_<-table.01$n_; n_
type_<-table.01$type_; type_
table.02<-data.frame(n_, type_, table.02);table.02

dist.table <- dist(table.02 [,3:9]); dist.table


clust.city <- hclust(dist.table); clust.city
plot(clust.city, labels = table.01$type_,main="Дендрограмма",ylab="Cходство",xlab="fdd")

k = 4
rect.hclust(clust.city, k = 4, border="red")
abline(h = 1.5, col = "blue", lwd='3') # h - horizontal line, col - color

plot(clust.city$height, type='b',xlab="Номер компоненты",ylab = "Собственное значение")

#  Разделим Страны на 4 кластера
#  Вектор groups содержит номер кластера, в который попал классифицируемый объект 
groups <- cutree(clust.city, k) ; groups

#dend <- as.dendrogram(clust.city); dend
#dend <- color_branches(dend, k) 
#plot(dend)

table.01[groups==1, 2]
table.01[groups==2, 2]
table.01[groups==3, 2]
table.01[groups==4, 2]

g1<-colMeans(table.01[groups==1, 3:9]);g1
g2<-colMeans(table.01[groups==2, 3:9]);g2
g3<-colMeans(table.01[groups==3, 3:9]);g3
g4<-colMeans(table.01[groups==4, 3:9]);g4


g11<-colMeans(table.02[groups==1, 3:9]);g11
g12<-colMeans(table.02[groups==2, 3:9]);g12
g13<-colMeans(table.02[groups==3, 3:9]);g13
g14<-colMeans(table.02[groups==4, 3:9]);g14

df<-data.frame(g1,g2,g3,g4); df
df2<-data.frame(g11,g12,g13,g14);df2
df1<-t(df2); df1

barplot(as.matrix(df2), col=c("magenta","red", "grey", "yellow","blue","green","orange")) 
legend("topleft",cex=0.6, rownames(df2),fill=c("magenta","red", "grey","yellow","blue","green","orange") )

barplot(df1[,1], ylim=range(pretty(c(0,max(df1[,1])))), 
        main="Хлеб", 
        col=c("red","yellow","blue","green"))

barplot(df1[,2], ylim=range(pretty(c(0,max(df1[,2])))), 
        main="Овощи", 
        col=c("red","yellow","blue","green"))

barplot(df1[,3], ylim=range(pretty(c(0,max(df1[,3])))), 
        main="Фрукты", 
        col=c("red","yellow","blue","green"))

barplot(df1[,4], ylim=range(pretty(c(0,max(df1[,4])))), 
        main="Мясо", 
        col=c("red","yellow","blue","green"))

barplot(df1[,5], ylim=range(pretty(c(0,max(df1[,5])))), 
        main="Птица", 
        col=c("red","yellow","blue","green"))

barplot(df1[,6], ylim=range(pretty(c(0,max(df1[,6])))), 
        main="Молоко", 
        col=c("red","yellow","blue","green"))

barplot(df1[,7], ylim=range(pretty(c(0,max(df1[,7])))), 
        main="Вино", 
        col=c("red","yellow","blue","green"))

df3["Group"]<-groups;df3
library(lattice)
xyplot(fruits ~ vegetables,group = Group, data = df3,auto.key = TRUE,pch = 20,cex = 1.5)
boxplot(bread~Group , data =df3, ylab = "Хлеб", frame = FALSE, col = rainbow(4))
boxplot(meat~Group , data =df3, ylab = "Мясо", frame = FALSE, col = rainbow(4))
boxplot(vegetables~Group , data =df3, ylab = "Мясо", frame = FALSE, col = rainbow(4))

xyplot(wine~meat+vegetables|Group,data=df3, grid = T, auto.key=TRUE,pch = 20,cex = 1.5)
cloud(bread~meat*vegetables, group = Group, data = df3, auto.key = TRUE,pch = 20,cex = 1.5) 

packages <- c('ggplot2', 'dplyr', 'tidyr', 'tibble')
install.packages(packages)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
df3 %>%
  ggplot(aes(vegetables, fruits, color=Group))+geom_point()

