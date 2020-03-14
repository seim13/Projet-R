# project in applied statistic
# groupe: Gilles Robin and Juliette Seimandi
# nom de code: Silence ça pousse


data(iris)
iris
plot(iris, type="l")
str(iris)

#bar plot
x=iris$Species #agument
y=iris$Petal.Width
mode(x)
cc<-c("#FF3333","chartreuse3","gold") #color use for each species
plot(x,y,col=cc) #draw graph

x=iris$Species
y=iris$Sepal.Length
mode(x)
cc<-c("#FF3333","chartreuse3","gold") #color use for each species
plot(x,y,col=cc) #draw graph

x=iris$Species
y=iris$Sepal.Width
mode(x)
cc<-c("#FF3333","chartreuse3","gold") #color use for each species
plot(x,y,col=cc) #draw graph

x=iris$Species
y=iris$Petal.Length 
cc<-c("#FF3333","chartreuse3","gold") #color use for each species
plot(x,y,col=cc) #draw graph


#stock values by species
setosa_PW<-iris$Petal.Width[0:50] 
versi_PW<-iris$Petal.Width[51:100]
virg_PW<-iris$Petal.Width[101:150]
setosa_LG<-iris$Petal.Length[0:50] 
versi_LG<-iris$Petal.Length[51:100]
virg_LG<-iris$Petal.Length[101:150]
setosa_SW<-iris$Sepal.Width[0:50]
versi_SW<-iris$Sepal.Width[51:100]
virg_SW<-iris$Sepal.Width[101:150]
setosa_SL<-iris$Sepal.Length[0:50] 
versi_SL<-iris$Sepal.Length[51:100]
virg_SL<-iris$Sepal.Length[101:150]

#mean of each columns by species                  
spw<-mean(setosa_PW)
vepw<-mean(versi_PW)
vgpw<-mean(virg_PW)
slg<-mean(setosa_LG)
velg<-mean(versi_LG)
vglg<-mean(virg_LG)
ssw<-mean(setosa_SW)
vesw<-mean(versi_SW)
vgsw<-mean(virg_SW)
ssl<-mean(setosa_SL)
vesl<-mean(versi_SL)
vgsl<-mean(virg_SL)

#histogram

hist(tot,xlab="graph for each single numeric variable") #draw histogram with the fonction hist()
sep=iris$Sepal.Length+iris$Sepal.Width
pet=iris$Petal.Length+iris$Petal.Width
tot=sep+pet #total of each numerical values without species' column

#classical draw of each histogram
hist(iris$Sepal.Length,main="Repartition of numeric value of sepal length",xlab="sepal length")
hist(iris$Sepal.Width,main="Repartition of numeric value of sepal width",xlab="sepal width")
hist(iris$Petal.Length,main="Repartition of numeric value of petal length",xlab="petal length")
hist(iris$Petal.Width,main="Repartition of numeric value of petal width",xlab="petal width")

#histogram representation using ggplot
ggplot(iris, aes(iris$Sepal.Width)) +geom_bar() +theme(axis.text.x = element_text(angle=70, vjust=0.5))
ggplot(iris, aes(iris$Sepal.Length)) +geom_bar() +theme(axis.text.x = element_text(angle=70, vjust=0.5))
ggplot(iris, aes(iris$Petal.Length)) +geom_bar() +theme(axis.text.x = element_text(angle=70, vjust=0.5))


#3D
install.packages("scatterplot3d")
library("scatterplot3d")

X<-setosa_PW #definition of arguments
Y<-versi_PW
Z<-virg_PW
shapes = c(16, 17, 18) 
shapes <- shapes[as.numeric(iris$Species)]

colors <- c("#999999", "#E69F00", "#56B4E9")
colors <- colors[as.numeric(iris$Species)]
s3d<-scatterplot3d(iris[,1:3], pch = 16, color=colors,angle=55) # same but with colors by species
legend("right", legend = levels(iris$Species), col =  c("#999999", "#E69F00", "#56B4E9"), pch = 16)

#regression lineaire
par(mfrow = c(3,1)) # print the 3 graph on the same page
plot (setosa_PW~setosa_LG) # draw classic graph 
iris.lm<-lm(setosa_PW~setosa_LG,data=iris) # calcule coef
print(iris.lm) # print coef
abline(iris.lm) # add the linear regression line on the graph

plot(virg_PW~virg_LG)
virg.lm<-lm(virg_PW~virg_LG,data=iris)
print(virg.lm)
abline(virg.lm)

plot(versi_PW~versi_LG)
versi.lm<-lm(versi_PW~versi_LG, data=iris)
print(versi.lm)
abline(versi.lm)


#smooth lineaire
install.packages("gridExtra")
library("gridExtra")

install.packages("cowplot")
library("cowplot")

install.packages("ggplot2")
library("ggplot2")


#regression lineaire representation using ggplot
ggplot(iris, aes(x=iris$Petal.Length, y=iris$Sepal.Length)) + geom_point()+geom_smooth(method=lm) #draw linear regression model with smooth opption that made the model more adapt to our representation

#create a new data frame with a new row of a iris to identify

de<-data.frame(5.0,2.8,3.8,1.1,"unknown")
names(de)<-c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")
de
newdf <- rbind(iris, de)
newdf
newdf[51:100,4:5]

install.packages("scatterplot3d")
library("scatterplot3d")

colors <- c("#999999", "#E69F00", "#56B4E9","#e62c00")
colors <- colors[as.numeric(newdf$Species)]
s3d<-scatterplot3d(newdf[,1:3], pch = 16, color=colors,angle=55)
legend("right", legend = levels(newdf$Species), col =  c("#999999", "#E69F00", "#56B4E9","#e62c00"), pch = 16)

# we can identified the point i had on the graph and make a supposition of its species 
# use the non use data Petal.Width to confirme our supposition 
# with the bar plot draw before we saw that its a revelant criteria to identifiy a species 
# see the interval for species of Petal.Width with histogram

library(ggplot2)
qplot(Petal.Width,data = newdf) +
  facet_wrap(~ Species)
#we can clearly see that our unknow species belong to the versicolor's Petal.Width interval
# that confim our suposition
