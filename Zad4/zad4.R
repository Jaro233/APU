library("C50")
setwd("C:/Users/jaro9/OneDrive/Desktop/apu/Zad4")
dane <- read.csv('lodowki.csv')
dane$ocena_klientow <- factor(dane$ocena_klientow)
head(dane)
treeModel <- C5.0(x=dane[,2:5], y=dane$ocena_klientow)
treeModel
summary(treeModel)
plot(treeModel)
