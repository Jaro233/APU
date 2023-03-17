a <- 14 
a <- 14 * cos(3.14)
b <- a * 2
min(a, b)
a <- 900:1150
help(log)
getwd()
setwd("C:/Users/jaro9/OneDrive/Desktop/apu")
getwd()
a <- "lodówka turystyczna"
save(a, file = "workspace.RData")
rm(a)
a
load("workspace.RData")
a
install.packages("gridExtra")
x1<-seq(1400,1500,by=5)
a <- 25:5
b <- 50:70
d <- b + a
d
nazwa <- c("HB", "Curver", "Ravanson", "QVANT", "Yeticool", "G21", "Camry", "Dometic", "Sencor", 
           "Aisberg", "Mobicool", "Polarbox", "Fresh", "Newestcool", "Fantastic")
waga <- c(4.3, 4.4, 4.5, 4.6, 4.7, 4.7, 4.2, 4.3, 4.4, 4.9, 4.9, 4.7, 4.7, 4.8, 5)
cena <- c(300, 800, 1500, 2000, 3000, 450, 600, 900, 1200, 1300, 
          100, 150, 200, 300, 500)
liczba_opinii <- c(0, 40, 80, 120, 130, 10, 70, 80, 40, 20, 90, 150, 25, 55, 5)
df <- data.frame(nazwa, waga, cena, liczba_opinii)
mean(df[[3]])
df[nrow(df) + 1,] <- list("Polarbox2.0", 5.5, 550, 15)
mean(df[[3]])
ocena_klientow1 <- seq(0, 5, by=0.5) 
ocena_klientow2 <- seq(0, 2, by=0.5)
ocena_klientow <- append(ocena_klientow1, ocena_klientow2)
df["ocena_klientow"] = ocena_klientow
lodowki <- df
lodowki[nrow(lodowki) + 1,] <- list("CV60", 5, 2000, 12, 2.5)
lodowki[nrow(lodowki) + 1,] <- list("CV70", 4.5, 2400, 11, 3)
lodowki[nrow(lodowki) + 1,] <- list("CV80", 2, 1000, 5, 3.5)
lodowki[nrow(lodowki) + 1,] <- list("CV90", 1, 800, 2, 4)
count <- table(lodowki$ocena_klientow)
barplot(count,
        main = "liczebnosc reprezentantow kazdej z ocen klientow",
        ylim = c(0, 5),
        xlab = "Ocena klienta",
        ylab = "Ilosc")
percentage <- table(lodowki$ocena_klientow) /
  length(lodowki$ocena_klientow)
pie(percentage)

percentage2 <- table(lodowki$ocena_klientow) /
  length(lodowki$ocena_klientow)
fan.plot(percentage2, labels = names(percentage2))

lodowki$status_opinii <- with(lodowki, ifelse(liczba_opinii > 100, 'wiecej niz 100 opinii', 
                              ifelse(liczba_opinii > 50, '50-100 opinii',
                              ifelse(liczba_opinii == 0, 'nie ma', 'mniej niz 50 opinii'))))


for(i in 1:nrow(lodowki)) {
  cat(lodowki[i, 1], "ma ocenę klientów", lodowki[i, 5], "bo ma liczbę opinii", lodowki[i, 4], '\n')
}

write.csv(lodowki, "C:/Users/jaro9/OneDrive/Desktop/apu/lodowki.csv", row.names=FALSE)

read.csv("C:/Users/jaro9/OneDrive/Desktop/apu/lodowki.csv")
