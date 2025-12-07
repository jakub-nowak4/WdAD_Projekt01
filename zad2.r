library(MASS)

set.seed(123456789)

cena = Cars93$Price
moc = Cars93$Horsepower
spalanie = Cars93$MPG.city
n = length(cena)

#Analiza dla cena samochodu

par(mfrow=c(1,2))
hist(cena, main="Histogram - Cena", col="skyblue", xlab="Cena")
qqnorm(cena, main="Q-Q Plot - Cena")
qqline(cena, col="red", lwd=2)
par(mfrow=c(1,1))


#Metoda Bootstrap

boot_wyniki = replicate(10000, {
  probka = sample(cena, size=n, replace=TRUE)
  c(mean(probka), sd(probka), var(probka))
})


pu_srednia = quantile(boot_wyniki[1, ], c(0.025, 0.975))
pu_sd = quantile(boot_wyniki[2, ], c(0.025, 0.975))
pu_var = quantile(boot_wyniki[3, ], c(0.025, 0.975))

# Wyświetlanie wyników
cat("\nWyniki dla analizy ceny samochodu\n")
cat("Metoda Bootstrap (95%)\n")
cat("Przedział dla ŚREDNIEJ:  ", round(pu_srednia[1], 2), " - ", round(pu_srednia[2], 2),"\n")
cat("Przedział dla SD:        ", round(pu_sd[1], 2), " - ", round(pu_sd[2], 2),"\n")
cat("Przedział dla WARIANCJI: ", round(pu_var[1], 2), " - ", round(pu_var[2], 2),"\n")


#Analiza dla moc samochodu

n = length(moc)


par(mfrow=c(1,2))
hist(moc, main="Histogram - Moc", col="skyblue", xlab="Moc")
qqnorm(moc, main="Q-Q Plot - Moc")
qqline(moc, col="red", lwd=2)
par(mfrow=c(1,1))

#Metoda Bootstrap

boot_wyniki_moc = replicate(10000, {
  probka = sample(moc, size=n, replace=TRUE)
  c(mean(probka), sd(probka), var(probka))
})

pu_srednia_moc = quantile(boot_wyniki_moc[1, ], c(0.025, 0.975))
pu_sd_moc = quantile(boot_wyniki_moc[2, ], c(0.025, 0.975))
pu_var_moc = quantile(boot_wyniki_moc[3, ], c(0.025, 0.975))


# Wyświetlanie wyników
cat("\nWyniki dla analizy moc samochodu\n")
cat("Metoda Bootstrap (95%)\n")
cat("Przedział dla ŚREDNIEJ:  ", round(pu_srednia_moc[1], 2), " - ", round(pu_srednia_moc[2], 2),"\n")
cat("Przedział dla SD:        ", round(pu_sd_moc[1], 2), " - ", round(pu_sd_moc[2], 2),"\n")
cat("Przedział dla WARIANCJI: ", round(pu_var_moc[1], 2), " - ", round(pu_var_moc[2], 2),"\n")


#Analiza dla spalanie samochodu

n = length(spalanie)

par(mfrow=c(1,2))
hist(spalanie, main="Histogram - Spalanie (MPG)", col="orange", xlab="MPG (Mile na galon)")
qqnorm(spalanie, main="Q-Q Plot - Spalanie")
qqline(spalanie, col="red", lwd=2)
par(mfrow=c(1,1))


#Metoda Bootstrap
boot_wyniki_spalanie = replicate(10000, {
  probka = sample(spalanie, size=n, replace=TRUE) 
  c(mean(probka), sd(probka), var(probka))
})


pu_srednia_spal = quantile(boot_wyniki_spalanie[1, ], c(0.025, 0.975))
pu_sd_spal = quantile(boot_wyniki_spalanie[2, ], c(0.025, 0.975))
pu_var_spal = quantile(boot_wyniki_spalanie[3, ], c(0.025, 0.975))



# Wyświetlanie wyników
cat("\nWyniki dla analizy spalania\n")
cat("Metoda Bootstrap (95%)\n")
cat("Przedział dla ŚREDNIEJ:  ", round(pu_srednia_spal[1], 2), " - ", round(pu_srednia_spal[2], 2),"\n")
cat("Przedział dla SD:        ", round(pu_sd_spal[1], 2),      " - ", round(pu_sd_spal[2], 2),"\n")
cat("Przedział dla WARIANCJI: ", round(pu_var_spal[1], 2),     " - ", round(pu_var_spal[2], 2),"\n")