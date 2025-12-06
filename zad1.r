zarobki = c(45617, 7166, 18594, 2236, 1278, 19828, 4033, 28151 , 2414, 3800)

n = length(zarobki)
srednia = mean(zarobki)


cat("Średnia zarobków:", srednia, "\n")

#Podpunkt a)

#Stosujemy rozklad Z

sigma = 15000 
blad_std_znany = sigma / sqrt(n)

#Przedzial ufnosci 90%
z_90 = qnorm(0.95)
dolna_90a = srednia  - z_90 * blad_std_znany
gorna_90a = srednia  + z_90 * blad_std_znany


#Przedziały ufności 95%
z_95 = qnorm(0.975)
dolna_95a = srednia - z_95 * blad_std_znany
gorna_95a = srednia + z_95 * blad_std_znany

cat("\nWyniki dla a)\n")
cat("Rozklad Z\n")
cat("Przedział ufności 90%:", round(dolna_90a, 2), " - ", round(gorna_90a, 2), "\n")
cat("Przedział ufności 95%:", round(dolna_95a, 2), " - ", round(gorna_95a, 2), "\n")


#Podpunkt b)

#Stosujemy rozklad t-Studenta -> mala probka n<30

s = sd(zarobki)
blad_std_nieznany = s / sqrt(n)
df = n - 1

#Przedzial ufnosci 90%
t_90 = qt(0.95,df)
dolna_90b = srednia - t_90 * blad_std_nieznany
gorna_90b = srednia + t_90 * blad_std_nieznany

#Przedzial ufnosci 95%
t_95 = qt(0.975,df)
dolna_95b = srednia - t_95 * blad_std_nieznany
gorna_95b = srednia + t_95 * blad_std_nieznany

cat("\nWyniki dla b)\n")
cat("Rozklad t-Student\n")
cat("Odchylenie standardowe z probki: ",round(s,2),"\n")
cat("Przedział ufności 90%:", round(dolna_90b, 2), " - ", round(gorna_90b, 2), "\n")
cat("Przedział ufności 95%:", round(dolna_95b, 2), " - ", round(gorna_95b, 2), "\n")

#Podpunkt c)

#Metoda bootstrap

par(mfrow = c(1, 2)) 
hist(zarobki, main = "Histogram zarobków", col = "lightblue")
qqnorm(zarobki, main = "Wykres Q-Q")
qqline(zarobki, col = "red")
par(mfrow = c(1, 1))


set.seed(123456789)
l_powtorzen = 10000

b_srednie = replicate(l_powtorzen,
{
    probka = sample(zarobki,size = n,replace = TRUE) #replace TRUE realizuje zwracanie do puli
    mean(probka)
})

b_90 = quantile(b_srednie, probs = c(0.05,0.95))
b_95 = quantile(b_srednie, probs = c(0.025,0.975))

cat("\nWyniki dla c)\n")
cat("Metoda Bootstrap\n")
cat("Przedział 90%:", round(b_90[1], 2), " - ", round(b_90[2], 2), "\n")
cat("Przedział 95%:", round(b_95[1], 2), " - ", round(b_95[2], 2), "\n")