## Loi des grands Nombres
# 1) Loi Unfiorme
x <- runif(1000, min=2, max=4)  # Génère 1000 nombres aléatoires uniformes entre 0 et 1
moyennes_cumulees <- cumsum(x) / seq_along(x)  # Calcul des moyennes cumulées 
plot(seq_along(x), moyennes_cumulees, type='l', col='blue',
     main='Loi des Grands Nombres',
     xlab='Nombre d\'échantillons', ylab='Moyenne Cumulée')
abline(h=mean(x), col='red', lty=4)  # Ligne de la moyenne théorique
legend('topright', legend=c('Moyenne Cumulée', 'Moyenne Théorique'),
       col=c('blue', 'red'), lty=c(1,4))

# 2) Loi de Bernoulli
set.seed(123)  # Pour la reproductibilité
n <- 1000  # Nombre d'essais
p <- 0.3   # Probabilité de succès
bernoulli_samples <- rbinom(n, size=1, prob=p)  # Génère des échantillons de Bernoulli
moyennes_cumulees_bernoulli <- cumsum(bernoulli_samples) / seq_along(bernoulli_samples)  # Calcul des moyennes cumulées
plot(seq_along(bernoulli_samples), moyennes_cumulees_bernoulli, type='l', col='blue',
     main='Loi des Grands Nombres - Bernoulli',
     xlab='Nombre d\'échantillons', ylab='Moyenne Cumulée')
abline(h=p, col='red', lty=4)  # Ligne de la moyenne théorique
legend('topright', legend=c('Moyenne Cumulée', 'Moyenne Théorique'),
       col=c('blue', 'red'), lty=c(1,4))

# 3) Loi binomiale
set.seed(123)  # Pour la reproductibilité
n <- 1000  # Nombre d'essais
size <- 10  # Taille de chaque essai
prob <- 0.4  # Probabilité de succès
binomial_samples <- rbinom(n, size=size, prob=prob)  # Génère des échantillons binomiaux
moyennes_cumulees_binomiale <- cumsum(binomial_samples) / seq_along(binomial_samples)  # Calcul des moyennes cumulées
plot(seq_along(binomial_samples), moyennes_cumulees_binomiale, type='l', col='blue',
     main='Loi des Grands Nombres - Binomiale',
     xlab='Nombre d\'échantillons', ylab='Moyenne Cumulée')
abline(h=size*prob, col='red', lty=4)  # Ligne de la moyenne théorique
legend('topright', legend=c('Moyenne Cumulée', 'Moyenne Théorique'),
       col=c('blue', 'red'), lty=c(1,4))

# 3) loi normale
set.seed(123)  #Pour la reproductibilité
n <- 1000  # Nombre d'essais
mean_norm <- 5  # Moyenne de la loi normale
sd_norm <- 2    # Écart-type de la loi normale
normal_samples <- rnorm(n, mean=mean_norm, sd=sd_norm)  # Génère des échantillons normaux
moyennes_cumulees_normale <- cumsum(normal_samples) / seq_along(normal_samples)  # Calcul des moyennes cumulées 
plot(seq_along(normal_samples), moyennes_cumulees_normale, type='l', col='blue',
     main='Loi des Grands Nombres - Normale',
     xlab='Nombre d\'échantillons', ylab='Moyenne Cumulée')
abline(h=mean_norm, col='red', lty=4)  # Ligne de la moyenne théorique
legend('topright', legend=c('Moyenne Cumulée', 'Moyenne Théorique'),
       col=c('blue', 'red'), lty=c(1,4))


## théorème Centrale Limite
# 1) Loi Uniforme
set.seed(123)  # Pour la reproductibilité
n <- 50000  # Nombre d'échantillons
m <- 30    # Taille de chaque échantillon
uniform_sample_means <- replicate(n, mean(runif(m, min=2, max=4)))  # Moyennes d'échantillons
Y <- sqrt(n)* (uniform_sample_means - 3)/sqrt(1/3)  # Normalisation des moyennes d'échantillons
hist(Y, breaks=30, col='blue', 
     main='Théorème Central Limite - Loi Uniforme',
     xlab='Moyennes d\'échantillons', ylab='Fréquence', prob=TRUE)
abline(v=mean(Y), col='red', lty=2, lwd=4)  # Moyenne des moyennes d'échantillons
lines(density(Y), col='green', lwd=4)  # Densité estimée
legend('topright', legend=c('moyenne','Histogramme ','densité'),
       col=c('red', 'blue','green'), lty=2)

# 2) Loi de Binomiale
set.seed(123)  # Pour la reproductibilité
n <- 5000  # Nombre d'échantillons
m <- 30     # Taille de chaque échantillon
size <- 10  # Taille de chaque essai binomial
prob <- 0.4  # Probabilité de succès
binomial_sample_means <- replicate(n, mean(rbinom(m, size=size, prob=prob)))  # Moyennes d'échantillons
Y <- sqrt(n)* (binomial_sample_means - size*prob)/sqrt(size*prob*(1-prob))  # Normalisation des moyennes d'échantillons
hist(Y, breaks=30, col='lightblue', 
     main='Théorème Central Limite - Loi Binomiale',
     xlab='Moyennes d\'échantillons normalisées', ylab='Fréquence', prob=TRUE)
abline(v=mean(Y), col='red', lty=2, lwd=4)  # Moyenne des moyennes d'échantillons normalisées
abline(v=0, col='blue', lty=2)  # Moyenne théorique normalisée
lines(density(Y), col='darkgreen', lwd=4)  # Densité estimée
legend('topright', legend=c('Moyenne des Moyennes Normalisées', 'Moyenne Théorique Normalisée','Densité'),
       col=c('red', 'blue','darkgreen'), lty=2)

### Estimation Ponctuelle
# 1) La Moyenne
set.seed(123)  # Pour la reproductibilité
n <- 1000  # Taille de l'échantillon
sample_data <- rnorm(n, mean=50, sd=10)  # Génère un échantillon aléatoire
sample_mean <- mean(sample_data)  # Calcul de la moyenne de l'échantillon
sample_mean  # Affichage de la moyenne de l'échantillon
# 2) La Variance
set.seed(123)  # Pour la reproductibilité
n <- 1000  # Taille de l'échantillon
sample_data <- rnorm(n, mean=50, sd=10)  # Génère un échantillon aléatoire
sample_variance <- var(sample_data)  # Calcul de la variance de l'échantillon
sample_variance  # Affichage de la variance de l'échantillon
# 3) La Proportion
set.seed(123)
n <- 1000  # Taille de l'échantillon
p <- 0.3   # Probabilité de succès
bernoulli_sample <- rbinom(n, size=1, prob=p)  # Génère un échantillon de Bernoulli
sample_proportion <- mean(bernoulli_sample)  # Calcul de la proportion de succès
sample_proportion  # Affichage de la proportion de succès

### Estimation par Intervalle de Confiance
# 1) Intervalle de Confiance pour la Moyenne
set.seed(123)  # Pour la reproductibilité
n <- 30  # Taille de l'échantillon
sample_data <- rnorm(n, mean=100, sd=15)  # Génère un échantillon aléatoire
sample_mean <- mean(sample_data)  # Calcul de la moyenne de l'échantillon
sample_sd <- sd(sample_data)  # Calcul de l'écart-type de l'échantillon
alpha <- 0.05  # Niveau de confiance
error_margin <- qt(1 - alpha/2, df=n-1) * (sample_sd / sqrt(n))  # Marge d'erreur
ci_lower <- sample_mean - error_margin  # Borne inférieure
ci_upper <- sample_mean + error_margin  # Borne supérieure
ci_lower  # Affichage de la borne inférieure
ci_upper  # Affichage de la borne supérieure
t.test(sample_data, conf.level=0.95)  # Vérification avec t.test

# 2) Intervalle de Confiance pour la Proportion
set.seed(123)  # Pour la reproductibilité
n <- 200  # Taille de l'échantillon
p <- 0.4  # Probabilité de succès
bernoulli_sample <- rbinom(n, size=1, prob=p)  # Génère un échantillon de Bernoulli
sample_proportion <- mean(bernoulli_sample)  # Calcul de la proportion de succès
alpha <- 0.05  # Niveau de confiance
error_margin <- qnorm(1 - alpha/2) * sqrt((sample_proportion * (1 - sample_proportion)) / n)  # Marge d'erreur  
ci_lower <- sample_proportion - error_margin  # Borne inférieure
ci_upper <- sample_proportion + error_margin  # Borne supérieure
ci_lower  # Affichage de la borne inférieure
ci_upper  # Affichage de la borne supérieure
prop.test(sum(bernoulli_sample), n, conf.level=0.95)  # Vérification avec prop.test

library(psych)
library(DescTools)
# 3) Intervalle de Confiance pour la  Variance
set.seed(123)  # Pour la reproductibilité
n <- 30  # Taille de l'échantillon
sample_data <- rnorm(n, mean=0, sd=5)  # Génère un échantillon aléatoire
sample_variance <- var(sample_data)  # Calcul de la variance de l'échantillon
alpha <- 0.05  # Niveau de confiance
ci_lower <- (n - 1) * sample_variance / qchisq(1 - alpha/2, df=n-1)  # Borne inférieure
ci_upper <- (n - 1) * sample_variance / qchisq(alpha/2, df=n-1)  # Borne supérieure
ci_lower  # Affichage de la borne inférieure
ci_upper  # Affichage de la borne supérieure

s2 <- var(sample_data)  # Variance de l'échantillon
alpha <- 0.05  # Niveau de confiance
# Degrés de liberté
df <- n - 1

# Quantiles du chi-carré
chi2_lower <- qchisq(alpha/2, df, lower.tail = FALSE)
chi2_upper <- qchisq(alpha/2, df)

# Intervalle de confiance pour la variance
IC_inf <- (df * s2) / chi2_lower
IC_sup <- (df * s2) / chi2_upper

cat("Intervalle de confiance pour la variance : [", IC_inf, ", ", IC_sup, "]\n")
 
VarCI(sample_data, conf.level=0.95)  # Vérification avec VarCI de psych

#####
Taux <- c(3, 1.8, 2.5, 2.1, 2.7, 1.9, 1.5, 1.7, 2, 1.6)
length(Taux)
t.test(Taux, level=0.95)

## Par bootstrap : La moyenne
library(boot)
moyenne <- function(x, indices) mean(x[indices])
taux.boot <- boot(Taux, moyenne, R=999, stype="i", sim="ordinary")
boot.ci(taux.boot, conf=0.95, type=c("norm","basic", "perc", "bca"))

# Par bootstrap la variance
variance <- function(x, indice) var(x[indice])
taux.boot <- boot(Taux, variance, R=999, stype ="i", sim='ordinary')
boot.ci(taux.boot, conf=0.95, type=c("norm", "basic", "perc","bca"))
m1 <- qbinom(0.025, length(Taux), 0.5)
m2 <- qbinom(0.975, length(Taux), 0.5)
median.ic <- (c(sort(Taux)[m1], sort(Taux)[m2+1]))
median.ic
wilcox.test(Taux)$conf
sort(Taux)
