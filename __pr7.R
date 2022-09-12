# Pod uwage wzieto trzy indeksy gieldowe (swiatowe): francuski (CAC 40), polski (WIG 30) oraz amerykanski (S&P 500).
# Dane dzienne dotyczace okresu 1.01.2006-31.12.2021.



cac <- read.csv("C:/Users/katar/OneDrive/Pulpit/IMRR_PROJEKT1/cac_d.csv", row.names=1)
spx <- read.csv("C:/Users/katar/OneDrive/Pulpit/IMRR_PROJEKT1/spx_d.csv", row.names=1)
wig30 <- read.csv("C:/Users/katar/OneDrive/Pulpit/IMRR_PROJEKT1/wig30_d.csv", row.names=1)



## Statystyki opisowe

# CAC
summary(cac)

# S&P
summary(spx)

# WIG30
summary(wig30)


# Sporzadzono wykresy pudelkowe dla najwyzszych i najnizszych zanotowanych w danym dniu wartosci indeksow

# Najwyzsza wartosc 

par(mfrow=c(1,3))
boxplot(cac$Najwyzszy, main="CAC")
boxplot(spx$Najwyzszy, main="S&P")
boxplot(wig30$Najwyzszy, main="WIG30")



# Najnizsza wartosc

par(mfrow=c(1,3))
boxplot(cac$Najnizszy, main="CAC")
boxplot(spx$Najnizszy, main="S&P")
boxplot(wig30$Najnizszy, main="WIG30")



## Stopy zwrotu (proste)

cac$stopa_zwrotu <- NA
cac$stopa_zwrotu <- (cac$Zamkniecie-cac$Otwarcie)/cac$Otwarcie


spx$stopa_zwrotu <- NA
spx$stopa_zwrotu <- (spx$Zamkniecie-spx$Otwarcie)/spx$Otwarcie


wig30$stopa_zwrotu <- NA
wig30$stopa_zwrotu <- (wig30$Zamkniecie-wig30$Otwarcie)/wig30$Otwarcie

# Wykresy Stop
# CAC 40
plot(cac$stopa_zwrotu,type = "l")
# S&P 500
plot(spx$stopa_zwrotu,type = "l")
# WIG30
plot(wig30$stopa_zwrotu,type = "l")

#--------------------------------------------------------------------------------------

### CAC


## Metoda Historyczna dla 99%

library(dplyr)
t = 250
cac$met_historyczna_var99_250obs <- NA
cac$met_historyczna_es99_250obs <- NA
iloscWierszy_cac <- nrow(cac)
max_cac = iloscWierszy_cac-t

for (i in c(1:max_cac)){
  #VaR
  fragmentDanych_cac <- cac[i:(t+i-1),]
  kw_99_H_cac = quantile(fragmentDanych_cac$stopa_zwrotu, 0.99)
  cac$met_historyczna_var99_250obs[t+i] <- kw_99_H_cac
  
  #ES
  fragmentDanych2_cac <- fragmentDanych_cac %>% filter(stopa_zwrotu > kw_99_H_cac)
  es_99_H_cac = mean(fragmentDanych2_cac$stopa_zwrotu)
  cac$met_historyczna_es99_250obs[t+i] <- es_99_H_cac
}

plot(cac$met_historyczna_var99_250obs, type = "l", ylab = "VaR 99%", main = "Jak zmienialo sie VaR w czasie - metoda historyczna")
plot(cac$met_historyczna_es99_250obs,type = "l", ylab = "ES 99%", main = "Jak zmienialo sie ES w czasie - metoda historyczna")


## Metoda Historyczna z Wagami dla 99%

library(dplyr)
t = 250
cac$met_historycznaWAGI_var99_250obs <- NA
cac$met_historycznaWAGI_es99_250obs <- NA
iloscWierszy_cac <- nrow(cac)
max_cac = iloscWierszy_cac-t
q <- 0.995

for (i in c(1:max_cac)){
  fragmentDanych_cac <- cac[i:(t+i-1),]
  fragmentDanych_cac$prawdopodobienstwa <- NA
  
  for (j in c(1:nrow(fragmentDanych_cac))) {
    wykladnik <- t - j
    licznik <- q**wykladnik*(1-q)
    mianownik <- 1-q**t
    wartosci <- licznik/mianownik 
    fragmentDanych_cac$prawdopodobienstwa[j] <- wartosci
    }
    #sortowanie stopy zwrotu (do dystrybuanty)
    posortowaneDane <- fragmentDanych_cac[order(fragmentDanych_cac$stopa_zwrotu),]
    
 
    #dystrybuanta
    posortowaneDane$dystrybuanta <- NA
    for (j in c(1:nrow(posortowaneDane))) {
      doDystrybuanty <- sum(posortowaneDane[1:j,"prawdopodobienstwa"])
      posortowaneDane$dystrybuanta[j] <- doDystrybuanty
    }
    
    # VaR 99% - kwantyl brany z dystrybuanty
    wiecejNizkwantyl99 <- posortowaneDane %>% filter(dystrybuanta >="0.99")
    kw_99_HW_cac <- wiecejNizkwantyl99[1,"stopa_zwrotu"]
    cac$met_historycznaWAGI_var99_250obs[t+i] <- kw_99_HW_cac
    
    
    fragmentDanych2_cac <- fragmentDanych_cac %>% filter(stopa_zwrotu > kw_99_HW_cac)
    es_99_HW_cac = mean(fragmentDanych2_cac$stopa_zwrotu)
    cac$met_historycznaWAGI_es99_250obs[t+i] <- es_99_HW_cac
}


plot(cac$met_historycznaWAGI_var99_250obs, type = "l", ylab = "VaR 99%", main = "Jak zmienialo sie VaR w czasie - metoda historyczna z wagami")
plot(cac$met_historycznaWAGI_es99_250obs,type = "l", ylab = "ES 99%", main = "Jak zmienialo sie ES w czasie - metoda historyczna z wagami")






# METODA MONTE CARLO dla 99%

t = 250
cac$monteCarlo_var99 <- NA
cac$monteCarlo_es99 <- NA
iloscWierszy_cac <- nrow(cac)
max_cac = iloscWierszy_cac-t

for (i in c(1:max_cac)){
  #VaR
  fragmentDanych_cac <- cac[i:(t+i-1),]
  fragmentDanychh_cac <- rnorm(t,mean = mean(fragmentDanych_cac$stopa_zwrotu),sd=sd(fragmentDanych_cac$stopa_zwrotu))
  kw_99_MC_cac = quantile(fragmentDanychh_cac, 0.99)
  cac$monteCarlo_var99[t+i] <- kw_99_MC_cac
  
  #ES
  fragmentDanych2_cac <- fragmentDanych_cac %>% filter(stopa_zwrotu > kw_99_MC_cac)
  es_99_MC_cac = mean(fragmentDanych2_cac$stopa_zwrotu)
  cac$monteCarlo_es99[t+i] <- es_99_MC_cac
}


plot(cac$monteCarlo_var99,type = "l", ylab = "VaR 99%", main = "Jak zmienialo sie VaR w czasie - metoda Monte Carlo")
plot(cac$monteCarlo_es99,type = "l", ylab = "ES 99%", main = "Jak zmienialo sie ES w czasie - metoda Monte Carlo")



#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------





## Metoda Historyczna dla 95%


library(dplyr)

t = 250
cac$met_historyczna_var95_250obs <- NA
cac$met_historyczna_es95_250obs <- NA
iloscWierszy_cac <- nrow(cac)
max_cac = iloscWierszy_cac-t

for (i in c(1:max_cac)){
  #VaR
  fragmentDanych_cac <- cac[i:(t+i-1),]
  kw_95_H_cac = quantile(fragmentDanych_cac$stopa_zwrotu, 0.95)
  cac$met_historyczna_var95_250obs[t+i] <- kw_95_H_cac
  
  #ES
  fragmentDanych2_cac <- fragmentDanych_cac %>% filter(stopa_zwrotu > kw_95_H_cac)
  es_95_H_cac = mean(fragmentDanych2_cac$stopa_zwrotu)
  cac$met_historyczna_es95_250obs[t+i] <- es_95_H_cac
}


plot(cac$met_historyczna_var95_250obs, type = "l", ylab = "VaR 95%", main = "Jak zmienialo sie VaR w czasie - metoda historyczna")
plot(cac$met_historyczna_es95_250obs,type = "l", ylab = "ES 95%", main = "Jak zmienialo sie ES w czasie - metoda historyczna")


## Metoda Historyczna z Wagami dla 95%

library(dplyr)
t = 250
cac$met_historycznaWAGI_var95_250obs <- NA
cac$met_historycznaWAGI_es95_250obs <- NA
iloscWierszy_cac <- nrow(cac)
max_cac = iloscWierszy_cac-t
q <- 0.995

for (i in c(1:max_cac)){
  fragmentDanych_cac <- cac[i:(t+i-1),]
  fragmentDanych_cac$prawdopodobienstwa <- NA
  
  for (j in c(1:nrow(fragmentDanych_cac))) {
    wykladnik <- t - j
    licznik <- q**wykladnik*(1-q)
    mianownik <- 1-q**t
    wartosci <- licznik/mianownik 
    fragmentDanych_cac$prawdopodobienstwa[j] <- wartosci
  }
  #sortowanie stopy zwrotu (do dystrybuanty)
  posortowaneDane <- fragmentDanych_cac[order(fragmentDanych_cac$stopa_zwrotu),]
  
  
  #dystrybuanta
  posortowaneDane$dystrybuanta <- NA
  for (j in c(1:nrow(posortowaneDane))) {
    doDystrybuanty <- sum(posortowaneDane[1:j,"prawdopodobienstwa"])
    posortowaneDane$dystrybuanta[j] <- doDystrybuanty
  }
  
  # VaR 95% - kwantyl brany z dystrybuanty
  wiecejNizkwantyl95 <- posortowaneDane %>% filter(dystrybuanta >="0.95")
  kw_95_HW_cac <- wiecejNizkwantyl95[1,"stopa_zwrotu"]
  cac$met_historycznaWAGI_var95_250obs[t+i] <- kw_95_HW_cac
  
  
  fragmentDanych2_cac <- fragmentDanych_cac %>% filter(stopa_zwrotu > kw_95_HW_cac)
  es_95_HW_cac = mean(fragmentDanych2_cac$stopa_zwrotu)
  cac$met_historycznaWAGI_es95_250obs[t+i] <- es_95_HW_cac     
  
}

plot(cac$met_historycznaWAGI_var95_250obs, type = "l", ylab = "VaR 95%", main = "Jak zmienialo sie VaR w czasie - metoda historyczna z wagami")
plot(cac$met_historycznaWAGI_es95_250obs,type = "l", ylab = "ES 95%", main = "Jak zmienialo sie ES w czasie - metoda historyczna z wagami")


# METODA MONTE CARLO dla 95%


t = 250
cac$monteCarlo_var95 <- NA
cac$monteCarlo_es95 <- NA
iloscWierszy_cac <- nrow(cac)
max_cac = iloscWierszy_cac-t

for (i in c(1:max_cac)){
  #VaR
  fragmentDanych_cac <- cac[i:(t+i-1),]
  fragmentDanychh_cac <- rnorm(t,mean = mean(fragmentDanych_cac$stopa_zwrotu),sd=sd(fragmentDanych_cac$stopa_zwrotu))
  kw_95_MC_cac = quantile(fragmentDanychh_cac, 0.95)
  cac$monteCarlo_var95[t+i] <- kw_95_MC_cac
  
  #ES
  fragmentDanych2_cac <- fragmentDanych_cac %>% filter(stopa_zwrotu > kw_95_MC_cac)
  es_95_MC_cac = mean(fragmentDanych2_cac$stopa_zwrotu)
  cac$monteCarlo_es95[t+i] <- es_95_MC_cac
}


plot(cac$monteCarlo_var95,type = "l", ylab = "VaR 95%", main = "Jak zmienialo sie VaR w czasie - metoda Monte Carlo")
plot(cac$monteCarlo_es95,type = "l", ylab = "ES 95%", main = "Jak zmienialo sie ES w czasie - metoda Monte Carlo")







# CAC
## Porownanie 3 metod liczenia VaR dla 99%
par(mfrow=c(1,3))
plot(cac$met_historyczna_var99_250obs, type = "l", ylab = "VaR 99%", main = "metoda historyczna")+
  plot(cac$met_historycznaWAGI_var99_250obs, type = "l", ylab = "VaR 99%", main = "metoda historyczna z wagami")+
  plot(cac$monteCarlo_var99,type = "l", ylab = "VaR 99%", main = "metoda Monte Carlo")




## Porownanie 3 metod liczenia VaR dla 95%
par(mfrow=c(1,3))
plot(cac$met_historyczna_var95_250obs, type = "l", ylab = "VaR 95%", main = "metoda historyczna")+
  plot(cac$met_historycznaWAGI_var95_250obs, type = "l", ylab = "VaR 95%", main = "metoda historyczna z wagami")+
  plot(cac$monteCarlo_var95,type = "l", ylab = "VaR 95%", main = "metoda Monte Carlo")




## Porownanie metod liczenia VaR dla 99% i 95%

# Metoda Historyczna
par(mfrow=c(1,2))
plot(cac$met_historyczna_var99_250obs, type = "l", main = "VaR 99%")+
plot(cac$met_historyczna_var95_250obs, type = "l", main = "VaR 95%")

# Metoda Historyczna z wagami
par(mfrow=c(1,2))
plot(cac$met_historycznaWAGI_var99_250obs, type = "l", main = "VaR 99%")+
plot(cac$met_historycznaWAGI_var95_250obs, type = "l", main = "VaR 95%")
  

# Metoda Monte Carlo
par(mfrow=c(1,2))
plot(cac$monteCarlo_var99,type = "l", main = "VaR 99%")+
plot(cac$monteCarlo_var95,type = "l", main = "VaR 95%")



# ES

## Porownanie 3 metod liczenia ES dla 99%
par(mfrow=c(1,3))
plot(cac$met_historyczna_es99_250obs, type = "l", ylab = "ES 99%", main = "metoda historyczna")+
  plot(cac$met_historycznaWAGI_es99_250obs, type = "l", ylab = "ES 99%", main = "metoda historyczna z wagami")+
  plot(cac$monteCarlo_es99,type = "l", ylab = "ES 99%", main = "metoda Monte Carlo")




## Porownanie 3 metod liczenia ES dla 95%
par(mfrow=c(1,3))
plot(cac$met_historyczna_es95_250obs, type = "l", ylab = "ES 95%", main = "Jak zmienialo sie VaR w czasie - metoda historyczna")+
  plot(cac$met_historycznaWAGI_es95_250obs, type = "l", ylab = "ES 95%", main = "metoda historyczna z wagami")+
  plot(cac$monteCarlo_es95,type = "l", ylab = "ES 95%", main = "metoda Monte Carlo")




## Porownanie metod liczenia ES dla 99% i 95%

# Metoda Historyczna
par(mfrow=c(1,2))
plot(cac$met_historyczna_es99_250obs, type = "l", main = "ES 99%")+
  plot(cac$met_historyczna_es95_250obs, type = "l", main = "ES 95%")

# Metoda Historyczna z wagami
par(mfrow=c(1,2))
plot(cac$met_historycznaWAGI_es99_250obs, type = "l", main = "ES 99%")+
  plot(cac$met_historycznaWAGI_es95_250obs, type = "l", main = "ES 95%")


# Metoda Monte Carlo
par(mfrow=c(1,2))
plot(cac$monteCarlo_es99,type = "l", main = "ES 99%")+
  plot(cac$monteCarlo_es95,type = "l", main = "ES 95%")









# koniec CAC
############################################################################################################################################

### S&P


## Metoda Historyczna dla 99%

library(dplyr)

t = 250
spx$met_historyczna_var99_250obs <- NA
spx$met_historyczna_es99_250obs <- NA
iloscWierszy_spx <- nrow(spx)
max_spx = iloscWierszy_spx-t

for (i in c(1:max_spx)){
  #VaR
  fragmentDanych_spx <- spx[i:(t+i-1),]
  kw_99_H_spx = quantile(fragmentDanych_spx$stopa_zwrotu, 0.99)
  spx$met_historyczna_var99_250obs[t+i] <- kw_99_H_spx
  
  #ES
  fragmentDanych2_spx <- fragmentDanych_spx %>% filter(stopa_zwrotu > kw_99_H_spx)
  es_99_H_spx = mean(fragmentDanych2_spx$stopa_zwrotu)
  spx$met_historyczna_es99_250obs[t+i] <- es_99_H_spx
}

plot(spx$met_historyczna_var99_250obs, type = "l", ylab = "VaR 99%", main = "Jak zmienialo sie VaR w czasie - metoda historyczna")
plot(spx$met_historyczna_es99_250obs, type = "l", ylab = "ES 99%", main = "Jak zmienialo sie VaR w czasie - metoda historyczna")



## Metoda Historyczna z Wagami dla 99%

library(dplyr)
t = 250
spx$met_historycznaWAGI_var99_250obs <- NA
spx$met_historycznaWAGI_es99_250obs <- NA
iloscWierszy_spx <- nrow(spx)
max_spx = iloscWierszy_spx-t
q <- 0.995

for (i in c(1:max_spx)){
  fragmentDanych_spx <- spx[i:(t+i-1),]
  fragmentDanych_spx$prawdopodobienstwa <- NA
  
  for (j in c(1:nrow(fragmentDanych_spx))) {
    wykladnik <- t - j
    licznik <- q**wykladnik*(1-q)
    mianownik <- 1-q**t
    wartosci <- licznik/mianownik 
    fragmentDanych_spx$prawdopodobienstwa[j] <- wartosci
  }
  #sortowanie stopy zwrotu (do dystrybuanty)
  posortowaneDane <- fragmentDanych_spx[order(fragmentDanych_spx$stopa_zwrotu),]
  
  
  #dystrybuanta
  posortowaneDane$dystrybuanta <- NA
  for (j in c(1:nrow(posortowaneDane))) {
    doDystrybuanty <- sum(posortowaneDane[1:j,"prawdopodobienstwa"])
    posortowaneDane$dystrybuanta[j] <- doDystrybuanty
  }
  
  # VaR 99% - kwantyl brany z dystrybuanty
  wiecejNizkwantyl99 <- posortowaneDane %>% filter(dystrybuanta >="0.99")
  kw_99_HW_spx <- wiecejNizkwantyl99[1,"stopa_zwrotu"]
  spx$met_historycznaWAGI_var99_250obs[t+i] <- kw_99_HW_spx
  
  
  fragmentDanych2_spx <- fragmentDanych_spx %>% filter(stopa_zwrotu > kw_99_HW_spx)
  es_99_HW_spx = mean(fragmentDanych2_spx$stopa_zwrotu)
  spx$met_historycznaWAGI_es99_250obs[t+i] <- es_99_HW_spx
}


plot(spx$met_historycznaWAGI_var99_250obs, type = "l", ylab = "VaR 99%", main = "Jak zmienialo sie VaR w czasie - metoda historyczna z wagami")
plot(spx$met_historycznaWAGI_es99_250obs,type = "l", ylab = "ES 99%", main = "Jak zmienialo sie ES w czasie - metoda historyczna z wagami")






## Metoda Monte Carlo dla 99%


t = 250
spx$monteCarlo_var99 <- NA
spx$monteCarlo_es99 <- NA

iloscWierszy_spx <- nrow(spx)
max_spx = iloscWierszy_spx-t

for (i in c(1:max_spx)){
  #VaR
  fragmentDanych_spx <- spx[i:(t+i-1),]
  fragmentDanychh_spx <- rnorm(t,mean = mean(fragmentDanych_spx$stopa_zwrotu),sd=sd(fragmentDanych_spx$stopa_zwrotu))
  kw_99_MC_spx = quantile(fragmentDanychh_spx, 0.99)
  spx$monteCarlo_var99[t+i] <- kw_99_MC_spx
  
  #ES
  fragmentDanych2_spx <- fragmentDanych_spx %>% filter(stopa_zwrotu > kw_99_MC_spx)
  es_99_MC_spx = mean(fragmentDanych2_spx$stopa_zwrotu)
  spx$monteCarlo_es99[t+i] <- es_99_MC_spx
}


plot(spx$monteCarlo_var99,type = "l", ylab = "VaR 99%", main = "Jak zmienialo sie VaR w czasie - metoda Monte Carlo")
plot(spx$monteCarlo_es99,type = "l", ylab = "ES 99%", main = "Jak zmienialo sie ES w czasie - metoda Monte Carlo")





# -------------------------------------------------------------------------------------------------



## Metoda Historyczna dla 95%

library(dplyr)

t = 250
spx$met_historyczna_var95_250obs <- NA
spx$met_historyczna_es95_250obs <- NA
iloscWierszy_spx <- nrow(spx)
max_spx = iloscWierszy_spx-t

for (i in c(1:max_spx)){
  #VaR
  fragmentDanych_spx <- spx[i:(t+i-1),]
  kw_95_H_spx = quantile(fragmentDanych_spx$stopa_zwrotu, 0.95)
  spx$met_historyczna_var95_250obs[t+i] <- kw_95_H_spx
  
  #ES
  fragmentDanych2_spx <- fragmentDanych_spx %>% filter(stopa_zwrotu > kw_95_H_spx)
  es_95_H_spx = mean(fragmentDanych2_spx$stopa_zwrotu)
  spx$met_historyczna_es95_250obs[t+i] <- es_95_H_spx
}

plot(spx$met_historyczna_var95_250obs, type = "l", ylab = "VaR 95%", main = "Jak zmienialo sie VaR w czasie - metoda historyczna")
plot(spx$met_historyczna_es95_250obs, type = "l", ylab = "ES 95%", main = "Jak zmienialo sie ES w czasie - metoda historyczna")





## Metoda Historyczna z Wagami dla 95%


library(dplyr)
t = 250
spx$met_historycznaWAGI_var95_250obs <- NA
spx$met_historycznaWAGI_es95_250obs <- NA
iloscWierszy_spx <- nrow(spx)
max_spx = iloscWierszy_spx-t
q <- 0.995

for (i in c(1:max_spx)){
  fragmentDanych_spx <- spx[i:(t+i-1),]
  fragmentDanych_spx$prawdopodobienstwa <- NA
  
  for (j in c(1:nrow(fragmentDanych_spx))) {
    wykladnik <- t - j
    licznik <- q**wykladnik*(1-q)
    mianownik <- 1-q**t
    wartosci <- licznik/mianownik 
    fragmentDanych_spx$prawdopodobienstwa[j] <- wartosci
  }
  #sortowanie stopy zwrotu (do dystrybuanty)
  posortowaneDane <- fragmentDanych_spx[order(fragmentDanych_spx$stopa_zwrotu),]
  
  
  #dystrybuanta
  posortowaneDane$dystrybuanta <- NA
  for (j in c(1:nrow(posortowaneDane))) {
    doDystrybuanty <- sum(posortowaneDane[1:j,"prawdopodobienstwa"])
    posortowaneDane$dystrybuanta[j] <- doDystrybuanty
  }
  
  # VaR 95% - kwantyl brany z dystrybuanty
  wiecejNizkwantyl95 <- posortowaneDane %>% filter(dystrybuanta >="0.95")
  kw_95_HW_spx <- wiecejNizkwantyl95[1,"stopa_zwrotu"]
  spx$met_historycznaWAGI_var95_250obs[t+i] <- kw_95_HW_spx
  
  
  fragmentDanych2_spx <- fragmentDanych_spx %>% filter(stopa_zwrotu > kw_95_HW_spx)
  es_95_HW_spx = mean(fragmentDanych2_spx$stopa_zwrotu)
  spx$met_historycznaWAGI_es95_250obs[t+i] <- es_95_HW_spx     
  
}

plot(spx$met_historycznaWAGI_var95_250obs, type = "l", ylab = "VaR 95%", main = "Jak zmienialo sie VaR w czasie - metoda historyczna z wagami")
plot(spx$met_historycznaWAGI_es95_250obs,type = "l", ylab = "ES 95%", main = "Jak zmienialo sie ES w czasie - metoda historyczna z wagami")





## Metoda Monte Carlo dla 95%



t = 250
spx$monteCarlo_var95 <- NA
spx$monteCarlo_es95 <- NA
iloscWierszy_spx <- nrow(spx)
max_spx = iloscWierszy_spx-t

for (i in c(1:max_spx)){
  #VaR
  fragmentDanych_spx <- spx[i:(t+i-1),]
  fragmentDanychh_spx <- rnorm(t,mean = mean(fragmentDanych_spx$stopa_zwrotu),sd=sd(fragmentDanych_spx$stopa_zwrotu))
  kw_95_MC_spx = quantile(fragmentDanychh_spx, 0.95)
  spx$monteCarlo_var95[t+i] <- kw_95_MC_spx
  
  #ES
  fragmentDanych2_spx <- fragmentDanych_spx %>% filter(stopa_zwrotu > kw_95_MC_spx)
  es_95_MC_spx = mean(fragmentDanych2_spx$stopa_zwrotu)
  spx$monteCarlo_es95[t+i] <- es_95_MC_spx
}



plot(spx$monteCarlo_var95,type = "l", ylab = "VaR 95%", main = "Jak zmienialo sie VaR w czasie - metoda Monte Carlo")
plot(spx$monteCarlo_es95,type = "l", ylab = "ES 95%", main = "Jak zmienialo sie ES w czasie - metoda Monte Carlo")




# S&P


## Porownanie 3 metod liczenia VaR dla 99%
par(mfrow=c(1,3))
plot(spx$met_historyczna_var99_250obs, type = "l", ylab = "VaR 99%", main = "metoda historyczna")+
  plot(spx$met_historycznaWAGI_var99_250obs, type = "l", ylab = "VaR 99%", main = "metoda historyczna z wagami")+
  plot(spx$monteCarlo_var99,type = "l", ylab = "VaR 99%", main = "metoda Monte Carlo")




## Porownanie 3 metod liczenia VaR dla 95%
par(mfrow=c(1,3))
plot(spx$met_historyczna_var95_250obs, type = "l", ylab = "VaR 95%", main = "metoda historyczna")+
  plot(spx$met_historycznaWAGI_var95_250obs, type = "l", ylab = "VaR 95%", main = "metoda historyczna z wagami")+
  plot(spx$monteCarlo_var95,type = "l", ylab = "VaR 95%", main = "metoda Monte Carlo")




## Porownanie metod liczenia VaR dla 99% i 95%

# Metoda Historyczna
par(mfrow=c(1,2))
plot(spx$met_historyczna_var99_250obs, type = "l", main = "VaR 99%")+
  plot(spx$met_historyczna_var95_250obs, type = "l", main = "VaR 95%")

# Metoda Historyczna z wagami
par(mfrow=c(1,2))
plot(spx$met_historycznaWAGI_var99_250obs, type = "l", main = "VaR 99%")+
  plot(spx$met_historycznaWAGI_var95_250obs, type = "l", main = "VaR 95%")


# Metoda Monte Carlo
par(mfrow=c(1,2))
plot(spx$monteCarlo_var99,type = "l", main = "VaR 99%")+
  plot(spx$monteCarlo_var95,type = "l", main = "VaR 95%")



# ES

## Porownanie 3 metod liczenia ES dla 99%
par(mfrow=c(1,3))
plot(spx$met_historyczna_es99_250obs, type = "l", ylab = "ES 99%", main = "metoda historyczna")+
  plot(spx$met_historycznaWAGI_es99_250obs, type = "l", ylab = "ES 99%", main = "metoda historyczna z wagami")+
  plot(spx$monteCarlo_es99,type = "l", ylab = "ES 99%", main = "metoda Monte Carlo")




## Porownanie 3 metod liczenia ES dla 95%
par(mfrow=c(1,3))
plot(spx$met_historyczna_es95_250obs, type = "l", ylab = "ES 95%", main = "Jak zmienialo sie VaR w czasie - metoda historyczna")+
  plot(spx$met_historycznaWAGI_es95_250obs, type = "l", ylab = "ES 95%", main = "metoda historyczna z wagami")+
  plot(spx$monteCarlo_es95,type = "l", ylab = "ES 95%", main = "metoda Monte Carlo")




## Porownanie metod liczenia ES dla 99% i 95%

# Metoda Historyczna
par(mfrow=c(1,2))
plot(spx$met_historyczna_es99_250obs, type = "l", main = "ES 99%")+
  plot(spx$met_historyczna_es95_250obs, type = "l", main = "ES 95%")

# Metoda Historyczna z wagami
par(mfrow=c(1,2))
plot(spx$met_historycznaWAGI_es99_250obs, type = "l", main = "ES 99%")+
  plot(spx$met_historycznaWAGI_es95_250obs, type = "l", main = "ES 95%")


# Metoda Monte Carlo
par(mfrow=c(1,2))
plot(spx$monteCarlo_es99,type = "l", main = "ES 99%")+
  plot(spx$monteCarlo_es95,type = "l", main = "ES 95%")







# koniec S&P
###################################################################################################
### WIG 30

## Metoda Historyczna dla 99%

library(dplyr)

t = 250
wig30$met_historyczna_var99_250obs <- NA
wig30$met_historyczna_es99_250obs <- NA
iloscWierszy_wig30 <- nrow(wig30)
max_wig30 = iloscWierszy_wig30-t

for (i in c(1:max_wig30)){
  #VaR
  fragmentDanych_wig30 <- wig30[i:(t+i-1),]
  kw_99_H_wig30 = quantile(fragmentDanych_wig30$stopa_zwrotu, 0.99)
  wig30$met_historyczna_var99_250obs[t+i] <- kw_99_H_wig30
  
  #ES
  fragmentDanych2_wig30 <- fragmentDanych_wig30 %>% filter(stopa_zwrotu > kw_99_H_wig30)
  es_99_H_wig30 = mean(fragmentDanych2_wig30$stopa_zwrotu)
  wig30$met_historyczna_es99_250obs[t+i] <- es_99_H_wig30
}

plot(wig30$met_historyczna_var99_250obs, type = "l", ylab = "VaR 99%", main = "Jak zmienialo sie VaR w czasie - metoda historyczna")
plot(wig30$met_historyczna_es99_250obs, type = "l", ylab = "ES 99%", main = "Jak zmienialo sie ES w czasie - metoda historyczna")




## Metoda Historyczna z Wagami dla 99%

library(dplyr)
t = 250
wig30$met_historycznaWAGI_var99_250obs <- NA
wig30$met_historycznaWAGI_es99_250obs <- NA
iloscWierszy_wig30 <- nrow(wig30)
max_wig30 = iloscWierszy_wig30-t
q <- 0.995

for (i in c(1:max_wig30)){
  fragmentDanych_wig30 <- wig30[i:(t+i-1),]
  fragmentDanych_wig30$prawdopodobienstwa <- NA
  
  for (j in c(1:nrow(fragmentDanych_wig30))) {
    wykladnik <- t - j
    licznik <- q**wykladnik*(1-q)
    mianownik <- 1-q**t
    wartosci <- licznik/mianownik 
    fragmentDanych_wig30$prawdopodobienstwa[j] <- wartosci
  }
  #sortowanie stopy zwrotu (do dystrybuanty)
  posortowaneDane <- fragmentDanych_wig30[order(fragmentDanych_wig30$stopa_zwrotu),]
  
  
  #dystrybuanta
  posortowaneDane$dystrybuanta <- NA
  for (j in c(1:nrow(posortowaneDane))) {
    doDystrybuanty <- sum(posortowaneDane[1:j,"prawdopodobienstwa"])
    posortowaneDane$dystrybuanta[j] <- doDystrybuanty
  }
  
  # VaR 99% - kwantyl brany z dystrybuanty
  wiecejNizkwantyl99 <- posortowaneDane %>% filter(dystrybuanta >="0.99")
  kw_99_HW_wig30 <- wiecejNizkwantyl99[1,"stopa_zwrotu"]
  wig30$met_historycznaWAGI_var99_250obs[t+i] <- kw_99_HW_wig30
  
  
  fragmentDanych2_wig30 <- fragmentDanych_wig30 %>% filter(stopa_zwrotu > kw_99_HW_wig30)
  es_99_HW_wig30 = mean(fragmentDanych2_wig30$stopa_zwrotu)
  wig30$met_historycznaWAGI_es99_250obs[t+i] <- es_99_HW_wig30
}


plot(wig30$met_historycznaWAGI_var99_250obs, type = "l", ylab = "VaR 99%", main = "Jak zmienialo sie VaR w czasie - metoda historyczna z wagami")
plot(wig30$met_historycznaWAGI_es99_250obs,type = "l", ylab = "ES 99%", main = "Jak zmienialo sie ES w czasie - metoda historyczna z wagami")






## Metoda Monte Carlo dla 99%


t = 250
wig30$monteCarlo_var99 <- NA
wig30$monteCarlo_es99 <- NA

iloscWierszy_wig30 <- nrow(wig30)
max_wig30 = iloscWierszy_wig30-t

for (i in c(1:max_wig30)){
  #VaR
  fragmentDanych_wig30 <- wig30[i:(t+i-1),]
  fragmentDanychh_wig30 <- rnorm(t,mean = mean(fragmentDanych_wig30$stopa_zwrotu),sd=sd(fragmentDanych_wig30$stopa_zwrotu))
  kw_99_MC_wig30 = quantile(fragmentDanychh_wig30, 0.99)
  wig30$monteCarlo_var99[t+i] <- kw_99_MC_wig30
  
  #ES
  fragmentDanych2_wig30 <- fragmentDanych_wig30 %>% filter(stopa_zwrotu > kw_99_MC_wig30)
  es_99_MC_wig30 = mean(fragmentDanych2_wig30$stopa_zwrotu)
  wig30$monteCarlo_es99[t+i] <- es_99_MC_wig30
}


plot(wig30$monteCarlo_var99,type = "l", ylab = "VaR 99%", main = "Jak zmienialo sie VaR w czasie - metoda Monte Carlo")
plot(wig30$monteCarlo_es99,type = "l", ylab = "ES 99%", main = "Jak zmienialo sie ES w czasie - metoda Monte Carlo")





# -------------------------------------------------------------------------------------------------




## Metoda Historyczna dla 95%

library(dplyr)

t = 250
wig30$met_historyczna_var95_250obs <- NA
wig30$met_historyczna_es95_250obs <- NA
iloscWierszy_wig30 <- nrow(wig30)
max_wig30 = iloscWierszy_wig30-t

for (i in c(1:max_wig30)){
  #VaR
  fragmentDanych_wig30 <- wig30[i:(t+i-1),]
  kw_95_H_wig30 = quantile(fragmentDanych_wig30$stopa_zwrotu, 0.95)
  wig30$met_historyczna_var95_250obs[t+i] <- kw_95_H_wig30
  
  #ES
  fragmentDanych2_wig30 <- fragmentDanych_wig30 %>% filter(stopa_zwrotu > kw_95_H_wig30)
  es_95_H_wig30 = mean(fragmentDanych2_wig30$stopa_zwrotu)
  wig30$met_historyczna_es95_250obs[t+i] <- es_95_H_wig30
}

plot(wig30$met_historyczna_var95_250obs, type = "l", ylab = "VaR 95%", main = "Jak zmienialo sie VaR w czasie - metoda historyczna")
plot(wig30$met_historyczna_es95_250obs, type = "l", ylab = "ES 95%", main = "Jak zmienialo sie ES w czasie - metoda historyczna")





## Metoda Historyczna z Wagami dla 95%


library(dplyr)
t = 250
wig30$met_historycznaWAGI_var95_250obs <- NA
wig30$met_historycznaWAGI_es95_250obs <- NA
iloscWierszy_wig30 <- nrow(wig30)
max_wig30 = iloscWierszy_wig30-t
q <- 0.995

for (i in c(1:max_wig30)){
  fragmentDanych_wig30 <- wig30[i:(t+i-1),]
  fragmentDanych_wig30$prawdopodobienstwa <- NA
  
  for (j in c(1:nrow(fragmentDanych_wig30))) {
    wykladnik <- t - j
    licznik <- q**wykladnik*(1-q)
    mianownik <- 1-q**t
    wartosci <- licznik/mianownik 
    fragmentDanych_wig30$prawdopodobienstwa[j] <- wartosci
  }
  #sortowanie stopy zwrotu (do dystrybuanty)
  posortowaneDane <- fragmentDanych_wig30[order(fragmentDanych_wig30$stopa_zwrotu),]
  
  
  #dystrybuanta
  posortowaneDane$dystrybuanta <- NA
  for (j in c(1:nrow(posortowaneDane))) {
    doDystrybuanty <- sum(posortowaneDane[1:j,"prawdopodobienstwa"])
    posortowaneDane$dystrybuanta[j] <- doDystrybuanty
  }
  
  # VaR 95% - kwantyl brany z dystrybuanty
  wiecejNizkwantyl95 <- posortowaneDane %>% filter(dystrybuanta >="0.95")
  kw_95_HW_wig30 <- wiecejNizkwantyl95[1,"stopa_zwrotu"]
  wig30$met_historycznaWAGI_var95_250obs[t+i] <- kw_95_HW_wig30
  
  
  fragmentDanych2_wig30 <- fragmentDanych_wig30 %>% filter(stopa_zwrotu > kw_95_HW_wig30)
  es_95_HW_wig30 = mean(fragmentDanych2_wig30$stopa_zwrotu)
  wig30$met_historycznaWAGI_es95_250obs[t+i] <- es_95_HW_wig30     
  
}

plot(wig30$met_historycznaWAGI_var95_250obs, type = "l", ylab = "VaR 95%", main = "Jak zmienialo sie VaR w czasie - metoda historyczna z wagami")
plot(wig30$met_historycznaWAGI_es95_250obs,type = "l", ylab = "ES 95%", main = "Jak zmienialo sie ES w czasie - metoda historyczna z wagami")





## Metoda Monte Carlo dla 95%



t = 250
wig30$monteCarlo_var95 <- NA
wig30$monteCarlo_es95 <- NA
iloscWierszy_wig30 <- nrow(wig30)
max_wig30 = iloscWierszy_wig30-t

for (i in c(1:max_wig30)){
  #VaR
  fragmentDanych_wig30 <- wig30[i:(t+i-1),]
  fragmentDanychh_wig30 <- rnorm(t,mean = mean(fragmentDanych_wig30$stopa_zwrotu),sd=sd(fragmentDanych_wig30$stopa_zwrotu))
  kw_95_MC_wig30 = quantile(fragmentDanychh_wig30, 0.95)
  wig30$monteCarlo_var95[t+i] <- kw_95_MC_wig30
  
  #ES
  fragmentDanych2_wig30 <- fragmentDanych_wig30 %>% filter(stopa_zwrotu > kw_95_MC_wig30)
  es_95_MC_wig30 = mean(fragmentDanych2_wig30$stopa_zwrotu)
  wig30$monteCarlo_es95[t+i] <- es_95_MC_wig30
}



plot(wig30$monteCarlo_var95,type = "l", ylab = "VaR 95%", main = "Jak zmienialo sie VaR w czasie - metoda Monte Carlo")
plot(wig30$monteCarlo_es95,type = "l", ylab = "ES 95%", main = "Jak zmienialo sie ES w czasie - metoda Monte Carlo")







# WIG 30


## Porownanie 3 metod liczenia VaR dla 99%
par(mfrow=c(1,3))
plot(wig30$met_historyczna_var99_250obs, type = "l", ylab = "VaR 99%", main = "metoda historyczna")+
  plot(wig30$met_historycznaWAGI_var99_250obs, type = "l", ylab = "VaR 99%", main = "metoda historyczna z wagami")+
  plot(wig30$monteCarlo_var99,type = "l", ylab = "VaR 99%", main = "metoda Monte Carlo")




## Porownanie 3 metod liczenia VaR dla 95%
par(mfrow=c(1,3))
plot(wig30$met_historyczna_var95_250obs, type = "l", ylab = "VaR 95%", main = "metoda historyczna")+
  plot(wig30$met_historycznaWAGI_var95_250obs, type = "l", ylab = "VaR 95%", main = "metoda historyczna z wagami")+
  plot(wig30$monteCarlo_var95,type = "l", ylab = "VaR 95%", main = "metoda Monte Carlo")




## Porownanie metod liczenia VaR dla 99% i 95%

# Metoda Historyczna
par(mfrow=c(1,2))
plot(wig30$met_historyczna_var99_250obs, type = "l", main = "VaR 99%")+
  plot(wig30$met_historyczna_var95_250obs, type = "l", main = "VaR 95%")

# Metoda Historyczna z wagami
par(mfrow=c(1,2))
plot(wig30$met_historycznaWAGI_var99_250obs, type = "l", main = "VaR 99%")+
  plot(wig30$met_historycznaWAGI_var95_250obs, type = "l", main = "VaR 95%")


# Metoda Monte Carlo
par(mfrow=c(1,2))
plot(wig30$monteCarlo_var99,type = "l", main = "VaR 99%")+
  plot(wig30$monteCarlo_var95,type = "l", main = "VaR 95%")



# ES

## Porownanie 3 metod liczenia ES dla 99%
par(mfrow=c(1,3))
plot(wig30$met_historyczna_es99_250obs, type = "l", ylab = "ES 99%", main = "metoda historyczna")+
  plot(wig30$met_historycznaWAGI_es99_250obs, type = "l", ylab = "ES 99%", main = "metoda historyczna z wagami")+
  plot(wig30$monteCarlo_es99,type = "l", ylab = "ES 99%", main = "metoda Monte Carlo")




## Porownanie 3 metod liczenia ES dla 95%
par(mfrow=c(1,3))
plot(wig30$met_historyczna_es95_250obs, type = "l", ylab = "ES 95%", main = "=metoda historyczna")+
  plot(wig30$met_historycznaWAGI_es95_250obs, type = "l", ylab = "ES 95%", main = "metoda historyczna z wagami")+
  plot(wig30$monteCarlo_es95,type = "l", ylab = "ES 95%", main = "metoda Monte Carlo")




## Porownanie metod liczenia ES dla 99% i 95%

# Metoda Historyczna
par(mfrow=c(1,2))
plot(wig30$met_historyczna_es99_250obs, type = "l", main = "ES 99%")+
  plot(wig30$met_historyczna_es95_250obs, type = "l", main = "ES 95%")

# Metoda Historyczna z wagami
par(mfrow=c(1,2))
plot(wig30$met_historycznaWAGI_es99_250obs, type = "l", main = "ES 99%")+
  plot(wig30$met_historycznaWAGI_es95_250obs, type = "l", main = "ES 95%")


# Metoda Monte Carlo
par(mfrow=c(1,2))
plot(wig30$monteCarlo_es99,type = "l", main = "ES 99%")+
  plot(wig30$monteCarlo_es95,type = "l", main = "ES 95%")





#koniec WIG30
###################################################################################################



# ----------------------------------------------------------------------------------------------
## Testy wsteczne 

# test Christoffersena
# H0 - VaR wyznaczony poprawnie
# H1 - VaR wyznaczony niepoprawnie



## *Francja (CAC 40)*


# Metoda Historyczna - VaR 99%


library(Dowd)
poziomUfnosci <- 0.95
cac$Christofersen_H99 <- NA
library(segMGarch)
cac$TL_H99 <- NA
iloscWierszy_cac <- nrow(cac)
max_cac = iloscWierszy_cac-t

for (i in c(1:max_cac)){
  
  fragmentDanych_cac <- cac[i:(t+i-1),]
  kw_99_H_cac = quantile(fragmentDanych_cac$stopa_zwrotu, 0.99)
  christoffersen_test = ChristoffersenBacktestForUnconditionalCoverage(fragmentDanych_cac$stopa_zwrotu,kw_99_H_cac,poziomUfnosci)
  cac$Christofersen_H99[t+i] <- christoffersen_test
  tl_test <- TL(fragmentDanych_cac$stopa_zwrotu,VaR =kw_99_H_cac,VaR_level =  poziomUfnosci)$color
  cac$TL_H99[t+i] <- tl_test
}

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierz_cac_H99<- cac$Christofersen_H99 > 0.05
macierz_cac_H99 <- na.omit(macierz_cac_H99)
przyjeteH0_cac_H99 <- sum(ifelse(macierz_cac_H99==TRUE,1,0))
odrzuconoH0_cac_H99 <- sum(ifelse(macierz_cac_H99==FALSE,1,0))
# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierzTL_cac_H99<- cac$TL_H99 
macierzTL_cac_H99 <- na.omit(macierzTL_cac_H99)
strefaCzerwonaTL_H99_cac <- sum(ifelse(macierzTL_cac_H99=="red",1,0))
strefaZolta_H99_cac <- sum(ifelse(macierzTL_cac_H99=="yellow",1,0))
strefaZielona_H99_cac <- sum(ifelse(macierzTL_cac_H99=="green",1,0))

# Przyjeto H0
przyjeteH0_cac_H99
# Odrzucono H0
odrzuconoH0_cac_H99 

#--------------------------------------------------------------------------------------------------------

# Metoda Historyczna z wagami - VaR 99%

library(dplyr)
library(Dowd)
poziomUfnosci <- 0.95
cac$Christofersen_HW99 <- NA
t = 250
iloscWierszy_cac <- nrow(cac)
max_cac = iloscWierszy_cac-t
q <- 0.995
library(segMGarch)
cac$TL_HW99 <- NA

for (i in c(1:max_cac)){
  fragmentDanych_cac <- cac[i:(t+i-1),]
  fragmentDanych_cac$prawdopodobienstwa <- NA
  
  for (j in c(1:nrow(fragmentDanych_cac))) {
    wykladnik <- t - j
    licznik <- q**wykladnik*(1-q)
    mianownik <- 1-q**t
    wartosci <- licznik/mianownik 
    fragmentDanych_cac$prawdopodobienstwa[j] <- wartosci
  }
  #sortowanie stopy zwrotu (do dystrybuanty)
  posortowaneDane <- fragmentDanych_cac[order(fragmentDanych_cac$stopa_zwrotu),]
  
  
  #dystrybuanta
  posortowaneDane$dystrybuanta <- NA
  for (j in c(1:nrow(posortowaneDane))) {
    doDystrybuanty <- sum(posortowaneDane[1:j,"prawdopodobienstwa"])
    posortowaneDane$dystrybuanta[j] <- doDystrybuanty
  }
  
  # VaR 99% - kwantyl brany z dystrybuanty
  wiecejNizkwantyl99 <- posortowaneDane %>% filter(dystrybuanta >="0.99")
  kw_99_HW_cac <- wiecejNizkwantyl99[1,"stopa_zwrotu"]
  cac$met_historycznaWAGI_var99_250obs[t+i] <- kw_99_HW_cac
  christoffersen_test = ChristoffersenBacktestForUnconditionalCoverage(fragmentDanych_cac$stopa_zwrotu,kw_99_HW_cac,poziomUfnosci)
  cac$Christofersen_HW99[t+i] <- christoffersen_test
  tl_test <- TL(fragmentDanych_cac$stopa_zwrotu,VaR =kw_99_HW_cac,VaR_level =  poziomUfnosci)$color
  cac$TL_HW99[t+i] <- tl_test
}

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierz_cac_HW99<- cac$Christofersen_HW99 > 0.05
macierz_cac_HW99 <- na.omit(macierz_cac_HW99)
przyjeteH0_cac_HW99 <- sum(ifelse(macierz_cac_HW99==TRUE,1,0))
odrzuconoH0_cac_HW99 <- sum(ifelse(macierz_cac_HW99==FALSE,1,0))

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierzTL_cac_HW99<- cac$TL_HW99 
macierzTL_cac_HW99 <- na.omit(macierzTL_cac_HW99)
strefaCzerwonaTL_HW99_cac <- sum(ifelse(macierzTL_cac_HW99=="red",1,0))
strefaZolta_HW99_cac <- sum(ifelse(macierzTL_cac_HW99=="yellow",1,0))
strefaZielona_HW99_cac <- sum(ifelse(macierzTL_cac_HW99=="green",1,0))

# Przyjeto H0
przyjeteH0_cac_HW99
# Odrzucono H0
odrzuconoH0_cac_HW99 

#--------------------------------------------------------------------------------------------------------

# Metoda Monte Carlo - VaR 99%



t = 250
library(Dowd)
poziomUfnosci <- 0.95
cac$Christofersen_MC99 <- NA
iloscWierszy_cac <- nrow(cac)
max_cac = iloscWierszy_cac-t
library(segMGarch)
cac$TL_MC99 <- NA


for (i in c(1:max_cac)){
  #VaR
  fragmentDanych_cac <- cac[i:(t+i-1),]
  fragmentDanychh_cac <- rnorm(t,mean = mean(fragmentDanych_cac$stopa_zwrotu),sd=sd(fragmentDanych_cac$stopa_zwrotu))
  kw_99_MC_cac = quantile(fragmentDanychh_cac, 0.99)
  cac$monteCarlo_var99[t+i] <- kw_99_MC_cac
  christoffersen_test = ChristoffersenBacktestForUnconditionalCoverage(fragmentDanych_cac$stopa_zwrotu,kw_99_MC_cac,poziomUfnosci)
  cac$Christofersen_MC99[t+i] <- christoffersen_test
  tl_test <- TL(fragmentDanych_cac$stopa_zwrotu,VaR =kw_99_MC_cac,VaR_level =  poziomUfnosci)$color
  cac$TL_MC99[t+i] <- tl_test

}

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierz_cac_MC99<- cac$Christofersen_MC99 > 0.05
macierz_cac_MC99 <- na.omit(macierz_cac_MC99)
przyjeteH0_cac_MC99 <- sum(ifelse(macierz_cac_MC99==TRUE,1,0))
odrzuconoH0_cac_MC99 <- sum(ifelse(macierz_cac_MC99==FALSE,1,0))

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierzTL_cac_MC99<- cac$TL_MC99 
macierzTL_cac_MC99 <- na.omit(macierzTL_cac_MC99)
strefaCzerwonaTL_MC99_cac <- sum(ifelse(macierzTL_cac_MC99=="red",1,0))
strefaZolta_MC99_cac <- sum(ifelse(macierzTL_cac_MC99=="yellow",1,0))
strefaZielona_MC99_cac <- sum(ifelse(macierzTL_cac_MC99=="green",1,0))

# Przyjeto H0
przyjeteH0_cac_MC99
# Odrzucono H0
odrzuconoH0_cac_MC99 




# Metoda Historyczna - VaR 95%


library(Dowd)
poziomUfnosci <- 0.95
cac$Christofersen_H95 <- NA
library(segMGarch)
cac$TL_H95 <- NA
iloscWierszy_cac <- nrow(cac)
max_cac = iloscWierszy_cac-t

for (i in c(1:max_cac)){
  
  fragmentDanych_cac <- cac[i:(t+i-1),]
  kw_95_H_cac = quantile(fragmentDanych_cac$stopa_zwrotu, 0.95)
  christoffersen_test = ChristoffersenBacktestForUnconditionalCoverage(fragmentDanych_cac$stopa_zwrotu,kw_95_H_cac,poziomUfnosci)
  cac$Christofersen_H95[t+i] <- christoffersen_test
  tl_test <- TL(fragmentDanych_cac$stopa_zwrotu,VaR =kw_95_H_cac,VaR_level =  poziomUfnosci)$color
  cac$TL_H95[t+i] <- tl_test
}

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierz_cac_H95<- cac$Christofersen_H95 > 0.05
macierz_cac_H95 <- na.omit(macierz_cac_H95)
przyjeteH0_cac_H95 <- sum(ifelse(macierz_cac_H95==TRUE,1,0))
odrzuconoH0_cac_H95 <- sum(ifelse(macierz_cac_H95==FALSE,1,0))
# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierzTL_cac_H95<- cac$TL_H95 
macierzTL_cac_H95 <- na.omit(macierzTL_cac_H95)
strefaCzerwonaTL_H95_cac <- sum(ifelse(macierzTL_cac_H95=="red",1,0))
strefaZolta_H95_cac <- sum(ifelse(macierzTL_cac_H95=="yellow",1,0))
strefaZielona_H95_cac <- sum(ifelse(macierzTL_cac_H95=="green",1,0))



# Przyjeto H0
przyjeteH0_cac_H95
# Odrzucono H0
odrzuconoH0_cac_H95 

#--------------------------------------------------------------------------------------------------------

# Metoda Historyczna z wagami - VaR 95%


library(Dowd)
poziomUfnosci <- 0.95
cac$Christofersen_HW95 <- NA
library(dplyr)
t = 250
iloscWierszy_cac <- nrow(cac)
max_cac = iloscWierszy_cac-t
q <- 0.995
library(segMGarch)
cac$TL_HW95 <- NA


for (i in c(1:max_cac)){
  fragmentDanych_cac <- cac[i:(t+i-1),]
  fragmentDanych_cac$prawdopodobienstwa <- NA
  
  for (j in c(1:nrow(fragmentDanych_cac))) {
    wykladnik <- t - j
    licznik <- q**wykladnik*(1-q)
    mianownik <- 1-q**t
    wartosci <- licznik/mianownik 
    fragmentDanych_cac$prawdopodobienstwa[j] <- wartosci
  }
  #sortowanie stopy zwrotu (do dystrybuanty)
  posortowaneDane <- fragmentDanych_cac[order(fragmentDanych_cac$stopa_zwrotu),]
  
  
  #dystrybuanta
  posortowaneDane$dystrybuanta <- NA
  for (j in c(1:nrow(posortowaneDane))) {
    doDystrybuanty <- sum(posortowaneDane[1:j,"prawdopodobienstwa"])
    posortowaneDane$dystrybuanta[j] <- doDystrybuanty
  }
  
  # VaR 95% - kwantyl brany z dystrybuanty
  wiecejNizkwantyl95 <- posortowaneDane %>% filter(dystrybuanta >="0.95")
  kw_95_HW_cac <- wiecejNizkwantyl95[1,"stopa_zwrotu"]
  cac$met_historycznaWAGI_var95_250obs[t+i] <- kw_95_HW_cac
  christoffersen_test = ChristoffersenBacktestForUnconditionalCoverage(fragmentDanych_cac$stopa_zwrotu,kw_95_HW_cac,poziomUfnosci)
  cac$Christofersen_HW95[t+i] <- christoffersen_test
  tl_test <- TL(fragmentDanych_cac$stopa_zwrotu,VaR =kw_95_HW_cac,VaR_level =  poziomUfnosci)$color
  cac$TL_HW95[t+i] <- tl_test
}

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierz_cac_HW95<- cac$Christofersen_HW95 > 0.05
macierz_cac_HW95 <- na.omit(macierz_cac_HW95)
przyjeteH0_cac_HW95 <- sum(ifelse(macierz_cac_HW95==TRUE,1,0))
odrzuconoH0_cac_HW95 <- sum(ifelse(macierz_cac_HW95==FALSE,1,0))

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierzTL_cac_HW95<- cac$TL_HW95 
macierzTL_cac_HW95 <- na.omit(macierzTL_cac_HW95)
strefaCzerwonaTL_HW95_cac <- sum(ifelse(macierzTL_cac_HW95=="red",1,0))
strefaZolta_HW95_cac <- sum(ifelse(macierzTL_cac_HW95=="yellow",1,0))
strefaZielona_HW95_cac <- sum(ifelse(macierzTL_cac_HW95=="green",1,0))



# Przyjeto H0
przyjeteH0_cac_HW95
# Odrzucono H0
odrzuconoH0_cac_HW95 

#--------------------------------------------------------------------------------------------------------

# Metoda Monte Carlo - VaR 95%


library(Dowd)
poziomUfnosci <- 0.95
cac$Christofersen_MC95 <- NA
t = 250
iloscWierszy_spx <- nrow(cac)
max_spx = iloscWierszy_cac-t
library(segMGarch)
cac$TL_MC95 <- NA


for (i in c(1:max_cac)){
  #VaR
  fragmentDanych_cac <- cac[i:(t+i-1),]
  fragmentDanychh_cac <- rnorm(t,mean = mean(fragmentDanych_cac$stopa_zwrotu),sd=sd(fragmentDanych_cac$stopa_zwrotu))
  kw_95_MC_cac = quantile(fragmentDanychh_cac, 0.95)
  cac$monteCarlo_var95[t+i] <- kw_95_MC_cac
  christoffersen_test = ChristoffersenBacktestForUnconditionalCoverage(fragmentDanych_cac$stopa_zwrotu,kw_95_MC_cac,poziomUfnosci)
  cac$Christofersen_MC95[t+i] <- christoffersen_test
  tl_test <- TL(fragmentDanych_cac$stopa_zwrotu,VaR =kw_95_MC_cac,VaR_level =  poziomUfnosci)$color
  cac$TL_MC95[t+i] <- tl_test
}

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierz_cac_MC95<- cac$Christofersen_MC95 > 0.05
macierz_cac_MC95 <- na.omit(macierz_cac_MC95)
przyjeteH0_cac_MC95 <- sum(ifelse(macierz_cac_MC95==TRUE,1,0))
odrzuconoH0_cac_MC95 <- sum(ifelse(macierz_cac_MC95==FALSE,1,0))

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierzTL_cac_MC95<- cac$TL_MC95 
macierzTL_cac_MC95 <- na.omit(macierzTL_cac_MC95)
strefaCzerwonaTL_MC95_cac <- sum(ifelse(macierzTL_cac_MC95=="red",1,0))
strefaZolta_MC95_cac <- sum(ifelse(macierzTL_cac_MC95=="yellow",1,0))
strefaZielona_MC95_cac <- sum(ifelse(macierzTL_cac_MC95=="green",1,0))


# Przyjeto H0
przyjeteH0_cac_MC95
# Odrzucono H0
odrzuconoH0_cac_MC95 


###########################################################################################################################
## *Ameryka (S&P 500)*

# Metoda Historyczna - VaR 99%
library(Dowd)
poziomUfnosci <- 0.95
spx$Christofersen_H99 <- NA
library(segMGarch)
spx$TL_H99 <- NA
iloscWierszy_spx <- nrow(spx)
max_spx = iloscWierszy_spx-t

for (i in c(1:max_spx)){
  
  fragmentDanych_spx <- spx[i:(t+i-1),]
  kw_99_H_spx = quantile(fragmentDanych_spx$stopa_zwrotu, 0.99)
  christoffersen_test = ChristoffersenBacktestForUnconditionalCoverage(fragmentDanych_spx$stopa_zwrotu,kw_99_H_spx,poziomUfnosci)
  spx$Christofersen_H99[t+i] <- christoffersen_test
  tl_test <- TL(fragmentDanych_spx$stopa_zwrotu,VaR =kw_99_H_spx,VaR_level =  poziomUfnosci)$color
  spx$TL_H99[t+i] <- tl_test
}

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierz_spx_H99<- spx$Christofersen_H99 > 0.05
macierz_spx_H99 <- na.omit(macierz_spx_H99)
przyjeteH0_spx_H99 <- sum(ifelse(macierz_spx_H99==TRUE,1,0))
odrzuconoH0_spx_H99 <- sum(ifelse(macierz_spx_H99==FALSE,1,0))
# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierzTL_spx_H99<- spx$TL_H99 
macierzTL_spx_H99 <- na.omit(macierzTL_spx_H99)
strefaCzerwonaTL_H99_spx <- sum(ifelse(macierzTL_spx_H99=="red",1,0))
strefaZolta_H99_spx <- sum(ifelse(macierzTL_spx_H99=="yellow",1,0))
strefaZielona_H99_spx <- sum(ifelse(macierzTL_spx_H99=="green",1,0))


# Przyjeto H0
przyjeteH0_spx_H99
# Odrzucono H0
odrzuconoH0_spx_H99 

#--------------------------------------------------------------------------------------------------------

# Metoda Historyczna z wagami - VaR 99%
library(Dowd)
poziomUfnosci <- 0.95
spx$Christofersen_HW99 <- NA
library(dplyr)
t = 250
iloscWierszy_spx <- nrow(spx)
max_spx = iloscWierszy_spx-t
q <- 0.995
library(segMGarch)
spx$TL_HW99 <- NA


for (i in c(1:max_spx)){
  fragmentDanych_spx <- spx[i:(t+i-1),]
  fragmentDanych_spx$prawdopodobienstwa <- NA
  
  for (j in c(1:nrow(fragmentDanych_spx))) {
    wykladnik <- t - j
    licznik <- q**wykladnik*(1-q)
    mianownik <- 1-q**t
    wartosci <- licznik/mianownik 
    fragmentDanych_spx$prawdopodobienstwa[j] <- wartosci
  }
  #sortowanie stopy zwrotu (do dystrybuanty)
  posortowaneDane <- fragmentDanych_spx[order(fragmentDanych_spx$stopa_zwrotu),]
  
  
  #dystrybuanta
  posortowaneDane$dystrybuanta <- NA
  for (j in c(1:nrow(posortowaneDane))) {
    doDystrybuanty <- sum(posortowaneDane[1:j,"prawdopodobienstwa"])
    posortowaneDane$dystrybuanta[j] <- doDystrybuanty
  }
  
  # VaR 99% - kwantyl brany z dystrybuanty
  wiecejNizkwantyl99 <- posortowaneDane %>% filter(dystrybuanta >="0.99")
  kw_99_HW_spx <- wiecejNizkwantyl99[1,"stopa_zwrotu"]
  spx$met_historycznaWAGI_var99_250obs[t+i] <- kw_99_HW_spx
  christoffersen_test = ChristoffersenBacktestForUnconditionalCoverage(fragmentDanych_spx$stopa_zwrotu,kw_99_HW_spx,poziomUfnosci)
  spx$Christofersen_HW99[t+i] <- christoffersen_test
  tl_test <- TL(fragmentDanych_spx$stopa_zwrotu,VaR =kw_99_HW_spx,VaR_level =  poziomUfnosci)$color
  spx$TL_HW99[t+i] <- tl_test
}

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierz_spx_HW99<- spx$Christofersen_HW99 > 0.05
macierz_spx_HW99 <- na.omit(macierz_spx_HW99)
przyjeteH0_spx_HW99 <- sum(ifelse(macierz_spx_HW99==TRUE,1,0))
odrzuconoH0_spx_HW99 <- sum(ifelse(macierz_spx_HW99==FALSE,1,0))

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierzTL_spx_HW99<- cac$TL_HW99 
macierzTL_spx_HW99 <- na.omit(macierzTL_spx_HW99)
strefaCzerwonaTL_HW99_spx <- sum(ifelse(macierzTL_spx_HW99=="red",1,0))
strefaZolta_HW99_spx <- sum(ifelse(macierzTL_spx_HW99=="yellow",1,0))
strefaZielona_HW99_spx <- sum(ifelse(macierzTL_spx_HW99=="green",1,0))

# Przyjeto H0
przyjeteH0_spx_HW99
# Odrzucono H0
odrzuconoH0_spx_HW99 

#--------------------------------------------------------------------------------------------------------

# Metoda Monte Carlo - VaR 99%


library(Dowd)
poziomUfnosci <- 0.95
spx$Christofersen_MC99 <- NA
t = 250
spx$monteCarlo_var99 <- NA
spx$monteCarlo_es99 <- NA
iloscWierszy_spx <- nrow(spx)
max_spx = iloscWierszy_spx-t
library(segMGarch)
spx$TL_MC99 <- NA


for (i in c(1:max_spx)){
  #VaR
  fragmentDanych_spx <- spx[i:(t+i-1),]
  fragmentDanychh_spx <- rnorm(t,mean = mean(fragmentDanych_spx$stopa_zwrotu),sd=sd(fragmentDanych_spx$stopa_zwrotu))
  kw_99_MC_spx = quantile(fragmentDanychh_spx, 0.99)
  spx$monteCarlo_var99[t+i] <- kw_99_MC_spx
  christoffersen_test = ChristoffersenBacktestForUnconditionalCoverage(fragmentDanych_spx$stopa_zwrotu,kw_99_MC_spx,poziomUfnosci)
  spx$Christofersen_MC99[t+i] <- christoffersen_test
  tl_test <- TL(fragmentDanych_spx$stopa_zwrotu,VaR =kw_99_MC_spx,VaR_level =  poziomUfnosci)$color
  spx$TL_MC99[t+i] <- tl_test

}

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierz_spx_MC99<- spx$Christofersen_MC99 > 0.05
macierz_spx_MC99 <- na.omit(macierz_spx_MC99)
przyjeteH0_spx_MC99 <- sum(ifelse(macierz_spx_MC99==TRUE,1,0))
odrzuconoH0_spx_MC99 <- sum(ifelse(macierz_spx_MC99==FALSE,1,0))

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierzTL_spx_MC99<- cac$TL_MC99 
macierzTL_spx_MC99 <- na.omit(macierzTL_spx_MC99)
strefaCzerwonaTL_MC99_spx <- sum(ifelse(macierzTL_spx_MC99=="red",1,0))
strefaZolta_MC99_spx <- sum(ifelse(macierzTL_spx_MC99=="yellow",1,0))
strefaZielona_MC99_spx <- sum(ifelse(macierzTL_spx_MC99=="green",1,0))

# Przyjeto H0
przyjeteH0_spx_MC99
# Odrzucono H0
odrzuconoH0_spx_MC99 







# Metoda Historyczna - VaR 95%


library(Dowd)
poziomUfnosci <- 0.95
spx$Christofersen_H95 <- NA
library(segMGarch)
spx$TL_H95 <- NA
iloscWierszy_spx <- nrow(spx)
max_spx = iloscWierszy_spx-t


for (i in c(1:max_spx)){
  
  fragmentDanych_spx <- spx[i:(t+i-1),]
  kw_95_H_spx = quantile(fragmentDanych_spx$stopa_zwrotu, 0.95)
  christoffersen_test = ChristoffersenBacktestForUnconditionalCoverage(fragmentDanych_spx$stopa_zwrotu,kw_95_H_spx,poziomUfnosci)
  spx$Christofersen_H95[t+i] <- christoffersen_test
  tl_test <- TL(fragmentDanych_spx$stopa_zwrotu,VaR =kw_95_H_spx,VaR_level =  poziomUfnosci)$color
  spx$TL_H95[t+i] <- tl_test
}

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierz_spx_H95<- spx$Christofersen_H95 > 0.05
macierz_spx_H95 <- na.omit(macierz_spx_H95)
przyjeteH0_spx_H95 <- sum(ifelse(macierz_spx_H95==TRUE,1,0))
odrzuconoH0_spx_H95 <- sum(ifelse(macierz_spx_H95==FALSE,1,0))

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierzTL_spx_H95<- cac$TL_H95 
macierzTL_spx_H95 <- na.omit(macierzTL_spx_H95)
strefaCzerwonaTL_H95_spx <- sum(ifelse(macierzTL_spx_H95=="red",1,0))
strefaZolta_H95_spx <- sum(ifelse(macierzTL_spx_H95=="yellow",1,0))
strefaZielona_H95_spx <- sum(ifelse(macierzTL_spx_H95=="green",1,0))


# Przyjeto H0
przyjeteH0_spx_H95
# Odrzucono H0
odrzuconoH0_spx_H95 

#--------------------------------------------------------------------------------------------------------

# Metoda Historyczna z wagami - VaR 95%
library(Dowd)
poziomUfnosci <- 0.95
spx$Christofersen_HW95 <- NA
library(dplyr)
t = 250
iloscWierszy_spx <- nrow(spx)
max_spx = iloscWierszy_spx-t
q <- 0.995
library(segMGarch)
spx$TL_HW95 <- NA


for (i in c(1:max_spx)){
  fragmentDanych_spx <- spx[i:(t+i-1),]
  fragmentDanych_spx$prawdopodobienstwa <- NA
  
  for (j in c(1:nrow(fragmentDanych_spx))) {
    wykladnik <- t - j
    licznik <- q**wykladnik*(1-q)
    mianownik <- 1-q**t
    wartosci <- licznik/mianownik 
    fragmentDanych_spx$prawdopodobienstwa[j] <- wartosci
  }
  #sortowanie stopy zwrotu (do dystrybuanty)
  posortowaneDane <- fragmentDanych_spx[order(fragmentDanych_spx$stopa_zwrotu),]
  
  
  #dystrybuanta
  posortowaneDane$dystrybuanta <- NA
  for (j in c(1:nrow(posortowaneDane))) {
    doDystrybuanty <- sum(posortowaneDane[1:j,"prawdopodobienstwa"])
    posortowaneDane$dystrybuanta[j] <- doDystrybuanty
  }
  
  # VaR 95% - kwantyl brany z dystrybuanty
  wiecejNizkwantyl95 <- posortowaneDane %>% filter(dystrybuanta >="0.95")
  kw_95_HW_spx <- wiecejNizkwantyl95[1,"stopa_zwrotu"]
  spx$met_historycznaWAGI_var95_250obs[t+i] <- kw_95_HW_spx
  christoffersen_test = ChristoffersenBacktestForUnconditionalCoverage(fragmentDanych_spx$stopa_zwrotu,kw_95_HW_spx,poziomUfnosci)
  spx$Christofersen_HW95[t+i] <- christoffersen_test 
  tl_test <- TL(fragmentDanych_spx$stopa_zwrotu,VaR =kw_95_HW_spx,VaR_level =  poziomUfnosci)$color
  spx$TL_HW95[t+i] <- tl_test
}


# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierz_spx_HW95<- spx$Christofersen_HW95 > 0.05
macierz_spx_HW95 <- na.omit(macierz_spx_HW95)
przyjeteH0_spx_HW95 <- sum(ifelse(macierz_spx_HW95==TRUE,1,0))
odrzuconoH0_spx_HW95 <- sum(ifelse(macierz_spx_HW95==FALSE,1,0))

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierzTL_spx_HW95<- cac$TL_HW95 
macierzTL_spx_HW95 <- na.omit(macierzTL_spx_HW95)
strefaCzerwonaTL_HW95_spx <- sum(ifelse(macierzTL_spx_HW95=="red",1,0))
strefaZolta_HW95_spx <- sum(ifelse(macierzTL_spx_HW95=="yellow",1,0))
strefaZielona_HW95_spx <- sum(ifelse(macierzTL_spx_HW95=="green",1,0))

# Przyjeto H0
przyjeteH0_spx_HW95
# Odrzucono H0
odrzuconoH0_spx_HW95 

#--------------------------------------------------------------------------------------------------------

# Metoda Monte Carlo - VaR 95%


library(Dowd)
poziomUfnosci <- 0.95
spx$Christofersen_MC95 <- NA
t = 250
iloscWierszy_spx <- nrow(spx)
max_spx = iloscWierszy_spx-t
library(segMGarch)
spx$TL_MC95 <- NA


for (i in c(1:max_spx)){
  #VaR
  fragmentDanych_spx <- spx[i:(t+i-1),]
  fragmentDanychh_spx <- rnorm(t,mean = mean(fragmentDanych_spx$stopa_zwrotu),sd=sd(fragmentDanych_spx$stopa_zwrotu))
  kw_95_MC_spx = quantile(fragmentDanychh_spx, 0.95)
  spx$monteCarlo_var95[t+i] <- kw_95_MC_spx
  christoffersen_test = ChristoffersenBacktestForUnconditionalCoverage(fragmentDanych_spx$stopa_zwrotu,kw_95_MC_spx,poziomUfnosci)
  spx$Christofersen_MC95[t+i] <- christoffersen_test
  tl_test <- TL(fragmentDanych_spx$stopa_zwrotu,VaR =kw_95_MC_spx,VaR_level =  poziomUfnosci)$color
  spx$TL_MC95[t+i] <- tl_test
}

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierz_spx_MC95<- spx$Christofersen_MC95 > 0.05
macierz_spx_MC95 <- na.omit(macierz_spx_MC95)
przyjeteH0_spx_MC95 <- sum(ifelse(macierz_spx_MC95==TRUE,1,0))
odrzuconoH0_spx_MC95 <- sum(ifelse(macierz_spx_MC95==FALSE,1,0))

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierzTL_spx_MC95<- cac$TL_MC95 
macierzTL_spx_MC95 <- na.omit(macierzTL_spx_MC95)
strefaCzerwonaTL_MC95_spx <- sum(ifelse(macierzTL_spx_MC95=="red",1,0))
strefaZolta_MC95_spx <- sum(ifelse(macierzTL_spx_MC95=="yellow",1,0))
strefaZielona_MC95_spx <- sum(ifelse(macierzTL_spx_MC95=="green",1,0))

# Przyjeto H0
przyjeteH0_spx_MC95
# Odrzucono H0
odrzuconoH0_spx_MC95 



#######################################################################################################################
## *Polska (WIG 30)*

# Metoda Historyczna - VaR 99%
t=250
library(Dowd)
poziomUfnosci <- 0.95
wig30$Christofersen_H99 <- NA
library(segMGarch)
wig30$TL_H99 <- NA
iloscWierszy_wig30 <- nrow(wig30)
max_wig30 = iloscWierszy_wig30-t

for (i in c(1:max_wig30)){
  fragmentDanych_wig30 <- wig30[i:(t+i-1),]
  kw_99_H_wig30 = quantile(fragmentDanych_wig30$stopa_zwrotu, 0.99)
  christoffersen_test = ChristoffersenBacktestForUnconditionalCoverage(fragmentDanych_wig30$stopa_zwrotu,kw_99_H_wig30,poziomUfnosci)
  wig30$Christofersen_H99[t+i] <- christoffersen_test
  tl_test <- TL(fragmentDanych_wig30$stopa_zwrotu,VaR =kw_99_H_wig30,VaR_level =  poziomUfnosci)$color
  wig30$TL_H99[t+i] <- tl_test
}

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierz_wig30_H99<- wig30$Christofersen_H99 > 0.05
macierz_wig30_H99 <- na.omit(macierz_wig30_H99)
przyjeteH0_wig30_H99 <- sum(ifelse(macierz_wig30_H99==TRUE,1,0))
odrzuconoH0_wig30_H99 <- sum(ifelse(macierz_wig30_H99==FALSE,1,0))

macierzTL_wig30_H99<- wig30$TL_H99 
macierzTL_wig30_H99 <- na.omit(macierzTL_wig30_H99)
strefaCzerwonaTL_H99_wig30 <- sum(ifelse(macierzTL_wig30_H99=="red",1,0))
strefaZolta_H99_wig30 <- sum(ifelse(macierzTL_wig30_H99=="yellow",1,0))
strefaZielona_H99_wig30 <- sum(ifelse(macierzTL_wig30_H99=="green",1,0))

# Przyjeto H0
przyjeteH0_wig30_H99
# Odrzucono H0
odrzuconoH0_wig30_H99 

#--------------------------------------------------------------------------------------------------------

# Metoda Historyczna z wagami - VaR 99%
library(Dowd)
poziomUfnosci <- 0.95
wig30$Christofersen_HW99 <- NA
library(dplyr)
t = 250
iloscWierszy_wig30 <- nrow(wig30)
max_wig30 = iloscWierszy_wig30-t
q <- 0.995
library(segMGarch)
wig30$TL_HW99 <- NA


for (i in c(1:max_wig30)){
  fragmentDanych_wig30 <- wig30[i:(t+i-1),]
  fragmentDanych_wig30$prawdopodobienstwa <- NA
  
  for (j in c(1:nrow(fragmentDanych_wig30))) {
    wykladnik <- t - j
    licznik <- q**wykladnik*(1-q)
    mianownik <- 1-q**t
    wartosci <- licznik/mianownik 
    fragmentDanych_wig30$prawdopodobienstwa[j] <- wartosci
  }
  #sortowanie stopy zwrotu (do dystrybuanty)
  posortowaneDane <- fragmentDanych_wig30[order(fragmentDanych_wig30$stopa_zwrotu),]
  
  
  #dystrybuanta
  posortowaneDane$dystrybuanta <- NA
  for (j in c(1:nrow(posortowaneDane))) {
    doDystrybuanty <- sum(posortowaneDane[1:j,"prawdopodobienstwa"])
    posortowaneDane$dystrybuanta[j] <- doDystrybuanty
  }
  
  # VaR 99% - kwantyl brany z dystrybuanty
  wiecejNizkwantyl99 <- posortowaneDane %>% filter(dystrybuanta >="0.99")
  kw_99_HW_wig30 <- wiecejNizkwantyl99[1,"stopa_zwrotu"]
  wig30$met_historycznaWAGI_var99_250obs[t+i] <- kw_99_HW_wig30
  christoffersen_test = ChristoffersenBacktestForUnconditionalCoverage(fragmentDanych_wig30$stopa_zwrotu,kw_99_HW_wig30,poziomUfnosci)
  wig30$Christofersen_HW99[t+i] <- christoffersen_test
  tl_test <- TL(fragmentDanych_wig30$stopa_zwrotu,VaR =kw_99_HW_wig30,VaR_level =  poziomUfnosci)$color
  wig30$TL_HW99[t+i] <- tl_test
}

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierz_wig30_HW99<- wig30$Christofersen_HW99 > 0.05
macierz_wig30_HW99 <- na.omit(macierz_wig30_HW99)
przyjeteH0_wig30_HW99 <- sum(ifelse(macierz_wig30_HW99==TRUE,1,0))
odrzuconoH0_wig30_HW99 <- sum(ifelse(macierz_wig30_HW99==FALSE,1,0))

macierzTL_wig30_HW99<- wig30$TL_HW99 
macierzTL_wig30_HW99 <- na.omit(macierzTL_wig30_HW99)
strefaCzerwonaTL_HW99_wig30 <- sum(ifelse(macierzTL_wig30_HW99=="red",1,0))
strefaZolta_HW99_wig30 <- sum(ifelse(macierzTL_wig30_HW99=="yellow",1,0))
strefaZielona_HW99_wig30 <- sum(ifelse(macierzTL_wig30_HW99=="green",1,0))

# Przyjeto H0
przyjeteH0_wig30_HW99
# Odrzucono H0
odrzuconoH0_wig30_HW99 

#--------------------------------------------------------------------------------------------------------

# Metoda Monte Carlo - VaR 99%


library(Dowd)
poziomUfnosci <- 0.95
wig30$Christofersen_MC99 <- NA
t = 250
iloscWierszy_wig30 <- nrow(wig30)
max_wig30 = iloscWierszy_wig30-t
library(segMGarch)
wig30$TL_MC99 <- NA


for (i in c(1:max_wig30)){
  #VaR
  fragmentDanych_wig30 <- wig30[i:(t+i-1),]
  fragmentDanychh_wig30 <- rnorm(t,mean = mean(fragmentDanych_wig30$stopa_zwrotu),sd=sd(fragmentDanych_wig30$stopa_zwrotu))
  kw_99_MC_wig30 = quantile(fragmentDanychh_wig30, 0.99)
  wig30$monteCarlo_var99[t+i] <- kw_99_MC_wig30
  christoffersen_test = ChristoffersenBacktestForUnconditionalCoverage(fragmentDanych_wig30$stopa_zwrotu,kw_99_MC_wig30,poziomUfnosci)
  wig30$Christofersen_MC99[t+i] <- christoffersen_test
  tl_test <- TL(fragmentDanych_wig30$stopa_zwrotu,VaR =kw_99_MC_wig30,VaR_level =  poziomUfnosci)$color
  wig30$TL_MC99[t+i] <- tl_test
}

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierz_wig30_MC99<- wig30$Christofersen_MC99 > 0.05
macierz_wig30_MC99 <- na.omit(macierz_wig30_MC99)
przyjeteH0_wig30_MC99 <- sum(ifelse(macierz_wig30_MC99==TRUE,1,0))
odrzuconoH0_wig30_MC99 <- sum(ifelse(macierz_wig30_MC99==FALSE,1,0))

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierzTL_wig30_MC99<- wig30$TL_MC99 
macierzTL_wig30_MC99 <- na.omit(macierzTL_wig30_MC99)
strefaCzerwonaTL_MC99_wig30 <- sum(ifelse(macierzTL_wig30_MC99=="red",1,0))
strefaZolta_MC99_wig30 <- sum(ifelse(macierzTL_wig30_MC99=="yellow",1,0))
strefaZielona_MC99_wig30 <- sum(ifelse(macierzTL_wig30_MC99=="green",1,0))

# Przyjeto H0
przyjeteH0_wig30_MC99
# Odrzucono H0
odrzuconoH0_wig30_MC99 







# Metoda Historyczna - VaR 95%


library(Dowd)
poziomUfnosci <- 0.95
wig30$Christofersen_H95 <- NA
library(segMGarch)
wig30$TL_H95 <- NA
iloscWierszy_wig30 <- nrow(wig30)
max_wig30 = iloscWierszy_wig30-t

for (i in c(1:max_wig30)){
  
  fragmentDanych_wig30 <- wig30[i:(t+i-1),]
  kw_95_H_wig30 = quantile(fragmentDanych_wig30$stopa_zwrotu, 0.95)
  christoffersen_test = ChristoffersenBacktestForUnconditionalCoverage(fragmentDanych_wig30$stopa_zwrotu,kw_95_H_wig30,poziomUfnosci)
  wig30$Christofersen_H95[t+i] <- christoffersen_test
  tl_test <- TL(fragmentDanych_wig30$stopa_zwrotu,VaR =kw_95_H_wig30,VaR_level =  poziomUfnosci)$color
  wig30$TL_H95[t+i] <- tl_test
}

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierz_wig30_H95<- wig30$Christofersen_H95 > 0.05
macierz_wig30_H95 <- na.omit(macierz_wig30_H95)
przyjeteH0_wig30_H95 <- sum(ifelse(macierz_wig30_H95==TRUE,1,0))
odrzuconoH0_wig30_H95 <- sum(ifelse(macierz_wig30_H95==FALSE,1,0))

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierzTL_wig30_H95<- wig30$TL_H95 
macierzTL_wig30_H95 <- na.omit(macierzTL_wig30_H95)
strefaCzerwonaTL_H95_wig30 <- sum(ifelse(macierzTL_wig30_H95=="red",1,0))
strefaZolta_H95_wig30 <- sum(ifelse(macierzTL_wig30_H95=="yellow",1,0))
strefaZielona_H95_wig30 <- sum(ifelse(macierzTL_wig30_H95=="green",1,0))

# Przyjeto H0
przyjeteH0_wig30_H95
# Odrzucono H0
odrzuconoH0_wig30_H95 

#--------------------------------------------------------------------------------------------------------

# Metoda Historyczna z wagami - VaR 95%
library(Dowd)
poziomUfnosci <- 0.95
wig30$Christofersen_HW95 <- NA
library(dplyr)
iloscWierszy_wig30 <- nrow(wig30)
max_wig30 = iloscWierszy_wig30-t
t = 250
q <- 0.995
library(segMGarch)
wig30$TL_HW95 <- NA


for (i in c(1:max_wig30)){
  fragmentDanych_wig30 <- wig30[i:(t+i-1),]
  fragmentDanych_wig30$prawdopodobienstwa <- NA
  
  for (j in c(1:nrow(fragmentDanych_wig30))) {
    wykladnik <- t - j
    licznik <- q**wykladnik*(1-q)
    mianownik <- 1-q**t
    wartosci <- licznik/mianownik 
    fragmentDanych_wig30$prawdopodobienstwa[j] <- wartosci
  }
  #sortowanie stopy zwrotu (do dystrybuanty)
  posortowaneDane <- fragmentDanych_wig30[order(fragmentDanych_wig30$stopa_zwrotu),]
  
  
  #dystrybuanta
  posortowaneDane$dystrybuanta <- NA
  for (j in c(1:nrow(posortowaneDane))) {
    doDystrybuanty <- sum(posortowaneDane[1:j,"prawdopodobienstwa"])
    posortowaneDane$dystrybuanta[j] <- doDystrybuanty
  }
  
  # VaR 95% - kwantyl brany z dystrybuanty
  wiecejNizkwantyl95 <- posortowaneDane %>% filter(dystrybuanta >="0.95")
  kw_95_HW_wig30 <- wiecejNizkwantyl95[1,"stopa_zwrotu"]
  wig30$met_historycznaWAGI_var95_250obs[t+i] <- kw_95_HW_wig30
  christoffersen_test = ChristoffersenBacktestForUnconditionalCoverage(fragmentDanych_wig30$stopa_zwrotu,kw_95_HW_wig30,poziomUfnosci)
  wig30$Christofersen_HW95[t+i] <- christoffersen_test
  tl_test <- TL(fragmentDanych_wig30$stopa_zwrotu,VaR =kw_95_HW_wig30,VaR_level =  poziomUfnosci)$color
  wig30$TL_HW95[t+i] <- tl_test
  
}

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierz_wig30_HW95<- wig30$Christofersen_HW95 > 0.05
macierz_wig30_HW95 <- na.omit(macierz_wig30_HW95)
przyjeteH0_wig30_HW95 <- sum(ifelse(macierz_wig30_HW95==TRUE,1,0))
odrzuconoH0_wig30_HW95 <- sum(ifelse(macierz_wig30_HW95==FALSE,1,0))

macierzTL_wig30_HW95<- wig30$TL_HW95 
macierzTL_wig30_HW95 <- na.omit(macierzTL_wig30_HW95)
strefaCzerwonaTL_HW95_wig30 <- sum(ifelse(macierzTL_wig30_HW95=="red",1,0))
strefaZolta_HW95_wig30 <- sum(ifelse(macierzTL_wig30_HW95=="yellow",1,0))
strefaZielona_HW95_wig30 <- sum(ifelse(macierzTL_wig30_HW95=="green",1,0))

# Przyjeto H0
przyjeteH0_wig30_HW95
# Odrzucono H0
odrzuconoH0_wig30_HW95 

#--------------------------------------------------------------------------------------------------------

# Metoda Monte Carlo - VaR 95%


library(Dowd)
poziomUfnosci <- 0.95
wig30$Christofersen_MC95 <- NA
t = 250
iloscWierszy_wig30 <- nrow(wig30)
max_wig30 = iloscWierszy_wig30-t
library(segMGarch)
wig30$TL_MC95 <- NA


for (i in c(1:max_wig30)){
  #VaR
  fragmentDanych_wig30 <- wig30[i:(t+i-1),]
  fragmentDanychh_wig30 <- rnorm(t,mean = mean(fragmentDanych_wig30$stopa_zwrotu),sd=sd(fragmentDanych_wig30$stopa_zwrotu))
  kw_95_MC_wig30 = quantile(fragmentDanychh_wig30, 0.95)
  wig30$monteCarlo_var95[t+i] <- kw_95_MC_wig30
  christoffersen_test = ChristoffersenBacktestForUnconditionalCoverage(fragmentDanych_wig30$stopa_zwrotu,kw_95_MC_wig30,poziomUfnosci)
  wig30$Christofersen_MC95[t+i] <- christoffersen_test
  tl_test <- TL(fragmentDanych_wig30$stopa_zwrotu,VaR =kw_95_MC_wig30,VaR_level =  poziomUfnosci)$color
  wig30$TL_MC95[t+i] <- tl_test

}

# zliczanie gdzie hipoteza H0 zostala przyjeta (nieodrzucona) czyli VaR jest OK 
macierz_wig30_MC95<- wig30$Christofersen_MC95 > 0.05
macierz_wig30_MC95 <- na.omit(macierz_wig30_MC95)
przyjeteH0_wig30_MC95 <- sum(ifelse(macierz_wig30_MC95==TRUE,1,0))
odrzuconoH0_wig30_MC95 <- sum(ifelse(macierz_wig30_MC95==FALSE,1,0))

macierzTL_wig30_MC95<- wig30$TL_MC95 
macierzTL_wig30_MC95 <- na.omit(macierzTL_wig30_MC95)
strefaCzerwonaTL_MC95_wig30 <- sum(ifelse(macierzTL_wig30_MC95=="red",1,0))
strefaZolta_MC95_wig30 <- sum(ifelse(macierzTL_wig30_MC95=="yellow",1,0))
strefaZielona_MC95_wig30 <- sum(ifelse(macierzTL_wig30_MC95=="green",1,0))

# Przyjeto H0
przyjeteH0_wig30_MC95
# Odrzucono H0
odrzuconoH0_wig30_MC95 












## *Francja (CAC 40)* VAR 
tabelka_cac_VAR1 <- c(przyjeteH0_cac_H99,przyjeteH0_cac_HW99,przyjeteH0_cac_MC99)
tabelka_cac_VAR2 <- c(odrzuconoH0_cac_H99,odrzuconoH0_cac_HW99,odrzuconoH0_cac_MC99)
tabelka_cac_VAR3 <- c(przyjeteH0_cac_H95,przyjeteH0_cac_HW95,przyjeteH0_cac_MC95)
tabelka_cac_VAR4 <- c(odrzuconoH0_cac_H95,odrzuconoH0_cac_HW95,odrzuconoH0_cac_MC95)
tabelka_cac_VAR<- data.frame(tabelka_cac_VAR1,tabelka_cac_VAR2,tabelka_cac_VAR3,tabelka_cac_VAR4)
rownames(tabelka_cac_VAR) <- c("Metoda Historyczna","Metoda Historyczna z wagami","Metoda Monte Carlo")
colnames(tabelka_cac_VAR) <- c("VaR 99% poprawne","VaR 99% niepoprawne","VaR 95% poprawne","VaR 95% niepoprawne")
tabelka_cac_VAR





## *Ameryka (S&P 500)* VAR 
tabelka_spx_VAR1 <- c(przyjeteH0_spx_H99,przyjeteH0_spx_HW99,przyjeteH0_spx_MC99)
tabelka_spx_VAR2 <- c(odrzuconoH0_spx_H99,odrzuconoH0_spx_HW99,odrzuconoH0_spx_MC99)
tabelka_spx_VAR3 <- c(przyjeteH0_spx_H95,przyjeteH0_spx_HW95,przyjeteH0_spx_MC95)
tabelka_spx_VAR4 <- c(odrzuconoH0_spx_H95,odrzuconoH0_spx_HW95,odrzuconoH0_spx_MC95)
tabelka_spx_VAR<- data.frame(tabelka_spx_VAR1,tabelka_spx_VAR2,tabelka_spx_VAR3,tabelka_spx_VAR4)
rownames(tabelka_spx_VAR) <- c("Metoda Historyczna","Metoda Historyczna z wagami","Metoda Monte Carlo")
colnames(tabelka_spx_VAR) <- c("VaR 99% poprawne","VaR 99% niepoprawne","VaR 95% poprawne","VaR 95% niepoprawne")
tabelka_spx_VAR





## *Polska (WIG 30)* VAR
tabelka_wig30_VAR1 <- c(przyjeteH0_wig30_H99,przyjeteH0_wig30_HW99,przyjeteH0_wig30_MC99)
tabelka_wig30_VAR2 <- c(odrzuconoH0_wig30_H99,odrzuconoH0_wig30_HW99,odrzuconoH0_wig30_MC99)
tabelka_wig30_VAR3 <- c(przyjeteH0_wig30_H95,przyjeteH0_wig30_HW95,przyjeteH0_wig30_MC95)
tabelka_wig30_VAR4 <- c(odrzuconoH0_wig30_H95,odrzuconoH0_wig30_HW95,odrzuconoH0_wig30_MC95)
tabelka_wig30_VAR<- data.frame(tabelka_wig30_VAR1,tabelka_wig30_VAR2,tabelka_wig30_VAR3,tabelka_wig30_VAR4)
rownames(tabelka_wig30_VAR) <- c("Metoda Historyczna","Metoda Historyczna z wagami","Metoda Monte Carlo")
colnames(tabelka_wig30_VAR) <- c("VaR 99% poprawne","VaR 99% niepoprawne","VaR 95% poprawne","VaR 95% niepoprawne")
tabelka_wig30_VAR



# TL


## *Francja (CAC 40)* VAR 99%
tabelka_cac_VAR1TL99 <- c(strefaZielona_H99_cac,strefaZielona_HW99_cac,strefaZielona_MC99_cac)
tabelka_cac_VAR2TL99 <- c(strefaZolta_H99_cac,strefaZolta_HW99_cac,strefaZolta_MC99_cac)
tabelka_cac_VAR3TL99 <- c(strefaCzerwonaTL_H99_cac,strefaCzerwonaTL_HW99_cac,strefaCzerwonaTL_MC99_cac)
tabelka_cac_VARTL99<- data.frame(tabelka_cac_VAR1TL99,tabelka_cac_VAR2TL99,tabelka_cac_VAR3TL99)
rownames(tabelka_cac_VARTL99) <- c("Metoda Historyczna","Metoda Historyczna z wagami","Metoda Monte Carlo")
colnames(tabelka_cac_VARTL99) <- c("Strefa Zielona","Strefa Zolta","Strefa Czerwona")
tabelka_cac_VARTL99

## *Francja (CAC 40)* VAR 95%
tabelka_cac_VAR1TL95 <- c(strefaZielona_H95_cac,strefaZielona_HW95_cac,strefaZielona_MC95_cac)
tabelka_cac_VAR2TL95 <- c(strefaZolta_H95_cac,strefaZolta_HW95_cac,strefaZolta_MC95_cac)
tabelka_cac_VAR3TL95 <- c(strefaCzerwonaTL_H95_cac,strefaCzerwonaTL_HW95_cac,strefaCzerwonaTL_MC95_cac)
tabelka_cac_VARTL95<- data.frame(tabelka_cac_VAR1TL95,tabelka_cac_VAR2TL95,tabelka_cac_VAR3TL95)
rownames(tabelka_cac_VARTL95) <- c("Metoda Historyczna","Metoda Historyczna z wagami","Metoda Monte Carlo")
colnames(tabelka_cac_VARTL95) <- c("Strefa Zielona","Strefa Zolta","Strefa Czerwona")
tabelka_cac_VARTL95


## *Ameryka (S&P 500)* VAR 99%
tabelka_spx_VAR1TL99 <- c(strefaZielona_H99_spx,strefaZielona_HW99_spx,strefaZielona_MC99_spx)
tabelka_spx_VAR2TL99 <- c(strefaZolta_H99_spx,strefaZolta_HW99_spx,strefaZolta_MC99_spx)
tabelka_spx_VAR3TL99 <- c(strefaCzerwonaTL_H99_spx,strefaCzerwonaTL_HW99_spx,strefaCzerwonaTL_MC99_spx)
tabelka_spx_VARTL99<- data.frame(tabelka_spx_VAR1TL99,tabelka_spx_VAR2TL99,tabelka_spx_VAR3TL99)
rownames(tabelka_spx_VARTL99) <- c("Metoda Historyczna","Metoda Historyczna z wagami","Metoda Monte Carlo")
colnames(tabelka_spx_VARTL99) <- c("Strefa Zielona","Strefa Zolta","Strefa Czerwona")
tabelka_spx_VARTL99

## *Ameryka (S&P 500)* VAR 95%
tabelka_spx_VAR1TL95 <- c(strefaZielona_H95_spx,strefaZielona_HW95_spx,strefaZielona_MC95_spx)
tabelka_spx_VAR2TL95 <- c(strefaZolta_H95_spx,strefaZolta_HW95_spx,strefaZolta_MC95_spx)
tabelka_spx_VAR3TL95 <- c(strefaCzerwonaTL_H95_spx,strefaCzerwonaTL_HW95_spx,strefaCzerwonaTL_MC95_spx)
tabelka_spx_VARTL95<- data.frame(tabelka_spx_VAR1TL95,tabelka_spx_VAR2TL95,tabelka_spx_VAR3TL95)
rownames(tabelka_spx_VARTL95) <- c("Metoda Historyczna","Metoda Historyczna z wagami","Metoda Monte Carlo")
colnames(tabelka_spx_VARTL95) <- c("Strefa Zielona","Strefa Zolta","Strefa Czerwona")
tabelka_spx_VARTL95





## *Polska (WIG 30)* VAR 99%
tabelka_wig30_VAR1TL99 <- c(strefaZielona_H99_wig30,strefaZielona_HW99_wig30,strefaZielona_MC99_wig30)
tabelka_wig30_VAR2TL99 <- c(strefaZolta_H99_wig30,strefaZolta_HW99_wig30,strefaZolta_MC99_wig30)
tabelka_wig30_VAR3TL99 <- c(strefaCzerwonaTL_H99_wig30,strefaCzerwonaTL_HW99_wig30,strefaCzerwonaTL_MC99_wig30)
tabelka_wig30_VARTL99<- data.frame(tabelka_wig30_VAR1TL99,tabelka_wig30_VAR2TL99,tabelka_wig30_VAR3TL99)
rownames(tabelka_wig30_VARTL99) <- c("Metoda Historyczna","Metoda Historyczna z wagami","Metoda Monte Carlo")
colnames(tabelka_wig30_VARTL99) <- c("Strefa Zielona","Strefa Zolta","Strefa Czerwona")
tabelka_wig30_VARTL99

## *Polska (WIG 30)* VAR 95%
tabelka_wig30_VAR1TL95 <- c(strefaZielona_H95_wig30,strefaZielona_HW95_wig30,strefaZielona_MC95_wig30)
tabelka_wig30_VAR2TL95 <- c(strefaZolta_H95_wig30,strefaZolta_HW95_wig30,strefaZolta_MC95_wig30)
tabelka_wig30_VAR3TL95 <- c(strefaCzerwonaTL_H95_wig30,strefaCzerwonaTL_HW95_wig30,strefaCzerwonaTL_MC95_wig30)
tabelka_wig30_VARTL95<- data.frame(tabelka_wig30_VAR1TL95,tabelka_wig30_VAR2TL95,tabelka_wig30_VAR3TL95)
rownames(tabelka_wig30_VARTL95) <- c("Metoda Historyczna","Metoda Historyczna z wagami","Metoda Monte Carlo")
colnames(tabelka_wig30_VARTL95) <- c("Strefa Zielona","Strefa Zolta","Strefa Czerwona")
tabelka_wig30_VARTL95



# PODSUMOWANIE



#CAC 40

tab91 <- c(round(strefaZielona_H95_cac/(length(macierzTL_cac_H95)),2),round(strefaCzerwonaTL_H95_cac/(length(macierzTL_cac_H95)),2),
           round(przyjeteH0_cac_H95/(length(macierz_cac_H95)),2),round(odrzuconoH0_cac_H95/(length(macierz_cac_H95)),2),
           round(strefaZielona_H99_cac/(length(macierzTL_cac_H99)),2),round(strefaCzerwonaTL_H99_cac/(length(macierzTL_cac_H99)),2),
           round(przyjeteH0_cac_H99/(length(macierz_cac_H99)),2),round(odrzuconoH0_cac_H99/(length(macierz_cac_H99)),2)
)
tab92 <- c(round(strefaZielona_HW95_cac/(length(macierzTL_cac_HW95)),2),round(strefaCzerwonaTL_HW95_cac/(length(macierzTL_cac_HW95)),2),
           round(przyjeteH0_cac_HW95/(length(macierz_cac_HW95)),2),round(odrzuconoH0_cac_HW95/(length(macierz_cac_HW95)),2),
           round(strefaZielona_HW99_cac/(length(macierzTL_cac_HW99)),2),round(strefaCzerwonaTL_HW99_cac/(length(macierzTL_cac_HW99)),2),
           round(przyjeteH0_cac_HW99/(length(macierz_cac_HW99)),2),round(odrzuconoH0_cac_HW99/(length(macierz_cac_HW99)),2)
)
tab93 <- c(round(strefaZielona_MC95_cac/(length(macierzTL_cac_MC95)),2),round(strefaCzerwonaTL_MC95_cac/(length(macierzTL_cac_MC95)),2),
           round(przyjeteH0_cac_MC95/(length(macierz_cac_MC95)),2),round(odrzuconoH0_cac_MC95/(length(macierz_cac_MC95)),2),
           round(strefaZielona_MC99_cac/(length(macierzTL_cac_MC99)),2),round(strefaCzerwonaTL_MC99_cac/(length(macierzTL_cac_MC99)),2),
           round(przyjeteH0_cac_MC99/(length(macierz_cac_MC99)),2),round(odrzuconoH0_cac_MC99/(length(macierz_cac_MC99)),2)
)

tabela_CAC<- data.frame(tab91,tab92,tab93)
colnames(tabela_CAC) <- c("Metoda Historyczna","Metoda Historyczna z wagami","Metoda Monte Carlo")
rownames(tabela_CAC) <- c("VaR 95% poprawne (TL)","VaR 95% niepoprawne (TL)",
                                     "VaR 95% poprawne (Christoffersen)","VaR 95% niepoprawne (Christoffersen)",
                                     "VaR 99% poprawne (TL)","VaR 99% niepoprawne (TL)",
                                     "VaR 99% poprawne (Christoffersen)","VaR 99% niepoprawne (Christoffersen)")


tabela_CAC



# S&P 500

tab81 <- c(round(strefaZielona_H95_spx/(length(macierzTL_spx_H95)),2),round(strefaCzerwonaTL_H95_spx/(length(macierzTL_spx_H95)),2),
           round(przyjeteH0_spx_H95/(length(macierz_spx_H95)),2),round(odrzuconoH0_spx_H95/(length(macierz_spx_H95)),2),
           round(strefaZielona_H99_spx/(length(macierzTL_spx_H99)),2),round(strefaCzerwonaTL_H99_spx/(length(macierzTL_spx_H99)),2),
           round(przyjeteH0_spx_H99/(length(macierz_spx_H99)),2),round(odrzuconoH0_spx_H99/(length(macierz_spx_H99)),2)
)
tab82 <- c(round(strefaZielona_HW95_spx/(length(macierzTL_spx_HW95)),2),round(strefaCzerwonaTL_HW95_spx/(length(macierzTL_spx_HW95)),2),
           round(przyjeteH0_spx_HW95/(length(macierz_spx_HW95)),2),round(odrzuconoH0_spx_HW95/(length(macierz_spx_HW95)),2),
           round(strefaZielona_HW99_spx/(length(macierzTL_spx_HW99)),2),round(strefaCzerwonaTL_HW99_spx/(length(macierzTL_spx_HW99)),2),
           round(przyjeteH0_spx_HW99/(length(macierz_spx_HW99)),2),round(odrzuconoH0_spx_HW99/(length(macierz_spx_HW99)),2)
)
tab83 <- c(round(strefaZielona_MC95_spx/(length(macierzTL_spx_MC95)),2),round(strefaCzerwonaTL_MC95_spx/(length(macierzTL_spx_MC95)),2),
           round(przyjeteH0_spx_MC95/(length(macierz_spx_MC95)),2),round(odrzuconoH0_spx_MC95/(length(macierz_spx_MC95)),2),
           round(strefaZielona_MC99_spx/(length(macierzTL_spx_MC99)),2),round(strefaCzerwonaTL_MC99_spx/(length(macierzTL_spx_MC99)),2),
           round(przyjeteH0_spx_MC99/(length(macierz_spx_MC99)),2),round(odrzuconoH0_spx_MC99/(length(macierz_spx_MC99)),2)
)

tabela_SPX<- data.frame(tab81,tab82,tab83)
colnames(tabela_SPX) <- c("Metoda Historyczna","Metoda Historyczna z wagami","Metoda Monte Carlo")
rownames(tabela_SPX) <- c("VaR 95% poprawne (TL)","VaR 95% niepoprawne (TL)",
                          "VaR 95% poprawne (Christoffersen)","VaR 95% niepoprawne (Christoffersen)",
                          "VaR 99% poprawne (TL)","VaR 99% niepoprawne (TL)",
                          "VaR 99% poprawne (Christoffersen)","VaR 99% niepoprawne (Christoffersen)")


tabela_SPX



# WIG 30

tab71 <- c(round(strefaZielona_H95_wig30/(length(macierzTL_wig30_H95)),2),round(strefaCzerwonaTL_H95_wig30/(length(macierzTL_wig30_H95)),2),
           round(przyjeteH0_wig30_H95/(length(macierz_wig30_H95)),2),round(odrzuconoH0_wig30_H95/(length(macierz_wig30_H95)),2),
           round(strefaZielona_H99_wig30/(length(macierzTL_wig30_H99)),2),round(strefaCzerwonaTL_H99_wig30/(length(macierzTL_wig30_H99)),2),
           round(przyjeteH0_wig30_H99/(length(macierz_wig30_H99)),2),round(odrzuconoH0_wig30_H99/(length(macierz_wig30_H99)),2)
)
tab72 <- c(round(strefaZielona_HW95_wig30/(length(macierzTL_wig30_HW95)),2),round(strefaCzerwonaTL_HW95_wig30/(length(macierzTL_wig30_HW95)),2),
           round(przyjeteH0_wig30_HW95/(length(macierz_wig30_HW95)),2),round(odrzuconoH0_wig30_HW95/(length(macierz_wig30_HW95)),2),
           round(strefaZielona_HW99_wig30/(length(macierzTL_wig30_HW99)),2),round(strefaCzerwonaTL_HW99_wig30/(length(macierzTL_wig30_HW99)),2),
           round(przyjeteH0_wig30_HW99/(length(macierz_wig30_HW99)),2),round(odrzuconoH0_wig30_HW99/(length(macierz_wig30_HW99)),2)
)
tab73 <- c(round(strefaZielona_MC95_wig30/(length(macierzTL_wig30_MC95)),2),round(strefaCzerwonaTL_MC95_wig30/(length(macierzTL_wig30_MC95)),2),
           round(przyjeteH0_wig30_MC95/(length(macierz_wig30_MC95)),2),round(odrzuconoH0_wig30_MC95/(length(macierz_wig30_MC95)),2),
           round(strefaZielona_MC99_wig30/(length(macierzTL_wig30_MC99)),2),round(strefaCzerwonaTL_MC99_wig30/(length(macierzTL_wig30_MC99)),2),
           round(przyjeteH0_wig30_MC99/(length(macierz_wig30_MC99)),2),round(odrzuconoH0_wig30_MC99/(length(macierz_wig30_MC99)),2)
)

tabela_WIG30<- data.frame(tab71,tab72,tab73)
colnames(tabela_WIG30) <- c("Metoda Historyczna","Metoda Historyczna z wagami","Metoda Monte Carlo")
rownames(tabela_WIG30) <- c("VaR 95% poprawne (TL)","VaR 95% niepoprawne (TL)",
                          "VaR 95% poprawne (Christoffersen)","VaR 95% niepoprawne (Christoffersen)",
                          "VaR 99% poprawne (TL)","VaR 99% niepoprawne (TL)",
                          "VaR 99% poprawne (Christoffersen)","VaR 99% niepoprawne (Christoffersen)")


tabela_WIG30



# Wed³ug Bazylei model VaR jest uwa¿any za wa¿ny, jeœli skumulowane prawdopodobieñstwo 
#zaobserwowania do n_f awarii jest mniejsze ni¿ 0,95 (zielona strefa) w rozk³adzie 
# dwumianowym z n (wielkoœæ próbki) i poziomem Var jako parametrami. Jeœli skumulowane 
#prawdopodobieñstwo wynosi od 0,95 do 0,9999, model VaR znajduje siê w 
#strefie ¿ó³tej. W przeciwnym razie (>0,9999) model VaR znajduje siê w czerwonej strefie.














