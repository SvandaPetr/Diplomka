#----------------------------------------------------------#
#
#
#                   Diplomova prace
#
# Efektivita prenosu pylu jakozto funkce odberu a depozice pylu
#
#
#                       P. Svanda
#                         2024
#
#----------------------------------------------------------#
  
#Toto je skript zpracování a analýzy dat pro diplomovu práci
#Pri vyuziti balicku, bude snahout vzdy zminit i z jakeho balicku je funkce vyuzivana napr. here::("x")


#----------------------------------------------------------#
# 0. Priprava skriptu ----
#----------------------------------------------------------#

#skript jede rovnou na package {renv}, seznam packages bude prilozen ve slozce R.

renv::install("here")
renv::install("tidyverse")  
renv::install("readxl")
renv::install("usethis")
renv::install("waffle")

library("waffle")
library("here")  
library("tidyverse")  
library("readxl")
library("usethis")

#----------------------------------------------------------#
# 0.1 Sparovani s Githubem ----
#----------------------------------------------------------#


usethis::use_git()
usethis::use_github()


#----------------------------------------------------------#
# 1. Vlozeni dat ----
#----------------------------------------------------------#

here::here("Data","Handrkov.raw.xlsx")

handrkov <- readxl::read_xlsx("Data/Handrkov_raw.xlsx", 
                              na = c("", NA), 
                              col_types = c("text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "numeric", "skip", "numeric", "date", "text", "numeric"),
                              col_names = c("id", "druh_kytky", "typ", "klec_pytlik", "mnozstvi_roztoku", "mnozstvi_pylu_konspecificky", "mnozstvi_pylu_celkem", "mnozstvi_pylu_heterospecificky", "vyska_rostliny", "barva_pytliku", "opylovac", "chovani", "delka_navstevy", "zacatek_konec_cas", "pocet_navstev_hodinu", "cas_navstevy", "pohlavi", "rok"),
                              skip = 1
                              )


#----------------------------------------------------------#
# 2. Uprava dat ----
#----------------------------------------------------------#


## 2.1 Cisteni datasetu, sjednoceni jmen ----
#----------------------------------------------------------#

bom <- which(handrkov$opylovac == "Bom.Lap.")
handrkov$opylovac[bom] <- "bom.lap."
syl <- which(handrkov$opylovac == "bom.sylvarum")
handrkov$opylovac[syl] <- "bom.syl."
arb <- which(handrkov$opylovac == "Eri.Arb.")
handrkov$opylovac[arb] <- "eri.arb."
intricaria <- which(handrkov$opylovac == "intricaria")
handrkov$opylovac[intricaria] <- "eri.intr."
sca <- which(handrkov$opylovac == "scaera")
handrkov$opylovac[sca] <- "scaeva"

interuptus <- which(handrkov$opylovac == "interuptus")
handrkov$opylovac[interuptus] <- "eri.inte."


## 2.2 nove promenne pro rok, den, hodinu a minutu ----
#----------------------------------------------------------#

handrkov$rok <-  substring(handrkov$cas_navstevy,1,4)
handrkov$den <-  substring(handrkov$cas_navstevy,9,10)
handrkov$casH <- substring(handrkov$cas_navstevy,12,13)
handrkov$casM <- substring(handrkov$cas_navstevy,15,16)


#----------------------------------------------------------#
# 3. Vizualizace dat ----
#----------------------------------------------------------#


## 3.1 subset tabulky  pro depozice ----
tabulka_depozice <- handrkov[handrkov$typ == "deposition",] #subset jen pro depozice
View(tabulka_depozice)


## 3.2 Mnozstvi deponovaneho pylu v zavislosti na druhu opylovace ----
#----------------------------------------------------------#

ggplot2::ggplot(
  data = tabulka_depozice,
  mapping = aes(x = opylovac, y = mnozstvi_pylu_konspecificky, color = opylovac)
) +
  geom_boxplot() +
  labs (
    title = "Mnozstvi deponovaneho pylu v zavislosti na druhu opylovace",
    x = "Druh opylovace",
    y = "mnozstvi konspecifickeho deponovaneho pylu"
  ) +
  geom_jitter(
    
  )


### 3.2.1 subset tabulky  pro tenax a bom.lap ----
#----------------------------------------------------------#

tabulka_depozice_tenax <- tabulka_depozice[tabulka_depozice$opylovac == "tenax",]
tabulka_depozice_bom <- tabulka_depozice[tabulka_depozice$opylovac == "bom.lap.",]
tabulka_depozice_tenax_bom <- rbind(tabulka_depozice_bom, tabulka_depozice_tenax)


### 3.2.2 Depozice jen pro tenax a bom.lap. ----
#----------------------------------------------------------#

ggplot2::ggplot(
  data = tabulka_depozice_tenax_bom,
  mapping = aes(x = opylovac, y = mnozstvi_pylu_konspecificky, color = opylovac)
) +
  geom_boxplot() +
  labs (
    title = "Mnozstvi deponovaneho pylu v zavislosti na druhu opylovace",
    x = "Druh opylovace",
    y = "mnozstvi konspecifickeho deponovaneho pylu"
  ) +
  geom_jitter(
    
  )


## 3.3 mnozstvi deponovaneho pylu podle pohlavi u tenax ----
#----------------------------------------------------------#

ggplot2::ggplot(
  data = tabulka_depozice_tenax,
  mapping = aes(x = pohlavi, y = mnozstvi_pylu_konspecificky, color = pohlavi)
) +
  geom_boxplot() +
  labs (
    title = "Mnozstvi deponovaneho pylu v zavislosti na pohlavi Eristalis tenax",
    x = "Druh opylovace",
    y = "mnozstvi konspecifickeho deponovaneho pylu"
  ) +
  geom_jitter(
    
  )

## 3.4 subset tabulky  pro chovani ----
#----------------------------------------------------------#

tabulka_depozice_pyl <- tabulka_depozice[tabulka_depozice$chovani == "pyl",]
tabulka_depozice_nektar <- tabulka_depozice[tabulka_depozice$chovani == "nektar",]
tabulka_depozice_pyl_nektar <- rbind(tabulka_depozice_pyl, tabulka_depozice_nektar)


## 3.5 mnozstvi deponovaneho pylu podle chovani ----
#----------------------------------------------------------#


ggplot2::ggplot(
  data = tabulka_depozice_pyl_nektar,
  mapping = aes(x = chovani, y = mnozstvi_pylu_konspecificky, color = chovani)
) +
  geom_boxplot() +
  labs (
    title = "Mnozstvi deponovaneho pylu v zavislosti na chovani",
    x = "Druh chovani",
    y = "mnozstvi konspecifickeho deponovaneho pylu"
  ) +
  geom_jitter(
    
  )


### 3.5.1 mnozstvi deponovaneho pylu podle chovani u eristalis tenax ----
#----------------------------------------------------------#

tabulka_depozice_pyl_nektar_tenax <- tabulka_depozice_pyl_nektar[tabulka_depozice_pyl_nektar$opylovac == "tenax",]


ggplot2::ggplot(
  data = tabulka_depozice_pyl_nektar_tenax,
  mapping = aes(x = chovani, y = mnozstvi_pylu_konspecificky, color = chovani)
) +
  geom_boxplot(na.rm = TRUE) +
  labs (
    title = "Mnozstvi deponovaneho pylu v zavislosti na chovani u eristalis tenax",
    x = "Druh chovani",
    y = "mnozstvi konspecifickeho deponovaneho pylu"
  ) +
  geom_jitter(
    
  )

### 3.6 mnozstvi deponovaneho pylu podle delky navstevy ----
#----------------------------------------------------------#


ggplot2::ggplot(
  data = tabulka_depozice,
  mapping = aes(x = delka_navstevy, y = mnozstvi_pylu_konspecificky, color = opylovac)
) +
  geom_point(na.rm = TRUE, size = 4) +
  labs (
    title = "Mnozstvi deponovaneho pylu v zavislosti na delce navstevy",
    x = "Druh chovani",
    y = "mnozstvi konspecifickeho deponovaneho pylu"
  ) 
 
 
### 3.6.1 mnozstvi deponovaneho pylu podle delky navstevy u tenax ----
#----------------------------------------------------------#

ggplot2::ggplot(
  data = tabulka_depozice_tenax,
  mapping = aes(x = delka_navstevy, y = mnozstvi_pylu_konspecificky, color = pohlavi)
) +
  geom_point(na.rm = TRUE, size = 4) +
  labs (
    title = "Mnozstvi deponovaneho pylu v zavislosti na delce navstevy",
    x = "Druh chovani",
    y = "mnozstvi konspecifickeho deponovaneho pylu"
  ) 


### 3.6.0 celkove spektrum opylovacu ----
#----------------------------------------------------------#
opylovaci <- handrkov$opylovac


waffle::waffle(x = opylovaci, rows = 8)


