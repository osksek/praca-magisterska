Sys.sleep(runif(1,30,90)) ### Liczba ofert wynajmu w przedziałach cenowych i pokoje dla województw ###

rm(list=ls())
install.packages("tidyverse")
install.packages("rvest")
install.packages("glue")
install.packages("stringr")
install.packages("lubridate")
install.packages("knitr")
install.packages("kableExtra")
install.packages("readr")
install.packages("writexlsx")

library(tidyverse)
library(rvest)
library(glue)
library(stringr)
library(lubridate)
library(knitr)
library(kableExtra)
library(readr)
library(writexl)

nazwy <- c('Województwo','Znalezione','Odfiltrowane','gg')


### SKRYPT  1 1 POKOJOWE DO 500 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach


Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp


Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub


Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_1pok500 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                            df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_1pok500                   



write_xlsx(x = oferty_woj_1pok500, path = "oferty_woj_1pok500.xlsx", col_names = TRUE)





### SKRYPT  1 1 POKOJOWE 500 - 1 tys. ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp


Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_1pok1000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)
oferty_woj_1pok1000                   



write_xlsx(x = oferty_woj_1pok1000, path = "oferty_woj_1pok1000.xlsx", col_names = TRUE)


### SKRYPT  1 1 POKOJOWE 1 - 1,5 tys. ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1001&search[filter-price_to]=1500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_1pok1500 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)
oferty_woj_1pok1500                   



write_xlsx(x = oferty_woj_1pok1500, path = "oferty_woj_1pok1500.xlsx", col_names = TRUE)



### SKRYPT  1 1 POKOJOWE 1,5 - 2 tys. ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub




Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_1pok2000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)
oferty_woj_1pok2000                   



write_xlsx(x = oferty_woj_1pok2000, path = "oferty_woj_1pok2000.xlsx", col_names = TRUE)




### SKRYPT  1 1 POKOJOWE 2 - 2,5 tys. ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_1pok2500 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)
oferty_woj_1pok2500                   



write_xlsx(x = oferty_woj_1pok2500, path = "oferty_woj_1pok2500.xlsx", col_names = TRUE)





### SKRYPT  1 1 POKOJOWE 2,5 - 3 tys. ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_1pok3000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)
oferty_woj_1pok3000                  



write_xlsx(x = oferty_woj_1pok3000, path = "oferty_woj_1pok3000.xlsx", col_names = TRUE)




### SKRYPT  1 1 POKOJOWE 3 - 3,5 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3001&search[filter-price_to]=3500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_1pok3500 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)
oferty_woj_1pok3500                  



write_xlsx(x = oferty_woj_1pok3500, path = "oferty_woj_1pok3500.xlsx", col_names = TRUE)




### SKRYPT  1 1 POKOJOWE 3,5 - 4 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_1pok4000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)
oferty_woj_1pok4000                  



write_xlsx(x = oferty_woj_1pok4000, path = "oferty_woj_1pok4000.xlsx", col_names = TRUE)




### SKRYPT  1 1 POKOJOWE 4 - 4,5 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4001&search[filter-price_to]=4500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub




Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_1pok4500 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)
oferty_woj_1pok4500                  



write_xlsx(x = oferty_woj_1pok4500, path = "oferty_woj_1pok4500.xlsx", col_names = TRUE)



### SKRYPT  1 1 POKOJOWE 4,5 - 5 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub




Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_1pok5000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)
oferty_woj_1pok5000                  



write_xlsx(x = oferty_woj_1pok5000, path = "oferty_woj_1pok5000.xlsx", col_names = TRUE)



### SKRYPT  1 1 POKOJOWE 5 - 6 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5001&search[filter-price_to]=6000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_1pok6000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)
oferty_woj_1pok6000                  



write_xlsx(x = oferty_woj_1pok6000, path = "oferty_woj_1pok6000.xlsx", col_names = TRUE)



### SKRYPT  1 1 POKOJOWE 6 - 7 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp


Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_1pok7000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)
oferty_woj_1pok7000                  



write_xlsx(x = oferty_woj_1pok7000, path = "oferty_woj_1pok7000.xlsx", col_names = TRUE)



### SKRYPT  1 1 POKOJOWE 7-8 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_1pok8000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)
oferty_woj_1pok8000                  



write_xlsx(x = oferty_woj_1pok8000, path = "oferty_woj_1pok8000.xlsx", col_names = TRUE)




### SKRYPT  1 1 POKOJOWE 8-9 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_1pok9000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)
oferty_woj_1pok9000                  



write_xlsx(x = oferty_woj_1pok9000, path = "oferty_woj_1pok9000.xlsx", col_names = TRUE)



### SKRYPT  1 1 POKOJOWE 9-10 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_1pok10000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                              df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)
oferty_woj_1pok10000                  



write_xlsx(x = oferty_woj_1pok10000, path = "oferty_woj_1pok10000.xlsx", col_names = TRUE)


### SKRYPT  1 1 POKOJOWE pow 10 tys. ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=1&search[filter-number_of_rooms_to]=1&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_1pok_pow_10000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                                   df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)
oferty_woj_1pok_pow_10000                  



write_xlsx(x = oferty_woj_1pok_pow_10000 , path = "oferty_woj_1pok_pow_10000.xlsx", col_names = TRUE)












### SKRYPT  1 2 POKOJOWE DO 500 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub




Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_2pok500 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                            df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_2pok500                   



write_xlsx(x = oferty_woj_2pok500, path = "oferty_woj_2pok500.xlsx", col_names = TRUE)





### SKRYPT  1 2 POKOJOWE 500 - 1 tys. ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp


Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub

Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_2pok1000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_2pok1000                   



write_xlsx(x = oferty_woj_2pok1000, path = "oferty_woj_2pok1000.xlsx", col_names = TRUE)


### SKRYPT  1 2 POKOJOWE 1 - 1,5 tys. ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1001&search[filter-price_to]=1500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp


Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_2pok1500 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_2pok1500                   



write_xlsx(x = oferty_woj_2pok1500, path = "oferty_woj_2pok1500.xlsx", col_names = TRUE)



### SKRYPT  1 2 POKOJOWE 1,5 - 2 tys. ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp


Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_2pok2000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_2pok2000                   



write_xlsx(x = oferty_woj_2pok2000, path = "oferty_woj_2pok2000.xlsx", col_names = TRUE)




### SKRYPT  1 2 POKOJOWE 2 - 2,5 tys. ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_2pok2500 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_2pok2500                   



write_xlsx(x = oferty_woj_2pok2500, path = "oferty_woj_2pok2500.xlsx", col_names = TRUE)





### SKRYPT  1 2 POKOJOWE 2,5 - 3 tys. ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp


Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_2pok3000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_2pok3000                  



write_xlsx(x = oferty_woj_2pok3000, path = "oferty_woj_2pok3000.xlsx", col_names = TRUE)




### SKRYPT  1 2 POKOJOWE 3 - 3,5 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3001&search[filter-price_to]=3500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_2pok3500 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_2pok3500                  



write_xlsx(x = oferty_woj_2pok3500, path = "oferty_woj_2pok3500.xlsx", col_names = TRUE)




### SKRYPT  1 2 POKOJOWE 3,5 - 4 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp


Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_2pok4000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_2pok4000                  



write_xlsx(x = oferty_woj_2pok4000, path = "oferty_woj_2pok4000.xlsx", col_names = TRUE)




### SKRYPT  1 2 POKOJOWE 4 - 4,5 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4001&search[filter-price_to]=4500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp


Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_2pok4500 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_2pok4500                  



write_xlsx(x = oferty_woj_2pok4500, path = "oferty_woj_2pok4500.xlsx", col_names = TRUE)



### SKRYPT  1 2 POKOJOWE 4,5 - 5 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_2pok5000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_2pok5000                  



write_xlsx(x = oferty_woj_2pok5000, path = "oferty_woj_2pok5000.xlsx", col_names = TRUE)



### SKRYPT  1 2 POKOJOWE 5 - 6 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5001&search[filter-price_to]=6000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_2pok6000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_2pok6000                  



write_xlsx(x = oferty_woj_2pok6000, path = "oferty_woj_2pok6000.xlsx", col_names = TRUE)



### SKRYPT  1 2 POKOJOWE 6 - 7 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub


Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_2pok7000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_2pok7000                  



write_xlsx(x = oferty_woj_2pok7000, path = "oferty_woj_2pok7000.xlsx", col_names = TRUE)



### SKRYPT  1 2 POKOJOWE 7-8 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_2pok8000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_2pok8000                  



write_xlsx(x = oferty_woj_2pok8000, path = "oferty_woj_2pok8000.xlsx", col_names = TRUE)




### SKRYPT  1 2 POKOJOWE 8-9 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_2pok9000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_2pok9000                  



write_xlsx(x = oferty_woj_2pok9000, path = "oferty_woj_2pok9000.xlsx", col_names = TRUE)



### SKRYPT  1 2 POKOJOWE 9-10 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_2pok10000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                              df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_2pok10000                  



write_xlsx(x = oferty_woj_2pok10000, path = "oferty_woj_2pok10000.xlsx", col_names = TRUE)


### SKRYPT  1 2 POKOJOWE pow 10 tys. ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=2&search[filter-number_of_rooms_to]=2&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_2pok_pow_10000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                                   df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_2pok_pow_10000                  



write_xlsx(x = oferty_woj_2pok_pow_10000 , path = "oferty_woj_2pok_pow_10000.xlsx", col_names = TRUE)
























### SKRYPT  1 3 pokojowe DO 500 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub


Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_3pok500 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                            df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_3pok500                   



write_xlsx(x = oferty_woj_3pok500, path = "oferty_woj_3pok500.xlsx", col_names = TRUE)





### SKRYPT  1 3 pokojowe 500 - 1 tys. ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_3pok1000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_3pok1000                   



write_xlsx(x = oferty_woj_3pok1000, path = "oferty_woj_3pok1000.xlsx", col_names = TRUE)


### SKRYPT  1 3 pokojowe 1 - 1,5 tys. ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1001&search[filter-price_to]=1500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp


Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_3pok1500 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_3pok1500                   



write_xlsx(x = oferty_woj_3pok1500, path = "oferty_woj_3pok1500.xlsx", col_names = TRUE)



### SKRYPT  1 3 pokojowe 1,5 - 2 tys. ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_3pok2000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_3pok2000                   



write_xlsx(x = oferty_woj_3pok2000, path = "oferty_woj_3pok2000.xlsx", col_names = TRUE)




### SKRYPT  1 3 pokojowe 2 - 2,5 tys. ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_3pok2500 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_3pok2500                   



write_xlsx(x = oferty_woj_3pok2500, path = "oferty_woj_3pok2500.xlsx", col_names = TRUE)





### SKRYPT  1 3 pokojowe 2,5 - 3 tys. ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp


Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_3pok3000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_3pok3000                  



write_xlsx(x = oferty_woj_3pok3000, path = "oferty_woj_3pok3000.xlsx", col_names = TRUE)




### SKRYPT  1 3 pokojowe 3 - 3,5 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3001&search[filter-price_to]=3500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_3pok3500 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_3pok3500                  



write_xlsx(x = oferty_woj_3pok3500, path = "oferty_woj_3pok3500.xlsx", col_names = TRUE)




### SKRYPT  1 3 pokojowe 3,5 - 4 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_3pok4000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_3pok4000                  



write_xlsx(x = oferty_woj_3pok4000, path = "oferty_woj_3pok4000.xlsx", col_names = TRUE)




### SKRYPT  1 3 pokojowe 4 - 4,5 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4001&search[filter-price_to]=4500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_3pok4500 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_3pok4500                  



write_xlsx(x = oferty_woj_3pok4500, path = "oferty_woj_3pok4500.xlsx", col_names = TRUE)



### SKRYPT  1 3 pokojowe 4,5 - 5 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_3pok5000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_3pok5000                  



write_xlsx(x = oferty_woj_3pok5000, path = "oferty_woj_3pok5000.xlsx", col_names = TRUE)



### SKRYPT  1 3 pokojowe 5 - 6 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5001&search[filter-price_to]=6000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_3pok6000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_3pok6000                  



write_xlsx(x = oferty_woj_3pok6000, path = "oferty_woj_3pok6000.xlsx", col_names = TRUE)



### SKRYPT  1 3 pokojowe 6 - 7 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_3pok7000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_3pok7000                  



write_xlsx(x = oferty_woj_3pok7000, path = "oferty_woj_3pok7000.xlsx", col_names = TRUE)



### SKRYPT  1 3 pokojowe 7-8 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_3pok8000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_3pok8000                  



write_xlsx(x = oferty_woj_3pok8000, path = "oferty_woj_3pok8000.xlsx", col_names = TRUE)




### SKRYPT  1 3 pokojowe 8-9 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_3pok9000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_3pok9000                  



write_xlsx(x = oferty_woj_3pok9000, path = "oferty_woj_3pok9000.xlsx", col_names = TRUE)



### SKRYPT  1 3 pokojowe 9-10 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp


Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_3pok10000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                              df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_3pok10000                  



write_xlsx(x = oferty_woj_3pok10000, path = "oferty_woj_3pok10000.xlsx", col_names = TRUE)


### SKRYPT  1 3 pokojowe pow 10 tys. ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp


Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=3&search[filter-number_of_rooms_to]=3&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_3pok_pow_10000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                                   df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_3pok_pow_10000                  



write_xlsx(x = oferty_woj_3pok_pow_10000 , path = "oferty_woj_3pok_pow_10000.xlsx", col_names = TRUE)





































### SKRYPT  1 4 lub więcej pokoi DO 500 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_4pok500 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                            df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_4pok500                   



write_xlsx(x = oferty_woj_4pok500, path = "oferty_woj_4pok500.xlsx", col_names = TRUE)





### SKRYPT  1 4 lub więcej pokoi 500 - 1 tys. ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=501&search[filter-price_to]=1%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_4pok1000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_4pok1000                   



write_xlsx(x = oferty_woj_4pok1000, path = "oferty_woj_4pok1000.xlsx", col_names = TRUE)


### SKRYPT  1 4 lub więcej pokoi 1 - 1,5 tys. ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1001&search[filter-price_to]=1500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub

Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20001&search[filter-price_to]=1%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_4pok1500 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_4pok1500                   



write_xlsx(x = oferty_woj_4pok1500, path = "oferty_woj_4pok1500.xlsx", col_names = TRUE)



### SKRYPT  1 4 lub więcej pokoi 1,5 - 2 tys. ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub


Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20501&search[filter-price_to]=2%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_4pok2000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_4pok2000                   



write_xlsx(x = oferty_woj_4pok2000, path = "oferty_woj_4pok2000.xlsx", col_names = TRUE)




### SKRYPT  1 4 lub więcej pokoi 2 - 2,5 tys. ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub


Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20001&search[filter-price_to]=2%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_4pok2500 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_4pok2500                   



write_xlsx(x = oferty_woj_4pok2500, path = "oferty_woj_4pok2500.xlsx", col_names = TRUE)





### SKRYPT  1 4 lub więcej pokoi 2,5 - 3 tys. ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub


Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20501&search[filter-price_to]=3%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_4pok3000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_4pok3000                  



write_xlsx(x = oferty_woj_4pok3000, path = "oferty_woj_4pok3000.xlsx", col_names = TRUE)




### SKRYPT  1 4 lub więcej pokoi 3 - 3,5 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3001&search[filter-price_to]=3500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20001&search[filter-price_to]=3%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_4pok3500 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_4pok3500                  



write_xlsx(x = oferty_woj_4pok3500, path = "oferty_woj_4pok3500.xlsx", col_names = TRUE)




### SKRYPT  1 4 lub więcej pokoi 3,5 - 4 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20501&search[filter-price_to]=4%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_4pok4000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_4pok4000                  



write_xlsx(x = oferty_woj_4pok4000, path = "oferty_woj_4pok4000.xlsx", col_names = TRUE)




### SKRYPT  1 4 lub więcej pokoi 4 - 4,5 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4001&search[filter-price_to]=4500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20001&search[filter-price_to]=4%20500&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_4pok4500 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_4pok4500                  



write_xlsx(x = oferty_woj_4pok4500, path = "oferty_woj_4pok4500.xlsx", col_names = TRUE)



### SKRYPT  1 4 lub więcej pokoi 4,5 - 5 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20501&search[filter-price_to]=5%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_4pok5000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_4pok5000                  



write_xlsx(x = oferty_woj_4pok5000, path = "oferty_woj_4pok5000.xlsx", col_names = TRUE)



### SKRYPT  1 4 lub więcej pokoi 5 - 6 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5001&search[filter-price_to]=6000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp


Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20001&search[filter-price_to]=6%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_4pok6000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_4pok6000                  



write_xlsx(x = oferty_woj_4pok6000, path = "oferty_woj_4pok6000.xlsx", col_names = TRUE)



### SKRYPT  1 4 lub więcej pokoi 6 - 7 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=6%20001&search[filter-price_to]=7%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_4pok7000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_4pok7000                  



write_xlsx(x = oferty_woj_4pok7000, path = "oferty_woj_4pok7000.xlsx", col_names = TRUE)



### SKRYPT  1 4 lub więcej pokoi 7-8 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=7%20001&search[filter-price_to]=8%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_4pok8000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_4pok8000                  



write_xlsx(x = oferty_woj_4pok8000, path = "oferty_woj_4pok8000.xlsx", col_names = TRUE)




### SKRYPT  1 4 lub więcej pokoi 8-9 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=8%20001&search[filter-price_to]=9%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_4pok9000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                             df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_4pok9000                  



write_xlsx(x = oferty_woj_4pok9000, path = "oferty_woj_4pok9000.xlsx", col_names = TRUE)



### SKRYPT  1 4 lub więcej pokoi 9-10 ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub



Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=9%20001&search[filter-price_to]=10%20000&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_4pok10000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                              df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_4pok10000                  



write_xlsx(x = oferty_woj_4pok10000, path = "oferty_woj_4pok10000.xlsx", col_names = TRUE)


### SKRYPT  1 4 lub więcej pokoi pow 10 tys. ###

Sys.sleep(runif(1,30,90)) ### ZACHODNIOPOMORSKIE ###

zach <- read_html("https://www.XXX/offer_list?search[locations][0]=99484&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zach <-str_replace_all(zach, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zach <- regmatches(zach, gregexpr("[[:digit:]]+", zach))
zach <- as.numeric(unlist(zach))
df_zach <- data.frame(c('Zachodniopomorskie'), zach[1], zach[2], zach[3])
colnames(df_zach)<-c('Województwo','Znalezione','Odfiltrowane','Reszta') 
df_zach

Sys.sleep(runif(1,30,90)) ### POMORSKIE ###

pom <- read_html("https://www.XXX/offer_list?search[locations][0]=70129&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pom <-str_replace_all(pom, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pom <- regmatches(pom, gregexpr("[[:digit:]]+", pom))
pom <- as.numeric(unlist(pom))
df_pom <- data.frame(c('Pomorskie'), pom[1], pom[2], pom[3])
colnames(df_pom)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pom

Sys.sleep(runif(1,30,90)) ### WARMIŃSKO-MAZURSKIE ###

wm <- read_html("https://www.XXX/offer_list?search[locations][0]=86743&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wm <-str_replace_all(wm, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wm <- regmatches(wm, gregexpr("[[:digit:]]+", wm))
wm <- as.numeric(unlist(wm))
df_wm <- data.frame(c('Warmińsko-Mazurskie'), wm[1], wm[2], wm[3])
colnames(df_wm)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wm

Sys.sleep(runif(1,30,90)) ### Podlaskie ###

podl <- read_html("https://www.XXX/offer_list?search[locations][0]=64927&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
podl <-str_replace_all(podl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
podl <- regmatches(podl, gregexpr("[[:digit:]]+", podl))
podl <- as.numeric(unlist(podl))
df_podl <- data.frame(c('Podlaskie'), podl[1], podl[2], podl[3])
colnames(df_podl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_podl

Sys.sleep(runif(1,30,90)) ### Mazowieckie ###

maz <- read_html("https://www.XXX/offer_list?search[locations][0]=42045&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
maz <-str_replace_all(maz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
maz <- regmatches(maz, gregexpr("[[:digit:]]+", maz))
maz <- as.numeric(unlist(maz))
df_maz <- data.frame(c('Mazowieckie'), maz[1], maz[2], maz[3])
colnames(df_maz)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_maz


Sys.sleep(runif(1,30,90)) ### KUJAWSKO-POMORSKIE ###

kp <- read_html("https://www.XXX/offer_list?search[locations][0]=3649&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kp <-str_replace_all(kp, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kp <- regmatches(kp, gregexpr("[[:digit:]]+", kp))
kp <- as.numeric(unlist(kp))
df_kp <- data.frame(c('Kujawsko-Pomorskie'), kp[1], kp[2], kp[3])
colnames(df_kp)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_kp



Sys.sleep(runif(1,30,90)) ### WIELKOPOLSKIE ###

wie <- read_html("https://www.XXX/offer_list?search[locations][0]=91540&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wie <-str_replace_all(wie, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wie <- regmatches(wie, gregexpr("[[:digit:]]+", wie))
wie <- as.numeric(unlist(wie))
df_wie <- data.frame(c('Wielkopolskie'), wie[1], wie[2], wie[3])
colnames(df_wie)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_wie


Sys.sleep(runif(1,30,90)) ### LUBUSKIE ###

lub <- read_html("https://www.XXX/offer_list?search[locations][0]=17392&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lub <-str_replace_all(lub, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lub <- regmatches(lub, gregexpr("[[:digit:]]+", lub))
lub <- as.numeric(unlist(lub))
df_lub <- data.frame(c('Lubuskie'), lub[1], lub[2], lub[3])
colnames(df_lub)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lub


Sys.sleep(runif(1,30,90)) ### DOLNOŚLĄSKIE ###

dol <- read_html("https://www.XXX/offer_list?search[locations][0]=2&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
dol <-str_replace_all(dol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
dol <- regmatches(dol, gregexpr("[[:digit:]]+", dol))
dol <- as.numeric(unlist(dol))
df_dol <- data.frame(c('Dolnośląskie'), dol[1], dol[2], dol[3])
colnames(df_dol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_dol


Sys.sleep(runif(1,30,90)) ### ŁÓDZKIE ###

lod <- read_html("https://www.XXX/offer_list?search[locations][0]=19115&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lod <-str_replace_all(lod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lod <- regmatches(lod, gregexpr("[[:digit:]]+", lod))
lod <- as.numeric(unlist(lod))
df_lod <- data.frame(c('Łódzkie'), lod[1], lod[2], lod[3])
colnames(df_lod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lod

Sys.sleep(runif(1,30,90)) ### LUBELSKIE ###

lubel <- read_html("https://www.XXX/offer_list?search[locations][0]=9384&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lubel <-str_replace_all(lubel, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lubel <- regmatches(lubel, gregexpr("[[:digit:]]+", lubel))
lubel <- as.numeric(unlist(lubel))
df_lubel <- data.frame(c('Lubelskie'), lubel[1], lubel[2], lubel[3])
colnames(df_lubel)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_lubel


Sys.sleep(runif(1,30,90)) ### ŚWIĘTOKRZYSKIE ###

sw <- read_html("https://www.XXX/offer_list?search[locations][0]=79565&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sw <-str_replace_all(sw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sw <- regmatches(sw, gregexpr("[[:digit:]]+", sw))
sw <- as.numeric(unlist(sw))
df_sw <- data.frame(c('Świętokrzyskie'), sw[1], sw[2], sw[3])
colnames(df_sw)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sw


Sys.sleep(runif(1,30,90)) ### OPOLSKIE ###

opol <- read_html("https://www.XXX/offer_list?search[locations][0]=55264&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opol <-str_replace_all(opol, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opol <- regmatches(opol, gregexpr("[[:digit:]]+", opol))
opol <- as.numeric(unlist(opol))
df_opol <- data.frame(c('Opolskie'), opol[1], opol[2], opol[3])
colnames(df_opol)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_opol


Sys.sleep(runif(1,30,90)) ### ŚLĄSKIE ###

sl <- read_html("https://www.XXX/offer_list?search[locations][0]=74442&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
sl <-str_replace_all(sl, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
sl <- regmatches(sl, gregexpr("[[:digit:]]+", sl))
sl <- as.numeric(unlist(sl))
df_sl <- data.frame(c('Śląskie'), sl[1], sl[2], sl[3])
colnames(df_sl)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_sl


Sys.sleep(runif(1,30,90)) ### MAŁOPOLSKIE ###

mal <- read_html("https://www.XXX/offer_list?search[locations][0]=27529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
mal <-str_replace_all(mal, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
mal <- regmatches(mal, gregexpr("[[:digit:]]+", mal))
mal <- as.numeric(unlist(mal))
df_mal <- data.frame(c('Małopolskie'), mal[1], mal[2], mal[3])
colnames(df_mal)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_mal

Sys.sleep(runif(1,30,90)) ### PODKARPACKIE ###

pod <- read_html("https://www.XXX/offer_list?search[locations][0]=57216&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=2&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=10%20001&search[filter-number_of_rooms_from]=4&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
pod <-str_replace_all(pod, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
pod <- regmatches(pod, gregexpr("[[:digit:]]+", pod))
pod <- as.numeric(unlist(pod))
df_pod <- data.frame(c('Podkarpackie'), pod[1], pod[2], pod[3])
colnames(df_pod)<-c('Województwo','Znalezione','Odfiltrowane','Reszta')
df_pod


oferty_woj_4pok_pow_10000 <- rbind(df_zach, df_pom, df_wm, df_podl, df_maz, df_kp, df_wie,
                                   df_lub, df_dol,df_lod,df_lubel,df_sw,df_opol,df_sl,df_mal,df_pod)

oferty_woj_4pok_pow_10000                  



write_xlsx(x = oferty_woj_4pok_pow_10000 , path = "oferty_woj_4pok_pow_10000.xlsx", col_names = TRUE)
