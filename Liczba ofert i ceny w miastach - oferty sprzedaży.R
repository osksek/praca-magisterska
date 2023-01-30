Sys.sleep(runif(1,30,90)) ###Liczba ofertsprzedaży w przedziałach cenowych dla miast ###

rm(list=ls())
install.packages("tidyverse")
install.packages("rvest")
install.packages("glue")
install.packages("stringr")
install.packages("lubridate")
install.packages("knitr")
install.packages("kableExtra")
install.packages("readr")

library(tidyverse)
library(rvest)
library(glue)
library(stringr)
library(lubridate)
library(knitr)
library(kableExtra)
library(readr)
library(writexl)


nazwy <- c('Miasto','Znalezione','Odfiltrowane','gg')


### SKRYPT  1 CENA DO 100 TYS. ZŁ ###

Sys.sleep(runif(1,30,90)) ###Szczecin ###
                     
szczecin <- read_html("https://www.XXX/offer_list?search[locations][0]=103059&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=100000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
szczecin <-str_replace_all(szczecin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
szczecin <- regmatches(szczecin, gregexpr("[[:digit:]]+", szczecin))
szczecin <- as.numeric(unlist(szczecin))
df_szczecin <- data.frame(c('Szczecin'), szczecin[1], szczecin[2], szczecin[3])
colnames(df_szczecin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta') 
df_szczecin

Sys.sleep(runif(1,30,90)) ###Gdańsk ###

gdansk <- read_html("https://www.XXX/offer_list?search[locations][0]=74287&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=100%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gdansk <-str_replace_all(gdansk, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gdansk <- regmatches(gdansk, gregexpr("[[:digit:]]+", gdansk))
gdansk <- as.numeric(unlist(gdansk))
df_gdansk <- data.frame(c('Gdańsk'), gdansk[1], gdansk[2], gdansk[3])
colnames(df_gdansk)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gdansk

Sys.sleep(runif(1,30,90)) ###Olsztyn ###

olsztyn <- read_html("https://www.XXX/offer_list?search[locations][0]=91529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=100%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
olsztyn <-str_replace_all(olsztyn, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
olsztyn <- regmatches(olsztyn, gregexpr("[[:digit:]]+", olsztyn))
olsztyn <- as.numeric(unlist(olsztyn))
df_olsztyn <- data.frame(c('Olsztyn'), olsztyn[1], olsztyn[2], olsztyn[3])
colnames(df_olsztyn)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_olsztyn

Sys.sleep(runif(1,30,90)) ###Białystok ###

bialystok <- read_html("https://www.XXX/offer_list?search[locations][0]=70081&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=100%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bialystok <-str_replace_all(bialystok, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bialystok <- regmatches(bialystok, gregexpr("[[:digit:]]+", bialystok))
bialystok <- as.numeric(unlist(bialystok))
df_bialystok <- data.frame(c('Białystok'), bialystok[1], bialystok[2], bialystok[3])
colnames(df_bialystok)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bialystok

Sys.sleep(runif(1,30,90)) ###Warszawa ###

warszawa <- read_html("https://www.XXX/offer_list?search[locations][0]=55060&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=100%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
warszawa <-str_replace_all(warszawa, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
warszawa <- regmatches(warszawa, gregexpr("[[:digit:]]+", warszawa))
warszawa <- as.numeric(unlist(warszawa))
df_warszawa <- data.frame(c('Warszawa'), warszawa[1], warszawa[2], warszawa[3])
colnames(df_warszawa)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_warszawa


Sys.sleep(runif(1,30,90)) ###Toruń ###

torun <- read_html("https://www.XXX/offer_list?search[locations][0]=9292&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=100%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
torun <-str_replace_all(torun, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
torun <- regmatches(torun, gregexpr("[[:digit:]]+", torun))
torun <- as.numeric(unlist(torun))
df_torun <- data.frame(c('Toruń'), torun[1], torun[2], torun[3])
colnames(df_torun)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_torun

Sys.sleep(runif(1,30,90)) ###Bydgoszcz ###

bydgoszcz <- read_html("https://www.XXX/offer_list?search[locations][0]=9232&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=100%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bydgoszcz <-str_replace_all(bydgoszcz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bydgoszcz <- regmatches(bydgoszcz, gregexpr("[[:digit:]]+", bydgoszcz))
bydgoszcz <- as.numeric(unlist(bydgoszcz))
df_bydgoszcz <- data.frame(c('Bydgoszcz'), bydgoszcz[1], bydgoszcz[2], bydgoszcz[3])
colnames(df_bydgoszcz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bydgoszcz

Sys.sleep(runif(1,30,90)) ###Poznań ###

poznan <- read_html("https://www.XXX/offer_list?search[locations][0]=99366&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=100%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
poznan <-str_replace_all(poznan, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
poznan <- regmatches(poznan, gregexpr("[[:digit:]]+", poznan))
poznan <- as.numeric(unlist(poznan))
df_poznan <- data.frame(c('Poznań'), poznan[1], poznan[2], poznan[3])
colnames(df_poznan)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_poznan


Sys.sleep(runif(1,30,90)) ###Zielona Góra ###

zielona <- read_html("https://www.XXX/offer_list?search[locations][0]=19087&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=100%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zielona <-str_replace_all(zielona, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zielona <- regmatches(zielona, gregexpr("[[:digit:]]+", zielona))
zielona <- as.numeric(unlist(zielona))
df_zielona <- data.frame(c('Zielona Góra'), zielona[1], zielona[2], zielona[3])
colnames(df_zielona)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_zielona


Sys.sleep(runif(1,30,90)) ###Gorzów Wielkopolski ###

gorzow <- read_html("https://www.XXX/offer_list?search[locations][0]=19073&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=100%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gorzow <-str_replace_all(gorzow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gorzow <- regmatches(gorzow, gregexpr("[[:digit:]]+", gorzow))
gorzow <- as.numeric(unlist(gorzow))
df_gorzow <- data.frame(c('Gorzów Wielkopolski'), gorzow[1], gorzow[2], gorzow[3])
colnames(df_gorzow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gorzow

Sys.sleep(runif(1,30,90)) ###Wrocław ###

wroclaw <- read_html("https://www.XXX/offer_list?search[locations][0]=3548&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=100%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wroclaw <-str_replace_all(wroclaw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wroclaw <- regmatches(wroclaw, gregexpr("[[:digit:]]+", wroclaw))
wroclaw <- as.numeric(unlist(wroclaw))
df_wroclaw <- data.frame(c('Wrocław'), wroclaw[1], wroclaw[2], wroclaw[3])
colnames(df_wroclaw)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_wroclaw


Sys.sleep(runif(1,30,90)) ###Łódź ###

lodz <- read_html("https://www.XXX/offer_list?search[locations][0]=27391&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=100%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lodz <-str_replace_all(lodz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lodz <- regmatches(lodz, gregexpr("[[:digit:]]+", lodz))
lodz <- as.numeric(unlist(lodz))
df_lodz <- data.frame(c('Łódź'), lodz[1], lodz[2], lodz[3])
colnames(df_lodz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lodz

Sys.sleep(runif(1,30,90)) ###Lublin ###

lublin <- read_html("https://www.XXX/offer_list?search[locations][0]=17291&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=100%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lublin <-str_replace_all(lublin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lublin <- regmatches(lublin, gregexpr("[[:digit:]]+", lublin))
lublin <- as.numeric(unlist(lublin))
df_lublin <- data.frame(c('Lublin'), lublin[1], lublin[2], lublin[3])
colnames(df_lublin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lublin


Sys.sleep(runif(1,30,90)) ###Kielce ###

kielce <- read_html("https://www.XXX/offer_list?search[locations][0]=86670&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=100%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kielce <-str_replace_all(kielce, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kielce <- regmatches(kielce, gregexpr("[[:digit:]]+", kielce))
kielce <- as.numeric(unlist(kielce))
df_kielce <- data.frame(c('Kielce'), kielce[1], kielce[2], kielce[3])
colnames(df_kielce)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_kielce


Sys.sleep(runif(1,30,90)) ###Opole ###

opole <- read_html("https://www.XXX/offer_list?search[locations][0]=57175&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=100%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opole <-str_replace_all(opole, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opole <- regmatches(opole, gregexpr("[[:digit:]]+", opole))
opole <- as.numeric(unlist(opole))
df_opole <- data.frame(c('Opole'), opole[1], opole[2], opole[3])
colnames(df_opole)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_opole


Sys.sleep(runif(1,30,90)) ###Katowice ###

katowice <- read_html("https://www.XXX/offer_list?search[locations][0]=79344&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=100%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
katowice <-str_replace_all(katowice, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
katowice <- regmatches(katowice, gregexpr("[[:digit:]]+", katowice))
katowice <- as.numeric(unlist(katowice))
df_katowice <- data.frame(c('Katowice'), katowice[1], katowice[2], katowice[3])
colnames(df_katowice)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_katowice


Sys.sleep(runif(1,30,90)) ###Kraków ###

krakow <- read_html("https://www.XXX/offer_list?search[locations][0]=41878&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=100%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
krakow <-str_replace_all(krakow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
krakow <- regmatches(krakow, gregexpr("[[:digit:]]+", krakow))
krakow <- as.numeric(unlist(krakow))
df_krakow <- data.frame(c('Kraków'), krakow[1], krakow[2], krakow[3])
colnames(df_krakow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_krakow

Sys.sleep(runif(1,30,90)) ###Rzeszów ###

rzeszow <- read_html("https://www.XXX/offer_list?search[locations][0]=64847&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1&search[filter-price_to]=100%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
rzeszow <-str_replace_all(rzeszow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
rzeszow <- regmatches(rzeszow, gregexpr("[[:digit:]]+", rzeszow))
rzeszow <- as.numeric(unlist(rzeszow))
df_rzeszow <- data.frame(c('Rzeszów'), rzeszow[1], rzeszow[2], rzeszow[3])
colnames(df_rzeszow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_rzeszow


oferty_miasta_sprz100 <- rbind(df_bialystok,df_bydgoszcz,df_gdansk,df_gorzow,df_katowice,df_kielce,df_krakow,
                                 df_lodz,df_lublin,df_olsztyn,df_opole,df_poznan,df_rzeszow,df_szczecin,
                                 df_torun,df_warszawa,df_wroclaw,df_zielona)
oferty_miasta_sprz100                    



write_xlsx(x = oferty_miasta_sprz100, path = "oferty_miasta_sprz100.xlsx", col_names = TRUE)





###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################



### SKRYPT  100-200 TYS. ZŁ ###

Sys.sleep(runif(1,30,90)) ###Szczecin ###

szczecin <- read_html("https://www.XXX/offer_list?search[locations][0]=103059&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=100001&search[filter-price_to]=200000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
szczecin <-str_replace_all(szczecin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
szczecin <- regmatches(szczecin, gregexpr("[[:digit:]]+", szczecin))
szczecin <- as.numeric(unlist(szczecin))
df_szczecin <- data.frame(c('Szczecin'), szczecin[1], szczecin[2], szczecin[3])
colnames(df_szczecin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta') 
df_szczecin

Sys.sleep(runif(1,30,90)) ###Gdańsk ###

gdansk <- read_html("https://www.XXX/offer_list?search[locations][0]=74287&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=100%20001&search[filter-price_to]=200%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gdansk <-str_replace_all(gdansk, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gdansk <- regmatches(gdansk, gregexpr("[[:digit:]]+", gdansk))
gdansk <- as.numeric(unlist(gdansk))
df_gdansk <- data.frame(c('Gdańsk'), gdansk[1], gdansk[2], gdansk[3])
colnames(df_gdansk)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gdansk

Sys.sleep(runif(1,30,90)) ###Olsztyn ###

olsztyn <- read_html("https://www.XXX/offer_list?search[locations][0]=91529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=100%20001&search[filter-price_to]=200%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
olsztyn <-str_replace_all(olsztyn, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
olsztyn <- regmatches(olsztyn, gregexpr("[[:digit:]]+", olsztyn))
olsztyn <- as.numeric(unlist(olsztyn))
df_olsztyn <- data.frame(c('Olsztyn'), olsztyn[1], olsztyn[2], olsztyn[3])
colnames(df_olsztyn)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_olsztyn

Sys.sleep(runif(1,30,90)) ###Białystok ###

bialystok <- read_html("https://www.XXX/offer_list?search[locations][0]=70081&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=100%20001&search[filter-price_to]=200%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bialystok <-str_replace_all(bialystok, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bialystok <- regmatches(bialystok, gregexpr("[[:digit:]]+", bialystok))
bialystok <- as.numeric(unlist(bialystok))
df_bialystok <- data.frame(c('Białystok'), bialystok[1], bialystok[2], bialystok[3])
colnames(df_bialystok)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bialystok

Sys.sleep(runif(1,30,90)) ###Warszawa ###

warszawa <- read_html("https://www.XXX/offer_list?search[locations][0]=55060&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=100%20001&search[filter-price_to]=200%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
warszawa <-str_replace_all(warszawa, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
warszawa <- regmatches(warszawa, gregexpr("[[:digit:]]+", warszawa))
warszawa <- as.numeric(unlist(warszawa))
df_warszawa <- data.frame(c('Warszawa'), warszawa[1], warszawa[2], warszawa[3])
colnames(df_warszawa)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_warszawa


Sys.sleep(runif(1,30,90)) ###Toruń ###

torun <- read_html("https://www.XXX/offer_list?search[locations][0]=9292&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=100%20001&search[filter-price_to]=200%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
torun <-str_replace_all(torun, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
torun <- regmatches(torun, gregexpr("[[:digit:]]+", torun))
torun <- as.numeric(unlist(torun))
df_torun <- data.frame(c('Toruń'), torun[1], torun[2], torun[3])
colnames(df_torun)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_torun

Sys.sleep(runif(1,30,90)) ###Bydgoszcz ###

bydgoszcz <- read_html("https://www.XXX/offer_list?search[locations][0]=9232&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=100%20001&search[filter-price_to]=200%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bydgoszcz <-str_replace_all(bydgoszcz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bydgoszcz <- regmatches(bydgoszcz, gregexpr("[[:digit:]]+", bydgoszcz))
bydgoszcz <- as.numeric(unlist(bydgoszcz))
df_bydgoszcz <- data.frame(c('Bydgoszcz'), bydgoszcz[1], bydgoszcz[2], bydgoszcz[3])
colnames(df_bydgoszcz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bydgoszcz

Sys.sleep(runif(1,30,90)) ###Poznań ###

poznan <- read_html("https://www.XXX/offer_list?search[locations][0]=99366&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=100%20001&search[filter-price_to]=200%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
poznan <-str_replace_all(poznan, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
poznan <- regmatches(poznan, gregexpr("[[:digit:]]+", poznan))
poznan <- as.numeric(unlist(poznan))
df_poznan <- data.frame(c('Poznań'), poznan[1], poznan[2], poznan[3])
colnames(df_poznan)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_poznan


Sys.sleep(runif(1,30,90)) ###Zielona Góra ###

zielona <- read_html("https://www.XXX/offer_list?search[locations][0]=19087&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=100%20001&search[filter-price_to]=200%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zielona <-str_replace_all(zielona, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zielona <- regmatches(zielona, gregexpr("[[:digit:]]+", zielona))
zielona <- as.numeric(unlist(zielona))
df_zielona <- data.frame(c('Zielona Góra'), zielona[1], zielona[2], zielona[3])
colnames(df_zielona)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_zielona


Sys.sleep(runif(1,30,90)) ###Gorzów Wielkopolski ###

gorzow <- read_html("https://www.XXX/offer_list?search[locations][0]=19073&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=100%20001&search[filter-price_to]=200%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gorzow <-str_replace_all(gorzow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gorzow <- regmatches(gorzow, gregexpr("[[:digit:]]+", gorzow))
gorzow <- as.numeric(unlist(gorzow))
df_gorzow <- data.frame(c('Gorzów Wielkopolski'), gorzow[1], gorzow[2], gorzow[3])
colnames(df_gorzow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gorzow

Sys.sleep(runif(1,30,90)) ###Wrocław ###

wroclaw <- read_html("https://www.XXX/offer_list?search[locations][0]=3548&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=100%20001&search[filter-price_to]=200%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wroclaw <-str_replace_all(wroclaw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wroclaw <- regmatches(wroclaw, gregexpr("[[:digit:]]+", wroclaw))
wroclaw <- as.numeric(unlist(wroclaw))
df_wroclaw <- data.frame(c('Wrocław'), wroclaw[1], wroclaw[2], wroclaw[3])
colnames(df_wroclaw)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_wroclaw


Sys.sleep(runif(1,30,90)) ###Łódź ###

lodz <- read_html("https://www.XXX/offer_list?search[locations][0]=27391&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=100%20001&search[filter-price_to]=200%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lodz <-str_replace_all(lodz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lodz <- regmatches(lodz, gregexpr("[[:digit:]]+", lodz))
lodz <- as.numeric(unlist(lodz))
df_lodz <- data.frame(c('Łódź'), lodz[1], lodz[2], lodz[3])
colnames(df_lodz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lodz

Sys.sleep(runif(1,30,90)) ###Lublin ###

lublin <- read_html("https://www.XXX/offer_list?search[locations][0]=17291&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=100%20001&search[filter-price_to]=200%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lublin <-str_replace_all(lublin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lublin <- regmatches(lublin, gregexpr("[[:digit:]]+", lublin))
lublin <- as.numeric(unlist(lublin))
df_lublin <- data.frame(c('Lublin'), lublin[1], lublin[2], lublin[3])
colnames(df_lublin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lublin


Sys.sleep(runif(1,30,90)) ###Kielce ###

kielce <- read_html("https://www.XXX/offer_list?search[locations][0]=86670&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=100%20001&search[filter-price_to]=200%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kielce <-str_replace_all(kielce, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kielce <- regmatches(kielce, gregexpr("[[:digit:]]+", kielce))
kielce <- as.numeric(unlist(kielce))
df_kielce <- data.frame(c('Kielce'), kielce[1], kielce[2], kielce[3])
colnames(df_kielce)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_kielce


Sys.sleep(runif(1,30,90)) ###Opole ###

opole <- read_html("https://www.XXX/offer_list?search[locations][0]=57175&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=100%20001&search[filter-price_to]=200%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opole <-str_replace_all(opole, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opole <- regmatches(opole, gregexpr("[[:digit:]]+", opole))
opole <- as.numeric(unlist(opole))
df_opole <- data.frame(c('Opole'), opole[1], opole[2], opole[3])
colnames(df_opole)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_opole


Sys.sleep(runif(1,30,90)) ###Katowice ###

katowice <- read_html("https://www.XXX/offer_list?search[locations][0]=79344&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=100%20001&search[filter-price_to]=200%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
katowice <-str_replace_all(katowice, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
katowice <- regmatches(katowice, gregexpr("[[:digit:]]+", katowice))
katowice <- as.numeric(unlist(katowice))
df_katowice <- data.frame(c('Katowice'), katowice[1], katowice[2], katowice[3])
colnames(df_katowice)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_katowice


Sys.sleep(runif(1,30,90)) ###Kraków ###

krakow <- read_html("https://www.XXX/offer_list?search[locations][0]=41878&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=100%20001&search[filter-price_to]=200%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
krakow <-str_replace_all(krakow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
krakow <- regmatches(krakow, gregexpr("[[:digit:]]+", krakow))
krakow <- as.numeric(unlist(krakow))
df_krakow <- data.frame(c('Kraków'), krakow[1], krakow[2], krakow[3])
colnames(df_krakow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_krakow

Sys.sleep(runif(1,30,90)) ###Rzeszów ###

rzeszow <- read_html("https://www.XXX/offer_list?search[locations][0]=64847&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=100%20001&search[filter-price_to]=200%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
rzeszow <-str_replace_all(rzeszow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
rzeszow <- regmatches(rzeszow, gregexpr("[[:digit:]]+", rzeszow))
rzeszow <- as.numeric(unlist(rzeszow))
df_rzeszow <- data.frame(c('Rzeszów'), rzeszow[1], rzeszow[2], rzeszow[3])
colnames(df_rzeszow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_rzeszow


oferty_miasta_sprz200 <- rbind(df_bialystok,df_bydgoszcz,df_gdansk,df_gorzow,df_katowice,df_kielce,df_krakow,
                               df_lodz,df_lublin,df_olsztyn,df_opole,df_poznan,df_rzeszow,df_szczecin,
                               df_torun,df_warszawa,df_wroclaw,df_zielona)
oferty_miasta_sprz200                   



write_xlsx(x = oferty_miasta_sprz200, path = "oferty_miasta_sprz200.xlsx", col_names = TRUE)


###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################



### SKRYPT  200-300 TYS. ZŁ ###

Sys.sleep(runif(1,30,90)) ###Szczecin ###

szczecin <- read_html("https://www.XXX/offer_list?search[locations][0]=103059&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=200001&search[filter-price_to]=300000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
szczecin <-str_replace_all(szczecin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
szczecin <- regmatches(szczecin, gregexpr("[[:digit:]]+", szczecin))
szczecin <- as.numeric(unlist(szczecin))
df_szczecin <- data.frame(c('Szczecin'), szczecin[1], szczecin[2], szczecin[3])
colnames(df_szczecin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta') 
df_szczecin

Sys.sleep(runif(1,30,90)) ###Gdańsk ###

gdansk <- read_html("https://www.XXX/offer_list?search[locations][0]=74287&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=200%20001&search[filter-price_to]=300%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gdansk <-str_replace_all(gdansk, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gdansk <- regmatches(gdansk, gregexpr("[[:digit:]]+", gdansk))
gdansk <- as.numeric(unlist(gdansk))
df_gdansk <- data.frame(c('Gdańsk'), gdansk[1], gdansk[2], gdansk[3])
colnames(df_gdansk)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gdansk

Sys.sleep(runif(1,30,90)) ###Olsztyn ###

olsztyn <- read_html("https://www.XXX/offer_list?search[locations][0]=91529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=200%20001&search[filter-price_to]=300%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
olsztyn <-str_replace_all(olsztyn, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
olsztyn <- regmatches(olsztyn, gregexpr("[[:digit:]]+", olsztyn))
olsztyn <- as.numeric(unlist(olsztyn))
df_olsztyn <- data.frame(c('Olsztyn'), olsztyn[1], olsztyn[2], olsztyn[3])
colnames(df_olsztyn)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_olsztyn

Sys.sleep(runif(1,30,90)) ###Białystok ###

bialystok <- read_html("https://www.XXX/offer_list?search[locations][0]=70081&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=200%20001&search[filter-price_to]=300%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bialystok <-str_replace_all(bialystok, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bialystok <- regmatches(bialystok, gregexpr("[[:digit:]]+", bialystok))
bialystok <- as.numeric(unlist(bialystok))
df_bialystok <- data.frame(c('Białystok'), bialystok[1], bialystok[2], bialystok[3])
colnames(df_bialystok)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bialystok

Sys.sleep(runif(1,30,90)) ###Warszawa ###

warszawa <- read_html("https://www.XXX/offer_list?search[locations][0]=55060&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=200%20001&search[filter-price_to]=300%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
warszawa <-str_replace_all(warszawa, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
warszawa <- regmatches(warszawa, gregexpr("[[:digit:]]+", warszawa))
warszawa <- as.numeric(unlist(warszawa))
df_warszawa <- data.frame(c('Warszawa'), warszawa[1], warszawa[2], warszawa[3])
colnames(df_warszawa)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_warszawa


Sys.sleep(runif(1,30,90)) ###Toruń ###

torun <- read_html("https://www.XXX/offer_list?search[locations][0]=9292&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=200%20001&search[filter-price_to]=300%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
torun <-str_replace_all(torun, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
torun <- regmatches(torun, gregexpr("[[:digit:]]+", torun))
torun <- as.numeric(unlist(torun))
df_torun <- data.frame(c('Toruń'), torun[1], torun[2], torun[3])
colnames(df_torun)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_torun

Sys.sleep(runif(1,30,90)) ###Bydgoszcz ###

bydgoszcz <- read_html("https://www.XXX/offer_list?search[locations][0]=9232&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=200%20001&search[filter-price_to]=300%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bydgoszcz <-str_replace_all(bydgoszcz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bydgoszcz <- regmatches(bydgoszcz, gregexpr("[[:digit:]]+", bydgoszcz))
bydgoszcz <- as.numeric(unlist(bydgoszcz))
df_bydgoszcz <- data.frame(c('Bydgoszcz'), bydgoszcz[1], bydgoszcz[2], bydgoszcz[3])
colnames(df_bydgoszcz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bydgoszcz

Sys.sleep(runif(1,30,90)) ###Poznań ###

poznan <- read_html("https://www.XXX/offer_list?search[locations][0]=99366&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=200%20001&search[filter-price_to]=300%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
poznan <-str_replace_all(poznan, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
poznan <- regmatches(poznan, gregexpr("[[:digit:]]+", poznan))
poznan <- as.numeric(unlist(poznan))
df_poznan <- data.frame(c('Poznań'), poznan[1], poznan[2], poznan[3])
colnames(df_poznan)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_poznan


Sys.sleep(runif(1,30,90)) ###Zielona Góra ###

zielona <- read_html("https://www.XXX/offer_list?search[locations][0]=19087&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=200%20001&search[filter-price_to]=300%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zielona <-str_replace_all(zielona, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zielona <- regmatches(zielona, gregexpr("[[:digit:]]+", zielona))
zielona <- as.numeric(unlist(zielona))
df_zielona <- data.frame(c('Zielona Góra'), zielona[1], zielona[2], zielona[3])
colnames(df_zielona)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_zielona


Sys.sleep(runif(1,30,90)) ###Gorzów Wielkopolski ###

gorzow <- read_html("https://www.XXX/offer_list?search[locations][0]=19073&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=200%20001&search[filter-price_to]=300%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gorzow <-str_replace_all(gorzow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gorzow <- regmatches(gorzow, gregexpr("[[:digit:]]+", gorzow))
gorzow <- as.numeric(unlist(gorzow))
df_gorzow <- data.frame(c('Gorzów Wielkopolski'), gorzow[1], gorzow[2], gorzow[3])
colnames(df_gorzow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gorzow

Sys.sleep(runif(1,30,90)) ###Wrocław ###

wroclaw <- read_html("https://www.XXX/offer_list?search[locations][0]=3548&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=200%20001&search[filter-price_to]=300%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wroclaw <-str_replace_all(wroclaw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wroclaw <- regmatches(wroclaw, gregexpr("[[:digit:]]+", wroclaw))
wroclaw <- as.numeric(unlist(wroclaw))
df_wroclaw <- data.frame(c('Wrocław'), wroclaw[1], wroclaw[2], wroclaw[3])
colnames(df_wroclaw)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_wroclaw


Sys.sleep(runif(1,30,90)) ###Łódź ###

lodz <- read_html("https://www.XXX/offer_list?search[locations][0]=27391&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=200%20001&search[filter-price_to]=300%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lodz <-str_replace_all(lodz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lodz <- regmatches(lodz, gregexpr("[[:digit:]]+", lodz))
lodz <- as.numeric(unlist(lodz))
df_lodz <- data.frame(c('Łódź'), lodz[1], lodz[2], lodz[3])
colnames(df_lodz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lodz

Sys.sleep(runif(1,30,90)) ###Lublin ###

lublin <- read_html("https://www.XXX/offer_list?search[locations][0]=17291&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=200%20001&search[filter-price_to]=300%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lublin <-str_replace_all(lublin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lublin <- regmatches(lublin, gregexpr("[[:digit:]]+", lublin))
lublin <- as.numeric(unlist(lublin))
df_lublin <- data.frame(c('Lublin'), lublin[1], lublin[2], lublin[3])
colnames(df_lublin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lublin


Sys.sleep(runif(1,30,90)) ###Kielce ###

kielce <- read_html("https://www.XXX/offer_list?search[locations][0]=86670&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=200%20001&search[filter-price_to]=300%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kielce <-str_replace_all(kielce, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kielce <- regmatches(kielce, gregexpr("[[:digit:]]+", kielce))
kielce <- as.numeric(unlist(kielce))
df_kielce <- data.frame(c('Kielce'), kielce[1], kielce[2], kielce[3])
colnames(df_kielce)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_kielce


Sys.sleep(runif(1,30,90)) ###Opole ###

opole <- read_html("https://www.XXX/offer_list?search[locations][0]=57175&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=200%20001&search[filter-price_to]=300%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opole <-str_replace_all(opole, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opole <- regmatches(opole, gregexpr("[[:digit:]]+", opole))
opole <- as.numeric(unlist(opole))
df_opole <- data.frame(c('Opole'), opole[1], opole[2], opole[3])
colnames(df_opole)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_opole


Sys.sleep(runif(1,30,90)) ###Katowice ###

katowice <- read_html("https://www.XXX/offer_list?search[locations][0]=79344&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=200%20001&search[filter-price_to]=300%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
katowice <-str_replace_all(katowice, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
katowice <- regmatches(katowice, gregexpr("[[:digit:]]+", katowice))
katowice <- as.numeric(unlist(katowice))
df_katowice <- data.frame(c('Katowice'), katowice[1], katowice[2], katowice[3])
colnames(df_katowice)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_katowice


Sys.sleep(runif(1,30,90)) ###Kraków ###

krakow <- read_html("https://www.XXX/offer_list?search[locations][0]=41878&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=200%20001&search[filter-price_to]=300%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
krakow <-str_replace_all(krakow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
krakow <- regmatches(krakow, gregexpr("[[:digit:]]+", krakow))
krakow <- as.numeric(unlist(krakow))
df_krakow <- data.frame(c('Kraków'), krakow[1], krakow[2], krakow[3])
colnames(df_krakow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_krakow

Sys.sleep(runif(1,30,90)) ###Rzeszów ###

rzeszow <- read_html("https://www.XXX/offer_list?search[locations][0]=64847&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=200%20001&search[filter-price_to]=300%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
rzeszow <-str_replace_all(rzeszow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
rzeszow <- regmatches(rzeszow, gregexpr("[[:digit:]]+", rzeszow))
rzeszow <- as.numeric(unlist(rzeszow))
df_rzeszow <- data.frame(c('Rzeszów'), rzeszow[1], rzeszow[2], rzeszow[3])
colnames(df_rzeszow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_rzeszow


oferty_miasta_sprz300 <- rbind(df_bialystok,df_bydgoszcz,df_gdansk,df_gorzow,df_katowice,df_kielce,df_krakow,
                               df_lodz,df_lublin,df_olsztyn,df_opole,df_poznan,df_rzeszow,df_szczecin,
                               df_torun,df_warszawa,df_wroclaw,df_zielona)
oferty_miasta_sprz300                   



write_xlsx(x = oferty_miasta_sprz300, path = "oferty_miasta_sprz300.xlsx", col_names = TRUE)


###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################



### SKRYPT  300-400 TYS. ZŁ ###

Sys.sleep(runif(1,30,90)) ###Szczecin ###

szczecin <- read_html("https://www.XXX/offer_list?search[locations][0]=103059&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=300%20001&search[filter-price_to]=400%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
szczecin <-str_replace_all(szczecin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
szczecin <- regmatches(szczecin, gregexpr("[[:digit:]]+", szczecin))
szczecin <- as.numeric(unlist(szczecin))
df_szczecin <- data.frame(c('Szczecin'), szczecin[1], szczecin[2], szczecin[3])
colnames(df_szczecin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta') 
df_szczecin

Sys.sleep(runif(1,30,90)) ###Gdańsk ###

gdansk <- read_html("https://www.XXX/offer_list?search[locations][0]=74287&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=300%20001&search[filter-price_to]=400%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gdansk <-str_replace_all(gdansk, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gdansk <- regmatches(gdansk, gregexpr("[[:digit:]]+", gdansk))
gdansk <- as.numeric(unlist(gdansk))
df_gdansk <- data.frame(c('Gdańsk'), gdansk[1], gdansk[2], gdansk[3])
colnames(df_gdansk)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gdansk

Sys.sleep(runif(1,30,90)) ###Olsztyn ###

olsztyn <- read_html("https://www.XXX/offer_list?search[locations][0]=91529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=300%20001&search[filter-price_to]=400%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
olsztyn <-str_replace_all(olsztyn, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
olsztyn <- regmatches(olsztyn, gregexpr("[[:digit:]]+", olsztyn))
olsztyn <- as.numeric(unlist(olsztyn))
df_olsztyn <- data.frame(c('Olsztyn'), olsztyn[1], olsztyn[2], olsztyn[3])
colnames(df_olsztyn)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_olsztyn

Sys.sleep(runif(1,30,90)) ###Białystok ###

bialystok <- read_html("https://www.XXX/offer_list?search[locations][0]=70081&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=300%20001&search[filter-price_to]=400%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bialystok <-str_replace_all(bialystok, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bialystok <- regmatches(bialystok, gregexpr("[[:digit:]]+", bialystok))
bialystok <- as.numeric(unlist(bialystok))
df_bialystok <- data.frame(c('Białystok'), bialystok[1], bialystok[2], bialystok[3])
colnames(df_bialystok)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bialystok

Sys.sleep(runif(1,30,90)) ###Warszawa ###

warszawa <- read_html("https://www.XXX/offer_list?search[locations][0]=55060&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=300%20001&search[filter-price_to]=400%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
warszawa <-str_replace_all(warszawa, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
warszawa <- regmatches(warszawa, gregexpr("[[:digit:]]+", warszawa))
warszawa <- as.numeric(unlist(warszawa))
df_warszawa <- data.frame(c('Warszawa'), warszawa[1], warszawa[2], warszawa[3])
colnames(df_warszawa)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_warszawa


Sys.sleep(runif(1,30,90)) ###Toruń ###

torun <- read_html("https://www.XXX/offer_list?search[locations][0]=9292&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=300%20001&search[filter-price_to]=400%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
torun <-str_replace_all(torun, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
torun <- regmatches(torun, gregexpr("[[:digit:]]+", torun))
torun <- as.numeric(unlist(torun))
df_torun <- data.frame(c('Toruń'), torun[1], torun[2], torun[3])
colnames(df_torun)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_torun

Sys.sleep(runif(1,30,90)) ###Bydgoszcz ###

bydgoszcz <- read_html("https://www.XXX/offer_list?search[locations][0]=9232&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=300%20001&search[filter-price_to]=400%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bydgoszcz <-str_replace_all(bydgoszcz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bydgoszcz <- regmatches(bydgoszcz, gregexpr("[[:digit:]]+", bydgoszcz))
bydgoszcz <- as.numeric(unlist(bydgoszcz))
df_bydgoszcz <- data.frame(c('Bydgoszcz'), bydgoszcz[1], bydgoszcz[2], bydgoszcz[3])
colnames(df_bydgoszcz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bydgoszcz

Sys.sleep(runif(1,30,90)) ###Poznań ###

poznan <- read_html("https://www.XXX/offer_list?search[locations][0]=99366&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=300%20001&search[filter-price_to]=400%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
poznan <-str_replace_all(poznan, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
poznan <- regmatches(poznan, gregexpr("[[:digit:]]+", poznan))
poznan <- as.numeric(unlist(poznan))
df_poznan <- data.frame(c('Poznań'), poznan[1], poznan[2], poznan[3])
colnames(df_poznan)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_poznan


Sys.sleep(runif(1,30,90)) ###Zielona Góra ###

zielona <- read_html("https://www.XXX/offer_list?search[locations][0]=19087&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=300%20001&search[filter-price_to]=400%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zielona <-str_replace_all(zielona, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zielona <- regmatches(zielona, gregexpr("[[:digit:]]+", zielona))
zielona <- as.numeric(unlist(zielona))
df_zielona <- data.frame(c('Zielona Góra'), zielona[1], zielona[2], zielona[3])
colnames(df_zielona)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_zielona


Sys.sleep(runif(1,30,90)) ###Gorzów Wielkopolski ###

gorzow <- read_html("https://www.XXX/offer_list?search[locations][0]=19073&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=300%20001&search[filter-price_to]=400%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gorzow <-str_replace_all(gorzow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gorzow <- regmatches(gorzow, gregexpr("[[:digit:]]+", gorzow))
gorzow <- as.numeric(unlist(gorzow))
df_gorzow <- data.frame(c('Gorzów Wielkopolski'), gorzow[1], gorzow[2], gorzow[3])
colnames(df_gorzow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gorzow

Sys.sleep(runif(1,30,90)) ###Wrocław ###

wroclaw <- read_html("https://www.XXX/offer_list?search[locations][0]=3548&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=300%20001&search[filter-price_to]=400%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wroclaw <-str_replace_all(wroclaw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wroclaw <- regmatches(wroclaw, gregexpr("[[:digit:]]+", wroclaw))
wroclaw <- as.numeric(unlist(wroclaw))
df_wroclaw <- data.frame(c('Wrocław'), wroclaw[1], wroclaw[2], wroclaw[3])
colnames(df_wroclaw)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_wroclaw


Sys.sleep(runif(1,30,90)) ###Łódź ###

lodz <- read_html("https://www.XXX/offer_list?search[locations][0]=27391&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=300%20001&search[filter-price_to]=400%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lodz <-str_replace_all(lodz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lodz <- regmatches(lodz, gregexpr("[[:digit:]]+", lodz))
lodz <- as.numeric(unlist(lodz))
df_lodz <- data.frame(c('Łódź'), lodz[1], lodz[2], lodz[3])
colnames(df_lodz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lodz

Sys.sleep(runif(1,30,90)) ###Lublin ###

lublin <- read_html("https://www.XXX/offer_list?search[locations][0]=17291&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=300%20001&search[filter-price_to]=400%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lublin <-str_replace_all(lublin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lublin <- regmatches(lublin, gregexpr("[[:digit:]]+", lublin))
lublin <- as.numeric(unlist(lublin))
df_lublin <- data.frame(c('Lublin'), lublin[1], lublin[2], lublin[3])
colnames(df_lublin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lublin


Sys.sleep(runif(1,30,90)) ###Kielce ###

kielce <- read_html("https://www.XXX/offer_list?search[locations][0]=86670&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=300%20001&search[filter-price_to]=400%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kielce <-str_replace_all(kielce, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kielce <- regmatches(kielce, gregexpr("[[:digit:]]+", kielce))
kielce <- as.numeric(unlist(kielce))
df_kielce <- data.frame(c('Kielce'), kielce[1], kielce[2], kielce[3])
colnames(df_kielce)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_kielce


Sys.sleep(runif(1,30,90)) ###Opole ###

opole <- read_html("https://www.XXX/offer_list?search[locations][0]=57175&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=300%20001&search[filter-price_to]=400%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opole <-str_replace_all(opole, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opole <- regmatches(opole, gregexpr("[[:digit:]]+", opole))
opole <- as.numeric(unlist(opole))
df_opole <- data.frame(c('Opole'), opole[1], opole[2], opole[3])
colnames(df_opole)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_opole


Sys.sleep(runif(1,30,90)) ###Katowice ###

katowice <- read_html("https://www.XXX/offer_list?search[locations][0]=79344&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=300%20001&search[filter-price_to]=400%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
katowice <-str_replace_all(katowice, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
katowice <- regmatches(katowice, gregexpr("[[:digit:]]+", katowice))
katowice <- as.numeric(unlist(katowice))
df_katowice <- data.frame(c('Katowice'), katowice[1], katowice[2], katowice[3])
colnames(df_katowice)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_katowice


Sys.sleep(runif(1,30,90)) ###Kraków ###

krakow <- read_html("https://www.XXX/offer_list?search[locations][0]=41878&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=300%20001&search[filter-price_to]=400%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
krakow <-str_replace_all(krakow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
krakow <- regmatches(krakow, gregexpr("[[:digit:]]+", krakow))
krakow <- as.numeric(unlist(krakow))
df_krakow <- data.frame(c('Kraków'), krakow[1], krakow[2], krakow[3])
colnames(df_krakow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_krakow

Sys.sleep(runif(1,30,90)) ###Rzeszów ###

rzeszow <- read_html("https://www.XXX/offer_list?search[locations][0]=64847&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=300%20001&search[filter-price_to]=400%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
rzeszow <-str_replace_all(rzeszow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
rzeszow <- regmatches(rzeszow, gregexpr("[[:digit:]]+", rzeszow))
rzeszow <- as.numeric(unlist(rzeszow))
df_rzeszow <- data.frame(c('Rzeszów'), rzeszow[1], rzeszow[2], rzeszow[3])
colnames(df_rzeszow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_rzeszow


oferty_miasta_sprz400 <- rbind(df_bialystok,df_bydgoszcz,df_gdansk,df_gorzow,df_katowice,df_kielce,df_krakow,
                               df_lodz,df_lublin,df_olsztyn,df_opole,df_poznan,df_rzeszow,df_szczecin,
                               df_torun,df_warszawa,df_wroclaw,df_zielona)
oferty_miasta_sprz400                   



write_xlsx(x = oferty_miasta_sprz400, path = "oferty_miasta_sprz400.xlsx", col_names = TRUE)


###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################



### SKRYPT  400-500 TYS. ZŁ ###

Sys.sleep(runif(1,30,90)) ###Szczecin ###

szczecin <- read_html("https://www.XXX/offer_list?search[locations][0]=103059&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=400%20001&search[filter-price_to]=500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
szczecin <-str_replace_all(szczecin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
szczecin <- regmatches(szczecin, gregexpr("[[:digit:]]+", szczecin))
szczecin <- as.numeric(unlist(szczecin))
df_szczecin <- data.frame(c('Szczecin'), szczecin[1], szczecin[2], szczecin[3])
colnames(df_szczecin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta') 
df_szczecin

Sys.sleep(runif(1,30,90)) ###Gdańsk ###

gdansk <- read_html("https://www.XXX/offer_list?search[locations][0]=74287&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=400%20001&search[filter-price_to]=500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gdansk <-str_replace_all(gdansk, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gdansk <- regmatches(gdansk, gregexpr("[[:digit:]]+", gdansk))
gdansk <- as.numeric(unlist(gdansk))
df_gdansk <- data.frame(c('Gdańsk'), gdansk[1], gdansk[2], gdansk[3])
colnames(df_gdansk)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gdansk

Sys.sleep(runif(1,30,90)) ###Olsztyn ###

olsztyn <- read_html("https://www.XXX/offer_list?search[locations][0]=91529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=400%20001&search[filter-price_to]=500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
olsztyn <-str_replace_all(olsztyn, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
olsztyn <- regmatches(olsztyn, gregexpr("[[:digit:]]+", olsztyn))
olsztyn <- as.numeric(unlist(olsztyn))
df_olsztyn <- data.frame(c('Olsztyn'), olsztyn[1], olsztyn[2], olsztyn[3])
colnames(df_olsztyn)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_olsztyn

Sys.sleep(runif(1,30,90)) ###Białystok ###

bialystok <- read_html("https://www.XXX/offer_list?search[locations][0]=70081&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=400%20001&search[filter-price_to]=500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bialystok <-str_replace_all(bialystok, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bialystok <- regmatches(bialystok, gregexpr("[[:digit:]]+", bialystok))
bialystok <- as.numeric(unlist(bialystok))
df_bialystok <- data.frame(c('Białystok'), bialystok[1], bialystok[2], bialystok[3])
colnames(df_bialystok)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bialystok

Sys.sleep(runif(1,30,90)) ###Warszawa ###

warszawa <- read_html("https://www.XXX/offer_list?search[locations][0]=55060&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=400%20001&search[filter-price_to]=500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
warszawa <-str_replace_all(warszawa, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
warszawa <- regmatches(warszawa, gregexpr("[[:digit:]]+", warszawa))
warszawa <- as.numeric(unlist(warszawa))
df_warszawa <- data.frame(c('Warszawa'), warszawa[1], warszawa[2], warszawa[3])
colnames(df_warszawa)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_warszawa


Sys.sleep(runif(1,30,90)) ###Toruń ###

torun <- read_html("https://www.XXX/offer_list?search[locations][0]=9292&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=400%20001&search[filter-price_to]=500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
torun <-str_replace_all(torun, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
torun <- regmatches(torun, gregexpr("[[:digit:]]+", torun))
torun <- as.numeric(unlist(torun))
df_torun <- data.frame(c('Toruń'), torun[1], torun[2], torun[3])
colnames(df_torun)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_torun

Sys.sleep(runif(1,30,90)) ###Bydgoszcz ###

bydgoszcz <- read_html("https://www.XXX/offer_list?search[locations][0]=9232&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=400%20001&search[filter-price_to]=500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bydgoszcz <-str_replace_all(bydgoszcz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bydgoszcz <- regmatches(bydgoszcz, gregexpr("[[:digit:]]+", bydgoszcz))
bydgoszcz <- as.numeric(unlist(bydgoszcz))
df_bydgoszcz <- data.frame(c('Bydgoszcz'), bydgoszcz[1], bydgoszcz[2], bydgoszcz[3])
colnames(df_bydgoszcz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bydgoszcz

Sys.sleep(runif(1,30,90)) ###Poznań ###

poznan <- read_html("https://www.XXX/offer_list?search[locations][0]=99366&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=400%20001&search[filter-price_to]=500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
poznan <-str_replace_all(poznan, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
poznan <- regmatches(poznan, gregexpr("[[:digit:]]+", poznan))
poznan <- as.numeric(unlist(poznan))
df_poznan <- data.frame(c('Poznań'), poznan[1], poznan[2], poznan[3])
colnames(df_poznan)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_poznan


Sys.sleep(runif(1,30,90)) ###Zielona Góra ###

zielona <- read_html("https://www.XXX/offer_list?search[locations][0]=19087&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=400%20001&search[filter-price_to]=500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zielona <-str_replace_all(zielona, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zielona <- regmatches(zielona, gregexpr("[[:digit:]]+", zielona))
zielona <- as.numeric(unlist(zielona))
df_zielona <- data.frame(c('Zielona Góra'), zielona[1], zielona[2], zielona[3])
colnames(df_zielona)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_zielona


Sys.sleep(runif(1,30,90)) ###Gorzów Wielkopolski ###

gorzow <- read_html("https://www.XXX/offer_list?search[locations][0]=19073&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=400%20001&search[filter-price_to]=500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gorzow <-str_replace_all(gorzow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gorzow <- regmatches(gorzow, gregexpr("[[:digit:]]+", gorzow))
gorzow <- as.numeric(unlist(gorzow))
df_gorzow <- data.frame(c('Gorzów Wielkopolski'), gorzow[1], gorzow[2], gorzow[3])
colnames(df_gorzow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gorzow

Sys.sleep(runif(1,30,90)) ###Wrocław ###

wroclaw <- read_html("https://www.XXX/offer_list?search[locations][0]=3548&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=400%20001&search[filter-price_to]=500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wroclaw <-str_replace_all(wroclaw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wroclaw <- regmatches(wroclaw, gregexpr("[[:digit:]]+", wroclaw))
wroclaw <- as.numeric(unlist(wroclaw))
df_wroclaw <- data.frame(c('Wrocław'), wroclaw[1], wroclaw[2], wroclaw[3])
colnames(df_wroclaw)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_wroclaw


Sys.sleep(runif(1,30,90)) ###Łódź ###

lodz <- read_html("https://www.XXX/offer_list?search[locations][0]=27391&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=400%20001&search[filter-price_to]=500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lodz <-str_replace_all(lodz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lodz <- regmatches(lodz, gregexpr("[[:digit:]]+", lodz))
lodz <- as.numeric(unlist(lodz))
df_lodz <- data.frame(c('Łódź'), lodz[1], lodz[2], lodz[3])
colnames(df_lodz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lodz

Sys.sleep(runif(1,30,90)) ###Lublin ###

lublin <- read_html("https://www.XXX/offer_list?search[locations][0]=17291&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=400%20001&search[filter-price_to]=500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lublin <-str_replace_all(lublin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lublin <- regmatches(lublin, gregexpr("[[:digit:]]+", lublin))
lublin <- as.numeric(unlist(lublin))
df_lublin <- data.frame(c('Lublin'), lublin[1], lublin[2], lublin[3])
colnames(df_lublin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lublin


Sys.sleep(runif(1,30,90)) ###Kielce ###

kielce <- read_html("https://www.XXX/offer_list?search[locations][0]=86670&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=400%20001&search[filter-price_to]=500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kielce <-str_replace_all(kielce, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kielce <- regmatches(kielce, gregexpr("[[:digit:]]+", kielce))
kielce <- as.numeric(unlist(kielce))
df_kielce <- data.frame(c('Kielce'), kielce[1], kielce[2], kielce[3])
colnames(df_kielce)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_kielce


Sys.sleep(runif(1,30,90)) ###Opole ###

opole <- read_html("https://www.XXX/offer_list?search[locations][0]=57175&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=400%20001&search[filter-price_to]=500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opole <-str_replace_all(opole, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opole <- regmatches(opole, gregexpr("[[:digit:]]+", opole))
opole <- as.numeric(unlist(opole))
df_opole <- data.frame(c('Opole'), opole[1], opole[2], opole[3])
colnames(df_opole)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_opole


Sys.sleep(runif(1,30,90)) ###Katowice ###

katowice <- read_html("https://www.XXX/offer_list?search[locations][0]=79344&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=400%20001&search[filter-price_to]=500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
katowice <-str_replace_all(katowice, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
katowice <- regmatches(katowice, gregexpr("[[:digit:]]+", katowice))
katowice <- as.numeric(unlist(katowice))
df_katowice <- data.frame(c('Katowice'), katowice[1], katowice[2], katowice[3])
colnames(df_katowice)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_katowice


Sys.sleep(runif(1,30,90)) ###Kraków ###

krakow <- read_html("https://www.XXX/offer_list?search[locations][0]=41878&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=400%20001&search[filter-price_to]=500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
krakow <-str_replace_all(krakow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
krakow <- regmatches(krakow, gregexpr("[[:digit:]]+", krakow))
krakow <- as.numeric(unlist(krakow))
df_krakow <- data.frame(c('Kraków'), krakow[1], krakow[2], krakow[3])
colnames(df_krakow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_krakow

Sys.sleep(runif(1,30,90)) ###Rzeszów ###

rzeszow <- read_html("https://www.XXX/offer_list?search[locations][0]=64847&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=400%20001&search[filter-price_to]=500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
rzeszow <-str_replace_all(rzeszow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
rzeszow <- regmatches(rzeszow, gregexpr("[[:digit:]]+", rzeszow))
rzeszow <- as.numeric(unlist(rzeszow))
df_rzeszow <- data.frame(c('Rzeszów'), rzeszow[1], rzeszow[2], rzeszow[3])
colnames(df_rzeszow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_rzeszow


oferty_miasta_sprz500 <- rbind(df_bialystok,df_bydgoszcz,df_gdansk,df_gorzow,df_katowice,df_kielce,df_krakow,
                               df_lodz,df_lublin,df_olsztyn,df_opole,df_poznan,df_rzeszow,df_szczecin,
                               df_torun,df_warszawa,df_wroclaw,df_zielona)
oferty_miasta_sprz500                   



write_xlsx(x = oferty_miasta_sprz500, path = "oferty_miasta_sprz500.xlsx", col_names = TRUE)


###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################



### SKRYPT  500-750 TYS. ZŁ ###

Sys.sleep(runif(1,30,90)) ###Szczecin ###


szczecin <- read_html("https://www.XXX/offer_list?search[locations][0]=103059&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=500%20001&search[filter-price_to]=750000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
szczecin <-str_replace_all(szczecin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
szczecin <- regmatches(szczecin, gregexpr("[[:digit:]]+", szczecin))
szczecin <- as.numeric(unlist(szczecin))
df_szczecin <- data.frame(c('Szczecin'), szczecin[1], szczecin[2], szczecin[3])
colnames(df_szczecin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta') 
df_szczecin

Sys.sleep(runif(1,30,90)) ###Gdańsk ###

gdansk <- read_html("https://www.XXX/offer_list?search[locations][0]=74287&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=500%20001&search[filter-price_to]=750%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gdansk <-str_replace_all(gdansk, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gdansk <- regmatches(gdansk, gregexpr("[[:digit:]]+", gdansk))
gdansk <- as.numeric(unlist(gdansk))
df_gdansk <- data.frame(c('Gdańsk'), gdansk[1], gdansk[2], gdansk[3])
colnames(df_gdansk)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gdansk

Sys.sleep(runif(1,30,90)) ###Olsztyn ###

olsztyn <- read_html("https://www.XXX/offer_list?search[locations][0]=91529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=500%20001&search[filter-price_to]=750%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
olsztyn <-str_replace_all(olsztyn, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
olsztyn <- regmatches(olsztyn, gregexpr("[[:digit:]]+", olsztyn))
olsztyn <- as.numeric(unlist(olsztyn))
df_olsztyn <- data.frame(c('Olsztyn'), olsztyn[1], olsztyn[2], olsztyn[3])
colnames(df_olsztyn)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_olsztyn

Sys.sleep(runif(1,30,90)) ###Białystok ###

bialystok <- read_html("https://www.XXX/offer_list?search[locations][0]=70081&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=500%20001&search[filter-price_to]=750%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bialystok <-str_replace_all(bialystok, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bialystok <- regmatches(bialystok, gregexpr("[[:digit:]]+", bialystok))
bialystok <- as.numeric(unlist(bialystok))
df_bialystok <- data.frame(c('Białystok'), bialystok[1], bialystok[2], bialystok[3])
colnames(df_bialystok)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bialystok

Sys.sleep(runif(1,30,90)) ###Warszawa ###

warszawa <- read_html("https://www.XXX/offer_list?search[locations][0]=55060&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=500%20001&search[filter-price_to]=750%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
warszawa <-str_replace_all(warszawa, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
warszawa <- regmatches(warszawa, gregexpr("[[:digit:]]+", warszawa))
warszawa <- as.numeric(unlist(warszawa))
df_warszawa <- data.frame(c('Warszawa'), warszawa[1], warszawa[2], warszawa[3])
colnames(df_warszawa)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_warszawa


Sys.sleep(runif(1,30,90)) ###Toruń ###

torun <- read_html("https://www.XXX/offer_list?search[locations][0]=9292&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=500%20001&search[filter-price_to]=750%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
torun <-str_replace_all(torun, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
torun <- regmatches(torun, gregexpr("[[:digit:]]+", torun))
torun <- as.numeric(unlist(torun))
df_torun <- data.frame(c('Toruń'), torun[1], torun[2], torun[3])
colnames(df_torun)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_torun

Sys.sleep(runif(1,30,90)) ###Bydgoszcz ###

bydgoszcz <- read_html("https://www.XXX/offer_list?search[locations][0]=9232&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=500%20001&search[filter-price_to]=750%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bydgoszcz <-str_replace_all(bydgoszcz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bydgoszcz <- regmatches(bydgoszcz, gregexpr("[[:digit:]]+", bydgoszcz))
bydgoszcz <- as.numeric(unlist(bydgoszcz))
df_bydgoszcz <- data.frame(c('Bydgoszcz'), bydgoszcz[1], bydgoszcz[2], bydgoszcz[3])
colnames(df_bydgoszcz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bydgoszcz

Sys.sleep(runif(1,30,90)) ###Poznań ###

poznan <- read_html("https://www.XXX/offer_list?search[locations][0]=99366&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=500%20001&search[filter-price_to]=750%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
poznan <-str_replace_all(poznan, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
poznan <- regmatches(poznan, gregexpr("[[:digit:]]+", poznan))
poznan <- as.numeric(unlist(poznan))
df_poznan <- data.frame(c('Poznań'), poznan[1], poznan[2], poznan[3])
colnames(df_poznan)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_poznan


Sys.sleep(runif(1,30,90)) ###Zielona Góra ###

zielona <- read_html("https://www.XXX/offer_list?search[locations][0]=19087&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=500%20001&search[filter-price_to]=750%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zielona <-str_replace_all(zielona, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zielona <- regmatches(zielona, gregexpr("[[:digit:]]+", zielona))
zielona <- as.numeric(unlist(zielona))
df_zielona <- data.frame(c('Zielona Góra'), zielona[1], zielona[2], zielona[3])
colnames(df_zielona)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_zielona


Sys.sleep(runif(1,30,90)) ###Gorzów Wielkopolski ###

gorzow <- read_html("https://www.XXX/offer_list?search[locations][0]=19073&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=500%20001&search[filter-price_to]=750%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gorzow <-str_replace_all(gorzow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gorzow <- regmatches(gorzow, gregexpr("[[:digit:]]+", gorzow))
gorzow <- as.numeric(unlist(gorzow))
df_gorzow <- data.frame(c('Gorzów Wielkopolski'), gorzow[1], gorzow[2], gorzow[3])
colnames(df_gorzow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gorzow

Sys.sleep(runif(1,30,90)) ###Wrocław ###

wroclaw <- read_html("https://www.XXX/offer_list?search[locations][0]=3548&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=500%20001&search[filter-price_to]=750%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wroclaw <-str_replace_all(wroclaw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wroclaw <- regmatches(wroclaw, gregexpr("[[:digit:]]+", wroclaw))
wroclaw <- as.numeric(unlist(wroclaw))
df_wroclaw <- data.frame(c('Wrocław'), wroclaw[1], wroclaw[2], wroclaw[3])
colnames(df_wroclaw)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_wroclaw


Sys.sleep(runif(1,30,90)) ###Łódź ###

lodz <- read_html("https://www.XXX/offer_list?search[locations][0]=27391&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=500%20001&search[filter-price_to]=750%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lodz <-str_replace_all(lodz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lodz <- regmatches(lodz, gregexpr("[[:digit:]]+", lodz))
lodz <- as.numeric(unlist(lodz))
df_lodz <- data.frame(c('Łódź'), lodz[1], lodz[2], lodz[3])
colnames(df_lodz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lodz

Sys.sleep(runif(1,30,90)) ###Lublin ###

lublin <- read_html("https://www.XXX/offer_list?search[locations][0]=17291&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=500%20001&search[filter-price_to]=750%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lublin <-str_replace_all(lublin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lublin <- regmatches(lublin, gregexpr("[[:digit:]]+", lublin))
lublin <- as.numeric(unlist(lublin))
df_lublin <- data.frame(c('Lublin'), lublin[1], lublin[2], lublin[3])
colnames(df_lublin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lublin


Sys.sleep(runif(1,30,90)) ###Kielce ###

kielce <- read_html("https://www.XXX/offer_list?search[locations][0]=86670&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=500%20001&search[filter-price_to]=750%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kielce <-str_replace_all(kielce, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kielce <- regmatches(kielce, gregexpr("[[:digit:]]+", kielce))
kielce <- as.numeric(unlist(kielce))
df_kielce <- data.frame(c('Kielce'), kielce[1], kielce[2], kielce[3])
colnames(df_kielce)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_kielce


Sys.sleep(runif(1,30,90)) ###Opole ###

opole <- read_html("https://www.XXX/offer_list?search[locations][0]=57175&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=500%20001&search[filter-price_to]=750%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opole <-str_replace_all(opole, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opole <- regmatches(opole, gregexpr("[[:digit:]]+", opole))
opole <- as.numeric(unlist(opole))
df_opole <- data.frame(c('Opole'), opole[1], opole[2], opole[3])
colnames(df_opole)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_opole


Sys.sleep(runif(1,30,90)) ###Katowice ###

katowice <- read_html("https://www.XXX/offer_list?search[locations][0]=79344&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=500%20001&search[filter-price_to]=750%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
katowice <-str_replace_all(katowice, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
katowice <- regmatches(katowice, gregexpr("[[:digit:]]+", katowice))
katowice <- as.numeric(unlist(katowice))
df_katowice <- data.frame(c('Katowice'), katowice[1], katowice[2], katowice[3])
colnames(df_katowice)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_katowice


Sys.sleep(runif(1,30,90)) ###Kraków ###

krakow <- read_html("https://www.XXX/offer_list?search[locations][0]=41878&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=500%20001&search[filter-price_to]=750%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
krakow <-str_replace_all(krakow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
krakow <- regmatches(krakow, gregexpr("[[:digit:]]+", krakow))
krakow <- as.numeric(unlist(krakow))
df_krakow <- data.frame(c('Kraków'), krakow[1], krakow[2], krakow[3])
colnames(df_krakow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_krakow

Sys.sleep(runif(1,30,90)) ###Rzeszów ###

rzeszow <- read_html("https://www.XXX/offer_list?search[locations][0]=64847&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=500%20001&search[filter-price_to]=750%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
rzeszow <-str_replace_all(rzeszow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
rzeszow <- regmatches(rzeszow, gregexpr("[[:digit:]]+", rzeszow))
rzeszow <- as.numeric(unlist(rzeszow))
df_rzeszow <- data.frame(c('Rzeszów'), rzeszow[1], rzeszow[2], rzeszow[3])
colnames(df_rzeszow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_rzeszow


oferty_miasta_sprz750 <- rbind(df_bialystok,df_bydgoszcz,df_gdansk,df_gorzow,df_katowice,df_kielce,df_krakow,
                               df_lodz,df_lublin,df_olsztyn,df_opole,df_poznan,df_rzeszow,df_szczecin,
                               df_torun,df_warszawa,df_wroclaw,df_zielona)
oferty_miasta_sprz750                   



write_xlsx(x = oferty_miasta_sprz750, path = "oferty_miasta_sprz750.xlsx", col_names = TRUE)



###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################

### SKRYPT  750-1 MLN ZŁ ###

Sys.sleep(runif(1,30,90)) ###Szczecin ###


szczecin <- read_html("https://www.XXX/offer_list?search[locations][0]=103059&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=750001&search[filter-price_to]=1000000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
szczecin <-str_replace_all(szczecin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
szczecin <- regmatches(szczecin, gregexpr("[[:digit:]]+", szczecin))
szczecin <- as.numeric(unlist(szczecin))
df_szczecin <- data.frame(c('Szczecin'), szczecin[1], szczecin[2], szczecin[3])
colnames(df_szczecin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta') 
df_szczecin

Sys.sleep(runif(1,30,90)) ###Gdańsk ###

gdansk <- read_html("https://www.XXX/offer_list?search[locations][0]=74287&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=750%20001&search[filter-price_to]=1%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gdansk <-str_replace_all(gdansk, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gdansk <- regmatches(gdansk, gregexpr("[[:digit:]]+", gdansk))
gdansk <- as.numeric(unlist(gdansk))
df_gdansk <- data.frame(c('Gdańsk'), gdansk[1], gdansk[2], gdansk[3])
colnames(df_gdansk)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gdansk

Sys.sleep(runif(1,30,90)) ###Olsztyn ###

olsztyn <- read_html("https://www.XXX/offer_list?search[locations][0]=91529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=750%20001&search[filter-price_to]=1%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
olsztyn <-str_replace_all(olsztyn, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
olsztyn <- regmatches(olsztyn, gregexpr("[[:digit:]]+", olsztyn))
olsztyn <- as.numeric(unlist(olsztyn))
df_olsztyn <- data.frame(c('Olsztyn'), olsztyn[1], olsztyn[2], olsztyn[3])
colnames(df_olsztyn)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_olsztyn

Sys.sleep(runif(1,30,90)) ###Białystok ###

bialystok <- read_html("https://www.XXX/offer_list?search[locations][0]=70081&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=750%20001&search[filter-price_to]=1%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bialystok <-str_replace_all(bialystok, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bialystok <- regmatches(bialystok, gregexpr("[[:digit:]]+", bialystok))
bialystok <- as.numeric(unlist(bialystok))
df_bialystok <- data.frame(c('Białystok'), bialystok[1], bialystok[2], bialystok[3])
colnames(df_bialystok)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bialystok

Sys.sleep(runif(1,30,90)) ###Warszawa ###

warszawa <- read_html("https://www.XXX/offer_list?search[locations][0]=55060&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=750%20001&search[filter-price_to]=1%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
warszawa <-str_replace_all(warszawa, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
warszawa <- regmatches(warszawa, gregexpr("[[:digit:]]+", warszawa))
warszawa <- as.numeric(unlist(warszawa))
df_warszawa <- data.frame(c('Warszawa'), warszawa[1], warszawa[2], warszawa[3])
colnames(df_warszawa)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_warszawa


Sys.sleep(runif(1,30,90)) ###Toruń ###

torun <- read_html("https://www.XXX/offer_list?search[locations][0]=9292&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=750%20001&search[filter-price_to]=1%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
torun <-str_replace_all(torun, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
torun <- regmatches(torun, gregexpr("[[:digit:]]+", torun))
torun <- as.numeric(unlist(torun))
df_torun <- data.frame(c('Toruń'), torun[1], torun[2], torun[3])
colnames(df_torun)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_torun

Sys.sleep(runif(1,30,90)) ###Bydgoszcz ###

bydgoszcz <- read_html("https://www.XXX/offer_list?search[locations][0]=9232&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=750%20001&search[filter-price_to]=1%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bydgoszcz <-str_replace_all(bydgoszcz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bydgoszcz <- regmatches(bydgoszcz, gregexpr("[[:digit:]]+", bydgoszcz))
bydgoszcz <- as.numeric(unlist(bydgoszcz))
df_bydgoszcz <- data.frame(c('Bydgoszcz'), bydgoszcz[1], bydgoszcz[2], bydgoszcz[3])
colnames(df_bydgoszcz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bydgoszcz

Sys.sleep(runif(1,30,90)) ###Poznań ###

poznan <- read_html("https://www.XXX/offer_list?search[locations][0]=99366&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=750%20001&search[filter-price_to]=1%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
poznan <-str_replace_all(poznan, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
poznan <- regmatches(poznan, gregexpr("[[:digit:]]+", poznan))
poznan <- as.numeric(unlist(poznan))
df_poznan <- data.frame(c('Poznań'), poznan[1], poznan[2], poznan[3])
colnames(df_poznan)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_poznan


Sys.sleep(runif(1,30,90)) ###Zielona Góra ###

zielona <- read_html("https://www.XXX/offer_list?search[locations][0]=19087&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=750%20001&search[filter-price_to]=1%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zielona <-str_replace_all(zielona, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zielona <- regmatches(zielona, gregexpr("[[:digit:]]+", zielona))
zielona <- as.numeric(unlist(zielona))
df_zielona <- data.frame(c('Zielona Góra'), zielona[1], zielona[2], zielona[3])
colnames(df_zielona)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_zielona


Sys.sleep(runif(1,30,90)) ###Gorzów Wielkopolski ###

gorzow <- read_html("https://www.XXX/offer_list?search[locations][0]=19073&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=750%20001&search[filter-price_to]=1%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gorzow <-str_replace_all(gorzow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gorzow <- regmatches(gorzow, gregexpr("[[:digit:]]+", gorzow))
gorzow <- as.numeric(unlist(gorzow))
df_gorzow <- data.frame(c('Gorzów Wielkopolski'), gorzow[1], gorzow[2], gorzow[3])
colnames(df_gorzow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gorzow

Sys.sleep(runif(1,30,90)) ###Wrocław ###

wroclaw <- read_html("https://www.XXX/offer_list?search[locations][0]=3548&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=750%20001&search[filter-price_to]=1%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wroclaw <-str_replace_all(wroclaw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wroclaw <- regmatches(wroclaw, gregexpr("[[:digit:]]+", wroclaw))
wroclaw <- as.numeric(unlist(wroclaw))
df_wroclaw <- data.frame(c('Wrocław'), wroclaw[1], wroclaw[2], wroclaw[3])
colnames(df_wroclaw)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_wroclaw


Sys.sleep(runif(1,30,90)) ###Łódź ###

lodz <- read_html("https://www.XXX/offer_list?search[locations][0]=27391&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=750%20001&search[filter-price_to]=1%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lodz <-str_replace_all(lodz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lodz <- regmatches(lodz, gregexpr("[[:digit:]]+", lodz))
lodz <- as.numeric(unlist(lodz))
df_lodz <- data.frame(c('Łódź'), lodz[1], lodz[2], lodz[3])
colnames(df_lodz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lodz

Sys.sleep(runif(1,30,90)) ###Lublin ###

lublin <- read_html("https://www.XXX/offer_list?search[locations][0]=17291&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=750%20001&search[filter-price_to]=1%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lublin <-str_replace_all(lublin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lublin <- regmatches(lublin, gregexpr("[[:digit:]]+", lublin))
lublin <- as.numeric(unlist(lublin))
df_lublin <- data.frame(c('Lublin'), lublin[1], lublin[2], lublin[3])
colnames(df_lublin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lublin


Sys.sleep(runif(1,30,90)) ###Kielce ###

kielce <- read_html("https://www.XXX/offer_list?search[locations][0]=86670&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=750%20001&search[filter-price_to]=1%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kielce <-str_replace_all(kielce, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kielce <- regmatches(kielce, gregexpr("[[:digit:]]+", kielce))
kielce <- as.numeric(unlist(kielce))
df_kielce <- data.frame(c('Kielce'), kielce[1], kielce[2], kielce[3])
colnames(df_kielce)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_kielce


Sys.sleep(runif(1,30,90)) ###Opole ###

opole <- read_html("https://www.XXX/offer_list?search[locations][0]=57175&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=750%20001&search[filter-price_to]=1%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opole <-str_replace_all(opole, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opole <- regmatches(opole, gregexpr("[[:digit:]]+", opole))
opole <- as.numeric(unlist(opole))
df_opole <- data.frame(c('Opole'), opole[1], opole[2], opole[3])
colnames(df_opole)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_opole


Sys.sleep(runif(1,30,90)) ###Katowice ###

katowice <- read_html("https://www.XXX/offer_list?search[locations][0]=79344&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=750%20001&search[filter-price_to]=1%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
katowice <-str_replace_all(katowice, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
katowice <- regmatches(katowice, gregexpr("[[:digit:]]+", katowice))
katowice <- as.numeric(unlist(katowice))
df_katowice <- data.frame(c('Katowice'), katowice[1], katowice[2], katowice[3])
colnames(df_katowice)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_katowice


Sys.sleep(runif(1,30,90)) ###Kraków ###

krakow <- read_html("https://www.XXX/offer_list?search[locations][0]=41878&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=750%20001&search[filter-price_to]=1%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
krakow <-str_replace_all(krakow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
krakow <- regmatches(krakow, gregexpr("[[:digit:]]+", krakow))
krakow <- as.numeric(unlist(krakow))
df_krakow <- data.frame(c('Kraków'), krakow[1], krakow[2], krakow[3])
colnames(df_krakow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_krakow

Sys.sleep(runif(1,30,90)) ###Rzeszów ###

rzeszow <- read_html("https://www.XXX/offer_list?search[locations][0]=64847&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=750%20001&search[filter-price_to]=1%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
rzeszow <-str_replace_all(rzeszow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
rzeszow <- regmatches(rzeszow, gregexpr("[[:digit:]]+", rzeszow))
rzeszow <- as.numeric(unlist(rzeszow))
df_rzeszow <- data.frame(c('Rzeszów'), rzeszow[1], rzeszow[2], rzeszow[3])
colnames(df_rzeszow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_rzeszow


oferty_miasta_sprz1mln <- rbind(df_bialystok,df_bydgoszcz,df_gdansk,df_gorzow,df_katowice,df_kielce,df_krakow,
                               df_lodz,df_lublin,df_olsztyn,df_opole,df_poznan,df_rzeszow,df_szczecin,
                               df_torun,df_warszawa,df_wroclaw,df_zielona)
oferty_miasta_sprz1mln                   



write_xlsx(x = oferty_miasta_sprz1mln, path = "oferty_miasta_sprz1mln.xlsx", col_names = TRUE)



###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################

### SKRYPT  1 - 1,5 MLN ZŁ ###

Sys.sleep(runif(1,30,90)) ###Szczecin ###


szczecin <- read_html("https://www.XXX/offer_list?search[locations][0]=103059&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20000%20001&search[filter-price_to]=1500000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
szczecin <-str_replace_all(szczecin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
szczecin <- regmatches(szczecin, gregexpr("[[:digit:]]+", szczecin))
szczecin <- as.numeric(unlist(szczecin))
df_szczecin <- data.frame(c('Szczecin'), szczecin[1], szczecin[2], szczecin[3])
colnames(df_szczecin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta') 
df_szczecin

Sys.sleep(runif(1,30,90)) ###Gdańsk ###

gdansk <- read_html("https://www.XXX/offer_list?search[locations][0]=74287&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20000%20001&search[filter-price_to]=1%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gdansk <-str_replace_all(gdansk, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gdansk <- regmatches(gdansk, gregexpr("[[:digit:]]+", gdansk))
gdansk <- as.numeric(unlist(gdansk))
df_gdansk <- data.frame(c('Gdańsk'), gdansk[1], gdansk[2], gdansk[3])
colnames(df_gdansk)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gdansk

Sys.sleep(runif(1,30,90)) ###Olsztyn ###

olsztyn <- read_html("https://www.XXX/offer_list?search[locations][0]=91529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20000%20001&search[filter-price_to]=1%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
olsztyn <-str_replace_all(olsztyn, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
olsztyn <- regmatches(olsztyn, gregexpr("[[:digit:]]+", olsztyn))
olsztyn <- as.numeric(unlist(olsztyn))
df_olsztyn <- data.frame(c('Olsztyn'), olsztyn[1], olsztyn[2], olsztyn[3])
colnames(df_olsztyn)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_olsztyn

Sys.sleep(runif(1,30,90)) ###Białystok ###

bialystok <- read_html("https://www.XXX/offer_list?search[locations][0]=70081&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20000%20001&search[filter-price_to]=1%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bialystok <-str_replace_all(bialystok, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bialystok <- regmatches(bialystok, gregexpr("[[:digit:]]+", bialystok))
bialystok <- as.numeric(unlist(bialystok))
df_bialystok <- data.frame(c('Białystok'), bialystok[1], bialystok[2], bialystok[3])
colnames(df_bialystok)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bialystok

Sys.sleep(runif(1,30,90)) ###Warszawa ###

warszawa <- read_html("https://www.XXX/offer_list?search[locations][0]=55060&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20000%20001&search[filter-price_to]=1%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
warszawa <-str_replace_all(warszawa, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
warszawa <- regmatches(warszawa, gregexpr("[[:digit:]]+", warszawa))
warszawa <- as.numeric(unlist(warszawa))
df_warszawa <- data.frame(c('Warszawa'), warszawa[1], warszawa[2], warszawa[3])
colnames(df_warszawa)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_warszawa


Sys.sleep(runif(1,30,90)) ###Toruń ###

torun <- read_html("https://www.XXX/offer_list?search[locations][0]=9292&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20000%20001&search[filter-price_to]=1%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
torun <-str_replace_all(torun, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
torun <- regmatches(torun, gregexpr("[[:digit:]]+", torun))
torun <- as.numeric(unlist(torun))
df_torun <- data.frame(c('Toruń'), torun[1], torun[2], torun[3])
colnames(df_torun)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_torun

Sys.sleep(runif(1,30,90)) ###Bydgoszcz ###

bydgoszcz <- read_html("https://www.XXX/offer_list?search[locations][0]=9232&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20000%20001&search[filter-price_to]=1%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bydgoszcz <-str_replace_all(bydgoszcz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bydgoszcz <- regmatches(bydgoszcz, gregexpr("[[:digit:]]+", bydgoszcz))
bydgoszcz <- as.numeric(unlist(bydgoszcz))
df_bydgoszcz <- data.frame(c('Bydgoszcz'), bydgoszcz[1], bydgoszcz[2], bydgoszcz[3])
colnames(df_bydgoszcz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bydgoszcz

Sys.sleep(runif(1,30,90)) ###Poznań ###

poznan <- read_html("https://www.XXX/offer_list?search[locations][0]=99366&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20000%20001&search[filter-price_to]=1%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
poznan <-str_replace_all(poznan, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
poznan <- regmatches(poznan, gregexpr("[[:digit:]]+", poznan))
poznan <- as.numeric(unlist(poznan))
df_poznan <- data.frame(c('Poznań'), poznan[1], poznan[2], poznan[3])
colnames(df_poznan)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_poznan


Sys.sleep(runif(1,30,90)) ###Zielona Góra ###

zielona <- read_html("https://www.XXX/offer_list?search[locations][0]=19087&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20000%20001&search[filter-price_to]=1%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zielona <-str_replace_all(zielona, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zielona <- regmatches(zielona, gregexpr("[[:digit:]]+", zielona))
zielona <- as.numeric(unlist(zielona))
df_zielona <- data.frame(c('Zielona Góra'), zielona[1], zielona[2], zielona[3])
colnames(df_zielona)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_zielona


Sys.sleep(runif(1,30,90)) ###Gorzów Wielkopolski ###

gorzow <- read_html("https://www.XXX/offer_list?search[locations][0]=19073&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20000%20001&search[filter-price_to]=1%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gorzow <-str_replace_all(gorzow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gorzow <- regmatches(gorzow, gregexpr("[[:digit:]]+", gorzow))
gorzow <- as.numeric(unlist(gorzow))
df_gorzow <- data.frame(c('Gorzów Wielkopolski'), gorzow[1], gorzow[2], gorzow[3])
colnames(df_gorzow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gorzow

Sys.sleep(runif(1,30,90)) ###Wrocław ###

wroclaw <- read_html("https://www.XXX/offer_list?search[locations][0]=3548&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20000%20001&search[filter-price_to]=1%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wroclaw <-str_replace_all(wroclaw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wroclaw <- regmatches(wroclaw, gregexpr("[[:digit:]]+", wroclaw))
wroclaw <- as.numeric(unlist(wroclaw))
df_wroclaw <- data.frame(c('Wrocław'), wroclaw[1], wroclaw[2], wroclaw[3])
colnames(df_wroclaw)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_wroclaw


Sys.sleep(runif(1,30,90)) ###Łódź ###

lodz <- read_html("https://www.XXX/offer_list?search[locations][0]=27391&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20000%20001&search[filter-price_to]=1%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lodz <-str_replace_all(lodz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lodz <- regmatches(lodz, gregexpr("[[:digit:]]+", lodz))
lodz <- as.numeric(unlist(lodz))
df_lodz <- data.frame(c('Łódź'), lodz[1], lodz[2], lodz[3])
colnames(df_lodz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lodz

Sys.sleep(runif(1,30,90)) ###Lublin ###

lublin <- read_html("https://www.XXX/offer_list?search[locations][0]=17291&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20000%20001&search[filter-price_to]=1%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lublin <-str_replace_all(lublin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lublin <- regmatches(lublin, gregexpr("[[:digit:]]+", lublin))
lublin <- as.numeric(unlist(lublin))
df_lublin <- data.frame(c('Lublin'), lublin[1], lublin[2], lublin[3])
colnames(df_lublin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lublin


Sys.sleep(runif(1,30,90)) ###Kielce ###

kielce <- read_html("https://www.XXX/offer_list?search[locations][0]=86670&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20000%20001&search[filter-price_to]=1%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kielce <-str_replace_all(kielce, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kielce <- regmatches(kielce, gregexpr("[[:digit:]]+", kielce))
kielce <- as.numeric(unlist(kielce))
df_kielce <- data.frame(c('Kielce'), kielce[1], kielce[2], kielce[3])
colnames(df_kielce)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_kielce


Sys.sleep(runif(1,30,90)) ###Opole ###

opole <- read_html("https://www.XXX/offer_list?search[locations][0]=57175&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20000%20001&search[filter-price_to]=1%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opole <-str_replace_all(opole, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opole <- regmatches(opole, gregexpr("[[:digit:]]+", opole))
opole <- as.numeric(unlist(opole))
df_opole <- data.frame(c('Opole'), opole[1], opole[2], opole[3])
colnames(df_opole)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_opole


Sys.sleep(runif(1,30,90)) ###Katowice ###

katowice <- read_html("https://www.XXX/offer_list?search[locations][0]=79344&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20000%20001&search[filter-price_to]=1%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
katowice <-str_replace_all(katowice, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
katowice <- regmatches(katowice, gregexpr("[[:digit:]]+", katowice))
katowice <- as.numeric(unlist(katowice))
df_katowice <- data.frame(c('Katowice'), katowice[1], katowice[2], katowice[3])
colnames(df_katowice)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_katowice


Sys.sleep(runif(1,30,90)) ###Kraków ###

krakow <- read_html("https://www.XXX/offer_list?search[locations][0]=41878&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20000%20001&search[filter-price_to]=1%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
krakow <-str_replace_all(krakow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
krakow <- regmatches(krakow, gregexpr("[[:digit:]]+", krakow))
krakow <- as.numeric(unlist(krakow))
df_krakow <- data.frame(c('Kraków'), krakow[1], krakow[2], krakow[3])
colnames(df_krakow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_krakow

Sys.sleep(runif(1,30,90)) ###Rzeszów ###

rzeszow <- read_html("https://www.XXX/offer_list?search[locations][0]=64847&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20000%20001&search[filter-price_to]=1%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
rzeszow <-str_replace_all(rzeszow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
rzeszow <- regmatches(rzeszow, gregexpr("[[:digit:]]+", rzeszow))
rzeszow <- as.numeric(unlist(rzeszow))
df_rzeszow <- data.frame(c('Rzeszów'), rzeszow[1], rzeszow[2], rzeszow[3])
colnames(df_rzeszow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_rzeszow


oferty_miasta_sprz15mln <- rbind(df_bialystok,df_bydgoszcz,df_gdansk,df_gorzow,df_katowice,df_kielce,df_krakow,
                               df_lodz,df_lublin,df_olsztyn,df_opole,df_poznan,df_rzeszow,df_szczecin,
                               df_torun,df_warszawa,df_wroclaw,df_zielona)
oferty_miasta_sprz15mln                  



write_xlsx(x = oferty_miasta_sprz15mln, path = "oferty_miasta_sprz15mln.xlsx", col_names = TRUE)


###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################

### SKRYPT  1,5-2 MLN ZŁ ###

Sys.sleep(runif(1,30,90)) ###Szczecin ###


szczecin <- read_html("https://www.XXX/offer_list?search[locations][0]=103059&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20500%20001&search[filter-price_to]=2000000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
szczecin <-str_replace_all(szczecin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
szczecin <- regmatches(szczecin, gregexpr("[[:digit:]]+", szczecin))
szczecin <- as.numeric(unlist(szczecin))
df_szczecin <- data.frame(c('Szczecin'), szczecin[1], szczecin[2], szczecin[3])
colnames(df_szczecin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta') 
df_szczecin

Sys.sleep(runif(1,30,90)) ###Gdańsk ###

gdansk <- read_html("https://www.XXX/offer_list?search[locations][0]=74287&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20500%20001&search[filter-price_to]=2%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gdansk <-str_replace_all(gdansk, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gdansk <- regmatches(gdansk, gregexpr("[[:digit:]]+", gdansk))
gdansk <- as.numeric(unlist(gdansk))
df_gdansk <- data.frame(c('Gdańsk'), gdansk[1], gdansk[2], gdansk[3])
colnames(df_gdansk)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gdansk

Sys.sleep(runif(1,30,90)) ###Olsztyn ###

olsztyn <- read_html("https://www.XXX/offer_list?search[locations][0]=91529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20500%20001&search[filter-price_to]=2%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
olsztyn <-str_replace_all(olsztyn, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
olsztyn <- regmatches(olsztyn, gregexpr("[[:digit:]]+", olsztyn))
olsztyn <- as.numeric(unlist(olsztyn))
df_olsztyn <- data.frame(c('Olsztyn'), olsztyn[1], olsztyn[2], olsztyn[3])
colnames(df_olsztyn)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_olsztyn

Sys.sleep(runif(1,30,90)) ###Białystok ###

bialystok <- read_html("https://www.XXX/offer_list?search[locations][0]=70081&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20500%20001&search[filter-price_to]=2%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bialystok <-str_replace_all(bialystok, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bialystok <- regmatches(bialystok, gregexpr("[[:digit:]]+", bialystok))
bialystok <- as.numeric(unlist(bialystok))
df_bialystok <- data.frame(c('Białystok'), bialystok[1], bialystok[2], bialystok[3])
colnames(df_bialystok)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bialystok

Sys.sleep(runif(1,30,90)) ###Warszawa ###

warszawa <- read_html("https://www.XXX/offer_list?search[locations][0]=55060&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20500%20001&search[filter-price_to]=2%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
warszawa <-str_replace_all(warszawa, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
warszawa <- regmatches(warszawa, gregexpr("[[:digit:]]+", warszawa))
warszawa <- as.numeric(unlist(warszawa))
df_warszawa <- data.frame(c('Warszawa'), warszawa[1], warszawa[2], warszawa[3])
colnames(df_warszawa)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_warszawa


Sys.sleep(runif(1,30,90)) ###Toruń ###

torun <- read_html("https://www.XXX/offer_list?search[locations][0]=9292&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20500%20001&search[filter-price_to]=2%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
torun <-str_replace_all(torun, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
torun <- regmatches(torun, gregexpr("[[:digit:]]+", torun))
torun <- as.numeric(unlist(torun))
df_torun <- data.frame(c('Toruń'), torun[1], torun[2], torun[3])
colnames(df_torun)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_torun

Sys.sleep(runif(1,30,90)) ###Bydgoszcz ###

bydgoszcz <- read_html("https://www.XXX/offer_list?search[locations][0]=9232&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20500%20001&search[filter-price_to]=2%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bydgoszcz <-str_replace_all(bydgoszcz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bydgoszcz <- regmatches(bydgoszcz, gregexpr("[[:digit:]]+", bydgoszcz))
bydgoszcz <- as.numeric(unlist(bydgoszcz))
df_bydgoszcz <- data.frame(c('Bydgoszcz'), bydgoszcz[1], bydgoszcz[2], bydgoszcz[3])
colnames(df_bydgoszcz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bydgoszcz

Sys.sleep(runif(1,30,90)) ###Poznań ###

poznan <- read_html("https://www.XXX/offer_list?search[locations][0]=99366&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20500%20001&search[filter-price_to]=2%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
poznan <-str_replace_all(poznan, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
poznan <- regmatches(poznan, gregexpr("[[:digit:]]+", poznan))
poznan <- as.numeric(unlist(poznan))
df_poznan <- data.frame(c('Poznań'), poznan[1], poznan[2], poznan[3])
colnames(df_poznan)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_poznan


Sys.sleep(runif(1,30,90)) ###Zielona Góra ###

zielona <- read_html("https://www.XXX/offer_list?search[locations][0]=19087&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20500%20001&search[filter-price_to]=2%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zielona <-str_replace_all(zielona, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zielona <- regmatches(zielona, gregexpr("[[:digit:]]+", zielona))
zielona <- as.numeric(unlist(zielona))
df_zielona <- data.frame(c('Zielona Góra'), zielona[1], zielona[2], zielona[3])
colnames(df_zielona)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_zielona


Sys.sleep(runif(1,30,90)) ###Gorzów Wielkopolski ###

gorzow <- read_html("https://www.XXX/offer_list?search[locations][0]=19073&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20500%20001&search[filter-price_to]=2%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gorzow <-str_replace_all(gorzow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gorzow <- regmatches(gorzow, gregexpr("[[:digit:]]+", gorzow))
gorzow <- as.numeric(unlist(gorzow))
df_gorzow <- data.frame(c('Gorzów Wielkopolski'), gorzow[1], gorzow[2], gorzow[3])
colnames(df_gorzow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gorzow

Sys.sleep(runif(1,30,90)) ###Wrocław ###

wroclaw <- read_html("https://www.XXX/offer_list?search[locations][0]=3548&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20500%20001&search[filter-price_to]=2%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wroclaw <-str_replace_all(wroclaw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wroclaw <- regmatches(wroclaw, gregexpr("[[:digit:]]+", wroclaw))
wroclaw <- as.numeric(unlist(wroclaw))
df_wroclaw <- data.frame(c('Wrocław'), wroclaw[1], wroclaw[2], wroclaw[3])
colnames(df_wroclaw)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_wroclaw


Sys.sleep(runif(1,30,90)) ###Łódź ###

lodz <- read_html("https://www.XXX/offer_list?search[locations][0]=27391&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20500%20001&search[filter-price_to]=2%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lodz <-str_replace_all(lodz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lodz <- regmatches(lodz, gregexpr("[[:digit:]]+", lodz))
lodz <- as.numeric(unlist(lodz))
df_lodz <- data.frame(c('Łódź'), lodz[1], lodz[2], lodz[3])
colnames(df_lodz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lodz

Sys.sleep(runif(1,30,90)) ###Lublin ###

lublin <- read_html("https://www.XXX/offer_list?search[locations][0]=17291&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20500%20001&search[filter-price_to]=2%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lublin <-str_replace_all(lublin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lublin <- regmatches(lublin, gregexpr("[[:digit:]]+", lublin))
lublin <- as.numeric(unlist(lublin))
df_lublin <- data.frame(c('Lublin'), lublin[1], lublin[2], lublin[3])
colnames(df_lublin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lublin


Sys.sleep(runif(1,30,90)) ###Kielce ###

kielce <- read_html("https://www.XXX/offer_list?search[locations][0]=86670&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20500%20001&search[filter-price_to]=2%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kielce <-str_replace_all(kielce, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kielce <- regmatches(kielce, gregexpr("[[:digit:]]+", kielce))
kielce <- as.numeric(unlist(kielce))
df_kielce <- data.frame(c('Kielce'), kielce[1], kielce[2], kielce[3])
colnames(df_kielce)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_kielce


Sys.sleep(runif(1,30,90)) ###Opole ###

opole <- read_html("https://www.XXX/offer_list?search[locations][0]=57175&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20500%20001&search[filter-price_to]=2%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opole <-str_replace_all(opole, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opole <- regmatches(opole, gregexpr("[[:digit:]]+", opole))
opole <- as.numeric(unlist(opole))
df_opole <- data.frame(c('Opole'), opole[1], opole[2], opole[3])
colnames(df_opole)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_opole


Sys.sleep(runif(1,30,90)) ###Katowice ###

katowice <- read_html("https://www.XXX/offer_list?search[locations][0]=79344&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20500%20001&search[filter-price_to]=2%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
katowice <-str_replace_all(katowice, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
katowice <- regmatches(katowice, gregexpr("[[:digit:]]+", katowice))
katowice <- as.numeric(unlist(katowice))
df_katowice <- data.frame(c('Katowice'), katowice[1], katowice[2], katowice[3])
colnames(df_katowice)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_katowice


Sys.sleep(runif(1,30,90)) ###Kraków ###

krakow <- read_html("https://www.XXX/offer_list?search[locations][0]=41878&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20500%20001&search[filter-price_to]=2%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
krakow <-str_replace_all(krakow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
krakow <- regmatches(krakow, gregexpr("[[:digit:]]+", krakow))
krakow <- as.numeric(unlist(krakow))
df_krakow <- data.frame(c('Kraków'), krakow[1], krakow[2], krakow[3])
colnames(df_krakow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_krakow

Sys.sleep(runif(1,30,90)) ###Rzeszów ###

rzeszow <- read_html("https://www.XXX/offer_list?search[locations][0]=64847&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=1%20500%20001&search[filter-price_to]=2%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
rzeszow <-str_replace_all(rzeszow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
rzeszow <- regmatches(rzeszow, gregexpr("[[:digit:]]+", rzeszow))
rzeszow <- as.numeric(unlist(rzeszow))
df_rzeszow <- data.frame(c('Rzeszów'), rzeszow[1], rzeszow[2], rzeszow[3])
colnames(df_rzeszow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_rzeszow


oferty_miasta_sprz2mln <- rbind(df_bialystok,df_bydgoszcz,df_gdansk,df_gorzow,df_katowice,df_kielce,df_krakow,
                                 df_lodz,df_lublin,df_olsztyn,df_opole,df_poznan,df_rzeszow,df_szczecin,
                                 df_torun,df_warszawa,df_wroclaw,df_zielona)
oferty_miasta_sprz2mln                  



write_xlsx(x = oferty_miasta_sprz2mln, path = "oferty_miasta_sprz2mln.xlsx", col_names = TRUE)



###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################

### SKRYPT  2-2,5 MLN ZŁ ###

Sys.sleep(runif(1,30,90)) ###Szczecin ###


szczecin <- read_html("https://www.XXX/offer_list?search[locations][0]=103059&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20000%20001&search[filter-price_to]=2500000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
szczecin <-str_replace_all(szczecin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
szczecin <- regmatches(szczecin, gregexpr("[[:digit:]]+", szczecin))
szczecin <- as.numeric(unlist(szczecin))
df_szczecin <- data.frame(c('Szczecin'), szczecin[1], szczecin[2], szczecin[3])
colnames(df_szczecin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta') 
df_szczecin

Sys.sleep(runif(1,30,90)) ###Gdańsk ###

gdansk <- read_html("https://www.XXX/offer_list?search[locations][0]=74287&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20000%20001&search[filter-price_to]=2%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gdansk <-str_replace_all(gdansk, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gdansk <- regmatches(gdansk, gregexpr("[[:digit:]]+", gdansk))
gdansk <- as.numeric(unlist(gdansk))
df_gdansk <- data.frame(c('Gdańsk'), gdansk[1], gdansk[2], gdansk[3])
colnames(df_gdansk)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gdansk

Sys.sleep(runif(1,30,90)) ###Olsztyn ###

olsztyn <- read_html("https://www.XXX/offer_list?search[locations][0]=91529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20000%20001&search[filter-price_to]=2%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
olsztyn <-str_replace_all(olsztyn, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
olsztyn <- regmatches(olsztyn, gregexpr("[[:digit:]]+", olsztyn))
olsztyn <- as.numeric(unlist(olsztyn))
df_olsztyn <- data.frame(c('Olsztyn'), olsztyn[1], olsztyn[2], olsztyn[3])
colnames(df_olsztyn)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_olsztyn

Sys.sleep(runif(1,30,90)) ###Białystok ###

bialystok <- read_html("https://www.XXX/offer_list?search[locations][0]=70081&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20000%20001&search[filter-price_to]=2%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bialystok <-str_replace_all(bialystok, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bialystok <- regmatches(bialystok, gregexpr("[[:digit:]]+", bialystok))
bialystok <- as.numeric(unlist(bialystok))
df_bialystok <- data.frame(c('Białystok'), bialystok[1], bialystok[2], bialystok[3])
colnames(df_bialystok)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bialystok

Sys.sleep(runif(1,30,90)) ###Warszawa ###

warszawa <- read_html("https://www.XXX/offer_list?search[locations][0]=55060&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20000%20001&search[filter-price_to]=2%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
warszawa <-str_replace_all(warszawa, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
warszawa <- regmatches(warszawa, gregexpr("[[:digit:]]+", warszawa))
warszawa <- as.numeric(unlist(warszawa))
df_warszawa <- data.frame(c('Warszawa'), warszawa[1], warszawa[2], warszawa[3])
colnames(df_warszawa)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_warszawa


Sys.sleep(runif(1,30,90)) ###Toruń ###

torun <- read_html("https://www.XXX/offer_list?search[locations][0]=9292&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20000%20001&search[filter-price_to]=2%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
torun <-str_replace_all(torun, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
torun <- regmatches(torun, gregexpr("[[:digit:]]+", torun))
torun <- as.numeric(unlist(torun))
df_torun <- data.frame(c('Toruń'), torun[1], torun[2], torun[3])
colnames(df_torun)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_torun

Sys.sleep(runif(1,30,90)) ###Bydgoszcz ###

bydgoszcz <- read_html("https://www.XXX/offer_list?search[locations][0]=9232&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20000%20001&search[filter-price_to]=2%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bydgoszcz <-str_replace_all(bydgoszcz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bydgoszcz <- regmatches(bydgoszcz, gregexpr("[[:digit:]]+", bydgoszcz))
bydgoszcz <- as.numeric(unlist(bydgoszcz))
df_bydgoszcz <- data.frame(c('Bydgoszcz'), bydgoszcz[1], bydgoszcz[2], bydgoszcz[3])
colnames(df_bydgoszcz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bydgoszcz

Sys.sleep(runif(1,30,90)) ###Poznań ###

poznan <- read_html("https://www.XXX/offer_list?search[locations][0]=99366&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20000%20001&search[filter-price_to]=2%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
poznan <-str_replace_all(poznan, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
poznan <- regmatches(poznan, gregexpr("[[:digit:]]+", poznan))
poznan <- as.numeric(unlist(poznan))
df_poznan <- data.frame(c('Poznań'), poznan[1], poznan[2], poznan[3])
colnames(df_poznan)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_poznan


Sys.sleep(runif(1,30,90)) ###Zielona Góra ###

zielona <- read_html("https://www.XXX/offer_list?search[locations][0]=19087&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20000%20001&search[filter-price_to]=2%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zielona <-str_replace_all(zielona, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zielona <- regmatches(zielona, gregexpr("[[:digit:]]+", zielona))
zielona <- as.numeric(unlist(zielona))
df_zielona <- data.frame(c('Zielona Góra'), zielona[1], zielona[2], zielona[3])
colnames(df_zielona)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_zielona


Sys.sleep(runif(1,30,90)) ###Gorzów Wielkopolski ###

gorzow <- read_html("https://www.XXX/offer_list?search[locations][0]=19073&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20000%20001&search[filter-price_to]=2%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gorzow <-str_replace_all(gorzow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gorzow <- regmatches(gorzow, gregexpr("[[:digit:]]+", gorzow))
gorzow <- as.numeric(unlist(gorzow))
df_gorzow <- data.frame(c('Gorzów Wielkopolski'), gorzow[1], gorzow[2], gorzow[3])
colnames(df_gorzow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gorzow

Sys.sleep(runif(1,30,90)) ###Wrocław ###

wroclaw <- read_html("https://www.XXX/offer_list?search[locations][0]=3548&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20000%20001&search[filter-price_to]=2%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wroclaw <-str_replace_all(wroclaw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wroclaw <- regmatches(wroclaw, gregexpr("[[:digit:]]+", wroclaw))
wroclaw <- as.numeric(unlist(wroclaw))
df_wroclaw <- data.frame(c('Wrocław'), wroclaw[1], wroclaw[2], wroclaw[3])
colnames(df_wroclaw)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_wroclaw


Sys.sleep(runif(1,30,90)) ###Łódź ###

lodz <- read_html("https://www.XXX/offer_list?search[locations][0]=27391&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20000%20001&search[filter-price_to]=2%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lodz <-str_replace_all(lodz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lodz <- regmatches(lodz, gregexpr("[[:digit:]]+", lodz))
lodz <- as.numeric(unlist(lodz))
df_lodz <- data.frame(c('Łódź'), lodz[1], lodz[2], lodz[3])
colnames(df_lodz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lodz

Sys.sleep(runif(1,30,90)) ###Lublin ###

lublin <- read_html("https://www.XXX/offer_list?search[locations][0]=17291&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20000%20001&search[filter-price_to]=2%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lublin <-str_replace_all(lublin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lublin <- regmatches(lublin, gregexpr("[[:digit:]]+", lublin))
lublin <- as.numeric(unlist(lublin))
df_lublin <- data.frame(c('Lublin'), lublin[1], lublin[2], lublin[3])
colnames(df_lublin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lublin


Sys.sleep(runif(1,30,90)) ###Kielce ###

kielce <- read_html("https://www.XXX/offer_list?search[locations][0]=86670&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20000%20001&search[filter-price_to]=2%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kielce <-str_replace_all(kielce, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kielce <- regmatches(kielce, gregexpr("[[:digit:]]+", kielce))
kielce <- as.numeric(unlist(kielce))
df_kielce <- data.frame(c('Kielce'), kielce[1], kielce[2], kielce[3])
colnames(df_kielce)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_kielce


Sys.sleep(runif(1,30,90)) ###Opole ###

opole <- read_html("https://www.XXX/offer_list?search[locations][0]=57175&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20000%20001&search[filter-price_to]=2%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opole <-str_replace_all(opole, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opole <- regmatches(opole, gregexpr("[[:digit:]]+", opole))
opole <- as.numeric(unlist(opole))
df_opole <- data.frame(c('Opole'), opole[1], opole[2], opole[3])
colnames(df_opole)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_opole


Sys.sleep(runif(1,30,90)) ###Katowice ###

katowice <- read_html("https://www.XXX/offer_list?search[locations][0]=79344&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20000%20001&search[filter-price_to]=2%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
katowice <-str_replace_all(katowice, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
katowice <- regmatches(katowice, gregexpr("[[:digit:]]+", katowice))
katowice <- as.numeric(unlist(katowice))
df_katowice <- data.frame(c('Katowice'), katowice[1], katowice[2], katowice[3])
colnames(df_katowice)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_katowice


Sys.sleep(runif(1,30,90)) ###Kraków ###

krakow <- read_html("https://www.XXX/offer_list?search[locations][0]=41878&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20000%20001&search[filter-price_to]=2%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
krakow <-str_replace_all(krakow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
krakow <- regmatches(krakow, gregexpr("[[:digit:]]+", krakow))
krakow <- as.numeric(unlist(krakow))
df_krakow <- data.frame(c('Kraków'), krakow[1], krakow[2], krakow[3])
colnames(df_krakow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_krakow

Sys.sleep(runif(1,30,90)) ###Rzeszów ###

rzeszow <- read_html("https://www.XXX/offer_list?search[locations][0]=64847&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20000%20001&search[filter-price_to]=2%20500%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
rzeszow <-str_replace_all(rzeszow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
rzeszow <- regmatches(rzeszow, gregexpr("[[:digit:]]+", rzeszow))
rzeszow <- as.numeric(unlist(rzeszow))
df_rzeszow <- data.frame(c('Rzeszów'), rzeszow[1], rzeszow[2], rzeszow[3])
colnames(df_rzeszow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_rzeszow


oferty_miasta_sprz25mln <- rbind(df_bialystok,df_bydgoszcz,df_gdansk,df_gorzow,df_katowice,df_kielce,df_krakow,
                                df_lodz,df_lublin,df_olsztyn,df_opole,df_poznan,df_rzeszow,df_szczecin,
                                df_torun,df_warszawa,df_wroclaw,df_zielona)
oferty_miasta_sprz25mln                  



write_xlsx(x = oferty_miasta_sprz25mln, path = "oferty_miasta_sprz25mln.xlsx", col_names = TRUE)

###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################

### SKRYPT  2,5-3 MLN ZŁ ###

Sys.sleep(runif(1,30,90)) ###Szczecin ###


szczecin <- read_html("https://www.XXX/offer_list?search[locations][0]=103059&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20500%20001&search[filter-price_to]=3000000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
szczecin <-str_replace_all(szczecin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
szczecin <- regmatches(szczecin, gregexpr("[[:digit:]]+", szczecin))
szczecin <- as.numeric(unlist(szczecin))
df_szczecin <- data.frame(c('Szczecin'), szczecin[1], szczecin[2], szczecin[3])
colnames(df_szczecin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta') 
df_szczecin

Sys.sleep(runif(1,30,90)) ###Gdańsk ###

gdansk <- read_html("https://www.XXX/offer_list?search[locations][0]=74287&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20500%20001&search[filter-price_to]=3%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gdansk <-str_replace_all(gdansk, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gdansk <- regmatches(gdansk, gregexpr("[[:digit:]]+", gdansk))
gdansk <- as.numeric(unlist(gdansk))
df_gdansk <- data.frame(c('Gdańsk'), gdansk[1], gdansk[2], gdansk[3])
colnames(df_gdansk)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gdansk

Sys.sleep(runif(1,30,90)) ###Olsztyn ###

olsztyn <- read_html("https://www.XXX/offer_list?search[locations][0]=91529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20500%20001&search[filter-price_to]=3%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
olsztyn <-str_replace_all(olsztyn, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
olsztyn <- regmatches(olsztyn, gregexpr("[[:digit:]]+", olsztyn))
olsztyn <- as.numeric(unlist(olsztyn))
df_olsztyn <- data.frame(c('Olsztyn'), olsztyn[1], olsztyn[2], olsztyn[3])
colnames(df_olsztyn)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_olsztyn

Sys.sleep(runif(1,30,90)) ###Białystok ###

bialystok <- read_html("https://www.XXX/offer_list?search[locations][0]=70081&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20500%20001&search[filter-price_to]=3%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bialystok <-str_replace_all(bialystok, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bialystok <- regmatches(bialystok, gregexpr("[[:digit:]]+", bialystok))
bialystok <- as.numeric(unlist(bialystok))
df_bialystok <- data.frame(c('Białystok'), bialystok[1], bialystok[2], bialystok[3])
colnames(df_bialystok)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bialystok

Sys.sleep(runif(1,30,90)) ###Warszawa ###

warszawa <- read_html("https://www.XXX/offer_list?search[locations][0]=55060&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20500%20001&search[filter-price_to]=3%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
warszawa <-str_replace_all(warszawa, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
warszawa <- regmatches(warszawa, gregexpr("[[:digit:]]+", warszawa))
warszawa <- as.numeric(unlist(warszawa))
df_warszawa <- data.frame(c('Warszawa'), warszawa[1], warszawa[2], warszawa[3])
colnames(df_warszawa)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_warszawa


Sys.sleep(runif(1,30,90)) ###Toruń ###

torun <- read_html("https://www.XXX/offer_list?search[locations][0]=9292&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20500%20001&search[filter-price_to]=3%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
torun <-str_replace_all(torun, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
torun <- regmatches(torun, gregexpr("[[:digit:]]+", torun))
torun <- as.numeric(unlist(torun))
df_torun <- data.frame(c('Toruń'), torun[1], torun[2], torun[3])
colnames(df_torun)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_torun

Sys.sleep(runif(1,30,90)) ###Bydgoszcz ###

bydgoszcz <- read_html("https://www.XXX/offer_list?search[locations][0]=9232&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20500%20001&search[filter-price_to]=3%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bydgoszcz <-str_replace_all(bydgoszcz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bydgoszcz <- regmatches(bydgoszcz, gregexpr("[[:digit:]]+", bydgoszcz))
bydgoszcz <- as.numeric(unlist(bydgoszcz))
df_bydgoszcz <- data.frame(c('Bydgoszcz'), bydgoszcz[1], bydgoszcz[2], bydgoszcz[3])
colnames(df_bydgoszcz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bydgoszcz

Sys.sleep(runif(1,30,90)) ###Poznań ###

poznan <- read_html("https://www.XXX/offer_list?search[locations][0]=99366&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20500%20001&search[filter-price_to]=3%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
poznan <-str_replace_all(poznan, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
poznan <- regmatches(poznan, gregexpr("[[:digit:]]+", poznan))
poznan <- as.numeric(unlist(poznan))
df_poznan <- data.frame(c('Poznań'), poznan[1], poznan[2], poznan[3])
colnames(df_poznan)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_poznan


Sys.sleep(runif(1,30,90)) ###Zielona Góra ###

zielona <- read_html("https://www.XXX/offer_list?search[locations][0]=19087&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20500%20001&search[filter-price_to]=3%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zielona <-str_replace_all(zielona, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zielona <- regmatches(zielona, gregexpr("[[:digit:]]+", zielona))
zielona <- as.numeric(unlist(zielona))
df_zielona <- data.frame(c('Zielona Góra'), zielona[1], zielona[2], zielona[3])
colnames(df_zielona)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_zielona


Sys.sleep(runif(1,30,90)) ###Gorzów Wielkopolski ###

gorzow <- read_html("https://www.XXX/offer_list?search[locations][0]=19073&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20500%20001&search[filter-price_to]=3%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gorzow <-str_replace_all(gorzow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gorzow <- regmatches(gorzow, gregexpr("[[:digit:]]+", gorzow))
gorzow <- as.numeric(unlist(gorzow))
df_gorzow <- data.frame(c('Gorzów Wielkopolski'), gorzow[1], gorzow[2], gorzow[3])
colnames(df_gorzow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gorzow

Sys.sleep(runif(1,30,90)) ###Wrocław ###

wroclaw <- read_html("https://www.XXX/offer_list?search[locations][0]=3548&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20500%20001&search[filter-price_to]=3%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wroclaw <-str_replace_all(wroclaw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wroclaw <- regmatches(wroclaw, gregexpr("[[:digit:]]+", wroclaw))
wroclaw <- as.numeric(unlist(wroclaw))
df_wroclaw <- data.frame(c('Wrocław'), wroclaw[1], wroclaw[2], wroclaw[3])
colnames(df_wroclaw)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_wroclaw


Sys.sleep(runif(1,30,90)) ###Łódź ###

lodz <- read_html("https://www.XXX/offer_list?search[locations][0]=27391&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20500%20001&search[filter-price_to]=3%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lodz <-str_replace_all(lodz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lodz <- regmatches(lodz, gregexpr("[[:digit:]]+", lodz))
lodz <- as.numeric(unlist(lodz))
df_lodz <- data.frame(c('Łódź'), lodz[1], lodz[2], lodz[3])
colnames(df_lodz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lodz

Sys.sleep(runif(1,30,90)) ###Lublin ###

lublin <- read_html("https://www.XXX/offer_list?search[locations][0]=17291&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20500%20001&search[filter-price_to]=3%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lublin <-str_replace_all(lublin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lublin <- regmatches(lublin, gregexpr("[[:digit:]]+", lublin))
lublin <- as.numeric(unlist(lublin))
df_lublin <- data.frame(c('Lublin'), lublin[1], lublin[2], lublin[3])
colnames(df_lublin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lublin


Sys.sleep(runif(1,30,90)) ###Kielce ###

kielce <- read_html("https://www.XXX/offer_list?search[locations][0]=86670&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20500%20001&search[filter-price_to]=3%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kielce <-str_replace_all(kielce, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kielce <- regmatches(kielce, gregexpr("[[:digit:]]+", kielce))
kielce <- as.numeric(unlist(kielce))
df_kielce <- data.frame(c('Kielce'), kielce[1], kielce[2], kielce[3])
colnames(df_kielce)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_kielce


Sys.sleep(runif(1,30,90)) ###Opole ###

opole <- read_html("https://www.XXX/offer_list?search[locations][0]=57175&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20500%20001&search[filter-price_to]=3%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opole <-str_replace_all(opole, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opole <- regmatches(opole, gregexpr("[[:digit:]]+", opole))
opole <- as.numeric(unlist(opole))
df_opole <- data.frame(c('Opole'), opole[1], opole[2], opole[3])
colnames(df_opole)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_opole


Sys.sleep(runif(1,30,90)) ###Katowice ###

katowice <- read_html("https://www.XXX/offer_list?search[locations][0]=79344&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20500%20001&search[filter-price_to]=3%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
katowice <-str_replace_all(katowice, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
katowice <- regmatches(katowice, gregexpr("[[:digit:]]+", katowice))
katowice <- as.numeric(unlist(katowice))
df_katowice <- data.frame(c('Katowice'), katowice[1], katowice[2], katowice[3])
colnames(df_katowice)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_katowice


Sys.sleep(runif(1,30,90)) ###Kraków ###

krakow <- read_html("https://www.XXX/offer_list?search[locations][0]=41878&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20500%20001&search[filter-price_to]=3%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
krakow <-str_replace_all(krakow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
krakow <- regmatches(krakow, gregexpr("[[:digit:]]+", krakow))
krakow <- as.numeric(unlist(krakow))
df_krakow <- data.frame(c('Kraków'), krakow[1], krakow[2], krakow[3])
colnames(df_krakow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_krakow

Sys.sleep(runif(1,30,90)) ###Rzeszów ###

rzeszow <- read_html("https://www.XXX/offer_list?search[locations][0]=64847&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=2%20500%20001&search[filter-price_to]=3%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
rzeszow <-str_replace_all(rzeszow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
rzeszow <- regmatches(rzeszow, gregexpr("[[:digit:]]+", rzeszow))
rzeszow <- as.numeric(unlist(rzeszow))
df_rzeszow <- data.frame(c('Rzeszów'), rzeszow[1], rzeszow[2], rzeszow[3])
colnames(df_rzeszow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_rzeszow


oferty_miasta_sprz3mln <- rbind(df_bialystok,df_bydgoszcz,df_gdansk,df_gorzow,df_katowice,df_kielce,df_krakow,
                                df_lodz,df_lublin,df_olsztyn,df_opole,df_poznan,df_rzeszow,df_szczecin,
                                df_torun,df_warszawa,df_wroclaw,df_zielona)
oferty_miasta_sprz3mln                  



write_xlsx(x = oferty_miasta_sprz3mln, path = "oferty_miasta_sprz3mln.xlsx", col_names = TRUE)



###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################

### SKRYPT  3-4 MLN ZŁ ###

Sys.sleep(runif(1,30,90)) ###Szczecin ###


szczecin <- read_html("https://www.XXX/offer_list?search[locations][0]=103059&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20000%20001&search[filter-price_to]=4%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
szczecin <-str_replace_all(szczecin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
szczecin <- regmatches(szczecin, gregexpr("[[:digit:]]+", szczecin))
szczecin <- as.numeric(unlist(szczecin))
df_szczecin <- data.frame(c('Szczecin'), szczecin[1], szczecin[2], szczecin[3])
colnames(df_szczecin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta') 
df_szczecin

Sys.sleep(runif(1,30,90)) ###Gdańsk ###

gdansk <- read_html("https://www.XXX/offer_list?search[locations][0]=74287&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20000%20001&search[filter-price_to]=4%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gdansk <-str_replace_all(gdansk, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gdansk <- regmatches(gdansk, gregexpr("[[:digit:]]+", gdansk))
gdansk <- as.numeric(unlist(gdansk))
df_gdansk <- data.frame(c('Gdańsk'), gdansk[1], gdansk[2], gdansk[3])
colnames(df_gdansk)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gdansk

Sys.sleep(runif(1,30,90)) ###Olsztyn ###

olsztyn <- read_html("https://www.XXX/offer_list?search[locations][0]=91529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20000%20001&search[filter-price_to]=4%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
olsztyn <-str_replace_all(olsztyn, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
olsztyn <- regmatches(olsztyn, gregexpr("[[:digit:]]+", olsztyn))
olsztyn <- as.numeric(unlist(olsztyn))
df_olsztyn <- data.frame(c('Olsztyn'), olsztyn[1], olsztyn[2], olsztyn[3])
colnames(df_olsztyn)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_olsztyn

Sys.sleep(runif(1,30,90)) ###Białystok ###

bialystok <- read_html("https://www.XXX/offer_list?search[locations][0]=70081&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20000%20001&search[filter-price_to]=4%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bialystok <-str_replace_all(bialystok, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bialystok <- regmatches(bialystok, gregexpr("[[:digit:]]+", bialystok))
bialystok <- as.numeric(unlist(bialystok))
df_bialystok <- data.frame(c('Białystok'), bialystok[1], bialystok[2], bialystok[3])
colnames(df_bialystok)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bialystok

Sys.sleep(runif(1,30,90)) ###Warszawa ###

warszawa <- read_html("https://www.XXX/offer_list?search[locations][0]=55060&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20000%20001&search[filter-price_to]=4%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
warszawa <-str_replace_all(warszawa, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
warszawa <- regmatches(warszawa, gregexpr("[[:digit:]]+", warszawa))
warszawa <- as.numeric(unlist(warszawa))
df_warszawa <- data.frame(c('Warszawa'), warszawa[1], warszawa[2], warszawa[3])
colnames(df_warszawa)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_warszawa


Sys.sleep(runif(1,30,90)) ###Toruń ###

torun <- read_html("https://www.XXX/offer_list?search[locations][0]=9292&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20000%20001&search[filter-price_to]=4%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
torun <-str_replace_all(torun, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
torun <- regmatches(torun, gregexpr("[[:digit:]]+", torun))
torun <- as.numeric(unlist(torun))
df_torun <- data.frame(c('Toruń'), torun[1], torun[2], torun[3])
colnames(df_torun)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_torun

Sys.sleep(runif(1,30,90)) ###Bydgoszcz ###

bydgoszcz <- read_html("https://www.XXX/offer_list?search[locations][0]=9232&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20000%20001&search[filter-price_to]=4%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bydgoszcz <-str_replace_all(bydgoszcz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bydgoszcz <- regmatches(bydgoszcz, gregexpr("[[:digit:]]+", bydgoszcz))
bydgoszcz <- as.numeric(unlist(bydgoszcz))
df_bydgoszcz <- data.frame(c('Bydgoszcz'), bydgoszcz[1], bydgoszcz[2], bydgoszcz[3])
colnames(df_bydgoszcz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bydgoszcz

Sys.sleep(runif(1,30,90)) ###Poznań ###

poznan <- read_html("https://www.XXX/offer_list?search[locations][0]=99366&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20000%20001&search[filter-price_to]=4%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
poznan <-str_replace_all(poznan, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
poznan <- regmatches(poznan, gregexpr("[[:digit:]]+", poznan))
poznan <- as.numeric(unlist(poznan))
df_poznan <- data.frame(c('Poznań'), poznan[1], poznan[2], poznan[3])
colnames(df_poznan)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_poznan


Sys.sleep(runif(1,30,90)) ###Zielona Góra ###

zielona <- read_html("https://www.XXX/offer_list?search[locations][0]=19087&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20000%20001&search[filter-price_to]=4%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zielona <-str_replace_all(zielona, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zielona <- regmatches(zielona, gregexpr("[[:digit:]]+", zielona))
zielona <- as.numeric(unlist(zielona))
df_zielona <- data.frame(c('Zielona Góra'), zielona[1], zielona[2], zielona[3])
colnames(df_zielona)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_zielona


Sys.sleep(runif(1,30,90)) ###Gorzów Wielkopolski ###

gorzow <- read_html("https://www.XXX/offer_list?search[locations][0]=19073&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20000%20001&search[filter-price_to]=4%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gorzow <-str_replace_all(gorzow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gorzow <- regmatches(gorzow, gregexpr("[[:digit:]]+", gorzow))
gorzow <- as.numeric(unlist(gorzow))
df_gorzow <- data.frame(c('Gorzów Wielkopolski'), gorzow[1], gorzow[2], gorzow[3])
colnames(df_gorzow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gorzow

Sys.sleep(runif(1,30,90)) ###Wrocław ###

wroclaw <- read_html("https://www.XXX/offer_list?search[locations][0]=3548&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20000%20001&search[filter-price_to]=4%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wroclaw <-str_replace_all(wroclaw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wroclaw <- regmatches(wroclaw, gregexpr("[[:digit:]]+", wroclaw))
wroclaw <- as.numeric(unlist(wroclaw))
df_wroclaw <- data.frame(c('Wrocław'), wroclaw[1], wroclaw[2], wroclaw[3])
colnames(df_wroclaw)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_wroclaw


Sys.sleep(runif(1,30,90)) ###Łódź ###

lodz <- read_html("https://www.XXX/offer_list?search[locations][0]=27391&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20000%20001&search[filter-price_to]=4%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lodz <-str_replace_all(lodz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lodz <- regmatches(lodz, gregexpr("[[:digit:]]+", lodz))
lodz <- as.numeric(unlist(lodz))
df_lodz <- data.frame(c('Łódź'), lodz[1], lodz[2], lodz[3])
colnames(df_lodz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lodz

Sys.sleep(runif(1,30,90)) ###Lublin ###

lublin <- read_html("https://www.XXX/offer_list?search[locations][0]=17291&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20000%20001&search[filter-price_to]=4%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lublin <-str_replace_all(lublin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lublin <- regmatches(lublin, gregexpr("[[:digit:]]+", lublin))
lublin <- as.numeric(unlist(lublin))
df_lublin <- data.frame(c('Lublin'), lublin[1], lublin[2], lublin[3])
colnames(df_lublin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lublin


Sys.sleep(runif(1,30,90)) ###Kielce ###

kielce <- read_html("https://www.XXX/offer_list?search[locations][0]=86670&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20000%20001&search[filter-price_to]=4%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kielce <-str_replace_all(kielce, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kielce <- regmatches(kielce, gregexpr("[[:digit:]]+", kielce))
kielce <- as.numeric(unlist(kielce))
df_kielce <- data.frame(c('Kielce'), kielce[1], kielce[2], kielce[3])
colnames(df_kielce)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_kielce


Sys.sleep(runif(1,30,90)) ###Opole ###

opole <- read_html("https://www.XXX/offer_list?search[locations][0]=57175&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20000%20001&search[filter-price_to]=4%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opole <-str_replace_all(opole, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opole <- regmatches(opole, gregexpr("[[:digit:]]+", opole))
opole <- as.numeric(unlist(opole))
df_opole <- data.frame(c('Opole'), opole[1], opole[2], opole[3])
colnames(df_opole)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_opole


Sys.sleep(runif(1,30,90)) ###Katowice ###

katowice <- read_html("https://www.XXX/offer_list?search[locations][0]=79344&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20000%20001&search[filter-price_to]=4%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
katowice <-str_replace_all(katowice, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
katowice <- regmatches(katowice, gregexpr("[[:digit:]]+", katowice))
katowice <- as.numeric(unlist(katowice))
df_katowice <- data.frame(c('Katowice'), katowice[1], katowice[2], katowice[3])
colnames(df_katowice)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_katowice


Sys.sleep(runif(1,30,90)) ###Kraków ###

krakow <- read_html("https://www.XXX/offer_list?search[locations][0]=41878&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20000%20001&search[filter-price_to]=4%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
krakow <-str_replace_all(krakow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
krakow <- regmatches(krakow, gregexpr("[[:digit:]]+", krakow))
krakow <- as.numeric(unlist(krakow))
df_krakow <- data.frame(c('Kraków'), krakow[1], krakow[2], krakow[3])
colnames(df_krakow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_krakow

Sys.sleep(runif(1,30,90)) ###Rzeszów ###

rzeszow <- read_html("https://www.XXX/offer_list?search[locations][0]=64847&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=3%20000%20001&search[filter-price_to]=4%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
rzeszow <-str_replace_all(rzeszow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
rzeszow <- regmatches(rzeszow, gregexpr("[[:digit:]]+", rzeszow))
rzeszow <- as.numeric(unlist(rzeszow))
df_rzeszow <- data.frame(c('Rzeszów'), rzeszow[1], rzeszow[2], rzeszow[3])
colnames(df_rzeszow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_rzeszow


oferty_miasta_sprz4mln <- rbind(df_bialystok,df_bydgoszcz,df_gdansk,df_gorzow,df_katowice,df_kielce,df_krakow,
                                df_lodz,df_lublin,df_olsztyn,df_opole,df_poznan,df_rzeszow,df_szczecin,
                                df_torun,df_warszawa,df_wroclaw,df_zielona)
oferty_miasta_sprz4mln                  



write_xlsx(x = oferty_miasta_sprz4mln, path = "oferty_miasta_sprz3mln.xlsx", col_names = TRUE)


###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################

### SKRYPT  4-5 MLN ZŁ ###

Sys.sleep(runif(1,30,90)) ###Szczecin ###


szczecin <- read_html("https://www.XXX/offer_list?search[locations][0]=103059&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20000%20001&search[filter-price_to]=5%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
szczecin <-str_replace_all(szczecin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
szczecin <- regmatches(szczecin, gregexpr("[[:digit:]]+", szczecin))
szczecin <- as.numeric(unlist(szczecin))
df_szczecin <- data.frame(c('Szczecin'), szczecin[1], szczecin[2], szczecin[3])
colnames(df_szczecin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta') 
df_szczecin

Sys.sleep(runif(1,30,90)) ###Gdańsk ###

gdansk <- read_html("https://www.XXX/offer_list?search[locations][0]=74287&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20000%20001&search[filter-price_to]=5%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gdansk <-str_replace_all(gdansk, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gdansk <- regmatches(gdansk, gregexpr("[[:digit:]]+", gdansk))
gdansk <- as.numeric(unlist(gdansk))
df_gdansk <- data.frame(c('Gdańsk'), gdansk[1], gdansk[2], gdansk[3])
colnames(df_gdansk)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gdansk

Sys.sleep(runif(1,30,90)) ###Olsztyn ###

olsztyn <- read_html("https://www.XXX/offer_list?search[locations][0]=91529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20000%20001&search[filter-price_to]=5%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
olsztyn <-str_replace_all(olsztyn, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
olsztyn <- regmatches(olsztyn, gregexpr("[[:digit:]]+", olsztyn))
olsztyn <- as.numeric(unlist(olsztyn))
df_olsztyn <- data.frame(c('Olsztyn'), olsztyn[1], olsztyn[2], olsztyn[3])
colnames(df_olsztyn)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_olsztyn

Sys.sleep(runif(1,30,90)) ###Białystok ###

bialystok <- read_html("https://www.XXX/offer_list?search[locations][0]=70081&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20000%20001&search[filter-price_to]=5%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bialystok <-str_replace_all(bialystok, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bialystok <- regmatches(bialystok, gregexpr("[[:digit:]]+", bialystok))
bialystok <- as.numeric(unlist(bialystok))
df_bialystok <- data.frame(c('Białystok'), bialystok[1], bialystok[2], bialystok[3])
colnames(df_bialystok)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bialystok

Sys.sleep(runif(1,30,90)) ###Warszawa ###

warszawa <- read_html("https://www.XXX/offer_list?search[locations][0]=55060&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20000%20001&search[filter-price_to]=5%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
warszawa <-str_replace_all(warszawa, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
warszawa <- regmatches(warszawa, gregexpr("[[:digit:]]+", warszawa))
warszawa <- as.numeric(unlist(warszawa))
df_warszawa <- data.frame(c('Warszawa'), warszawa[1], warszawa[2], warszawa[3])
colnames(df_warszawa)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_warszawa


Sys.sleep(runif(1,30,90)) ###Toruń ###

torun <- read_html("https://www.XXX/offer_list?search[locations][0]=9292&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20000%20001&search[filter-price_to]=5%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
torun <-str_replace_all(torun, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
torun <- regmatches(torun, gregexpr("[[:digit:]]+", torun))
torun <- as.numeric(unlist(torun))
df_torun <- data.frame(c('Toruń'), torun[1], torun[2], torun[3])
colnames(df_torun)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_torun

Sys.sleep(runif(1,30,90)) ###Bydgoszcz ###

bydgoszcz <- read_html("https://www.XXX/offer_list?search[locations][0]=9232&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20000%20001&search[filter-price_to]=5%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bydgoszcz <-str_replace_all(bydgoszcz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bydgoszcz <- regmatches(bydgoszcz, gregexpr("[[:digit:]]+", bydgoszcz))
bydgoszcz <- as.numeric(unlist(bydgoszcz))
df_bydgoszcz <- data.frame(c('Bydgoszcz'), bydgoszcz[1], bydgoszcz[2], bydgoszcz[3])
colnames(df_bydgoszcz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bydgoszcz

Sys.sleep(runif(1,30,90)) ###Poznań ###

poznan <- read_html("https://www.XXX/offer_list?search[locations][0]=99366&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20000%20001&search[filter-price_to]=5%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
poznan <-str_replace_all(poznan, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
poznan <- regmatches(poznan, gregexpr("[[:digit:]]+", poznan))
poznan <- as.numeric(unlist(poznan))
df_poznan <- data.frame(c('Poznań'), poznan[1], poznan[2], poznan[3])
colnames(df_poznan)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_poznan


Sys.sleep(runif(1,30,90)) ###Zielona Góra ###

zielona <- read_html("https://www.XXX/offer_list?search[locations][0]=19087&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20000%20001&search[filter-price_to]=5%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zielona <-str_replace_all(zielona, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zielona <- regmatches(zielona, gregexpr("[[:digit:]]+", zielona))
zielona <- as.numeric(unlist(zielona))
df_zielona <- data.frame(c('Zielona Góra'), zielona[1], zielona[2], zielona[3])
colnames(df_zielona)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_zielona


Sys.sleep(runif(1,30,90)) ###Gorzów Wielkopolski ###

gorzow <- read_html("https://www.XXX/offer_list?search[locations][0]=19073&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20000%20001&search[filter-price_to]=5%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gorzow <-str_replace_all(gorzow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gorzow <- regmatches(gorzow, gregexpr("[[:digit:]]+", gorzow))
gorzow <- as.numeric(unlist(gorzow))
df_gorzow <- data.frame(c('Gorzów Wielkopolski'), gorzow[1], gorzow[2], gorzow[3])
colnames(df_gorzow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gorzow

Sys.sleep(runif(1,30,90)) ###Wrocław ###

wroclaw <- read_html("https://www.XXX/offer_list?search[locations][0]=3548&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20000%20001&search[filter-price_to]=5%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wroclaw <-str_replace_all(wroclaw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wroclaw <- regmatches(wroclaw, gregexpr("[[:digit:]]+", wroclaw))
wroclaw <- as.numeric(unlist(wroclaw))
df_wroclaw <- data.frame(c('Wrocław'), wroclaw[1], wroclaw[2], wroclaw[3])
colnames(df_wroclaw)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_wroclaw


Sys.sleep(runif(1,30,90)) ###Łódź ###

lodz <- read_html("https://www.XXX/offer_list?search[locations][0]=27391&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20000%20001&search[filter-price_to]=5%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lodz <-str_replace_all(lodz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lodz <- regmatches(lodz, gregexpr("[[:digit:]]+", lodz))
lodz <- as.numeric(unlist(lodz))
df_lodz <- data.frame(c('Łódź'), lodz[1], lodz[2], lodz[3])
colnames(df_lodz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lodz

Sys.sleep(runif(1,30,90)) ###Lublin ###

lublin <- read_html("https://www.XXX/offer_list?search[locations][0]=17291&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20000%20001&search[filter-price_to]=5%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lublin <-str_replace_all(lublin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lublin <- regmatches(lublin, gregexpr("[[:digit:]]+", lublin))
lublin <- as.numeric(unlist(lublin))
df_lublin <- data.frame(c('Lublin'), lublin[1], lublin[2], lublin[3])
colnames(df_lublin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lublin


Sys.sleep(runif(1,30,90)) ###Kielce ###

kielce <- read_html("https://www.XXX/offer_list?search[locations][0]=86670&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20000%20001&search[filter-price_to]=5%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kielce <-str_replace_all(kielce, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kielce <- regmatches(kielce, gregexpr("[[:digit:]]+", kielce))
kielce <- as.numeric(unlist(kielce))
df_kielce <- data.frame(c('Kielce'), kielce[1], kielce[2], kielce[3])
colnames(df_kielce)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_kielce


Sys.sleep(runif(1,30,90)) ###Opole ###

opole <- read_html("https://www.XXX/offer_list?search[locations][0]=57175&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20000%20001&search[filter-price_to]=5%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opole <-str_replace_all(opole, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opole <- regmatches(opole, gregexpr("[[:digit:]]+", opole))
opole <- as.numeric(unlist(opole))
df_opole <- data.frame(c('Opole'), opole[1], opole[2], opole[3])
colnames(df_opole)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_opole


Sys.sleep(runif(1,30,90)) ###Katowice ###

katowice <- read_html("https://www.XXX/offer_list?search[locations][0]=79344&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20000%20001&search[filter-price_to]=5%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
katowice <-str_replace_all(katowice, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
katowice <- regmatches(katowice, gregexpr("[[:digit:]]+", katowice))
katowice <- as.numeric(unlist(katowice))
df_katowice <- data.frame(c('Katowice'), katowice[1], katowice[2], katowice[3])
colnames(df_katowice)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_katowice


Sys.sleep(runif(1,30,90)) ###Kraków ###

krakow <- read_html("https://www.XXX/offer_list?search[locations][0]=41878&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20000%20001&search[filter-price_to]=5%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
krakow <-str_replace_all(krakow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
krakow <- regmatches(krakow, gregexpr("[[:digit:]]+", krakow))
krakow <- as.numeric(unlist(krakow))
df_krakow <- data.frame(c('Kraków'), krakow[1], krakow[2], krakow[3])
colnames(df_krakow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_krakow

Sys.sleep(runif(1,30,90)) ###Rzeszów ###

rzeszow <- read_html("https://www.XXX/offer_list?search[locations][0]=64847&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=4%20000%20001&search[filter-price_to]=5%20000%20000&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
rzeszow <-str_replace_all(rzeszow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
rzeszow <- regmatches(rzeszow, gregexpr("[[:digit:]]+", rzeszow))
rzeszow <- as.numeric(unlist(rzeszow))
df_rzeszow <- data.frame(c('Rzeszów'), rzeszow[1], rzeszow[2], rzeszow[3])
colnames(df_rzeszow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_rzeszow


oferty_miasta_sprz5mln <- rbind(df_bialystok,df_bydgoszcz,df_gdansk,df_gorzow,df_katowice,df_kielce,df_krakow,
                                df_lodz,df_lublin,df_olsztyn,df_opole,df_poznan,df_rzeszow,df_szczecin,
                                df_torun,df_warszawa,df_wroclaw,df_zielona)
oferty_miasta_sprz5mln                  



write_xlsx(x = oferty_miasta_sprz5mln, path = "oferty_miasta_sprz5mln.xlsx", col_names = TRUE)


###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################
###############################################################################################################################################################################################

### SKRYPT  pow 5 MLN ZŁ ###

Sys.sleep(runif(1,30,90)) ###Szczecin ###


szczecin <- read_html("https://www.XXX/offer_list?search[locations][0]=103059&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20000%20001&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
szczecin <-str_replace_all(szczecin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
szczecin <- regmatches(szczecin, gregexpr("[[:digit:]]+", szczecin))
szczecin <- as.numeric(unlist(szczecin))
df_szczecin <- data.frame(c('Szczecin'), szczecin[1], szczecin[2], szczecin[3])
colnames(df_szczecin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta') 
df_szczecin

Sys.sleep(runif(1,30,90)) ###Gdańsk ###

gdansk <- read_html("https://www.XXX/offer_list?search[locations][0]=74287&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20000%20001&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gdansk <-str_replace_all(gdansk, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gdansk <- regmatches(gdansk, gregexpr("[[:digit:]]+", gdansk))
gdansk <- as.numeric(unlist(gdansk))
df_gdansk <- data.frame(c('Gdańsk'), gdansk[1], gdansk[2], gdansk[3])
colnames(df_gdansk)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gdansk

Sys.sleep(runif(1,30,90)) ###Olsztyn ###

olsztyn <- read_html("https://www.XXX/offer_list?search[locations][0]=91529&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20000%20001&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
olsztyn <-str_replace_all(olsztyn, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
olsztyn <- regmatches(olsztyn, gregexpr("[[:digit:]]+", olsztyn))
olsztyn <- as.numeric(unlist(olsztyn))
df_olsztyn <- data.frame(c('Olsztyn'), olsztyn[1], olsztyn[2], olsztyn[3])
colnames(df_olsztyn)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_olsztyn

Sys.sleep(runif(1,30,90)) ###Białystok ###

bialystok <- read_html("https://www.XXX/offer_list?search[locations][0]=70081&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20000%20001&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bialystok <-str_replace_all(bialystok, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bialystok <- regmatches(bialystok, gregexpr("[[:digit:]]+", bialystok))
bialystok <- as.numeric(unlist(bialystok))
df_bialystok <- data.frame(c('Białystok'), bialystok[1], bialystok[2], bialystok[3])
colnames(df_bialystok)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bialystok

Sys.sleep(runif(1,30,90)) ###Warszawa ###

warszawa <- read_html("https://www.XXX/offer_list?search[locations][0]=55060&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20000%20001&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
warszawa <-str_replace_all(warszawa, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
warszawa <- regmatches(warszawa, gregexpr("[[:digit:]]+", warszawa))
warszawa <- as.numeric(unlist(warszawa))
df_warszawa <- data.frame(c('Warszawa'), warszawa[1], warszawa[2], warszawa[3])
colnames(df_warszawa)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_warszawa


Sys.sleep(runif(1,30,90)) ###Toruń ###

torun <- read_html("https://www.XXX/offer_list?search[locations][0]=9292&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20000%20001&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
torun <-str_replace_all(torun, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
torun <- regmatches(torun, gregexpr("[[:digit:]]+", torun))
torun <- as.numeric(unlist(torun))
df_torun <- data.frame(c('Toruń'), torun[1], torun[2], torun[3])
colnames(df_torun)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_torun

Sys.sleep(runif(1,30,90)) ###Bydgoszcz ###

bydgoszcz <- read_html("https://www.XXX/offer_list?search[locations][0]=9232&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20000%20001&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
bydgoszcz <-str_replace_all(bydgoszcz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
bydgoszcz <- regmatches(bydgoszcz, gregexpr("[[:digit:]]+", bydgoszcz))
bydgoszcz <- as.numeric(unlist(bydgoszcz))
df_bydgoszcz <- data.frame(c('Bydgoszcz'), bydgoszcz[1], bydgoszcz[2], bydgoszcz[3])
colnames(df_bydgoszcz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_bydgoszcz

Sys.sleep(runif(1,30,90)) ###Poznań ###

poznan <- read_html("https://www.XXX/offer_list?search[locations][0]=99366&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20000%20001&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
poznan <-str_replace_all(poznan, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
poznan <- regmatches(poznan, gregexpr("[[:digit:]]+", poznan))
poznan <- as.numeric(unlist(poznan))
df_poznan <- data.frame(c('Poznań'), poznan[1], poznan[2], poznan[3])
colnames(df_poznan)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_poznan


Sys.sleep(runif(1,30,90)) ###Zielona Góra ###

zielona <- read_html("https://www.XXX/offer_list?search[locations][0]=19087&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20000%20001&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
zielona <-str_replace_all(zielona, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
zielona <- regmatches(zielona, gregexpr("[[:digit:]]+", zielona))
zielona <- as.numeric(unlist(zielona))
df_zielona <- data.frame(c('Zielona Góra'), zielona[1], zielona[2], zielona[3])
colnames(df_zielona)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_zielona


Sys.sleep(runif(1,30,90)) ###Gorzów Wielkopolski ###

gorzow <- read_html("https://www.XXX/offer_list?search[locations][0]=19073&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20000%20001&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
gorzow <-str_replace_all(gorzow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
gorzow <- regmatches(gorzow, gregexpr("[[:digit:]]+", gorzow))
gorzow <- as.numeric(unlist(gorzow))
df_gorzow <- data.frame(c('Gorzów Wielkopolski'), gorzow[1], gorzow[2], gorzow[3])
colnames(df_gorzow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_gorzow

Sys.sleep(runif(1,30,90)) ###Wrocław ###

wroclaw <- read_html("https://www.XXX/offer_list?search[locations][0]=3548&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20000%20001&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
wroclaw <-str_replace_all(wroclaw, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
wroclaw <- regmatches(wroclaw, gregexpr("[[:digit:]]+", wroclaw))
wroclaw <- as.numeric(unlist(wroclaw))
df_wroclaw <- data.frame(c('Wrocław'), wroclaw[1], wroclaw[2], wroclaw[3])
colnames(df_wroclaw)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_wroclaw


Sys.sleep(runif(1,30,90)) ###Łódź ###

lodz <- read_html("https://www.XXX/offer_list?search[locations][0]=27391&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20000%20001&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lodz <-str_replace_all(lodz, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lodz <- regmatches(lodz, gregexpr("[[:digit:]]+", lodz))
lodz <- as.numeric(unlist(lodz))
df_lodz <- data.frame(c('Łódź'), lodz[1], lodz[2], lodz[3])
colnames(df_lodz)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lodz

Sys.sleep(runif(1,30,90)) ###Lublin ###

lublin <- read_html("https://www.XXX/offer_list?search[locations][0]=17291&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20000%20001&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
lublin <-str_replace_all(lublin, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
lublin <- regmatches(lublin, gregexpr("[[:digit:]]+", lublin))
lublin <- as.numeric(unlist(lublin))
df_lublin <- data.frame(c('Lublin'), lublin[1], lublin[2], lublin[3])
colnames(df_lublin)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_lublin


Sys.sleep(runif(1,30,90)) ###Kielce ###

kielce <- read_html("https://www.XXX/offer_list?search[locations][0]=86670&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20000%20001&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
kielce <-str_replace_all(kielce, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
kielce <- regmatches(kielce, gregexpr("[[:digit:]]+", kielce))
kielce <- as.numeric(unlist(kielce))
df_kielce <- data.frame(c('Kielce'), kielce[1], kielce[2], kielce[3])
colnames(df_kielce)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_kielce


Sys.sleep(runif(1,30,90)) ###Opole ###

opole <- read_html("https://www.XXX/offer_list?search[locations][0]=57175&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20000%20001&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
opole <-str_replace_all(opole, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
opole <- regmatches(opole, gregexpr("[[:digit:]]+", opole))
opole <- as.numeric(unlist(opole))
df_opole <- data.frame(c('Opole'), opole[1], opole[2], opole[3])
colnames(df_opole)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_opole


Sys.sleep(runif(1,30,90)) ###Katowice ###

katowice <- read_html("https://www.XXX/offer_list?search[locations][0]=79344&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20000%20001&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
katowice <-str_replace_all(katowice, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
katowice <- regmatches(katowice, gregexpr("[[:digit:]]+", katowice))
katowice <- as.numeric(unlist(katowice))
df_katowice <- data.frame(c('Katowice'), katowice[1], katowice[2], katowice[3])
colnames(df_katowice)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_katowice


Sys.sleep(runif(1,30,90)) ###Kraków ###

krakow <- read_html("https://www.XXX/offer_list?search[locations][0]=41878&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20000%20001&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
krakow <-str_replace_all(krakow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
krakow <- regmatches(krakow, gregexpr("[[:digit:]]+", krakow))
krakow <- as.numeric(unlist(krakow))
df_krakow <- data.frame(c('Kraków'), krakow[1], krakow[2], krakow[3])
colnames(df_krakow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_krakow

Sys.sleep(runif(1,30,90)) ###Rzeszów ###

rzeszow <- read_html("https://www.XXX/offer_list?search[locations][0]=64847&search[oferent_kategoria][0]=1&search[oferent_kategoria][1]=2&search[filter-id_kategoria]=1&search[filter-id_typ_nieruchomosci]=1&search[filter-price_from]=5%20000%20001&search[filter_expression_group][0][id_filter_expression_type]=1&search[filter_expression_group][0][expression][0]=") %>%
  html_nodes("b") %>%
  html_text2()
rzeszow <-str_replace_all(rzeszow, "[\r\n• SZzukaszznalezioneofertyodfiltrowanychduplikatówzarejestrujsiębyzobaczyćpozostałeofertyzpełnąhistorią]" , "")
rzeszow <- regmatches(rzeszow, gregexpr("[[:digit:]]+", rzeszow))
rzeszow <- as.numeric(unlist(rzeszow))
df_rzeszow <- data.frame(c('Rzeszów'), rzeszow[1], rzeszow[2], rzeszow[3])
colnames(df_rzeszow)<-c('Miasto','Znalezione','Odfiltrowane','Reszta')
df_rzeszow


oferty_miasta_sprz_pow_5mln <- rbind(df_bialystok,df_bydgoszcz,df_gdansk,df_gorzow,df_katowice,df_kielce,df_krakow,
                                df_lodz,df_lublin,df_olsztyn,df_opole,df_poznan,df_rzeszow,df_szczecin,
                                df_torun,df_warszawa,df_wroclaw,df_zielona)
oferty_miasta_sprz_pow_5mln                  



write_xlsx(x = oferty_miasta_sprz_pow_5mln, path = "oferty_miasta_sprz_pow_5mln.xlsx", col_names = TRUE)