library(tidyverse)
library(janitor)
library(stringr)
library(lubridate)
library(rvest)

# Scraper, mille kaudu saad teha ÜHE koondtabeli:Nimi, Kategooria, Hind.
# 1) 10 kõige kallimat laste raamatut + hinnad
# 2) 10 kõige odavamat muusika raamatut + hinnad
# 3) 10 tähestiku jäärjekorras sorteeritud lasteraamatud

# 10 KÕIGE ODAVAMAAT MUUSIKA RAAMATUT + HINNAD
# Link 1 - Muusikaraamatud
link1 <- "http://books.toscrape.com/catalogue/category/books/music_14/index.html"

# Muusikaraamatute pealkirjad
muusika_pealkirjad <- link1%>% 
  read_html()%>% 
  html_nodes("h3 > a")%>% 
  html_text()

# Muusikaraamatute hinnad
muusika_hinnad <- link1%>% 
  read_html()%>% 
  html_nodes("div.product_price > p.price_color")%>% 
  html_text()

muusikaraamat<- data.frame(muusika_hinnad, muusika_pealkirjad) 

# 10 kõige odavamat muusikaraamatut

muusikaraamatud_odav<-muusikaraamat%>% 
  arrange(muusika_hinnad)%>% 
  head(10)

# 1) 10 KÕIGE KALLIMAT LASTERAAMATUT + HINNAD
# Lasteraamatud lehekülgede kaupa
#Lasteraamatud lehekülg 1

lasteraamatud1 <- "http://books.toscrape.com/catalogue/category/books/childrens_11/page-1.html"

# Lasteraamatute pealkirjad leheküljelt 1
lasteraamatud_pealkirjad1 <- lasteraamatud1%>% 
  read_html()%>%
  html_nodes("h3 > a")%>% 
  html_text()

# Lasteraamatute hinnad leheküljelt 1
lasteraamatud_hinnad1 <- lasteraamatud1%>% 
  read_html()%>%
  html_nodes("div.product_price > p.price_color")%>% 
  html_text()

lasteraamat_uks<-data.frame(lasteraamatud_pealkirjad1, lasteraamatud_hinnad1)

# Lasteraamatute lehekülg 2

lasteraamatud2 <- "http://books.toscrape.com/catalogue/category/books/childrens_11/page-2.html"

# Lasteraamatute pealkirjad leheküljelt 2

lasteraamatud_pealkirjad2 <- lasteraamatud2%>% 
  read_html()%>%
  html_nodes("h3 > a")%>% 
  html_text()

# Lasteraamatute hinnad leheküljelt 2
lasteraamatud_hinnad2 <- lasteraamatud2%>% 
  read_html()%>%
  html_nodes("div.product_price > p.price_color")%>% 
  html_text()

lasteraamat_kaks<-data.frame(lasteraamatud_pealkirjad2, lasteraamatud_hinnad2)

# Lasteraamatud üheks tabeliks

nimed1<-data.frame(pealkiri=c(lasteraamatud_pealkirjad1), hind=c(lasteraamatud_hinnad1))
nimed2<-data.frame(pealkiri=c(lasteraamatud_pealkirjad2), hind=c(lasteraamatud_hinnad2))
lapsed<-bind_rows(nimed1,nimed2)

# 10 kõige kallimat lasteraamatut
lasteraamatud_kallimast<-lapsed%>% 
  arrange(desc(hind))%>% 
  head(10)%>% 
  view()

# Lasteraamatud tähestikulises järjekorras

lasteraamatud_tähestik<-lapsed%>%
  arrange(pealkiri)%>% 
  head(10)%>% 
  view()

# Muusikaraamatud ja lasteraamatud kokku

test<-data.frame(pealkiri=c(muusikaraamatud_odav$muusika_pealkirjad), hind=c(muusikaraamatud_odav$muusika_hinnad))

# Tabel:10 kalleimad lasteraamatud, 10 odavaimad muusikaraamatud ja 10 tähestikulises järjekorras lasteraamatud

raamatud<-bind_rows(test, lasteraamatud_tähestik,lasteraamatud_kallimast)

# CSV fail

write_csv(raamatud, "lasteraamatud_muusikaraamatud")
