library(dplyr)
library(stringr)
library(ggplot2)

exclude_kode = c('CPGT', 'NAGA', 'DAJK', 'FINN', 'AMOR', 'SKBM', 'BRIS')
df_ipo <- read.csv('Aksi_Korporasi_IPO_Tahun_2011_2022.csv', sep = ';') %>%
  filter(Sektor != 'Technology')
df_ipo$tanggal_ipo <- strptime(df_ipo$Tanggal.Pencatatan.IPO, format = "%d-%b-%y")
df_ipo$harga_ipo <- as.integer(gsub("[^0-9]", "", df_ipo$Harga.IPO))
df_ipo <- filter(df_ipo, format(df_ipo$tanggal_ipo, '%Y') != '2022') %>%
  filter(!kode %in% exclude_kode)

df_reff_ <- read.csv('data_stock_references_bhar500.csv') %>%
  mutate(kode = substr(kode, 1, 4))
head(df_reff_)

# Menghitung return saham (Rbt)
df_sektor <- read.csv('Aksi_Korporasi_IPO_Tahun_2011_2022 - summary sector.csv')
df_sektor

df_reff <- merge(x = df_reff_,
                 y = df_sektor,
                 by.x = 'kode',
                 by.y = 'Kode') %>%
  select('kode', 'Date', 'Open','Close', 'Sektor')
return_rbt <- (df_reff$Close - df_reff$Open)/df_reff$Open
df_reff$return_reff <- return_rbt
tail(df_reff)

# Menghitung return price (Rit)
df_sektor_all <- select(df_ipo, 'kode', 'Sektor')
head(df_sektor_all)

df_prices <- merge(x = read.csv('data_stock_prices_bhar500_pluskrah.csv'),
                   y = df_sektor_all,
                   by.x = 'kode',
                   by.y = 'kode') %>%
  select('kode', 'Date', 'Sektor', 'Close', 'Open') %>%
  mutate(Sektor = str_trim(Sektor))
return_rit <- (df_prices$Close - df_prices$Open)/df_prices$Open
df_prices$return_stock <- return_rit

df_join <- merge(x = df_prices,
                 y = df_reff,
                 by.x = c('Sektor', 'Date'),
                 by.y = c('Sektor', 'Date'),
                 all.x = TRUE) %>%
  filter(!is.na(kode.y))

head(df_join)

BHARit <- (1+df_join$return_stock)
df_join$bharit <- BHARit
BHARbt <- (1+df_join$return_reff)
df_join$bharbt <- BHARbt

df_bhar <- df_join %>%
  group_by(kode.x, Sektor, kode.y) %>%
  summarise(phi_bharit_500 = prod(bharit),
            phi_bharbt_500 = prod(bharbt)) %>%
  as.data.frame() %>%
  arrange('kode.x')

bhar500 <- df_bhar$phi_bharit_500 - df_bhar$phi_bharbt_500
df_bhar$bhar500 <- bhar500
df_bhar <- select(df_bhar,
                  'kode.x',
                  'phi_bharit_500',
                  'phi_bharbt_500',
                  'bhar500')
tail(df_bhar)

df_all <- read.csv('data_final_new_20230530.csv')
df_all_plus_bhar500 <- merge(x = df_all,
                             y = df_bhar,
                             by.x = 'kode',
                             by.y = 'kode.x',
                             all.x = TRUE)
tail(df_all_plus_bhar500)

write.csv(df_all_plus_bhar500, 'data_final_plusbhar500_20230530.csv')

df_summary <- as.data.frame(summary(df_all_plus_bhar500))
write.csv(df_summary, 'data_final_summary_plusbhar500_20230530.csv')

#summary(df_all_plus_bhar500)
summary(df_bhar)
