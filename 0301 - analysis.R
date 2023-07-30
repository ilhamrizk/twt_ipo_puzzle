install.packages("ggpubr")

library(dplyr)
library(stringr)
library(ggplot2)

# Define
df_ipo <- read.csv('Aksi_Korporasi_IPO_Tahun_2011_2022.csv', sep = ';')
df_ipo$tanggal_ipo <- strptime(df_ipo$Tanggal.Pencatatan.IPO, format = "%d-%b-%y")
df_ipo$harga_ipo <- as.integer(gsub("[^0-9]", "", df_ipo$Harga.IPO))
df_ipo <- filter(df_ipo, format(df_ipo$tanggal_ipo, '%Y') != '2022')


df_final <- df_ipo %>%
  select('kode',
         'Nama.Emiten',
         'Sektor',
         'tanggal_ipo',
         'harga_ipo') %>%
  arrange(kode)
head(df_final)

# Untuk mendapatkan harga IPO dan Revision Price
df_correction <- read.csv('Data_analisis_akhir.csv', sep=';') %>%
  select('kode','bookbuilding', 'harga_ipo') %>%
  arrange(kode)
head(df_correction)
tail(df_correction)

df_ipo$correction <- case_when(
  str_trim(df_ipo$Harga.Penawaran.Awal) == '-' ~ 1,
  TRUE ~ 0
)

df_ipo_correction <- merge(x = df_ipo,
                           y = df_correction,
                           by = 'kode',
                           all.x = TRUE) %>%
  arrange(kode)
head(df_ipo_correction)

penawaran_awal_min <- str_split_i(df_ipo_correction$Harga.Penawaran.Awal, "-", 1) %>%
  str_trim() %>%
  as.numeric()
penawaran_awal_min_correction <- str_split_i(df_ipo_correction$bookbuilding, "-", 1) %>%
  str_trim() %>%
  as.numeric()
penawaran_awal_min_true <- case_when(
  df_ipo_correction$correction == 1 ~ penawaran_awal_min_correction,
  TRUE ~ penawaran_awal_min,
)

penawaran_awal_max <- str_split_i(df_ipo_correction$Harga.Penawaran.Awal, "-", 2) %>%
  str_trim() %>%
  as.numeric()
penawaran_awal_max_correction <- str_split_i(df_ipo_correction$bookbuilding, "-", 2) %>%
  str_trim() %>%
  as.numeric()
penawaran_awal_max_true <- case_when(
  df_ipo_correction$correction == 1 ~ penawaran_awal_max_correction,
  TRUE ~ penawaran_awal_max_correction,
)

harga_ipo <- case_when(
  df_ipo_correction$correction == 0 ~ df_ipo_correction$harga_ipo.x,
  df_ipo_correction$correction == 1 ~ df_ipo_correction$harga_ipo.y,
)

bookbuilding_correction <- df_ipo_correction$bookbuilding %>%
  str_trim() %>%
  as.numeric()
bookbuilding_ori <- df_ipo_correction$Harga.Penawaran.Awal %>%
  str_trim() %>%
  as.numeric()

# 1. tidak correction tapi bookbuilding-nya 1 angka
# 2. tidak correction dan bookbuilding-nya 2 angka
# 3. correction tapi bookbuilding-nya 1 angka, min menggunakan harga IPO.y
# 4. correction dan bookbuilding-nya 2 angka, min menggunakan min bookbuilding
penawaran_awal_min_coalesce <- case_when(
  (df_ipo_correction$correction == 0)
  & (!is.na(bookbuilding_ori)) ~ df_ipo_correction$harga_ipo.x,
  (df_ipo_correction$correction == 0)
  & (is.na(bookbuilding_ori)) ~ penawaran_awal_min,
  (df_ipo_correction$correction == 1)
  & (!is.na(bookbuilding_correction)) ~ df_ipo_correction$harga_ipo.y,
  (df_ipo_correction$correction == 1)
  & (is.na(bookbuilding_correction)) ~ penawaran_awal_min_correction
)

# 1. tidak correction tapi bookbuilding-nya 1 angka
# 2. tidak correction dan bookbuilding-nya 2 angka
# 3. correction tapi bookbuilding-nya 1 angka, max menggunakan harga IPO.y
# 4. correction dan bookbuilding-nya 2 angka, max menggunakan max bookbuilding
penawaran_awal_max_coalesce <- case_when(
  (df_ipo_correction$correction == 0)
  & (!is.na(bookbuilding_ori)) ~ bookbuilding_ori,
  (df_ipo_correction$correction == 0)
  & (is.na(bookbuilding_ori)) ~ penawaran_awal_max,
  (df_ipo_correction$correction == 1)
  & (!is.na(bookbuilding_correction)) ~ bookbuilding_correction,
  (df_ipo_correction$correction == 1)
  & (is.na(bookbuilding_correction)) ~ penawaran_awal_max_correction
)

price_revision_ori <- (harga_ipo - penawaran_awal_min_coalesce)/(penawaran_awal_max_coalesce - penawaran_awal_min_coalesce)
price_revision_ori <- case_when(
  is.na(price_revision_ori)  ~ 1,
  TRUE ~ price_revision_ori)
price_revision_capped <- case_when(
  price_revision_ori < 0 ~ 0,
  price_revision_ori > 1 ~ 1,
  TRUE ~ price_revision_ori)
price_revision_binary <- case_when(
  price_revision_capped < 0.5 ~ 0,
  price_revision_capped >= 0.5 ~ 1)

df_final$bookbuilding <- case_when(
  df_ipo_correction$correction == 0 ~ df_ipo_correction$Harga.Penawaran.Awal,
  df_ipo_correction$correction == 1 ~ df_ipo_correction$bookbuilding
)
df_final$correction <- df_ipo_correction$correction
df_final$penawaran_awal_min <- penawaran_awal_min_coalesce
df_final$penawaran_awal_max <- penawaran_awal_max_coalesce
df_final$price_revision_ori <- price_revision_ori
df_final$price_revision_capped <- price_revision_capped
df_final$price_revision_binary <- price_revision_binary
df_final$harga_ipo <- harga_ipo

filter(df_final, penawaran_awal_min > penawaran_awal_max)
head(df_final)

# exclude 7 emiten & technology
exclude_kode = c('CPGT', 'NAGA', 'DAJK', 'FINN', 'AMOR', 'SKBM', 'BRIS')
df_final <- filter(df_final, !kode %in% exclude_kode) %>%
  filter(Sektor != 'Technology')
nrow(df_final)

# Adjustment
df_final$price_revision_binary <- case_when((df_final$penawaran_awal_max < df_final$penawaran_awal_min) 
                                      & (df_final$price_revision_binary == 0) ~ 1,
                                      (df_final$penawaran_awal_max < df_final$penawaran_awal_min) 
                                      & (df_final$price_revision_binary == 1) ~ 0,
                                      (df_final$penawaran_awal_max == df_final$penawaran_awal_min)
                                      & (df_final$price_revision_binary == 1) ~ 0,
                                      TRUE ~ df_final$price_revision_binary)

# get close price
df_price <- read.csv('data_stock_prices - data_stock_prices.csv')
df_price_mindate <- df_price %>%
  group_by(kode) %>%
  summarise(mindate = min(Date)) %>%
  as.data.frame()

df_price <- merge(x = df_price,
                  y = df_price_mindate,
                  by.x = "kode",
                  by.y = "kode")
df_price <- filter(df_price, Date == mindate) %>%
  select('kode', 'Close')
head(df_price)
nrow(df_price)

df_final <- merge(x = df_final,
                    y = df_price,
                    by.x = "kode",
                    by.y = "kode",
                    all.x = TRUE) %>%
  arrange(kode)
head(df_final)
nrow(df_final)

# Initial return = ((harga penutupan perdana - harga penawaran IPO) / harga penawaran IPO) x 100%
initial_return <- ((df_final$Close - df_final$harga_ipo)/df_final$harga_ipo)*100
df_final$initial.return <- initial_return
tail(df_final)

# menghitung sentiment & jumlah tweet
#outputsvm_notech
is_first <- TRUE
for (x in 2011:2021) {
  print(x)
  filename <- sprintf('outputsvm_notech/preprocessed_%s.csv - Output SVM.csv', x)
  df_temp <- read.csv(filename)
  if (is_first == TRUE){
    df_all <- df_temp
    is_first <- FALSE
  } else {
    df_all <- union_all(df_all, df_temp)
  }
}
head(df_all)
print(nrow(df_all))

# all pre ipo
df_summary <- df_all %>%
  group_by(Actual.kode) %>%
  mutate(bullish = case_when(Predicted_class == 1 ~ 1, .default = 0),
         neutral = case_when(Predicted_class == 0 ~ 1, .default = 0),
         bearish = case_when(Predicted_class == -1 ~ 1, .default = 0)) %>%
  summarise(all.avg_sentiment = mean(Predicted_class),
            all.nbr_of_msg=n(),
            all.bullish = sum(bullish),
            all.neutral = sum(neutral),
            all.bearish = sum(bearish)) %>%
  as.data.frame()
df_summary$all.ln_msg <- log(df_summary$all.nbr_of_msg)
df_summary$all.bullishness_idx <- log((1+df_summary$all.bullish)/(1+df_summary$all.bearish))
df_summary$all.agreement_idx <- abs(df_summary$all.bullish-df_summary$all.bearish)/(df_summary$all.bullish+df_summary$all.bearish)
head(df_summary)
tail(df_summary)

df_final <- merge(x = df_final,
                y = df_summary,
                by.x = "kode",
                by.y = "Actual.kode",
                all.x = TRUE)
df_final <- mutate(df_final, all.agreement_idx = coalesce(all.agreement_idx, 0)) %>%
  arrange(kode)
head(df_final)

# fase 1
df_fase1 <- df_all %>%
  filter(Actual.fase == 'fase 1')
print(nrow(df_fase1))
head(df_fase1)
df_summary_fase1 <- df_fase1 %>%
  mutate(bullish = case_when(Predicted_class == 1 ~ 1, .default = 0),
         neutral = case_when(Predicted_class == 0 ~ 1, .default = 0),
         bearish = case_when(Predicted_class == -1 ~ 1, .default = 0)) %>%
  group_by(Actual.kode) %>%
  summarise(fase1.avg_sentiment = mean(Predicted_class), 
            fase1.nbr_of_msg=n(),
            fase1.bullish = sum(bullish),
            fase1.neutral = sum(neutral),
            fase1.bearish = sum(bearish)) %>%
  as.data.frame()
head(df_summary_fase1)
df_summary_fase1$fase1.ln_msg <- log(df_summary_fase1$fase1.nbr_of_msg)
df_summary_fase1$fase1.bullishness_idx <- log((1+df_summary_fase1$fase1.bullish)/(1+df_summary_fase1$fase1.bearish))
df_summary_fase1$fase1.agreement_idx <- abs(df_summary_fase1$fase1.bullish-df_summary_fase1$fase1.bearish)/(df_summary_fase1$fase1.bullish+df_summary_fase1$fase1.bearish)
head(df_summary_fase1)
tail(df_summary_fase1)

df_final <- merge(x = df_final,
                y = df_summary_fase1,
                by.x = "kode",
                by.y = "Actual.kode",
                all.x = TRUE)
df_final <- df_final %>%
  mutate(fase1.avg_sentiment = coalesce(fase1.avg_sentiment, 0),
         fase1.nbr_of_msg = coalesce(fase1.nbr_of_msg, 0),
         fase1.bullish = coalesce(fase1.bullish, 0),
         fase1.neutral = coalesce(fase1.neutral, 0),
         fase1.bearish = coalesce(fase1.bearish, 0),
         fase1.ln_msg = coalesce(fase1.ln_msg, 0),
         fase1.bullishness_idx = coalesce(fase1.bullishness_idx, 0),
         fase1.agreement_idx = coalesce(fase1.agreement_idx, 0)) %>%
  arrange(kode)

# fase 2
df_fase2 <- df_all %>%
  filter(Actual.fase == 'fase 2')
print(nrow(df_fase2))
head(df_fase2)
df_summary_fase2 <- df_fase2 %>%
  mutate(bullish = case_when(Predicted_class == 1 ~ 1, .default = 0),
         neutral = case_when(Predicted_class == 0 ~ 1, .default = 0),
         bearish = case_when(Predicted_class == -1 ~ 1, .default = 0)) %>%
  group_by(Actual.kode) %>%
  summarise(fase2.avg_sentiment = mean(Predicted_class), 
            fase2.nbr_of_msg=n(),
            fase2.bullish = sum(bullish),
            fase2.neutral = sum(neutral),
            fase2.bearish = sum(bearish)) %>%
  as.data.frame()
df_summary_fase2$fase2.ln_msg <- log(df_summary_fase2$fase2.nbr_of_msg)
df_summary_fase2$fase2.bullishness_idx <- log((1+df_summary_fase2$fase2.bullish)/(1+df_summary_fase2$fase2.bearish))
df_summary_fase2$fase2.agreement_idx <- abs(df_summary_fase2$fase2.bullish-df_summary_fase2$fase2.bearish)/(df_summary_fase2$fase2.bullish+df_summary_fase2$fase2.bearish)
head(df_summary_fase2)
tail(df_summary_fase2)

df_final <- merge(x = df_final,
                y = df_summary_fase2,
                by.x = "kode",
                by.y = "Actual.kode",
                all.x = TRUE)
df_final <- mutate(df_final, fase2.agreement_idx = coalesce(fase2.agreement_idx, 0)) %>%
  arrange(kode)
head(df_final)

write.csv(df_final, 'data_analysis_emiten_20230530.csv')

df_summary <- as.data.frame(summary(df_final))
write.csv(df_summary, 'data_analysis_emiten_summary_20230530.csv')

summary(df_final)

View(df_final)

