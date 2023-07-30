library(dplyr)
library(stringr)
library(ggplot2)

options("digits" = 22)

exclude_kode = c('CPGT', 'NAGA', 'DAJK', 'FINN', 'AMOR', 'SKBM', 'BRIS')
df <- read.csv('data_analysis_emiten_plusbhar250_20230530.csv') %>%
  filter(!kode %in% exclude_kode) %>%
  arrange(kode)
df_tambahan_age <- read.csv('data_analisis_akhir_perusahaan.csv', sep=';') %>%
  mutate(age = as.integer(AGE)) %>%
  select(kode, age) %>%
  arrange(kode)
df_tambahan_age$ln.age <- log(df_tambahan_age$age)
head(df_tambahan_age)

df_tambahan_ssr <- read.csv('Aksi_Korporasi_IPO_Tahun_2011_2022_raw.csv', sep=';') %>%
  filter( str_trim(Code) != '') %>%
  mutate(Shares.Offered = as.numeric(gsub("[^0-9]", "", gsub(",00", "", Shares.Offered))),
         Total.Listed.Shares = as.numeric(gsub("[^0-9]", "", gsub(",00", "", Total.Listed.Shares)))) %>%
  select(Code, Shares.Offered,Total.Listed.Shares) %>%
  filter( str_trim(Code) != '') %>%
  arrange(Code)
df_tambahan_ssr$ssr <- df_tambahan_ssr$Shares.Offered/df_tambahan_ssr$Total.Listed.Shares
df_tambahan_ssr$ssr_percent <- df_tambahan_ssr$ssr*100
tail(df_tambahan_ssr)

df_tambahan_debt <- read.csv('Data_analisis_akhir_ast_debt.csv', sep = ';') %>%
  mutate(aset = as.numeric(gsub("[^0-9]", "", gsub(",00", "", Aset))),
         debt = as.numeric(gsub(",", ".", DEBT.to.Asset.Ratio))) %>%
  select(kode, aset, debt)
df_tambahan_debt$ln.ast <- log(df_tambahan_debt$aset)
head(df_tambahan_debt)

df_merge_age <- merge(x = df,
                      y = df_tambahan_age,
                      by.x = 'kode',
                      by.y = 'kode',
                      all.x = TRUE)
df_merge_ssr <- merge(x = df_merge_age,
                      y = df_tambahan_ssr,
                      by.x = 'kode',
                      by.y = 'Code',
                      all.x = TRUE)
df_merge_debt <- merge(x = df_merge_ssr,
                       y = df_tambahan_debt,
                       by.x = 'kode',
                       by.y = 'kode',
                       all.x = TRUE)

df_final <- subset(df_merge_debt, select = -c(X.1, X, Sektor.y, kode.y)) %>%
  rename('Sektor' = 'Sektor.x')

df_tambahan_vcbacked <- read.csv('data_analisis_akhir_perusahaan_new.csv', 
                                 sep=';') %>%
  select(kode, 
         VC.backed.dummy, 
         Underwriter.dummy,
         Gross.proceeds) %>%
  mutate(Gross.proceeds = as.numeric(gsub("[^0-9]", "", gsub(",00", "", Gross.proceeds))))
df_final <- merge(x = df_final,
                  y = df_tambahan_vcbacked,
                  by.x = 'kode',
                  by.y = 'kode',
                  all.x = TRUE)

head(df_final)
select(df_final, Sektor) %>% distinct()
colnames(df_final)
write.csv(df_final, 'data_final_20230530.csv')