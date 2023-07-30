library(dplyr)
library(stringr)
library(ggplot2)

options("digits" = 22)

df <- read.csv('data_final_20230530.csv')
nrow(df)

df_open <- read.csv('data_market_condition_open.csv') 
df_open_min <- df_open %>%
  group_by(kode, kode_reff) %>%
  summarise(min_date = min(Date))

df_open_fix <- merge(x = df_open_min,
                     y = df_open,
                     by = c('kode' = 'kode', 
                            'kode_reff' = 'kode_reff'),
                     all.x = TRUE) %>%
  filter(min_date == Date) %>%
  select(kode, kode_reff, min_date, Open)

df_close <- read.csv('data_market_condition_close.csv')
df_close_max <- df_close %>%
  group_by(kode, kode_reff) %>%
  summarise(max_date = max(Date))
df_close_fix <- merge(x = df_close_max,
                      y = df_close,
                      by = c('kode' = 'kode', 
                             'kode_reff' = 'kode_reff'),
                      all.x = TRUE) %>%
  filter(max_date == Date) %>%
  select(kode, kode_reff, max_date, Close)

df_market_cond <- merge(x = df_open_fix,
                        y = df_close_fix,
                        by = c('kode' = 'kode', 'kode_reff' = 'kode_reff'))
df_market_cond$return <- (df_market_cond$Close - df_market_cond$Open)/df_market_cond$Open
diff <- difftime(df_market_cond$min_date, df_market_cond$max_date, units="days")
df_market_cond$diff <- as.numeric(gsub("([^0-9]+)", "", diff))
head(df_market_cond)

df_market_cond_ag <- df_market_cond %>%
  group_by(kode) %>%
  summarise(avg_return_marcon = mean(return),
            different_dt_marcon = mean(diff)) %>%
  as.data.frame()

df_market_cond_ag$market_condition <- df_market_cond_ag$avg_return_marcon * df_market_cond_ag$different_dt_marcon
head(df_market_cond_ag)

df_final <- merge(x = df,
                  y = df_market_cond_ag,
                  by.x = 'kode',
                  by.y = 'kode',
                  all.x = TRUE)

write.csv(df_final, 'data_final_new_20230530.csv')
