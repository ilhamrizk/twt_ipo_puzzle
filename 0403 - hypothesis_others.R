install.packages("reshape2")
install.packages("caret")

library(dplyr)
library(stringr)
library(ggplot2)
library(reshape2)
#library(caret)

str(df_h3_fase2)
model_lm_h2 <- lm(initial.return ~ fase2.ln_msg + fase2.bullishness_idx + fase2.agreement_idx
                  + ln.ast + debt + ln.age + VC.backed.dummy + Underwriter.dummy
                  + Gross.proceeds,
                  data = df_h3_fase2)
summary(model_lm_h2)




df$fase1.nbr_of_msg <- case_when(df$fase1.nbr_of_msg == 0 ~ 43,
                                 TRUE ~ df$fase1.nbr_of_msg)
df$fase1.ln_msg <- log10(df$fase1.nbr_of_msg)
nrow(df)
model_glm_h1 <- glm(formula = price_revision_binary ~ fase1.ln_msg + fase1.bullishness_idx + fase1.agreement_idx,
                    family = binomial, 
                    data = df)

df <- read.csv('data_final_20230509.csv')
df$price_revision_binary <- case_when((df$penawaran_awal_max < df$penawaran_awal_min) 
                                      & (df$price_revision_binary == 0) ~ 1,
                                      (df$penawaran_awal_max < df$penawaran_awal_min) 
                                      & (df$price_revision_binary == 1) ~ 0,
                                      TRUE ~ df$price_revision_binary)
df <- filter(df, fase1.nbr_of_msg > 0)
filter(df, penawaran_awal_max < penawaran_awal_min )

initial.return_norm <- as.data.frame(scale(df$initial.return))
df$initial.return_norm <- case_when(initial.return_norm$V1 >= 0 ~ 1,
                                    initial.return_norm$V1 < 0 ~ 0)
summary(df$initial.return_norm)

df$initial.return_norm <- initial.return_norm$V1
pre <- preProcess(as.data.frame(df$initial.return), method=c("range"))
df$initial.return_norm <- predict(pre, as.data.frame(df$initial.return))
head(df$initial.return_norm)
summary(df$initial.return_norm)

initial.return_norm <- as.data.frame(scale(df$initial.return))
df$initial.return_norm <- case_when(initial.return_norm$V1 >= 0 ~ 1,
                                    initial.return_norm$V1 < 0 ~ 0)
summary(df$initial.return_norm)
model_glm_h2 <- glm(formula = initial.return_norm ~ fase2.ln_msg + fase2.bullishness_idx + fase2.agreement_idx,
                    family = poisson(), 
                    data = df)
summary(model_glm_h2)

cor(df$price_revision_binary, df$fase1.ln_msg)
cor(df$price_revision_binary, df$fase1.bullishness_idx)
cor(df$price_revision_binary, df$fase1.agreement_idx)

cor(df$initial.return, df$fase1.ln_msg)
cor(df$initial.return, df$fase1.bullishness_idx)
cor(df$initial.return, df$fase1.agreement_idx)

df_num <- subset(df, select = -c(X, kode, Nama.Emiten, Sektor, tanggal_ipo, bookbuilding) )

# creating correlation matrix
corr_mat <- round(cor(df_num),2)

# reduce the size of correlation matrix
corr_mat <- cor(df_num,
                method = 'spearman',
                use="pairwise.complete.obs")
melted_corr_mat <- melt(corr_mat)
head(melted_corr_mat)

ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) + 
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 45, hjust=1))


# Load and install heatmaply package
install.packages("heatmaply")
library(heatmaply)

# plotting corr heatmap
heatmaply_cor(x = cor(df_num, method = 'spearman', use="pairwise.complete.obs"), k_col = 2, k_row = 2)


df <- read.csv('data_final_20230509.csv')
df_num <- subset(df, select = -c(kode, Nama.Emiten, Sektor, tanggal_ipo, bookbuilding) )
df_corr_pearson <- round(cor(df_num, method = 'pearson'),2) %>%
  as.data.frame() %>%
  select(X, bhar250)
df_corr_pearson$abs_bhar250 <- abs(df_corr_pearson$bhar250)
arrange(df_corr_pearson, abs_bhar250)

df_corr_spearman <- round(cor(df_num, method = 'spearman'),2)
write.csv(df_corr_pearson, 'data_corr_pearson_20230509.csv')
write.csv(df_corr_spearman, 'data_corr_spearman_20230509.csv')

df_num <- select(df_final, fase1.ln_msg, fase1.bullishness_idx,fase1.agreement_idx,
                 fase2.ln_msg, fase2.bullishness_idx,fase2.agreement_idx,
                 ln.age, ssr, ln.ast, debt)
df_corr_pearson <- round(cor(df_num, method = 'pearson', use="pairwise.complete.obs"),4)
df_corr_spearman <- round(cor(df_num, method = 'spearman', use="pairwise.complete.obs"),4)
write.csv(df_corr_pearson, 'data_corr_pearson_20230509.csv')
write.csv(df_corr_spearman, 'data_corr_spearman_20230509.csv')

df$AGE <- as.integer(df_$AGE)
df$aset <- as.numeric(gsub("[^0-9]", "", gsub(",00", "", df_$Aset)))
df$DEBT <- as.numeric(gsub(",", ".", df_$DEBT))
df$SSR <- as.numeric(gsub("[^0-9]", "", df_$outstanding.shares..SSR.))
df$gross_proceeds <- as.numeric(gsub("[^0-9]", "", gsub(",00", "", df_$Gross.proceeds)))