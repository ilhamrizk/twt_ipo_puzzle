library(dplyr)
library(stringr)
library(ggplot2)
library(reshape2)

options("digits" = 7)

df <- read.csv('data_final_plusbhar500_20230530.csv')

# Hipotesis 1 Fase1
df_h1 <- select(df, price_revision_binary, fase1.ln_msg, fase1.bullishness_idx, fase1.agreement_idx,
                market_condition, ssr_percent, ln.ast, debt, ln.age, VC.backed.dummy, Underwriter.dummy,
                Gross.proceeds)
head(df_h1)
model_glm_h1 <- glm(formula = price_revision_binary ~ fase1.ln_msg + fase1.bullishness_idx + fase1.agreement_idx
                    + market_condition + ssr_percent + ln.ast + debt
                    + ln.age + VC.backed.dummy + Underwriter.dummy + Gross.proceeds, 
                    family = binomial, 
                    data = df)
summary(model_glm_h1)

# hipotesis 2 all
df_h2_all = select(df, 
                   initial.return,
                   all.ln_msg,
                   all.bullishness_idx,
                   all.agreement_idx,
                   price_revision_binary,
                   market_condition,
                   ssr_percent,
                   ln.ast,
                   debt,
                   ln.age,
                   VC.backed.dummy,
                   Underwriter.dummy,
                   Gross.proceeds)
str(df_h3_fase2)
model_lm_h2 <- lm(initial.return ~ all.ln_msg + all.bullishness_idx + all.agreement_idx
                  + price_revision_binary + market_condition + ssr_percent
                  + ln.ast + debt + ln.age + VC.backed.dummy
                  + Underwriter.dummy + Gross.proceeds,
                  data = df_h2_all)
summary(model_lm_h2)

# hipotesis 2 fase2
df_h2_fase2 = select(df, 
                     initial.return,
                     fase2.ln_msg,
                     fase2.bullishness_idx,
                     fase2.agreement_idx,
                     price_revision_binary,
                     market_condition,
                     ssr_percent,
                     ln.ast,
                     debt,
                     ln.age,
                     VC.backed.dummy,
                     Underwriter.dummy,
                     Gross.proceeds)
str(df_h3_fase2)
model_lm_h2 <- lm(initial.return ~ fase2.ln_msg + fase2.bullishness_idx + fase2.agreement_idx
                  + price_revision_binary + market_condition + ssr_percent
                  + ln.ast + debt + ln.age + VC.backed.dummy
                  + Underwriter.dummy + Gross.proceeds,
                  data = df_h2_fase2)
summary(model_lm_h2)

# Hipotesis 3 All (BHAR500)
df_h3_all = select(df, 
                   bhar500,
                   all.ln_msg,
                   all.bullishness_idx,
                   all.agreement_idx,
                   ln.ast,
                   debt,
                   ln.age,
                   VC.backed.dummy,
                   Underwriter.dummy,
                   Gross.proceeds)
str(df_h3_all)
model_lm_h3 <- lm(bhar500 ~ all.ln_msg + all.bullishness_idx + all.agreement_idx
                  + ln.ast + debt + ln.age + VC.backed.dummy + Underwriter.dummy
                  + Gross.proceeds,
                  data = df_h3_all)
summary(model_lm_h3)

# Hipotesis 3 Fase2 (BHAR500)
df_h3_fase2 = select(df, 
                     bhar500,
                     fase2.ln_msg,
                     fase2.bullishness_idx,
                     fase2.agreement_idx,
                     ln.ast,
                     debt,
                     ln.age,
                     VC.backed.dummy,
                     Underwriter.dummy,
                     Gross.proceeds)
str(df_h3_fase2)
model_lm_h3 <- lm(bhar500 ~ fase2.ln_msg + fase2.bullishness_idx + fase2.agreement_idx
                  + ln.ast + debt + ln.age + VC.backed.dummy + Underwriter.dummy
                  + Gross.proceeds,
                  data = df_h3_fase2)
summary(model_lm_h3)


# Hipotesis 3 All (BHAR250)
df_h3_all = select(df, 
                   bhar250,
                   all.ln_msg,
                   all.bullishness_idx,
                   all.agreement_idx,
                   ln.ast,
                   debt,
                   ln.age,
                   VC.backed.dummy,
                   Underwriter.dummy,
                   Gross.proceeds)
str(df_h3_all)
model_lm_h3 <- lm(bhar250 ~ all.ln_msg + all.bullishness_idx + all.agreement_idx
                  + ln.ast + debt + ln.age + VC.backed.dummy + Underwriter.dummy
                  + Gross.proceeds,
                  data = df_h3_all)
summary(model_lm_h3)

# Hipotesis 3 Fase2 (BHAR250)
df_h3_fase2 = select(df, 
                     bhar250,
                     fase2.ln_msg,
                     fase2.bullishness_idx,
                     fase2.agreement_idx,
                     ln.ast,
                     debt,
                     ln.age,
                     VC.backed.dummy,
                     Underwriter.dummy,
                     Gross.proceeds)
str(df_h3_fase2)
model_lm_h3 <- lm(bhar250 ~ fase2.ln_msg + fase2.bullishness_idx + fase2.agreement_idx
                  + ln.ast + debt + ln.age + VC.backed.dummy + Underwriter.dummy
                  + Gross.proceeds,
                  data = df_h3_fase2)
summary(model_lm_h3)
