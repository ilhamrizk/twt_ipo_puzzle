library(dplyr)
library(stringr)
library(ggplot2)
library(reshape2)

options("digits" = 7)

df <- read.csv('data_final_plusbhar500_20230530.csv')
colnames(df)
#View(df)

# Hipotesis 1
model_lm_h1 <- lm(price_revision_binary ~ fase1.ln_msg + fase1.bullishness_idx + fase1.agreement_idx, 
            data = df)
summary(model_lm_h1)

model_glm_h1 <- glm(formula = price_revision_binary ~ fase1.ln_msg + fase1.bullishness_idx + fase1.agreement_idx,
                    family = binomial,
                    data = df)
summary(model_glm_h1)

# Hipotesis 2
model_lm_h2 <- lm(initial.return ~ fase2.ln_msg + fase2.bullishness_idx + fase2.agreement_idx,
                  data = df)
summary(model_lm_h2)
model_lm_h2 <- lm(initial.return ~ all.ln_msg + all.bullishness_idx + all.agreement_idx,
                  data = df)
summary(model_lm_h2)

select(df, 'Sektor') %>%
  distinct()

# Hipotesis 3
model_lm_h3 <- lm(bhar500 ~ all.ln_msg + all.bullishness_idx + all.agreement_idx,
                  data = df)
summary(model_lm_h3)
model_lm_h3 <- lm(bhar500 ~ fase2.ln_msg + fase2.bullishness_idx + fase2.agreement_idx,
                  data = df)
summary(model_lm_h3)

# Hipotesis 3 BHAR250
model_lm_h3 <- lm(bhar250 ~ all.ln_msg + all.bullishness_idx + all.agreement_idx,
                  data = df)
summary(model_lm_h3)
model_lm_h3 <- lm(bhar250 ~ fase2.ln_msg + fase2.bullishness_idx + fase2.agreement_idx,
                  data = df)
summary(model_lm_h3)
