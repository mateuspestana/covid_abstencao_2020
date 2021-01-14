pacman::p_load(lme4,
               performance,
               see)

# Testando alguns modelos

# Mas antes, preparando o banco:
# Filtrando covid_mun pelo sábado anterior à eleição (onde as pessoas
# em tese decidiriam)

tse_2020 <- tse %>%
  filter(ano == 2020, cargo == "prefeito", turno == 1) %>%
  left_join(covid_sabado,
            by = c("municipio", "sigla_uf" = "estado")
      ) %>% 
  mutate(prop_abstencoes = 1-prop_comparecimento)

nas <- tse_2020 %>% 
  filter(is.na(coduf))

# 42 NAs! Depois a gente vasculha eles...

mod1_a <- lm(prop_abstencoes ~ pct_casos_acumulados, data = tse_2020)
summary(mod1_a)
mod1_b <- lm(prop_abstencoes ~ pct_obitos_acumulados, data = tse_2020)
