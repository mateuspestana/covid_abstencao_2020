pacman::p_load(lme4,
               performance,
               see,
               broom,
               broom.mixed)

# Testando alguns modelos

# Mas antes, preparando o banco:
# Filtrando covid_mun pelo sábado anterior à eleição (onde as pessoas
# em tese decidiriam)

tse_2020 <- tse %>%
  filter(ano == 2020, cargo == "prefeito", turno == 1) %>%
  left_join(covid_sabado,
            by = c("municipio", "sigla_uf" = "estado")) %>%
  mutate(prop_abstencoes = 1 - prop_comparecimento) %>%
  left_join(tse_diff %>% select(codigo_ibge, abst_diff),
            by = c("codigo_ibge.x" = "codigo_ibge"))

nas <- tse_2020 %>%
  filter(is.na(coduf))

# 42 NAs! Depois a gente vasculha eles...

mod1_a <-
  lm(prop_abstencoes ~ pct_casos_acumulados, data = tse_2020)
summary(mod1_a)
mod1_b <-
  lm(prop_abstencoes ~ pct_obitos_acumulados, data = tse_2020)
summary(mod1_b)

mod2_a <- lm(abst_diff ~ pct_casos_acumulados, data = tse_2020)
summary(mod2_a)
mod2_b <- lm(abst_diff ~ pct_obitos_acumulados, data = tse_2020)
summary(mod2_b)

compare_performance(mod1_a, mod1_b)
compare_performance(mod2_a, mod2_b)

plot(compare_performance(mod1_a, mod1_b))
plot(compare_performance(mod2_a, mod2_b))

mods <- bind_rows(
  as.data.frame(tidy(mod1_a, conf.int = T)),
  as.data.frame(tidy(mod1_b, conf.int = T)),
  as.data.frame(tidy(mod2_a, conf.int = T)),
  as.data.frame(tidy(mod2_b, conf.int = T))
)

mods <-
  data.frame(
    mods,
    modelo = c(
      "mod1_a",
      "mod1_a",
      "mod1_b",
      "mod1_b",
      "mod2_a",
      "mod2_a",
      "mod2_b",
      "mod2_b"
    )
  ) %>% 
  select(modelo, everything())

# Óbito é melhor de usar, e o DIFF também. 

# Vamos tentar rodar um hierárquico para ver o quanto o ESTADO auxilia nisso 
# 
# # TODO: Criar IDs melhores para os estados e rodar os modelos novamente. 
mod3_a <- lmer(abst_diff ~ pct_obitos_acumulados + (1 | coduf), data = tse_2020, na.action = "na.omit")
summary(mod3_a)
icc(mod3_a)

mod4_a <- lmer(abst_diff ~ pct_obitos_acumulados + scale(aptos_tot) + (scale(aptos_tot) | coduf), data = tse_2020, na.action = "na.omit")
summary(mod4_a)
icc(mod4_a)

mod4_b <- lmer(abst_diff ~ pct_obitos_acumulados * scale(aptos_tot) + (scale(aptos_tot) | coduf), data = tse_2020, na.action = "na.omit")
summary(mod4_b)
icc(mod4_b)

compare_performance(mod3_a, mod4_a, mod4_b)
