pacman::p_load(lme4,
               performance,
               see,
               broom,
               broom.mixed,
               stargazer, tidyverse)

# Testando alguns modelos

# Mas antes, preparando o banco:
# Filtrando covid_mun pelo sábado anterior à eleição (onde as pessoas
# em tese decidiriam)

tse_2020 <- tse %>%
  filter(ano == 2020, cargo == "prefeito", turno == 1, sigla_uf != "AP") %>%
  left_join(covid_sabado,
            by = c("sigla_uf" = "estado", "codigo_sus" = "codigo_ibge")) %>%
  mutate(prop_abstencoes = 1 - prop_comparecimento) %>%
  left_join(tse_diff %>% select(codigo_ibge, abst_diff)) %>% 
  select(-municipio.y) %>% 
  mutate(prop_abst = 1-(prop_abstencoes/(prop_abstencoes-abst_diff)))

mod1_a <-
  lm(prop_abstencoes ~ pct_casos_acumulados, data = tse_2020 %>% 
       mutate(prop_abstencoes = 100*prop_abstencoes,
              pct_casos_acumulados = 100*pct_casos_acumulados))
summary(mod1_a)
 mod1_b <-
  lm(prop_abstencoes ~ pct_obitos_acumulados, data = tse_2020)
summary(mod1_b)

mod2_a <- lm(abst_diff ~ pct_casos_acumulados, data = tse_2020 %>% mutate(prop_abstencoes = 100*prop_abstencoes,
                                                                     pct_casos_acumulados = 100*pct_casos_acumulados))
summary(mod2_a)
mod2_b <- lm(abst_diff ~ pct_obitos_acumulados, data = tse_2020 %>% 
               mutate(abst_diff = 100*abst_diff,
                      pct_obitos_acumulados = 100*pct_obitos_acumulados))
summary(mod2_b)

mod_prop1 <- lm(prop_abst ~ pct_casos_acumulados, data = tse_2020 %>% 
                 mutate(prop_abstencoes = 100*prop_abstencoes,
                        pct_casos_acumulados = 100*pct_casos_acumulados))
mod_prop2 <- lm(prop_abst ~ pct_obitos_acumulados, data = tse_2020 %>% 
                  mutate(prop_abstencoes = 100*prop_abstencoes,
                         pct_obitos_acumulados = 100*pct_obitos_acumulados))

stargazer(mod2_a, mod2_b,mod_prop1, mod_prop2, type = "text")

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
mod3_reg <- lmer(abst_diff ~ pct_obitos_acumulados + (1 | id_regiao), data = tse_2020, na.action = "na.omit")
summary(mod3_reg)
icc(mod3_reg)
mod3_estado <- lmer(abst_diff ~ pct_obitos_acumulados + (1 | id_estado), data = tse_2020, na.action = "na.omit")
summary(mod3_estado)
icc(mod3_estado)
mod3_regestado <- lmer(abst_diff ~ pct_obitos_acumulados + (1| id_regiao) + (1 | id_estado), data = tse_2020, na.action = "na.omit")
summary(mod3_estado)
icc(mod3_estado)
compare_performance(mod3_reg, mod3_estado, mod3_regestado)


stargazer(mod3_reg, type = "text")

mod4_a <- lmer(abst_diff ~ pct_obitos_acumulados + scale(aptos_tot) + (scale(aptos_tot) | id_estado), data = tse_2020, na.action = "na.omit")
summary(mod4_a)
icc(mod4_a)

mod4_b <- lmer(abst_diff ~ pct_obitos_acumulados * scale(aptos_tot) + (scale(aptos_tot) | id_estado), data = tse_2020, na.action = "na.omit")
summary(mod4_b)
icc(mod4_b)

mod4_c <- lmer(abst_diff ~ pct_obitos_acumulados + scale(aptos_tot) + (scale(aptos_tot) | id_estado), data = tse_2020, na.action = "na.omit")
summary(mod4_a)
summary(mod4_c)
compare_performance(mod3_a, mod4_a, mod4_c)

stargazer(mod3_estado, mod4_a, type = "text")


mod5_estado <- lmer(prop_abst ~ pct_obitos_acumulados + (1 | id_estado), data = tse_2020, na.action = "na.omit")
mod5_a <- lmer(prop_abst ~ pct_obitos_acumulados + scale(aptos_tot) + (scale(aptos_tot) | id_estado), data = tse_2020, na.action = "na.omit")

summary(mod5_a)


# Tentando prever
tse_diff %>% 
  mutate(abst_diff_pred = predict(mod4_a, )) %>% 
  select(sigla_uf, municipio, abst_diff, abst_diff_pred, prop_abstencao)

extract_eq(mod4_a)
