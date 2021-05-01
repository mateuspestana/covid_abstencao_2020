# # # Script para artigo: 
# "COVID-19 e Abstenção Eleitoral: uma análise
# exploratória sobre as taxas de participação
# do eleitorado nas eleições municipais de 2020"
#
# Autores: 
# Matheus Cavalcanti Pestana / IESP-UERJ / Email: matheus.pestana@iesp.uerj.br
# Guilherme Dall'Orto Rocha / IESP-UERJ / Email: guilhermedallortorocha@iesp.uerj.br

# Pacotes utilizados
pacman::p_load(lme4,
               performance,
               see,
               broom,
               broom.mixed,
               stargazer, 
               tidyverse,
               flextable)


# Modelos Lineares -----------------------------------------------------------------



# Mod1_a (não utilizado)
mod1_a <-
  lm(prop_abstencoes ~ pct_casos_acumulados, data = tse_2020 %>% 
       mutate(prop_abstencoes = 100*prop_abstencoes,
              pct_casos_acumulados = 100*pct_casos_acumulados))
summary(mod1_a)

# Mod1_b (não utilizado)
 mod1_b <-
  lm(prop_abstencoes ~ pct_obitos_acumulados, data = tse_2020)
summary(mod1_b)

# Mod2_a (equivalente ao modelo 1 do artigo)
mod2_a <- lm(abst_diff ~ pct_casos_acumulados, data = tse_2020 %>% mutate(prop_abstencoes = 100*prop_abstencoes,
                                                                     pct_casos_acumulados = 100*pct_casos_acumulados))
summary(mod2_a)

# Mod2_b (equivalente ao modelo 2 do artigo)
mod2_b <- lm(abst_diff ~ pct_obitos_acumulados, data = tse_2020 %>% 
               mutate(abst_diff = 100*abst_diff,
                      pct_obitos_acumulados = 100*pct_obitos_acumulados))
summary(mod2_b)

# Mod_pro1 (equivalente ao modelo 3 do artigo)
mod_prop1 <- lm(prop_abst ~ pct_casos_acumulados, data = tse_2020 %>% 
                 mutate(prop_abstencoes = 100*prop_abstencoes,
                        pct_casos_acumulados = 100*pct_casos_acumulados))
summary(mod_prop1)

# Mod_prop2 (equivalente ao modelo 4 do artigo)
mod_prop2 <- lm(prop_abst ~ pct_obitos_acumulados, data = tse_2020 %>% 
                  mutate(prop_abstencoes = 100*prop_abstencoes,
                         pct_obitos_acumulados = 100*pct_obitos_acumulados))

# Tabela 3 - Resultados dos modelos lineares
stargazer(mod2_a, mod2_b,mod_prop1, mod_prop2, type = "text")


# Comparação de performance dos 4 modelos entre si
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

# Comparação de coeficientes e outras estatísticas dos modelos
qflextable(mods)



# Modelos hierárquicos ----------------------------------------------------
 
# Mod3_reg (Não utiizado)
mod3_reg <- lmer(abst_diff ~ pct_obitos_acumulados + (1 | id_regiao), data = tse_2020, na.action = "na.omit")
summary(mod3_reg)
icc(mod3_reg)

# Mod3_estado (equivalente ao modelo 5 do artigo)
mod3_estado <- lmer(abst_diff ~ pct_obitos_acumulados + (1 | id_estado), data = tse_2020, na.action = "na.omit")
summary(mod3_estado)
icc(mod3_estado)

# Mod3_regestado (não utilizado)
mod3_regestado <- lmer(abst_diff ~ pct_obitos_acumulados + (1| id_regiao) + (1 | id_estado), data = tse_2020, na.action = "na.omit")
summary(mod3_regestado)
icc(mod3_regestado)

# Comparando os 3 modelos e escolhendo o mod3_estado
compare_performance(mod3_reg, mod3_estado, mod3_regestado)

# Mod4_a (equivalente ao modelo 6 do artigo)
mod4_a <- lmer(abst_diff ~ pct_obitos_acumulados + scale(aptos_tot) + (scale(aptos_tot) | id_estado), data = tse_2020, na.action = "na.omit")
summary(mod4_a)
icc(mod4_a)

# Mod4_b (Não utilizado)
mod4_b <- lmer(abst_diff ~ pct_obitos_acumulados * scale(aptos_tot) + (scale(aptos_tot) | id_estado), data = tse_2020, na.action = "na.omit")
summary(mod4_b)
icc(mod4_b)

# Mod4_c (Não utilizado)
mod4_c <- lmer(abst_diff ~ pct_obitos_acumulados + scale(aptos_tot) + (scale(aptos_tot) | id_estado), data = tse_2020, na.action = "na.omit")
summary(mod4_c)
icc(mod4_c)

# Comparando os 3 modelos e escolhendoo mod4_a
compare_performance(mod4_a, mod4_b, mod4_c)

# Tabela 4 - Resultados dos modelos lineares multinível (variável de diferença percentual)
stargazer(mod3_estado, mod4_a, type = "text")


mod5_estado <- lmer(prop_abst ~ pct_obitos_acumulados + (1 | id_estado), data = tse_2020, na.action = "na.omit")
mod5_a <- lmer(prop_abst ~ pct_obitos_acumulados + scale(aptos_tot) + (scale(aptos_tot) | id_estado), data = tse_2020, na.action = "na.omit")

# Tabela 5 - Resultados dos modelos lineares multinível (variável de diferenca proporcional)
stargazer(mod5_estado, mod5_a, type = "text")

# save.image("bancos.RData")