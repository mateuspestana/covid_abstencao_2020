pacman::p_load(tidyverse,
               DataExplorer,
               hrbrthemes, 
               ggrepel,
               ggthemes, 
               sf, 
               geobr,
               viridis,
               equatiomatic)

# Carregando os bancos
load("bancos.RData")

# Definindo o tema (perfumaria, mas só pra ser bonitinho)
#import_titillium_web()
theme_set(hrbrthemes::theme_ipsum_tw())

# Plot só pra ver a curva dos estados...
ggplot(data = covid %>%
         filter(regiao != "Brasil") %>% 
         group_by(regiao, estado, data) %>% 
         summarise(soma_casos = sum(casos_acumulado, na.rm = T),
                   total = sum(populacao_tcu2019, na.rm = T)) %>% 
         ungroup() %>% 
         mutate(pct = soma_casos/total),
       aes(x = data, 
           y = pct, 
           color = estado, 
           group = estado))+
  geom_line(show.legend = F)+
  facet_wrap(~regiao)+
  labs(title = "Curvas da proporção de casos acumulados nas regiões e estados",
       subtitle = "Por dia, em relação à população",
       y = "% de casos acumulados em relação à população do estado",
       x = "Data",
       source = "Corovinavírus Brasil (covid.saude.gov.br)")+
  scale_y_percent()
  #scale_color_gdocs()

ggplot(data = covid %>%
         filter(regiao != "Brasil") %>% 
         group_by(regiao, estado, data) %>% 
         summarise(soma_obitos = sum(obitos_acumulado, na.rm = T),
                   total = sum(populacao_tcu2019, na.rm = T)) %>% 
         ungroup() %>% 
         mutate(pct = soma_obitos/total),
       aes(x = data, 
           y = pct, 
           color = estado, 
           group = estado))+
  geom_line(show.legend = F)+
  facet_wrap(~regiao)+
  labs(title = "Curvas da proporção óbitos acumulados nas regiões e estados",
       subtitle = "Por dia, em relação à população",
       y = "% de óbitos acumulados em relação à população do estado",
       x = "Data")+
  scale_y_percent()

# Dados de abstenção dos estados
tse %>% 
  filter(cargo %in% c("presidente", "prefeito") & turno == 1) %>% 
  group_by(ano, ambito_eleicao) %>% 
  summarise(total_apto = sum(aptos, na.rm = T),
            total_comparecimento = sum(comparecimento, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(prop_comparecimento = total_comparecimento/total_apto,
         prop_abstencao = 1-prop_comparecimento) %>% 
  ggplot(aes(x = ano, y = prop_abstencao, color = ambito_eleicao))+
  geom_line()+
  geom_label_repel(aes(label = paste0(format(prop_abstencao*100, digits = 4), "%")), 
                       color = "black", nudge_y = 0.005, nudge_x = -0.8)+
  theme(legend.position = "bottom")+
  scale_y_percent()+
  labs(title = "Abstenção ao longo do tempo",
       subtitle = "Separado por tipo de eleição",
       x = "Ano da eleição",
       y = "% de abstenções",
       color = "Tipo de eleição")+
  scale_color_gdocs()+
  scale_x_continuous(breaks = seq(1998, 2020, 2))
  
# Tabela de abstenção em cada estado
tse %>% 
  filter(cargo %in% c("presidente", "prefeito") & turno == 1 & ano == 2020) %>% 
  group_by(sigla_uf) %>% 
  summarise(total_apto = sum(aptos, na.rm = T),
            total_comparecimento = sum(comparecimento, na.rm = T)) %>% 
  ungroup() %>% 
  transmute(sigla_uf, 
            prop_comparecimento = total_comparecimento/total_apto,
         prop_abstencao = 1-prop_comparecimento) %>% 
  export("Tabela_Estados_ABST.xlsx")

tse_diff %>% 
  group_by(sigla_uf) %>% 
  summarise(total_diff = mean(abst_diff, na.rm = T)) %>% 
  export("Tabela_Estados_ABST_DIFF.xlsx")

# Tabela de covids
covid %>%
  filter(regiao != "Brasil") %>% 
  group_by(regiao, estado, data) %>% 
  summarise(soma_obitos = sum(obitos_acumulado, na.rm = T),
            total = sum(populacao_tcu2019, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(pct = soma_obitos/total) %>% 
  filter(data == as.Date("2020-11-14")) %>% 
  arrange(-pct) %>% 
  mutate(pct = 100*pct)

covid %>%
  filter(regiao != "Brasil") %>% 
  group_by(regiao, estado, data) %>% 
  summarise(soma_obitos = sum(casos_acumulado, na.rm = T),
            total = sum(populacao_tcu2019, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(pct = soma_obitos/total) %>% 
  filter(data == as.Date("2020-11-14")) %>% 
  arrange(-pct) %>% 
  mutate(pct = 100*pct)


mapa_mun <- geobr::read_municipality()

mapa_mun %>% 
  left_join(tse_2020, by = c("code_muni" = "codigo_ibge")) %>% 
  ggplot(aes(fill = prop_abstencoes*100))+
  geom_sf(color = "grey75", size = 0.005)+
  labs(title = "Abstenção no Brasil em 2020", 
       subtitle = "Por município",
       fill = "Abstenção (%)")+
  scale_fill_gradientn(colors = inferno(100, direction = 1))
