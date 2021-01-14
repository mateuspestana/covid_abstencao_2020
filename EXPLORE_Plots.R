pacman::p_load(tidyverse,
               DataExplorer,
               hrbrthemes, 
               ggrepel)
# Carregando os bancos
load("bancos.RData")

# Definindo o tema (perfumaria, mas só pra ser bonitinho)
#import_titillium_web()
theme_set(hrbrthemes::theme_ipsum_tw())

# Plot só pra ver a curva dos estados...
ggplot(data = covid %>%
         filter(regiao != "Brasil",
                semana_epi > 9) %>% 
         group_by(regiao, estado, semana_epi) %>% 
         summarise(soma_casos = sum(casos_acumulado, na.rm = T),
                   total = sum(populacao_tcu2019, na.rm = T)) %>% 
         ungroup() %>% 
         mutate(pct = soma_casos/total),
       aes(x = semana_epi, 
           y = pct, 
           color = estado, 
           group = estado))+
  geom_line(show.legend = F)+
  facet_wrap(~regiao)+
  labs(title = "Curvas de contágio nas regiões e estados",
       subtitle = "Por semana epidemiológica",
       y = "% de casos acumulados em relação à população do estado",
       x = "Semana epidemiológica")+
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
  scale_color_gdocs()
  