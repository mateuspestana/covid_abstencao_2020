# Pacotes 
pacman::p_load(tidyverse,
               janitor,
               vroom,
               bigrquery,
               DBI)

# Quando importei os dados pela primeira vez:

## Covid
covid <- vroom("~/Downloads/HIST_PAINEL_COVIDBR.csv") %>%
  clean_names() 

# Criando IDs únicos para cada estado, municipio e região
covid %>% filter(regiao != "Brasil") %>% 
  select(regiao) %>% 
  distinct() %>% 
  mutate(id_regiao = as.double(rownames(.)) - 1) -> ids_regiao

covid %>% filter(regiao != "Brasil") %>% 
  select(estado) %>% 
  distinct() %>% 
  arrange(estado) %>% 
  mutate(id_estado = as.double(rownames(.)) - 1) -> ids_estados


covid %>% filter(regiao != "Brasil") %>% 
  select(codmun) %>% 
  distinct() %>% 
  arrange(codmun) %>% 
  mutate(id_municipio = as.double(rownames(.)) - 1) -> ids_municipios

# joining dos ids
covid %>% 
  left_join(ids_regiao) %>% 
  left_join(ids_estados) %>% 
  left_join(ids_municipios) -> covid

covid_mun <- covid %>% 
  filter(!is.na(municipio)) %>% 
  mutate(municipio = str_to_upper(municipio))

covid_sabado <- covid_mun %>% 
  filter(data == as.Date("2020-11-14")) %>% 
  rename("codigo_ibge" = codmun) %>% 
  mutate(pct_casos_acumulados = casos_acumulado/populacao_tcu2019,
         pct_obitos_acumulados = obitos_acumulado/populacao_tcu2019) %>% 
  select(codigo_ibge, coduf, estado, starts_with("pct"), municipio, starts_with("id_")) 

## TSE 
### isso eu baixei do BigQuery, que é uma plataforma do google onde
### pessoal guarda bancos. Tem um projeto chamado BaseDosDados que 
### armazena e processa centenas de dados brasileiros, já tratados 
### e corrigidos. Peguei de lá. (Usa SQL)

con <- dbConnect(
  bigrquery::bigquery(),
  project = "basedosdados",
  dataset = "br_tse_eleicoes",
  billing = "bases-295912"
)

tse = "SELECT * FROM `basedosdados.br_tse_eleicoes.detalhes_votacao_municipio`;"

tse <- dbGetQuery(con, tse, page_size = 20000)

# Como o banco vem com o ID do TSE, que é zoado, é interessante 
# somar o ID do IBGE, para podermos georreferenciar posteriormente

ibge_id <- vroom("municipios_brasileiros_tse.csv") %>% 
  select(codigo_tse, capital, codigo_ibge, municipio) %>% 
  rename("id_municipio_tse" = "codigo_tse")

# Juntando e corrigindo algumas imperfeições 
# As correçòes são pra termos apenas as eleições ordinárias 
tse %>% 
  left_join(ibge_id) %>% 
  filter(tipo_eleicao == "eleicao ordinaria") %>% 
  mutate(ambito_eleicao = case_when(ano %in% seq(1998, 2018, 4) ~ 
                                      "Federal/Estadual",
                                    ano %in% seq(2000, 2020, 4)  ~ 
                                      "Municipal")) %>% 
  select(1:2, ambito_eleicao, 3, codigo_ibge, everything()) %>% 
  mutate_at(vars(starts_with("prop_")), function(x) x/100) -> tse
  
# Precisamos de um banco que agrupe por ano/municipio, já que esse 
# está agrupado por CARGO. Mas temos um problema: a abstenção é distinta
# (e não entendi o por quê) em alguns anos, a depender do cargo. 
# Votou-se para deputado estadual junto com governador, por exemplo, 
# mas em muitos casos esse valor é diferente. Como solucionamos?
# Minha sugestão: considerar o comparecimento/abstenção no cargo mais alto 
# daquele pleito (Presidente e Prefeito)

# Fazendo o DIFF de 2016 pra 2020 
tse %>% 
  filter(ano %in% c(2016, 2020) & turno == 1) %>% 
  select(1:cargo, prop_comparecimento, municipio, capital) %>% 
  mutate(prop_abstencao = 1 - prop_comparecimento) %>% 
  select(-prop_comparecimento) %>% 
  pivot_wider(names_from = ano, values_from = prop_abstencao, names_prefix = "abst_") %>% 
  mutate(abst_diff = abst_2020 - abst_2016) %>% 
  pivot_longer(names_to = "ano", 
               names_prefix = "abst_", 
               cols = c("abst_2016", "abst_2020"),
               values_to = "prop_abstencao") %>% 
  filter(ano == 2020 & cargo == "prefeito") -> tse_diff

  tse_diff %>% 
  count(sign(abst_diff)) %>% 
  clean_names() %>% 
  mutate(sinal = ifelse(sign_abst_diff == 1, "Aumento", "Diminuição")) %>% 
  select(-1) %>% 
  select(sinal, "qtde" = n)
  
# TSE DIFF> análise
tse_2020 %>% 
  select(sigla_uf, abst_diff, aptos_tot) %>% 
  

# Média de eleitores aptos 
tse_2020 %>% 
  left_join(tse_diff %>% select(codigo_ibge, abst_diff), by = c("codigo_ibge.x" = "codigo_ibge")) %>% 
  pull(aptos_tot) %>% 
  summary()

# Resolvendo os NAs:
tse <- tse %>% 
  mutate(codigo_sus = as.double(str_sub(codigo_ibge, end = -2)))


# Salvei a imagem em um RData, logo...
#save.image("bancos.RData")
#load("bancos.RData")
