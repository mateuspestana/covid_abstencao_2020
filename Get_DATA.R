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

covid_mun <- covid %>% 
  filter(!is.na(municipio)) %>% # Filtra pra sábado
  mutate(municipio = str_to_upper(municipio))

covid_sabado <- covid_mun %>% 
  filter(data == as.Date("2020-11-14")) %>% 
  rename("codigo_ibge" = codmun) %>% 
  select(codigo_ibge, coduf, estado, starts_with("pct"), municipio)

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

# Salvei a imagem em um RData, logo...
#save.image("bancos.RData")
#load("bancos.RData")
