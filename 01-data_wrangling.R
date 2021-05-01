# # # Script para artigo: 
# "COVID-19 e Abstenção Eleitoral: uma análise
# exploratória sobre as taxas de participação
# do eleitorado nas eleições municipais de 2020"
#
# Autores: 
# Matheus Cavalcanti Pestana / IESP-UERJ / Email: matheus.pestana@iesp.uerj.br
# Guilherme Dall'Orto Rocha / IESP-UERJ / Email: guilhermedallortorocha@iesp.uerj.br

# Pacotes utilizados
pacman::p_load(tidyverse,
               janitor,
               vroom,
               bigrquery,
               DBI, 
               fst)


# Dados COVID -------------------------------------------------------------

## Covid - banco principal, atualizado em 30 de janeiro de 2021
# covid <- vroom("HIST_PAINEL_COVIDBR.csv") %>%
#   clean_names() 
# Como o arquivo acima era muito pesado para o github (~140mb), converti para .fst,
# um formato comprimido (8.2mb). Por conta disso, alguns processamentos, como
# os das linhas 48-52 já foram incorporados no arquivo. 
# Os dados originais podem ser baixados em https://covid.saude.gov.br/
covid <- read.fst("covid.fst")

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

# Fazendo o joining dos IDs no banco principal - VER LINHA 
# covid %>% 
#   left_join(ids_regiao) %>% 
#   left_join(ids_estados) %>% 
#   left_join(ids_municipios) -> covid

# Removendo os dados agregados por estado/região (município constando como vazio)
covid_mun <- covid %>% 
  filter(!is.na(municipio)) %>% 
  mutate(municipio = str_to_upper(municipio))

# Criando banco com a filtragem do sábado,o dia anterior às eleições (14-11-2020)
covid_sabado <- covid_mun %>% 
  filter(data == as.Date("2020-11-14")) %>% 
  rename("codigo_ibge" = codmun) %>% 
  mutate(pct_casos_acumulados = casos_acumulado/populacao_tcu2019,
         pct_obitos_acumulados = obitos_acumulado/populacao_tcu2019) %>% 
  select(codigo_ibge, coduf, estado, starts_with("pct"), municipio, starts_with("id_")) 


# Dados TSE ---------------------------------------------------------------

# Criando conexão SQL para baixar os dados do BigQuery (BaseDosDados)

con <- dbConnect(
  bigrquery::bigquery(),
  project = "basedosdados",
  dataset = "br_tse_eleicoes",
  billing = "bases-295912"
)

# Query SQL para pegar os dados de votação por município desde 1998
tse = "SELECT * FROM `basedosdados.br_tse_eleicoes.detalhes_votacao_municipio`;"

# Baixando os dados
tse <- dbGetQuery(con, tse, page_size = 20000)

# CPara podermos geo-referenciar, adicionamos ao banco os IDs dados
# pelo IBGE, e não pelo TSE, dado que usam sistemas diferentes. 
# O padrão é o IBGE. 


# Código dos municípios do IBGE -------------------------------------------

ibge_id <- vroom("municipios_brasileiros_tse.csv") %>% 
  select(codigo_tse, capital, codigo_ibge, nome_municipio) %>% 
  rename("id_municipio_tse" = "codigo_tse",
         "municipio" = nome_municipio)



# Joining -----------------------------------------------------------------

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
# está agrupado por CARGO. Consideramos o comparecimento/abstenção no cargo mais alto 
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

# Resolvendo os NAs:
tse <- tse %>% 
  mutate(codigo_sus = as.double(str_sub(codigo_ibge, end = -2)))

# Criando o banco de 2020
tse_2020 <- tse %>%
  filter(ano == 2020, cargo == "prefeito", turno == 1, sigla_uf != "AP") %>%
  left_join(covid_sabado,
            by = c("sigla_uf" = "estado", "codigo_sus" = "codigo_ibge")) %>%
  mutate(prop_abstencoes = 1 - prop_comparecimento) %>%
  left_join(tse_diff %>% select(codigo_ibge, abst_diff)) %>% 
  select(-municipio.y) %>% 
  mutate(prop_abst = -1*(1-(prop_abstencoes/(prop_abstencoes-abst_diff))))

#save.image("covid.RData")
