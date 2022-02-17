
#exercício 2
library(basedosdados)
library(tidyverse)
library(data.table)
library(ggplot2)
library(zoo)
library(lubridate)
library(reshape2)

#baixando dados - crime
basedosdados::set_billing_id("projeto-estatistica-341521")
query <- "SELECT*FROM basedosdados.br_isp_estatisticas_seguranca.taxa_evolucao_mensal_uf"
df_crime = basedosdados::read_sql(query)
write.table(df_crime, file = "dados_crime.csv", col.names = TRUE)

#criando coluna para data "date"
df_crime <- df_crime %>%
  mutate(date = make_date(ano, mes, 1), .after = mes_ano)

#fazendo a média anual
query2 <- bdplyr("br_isp_estatisticas_seguranca.taxa_evolucao_mensal_uf") %>%
  select(ano, roubo_celular, furto_celular) %>%
  group_by(ano) %>%
  summarise(roubo = mean(roubo_celular, na.rm = TRUE),
            furto = mean(furto_celular, na.rm = TRUE))

df_cel_m = bd_collect(query2)

#montado gráfico

df_cel_melt <- melt(df_cel_m, id.vars = 'ano', variable.name = 'series')

df_cel_melt %>%
  ggplot(aes(x=ano, y=value)) + 
  geom_line(aes(color = series)) + 
  scale_y_continuous(breaks = c(1,3,5,7,9,11,13)) +
  labs(title="Média anual de furto e roubo de celular \nna cidade do Rio de Janeiro", 
       y = "Roubo e furto a cada 100mil habitantes",
       x = "ano")


#baixando dados - educação
basedosdados::set_billing_id("projeto-estatistica-341521")
query2 <- "SELECT*FROM basedosdados.br_inep_ideb.municipio"
df_ideb = basedosdados::read_sql(query2)
write.table(df_ideb, file = "dados_ideb.csv", col.names = TRUE)

#média por estado
query3 <- bdplyr("br_inep_ideb.municipio") %>%
  select(ano, sigla_uf, ideb) %>%
  filter(ano == 2019) %>%
  group_by(sigla_uf) %>%
  summarise(ideb_medio = mean(ideb, na.rm = TRUE))

df_ideb_m = bd_collect(query3)

#criando regiões
sudeste <- c("SP", "RJ", "MG", "ES")
co <- c("MT", "GO", "MS", "DF")
sul <- c("PR", "SC", "RS")
norte <- c("AC", "AM", "RO", "RR", "PA", "AP", "TO")
nordeste <- c("MA", "PI", "BA", "CE", "RN", "PB", "PE", "AL",
              "SE", "BA")

df_ideb_m <- df_ideb_m %>%
  mutate(regiao = if_else(sigla_uf %in% sudeste, "sudeste", 
                          if_else(sigla_uf %in% co, "centro_oeste",
                                  if_else(sigla_uf %in% sul, "sul",
                                          if_else(sigla_uf %in% norte, "norte", "nordeste")))))

ideb_regiao <- df_ideb_m %>%
  group_by(regiao) %>%
  summarise(media_regiao = mean(ideb_medio, na.rm = TRUE))

#gráfico
ideb_regiao %>%
  ggplot(aes(x=regiao, y=media_regiao)) + 
  geom_bar(stat = "identity") + 
  labs(title="Média ideb em 2019 por região do país", 
       y = "Ideb",
       x = "Região")


