#Libraries
library(DBI)
library(RPostgres)
library(tidyverse)


con = dbConnect(drv = RPostgres::Postgres(),
                dbname="data_sus",
                host = "150.161.248.197",
                port = "5432",
                user = "michelle",
                password = "gratiluz")
                
#No anomalies
query_nasc_sem_anomal_ano = paste0("
SELECT 
  EXTRACT(YEAR FROM dtnasc) AS ano_nasc,
  mun_res_uf,
COUNT(*) AS n
FROM 
  sinasc
WHERE idanomal = 'não'
AND mun_res_uf IS NOT null
GROUP BY 
  ano_nasc, mun_res_uf
")
nasc_sem_anomal_ano = dbGetQuery(con, query_nasc_sem_anomal_ano)

#Priority anomalies
query4_1= paste0("
SELECT 
  EXTRACT(YEAR FROM dtnasc) AS ano_nasc,
  mun_res_uf,
  anomal_prioritarias,
COUNT(*) AS cont_anomal
FROM 
  sinasc
WHERE anomal_prioritarias <> 'outras_anomalias'
GROUP BY 
  ano_nasc, mun_res_uf, anomal_prioritarias
")

df4_1 = dbGetQuery(con, query4_1)
df4_1= df4_1 %>% 
  complete(ano_nasc,mun_res_uf,anomal_prioritarias) %>% 
  mutate(cont_anomal=replace_na(cont_anomal,0)) %>% 
  left_join(.,nasc_sem_anomal_ano,join_by(ano_nasc == ano_nasc, 
                                          mun_res_uf==mun_res_uf)) %>% 
  mutate(incidencia = (cont_anomal/(cont_anomal + n))*1e5) %>% 
  mutate(incidencia_categ = cut(incidencia, 
                                breaks = c(0, 0.00001, 20, 40, 60, 80, Inf), 
                                labels = c("Sem informação","< 20", "20-40", 
                                           "40-60", "60-80", "> 80")))
#Maternal age

query6= paste0("
SELECT 
  EXTRACT(YEAR FROM dtnasc) AS ano_nasc,
  mun_res_uf,
  idanomal,
  CASE
  WHEN idademae < 18 THEN 'menor de 18'
	WHEN idademae >= 18 AND idademae < 35 THEN 'de 18 a 34'
	WHEN idademae >= 35 THEN 'maior ou igual a 35'
	ELSE 'não informado'
  END AS faixa_etaria_mae,
COUNT(*) AS n
FROM 
  sinasc
GROUP BY 
  ano_nasc, mun_res_uf, faixa_etaria_mae, idanomal
")

df6 = dbGetQuery(con, query6)

df6 = df6 %>%
  pivot_wider(names_from = idanomal, values_from = n) %>%
  rowwise() %>% 
  mutate(incidencia = (sim / (sim + não)) * 1e5) %>%
  mutate(incidencia_categ = cut(incidencia, 
                              breaks = c(0, 0.00001, 1000, 5000, 10000, 20000, 
                                         30000, 40000, Inf), 
                              labels = c("Sem Informação","0-1000", "1000-5000", 
                                         "5000-10000", "10000-20000", 
                                         "20000-30000", "30000-40000", 
                                         ">40000"))) 


#Education
query8= paste0("
SELECT 
  EXTRACT(YEAR FROM dtnasc) AS ano_nasc,
  mun_res_uf,
  idanomal,
  escmae,
COUNT(*) AS n
FROM 
  sinasc
GROUP BY 
  ano_nasc, mun_res_uf, escmae, idanomal
")

df8 = dbGetQuery(con, query8)
df8 = df8 %>% 
  pivot_wider(names_from = idanomal, values_from = n) %>%
  rowwise() %>% 
  mutate(incidencia = (sim / (sim + não)) * 1e5) %>%
  mutate(incidencia_categ = cut(incidencia, 
                              breaks = c(0, 0.00001, 1000, 5000, 10000, 20000, 
                                         30000, 40000, Inf), 
                              labels = c("Sem informação","0-1000", 
                                         "1000-5000", "5000-10000", 
                                         "10000-20000", "20000-30000",
                                         "30000-40000", ">40000"))) 

#Abortion
query9= paste0("
SELECT 
  EXTRACT(YEAR FROM dtnasc) AS ano_nasc,
  mun_res_uf,idanomal,
  qtdfilmort,
COUNT(*) AS n
FROM 
  sinasc
GROUP BY 
  ano_nasc, mun_res_uf,idanomal, qtdfilmort
")

df9 = dbGetQuery(con, query9)

df9 = df9 %>% 
  mutate(qtd_filmort = case_when(
    is.na(qtdfilmort) ~ "Ignorado",
    qtdfilmort == 0 ~ "Nenhum",
    qtdfilmort == 1 ~ "1",
    qtdfilmort >= 2 ~ "≥2" 
  )) %>% 
  group_by(ano_nasc, qtd_filmort, idanomal) %>% 
  pivot_wider(names_from = idanomal, values_from = n) %>%
  rowwise() %>% 
  mutate(incidencia = (sim / (sim + não)) * 1e5) %>%
  mutate(incidencia_categ = cut(incidencia, 
                              breaks = c(0, 0.00001, 1000, 5000, 10000, 20000, 
                                         30000, 40000, Inf), 
                              labels = c("Sem informação","0-1000", 
                                         "1000-5000", "5000-10000", 
                                         "10000-20000", "20000-30000",
                                         "30000-40000", ">40000")))   
#Prenatal consultations
query10 = paste0("
SELECT 
  EXTRACT(YEAR FROM dtnasc) AS ano_nasc,
  mun_res_uf,
  idanomal,consultas,
COUNT(*) AS n
FROM 
  sinasc
GROUP BY 
  ano_nasc, mun_res_uf, idanomal,consultas
")
  
df10 = dbGetQuery(con, query10)
  
df10 =df10 %>% 
    pivot_wider(names_from = idanomal,
                values_from = n) %>%
    rowwise() %>% 
    mutate(incidencia = (sim/(sim+não))*1e5) %>% 
    mutate(incidencia_categ = cut(incidencia, 
                                  breaks = c(0, 0.00001, 1000, 5000, 10000,
                                             20000, 
                                             30000, 40000, Inf), 
                                  labels = c("Sem informação","0-1000", 
                                             "1000-5000", "5000-10000", 
                                             "10000-20000", "20000-30000",
                                             "30000-40000", ">40000")))   
# Pregnancy weeks
query11= paste0("
SELECT 
  EXTRACT(YEAR FROM dtnasc) AS ano_nasc,
  mun_res_uf,
  idanomal,
  CASE
  WHEN gestacao IN ('Menos de 22 semanas', '22 a 27 semanas', '28 a 31 semanas') THEN 'menor de 18'
	WHEN gestacao IN ('37 a 41 semanas') THEN '37 a 41 semanas'
	WHEN gestacao IN ('42 semanas ou mais') THEN '42 semanas ou mais'
	ELSE 'não informado'
  END AS gestacao_agrupado,
COUNT(*) AS n
FROM 
  sinasc
GROUP BY 
  ano_nasc, mun_res_uf,gestacao_agrupado, idanomal
")
  
df11 = dbGetQuery(con, query11)
  
df11 = df11 %>% 
    pivot_wider(names_from = idanomal, values_from = n) %>% 
    rowwise() %>% 
    mutate(incidencia = (sim / (sim + não)) * 1e5) %>% 
    filter(gestacao_agrupado != "não informado") %>%
    mutate(incidencia_categ = cut(incidencia, 
                                  breaks = c(0, 0.00001, 1000, 5000, 10000,
                                             20000, 
                                             30000, 40000, Inf), 
                                  labels = c("Sem informação","0-1000", 
                                             "1000-5000", "5000-10000", 
                                             "10000-20000", "20000-30000",
                                             "30000-40000", ">40000")))   
  
#Single or multiple pregnancy
query12 = paste0("
SELECT 
  EXTRACT(YEAR FROM dtnasc) AS ano_nasc,
  mun_res_uf,
  gravidez,
  idanomal,
COUNT(*) AS n
FROM 
  sinasc
GROUP BY 
  ano_nasc, mun_res_uf, gravidez, idanomal
")
  
df12 = dbGetQuery(con, query12)
  
df12 = df12 %>% 
    pivot_wider(names_from = idanomal, values_from = n) %>% 
    mutate(incidencia = (sim / (sim + não)) * 1e5) %>% 
    filter(!is.na(gravidez)) %>% 
    mutate(incidencia_categ = cut(incidencia, 
                                  breaks = c(0, 0.00001, 1000, 5000, 10000,
                                             20000, 
                                             30000, 40000, Inf), 
                                  labels = c("Sem informação","0-1000", 
                                             "1000-5000", "5000-10000", 
                                             "10000-20000", "20000-30000",
                                             "30000-40000", ">40000")))   
#Type of delivery
query13= paste0("
SELECT 
  EXTRACT(YEAR FROM dtnasc) AS ano_nasc,
  mun_res_uf,
  parto,
  idanomal,
COUNT(*) AS n
FROM 
  sinasc
GROUP BY 
  ano_nasc, mun_res_uf, parto, idanomal
")
  
df13 = dbGetQuery(con, query13)
  
df13 = df13 %>% 
    pivot_wider(names_from = idanomal, values_from = n) %>%
    rowwise() %>% 
    mutate(incidencia = (sim / (sim + não)) * 1e5) %>%
    filter(!is.na(parto)) %>%
    mutate(incidencia_categ = cut(incidencia, 
                                  breaks = c(0, 0.00001, 1000, 5000, 10000,
                                             20000, 
                                             30000, 40000, Inf), 
                                  labels = c("Sem informação","0-1000", 
                                             "1000-5000", "5000-10000", 
                                             "10000-20000", "20000-30000",
                                             "30000-40000", ">40000")))  
# Baby gender
query14= paste0("
SELECT 
  EXTRACT(YEAR FROM dtnasc) AS ano_nasc,
  mun_res_uf,
  idanomal,
  CASE
  WHEN sexo IN ('Feminino') THEN 'Feminino'
	WHEN sexo IN ('Masculino')  THEN 'Masculino'
	ELSE 'não informado'
  END AS sexo_bb,
COUNT(*) AS n
FROM 
  sinasc
GROUP BY 
  ano_nasc, mun_res_uf,sexo_bb, idanomal
")
  
df14 = dbGetQuery(con, query14)
  
df14 = df14 %>% 
    pivot_wider(names_from = idanomal, values_from = n) %>% 
    rowwise() %>% 
    mutate(incidencia = (sim / (sim + não)) * 1e5) %>% 
    filter(sexo_bb != 'não informado') %>% 
    mutate(incidencia_categ = cut(incidencia, 
                                  breaks = c(0, 0.00001, 1000, 5000, 10000, 
                                             20000, 
                                             30000, 40000, Inf), 
                                  labels = c("Sem informação","0-1000", 
                                             "1000-5000", "5000-10000", 
                                             "10000-20000", "20000-30000",
                                             "30000-40000", ">40000")))
# 5-minute Apgar score
query15= paste0("
SELECT 
  EXTRACT(YEAR FROM dtnasc) AS ano_nasc,
  mun_res_uf,
  idanomal,
  CASE
  WHEN apgar5 < 7 THEN 'menor que 7'
	WHEN apgar5 >= 7 THEN 'maior que 7'
	ELSE 'não informado'
  END AS apgar_5,
COUNT(*) AS n
FROM 
  sinasc
GROUP BY 
  ano_nasc, mun_res_uf,apgar_5, idanomal
")
  
df15= dbGetQuery(con, query15)
  
df15 = df15 %>% 
    pivot_wider(names_from = idanomal, values_from = n) %>% 
    rowwise() %>% 
    mutate(incidencia = (sim / (sim + não)) * 1e5) %>% 
    filter(apgar_5 != "não informado") %>% 
    mutate(incidencia_categ = cut(incidencia, 
                                  breaks = c(0, 0.00001, 1000, 5000, 10000, 
                                             20000, 
                                             30000, 40000, Inf), 
                                  labels = c("Sem informação","0-1000", 
                                             "1000-5000", "5000-10000", 
                                             "10000-20000", "20000-30000",
                                             "30000-40000", ">40000")))     
#Newborn's weight
query16 = paste0("
SELECT 
  EXTRACT(YEAR FROM dtnasc) AS ano_nasc,
  mun_res_uf,
  idanomal,
  CASE
  WHEN peso < 2500 THEN 'menor que 2500'
	WHEN peso >= 2500 THEN 'maior que 2500'
	ELSE 'não informado'
  END AS peso_rn,
COUNT(*) AS n
FROM 
  sinasc
GROUP BY 
  ano_nasc, mun_res_uf,peso_rn, idanomal
")
  
df16= dbGetQuery(con, query16)
  
df16 = df16 %>% 
    pivot_wider(names_from = idanomal, values_from = n) %>% 
    rowwise() %>% 
    mutate(incidencia = (sim/(sim+não))*1e5) %>%
    filter(peso_rn != "não informado") %>% 
    mutate(incidencia_categ = cut(incidencia, 
                                  breaks = c(0, 0.00001, 1000, 5000, 10000, 20000, 
                                             30000, 40000, Inf), 
                                  labels = c("Sem informação","0-1000", 
                                             "1000-5000", "5000-10000", 
                                             "10000-20000", "20000-30000",
                                             "30000-40000", ">40000")))     
  
#disconnect to the db
dbDisconnect(con)

