library(GetDFPData2)
library(dplyr)
library(remotes)
library(ggplot2)

#------------------Puxando dados do Bancos-----------------
#Busquei informações dentro do pacote
Empresas_CVM <- get_info_companies()

#Dexei só as empresas com registro ativo
Empresas_CVM <- Empresas_CVM %>%
  filter(SIT_REG == "ATIVO") 

#Filtrei só por bancos
BANCOS <- Empresas_CVM2 %>%
  filter(segment%in% ("Bancos")) %>%
  select(CNPJ, DENOM_COMERC, CD_CVM, SETOR_ATIV)

#------------------Puxando por códigos---------------------

library(GetDFPData2)
Resultados_Contábeis <- GetDFPData2:::get_itr_data(
  companies_cvm_codes = BANCOS$CD_CVM,
  first_year = 2000,
  last_year = lubridate::year(Sys.Date()),
  type_docs = c("BPA", "BPP", "DRE"),
  type_format = ("con"),
  clean_data = TRUE,
  use_memoise = FALSE,
  cache_folder = "gdfpd2_cache"
)
names(Resultados_Contábeis)

#------------------Indentificando LL e PL---------------------
#Filtrei o LL e o PL
Lucro_Liquido1 <- Resultados_Contábeis[["DF Consolidado - Demonstração do Resultado"]] %>%
  filter(CD_CONTA == "3.11")

#Perccebi que era necessario remover essa rubrica "Lucro ou Prejuízo antes das Participações e Contribuições Estatutárias"
Lucro_Liquido2 <- Resultados_Contábeis[["DF Consolidado - Demonstração do Resultado"]] %>%
  filter(CD_CONTA == "3.09")
Lucro_Liquido2 <- Lucro_Liquido2 %>%
   filter((!DS_CONTA %in% "Lucro ou Prejuízo antes das Participações e Contribuições Estatutárias"))

#Juntando os 3 dfs
Lucro_Liquido <- bind_rows(Lucro_Liquido1,Lucro_Liquido2,Removendo_rúbrica) %>%
distinct()

Lucro_Liquido <- Lucro_Liquido %>% 
  select(-VERSAO, -GRUPO_DFP, -MOEDA, -ESCALA_MOEDA, -COLUNA_DF, -source_file, -quarter)

#------------TESTANDO LL--------------------------------------
# 1. Criação da Coluna de Diferença de Dias 
Colocandodias <- Lucro_Liquido %>%
  mutate(
    DIAS = as.numeric(difftime(DT_FIM_EXERC, DT_INI_EXERC, units = "days")))

ROE_RESUMO <- ROE >
  group_by(Data) %>%
  summarise(qtd_linhas = n())


# Filtramos para manter apenas as linhas entre 88 e 93 dias.
Lucro_Liquido <- Colocandodias %>%
  filter(DIAS >= 88 & DIAS <= 93)

#PARA PL
Patrimonio_Liquido <- Resultados_Contábeis[["DF Consolidado - Balanço Patrimonial Passivo"]] %>%
  filter(grepl("Patrimônio Líquido Consolidado", DS_CONTA, ignore.case = TRUE))

Patrimonio_Liquido_CD <- Resultados_Contábeis[["DF Consolidado - Balanço Patrimonial Passivo"]] %>%
  filter(CD_CONTA == "2.08")


Patri_Liqu_Consil <- setdiff(ROE_resumo$Nome,Patrimonio_Liquido_CD$DENOM_CIA)
Patri_Liqu_Consil <- interaction(ROE_resumo$Nome,Patrimonio_Liquido$DENOM_CIA)


Patrimonio_Liquido <- Patrimonio_Liquido %>% 
  select(-VERSAO, -GRUPO_DFP, -MOEDA, -ESCALA_MOEDA, -COLUNA_DF, -source_file, -quarter)

#------------------ROE----------------------------------------
LL_PL <- Lucro_Liquido %>%
  inner_join(
    Patrimonio_Liquido,
    by = c("DT_REFER", "DENOM_CIA"),
    suffix = c(".LL", ".PL")         
  )

ROE <- LL_PL %>%
  mutate(ROE = (VL_CONTA.LL / VL_CONTA.PL) * 100) %>% 
  select(DT_REFER, DENOM_CIA, ROE) %>%
  distinct()

colnames(ROE) <- c("Data", "Nome", "ROE")

library(writexl)
write_xlsx(ROE, "ROE.xlsx")


library(dplyr)
ROE_RESUMO <- ROE %>%
  group_by(Nome) %>%
  summarise(
    Qtd.Trimestres = n_distinct(Data))

df_resumo

#---------------ROA-------------------------------------------

LL_ROA <- Lucro_Liquido %>%
  inner_join(
    Ativo_total,
    by = c("DT_REFER", "DENOM_CIA"),
    suffix = c(".LL", ".AT")         
  )
ROA <- LL_PL %>%
  mutate(ROA = (VL_CONTA.LL / VL_CONTA.AT) * 100) %>%  
  select(DT_REFER, DENOM_CIA, ROA ) %>%
  distinct()

#------------------Identification AT---------------------
Ativo_total <- Resultados_Contábeis[["DF Consolidado - Balanço Patrimonial Ativo"]] %>%
  filter(grepl("Ativo Total", DS_CONTA, ignore.case = TRUE))

Ativo_total <- Ativo_total %>%
  select(DT_REFER,DENOM_CIA,VL_CONTA)

#---------------------Painel----------------
# Usando data.frame
Dados_Painel <- data.frame(
  Data = ROE$DT_REFER,
  Cia = ROA$DENOM_CIA,
  ROA = ROA$ROA,
  ROE = ROE$ROE
)

#---------------GRÁFICO-------------------
ggplot(ROE, aes(x = as.Date(Data), y = ROE)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") +
  facet_wrap(~ Nome, scales = "free_y") +
  labs(title = "Oscilações do ROE por banco",
       x = "Data do fim do exercício",
       y = "ROE (%)") +
  theme_minimal()

