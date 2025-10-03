library(dplyr)

DADOS <- bind_rows(Resultados_Contábeis, .id = "origem")

#Buscando appenas o PL
APENAS_PL <- DADOS %>%
  filter(
    DS_CONTA == "Patrimônio Líquido Consolidado" |
      CD_CONTA %in% c("2.07", "2.08")
  ) %>%
  distinct(DENOM_CIA, DS_CONTA, CD_CONTA) %>%
  group_by(DENOM_CIA) %>%
  summarise(qtd = n())

#Filtrei só por um bancos
BCO_MERCANTILDE_INVESTIMENTOSS.A. <- DADOS %>%
  filter(DENOM_CIA%in% ("BCO MERCANTIL DE INVESTIMENTOS S.A.")) %>%
  select(origem, CNPJ_CIA, DENOM_CIA, DT_REFER,CD_CONTA, DS_CONTA, VL_CONTA)

#Filtrei só por bancos
BANCO_DO_BRASIL <- BancoDFP %>%
  select(origem, CNPJ_CIA, DENOM_CIA, DT_REFER,CD_CONTA, DS_CONTA, VL_CONTA)

# information about companies
df_info <- get_info_companies(tempdir())
print(df_info )
search_company('grendene', cache_folder = tempdir())

#Para DFP
l_dfp <- get_dfp_data(companies_cvm_codes = 1023, 
                      use_memoise = FALSE,
                      clean_data = TRUE,
                      cache_folder = tempdir(), # use local folder in live code
                      type_docs = c('DRE', 'BPA'), 
                      type_format = 'con',
                      first_year = 2000, 
                      last_year = 2020)
str(l_dfp)

#Para ITR
l_itr <- GetDFPData2:::get_itr_data(
  companies_cvm_codes = 1023,   # Banco do Brasil
  use_memoise = FALSE,
  clean_data = TRUE,
  cache_folder = tempdir(),     # pasta de cache
  type_docs = c('DRE', 'BPA'),  # demonstrações desejadas
  type_format = 'con',          # consolidado
  first_year = 2000, 
  last_year = 2020
)