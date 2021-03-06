# Missing data in brazilian national evaluation of public schools
# author: Victor G. Alcantara
# date: 12.11.2021

# I. Packages and setup -------------------------------------------------------

library(tidyverse) # data management
library(vcd)       # for correlations
library(janitor)   # for tables
library(geobr)     # br geo info
require(sf)        

wd <- "C:/Users/VictorGabriel/Documents/DADOS/EDUCACAO/" # your working directory
setwd(wd)

# II. Import databases ---------------------------------------------------------

# Lista com as bases de dados a serem preenchidas na imputa��o por loop
saeb <- list(s13 = data.frame(), s15 = data.frame(), s17 = data.frame(),
             s19 = data.frame())

censo <- list(c13 = data.frame(), c15 = data.frame(), c17 = data.frame(),
              c19 = data.frame())

# Anos em que Saeb passou a ser censit�rio para o EF
ano = seq(from=2013,to=2019,by=2)

# SAEB
for(i in 1:length(ano)) {
  print(i)
  saeb[[i]] <- read_csv(paste0(wd,"SAEB/",ano[i],"/DADOS/TS_ESCOLA.csv")) # abra todas as bases
  gc()
}

# CENSO ESCOLAR
for(i in 1:length(ano)) {
  print(i)
  censo[[i]] <- read.delim(paste0(wd,"CENSO/",ano[i],"/DADOS/ESCOLAS.csv"), sep = "|") # abra todas as bases
  gc()
}

brasil <- read_state(year = 2019)

# III. Data management (select, mutate, rename) --------------------------------

# Nova base de dados com vari�veis selecionadas e padronizadas
my_saeb <- list(s13 = data.frame(), s15 = data.frame(), s17 = data.frame(),
                s19 = data.frame())

my_censo <- list(c13 = data.frame(), c15 = data.frame(), c17 = data.frame(),
                 c19 = data.frame())

# SAEB --------- 
for(i in 1:length(ano)) { # SAEB 2013-2015: censit�rio apenas para EF
  my_saeb[[i]] <- saeb[[i]] %>%
    select(
      # Identifica��o da Escola
      ID_ESCOLA,
      ID_DEPENDENCIA_ADM,
      
      # Localizacao da escola 
      ID_UF,
      ID_MUNICIPIO,
      ID_LOCALIZACAO,
      
      # Indicadores educacionais
      # Docentes
      # PC_FORMACAO_DOCENTE_INICIAL,
      # PC_FORMACAO_DOCENTE_FINAL,
      # PC_FORMACAO_DOCENTE_MEDIO,
      
      # Alunos 
      NIVEL_SOCIO_ECONOMICO,
      NU_MATRICULADOS_CENSO_5EF, NU_PRESENTES_5EF,
      NU_MATRICULADOS_CENSO_9EF, NU_PRESENTES_9EF,
      MEDIA_5EF_LP, MEDIA_5EF_MT,
      MEDIA_9EF_LP, MEDIA_9EF_MT,
    )
}

for(i in 3) { # SAEB 2017: inclusao do EM
  my_saeb[[i]] <- saeb[[i]] %>%
    select(
      # Identifica��o da Escola
      ID_ESCOLA,
      ID_DEPENDENCIA_ADM,
      
      # Localizacao da escola 
      ID_UF,
      ID_MUNICIPIO,
      ID_LOCALIZACAO,
      
      # Indicadores educacionais
      # Docentes
      # PC_FORMACAO_DOCENTE_INICIAL,
      # PC_FORMACAO_DOCENTE_FINAL,
      # PC_FORMACAO_DOCENTE_MEDIO,
      # Alunos 
      NIVEL_SOCIO_ECONOMICO,
      NU_MATRICULADOS_CENSO_5EF, NU_PRESENTES_5EF,
      NU_MATRICULADOS_CENSO_9EF, NU_PRESENTES_9EF,
      NU_MATRICULADOS_CENSO_3EM, NU_PRESENTES_3EM,
      MEDIA_5EF_LP, MEDIA_5EF_MT,
      MEDIA_9EF_LP, MEDIA_9EF_MT,
      MEDIA_3EM_LP, MEDIA_3EM_MT     
    )
}

for(i in 4) { # SAEB 2019: com distin��o entre EM Tradicional e Integrado ao tecnico
  my_saeb[[i]] <- saeb[[i]] %>%
    select(
      # Identifica��o da Escola
      ID_ESCOLA,
      ID_DEPENDENCIA_ADM,
      
      # Localizacao da escola 
      ID_UF,
      ID_MUNICIPIO,
      ID_LOCALIZACAO,
      
      # Indicadores educacionais
      # Docentes
      # PC_FORMACAO_DOCENTE_INICIAL,
      # PC_FORMACAO_DOCENTE_FINAL,
      # PC_FORMACAO_DOCENTE_MEDIO,
      # Alunos 
      NIVEL_SOCIO_ECONOMICO,
      NU_MATRICULADOS_CENSO_5EF, NU_PRESENTES_5EF,
      NU_MATRICULADOS_CENSO_9EF, NU_PRESENTES_9EF,
      NU_MATRICULADOS_CENSO_EMT, NU_PRESENTES_EMT,
      NU_MATRICULADOS_CENSO_EMI, NU_PRESENTES_EMI,
      NU_MATRICULADOS_CENSO_EM,  NU_PRESENTES_EM,
      MEDIA_5EF_LP, MEDIA_5EF_MT,
      MEDIA_9EF_LP, MEDIA_9EF_MT,
      MEDIA_EMT_LP, MEDIA_EMT_MT, # EMT: Ensino Medio Tradicional
      MEDIA_EMI_LP, MEDIA_EMI_MT, # EMI: Ensino Medio Integrado
      MEDIA_EM_LP,  MEDIA_EM_MT   # EM : Ensino Medio Integrado ou Tradicional  
    )
}


# CENSO ---------
for(i in 1) {
  my_censo[[i]] <- censo[[i]] %>% 
    select(
      # Identificacao adm
      PK_COD_ENTIDADE, NO_ENTIDADE,ID_DEPENDENCIA_ADM,
      
      # Localizacao da escola
      # GEO_REF_LATITUDE, GEO_REF_LONGITUDE,
      DESC_SITUACAO_FUNCIONAMENTO,
      FK_COD_ESTADO, FK_COD_MUNICIPIO,
      ID_LOCALIZACAO,
      
      # Estrutura escolar
      # IN_BIBLIOTECA_SALA_LEITURA,
      # IN_LABORATORIO_CIENCIAS,
      # IN_LABORATORIO_INFORMATICA,
      # IN_QUADRA_ESPORTES,
      # IN_SALA_ATELIE_ARTES,
      
      # Oferta de matr�cula
      ID_MOD_ENS_REGULAR,
      ID_REG_FUND_8_ANOS,
      ID_REG_FUND_9_ANOS,
      ID_FUND_CICLOS,
      ID_REG_MEDIO_MEDIO,
      ID_REG_MEDIO_INTEGRADO,
    ) %>%        
    rename(
      # Mantendo padr�o SAEB
      ID_UF = FK_COD_ESTADO,
      ID_ESCOLA = PK_COD_ENTIDADE,
      ID_MUNICIPIO = FK_COD_MUNICIPIO,
      # Mantendo padr�o censo 2019
      TP_SITUACAO_FUNCIONAMENTO = DESC_SITUACAO_FUNCIONAMENTO,
      IN_REGULAR = ID_MOD_ENS_REGULAR,
      IN_COMUM_FUND_AI = ID_REG_FUND_8_ANOS,
      IN_COMUM_FUND_AF = ID_REG_FUND_9_ANOS,
      IN_COMUM_MEDIO_MEDIO = ID_REG_MEDIO_MEDIO,
      IN_COMUM_MEDIO_INTEGRADO = ID_REG_MEDIO_INTEGRADO
    ) %>% 
    filter(
      # Condi��es para o SAEB
      TP_SITUACAO_FUNCIONAMENTO == 1,               # Escolas em funcionamento
      IN_COMUM_FUND_AI == 1 |IN_COMUM_FUND_AF == 1| # Oferece EF
      ID_FUND_CICLOS == 1
        )
}

for(i in 2) {
  my_censo[[i]] <- censo[[i]] %>% 
    select(
      # Identificacao adm
      CO_ENTIDADE, NO_ENTIDADE,TP_DEPENDENCIA,
      
      # Localizacao da escola
      # GEO_REF_LATITUDE, GEO_REF_LONGITUDE,
      TP_SITUACAO_FUNCIONAMENTO,
      CO_REGIAO, CO_MICRORREGIAO,
      CO_UF, CO_MUNICIPIO,
      TP_LOCALIZACAO,
      
      # Estrutura escolar
      # IN_BIBLIOTECA_SALA_LEITURA,
      # IN_LABORATORIO_CIENCIAS,
      # IN_LABORATORIO_INFORMATICA,
      # IN_QUADRA_ESPORTES,
      # IN_SALA_ATELIE_ARTES,
      
      # Oferta de matr�cula
      IN_REGULAR,
      IN_COMUM_FUND_AI,
      IN_COMUM_FUND_AF,
      ID_FUND_CICLOS == 1,
      IN_COMUM_MEDIO_MEDIO,
      IN_COMUM_MEDIO_INTEGRADO,
    ) %>%        
    rename(
      # Mantendo padr�o SAEB
      ID_UF = CO_UF,
      ID_ESCOLA = CO_ENTIDADE,
      ID_MUNICIPIO = CO_MUNICIPIO,
      ID_LOCALIZACAO = TP_LOCALIZACAO,
      ID_DEPENDENCIA_ADM = TP_DEPENDENCIA
    ) %>% 
    filter(
      TP_SITUACAO_FUNCIONAMENTO == 1,               # Escolas em funcionamento
      IN_COMUM_FUND_AI == 1 |IN_COMUM_FUND_AF == 1  # Oferece EF
        )
}

for(i in 3:4) {
  my_censo[[i]] <- censo[[i]] %>% 
    select(
      # Identificacao adm
      CO_ENTIDADE, NO_ENTIDADE,TP_DEPENDENCIA,
      
      # Localizacao da escola
      # GEO_REF_LATITUDE, GEO_REF_LONGITUDE,
      TP_SITUACAO_FUNCIONAMENTO,
      CO_REGIAO, CO_MICRORREGIAO,
      CO_UF, CO_MUNICIPIO,
      TP_LOCALIZACAO,
      
      # Estrutura escolar
      # IN_BIBLIOTECA_SALA_LEITURA,
      # IN_LABORATORIO_CIENCIAS,
      # IN_LABORATORIO_INFORMATICA,
      # IN_QUADRA_ESPORTES,
      # IN_SALA_ATELIE_ARTES,
      
      # Oferta de matr�cula
      IN_REGULAR,
      IN_COMUM_FUND_AI,
      IN_COMUM_FUND_AF,
      IN_COMUM_MEDIO_MEDIO,
      IN_COMUM_MEDIO_INTEGRADO,
    ) %>%        
    rename(
      # Mantendo padr�o SAEB
      ID_UF = CO_UF,
      ID_ESCOLA = CO_ENTIDADE,
      ID_MUNICIPIO = CO_MUNICIPIO,
      ID_LOCALIZACAO = TP_LOCALIZACAO,
      ID_DEPENDENCIA_ADM = TP_DEPENDENCIA
    ) %>% 
    filter( # Condicoes do SAEB
      TP_SITUACAO_FUNCIONAMENTO == 1,
      IN_COMUM_FUND_AI == 1 |
      IN_COMUM_FUND_AF == 1 |
      IN_COMUM_MEDIO_MEDIO == 1 |
      IN_COMUM_MEDIO_INTEGRADO == 1,
    )
}

# 3.1 Recodificando vari�veis --------

# SAEB
for(i in 1:length(ano)) {
  my_saeb[[i]] <- my_saeb[[i]] %>% 
    mutate(
      DepAdmin = 
        factor(.$ID_DEPENDENCIA_ADM,
               levels = c(1,2,3,4),
               labels = c("Federal", "Estadual", "Municipal", "Privada" ),
               ordered = T),
      
      INSE = NIVEL_SOCIO_ECONOMICO)
}

# CENSO
for(i in 1:length(ano)) {
  my_censo[[i]] <- my_censo[[i]] %>%
    mutate(
      DepAdmin = 
        factor(.$ID_DEPENDENCIA_ADM,
               levels = c(1,2,3,4),
               labels = c("Federal", "Estadual", "Municipal", "Privada"),
               ordered = T) )
}

# 3.2 INSE: recodificando as categorias para Baixo, Medio e Alto -----

for(i in 1:length(ano)) {
  my_saeb[[i]]$INSE <- iconv(my_saeb[[i]]$INSE, to = 'Latin1') }

# 2013
my_saeb[[1]] <- my_saeb[[1]] %>%
  mutate(INSE = case_when(INSE == "Grupo 1" ~ 1,
                          INSE == "Grupo 2" ~ 1,
                          INSE == "Grupo 3" ~ 2,
                          INSE == "Grupo 4" ~ 2,
                          INSE == "Grupo 5" ~ 2,
                          INSE == "Grupo 6" ~ 3,
                          INSE == "Grupo 7" ~ 3
  ) )

# 2015

for(i in c(2)) {
  my_saeb[[i]] <- my_saeb[[i]] %>%
    mutate(INSE = case_when(INSE == "Muito Baixo" ~ 1,
                            INSE == "Baixo"       ~ 1,
                            INSE == "M�dio Baixo" ~ 2,
                            INSE == "M�dio"       ~ 2,
                            INSE == "M�dio Alto"  ~ 2,
                            INSE == "Alto"        ~ 3,
                            INSE == "Muito Alto"  ~ 3
    ) )
}

# 2017 :  Por algum motivo n�o est� padronizado em 7 categorias
#         como nos outros anos. Estou for�ando a barra
#         recodificando as categorias para baixo, m�dio e alto.
#         � necess�rio conferir a constru��o dessas categorias.
#         Talvez reconstru�-las seguindo a metodologia.

my_saeb[[3]] <- my_saeb[[3]] %>%
  mutate(INSE = case_when(INSE == "Grupo 1" ~ 1,
                          INSE == "Grupo 2" ~ 1,
                          INSE == "Grupo 3" ~ 2,
                          INSE == "Grupo 4" ~ 2,
                          INSE == "Grupo 5" ~ 3,
                          INSE == "Grupo 6" ~ 3
  ) )
# 2019
my_saeb[[4]] <- my_saeb[[4]] %>%  
  mutate(INSE = case_when(INSE == "N�vel I"   ~ 1,
                          INSE == "N�vel II"  ~ 1,
                          INSE == "N�vel III" ~ 2,
                          INSE == "N�vel IV"  ~ 2,
                          INSE == "N�vel V"   ~ 2,
                          INSE == "N�vel VI"  ~ 3, 
                          INSE == "N�vel VII" ~ 3
  ) )

# Ordenando as categorias
for(i in 1:4) {
  my_saeb[[i]]$INSE <- factor(my_saeb[[i]]$INSE,
                              labels = c("Baixo", "Medio", "Alto"),
                              levels = c(1,2,3),
                              ordered = TRUE)
}

# 3.3 Criando indicador de desempenho m�dio geral por Escola -----------

# Padroniza��o das escalas para 0-10

for(i in 1:length(ano)) {
  x_i <- my_saeb[[i]]$MEDIA_9EF_LP
  x_1 <- min(my_saeb[[i]]$MEDIA_9EF_LP, na.rm = T) 
  x_n <- max(my_saeb[[i]]$MEDIA_9EF_LP, na.rm = T)
  
  my_saeb[[i]]$nota_padronizada_LP <- ((x_i - x_1) / (x_n - x_1) )* 10
  
  x_i <- my_saeb[[i]]$MEDIA_9EF_MT
  x_1 <- min(my_saeb[[i]]$MEDIA_9EF_MT, na.rm = T) 
  x_n <- max(my_saeb[[i]]$MEDIA_9EF_MT, na.rm = T)
  
  my_saeb[[i]]$nota_padronizada_MT <- ((x_i - x_1) / (x_n - x_1) )* 10
  
  rm(x_i,x_1,x_n)
  gc()
}

# M�dia simples das escalas padronizadas = Leitura + Matem�tica / 2

for(i in 1:length(ano)) {
  x_1 <- my_saeb[[i]]$nota_padronizada_LP
  x_2 <- my_saeb[[i]]$nota_padronizada_MT
  
  my_saeb[[i]]$desempenho_medio      <- (x_1 + x_2) /2
  
  my_saeb[[i]]$desempenho_categorias <- cut(my_saeb[[i]]$desempenho_medio,
                                            breaks = c(0, 2, 4, 6, 8, 10),
                                            labels = c("Menor que 2 | Muito Baixa",
                                                       "2 - 4 | Baixa",
                                                       "4 - 6 | M�dia",
                                                       "6 - 8 | Alta",
                                                       "8 - 10| Muito alta"),
                                            ordered_result = TRUE, # Try without this #
                                            right = FALSE)
  rm(x_1,x_2)
  gc()
}

# IV. Mensurando a participa��o das escolas no SAEB com refer�ncia ao Censo Escolar
# Por Estado (UF), Depend�ncia Administrativa (DA) e Localica��o

# SAEB | UF & DA --------------------------------------------------

# Criando uma lista com 4 bases de dados (uma para cada ano do SAEB) nas quais iremos colocar a frequ�ncia de escolas por depend�ncia administrativa para cada Estado (UF).

fq      <- data.frame(Federal  = rep(NA,27),
                      Estadual = rep(NA,27),
                      Municipal= rep(NA,27),
                      Privada  = rep(NA,27),
                      
                      Urbana   = rep(NA,27),
                      Rural    = rep(NA,27))

fq.saeb <- list(s13 = fq, s15 = fq, s17 = fq, s19 = fq)

# Guardando c�digo e siglas dos Estados (UF)
UF    = unique(brasil$abbrev_state) 
CO_UF = unique(saeb[[1]]$ID_UF)      

# Agora vamos rodar um loop que filtra cada UF e computa a frequ�ncia de escolas por DepAdmin, em todos os anos.
# Note que precisamos de duas condi��es para o loop: 1. para os anos do saeb e 2. para os Estados.

for(i in 1:length(ano)) { for(j in 1:length(UF)) {
  data <- my_saeb[[i]] %>% filter(ID_UF == CO_UF[j]) # Sele��o UF
  
  print(i)
  fq.saeb[[i]][j,"Federal"]     <- table(data$ID_DEPENDENCIA_ADM)["1"]
  fq.saeb[[i]][j,"Estadual"]    <- table(data$ID_DEPENDENCIA_ADM)["2"]
  fq.saeb[[i]][j,"Municipal"]   <- table(data$ID_DEPENDENCIA_ADM)["3"]
  fq.saeb[[i]][j,"Privada"]     <- table(data$ID_DEPENDENCIA_ADM)["4"]
  fq.saeb[[i]][j,"Urbana"]      <- table(data$ID_LOCALIZACAO)["1"]
  fq.saeb[[i]][j,"Rural"]       <- table(data$ID_LOCALIZACAO)["2"]
  rm(data)
  gc()
} }

# Agora computamos os c�digos e siglas dos Estados em cada base de frequ�ncia

for(i in 1:length(ano)) {
  fq.saeb[[i]]$UF    <- UF
  fq.saeb[[i]]$CO_UF <- CO_UF }

# CENSO ESCOLAR | UF & DA ----------------------------------------------------

# Agora fazemos o mesmo com o censo

fq.censo <- list(c12 = fq, c14 = fq, c16 = fq, c18 = fq)

for(i in 1:length(ano)) { for(j in 1:length(UF)) {
  data <- my_censo[[i]] %>% filter(ID_UF == CO_UF[j])
  
  print(i)
  fq.censo[[i]][j,"Federal"]    <- table(data$ID_DEPENDENCIA_ADM)["1"]
  fq.censo[[i]][j,"Estadual"]   <- table(data$ID_DEPENDENCIA_ADM)["2"]
  fq.censo[[i]][j,"Municipal"]  <- table(data$ID_DEPENDENCIA_ADM)["3"]
  fq.censo[[i]][j,"Privada"]    <- table(data$ID_DEPENDENCIA_ADM)["4"]
  fq.censo[[i]][j,"Urbana"]     <- table(data$ID_LOCALIZACAO)[1]
  fq.censo[[i]][j,"Rural"]      <- table(data$ID_LOCALIZACAO)[2]
  
  rm(data)
  gc()
} }

for(i in 1:length(ano)) {
  fq.censo[[i]]$UF <- UF
  fq.censo[[i]]$CO_UF <- CO_UF }

# 1.  Criamos duas tabelas com a frequ�ncia de escolas por Unidades da
# Federa��o (UF daqui em diante) e Depend�ncia Administrativa (DA
# daqui em diante), uma referente ao Saeb e outra ao Censo.

## Tabela de frequ�ncia de escolas p�blicas participantes do SAEB
fq.saeb[[1]][,]

## Tabela de frequ�ncia de escolas p�blicas cadastradas no censo
fq.censo[[1]][,]

# 2.  Com as duas tabelas, fazemos uma raz�o entre Saeb e Censo para saber
# quanto de escolas cadastradas no censo participaram da avalia��o,
# condicionadas por UF e DA. Temos, assim, uma 'taxa de participa��o
#     das escolas' na avalia��o nacional em que podemos saber quantas
# escolas em cada UF e por DA participaram da avalia��o.
# 
# Tomamos o censo como refer�ncia das escolas cadastradas no sistema pois
# � uma informa��o de preenchimento obrigat�rio e anual pelas escolas,
# portanto informa sobre as escolas cadastradas no INEP. O censo � tamb�m
# a base de refer�ncia para o planejamento da avalia��o nacional no ano de refer�ncia.

## Taxa de participa��o das escolas no SAEB
# Taxa de participa��o de escolas por UF e Dep. Adm.

tx <- data.frame(Federal = NA, Estadual = NA, Municipal = NA, Privada = NA,
                 Urbana = NA, Rural = NA)

tx_participacao <- list(tx13 = tx, tx15 = tx, tx17 = tx, tx19 = tx)

for(i in 1:length(ano)) {
  tx_participacao[[i]] <- round(fq.saeb[[i]][1:6]/fq.censo[[i]][1:6],2) # Saeb / Censo
  tx_participacao[[i]]$UF    <- UF       # Indicando as UF's
  tx_participacao[[i]]$CO_UF <- CO_UF
}

tx_participacao[[1]]

# Com essa tabela sint�tica temos a taxa de participa��o das escolas por Unidades da Federa��o, depend�ncia administrativa e localiza��o (urbana ou rural). Vemos que no SAEB de 2019, o Estado do Rio de Janeiro, por exemplo, participaram 0.65 das federais, 0.79 das estaduais e 0.36 das municipais.  Para saber a participa��o das escolas nas UF's em rela��o ao Brasil, computamos a m�dia da taxa de participa��o das escolas por DA e em
# seguida fazemos a diferen�a de cada UF em rela��o � m�dia nacional. Assim, teremos a seguinte tabela com a dist�ncia de cada UF e rela��o � m�dia nacional.

### Diferen�a entre m�dia da taxa de participa��o nacional e m�dia dos Estados (UF)

medias <- list(mean13 = fq, mean15 = fq, mean17 = fq, mean19 = fq)

for(i in 1:4) {
medias[[i]]["Federal"]   <-  rep(mean(tx_participacao[[i]]$Federal  , na.rm = T),27)
medias[[i]]["Estadual"]  <-  rep(mean(tx_participacao[[i]]$Estadual , na.rm = T),27)
medias[[i]]["Municipal"] <-  rep(mean(tx_participacao[[i]]$Municipal, na.rm = T),27)
medias[[i]]["Privada"]   <-  rep(mean(tx_participacao[[i]]$Municipal, na.rm = T),27)
medias[[i]]["Urbana"]   <-  rep(mean(tx_participacao[[i]]$Urbana, na.rm = T),27)
medias[[i]]["Rural"]   <-  rep(mean(tx_participacao[[i]]$Rural, na.rm = T),27)
}

dif.media <- list(dif13 = tx, dif15 = tx, dif17 = tx, dif19 = tx)

for(i in 1:4) {
dif.media[[i]] <- round(medias[[i]][,c(1:6)] - tx_participacao[[i]][,c(1:6)],2)
dif.media[[i]]$UF    <- UF
dif.media[[i]]$CO_UF <- CO_UF}

dif.media[[4]]

# Na tabela acima temos a dist�ncia das taxas dos Estados em rela��o �
# m�dia nacional por Depend�ncia Administrativa.

# Com essa tabela podemos observar que, em 2019, o Estado do Rio de
# Janeiro est� a 0.06 acima da m�dia nacional para escolas Federais, 0.007
# abaixo da m�dia nacional para estaduais e 0.13 abaixo da m�dia para
# escolas municipais, que nacionalmente possuem uma participa��o baixa no
# Saeb (m�dia nacional 0.41).

# 2. *Missing data* entre escolas que participaram da avalia��o

# Entre as escolas que participaram da avalia��o e que est�o registradas
# no Saeb temos ainda casos com falta de informa��es (*missing data*)

# Tabela com missing values em profici�ncia
missing <- list(s13 = data.frame(NA),s15 = data.frame(NA),
                s17 = data.frame(NA),s19 = data.frame(NA))

for(i in 1:length(ano)) {
# missing
  missing[[i]]$missing_AF_EF <- sum(is.na(my_saeb[[i]]$desempenho_categorias))
# observados (total - missing)
missing[[i]]$total_AF_EF <- length(my_saeb[[i]]$desempenho_medio)

missing[[i]]$obs_AF_EF <- missing[[i]]$total_AF_EF - missing[[i]]$missing_AF_EF
}
# Para analisar onde est�o os *missing values* podemos verificar a
# frequ�ncia de escolas por DA e desempenho m�dio padronizado. Observamos que todas as escolas participantes informam a DA, mas nem todas possuem informa��o sobre a profici�ncia.

## Dados faltantes por Depend�ncia Administrativa e INSE

# Imputando vari�vel que indica casos 'observados' e 'ausentes'
for(i in 1:4) {
 my_saeb[[i]]$missing <- ifelse(is.na(my_saeb[[i]]$desempenho_categorias) == FALSE, "observado","ausente")
}

tabs.da <- list(tab.da1 = NULL, tab.da2 = NULL,
                tab.da3 = NULL, tab.da4 = NULL)

# Tabela cruzara missing x DepAdmin
for(i in 1:4) {
tabs.da[[i]] <- xtabs(~ missing + DepAdmin, data = my_saeb[[i]])
}

# Tabela cruzada missing x INSE
tabs.inse <- list(tab.inse1 = NULL, tab.inse2 = NULL,
                tab.inse3 = NULL, tab.inse4 = NULL)

# Tabela cruzara missing x DepAdmin
for(i in 1:4) {
tabs.inse[[i]] <- xtabs(~ missing + INSE, data = my_saeb[[i]])
}

## Tabela 2. Dados faltantes por Depend�ncia Administrativa

# Com a propor��o de dados faltantes, vemos que h� uma lacuna
# significativa em todas as DA's. Entre as federais, 94% n�o possuem
# informa��es sobre profici�ncia dos alunos, enquanto que estaduais e
# municipais t�m, respectivamente, a aus�ncia da informa��o em 48% e 63%
#   dos casos. No agregado nacional, faltam as informa��es de 57% das
# escolas participantes. Portanto, podemos afirmar que faltam informa��es
# de profici�ncia de parte expressiva do sistema educacional, o que
# dificulta generaliza��es e afeta tanto a precis�o quanto a confian�a de
# predi��es para o sistema como um todo. Portanto, as an�lises de
# profici�ncia com os dados do Saeb, fundamentais para o diagn�stico do
# aprendizado, devem ser cautelosas quanto �s afirma��es. Ainda que a
# avalia��o seja censit�ria e obrigat�ria, temos informa��es de uma
# parcela enviesada de alunos que comparecem � Escola e se comprometem a
# realizar a avalia��o nacional com afinco. Esse enviesamento n�o invalida
# as an�lises com os dados do SAEB, mas exigem o cuidado.
# 
# Outro indicador importante da avalia��o nacional, al�m da profici�ncia
# dos alunos, � o Indicador de N�vel Socioecon�mico (INSE, daqui em
#                                                    diante). A tabela a seguir analisa a .

## Tabela 2. Dados faltantes por INSE

# Na tabela de frequ�ncia de dados faltantes por INSE podemos observar a incid�ncia de informa��es faltantes sobre o INSE � menor. Com a raz�o entre as informa��es faltantes e as observadas podemos averiguar com mais detalhe as informa��es contidas no Saeb.

## Correla��o dado faltante por DA e INSE

# Correla��o entre missing por categorias de interesse
# Correla��es
# Missing x DA
cors.da <- list(cor.da1 = NULL, cor.da2 = NULL,
                cor.da3 = NULL, cor.da4 = NULL)

for(i in 1:4) {
  cors.da[[i]] <- assocstats(tabs.da[[i]][,-4])
}

# Missing x INSE
cors.inse <- list(cor.inse1 = NULL, cor.inse2 = NULL,
                  cor.inse3 = NULL, cor.inse4 = NULL)
for(i in 1:4) {
  cors.inse[[i]] <- assocstats(tabs.inse[[4]]) }

# Outros registros
missing_inse <- list(s13 = data.frame(NA),s15 = data.frame(NA),
                     s17 = data.frame(NA),s19 = data.frame(NA))

for(i in 1:length(ano)) {
  missing_inse[[i]] <- table("INSE"      = my_saeb[[i]]$INSE, 
                             "Depend�ncia administrativa" = my_saeb[[i]]$DepAdmin, 
                             useNA = "ifany") %>%  
    addmargins(.) }

missing_inse[[4]][4,-4]  / missing_inse[[4]][5,-4] 

# Vemos que apenas 6% das escolas federais n�o possuem informa��es sobre o INSE, enquanto que as estaduais e municipais apenas 1% e 3%, respectivamente. No agregado nacional, apenas 2% das escolas participantes n�o possuem essa informa��o. O fato de n�o haver muita falta de informa��o sobre o INSE pode ser devido � metodologia de imputa��o aplicada  pelo INEP.

# Entre as escolas que participaram da avalia��o e possuem as informa��es computadas, � indicado uma taxa de participa��o dos estudantes para cada n�vel de ensino avaliado. Essa taxa � calculada pela raz�o entre os estudantes matriculados no censo e os estudantes presentes no dia da avalia��o.

# Mec�nica do V de Cramer : 
R <- c(sum(obs[1,]),sum(obs[2,]))
C <- c(sum(obs[,1]),sum(obs[,2]), sum(obs[,3]))
n <- sum(obs)
E <- matrix(,nrow = 2, ncol = 3)

E[1,1] <- R[1]*C[1]/n    
E[2,1] <- R[2]*C[1]/n    
E[1,2] <- R[1]*C[2]/n    
E[2,2] <- R[2]*C[2]/n    
E[1,3] <- R[1]*C[3]/n    
E[2,3] <- R[2]*C[3]/n    

obs - E

# � igual no teste qui-quadrado
teste <- chisq.test(obs)
obs - teste$expected

OE <- (obs - E)^2
x2 <- sum(OE)
C  <- sqrt(x2/(x2+n))

# Analisando a mec�nica do teste qui-quadrado, vemos que h� uma sobre-representa��o
# de dados observados e escolas com INSE alto. Com isso, podemos sugerir
# que entre os dados faltantes, a possibilidade de enviesamento � maior 
# para escolas de INSE baixo e m�dio, uma vez que elas possuem mais dados
# faltantes sobre profici�ncia comparado �s escolas com alto INSE.

# 3. Salvando os dados

# dir.create("analise_missing")
# wd <- "C:/Users/Victor Alcantara/Dropbox/mestrado PPGSA_VictorAlcantara/2_scripts/an�lise_descritiva/analise_missing/"
# setwd(wd)

# Participacao escolas x saeb

# Salvar/carregar dados
# save(fq.censo,file = paste0(wd,"fq.censo.RData"))
# save(fq.saeb,file = paste0(wd,"fq.saeb.RData"))
# save(missing,file = paste0(wd,"missing.RData"))
# save(tx_participacao,file = paste0(wd,"tx_participacao.RData"))
# save(tabs.da,file = paste0(wd,"tabs.da.RData"))
# save(tabs.inse,file = paste0(wd,"tabs.inse.RData"))
# save(cors.da,file = paste0(wd,"cors.da.RData"))
# save(cors.inse,file = paste0(wd,"cors.inse.RData"))

# load(file = paste0(wd,"fq.censo.RData"))
# load(file = paste0(wd,"fq.saeb.RData"))
# load(file = paste0(wd,"missing.RData"))
# load(file = paste0(wd,"tx_participacao.RData"))
# load(file = paste0(wd,"tabs.da.RData"))
# load(file = paste0(wd,"tabs.inse.RData"))
# load(file = paste0(wd,"cors.da.RData"))
# load(file = paste0(wd,"cors.inse.RData"))