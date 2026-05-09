# Script para leitura de bancos de dados diversos para geração de um data frame de uma única linha referente as informações do estado do aluno

# Ao receber este script esqueleto colocá-lo no repositório LOCAL Extensao, que deve ter sido clonado do GitHub
# Enviar o script esqueleto para o repositório REMOTO com o nome extensao-esqueleto.R

# Para realizar as tarefas da ETAPA 1, ABRIR ANTES uma branch de nome SINASC no main de Extensao e ir para ela
# Após os alunos concluírem a ETAPA 1 a professora orientará fazer o merge into main e depois abrir outro branch. Aguarde...
library(tidyverse)

####################################
# ETAPA 1: BANCO DE DADOS DO SINASC
####################################

# A ALTERAÇÃO DO SCRIPT ESQUELETO - ETAPA 1 - DEVERÁ SER FEITA DENTRO DA BRANCH SINASC

# Tarefa 1. Leitura do banco de dados do SINASC 2015  com 3017668 linhas e 61 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sinasc
dados_sinasc = read.csv("SINASC_2015.csv", sep = ";")

# Tarefa 2. Reduzir dados_sinasc apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sinasc_1
# as colunas serão 1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61
# nomes das respectivas variáveis: CONTADOR, CODMUNNASC, LOCNASC, IDADEMAE, ESTCIVMAE, CODMUNRES, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, PESO, IDANOMAL, ESCMAE2010, RACACORMAE, SEMAGESTAC, CONSPRENAT, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK
dados_sinasc_1 = dados_sinasc[,c(1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61)]
dados_sinasc_1 |> str()
# Tarefa 3. Reduzir dados_sinasc_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sinasc_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF 

# observar abaixo o número de nascimentos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 27918     12: 16980     13: 80097     14: 11409     15: 143657    16: 15750      17: 25110
# 21: 117564    22: 49253     23: 132516    24: 49099     25: 59089     26: 145024     27: 52257     28: 34917     29: 206655
# 31: 268305    32: 56941     33: 236960    35: 634026     
# 41: 160947    42: 97223     43: 148359
# 50: 44142     51: 56673     52: 100672    53: 46122 
dados_sinasc_1$CODMUNRES = dados_sinasc_1$CODMUNRES |> as.character()
dados_sinasc_2 = dados_sinasc_1[startsWith(dados_sinasc_1$CODMUNRES, "43"),]


# Ao concluir a Tarefa 3 da Etapa 1 commite e envie para o repositório REMOTO o script e dados_sinasc_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"

dados_sinasc_2 |> write.csv2("dados_sinasc_2.csv",row.names = FALSE)
#dados_sinasc_2 = read.csv2("dados_sinasc_2.csv")#Linha pra se eu fazer alguma besteira
rm(dados_sinasc)
rm(dados_sinasc_1)
gc()
# Tarefa 4. Verificar em dados_sinasc_2 a frequência das categorias das seguintes variáveis: LOCNASC, ESTCIVMAE, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, IDANOMAL, ESCMAE2010, RACACORMAE, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK
table(dados_sinasc_2$LOCNASC)
table(dados_sinasc_2$ESTCIVMAE)
table(dados_sinasc_2$GESTACAO)
table(dados_sinasc_2$GRAVIDEZ)
table(dados_sinasc_2$PARTO)
table(dados_sinasc_2$SEXO)
table(dados_sinasc_2$APGAR5)
table(dados_sinasc_2$RACACOR)
table(dados_sinasc_2$IDANOMAL)
table(dados_sinasc_2$ESCMAE2010)
table(dados_sinasc_2$RACACORMAE)
table(dados_sinasc_2$TPAPRESENT)
table(dados_sinasc_2$TPROBSON)
table(dados_sinasc_2$PARIDADE)
table(dados_sinasc_2$KOTELCHUCK)
# Tarefa 5. Atribuir para cada variável de dados_sinasc_2 como sendo NA a categoria de "Não informado ou Ignorado", geralmente com código 9
# KOTELCHUCK = 9 significa "não informado"   TPROBSON = 11 significa "não classificado por falta de informação"
# veja o dicionário do SINASC para identificar qual o código das categorias de cada variável
dados_sinasc_2 = dados_sinasc_2 |> mutate(ESTCIVMAE = na_if(ESTCIVMAE, 9)) |>
  mutate(SEXO = na_if(SEXO, 0)) |> mutate(APGAR5 = na_if(APGAR5, 99)) |>
  mutate(IDANOMAL = na_if(IDANOMAL, 9)) |> mutate(ESCMAE2010 = na_if(ESCMAE2010, 9)) |>
  mutate(TPAPRESENT = na_if(TPAPRESENT, 9)) |> mutate(TPROBSON = na_if(TPROBSON, 11)) |>
  mutate(KOTELCHUCK = na_if(KOTELCHUCK, 9))
# Tarefa 6. Atribuir legendas para as categorias das variáveis investigadas na etapa 4.
# Exemplo: dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5), 
# labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado",  
# "Mais que adequado")

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados
dados_sinasc_2$LOCNASC = factor(dados_sinasc_2$LOCNASC, 
                                levels = c(1,2,3,4),
                                labels = c("Hospital", "Outros estabelecimentos de saude", "Domicilio", "Outros"))
dados_sinasc_2$ESTCIVMAE = factor(dados_sinasc_2$ESTCIVMAE, 
                                levels = c(1,2,3,4,5),
                                labels = c("Solteira", "Casada", "Viuva", "Divorciada", "Uniao estavel"))
dados_sinasc_2$GESTACAO = factor(dados_sinasc_2$GESTACAO, 
                                  levels = c(1,2,3,4,5,6),
                                  labels = c("Menos de 22 semanas", "22 a 27 semanas", "28 a 31 semanas", "32 a 36 semanas", "37 a 41 semanas", "42 semanas e mais"))
dados_sinasc_2$GRAVIDEZ = factor(dados_sinasc_2$GRAVIDEZ, 
                                   levels = c(1,2,3),
                                   labels = c("Única", "Dupla", "Tripla ou mais"))
dados_sinasc_2$PARTO = factor(dados_sinasc_2$PARTO, 
                                  levels = c(1,2),
                                  labels = c("Vaginal", "Cesario"))
dados_sinasc_2$SEXO = factor(dados_sinasc_2$SEXO, 
                                  levels = c(1,2),
                                  labels = c("Masculino", "Feminino"))
dados_sinasc_2$RACACOR = factor(dados_sinasc_2$RACACOR, 
                                  levels = c(1,2,3,4,5),
                                  labels = c("Branca", "Preta", "Amarela", "Parda", "Indigena"))
dados_sinasc_2$IDANOMAL = factor(dados_sinasc_2$IDANOMAL, 
                                  levels = c(1,2),
                                  labels = c("Sim", "Nao"))
dados_sinasc_2$ESCMAE2010 = factor(dados_sinasc_2$ESCMAE2010, 
                                  levels = c(0,1,2,3,4,5),
                                  labels = c("Sem escolaridade", "Fundamental I", "Fundamental II", "Medio", "Superior incompleto", "Superior completo"))
dados_sinasc_2$RACACORMAE = factor(dados_sinasc_2$RACACORMAE, 
                                   levels = c(1,2,3,4,5),
                                   labels = c("Branca", "Preta", "Amarela", "Parda", "Indigena"))
dados_sinasc_2$TPAPRESENT = factor(dados_sinasc_2$TPAPRESENT, 
                                   levels = c(1,2,3),
                                   labels = c("Cefalico", "Pelvica ou podalica", "Transversa"))
dados_sinasc_2$PARIDADE = factor(dados_sinasc_2$PARIDADE, 
                                   levels = c(0,1),
                                   labels = c("Multipara", "Nulipara"))
dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, 
                                   levels = c(1,2,3,4,5),
                                   labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado", "Mais que adequado"))


# Tarefa 7. Categorizar as variáveis IDADEMAE, PESO e APGAR5
# nova variável: dados_sinasc_2$F_PESO com PESO: < 2500: Baixo peso, >=2500 e < 4000: Peso normal, >= 4000: Macrossomia
# nova variável dados_sinasc_2$F_IDADE com IDADEMAE: <15, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50+
# nova variável dados_sinasc_2$F_APGAR5 com APGAR5: < 7: Baixo, >= 7: Normal
# Atenção para casos de NA em IDADEMAE, PESO e APGAR5
# Ao categorizar as variáveis, garantir que sejam transformadas em tipo fator

dados_sinasc_2$F_PESO = cut(dados_sinasc_2$PESO, breaks = c(0, 2500, 4000, Inf), labels = c("Baixo Peso", "Peso normal", "Macrossomia"))
dados_sinasc_2$F_IDADE = cut(dados_sinasc_2$IDADEMAE, breaks = c(0, 15, 19, 24, 29, 34, 39, 44, 49, Inf), labels = c("<15", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+"))
dados_sinasc_2$F_APGAR5 = cut(dados_sinasc_2$APGAR5, breaks = c(0, 7, Inf), labels = c("Baixo", "Normal"))
str(dados_sinasc_2) #checar se tudo virou factor
# Tarefa 8. Agregar ao banco de dados_sinasc_2 as informações PESO_P10 e PESO_P90 a partir de Tabela_PIG_Brasil.csv
# a Tabela PIG informa P10 e P90 dos pesos, de acordo com a idade gestacional
# criar nova variável referente ao peso, de acordo com a idade gestacional, conforme indicado abaixo
# nova variável apenas para casos de GRAVIDEZ única: dados_sinasc_2$F_PIG: PIG: PESO < PESO_P10, AIG: PESO_P10 <= PESO <= PESO_P90, GIG: PESO > PESO_P90
# Atenção para casos de NA em SEMAGESTAC, PESO ou SEXO. Lembre-se também que em dados_sinasc_2 SEXO está como fator com as categorias Feminino e Masculino.
tabela_pig = read.csv("Tabela_PIG_Brasil.csv", header = TRUE, sep=";")
tabela_pig$SEXO = factor(tabela_pig$SEXO, levels = c("Masculino", "Feminino"))
dados_sinasc_2 = merge(dados_sinasc_2, tabela_pig, by = c("SEMAGESTAC","SEXO"), all.x = TRUE)
dados_sinasc_2$F_PIG=ifelse(dados_sinasc_2$GRAVIDEZ != "Única", NA,
                            ifelse(is.na(dados_sinasc_2$PESO)|is.na(dados_sinasc_2$PESO_P10)|is.na(dados_sinasc_2$PESO_P90),
                                   NA,
                                   ifelse(dados_sinasc_2$PESO < dados_sinasc_2$PESO_P10, "PIG",
                                          ifelse(dados_sinasc_2$PESO<=dados_sinasc_2$PESO_P90, "AIG", "GIG"))))
dados_sinasc_2$F_PIG = factor(dados_sinasc_2$F_PIG, levels = c("PIG","AIG","GIG"))
# criar nova variável referente ao deslocamento materno para realizar o parto, chamado de peregrinação
# nova variável: dados_sinasc_2$PERIG: Não: CODMUNNASC igual a CODMUNRES, Sim: CODMUNNASC diferente de CODMUNRES


#Tarefas 9 e 10 (reformulada) do script esqueleto:
  
#  Crie um banco de dados, de nome SINASC_UF.csv (Exemplo: SINASC_RJ.csv), contendo as 103 variáveis listadas no arquivo “Variáveis - Projeto - Tarefas 9 e 10 da Etapa 1.pdf”

#O banco final deverá possuir:
#  • 103 colunas, correspondentes às variáveis especificadas;
#• n + 1 linhas, onde:
#  • n corresponde ao número de municípios distintos da UF em análise
#• a primeira linha corresponde aos valores agregados para a UF como um todo;
#• as demais linhas correspondem aos municípios da UF.
#As variáveis devem ser construídas a partir dos microdados do SINASC (dados_sinasc, dados_sinasc_1 e dados_sinasc_2, respeitando os nomes e a ordem especificados.

# Base inicial (municípios)
# Cria um dataframe com uma única coluna (CODMUNRES) com valores ordenados e sem repetição
base = data.frame(CODMUNRES =sort(unique(dados_sinasc_2$CODMUNRES)))
base$ANO = 2015
base$NIVEL = "MUNICIPIO"
base = base[,c(2,3,1)]
# TN - total de nascimentos
TN = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES, levels = base$CODMUNRES)))
names(TN) = c("CODMUNRES","TN")
base = merge(base, TN, by = "CODMUNRES", all.x = TRUE)

# TNRC - completos nas 61 variáveis
dados_sinasc$CODMUNRES =dados_sinasc$CODMUNRES |> as.character()
dados_UF = dados_sinasc[startsWith(dados_sinasc$CODMUNRES, "43"),]
dados_UF_comp = dados_UF[complete.cases(dados_UF), ]
TNRC = as.data.frame(table(factor(dados_UF_comp$CODMUNRES, levels = base$CODMUNRES)))
names(TNRC) = c("CODMUNRES","TNRC")
base = merge(base, TNRC, by = "CODMUNRES", all.x = TRUE)

# TNRCR - completos nas 22 variáveis
dados_UF_1 = dados_sinasc_1[startsWith(dados_sinasc_1$CODMUNRES, "43"),]
dados_UF_1_comp = dados_UF_1[complete.cases(dados_UF_1), ]
TNRCR = as.data.frame(table(factor(dados_UF_1_comp$CODMUNRES, levels = base$CODMUNRES)))
names(TNRCR) = c("CODMUNRES","TNRCR")
base = merge(base, TNRCR, by = "CODMUNRES", all.x = TRUE)

# Idade categorizada
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$F_IDADE, levels = c("<15","15-
19","20-24","25-29", "30-34","35-39","40-44","45-49","50+")))
df = as.data.frame.matrix(tab)
names(df) = c( "TGI_15","TGI_15_19","TGI_20_24","TGI_25_29", "TGI_30_34","TGI_35_39","TGI_40_44",
               "TGI_45_49","TGI_50")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)
#Total de gestantes
TGIF = dados_sinasc_2 |> group_by(CODMUNRES) |> count()
names(TGIF) = c("CODMUNRES", "TGIF")
base = merge(base, TGIF, by = "CODMUNRES", all.x = TRUE)

# Exemplos de percentis
# Idade da mãe
p_idade = aggregate( IDADEMAE ~ CODMUNRES, dados_sinasc_2, function(x) quantile(x, probs =
                                                                                  c(0.25,0.5,0.75), na.rm = TRUE))
p_idade = do.call(data.frame, p_idade)
names(p_idade) = c("CODMUNRES","IM_P25","IM_P50", "IM_P75")
p_idade[, c("IM_P25","IM_P50","IM_P75")] = round(p_idade[, c("IM_P25","IM_P50","IM_P75")], 2)
base = merge(base, p_idade, by="CODMUNRES", all.x=TRUE)

# Medidas descritivas
#Exemplos de média e desvio-padrão
# Idade da mãe
media_idade = aggregate(IDADEMAE ~ CODMUNRES, dados_sinasc_2, mean, na.rm = TRUE)
media_idade$IDADEMAE = round(media_idade$IDADEMAE, 2)
names(media_idade)[2] = "IM_MD"
dp_idade = aggregate(IDADEMAE ~ CODMUNRES, dados_sinasc_2, sd, na.rm = TRUE)
dp_idade$IDADEMAE = round(dp_idade$IDADEMAE, 2)
names(dp_idade)[2] = "IM_DP"
temp = merge(media_idade, dp_idade, by = "CODMUNRES")
base = merge(base, temp, by = "CODMUNRES", all.x = TRUE)

#Escolaridade
escolaridade = dados_sinasc_2 |> group_by(CODMUNRES, ESCMAE2010) |> count() |> 
  pivot_wider(names_from = ESCMAE2010, values_from =  n) |> as.data.frame()
escolaridade[is.na(escolaridade)] = 0
escolaridade = escolaridade[,-ncol(escolaridade)]
names(escolaridade) = c("CODMUNRES","EM_S","EM_FI","EM_FII","EM_M","EM_SI","EM_SC")
base = merge(base, escolaridade, by = "CODMUNRES", all.x = TRUE)

#Raça/cor mae
raca = dados_sinasc_2 |> group_by(CODMUNRES, RACACORMAE) |> count() |> 
  pivot_wider(names_from = RACACORMAE, values_from =  n) |> as.data.frame()
raca[is.na(raca)] = 0
raca = raca[,-ncol(raca)]
names(raca) = c("CODMUNRES","TGRC_B","TGRC_PT","TGRC_PD","TGRC_I","TGRC_A")
base = merge(base, raca, by = "CODMUNRES", all.x = TRUE)

#Companheiro
companheiro = dados_sinasc_2
companheiro$ESTCIVMAE = ifelse(companheiro$ESTCIVMAE == "Casada" | companheiro$ESTCIVMAE == "Uniao estavel", 
                               "TGCC", "TGSC")
companheiro = companheiro |> group_by(CODMUNRES, ESTCIVMAE) |> count() |>
  pivot_wider(names_from = ESTCIVMAE, values_from =  n) |> as.data.frame()
companheiro[is.na(companheiro)] = 0
companheiro = companheiro[,-ncol(companheiro)]
base = merge(base, companheiro, by = "CODMUNRES", all.x = TRUE)

#Primeiro filho
paridade = dados_sinasc_2 |> group_by(CODMUNRES, PARIDADE) |> count() |> 
  pivot_wider(names_from = PARIDADE, values_from =  n) |> as.data.frame()
paridade[is.na(paridade)] = 0
names(paridade) = c("CODMUNRES","TGNPRI","TGPRI")
base = merge(base, paridade, by = "CODMUNRES", all.x = TRUE)

#Gemeos
gemeos = dados_sinasc_2
gemeos$GRAVIDEZ = ifelse(gemeos$GRAVIDEZ == "Unica", "TGU", "TGG")
gemeos = gemeos |> group_by(CODMUNRES, GRAVIDEZ) |> count() |>
  pivot_wider(names_from = GRAVIDEZ, values_from =  n) |> as.data.frame()
gemeos[is.na(gemeos)] = 0
base = merge(base, gemeos, by = "CODMUNRES", all.x = TRUE)

#tempo gestacao
tempo = dados_sinasc_2 |> group_by(CODMUNRES, GESTACAO) |> count() |> 
  pivot_wider(names_from = GESTACAO, values_from =  n) |> as.data.frame()
tempo[is.na(tempo)] = 0
tempo = tempo[,-7]
names(tempo) = c("CODMUNRES","TGD_32_36","TGD_37_41", "TGD_42","TGD_28_31","TGD_22_27","TGD_22")
base = merge(base, tempo, by = "CODMUNRES", all.x = TRUE)

#pre termo
pre_termo = dados_sinasc_2
pre_termo$GESTACAO = ifelse(pre_termo$GESTACAO == "Menos de 22 semanas" | 
                              pre_termo$GESTACAO == "22 a 27 semanas" |
                              pre_termo$GESTACAO == "28 a 31 semanas" |
                              pre_termo$GESTACAO == "32 a 36 semanas", 
                               "TGD_PRT", ifelse(pre_termo$GESTACAO == "37 a 41 semanas", "TGD_AT", "TGD_PST"))
pre_termo = pre_termo |> group_by(CODMUNRES, GESTACAO) |> count() |>
  pivot_wider(names_from = GESTACAO, values_from =  n) |> as.data.frame()
pre_termo[is.na(pre_termo)] = 0
pre_termo = pre_termo[,-ncol(pre_termo)]
base = merge(base, pre_termo, by = "CODMUNRES", all.x = TRUE)
#percentils duracao
p_gestacao = aggregate( SEMAGESTAC ~ CODMUNRES, dados_sinasc_2, function(x) quantile(x, probs =
                                                                                  c(0.25,0.5,0.75), na.rm = TRUE))
p_gestacao = do.call(data.frame, p_gestacao)
names(p_gestacao) = c("CODMUNRES","DG_P25","DG_P50", "DG_P75")
p_gestacao[, c("DG_P25","DG_P50","DG_P75")] = round(p_gestacao[, c("DG_P25","DG_P50","DG_P75")], 2)
base = merge(base, p_gestacao, by="CODMUNRES", all.x=TRUE)

#media e desvio gestante
media_gestacao = aggregate(SEMAGESTAC ~ CODMUNRES, dados_sinasc_2, mean, na.rm = TRUE)
media_gestacao$SEMAGESTAC = round(media_gestacao$SEMAGESTAC, 2)
names(media_gestacao)[2] = "DG_MD"
dp_gestacao = aggregate(SEMAGESTAC ~ CODMUNRES, dados_sinasc_2, sd, na.rm = TRUE)
dp_gestacao$SEMAGESTAC = round(dp_gestacao$SEMAGESTAC, 2)
names(dp_gestacao)[2] = "DG_DP"
temp = merge(media_idade, dp_gestacao, by = "CODMUNRES")
base = merge(base, temp, by = "CODMUNRES", all.x = TRUE)

#pre-natal
pre_natal = dados_sinasc_2 |> group_by(CODMUNRES, KOTELCHUCK) |> count() |> 
  pivot_wider(names_from = KOTELCHUCK, values_from =  n) |> as.data.frame()
pre_natal[is.na(pre_natal)] = 0
pre_natal = pre_natal[,-5]
names(pre_natal) = c("CODMUNRES","TKC_ID","TKC_AD","TKC_MAD","TKC_IT","TGRC_NR")
base = merge(base, pre_natal, by = "CODMUNRES", all.x = TRUE)

#peregrinacao
peregrinacao = dados_sinasc_2
peregrinacao$PEREGRINACAO = ifelse(peregrinacao$CODMUNRES != peregrinacao$CODMUNNASC, 
                               "TGPRG_S", "TGPRG_N")
peregrinacao = peregrinacao |> group_by(CODMUNRES, PEREGRINACAO) |> count() |>
  pivot_wider(names_from = PEREGRINACAO, values_from =  n) |> as.data.frame()
peregrinacao[is.na(peregrinacao)] = 0
base = merge(base, peregrinacao, by = "CODMUNRES", all.x = TRUE)

#tipo_parto
tipo_parto = dados_sinasc_2 |> group_by(CODMUNRES, PARTO) |> count() |> 
  pivot_wider(names_from = PARTO, values_from =  n) |> as.data.frame()
tipo_parto[is.na(tipo_parto)] = 0
tipo_parto = tipo_parto[,-ncol(tipo_parto)]
names(tipo_parto) = c("CODMUNRES","TPV","TPC")
base = merge(base, tipo_parto, by = "CODMUNRES", all.x = TRUE)

#TPAPRESENT
tpapresent = dados_sinasc_2 |> group_by(CODMUNRES, TPAPRESENT) |> count() |> 
  pivot_wider(names_from = TPAPRESENT, values_from =  n) |> as.data.frame()
tpapresent[is.na(tpapresent)] = 0
tpapresent = tpapresent[,-4]
names(tpapresent) = c("CODMUNRES","TRAP_C","TRAP_P", "TRAP_T")
base = merge(base, tpapresent, by = "CODMUNRES", all.x = TRUE)

#TPROBSON
tprobson = dados_sinasc_2 |> group_by(CODMUNRES, TPROBSON) |> count() |> 
  pivot_wider(names_from = TPROBSON, values_from =  n) |> as.data.frame()
tprobson[is.na(tprobson)] = 0
tprobson = tprobson[,-11]
names(tprobson) = c("CODMUNRES","TGROB_1","TGROB_2","TGROB_3","TGROB_4","TGROB_5",
                    "TGROB_6","TGROB_7","TGROB_8","TGROB_10","TGROB_9")
base = merge(base, tprobson, by = "CODMUNRES", all.x = TRUE)

#nasc
nasc = dados_sinasc_2 |> group_by(CODMUNRES, LOCNASC) |> count() |> 
  pivot_wider(names_from = LOCNASC, values_from =  n) |> as.data.frame()
nasc[is.na(nasc)] = 0
names(nasc) = c("CODMUNRES","TNLOC_H","TNLOC_O","TNLOC_ES","TNLOC_D")
nasc$TNLOC_AI = 0
base = merge(base, nasc, by = "CODMUNRES", all.x = TRUE)

#sexo
sexo = dados_sinasc_2 |> group_by(CODMUNRES, SEXO) |> count() |> 
  pivot_wider(names_from = SEXO, values_from =  n) |> as.data.frame()
sexo[is.na(sexo)] = 0
sexo = sexo[,-ncol(sexo)]
names(sexo) = c("CODMUNRES","TRS_M","TRS_F")
base = merge(base, sexo, by = "CODMUNRES", all.x = TRUE)

#raca/cor
raca = dados_sinasc_2 |> group_by(CODMUNRES, RACACOR) |> count() |> 
  pivot_wider(names_from = RACACOR, values_from =  n) |> as.data.frame()
raca[is.na(raca)] = 0
raca = raca[,-ncol(raca)]
names(raca) = c("CODMUNRES","TRRC_B","TRRC_PT","TRRC_PD","TRRC_I","TRRC_A")
base = merge(base, raca, by = "CODMUNRES", all.x = TRUE)

#peso
peso = dados_sinasc_2 |> group_by(CODMUNRES, F_PESO) |> count() |> 
  pivot_wider(names_from = F_PESO, values_from =  n) |> as.data.frame()
peso[is.na(peso)] = 0
peso = peso[,-ncol(peso)]
names(peso) = c("CODMUNRES","TRP_BP","TRP_N","TRP_M")
base = merge(base, peso, by = "CODMUNRES", all.x = TRUE)

#percentil peso
p_peso = aggregate( PESO ~ CODMUNRES, dados_sinasc_2, function(x) quantile(x, probs =
                                                                                       c(0.25,0.5,0.75), na.rm = TRUE))
p_peso = do.call(data.frame, p_peso)
names(p_peso) = c("CODMUNRES","PESO_P25","PESO_P50", "PESO_P75")
p_peso[, c("PESO_P25","PESO_P50","PESO_P75")] = round(p_peso[, c("PESO_P25","PESO_P50","PESO_P75")], 2)
base = merge(base, p_peso, by="CODMUNRES", all.x=TRUE)

#media e desvio padrao peso
media_peso = aggregate(PESO ~ CODMUNRES, dados_sinasc_2, mean, na.rm = TRUE)
media_peso$PESO = round(media_peso$PESO, 2)
names(media_idade)[2] = "PESO_MD"
dp_peso = aggregate(PESO ~ CODMUNRES, dados_sinasc_2, sd, na.rm = TRUE)
dp_peso$PESO = round(dp_peso$PESO, 2)
names(dp_peso)[2] = "PESO_DP"
temp = merge(media_peso, dp_peso, by = "CODMUNRES")
base = merge(base, temp, by = "CODMUNRES", all.x = TRUE)

#f_pig
f_pig = dados_sinasc_2 |> group_by(CODMUNRES, F_PIG) |> count() |> 
  pivot_wider(names_from = F_PIG, values_from =  n) |> as.data.frame()
f_pig[is.na(f_pig)] = 0
f_pig = f_pig[,-ncol(f_pig)]
names(f_pig) = c("CODMUNRES","TRPIG_P","TRPIG_A","TRPIG_G")
base = merge(base, f_pig, by = "CODMUNRES", all.x = TRUE)

#apgar 5
f_apgar5 = dados_sinasc_2 |> group_by(CODMUNRES, F_APGAR5) |> count() |> 
  pivot_wider(names_from = F_APGAR5, values_from =  n) |> as.data.frame()
f_apgar5[is.na(f_apgar5)] = 0
f_apgar5 = f_apgar5[,-ncol(f_apgar5)]
names(f_apgar5) = c("CODMUNRES","TRAPG5_B","TRAPG5_N")
base = merge(base, f_apgar5, by = "CODMUNRES", all.x = TRUE)

#media e desvio padrao apgar 5
media_apgar5 = aggregate(APGAR5 ~ CODMUNRES, dados_sinasc_2, mean, na.rm = TRUE)
media_apgar5$APGAR5 = round(media_apgar5$APGAR5, 2)
names(media_apgar5)[2] = "APGAR5_MD"
dp_apgar5 = aggregate(APGAR5 ~ CODMUNRES, dados_sinasc_2, sd, na.rm = TRUE)
dp_apgar5$APGAR5 = round(dp_apgar5$APGAR5, 2)
names(dp_apgar5)[2] = "APGAR5_DP"
temp = merge(media_apgar5, dp_apgar5, by = "CODMUNRES")
base = merge(base, temp, by = "CODMUNRES", all.x = TRUE)

#anomalia
anomalia = dados_sinasc_2 |> group_by(CODMUNRES, IDANOMAL) |> count() |> 
  pivot_wider(names_from = IDANOMAL, values_from =  n) |> as.data.frame()
anomalia[is.na(anomalia)] = 0
anomalia = anomalia[,-ncol(anomalia)]
names(anomalia) = c("CODMUNRES","TRSAC","TRAC")
base = merge(base, anomalia, by = "CODMUNRES", all.x = TRUE)


x = colSums(base[,4:ncol(base)])
y = c(NA,2015,"ESTADO",x)
base = rbind(base, y)
# Tarefa 11: Exporte o banco de dados com o nome SINASC_UF.csv
write.csv2(base, file = "SINASC_UF.csv")


# Ao terminar a ETAPA 1 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 1"



##################################
# ETAPA 2: BANCO DE DADOS DO SIM
##################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 2

##################################
# ETAPA 2: BANCO DE DADOS DO SIM
##################################
# Só inicie esta Etapa quando a professora orientar
# Altere o script esqueleto nas partes que se refere a ETAPA 2 e envie para o repositório Extensao tendo feito o commite "Esqueleto atualizado na Etapa 2"
# A partir de main crie a branch SIM
# ESTANDO NA BRANCH SIM, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 1 e só insira comandos na ETAPA 2
# Para realizar as tarefas da ETAPA 2, ABRIR ANTES uma branch de nome SIM no main de Extensao e ir para ela

# Tarefa 1. Leitura do banco de dados Mortalidade_Geral_2015 do SIM 2015 com 1264175 linhas e 87 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sim
dados_sim = read.csv2("Mortalidade_Geral_2015.csv")
str(dados_sim)


# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# as colunas serão: 1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84
# nomes das respectivas variáveis: CONTADOR, TIPOBITO, DTOBITO, DTNASC, IDADE, SEXO, RACACOR, ESC2010, CODMUNRES, TPMORTEOCO, 
# OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO
dados_sim_1 = dados_sim[,c(1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84)]
dados_sim_1$CODMUNRES = dados_sim_1$CODMUNRES |> as.character()
str(dados_sim_1)
# Tarefa 3. Reduzir dados_sim_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sim_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF 
dados_sim_2 = dados_sim_1[startsWith(dados_sim_1$CODMUNRES, "43"),]

# observar abaixo o número de óbitos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 7948      12: 3517      13: 16675     14: 2091      15: 37365     16: 2946       17: 7402
# 21: 33666     22: 19366     23: 55258     24: 20153     25: 26422     26: 62556      27: 19756     28: 13453     29: 87083
# 31: 131274    32: 22332     33: 127714    35: 287645     
# 41: 70839     42: 37984     43: 82349
# 50: 15457     51: 17095     52: 38854     53: 11975

# Exportar o arquivo com o nome dados_sim_2.csv
dados_sim_2 |> write.csv2("dados_sim_2.csv",row.names = FALSE)
#dados_sim_2 = read.csv2("dados_sim_2.csv")
rm(dados_sim)
rm(dados_sim_1)
gc()


# Ao concluir a Tarefa 3 da Etapa 2 commite e envie para o repositório REMOTO o script e dados_sim_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"


# Tarefa 4. Verificar em dados_sim_2 a frequência das categorias das seguintes variáveis: TIPOBITO, SEXO, RACACOR, 
# TPMORTEOCO, OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO
table(dados_sim_2$TIPOBITO)
table(dados_sim_2$SEXO)
table(dados_sim_2$RACACOR)
table(dados_sim_2$TPMORTEOCO)
table(dados_sim_2$OBITOGRAV)
table(dados_sim_2$OBITOPUERP)
table(dados_sim_2$CAUSABAS)
table(dados_sim_2$TPOBITOCOR)
table(dados_sim_2$MORTEPARTO)
# Tarefa 5. Atribuir para cada variável de dados_sim_2 como sendo NA a categoria de "Não informado ou Ignorado", geralmente com código 9
# veja o dicionário do SIM para identificar qual o código das categorias de cada variável
# Em variáveis quantitativas como IDADE verificar se existem valores como 99 para NA
dados_sim_2 = dados_sim_2 |> mutate(SEXO = na_if(SEXO, 0)) |>
  mutate(TPMORTEOCO = na_if(TPMORTEOCO, 9)) |> mutate(OBITOGRAV = na_if(OBITOGRAV, 9)) |>
  mutate(OBITOPUERP = na_if(OBITOPUERP, 9)) |> mutate(TPMORTEOCO = na_if(TPMORTEOCO, 8)) |>
  mutate(TPOBITOCOR = na_if(TPOBITOCOR, 9)) |> mutate(MORTEPARTO = na_if(MORTEPARTO, 9)) |> 
  mutate(IDADE = na_if(IDADE, 999))

# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4.
# Exemplo: dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO, levels = c(1,2), 
# labels = c("Fetal", "Não fetal")

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados

dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO, 
                                levels = c(1,2),
                                labels = c("Fetal", "Não Fetal"))
dados_sim_2$SEXO = factor(dados_sim_2$SEXO, 
                              levels = c(1,2),
                              labels = c("Masculino", "Feminino"))
dados_sim_2$RACACOR = factor(dados_sim_2$RACACOR, 
                          levels = c(1,2,3,4,5),
                          labels = c("Branca", "Preta", "Amarela", "Parda", "Indigena"))
dados_sim_2$TPMORTEOCO = factor(dados_sim_2$TPMORTEOCO, 
                             levels = c(1,2,3,4,5,8),
                             labels = c("Na gravidez", "No parto", "No abortamento", 
                                        "Até 42 dias após o término do parto", 
                                        "de 43 dias a 1 ano após o termino da gestação",
                                        "Não ocorreu nestes períodos"))
dados_sim_2$OBITOGRAV = factor(dados_sim_2$OBITOGRAV, 
                                levels = c(1,2),
                                labels = c("Sim", "Não"))
dados_sim_2$OBITOPUERP = factor(dados_sim_2$OBITOPUERP, 
                               levels = c(1,2,3),
                               labels = c("Sim, até 42 dias após o parto",
                                          "Sim, de 43 dias a 1 ano" ,"Não"))
dados_sim_2$TPOBITOCOR = factor(dados_sim_2$TPOBITOCOR, 
                                levels = c(1,2,3,4,5,6,7,8),
                                labels = c("Durante a gestação", "Durante o abortamento",
                                           "Após o abortamento", 
                                           "No parto ou até 1 hora após o parto",
                                           "No puerpério - até 42 dias após o parto", 
                                           "Entre 43 dias e até 1 ano após o parto", 
                                           "A investigação não identificou",
                                           "Mais de um ano após o parto"))
dados_sim_2$MORTEPARTO = factor(dados_sim_2$MORTEPARTO, 
                                levels = c(1,2,3),
                                labels = c("Antes",
                                           "Durante" ,"Após"))
# Tarefa 7. Crie um banco de dados, de nome SIM_UF.csv (Exemplo: SIM_RJ.csv), contendo as 41 variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 7 da Etapa 2.pdf”
# Atenção:
# 1. Para informações gerais utilize CAUSABAS, SEXO e IDADE
# 2. Para informações fetais utilize TIPOBITO
# 3. Para informações neonatais utilize TIPOBITO não fetal e IDADE entre 0 e 27 dias e RACACOR
# 4. Para informações maternas utilize TPMORTEOCO, ESC e IDADE
base = data.frame(CODMUNRES =sort(unique(dados_sim_2$CODMUNRES)))
base$ANO = 2015
base$NIVEL = "MUNICIPIO"
base = base[,c(2,3,1)]

######Informações de obitos 
#Informações Gerais
total = dados_sim_2 |> group_by(CODMUNRES) |> count()
names(total) = c("CODMUNRES", "TO")
base = merge(base, total, by = "CODMUNRES", all.x = TRUE)

dados_sim |> drop_na() |> nrow()
dados_sim_2|> drop_na() |> nrow()
# os dois sao zero entao:
base$TORC = 0
base$TORCR = 0

#causas
causas = dados_sim_2
causas$CAUSABAS = ifelse(startsWith(causas$CAUSABAS, c("V","W","X","Y")),"TO_NN","TO_N" )
causas = causas |> group_by(CODMUNRES, CAUSABAS) |> count() |> 
  pivot_wider(names_from = CAUSABAS, values_from = n) |> as.data.frame()
causas[is.na(causas)] = 0
base = merge(base, causas, by = "CODMUNRES", all.x = TRUE)

causas = dados_sim_2
causas$CAUSABAS = ifelse(startsWith(causas$CAUSABAS, c("A","B")),"TO_CB_I",
                         ifelse(startsWith(causas$CAUSABAS, c("C","D")),"TO_CB_N",
                         ifelse(startsWith(causas$CAUSABAS, "I"), "TO_CB_C",
                         ifelse(startsWith(causas$CAUSABAS, "J"), "TO_CB_R", "TO_CB_O"))))
causas = causas |> group_by(CODMUNRES, CAUSABAS) |> count() |> 
  pivot_wider(names_from = CAUSABAS, values_from = n) |> as.data.frame()
causas[is.na(causas)] = 0
base = merge(base, causas, by = "CODMUNRES", all.x = TRUE)

#sexo
sexo = dados_sim_2 |> group_by(CODMUNRES, SEXO) |> count() |> 
  pivot_wider(names_from = SEXO, values_from = n) |> as.data.frame()
sexo = sexo[,-ncol(sexo)]
names(sexo) = c("CODMUNRES", "TO_M", "TO_F")
sexo[is.na(sexo)] = 0
base = merge(base, sexo, by = "CODMUNRES", all.x = TRUE)

#idade_fertil
idade_fertil = dados_sim_2 |> filter(SEXO == "Feminino", IDADE >= 415, IDADE <= 449) |> group_by(CODMUNRES) |> count()
names(idade_fertil) = c("CODMUNRES", "TO_F_IF")
base = merge(base, idade_fertil, by = "CODMUNRES", all.x = TRUE)

#obitos_fetais
obitos = dados_sim_2 |> filter(TPOBITOCOR == "Durante a gestação" | TPOBITOCOR == "Durante o abortamento " |
                               TPOBITOCOR == "Após o abortamento" | 
                               TPOBITOCOR == "No parto ou até 1 hora após o parto") |> group_by(CODMUNRES) |>
  count()
names(obitos) = c("CODMUNRES", "TO_FT")
obitos[is.na(obitos)] = 0
base = merge(base, obitos, by = "CODMUNRES", all.x = TRUE)

#neo_natais
neo_natais = dados_sim_2 |> filter(IDADE <= 227) |> group_by(CODMUNRES) |> count()
names(neo_natais) = c("CODMUNRES", "TO_NT")
neo_natais[is.na(neo_natais)] = 0
base = merge(base, neo_natais, by = "CODMUNRES", all.x = TRUE)

neo_natais = dados_sim_2 |> filter(IDADE <= 227)
neo_natais$IDADE = ifelse(neo_natais$IDADE <= 6, "TO_NT_P", "TO_NT_T")
neo_natais = neo_natais |> group_by(CODMUNRES, IDADE) |> count() |> 
  pivot_wider(names_from = IDADE, values_from = n) |> as.data.frame()
neo_natais[is.na(neo_natais)] = 0
base = merge(base, neo_natais, by = "CODMUNRES", all.x = TRUE)

neo_natais = dados_sim_2 |> filter(IDADE <= 2364 & IDADE >= 228) |> group_by(CODMUNRES) |> count()
names(neo_natais) = c("CODMUNRES", "TO_PNT")
neo_natais[is.na(neo_natais)] = 0
base = merge(base, neo_natais, by = "CODMUNRES", all.x = TRUE)

neo_natais = dados_sim_2 |> filter(TPOBITOCOR == "Durante a gestação") |> group_by(CODMUNRES) |> count()
names(neo_natais) = c("CODMUNRES", "TO_MT_G")
neo_natais[is.na(neo_natais)] = 0
base = merge(base, neo_natais, by = "CODMUNRES", all.x = TRUE)

neo_natais = dados_sim_2 |> filter(IDADE <= 227) |> group_by(CODMUNRES, RACACOR) |> count() |>
  pivot_wider(names_from = RACACOR, values_from = n) |> as.data.frame()

neo_natais = neo_natais[, -6]
neo_natais[is.na(neo_natais)] = 0
names(neo_natais) = c("CODMUNRES", "TONT_B","TONT_PT","TONT_PD","TONT_I","TONT_A")
base = merge(base, neo_natais, by = "CODMUNRES", all.x = TRUE)


# Tarefa 8: Exporte o banco de dados com o nome SIM_UF.csv


#####################################################
# ETAPA 3: OUTROS BANCOS DE DADOS: IBGE, SNIS, ...
#####################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 3

# Tarefa 1. Acesso aos bancos de dados e obtenção da informação



#####################################################################################################
# ETAPA 4: GERAR BANCO DE DADOS FINAL DO ESTADO, BASEADO NAS ANÁLISES DE SINASC, SIM, IBGE, SNIS,...
######################################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 4

# Cada aluno gerar um dataframe de uma única linha (referente ao seu estado) com as variáveis na ordem indicada pela professora



############################################################################################
# ETAPA 5: EMPILHAMENTO DOS DATAFRAMES DE CADA ESTADO, GERANDO UM DATAFRAME DE 27 LINHAS
############################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 5

# 1. Enviar arquivos para as pastas do repositório da Professora no GitHUb
# 2. A professora fará o empilhamentos dos dataframes

