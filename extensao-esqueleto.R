# Script para leitura de bancos de dados diversos para geração de um data frame de uma única linha referente as informações do estado do aluno

# Ao receber este script esqueleto colocá-lo no repositório LOCAL Extensao, que deve ter sido clonado do GitHub
# Enviar o script esqueleto para o repositório REMOTO com o nome extensao-esqueleto.R

# Para realizar as tarefas da ETAPA 1, ABRIR ANTES uma branch de nome SINASC no main de Extensao e ir para ela
# Após os alunos concluírem a ETAPA 1 a professora orientará fazer o merge into main e depois abrir outro branch. Aguarde...
library(dplyr)

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
                                   labels = c("Unica", "Dupla", "Tripla ou mais"))
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


# Frequências de variáveis categóricas
# Exemplos:
#Sexo
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$SEXO, levels = c("Feminino",
                                                                             "Masculino")))
df = as.data.frame.matrix(tab)
names(df) = c("TRSEXO_F","TRSEXO_M")
df$CODMUNRES = rownames(df)

base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Tipo de Parto
tab = table( dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$PARTO, levels = c("Vaginal",
                                                                               "Cesário")))
df = as.data.frame.matrix(tab)
names(df) = c("TPV","TPC")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)






# Tarefa 11: Exporte o banco de dados com o nome SINASC_UF.csv
write.csv2(base, file = "SINASC_UF.csv")


# Ao terminar a ETAPA 1 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 1"



##################################
# ETAPA 2: BANCO DE DADOS DO SIM
##################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 2

# Tarefa 1. Leitura do banco de dados Mortalidade_Geral_2015 do SIM 2015 com 1216475 linhas e 87 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sim


# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# as colunas serão (a informar)
# nomes das respectivas variáveis: CONTADOR, TIPOBITO, CODMUNNATU, IDADE,  SEXO,  RACACOR,  ESTCIV, ESC2010, 
# CODMUNRES,  LOCOCOR, CODMUNOCOR, TPMORTEOCO,  OBITOGRAV, OBITOPUERP, CAUSABAS, CAUSABAS_O, TPOBITOCOR, MORTEPARTO



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

