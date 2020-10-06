#Instalando e carregando pacotes################################################

#install.packages("stringr")
library(stringr)

# Análise gráfica bidimensional para duas variáveis qualitativas
#install.packages('ggplot2')
library(ggplot2)
library(scales)
library(reshape2)

# Lendo um ano_2013 em formato excel (dataset reduzido)
library(readxl)
library(data.table)

library(knitr)
library(ggplot2)

library(readr)
library(sqldf)

library(dplyr)
#setar diretório de trabalho
setwd('C:/Users/tailo/OneDrive/CIMATEC/Trabalho\ Estatística')

#Leitura de dataset
dados = read.csv("DATASET_TRABALHO.csv", header = TRUE, sep = ';', dec = ',', stringsAsFactors = F)
#####

#Explorando o conjunto de dados#################################################
str(dados)
head(dados)
names(dados)

#Conta a quantidade de linhas e colunas
dim(dados)
#####

#Aplicando algumas tranformações nos dados######################################

#Conversão para data table
dados = as.data.table(dados)
View(dados)
#Converte para mínuscula
tolower(names(dados))

# colnames(dados) <- c("ano", "codigo_estado","sigla_estado","codigo_municipio","num_municipio",
#                  "codigo_escola_educacenso", "num_escola_educacenso","tipo_dependecia_administrativa",
#                  "localizacao_da_escola","num_matriculas","num_participantes_com_necessidades",
#                  "num_participantes", "taxa_participacao", "media_ciencias_da_natureza",
#                  "media_ciencias_humanas", "media_lingua_portuguesa", "media_matematica", "media_redacao",
#                  "media_objetivas", "media_total", "indicador_nivel_socioeconomico", 
#                  "adequacao_da_formacao_docente", "taxa_de_permanencia_escolar",
#                  "taxa_de_aprovacao", "taxa_de_reprovacao", "taxa_de_abandono", "porte_escolar"
# )

#Conversão de dados para inteiro
dados[,codigo_estado := as.numeric(codigo_estado)]
dados[,codigo_municipio := as.numeric(codigo_municipio)]
dados[,codigo_escola_educacenso := as.numeric(codigo_escola_educacenso)]
dados[,num_matriculas := as.numeric(num_matriculas)]
dados[,num_participantes_com_necessidades := as.numeric(num_participantes_com_necessidades)]
dados[,num_participantes := as.numeric(num_participantes)]
dados[,taxa_participacao := as.numeric(taxa_participacao)]
dados[,media_ciencias_da_natureza := as.numeric(media_ciencias_da_natureza)]
dados[,media_ciencias_humanas := as.numeric(media_ciencias_humanas)]
dados[,media_lingua_portuguesa := as.numeric(media_lingua_portuguesa)]
dados[,media_matematica := as.numeric(media_matematica)]
dados[,media_redacao := as.numeric(media_redacao)]
dados[,media_objetivas := as.numeric(media_objetivas)]
dados[,media_total := as.numeric(media_total)]
dados[,adequacao_da_formacao_docente := as.numeric(adequacao_da_formacao_docente)]
dados[,taxa_de_permanencia_escolar := as.numeric(taxa_de_permanencia_escolar)]
dados[,taxa_de_aprovacao := as.numeric(taxa_de_aprovacao)]
dados[,taxa_de_reprovacao := as.numeric(taxa_de_reprovacao)]
dados[,taxa_de_abandono := as.numeric(taxa_de_abandono)]
#####

#Agrupamento por ano####

ano_2015 = (dados[ano == 2015])

total_linha_2015 = nrow(ano_2015)

#####

#Loop para trocar o numero da coluna por sua respectviva dependência administrativa####
for (i in 1:total_linha_2015){
  if(ano_2015[,tipo_dependecia_administrativa][i] == 1){
    ano_2015$tipo_dependecia_administrativa[i] <- 'Federal'
  }else if(ano_2015[,tipo_dependecia_administrativa][i] == 2){
    ano_2015$tipo_dependecia_administrativa[i] <- 'Estadual'
  }else if(ano_2015[,tipo_dependecia_administrativa][i] == 3){
    ano_2015$tipo_dependecia_administrativa[i] <- 'Municipal'
  }else if(ano_2015[,tipo_dependecia_administrativa][i] == 4){ 
    ano_2015$tipo_dependecia_administrativa[i] <- 'Privada'
  }else{
    ano_2015$tipo_dependecia_administrativa[i] <- ''
  }
  print(i)
}
#####

#Loop para trocar o numero da coluna por sua respectviva localização####
for(j in 1:total_linha_2015){
  if(ano_2015[,localizacao_da_escola][j] == 1){
    ano_2015$localizacao_da_escola[j] <- 'Urbana'   
  }else if(ano_2015[,localizacao_da_escola][j] == 2){ 
    ano_2015$localizacao_da_escola[j] <- 'Rural'
  }else{
    ano_2015$localizacao_da_escola[i] <- ''
  }
  print(j)
}
#####

# Loop para calcular as médias totais sem as objetivas####

for(j in 1:total_linha_2015){
  soma =  ano_2015[,media_ciencias_da_natureza][j] + 
    ano_2015[,media_ciencias_humanas][j] +
    ano_2015[,media_lingua_portuguesa][j] +
    ano_2015[,media_matematica][j] +
    ano_2015[,media_redacao][j]
  media = soma/5
  ano_2015$media_total[j] = round(media, digits = 2)
}
View(ano_2015)
######

#Loop para calcular as médias sem redação####
for(k in 1:total_linha_2015){
  soma =  ano_2015[,media_ciencias_da_natureza][k] + 
    ano_2015[,media_ciencias_humanas][k] +
    ano_2015[,media_lingua_portuguesa][k] +
    ano_2015[,media_matematica][k]
  media = soma/4
  ano_2015$media_sem_redacao[k] = round(media, digits = 2)
  ano_2015$media_sem_redacao[k]
}
View(ano_2015)
######

#Criação de dataset com modificações####
View(ano_2015)
write.csv2(ano_2015, file = 'DATASET_TRABALHO.csv')
#####

#cálculo de frequência e proporção de colunas da média total####################
#Distribuições de frequência possibilitam o entendimento do comportamento das variáveis.
#Variáveis quantitativas devem ser agrupadas antes da construção de distribuições de frequências.

freq = table(ano_2015$taxa_participacao)
prop = round(prop.table(table(ano_2015$taxa_participacao))*100,2)
kable(cbind(freq,prop),col.names = c("Freq","Prop"))
####
freq = table(ano_2015$adequacao_da_formacao_docente)
prop = round(prop.table(table(ano_2015$adequacao_da_formacao_docente))*100,2)
kable(cbind(freq,prop),col.names = c("Freq","Prop"))
####
freq = table(ano_2015$taxa_de_aprovacao)
prop = round(prop.table(table(ano_2015$taxa_de_aprovacao))*100,2)
kable(cbind(freq,prop),col.names = c("Freq","Prop"))
####
freq = table(ano_2015$taxa_de_reprovacao)
prop = round(prop.table(table(ano_2015$taxa_de_reprovacao))*100,2)
kable(cbind(freq,prop),col.names = c("Freq","Prop"))
####
freq = table(ano_2015$taxa_de_abandono)
prop = round(prop.table(table(ano_2015$taxa_de_abandono))*100,2)
kable(cbind(freq,prop),col.names = c("Freq","Prop"))
#####

###Avaliando simetria dos dados#####
boxplot(ano_2015$num_matriculas)
hist(ano_2015$num_matriculas)

boxplot(ano_2015$num_participantes)
hist(ano_2015$num_participantes)

boxplot(ano_2015$num_participantes_com_necessidades)
hist(ano_2015$num_participantes_com_necessidades)

boxplot(ano_2015$taxa_participacao)
hist(ano_2015$taxa_participacao)

boxplot(ano_2015$media_ciencias_da_natureza)
hist(ano_2015$media_ciencias_da_natureza)

boxplot(ano_2015$media_ciencias_humanas)
hist(ano_2015$media_ciencias_humanas)

boxplot(ano_2015$media_lingua_portuguesa)
hist(ano_2015$media_lingua_portuguesa)

boxplot(ano_2015$media_matematica)
hist(ano_2015$media_matematica)

boxplot(ano_2015$media_redacao)
hist(ano_2015$media_redacao)

boxplot(ano_2015$media_total)
hist(ano_2015$media_total)

boxplot(ano_2015$adequacao_da_formacao_docente)
hist(ano_2015$adequacao_da_formacao_docente)

boxplot(ano_2015$taxa_de_permanencia_escolar)
hist(ano_2015$taxa_de_permanencia_escolar)

boxplot(ano_2015$taxa_de_aprovacao)
hist(ano_2015$taxa_de_aprovacao)

boxplot(ano_2015$taxa_de_reprovacao)
hist(ano_2015$taxa_de_reprovacao)

boxplot(ano_2015$taxa_de_abandono)
hist(ano_2015$taxa_de_abandono)
######

#Simetria e normalização#####
# Normalizando dados
# Aplicando transformaçõess
ano_2015poptransf <- ano_2015[ 
  , list(log = log(num_matriculas),
         p25 = num_matriculas^(1/4),
         p50 = num_matriculas^(1/2),
         p33 = num_matriculas^(1/3))
]

par(mfrow=c(2,2))
hist(ano_2015poptransf$log, main="log", ylab="", xlab="", col="darkgrey", border="white")
hist(ano_2015poptransf$p25, main="p=1/4", ylab="", xlab="", col="darkgrey", border="white")
hist(ano_2015poptransf$p50, main="p=1/2", ylab="", xlab="", col="darkgrey", border="white")
hist(ano_2015poptransf$p33, main="p=1/3", ylab="", xlab="", col="darkgrey", border="white")
#Normalizado com applicação do log

# Normalizando dados
# Aplicando transformaçõess
ano_2015poptransf <- ano_2015[ 
  , list(log = exp(num_participantes_com_necessidades),
         p25 = num_participantes_com_necessidades^(1/4),
         p50 = num_participantes_com_necessidades^(1/2),
         p33 = num_participantes_com_necessidades^(1/3))
]


par(mfrow=c(2,2))
hist(ano_2015poptransf$log, main="log", ylab="", xlab="", col="darkgrey", border="white")
hist(ano_2015poptransf$p25, main="p=1/4", ylab="", xlab="", col="darkgrey", border="white")
hist(ano_2015poptransf$p50, main="p=1/2", ylab="", xlab="", col="darkgrey", border="white")
hist(ano_2015poptransf$p33, main="p=1/3", ylab="", xlab="", col="darkgrey", border="white")
#Buscar uma outra maneira de normalizar ou retirar da análise devido a pouca quantidade de 
#participantes com necessidades especiais identificadas

# Normalizando dados
# Aplicando transformaçõess
ano_2015poptransf <- ano_2015[ 
  , list(log = log(num_participantes),
         p25 = num_participantes^(1/4),
         p50 = num_participantes^(1/2),
         p33 = num_participantes^(1/3))
]


par(mfrow=c(2,2))
hist(ano_2015poptransf$log, main="log", ylab="", xlab="", col="darkgrey", border="white")
hist(ano_2015poptransf$p25, main="p=1/4", ylab="", xlab="", col="darkgrey", border="white")
hist(ano_2015poptransf$p50, main="p=1/2", ylab="", xlab="", col="darkgrey", border="white")
hist(ano_2015poptransf$p33, main="p=1/3", ylab="", xlab="", col="darkgrey", border="white")
#Normalizado com applicação do log

# Normalizando dados
# Aplicando transformaçõess
ano_2015poptransf <- ano_2015[ 
  , list(log = log(taxa_de_permanencia_escolar),
         p25 = taxa_de_permanencia_escolar^(1/4),
         p50 = taxa_de_permanencia_escolar^(1/2),
         p33 = taxa_de_permanencia_escolar^(1/3))
]


par(mfrow=c(2,2))
hist(ano_2015poptransf$log, main="log", ylab="", xlab="", col="darkgrey", border="white")
hist(ano_2015poptransf$p25, main="p=1/4", ylab="", xlab="", col="darkgrey", border="white")
hist(ano_2015poptransf$p50, main="p=1/2", ylab="", xlab="", col="darkgrey", border="white")
hist(ano_2015poptransf$p33, main="p=1/3", ylab="", xlab="", col="darkgrey", border="white")
#Normalizado com applicação do log

# Normalizando dados
# Aplicando transformaçõess
ano_2015poptransf <- ano_2015[ 
  , list(log = log(taxa_de_reprovacao),
         p25 = taxa_de_reprovacao^(1/4),
         p50 = taxa_de_reprovacao^(1/2),
         p33 = taxa_de_reprovacao^(1/3))
]


par(mfrow=c(2,2))
hist(ano_2015poptransf$log, main="log", ylab="", xlab="", col="darkgrey", border="white")
hist(ano_2015poptransf$p25, main="p=1/4", ylab="", xlab="", col="darkgrey", border="white")
hist(ano_2015poptransf$p50, main="p=1/2", ylab="", xlab="", col="darkgrey", border="white")
hist(ano_2015poptransf$p33, main="p=1/3", ylab="", xlab="", col="darkgrey", border="white")
#Normalizado com applicação do log

# Normalizando dados
# Aplicando transformaçõess
ano_2015poptransf <- ano_2015[ 
  , list(log = log(taxa_de_abandono),
         p25 = taxa_de_abandono^(1/4),
         p50 = taxa_de_abandono^(1/2),
         p33 = taxa_de_abandono^(1/3))
]


par(mfrow=c(2,2))
hist(ano_2015poptransf$log, main="log", ylab="", xlab="", col="darkgrey", border="white")
hist(ano_2015poptransf$p25, main="p=1/4", ylab="", xlab="", col="darkgrey", border="white")
hist(ano_2015poptransf$p50, main="p=1/2", ylab="", xlab="", col="darkgrey", border="white")
hist(ano_2015poptransf$p33, main="p=1/3", ylab="", xlab="", col="darkgrey", border="white")
#Normalizado com applicação do log

# Normalizando dados
# Aplicando transformaçõess
ano_2015poptransf <- ano_2015[ 
  , list(log = log(taxa_participacao),
         p25 = taxa_participacao^(1/4),
         p50 = taxa_participacao^(1/2),
         p33 = taxa_participacao^(1/3))
]


par(mfrow=c(2,2))
hist(ano_2015poptransf$log, main="log", ylab="", xlab="", col="darkgrey", border="white")
hist(ano_2015poptransf$p25, main="p=1/4", ylab="", xlab="", col="darkgrey", border="white")
hist(ano_2015poptransf$p50, main="p=1/2", ylab="", xlab="", col="darkgrey", border="white")
hist(ano_2015poptransf$p33, main="p=1/3", ylab="", xlab="", col="darkgrey", border="white")
#Normalizado com log

# Normalizando dados
# Aplicando transformaçõess
ano_2015poptransf <- ano_2015[ 
  , list(log = log(taxa_de_aprovacao),
         p25 = taxa_de_aprovacao^(1/4),
         p50 = taxa_de_aprovacao^(1/2),
         p33 = taxa_de_aprovacao^(1/3))
]


par(mfrow=c(2,2))
hist(ano_2015poptransf$log, main="log", ylab="", xlab="", col="darkgrey", border="white")
hist(ano_2015poptransf$p25, main="p=1/4", ylab="", xlab="", col="darkgrey", border="white")
hist(ano_2015poptransf$p50, main="p=1/2", ylab="", xlab="", col="darkgrey", border="white")
hist(ano_2015poptransf$p33, main="p=1/3", ylab="", xlab="", col="darkgrey", border="white")
#Não houve normalização

#####

#COletar amostras###############################################################
set.seed(6520)

enem_2015 <- ano_2015[sample(nrow(ano_2015), 30), ]

#Agrupamento por estado Acre
escolas_ac <- ano_2015[sample(nrow(ano_2015[sigla_estado == "AC"]), 30), ]

#Agrupamento por estado Alagoas
escolas_al <- ano_2015[sample(nrow(ano_2015[sigla_estado == "AL"]), 30), ]

#Agrupamento por estado Amapá
escolas_ap <- ano_2015[sample(nrow(ano_2015[sigla_estado == "AP"]), 30), ]

#Agrupamento por estado Amazonas
escolas_am <- ano_2015[sample(nrow(ano_2015[sigla_estado == "AM"]), 30), ]

#Agrupamento por estado Bahia
escolas_ba <- ano_2015[sample(nrow(ano_2015[sigla_estado == "BA"]), 30), ]

#Agrupamento por estado Ceará
escolas_ce <- ano_2015[sample(nrow(ano_2015[sigla_estado == "CE"]), 30), ]

#Agrupamento por estado Distrito Federal
escolas_df <- ano_2015[sample(nrow(ano_2015[sigla_estado == "DF"]), 30), ]

#Agrupamento por estado Espiríto Santo
escolas_es <- ano_2015[sample(nrow(ano_2015[sigla_estado == "ES"]), 30), ]

#Agrupamento por estado Goiás
escolas_go <- ano_2015[sample(nrow(ano_2015[sigla_estado == "GO"]), 30), ]

#Agrupamento por estado Maranhão
escolas_ma <- ano_2015[sample(nrow(ano_2015[sigla_estado == "MA"]), 30), ]

#Agrupamento por estado Mato Grosso
escolas_mt <- ano_2015[sample(nrow(ano_2015[sigla_estado == "MT"]), 30), ]

#Agrupamento por estado Mato Grosso do Sul
escolas_ms <- ano_2015[sample(nrow(ano_2015[sigla_estado == "MS"]), 30), ]

#Agrupamento por estado Minas Gerais
escolas_mg <- ano_2015[sample(nrow(ano_2015[sigla_estado == "MG"]), 30), ]

#Agrupamento por estado Pará
escolas_pa <- ano_2015[sample(nrow(ano_2015[sigla_estado == "PA"]), 30), ]

#Agrupamento por estado Paraíba
escolas_pb <- ano_2015[sample(nrow(ano_2015[sigla_estado == "PB"]), 30), ]

#Agrupamento por estado paraná
escolas_pr <- ano_2015[sample(nrow(ano_2015[sigla_estado == "PR"]), 30), ]

#Agrupamento por estado Pernambuco
escolas_pe <- ano_2015[sample(nrow(ano_2015[sigla_estado == "PE"]), 30), ]

#Agrupamento por estado Piauí
escolas_pi <- ano_2015[sample(nrow(ano_2015[sigla_estado == "PI"]), 30), ]

#Agrupamento por estado Rio de Janeiro
escolas_rj <- ano_2015[sample(nrow(ano_2015[sigla_estado == "RJ"]), 30), ]

#Agrupamento por estado Rio Grande do Norte
escolas_rn <- ano_2015[sample(nrow(ano_2015[sigla_estado == "RN"]), 30), ]

#Agrupamento por estado Rondônia
escolas_ro <- ano_2015[sample(nrow(ano_2015[sigla_estado == "RO"]), 30), ]

#Agrupamento por estado Roraima
escolas_rr <- ano_2015[sample(nrow(ano_2015[sigla_estado == "RR"]), 30), ]

#Agrupamento por estado Rio Grande do Sul
escolas_rs <- ano_2015[sample(nrow(ano_2015[sigla_estado == "RS"]), 30), ]

#Agrupamento por estado Santa Catarina
escolas_sc <- ano_2015[sample(nrow(ano_2015[sigla_estado == "SC"]), 30), ]

#Agrupamento por estado São Paulo
escolas_sp <- ano_2015[sample(nrow(ano_2015[sigla_estado == "SP"]), 30), ]

#Agrupamento por estado Sergipe
escolas_se <- ano_2015[sample(nrow(ano_2015[sigla_estado == "SE"]), 30), ]

#Agrupamento por estado Sergipe
escolas_to <- ano_2015[sample(nrow(ano_2015[sigla_estado == "TO"]), 30), ]
#####
#-------------------------------------------------------------------------------|
###Inferência dos dados#####

# Distribuição amostral
mi_media_total_amostra = mean(ano_2015$media_total,na.rm=T)
round(mi_media_total_amostra, digits = 2)
var_media_total = var(ano_2015$media_total,na.rm=T)
round(var_media_total, digits = 2)
sd_media_amostral = sd(ano_2015$media_total,na.rm=T)

medias = c(mean(sample(ano_2015[!is.na(media_total)]$media_total,2))
           ,mean(sample(ano_2015[!is.na(media_total)]$media_total,4))
           ,mean(sample(ano_2015[!is.na(media_total)]$media_total,6))
           ,mean(sample(ano_2015[!is.na(media_total)]$media_total,8))
           ,mean(sample(ano_2015[!is.na(media_total)]$media_total,20))
           ,mean(sample(ano_2015[!is.na(media_total)]$media_total,50))
           ,mean(sample(ano_2015[!is.na(media_total)]$media_total,100))
           ,mean(sample(ano_2015[!is.na(media_total)]$media_total,200))
           ,mean(sample(ano_2015[!is.na(media_total)]$media_total,300))
           ,mean(sample(ano_2015[!is.na(media_total)]$media_total,400))
           ,mean(sample(ano_2015[!is.na(media_total)]$media_total,500))
           ,mean(sample(ano_2015[!is.na(media_total)]$media_total,600))
           ,mean(sample(ano_2015[!is.na(media_total)]$media_total,700))
           ,mean(sample(ano_2015[!is.na(media_total)]$media_total,800))
           ,mean(sample(ano_2015[!is.na(media_total)]$media_total,900))
           ,mean(sample(ano_2015[!is.na(media_total)]$media_total,1000))
)
plot(medias)
abline(a=mean(ano_2015$media_total,na.rm=T),b=0)

#Inicializando as variaveis como vetores numericos
## Media para amostras de tam 4
xbar4<-numeric()
## Media para amostras de tam 8
xbar8<-numeric()
## Media para amostras de tam 11
xbar11<-numeric()
## Media para amostras de tam 15
xbar15<-numeric()
## Media para amostras de tam 30
xbar30<-numeric()
## Media para amostras de tam 100
xbar100<-numeric()

for ( i in 1:200){
  # Extraindo amostras de tamanho 4 e calculando a média
  smp<-sample(ano_2015[!is.na(media_total)]$media_total,size = 4)
  xbar4[i]<-round(mean(smp),2)
  
  # Extraindo amostras de tamanho 8 e calculando a média
  smp<-sample(ano_2015[!is.na(media_total)]$media_total,size = 8)
  xbar8[i]<-round(mean(smp),2)
  
  # Extraindo amostras de tamanho 11 e calculando a média
  smp<-sample(ano_2015[!is.na(media_total)]$media_total,size = 11)
  xbar11[i]<-round(mean(smp),2)
  
  # Extraindo amostras de tamanho 15 e calculando a média
  smp<-sample(ano_2015[!is.na(media_total)]$media_total,size = 15)
  xbar15[i]<-round(mean(smp),2)
  
  # Extraindo amostras de tamanho 30 e calculando a média
  smp<-sample(ano_2015[!is.na(media_total)]$media_total,size = 30)
  xbar30[i]<-round(mean(smp),2)
  
  # Extraindo amostras de tamanho 100 e calculando a média
  smp<-sample(ano_2015[!is.na(media_total)]$media_total,size = 100)
  xbar100[i]<-round(mean(smp),2)
}

par(mfrow=c(2,3))
hist(xbar4, col="lightblue4", border="white",freq = FALSE, breaks = 15, main=paste0("n=4 ",round(mean(xbar15),2)))
hist(xbar8, col="lightblue4", border="white",freq = FALSE, breaks = 15, main=paste0("n=8 ",round(mean(xbar30),2)))
hist(xbar11, col="lightblue4", border="white",freq = FALSE, breaks = 15, main=paste0("n=11 ",round(mean(xbar100),2)))
hist(xbar15, col="lightblue4", border="white",freq = FALSE, breaks = 15, main=paste0("n=15 ",round(mean(xbar15),2)))
hist(xbar30, col="lightblue4", border="white",freq = FALSE, breaks = 15, main=paste0("n=30 ",round(mean(xbar30),2)))
hist(xbar100, col="lightblue4", border="white",freq = FALSE, breaks = 15, main=paste0("n=100 ",round(mean(xbar100),2)))
####


x_barra = mean(ano_2015$media_total)

z = x_barra - mi_media_total_amostra / (var_media_total/sqrt(30))

# Calculando probabilidades

q1 = (32.07-42.07)/(var(df$idade_anos,na.rm=T)/sqrt(1000))
q2 = (52.07-42.07)/(var(df$idade_anos,na.rm=T)/sqrt(1000))

pnorm(q1,lower.tail = F) - pnorm(q2,lower.tail = F)

# Calculando proporção de óbitos por sexo 
prop.table(table(obitos$SEXO))
p = 0.56
q = 0.44
n = 10
variancia = p*q/n  
q1 = (-0.01)/(sqrt(variancia))
q2 = (0.01)/(sqrt(variancia))

pnorm(q1,lower.tail = F) - pnorm(q2,lower.tail = F)

#####

#Propriedades dos estimadores###############################
parametro = mean(dados$media_total,na.rm=T)
# Extraindo 50 amostras de tamanho 100 e calculando a média amostral através do estimador x barra
n = 100
amostras = 50
x_barra = c()
for( i in 1:amostras){
  x_barra[i]<-mean(sample(dados[!is.na(media_total)]$media_total, size = n, replace=T)) 
}
x_barra
min(x_barra)
max(x_barra)

# Checando se em média o estimador x barra acerta o parâmetro
mean(x_barra)
parametro

# Checando se a diferença entre a estimativa e o parâmetro tende a ser pequena para n grande 
ns = c(100,500,1000,2000,5000,10000)
amostras = 50
var_amostral = c()
x_barra = c()
for(n in 1:length(ns)){# para cada tamanho de amostra
  print(paste0("Obtendo 50 amostras de tamanho: ",ns[n]," e calculando a média."))
  for( i in 1:amostras){#Retirar 50 amostras aleatórias e calcular variância
    x_barra[i]<-mean(sample(ano_2015[!is.na(media_total)]$media_total, size = ns[n], replace=T))
  }
  print("Médias:")
  print(x_barra)
  var_amostral[n] <- var(x_barra)
}
var_amostral
plot(ns,var_amostral,xlab="Tamanho da amostra",
     ylab = "Variância amostral",
     type="b",
     pch = 19, 
     col = "red")

#####

#Intervalos de confiança##########################

amostras = 100 # Numero de amostras
n=50 # Numero de observações por amostra

x_sim<-matrix(NA,n,amostras) 
# x_sim armazenara as amostras simuladas
# x_sim é uma matriz de dados ( a príncipio preenchida com NAs) onde cada coluna da patriz representa
# uma amostra aleatória retirada da população, cada amostra tem 25 observações. 

for( i in 1:amostras){
  x_sim[,i]<-sample(dados[!is.na(media_total)]$media_total, size = n, replace=T) 
  #x_sim <- cbind(x_sim,amostra)
}


# Calculando intervalos de confiança
# Exemplo IC distribuição normal e variância conhecida:
# Deinifição dos limites inferiores e superiores

li90 <- mean(ano_2015$media_total) - 1.645*1/sqrt(length(ano_2015$media_total))
ls90 <- mean(ano_2015$media_total) + 1.645*1/sqrt(length(ano_2015$media_total))

ic90 <- c(li90,ls90)
ic90
#####

# Teste de hipótese####

# A média de aprovação de alunos de escola particular é maior que 90

# Definição das Hipóteses

#Ho: Alunos de escolas privadas tem taixa de aprovação igual ou acima de 90%
#H1: Alunos de escolas privadas tem taixa de aprovação abaixo de 90%
#com notas inferiores


# H0 >= 90
# H1 < 90
# 1
# Cálculo da média populacional
# com variância conhecida e tamanho de amostra pequeno 
mean(ano_2015[!is.na(taxa_de_aprovacao) & tipo_dependencia %in% "Privada"]$taxa_de_aprovacao)
x <- sample(ano_2015[!is.na(taxa_de_aprovacao) & tipo_dependencia %in% "Privada"]$taxa_de_aprovacao,30)

hist(x)

quantis <- qt(0.90, df = length(x)-1)

# 2
# cálcular estimador
xbarra <- mean(x)
desvio <- sd(x)
n <- length(x)

# 3
# Nível de significância
# Por padrão é usaod 0,5


# 4
#Estatística de teste
t <- (xbarra - enem_2015$media_redacao)/(desvio/sqrt(n))

# 5
# Interpretação e coclusão

# Rejeitamos H0 se t > quantis
t > quantis

# Teste unilateral esquerdo
quantis <- qt(0.1, df = length(x)-1)

# Rejeitamos H0 se t < quantis
t < quantis


#####
#-------------------------------------------------------------------------------|

#####Avaliação das taxas de aprovação e reprovação com base nas médias#####
descip_medias = c("Ciências da Natureza", "Ciências Humanas", "Matemática", "Língua Portuguesa",
                  "Redação", "Médias sem Redação", "Taxa de aprovação")

kable(cbind(enem_2015$media_ciencias_da_natureza, enem_2015$media_ciencias_humanas
            , enem_2015$media_matematica, enem_2015$media_lingua_portuguesa, 
            enem_2015$media_redacao, enem_2015$media_sem_redacao, 
            enem_2015$taxa_de_aprovacao[order(enem_2015$taxa_de_aprovacao, decreasing=TRUE)]),col.names = c(descip_medias))

kable(cbind(enem_2015$media_ciencias_da_natureza, enem_2015$media_ciencias_humanas
            , enem_2015$media_matematica, enem_2015$media_lingua_portuguesa, 
            enem_2015$media_redacao, enem_2015$media_sem_redacao, 
            enem_2015$taxa_de_aprovacao[order(enem_2015$taxa_de_reprovacao, decreasing=FALSE)]),col.names = c(descip_medias))


#####

################################################################################
#
# Exemplo IC distribuição não normal, variância desconhecida e tamanho de amostra grande (>40):
# x <- rexp(500)
# Checando que x não parece vir de uma distribuição normal 
# hist(x)
# 
# xbarra <- mean(dados$num_matriculas)
# desvio <- sd(dados$num_matriculas)
# 
# li95 <- mean(dados$num_matriculas)-1.96*desvio/sqrt(length(dados$num_matriculas))
# ls95 <- mean(dados$num_matriculas)+1.96*desvio/sqrt(length(dados$num_matriculas))
# 
# ic95 <- c(li95,ls95)
# ic95
# Exemplo IC distribuição normal, variância desconhecida e tamanho de amostra pequeno 
# x <- sample(dados[!is.na(media_total) & sigla_estado %in% "BA"]$media_total,20)
# 
# hist(x)

# Como é razoável assumir que a distribuição dos dados é Normal, podemos utilizar a distribuição t.
# Encontrando quantis da distribuição t


#

#Quantis

quantis <- qt(c(0.025, 0.975), df = length(dados$num_matriculas)-1)
xbarra <- mean(dados$num_matriculas)
desvio <- sd(dados$num_matriculas)
n <- length(dados$num_matriculas)

(ic95T <- xbarra + quantis*desvio/sqrt(n))

t.test(dados$num_matriculas,conf.level = 0.95)

# Intervalo de confiança para proporção
prop.test(x = 45,
          n = 100,
          conf.level = 0.95)


#

# Intervalo de confiança para dados pareados
marca_A <- c(80, 72, 65, 78, 85)
marca_B <- c(75, 70, 60, 72, 78)

diff_AB <- marca_A - marca_B
summary(diff_AB)
sd(diff_AB)

t.test(marca_A, marca_B,
       paired = T, 
       conf.level = 0.90)

################################################################################

#Responder questionamentos######################################################
#####

# Agrupamento por Regiões#####
soma_ac <- sum(escolas_ac$media_total, na.rm=T)
soma_ap <- sum(escolas_ap$media_total, na.rm=T)
soma_am <- sum(escolas_am$media_total, na.rm=T)
soma_pa <- sum(escolas_pa$media_total, na.rm=T)
soma_ro <- sum(escolas_ro$media_total, na.rm=T)
soma_rr <- sum(escolas_rr$media_total, na.rm=T)
soma_to <- sum(escolas_to$media_total, na.rm=T)
soma_regiao_norte <- sum(soma_ac, soma_ap, soma_am, soma_pr, soma_ro, soma_rr, soma_to)
media_regiao_norte <- soma_regiao_norte/(length(escolas_ac$media_total) 
                                         + length(escolas_ap$media_total) + length(escolas_am$media_total) 
                                         + length(escolas_pr$media_total) + length(escolas_ro$media_total) 
                                         + length(escolas_rr$media_total) + length(escolas_to$media_total))
#format remove casas decimais, mas converte variável para charater
#media_regiao_norte <- format(round(media_regiao_norte, 0), nsmall = 0) 
#media_regiao_norte <- round(media_regiao_norte, 0)
media_regiao_norte = round(media_regiao_norte, digits = 2) 

#Agrupamento por Região Centro-Oeste
soma_df <- sum(escolas_df$media_total, na.rm=T)
soma_go <- sum(escolas_go$media_total, na.rm=T)
soma_mt <- sum(escolas_mt$media_total, na.rm=T)
soma_ms <- sum(escolas_ms$media_total, na.rm=T)
soma_regiao_centro_oeste <- sum(soma_go, soma_mt, soma_ms)
media_regiao_centro_oeste <- soma_regiao_centro_oeste/(length(escolas_df$media_total) + length(escolas_go$media_total)
                                                       + length(escolas_mt$media_total) + length(escolas_ms$media_total))
media_regiao_centro_oeste = round(media_regiao_centro_oeste, digits = 2)

#Agrupamento por Região Nordeste
soma_al <- sum(escolas_al$media_total, na.rm=T)
soma_ba <- sum(escolas_ba$media_total, na.rm=T)
soma_ce <- sum(escolas_ce$media_total, na.rm=T)
soma_ma <- sum(escolas_ma$media_total, na.rm=T)
soma_pb <- sum(escolas_pb$media_total, na.rm=T)
soma_pe <- sum(escolas_pe$media_total, na.rm=T)
soma_pi <- sum(escolas_pi$media_total, na.rm=T)
soma_rn <- sum(escolas_rn$media_total, na.rm=T)
soma_se <- sum(escolas_se$media_total, na.rm=T)
soma_regiao_nordeste <- sum(soma_al, soma_ba, soma_ce, soma_ma, soma_pb, soma_pe, soma_pi, soma_rn, soma_se)
media_regiao_nordeste <- soma_regiao_nordeste/(length(escolas_al$media_total) + length(escolas_ba$media_total) 
                                               + length(escolas_ce$media_total) + length(escolas_ma$media_total) 
                                               + length(escolas_pb$media_total) + length(escolas_pe$media_total) 
                                               + length(escolas_pi$media_total) + length(escolas_rn$media_total) 
                                               + length(escolas_se$media_total))
media_regiao_nordeste = round(media_regiao_nordeste, digits = 2) 

#Agrupamento por Região Sudeste
soma_es <- sum(escolas_es$media_total, na.rm=T)
soma_mg <- sum(escolas_mg$media_total, na.rm=T)
soma_rj <- sum(escolas_rj$media_total, na.rm=T)
soma_sp <- sum(escolas_sp$media_total, na.rm=T)
soma_regiao_sudeste <- sum(soma_es, soma_mg, soma_rj, soma_sp)
media_regiao_sudeste <- soma_regiao_sudeste/(length(escolas_es$media_total) + length(escolas_mg$media_total) 
                                             + length(escolas_rj$media_total) + length(escolas_sp$media_total))
media_regiao_sudeste = round(media_regiao_sudeste, digits = 2)

#Agrupamento por Região Sul
soma_pr <- sum(escolas_pr$media_total, na.rm=T)
soma_rs <- sum(escolas_rs$media_total, na.rm=T)
soma_sc <- sum(escolas_sc$media_total, na.rm=T)
soma_regiao_sul <- sum(soma_pr, soma_rs, soma_sc)
media_regiao_sul <- soma_regiao_sul/(length(escolas_pr$media_total) + length(escolas_rs$media_total) + length(escolas_sc$media_total))
media_regiao_sul = round(media_regiao_sul, digits = 2)


# Análise bivariada para duas variáveis quantitativas
temp<-data.table(regioes=c("Norte","Centro-Oeste","Nordeste","Sudeste","Sul"),
                 medias_por_regiao=c(media_regiao_norte,media_regiao_centro_oeste,media_regiao_nordeste
                                     ,media_regiao_sudeste,media_regiao_sul))



ggplot(temp, aes(x=regioes, y=medias_por_regiao)) +
  geom_bar(stat="identity",fill="steelblue")
#####

#-Média de região####
soma_ac <- sum(escolas_ac$media_total, na.rm=T)
soma_ap <- sum(escolas_ap$media_total, na.rm=T)
soma_am <- sum(escolas_am$media_total, na.rm=T)
soma_pa <- sum(escolas_pa$media_total, na.rm=T)
soma_ro <- sum(escolas_ro$media_total, na.rm=T)
soma_rr <- sum(escolas_rr$media_total, na.rm=T)
soma_to <- sum(escolas_to$media_total, na.rm=T)
soma_regiao_norte <- sum(soma_ac, soma_ap, soma_am, soma_pr, soma_ro, soma_rr, soma_to)
media_regiao_norte <- soma_regiao_norte/(length(escolas_ac$media_total) 
                                         + length(escolas_ap$media_total) + length(escolas_am$media_total) 
                                         + length(escolas_pr$media_total) + length(escolas_ro$media_total) 
                                         + length(escolas_rr$media_total) + length(escolas_to$media_total))
#format remove casas decimais, mas converte variável para charater
#media_regiao_norte <- format(round(media_regiao_norte, 0), nsmall = 0) 
#media_regiao_norte <- round(media_regiao_norte, 0)
media_regiao_norte = round(media_regiao_norte, digits = 2) 

#Agrupamento por Região Centro-Oeste
soma_df <- sum(escolas_df$media_total, na.rm=T)
soma_go <- sum(escolas_go$media_total, na.rm=T)
soma_mt <- sum(escolas_mt$media_total, na.rm=T)
soma_ms <- sum(escolas_ms$media_total, na.rm=T)
soma_regiao_centro_oeste <- sum(soma_go, soma_mt, soma_ms)
media_regiao_centro_oeste <- soma_regiao_centro_oeste/(length(escolas_df$media_total) + length(escolas_go$media_total)
                                                       + length(escolas_mt$media_total) + length(escolas_ms$media_total))
media_regiao_centro_oeste = round(media_regiao_centro_oeste, digits = 2)

#Agrupamento por Região Nordeste
soma_al <- sum(escolas_al$media_total, na.rm=T)
soma_ba <- sum(escolas_ba$media_total, na.rm=T)
soma_ce <- sum(escolas_ce$media_total, na.rm=T)
soma_ma <- sum(escolas_ma$media_total, na.rm=T)
soma_pb <- sum(escolas_pb$media_total, na.rm=T)
soma_pe <- sum(escolas_pe$media_total, na.rm=T)
soma_pi <- sum(escolas_pi$media_total, na.rm=T)
soma_rn <- sum(escolas_rn$media_total, na.rm=T)
soma_se <- sum(escolas_se$media_total, na.rm=T)
soma_regiao_nordeste <- sum(soma_al, soma_ba, soma_ce, soma_ma, soma_pb, soma_pe, soma_pi, soma_rn, soma_se)
media_regiao_nordeste <- soma_regiao_nordeste/(length(escolas_al$media_total) + length(escolas_ba$media_total) 
                                               + length(escolas_ce$media_total) + length(escolas_ma$media_total) 
                                               + length(escolas_pb$media_total) + length(escolas_pe$media_total) 
                                               + length(escolas_pi$media_total) + length(escolas_rn$media_total) 
                                               + length(escolas_se$media_total))
media_regiao_nordeste = round(media_regiao_nordeste, digits = 2) 

#Agrupamento por Região Sudeste
soma_es <- sum(escolas_es$media_total, na.rm=T)
soma_mg <- sum(escolas_mg$media_total, na.rm=T)
soma_rj <- sum(escolas_rj$media_total, na.rm=T)
soma_sp <- sum(escolas_sp$media_total, na.rm=T)
soma_regiao_sudeste <- sum(soma_es, soma_mg, soma_rj, soma_sp)
media_regiao_sudeste <- soma_regiao_sudeste/(length(escolas_es$media_total) + length(escolas_mg$media_total) 
                                             + length(escolas_rj$media_total) + length(escolas_sp$media_total))
media_regiao_sudeste = round(media_regiao_sudeste, digits = 2)

#Agrupamento por Região Sul
soma_pr <- sum(escolas_pr$media_total, na.rm=T)
soma_rs <- sum(escolas_rs$media_total, na.rm=T)
soma_sc <- sum(escolas_sc$media_total, na.rm=T)
soma_regiao_sul <- sum(soma_pr, soma_rs, soma_sc)
media_regiao_sul <- soma_regiao_sul/(length(escolas_pr$media_total) + length(escolas_rs$media_total) 
                                     + length(escolas_sc$media_total))
media_regiao_sul = round(media_regiao_sul, digits = 2)


# Análise bivariada para duas variáveis quantitativas
temp<-data.table(regioes=c("Norte","Centro-Oeste","Nordeste","Sudeste","Sul"),
                 taxa_de_abandono=c(media_regiao_norte,media_regiao_centro_oeste,media_regiao_nordeste
                                     ,media_regiao_sudeste,media_regiao_sul))



ggplot(temp, aes(x=regioes, y=taxa_de_abandono)) +
  geom_bar(stat="identity",fill="steelblue")
#####

# Plotagem de gráficos ####
# Análise bivariada para duas variáveis quantitativas
temp<-data.table(medias=c("AC","AL","AM","AP","BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", "PA",
                          "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"),
                 medias_por_estado=c(soma_ac,soma_al,soma_am, soma_ap, soma_ba, soma_ce, soma_df, soma_es, soma_go
                                     ,soma_ma, soma_mg, soma_ms, soma_mt, soma_pa, soma_pb, soma_pe, soma_pi, soma_pr
                                     ,soma_rj, soma_rn, soma_ro, soma_rr, soma_rs, soma_sc, soma_se, soma_sp, soma_to))



ggplot(temp, aes(x=medias, y=medias_por_estado)) +
  geom_bar(stat="identity",fill="steelblue")
(table(dados$sigla_estado))
#####

#Agrupamento por escolas públicas e privadas#####

escolas_privadas = (enem_2015[tipo_dependecia_administrativa == "Privada"])
escolas_publicas = (enem_2015[tipo_dependecia_administrativa != "Privada"])



mean(escolas_privadas$adequacao_da_formacao_docente)
summary(escolas_privadas$adequacao_da_formacao_docente)


mean(escolas_publicas$adequacao_da_formacao_docente)
summary(escolas_publicas$adequacao_da_formacao_docente)
#####

######Taxa de abandodo#####
#Agrupamento por Região Norte
soma_ac <- sum(escolas_ac$taxa_de_abandono, na.rm=T)
soma_ap <- sum(escolas_ap$taxa_de_abandono, na.rm=T)
soma_am <- sum(escolas_am$taxa_de_abandono, na.rm=T)
soma_pr <- sum(escolas_pr$taxa_de_abandono, na.rm=T)
soma_ro <- sum(escolas_ro$taxa_de_abandono, na.rm=T)
soma_rr <- sum(escolas_rr$taxa_de_abandono, na.rm=T)
soma_to <- sum(escolas_to$taxa_de_abandono, na.rm=T)
soma_regiao_norte <- sum(soma_ac, soma_ap, soma_am, soma_pr, soma_ro, soma_rr, soma_to)
media_regiao_norte <- soma_regiao_norte/(length(escolas_ac$taxa_de_abandono) 
                                         + length(escolas_ap$taxa_de_abandono) + length(escolas_am$taxa_de_abandono) 
                                         + length(escolas_pr$taxa_de_abandono) + length(escolas_ro$taxa_de_abandono) 
                                         + length(escolas_rr$taxa_de_abandono) + length(escolas_to$taxa_de_abandono))
#format remove casas decimais, mas converte variável para charater
#media_regiao_norte <- format(round(media_regiao_norte, 0), nsmall = 0) 
#media_regiao_norte <- round(media_regiao_norte, 0)
media_regiao_norte = round(media_regiao_norte, digits = 2) 

#Agrupamento por Região Centro-Oeste
soma_df <- sum(escolas_df$taxa_de_abandono, na.rm=T)
soma_go <- sum(escolas_go$taxa_de_abandono, na.rm=T)
soma_mt <- sum(escolas_mt$taxa_de_abandono, na.rm=T)
soma_ms <- sum(escolas_ms$taxa_de_abandono, na.rm=T)
soma_regiao_centro_oeste <- sum(soma_go, soma_mt, soma_ms)
media_regiao_centro_oeste <- soma_regiao_centro_oeste/(length(escolas_df$taxa_de_abandono) + length(escolas_go$taxa_de_abandono)
                                                       + length(escolas_mt$taxa_de_abandono) + length(escolas_ms$taxa_de_abandono))
media_regiao_centro_oeste = round(media_regiao_centro_oeste, digits = 2)

#Agrupamento por Região Nordeste
soma_al <- sum(escolas_al$taxa_de_abandono, na.rm=T)
soma_ba <- sum(escolas_ba$taxa_de_abandono, na.rm=T)
soma_ce <- sum(escolas_ce$taxa_de_abandono, na.rm=T)
soma_ma <- sum(escolas_ma$taxa_de_abandono, na.rm=T)
soma_pb <- sum(escolas_pb$taxa_de_abandono, na.rm=T)
soma_pe <- sum(escolas_pe$taxa_de_abandono, na.rm=T)
soma_pi <- sum(escolas_pi$taxa_de_abandono, na.rm=T)
soma_rn <- sum(escolas_rn$taxa_de_abandono, na.rm=T)
soma_se <- sum(escolas_se$taxa_de_abandono, na.rm=T)
soma_regiao_nordeste <- sum(soma_al, soma_ba, soma_ce, soma_ma, soma_pb, soma_pe, soma_pi, soma_rn, soma_se)
media_regiao_nordeste <- soma_regiao_nordeste/(length(escolas_al$taxa_de_abandono) + length(escolas_ba$taxa_de_abandono) 
                                               + length(escolas_ce$taxa_de_abandono) + length(escolas_ma$taxa_de_abandono) 
                                               + length(escolas_pb$taxa_de_abandono) + length(escolas_pe$taxa_de_abandono) 
                                               + length(escolas_pi$taxa_de_abandono) + length(escolas_rn$taxa_de_abandono) 
                                               + length(escolas_se$taxa_de_abandono))
media_regiao_nordeste = round(media_regiao_nordeste, digits = 2) 

#Agrupamento por Região Sudeste
soma_es <- sum(escolas_es$taxa_de_abandono, na.rm=T)
soma_mg <- sum(escolas_mg$taxa_de_abandono, na.rm=T)
soma_rj <- sum(escolas_rj$taxa_de_abandono, na.rm=T)
soma_sp <- sum(escolas_sp$taxa_de_abandono, na.rm=T)
soma_regiao_sudeste <- sum(soma_es, soma_mg, soma_rj, soma_sp)
media_regiao_sudeste <- soma_regiao_sudeste/(length(escolas_es$taxa_de_abandono) + length(escolas_mg$taxa_de_abandono) 
                                             + length(escolas_rj$taxa_de_abandono) + length(escolas_sp$taxa_de_abandono))
media_regiao_sudeste = round(media_regiao_sudeste, digits = 2)

#Agrupamento por Região Sul
soma_pr <- sum(escolas_pr$taxa_de_abandono, na.rm=T)
soma_rs <- sum(escolas_rs$taxa_de_abandono, na.rm=T)
soma_sc <- sum(escolas_sc$taxa_de_abandono, na.rm=T)
soma_regiao_sul <- sum(soma_pr, soma_rs, soma_sc)
media_regiao_sul <- soma_regiao_sul/(length(escolas_pr$taxa_de_abandono) + length(escolas_rs$taxa_de_abandono) + length(escolas_sc$taxa_de_abandono))
media_regiao_sul = round(media_regiao_sul, digits = 2)


# Análise bivariada para duas variáveis quantitativas
temp<-data.table(medias=c("Norte","Centro-Oeste","Nordeste","Sudeste","Sul"),
                 taxa_de_abandono=c(media_regiao_norte,media_regiao_centro_oeste,media_regiao_nordeste
                                     ,media_regiao_sudeste,media_regiao_sul))



ggplot(temp, aes(x=medias, y=taxa_de_abandono)) +
  geom_bar(stat="identity",fill="steelblue")



#####

######Número de matriculados#####
#Agrupamento por Região Norte
matriculados_ac <- sum(escolas_ac$num_matriculas, na.rm=T)
matriculados_ap <- sum(escolas_ap$num_matriculas, na.rm=T)
matriculados_am <- sum(escolas_am$num_matriculas, na.rm=T)
matriculados_pr <- sum(escolas_pr$num_matriculas, na.rm=T)
matriculados_ro <- sum(escolas_ro$num_matriculas, na.rm=T)
matriculados_rr <- sum(escolas_rr$num_matriculas, na.rm=T)
matriculados_to <- sum(escolas_to$num_matriculas, na.rm=T)
soma_matriculados_regiao_norte <- sum(matriculados_ac, matriculados_ap, matriculados_am, matriculados_pr, matriculados_ro, matriculados_rr, matriculados_to)

#Agrupamento por Região Centro-Oeste
matriculados_df <- sum(escolas_df$num_matriculas, na.rm=T)
matriculados_go <- sum(escolas_go$num_matriculas, na.rm=T)
matriculados_mt <- sum(escolas_mt$num_matriculas, na.rm=T)
matriculados_ms <- sum(escolas_ms$num_matriculas, na.rm=T)
soma_matriculados_regiao_centro_oeste <- sum(matriculados_df, matriculados_go, matriculados_mt, matriculados_ms)


#Agrupamento por Região Nordeste
matriculados_al <- sum(escolas_al$num_matriculas, na.rm=T)
matriculados_ba <- sum(escolas_ba$num_matriculas, na.rm=T)
matriculados_ce <- sum(escolas_ce$num_matriculas, na.rm=T)
matriculados_ma <- sum(escolas_ma$num_matriculas, na.rm=T)
matriculados_pb <- sum(escolas_pb$num_matriculas, na.rm=T)
matriculados_pe <- sum(escolas_pe$num_matriculas, na.rm=T)
matriculados_pi <- sum(escolas_pi$num_matriculas, na.rm=T)
matriculados_rn <- sum(escolas_rn$num_matriculas, na.rm=T)
matriculados_se <- sum(escolas_se$num_matriculas, na.rm=T)
soma_matriculados_regiao_nordeste <- sum(matriculados_al, matriculados_ba, matriculados_ce, matriculados_ma, matriculados_pb, matriculados_pe, matriculados_pi, matriculados_rn, matriculados_se)

#Agrupamento por Região Sudeste
matriculados_es <- sum(escolas_es$num_matriculas, na.rm=T)
matriculados_mg <- sum(escolas_mg$num_matriculas, na.rm=T)
matriculados_rj <- sum(escolas_rj$num_matriculas, na.rm=T)
matriculados_sp <- sum(escolas_sp$num_matriculas, na.rm=T)
soma_matriculados_regiao_sudeste <- sum(matriculados_es, matriculados_mg, matriculados_rj, matriculados_sp)

#Agrupamento por Região Sul
matriculados_pr <- sum(escolas_pr$num_matriculas, na.rm=T)
matriculados_rs <- sum(escolas_rs$num_matriculas, na.rm=T)
matriculados_sc <- sum(escolas_sc$num_matriculas, na.rm=T)
soma_matriculados_regiao_sul <- sum(matriculados_pr, matriculados_rs, matriculados_sc)


# Análise bivariada para duas variáveis quantitativas
temp<-data.table(regiao=c("Norte","Centro-Oeste","Nordeste","Sudeste","Sul"),
                 matriculados=c(soma_matriculados_regiao_norte,soma_matriculados_regiao_centro_oeste
                                ,soma_matriculados_regiao_nordeste
                                ,soma_matriculados_regiao_sudeste,soma_matriculados_regiao_sul))



ggplot(temp, aes(x=regiao, y=matriculados)) +
  geom_bar(stat="identity",fill="steelblue")



#####

######frequência localização de escolas#####
#Agrupamento por Região Norte
matriculados_ac <- escolas_ac$localizacao_da_escola
matriculados_ap <- escolas_ap$localizacao_da_escola
matriculados_am <- escolas_am$localizacao_da_escola
matriculados_pr <- escolas_pr$localizacao_da_escola
matriculados_ro <- escolas_ro$localizacao_da_escola
matriculados_rr <- escolas_rr$localizacao_da_escola
matriculados_to <- escolas_to$localizacao_da_escola
soma_matriculados_regiao_norte <- c(matriculados_ac, matriculados_ap, matriculados_am, matriculados_pr, matriculados_ro, matriculados_rr, matriculados_to)

freq = table(soma_matriculados_regiao_norte)
prop = round(prop.table(table(soma_matriculados_regiao_norte))*100,2)
kable(cbind(freq,prop),col.names = c("Freq","Prop"))

#Agrupamento por Região Centro-Oeste
matriculados_df <- escolas_df$localizacao_da_escola
matriculados_go <- escolas_go$localizacao_da_escola
matriculados_mt <- escolas_mt$localizacao_da_escola
matriculados_ms <- escolas_ms$localizacao_da_escola
soma_matriculados_regiao_centro_oeste <- c(matriculados_df, matriculados_go, matriculados_mt, matriculados_ms)

freq = table(soma_matriculados_regiao_centro_oeste)
prop = round(prop.table(table(soma_matriculados_regiao_centro_oeste))*100,2)
kable(cbind(freq,prop),col.names = c("Freq","Prop"))

#Agrupamento por Região Nordeste
matriculados_al <- escolas_al$localizacao_da_escola
matriculados_ba <- escolas_ba$localizacao_da_escola
matriculados_ce <- escolas_ce$localizacao_da_escola
matriculados_ma <- escolas_ma$localizacao_da_escola
matriculados_pb <- escolas_pb$localizacao_da_escola
matriculados_pe <- escolas_pe$localizacao_da_escola
matriculados_pi <- escolas_pi$localizacao_da_escola
matriculados_rn <- escolas_rn$localizacao_da_escola
matriculados_se <- escolas_se$localizacao_da_escola
soma_matriculados_regiao_nordeste <- c(matriculados_al, matriculados_ba, matriculados_ce, matriculados_ma, matriculados_pb, matriculados_pe, matriculados_pi, matriculados_rn, matriculados_se)

freq = table(soma_matriculados_regiao_nordeste)
prop = round(prop.table(table(soma_matriculados_regiao_nordeste))*100,2)
kable(cbind(freq,prop),col.names = c("Freq","Prop"))

#Agrupamento por Região Sudeste
matriculados_es <- escolas_es$localizacao_da_escola
matriculados_mg <- escolas_mg$localizacao_da_escola
matriculados_rj <- escolas_rj$localizacao_da_escola
matriculados_sp <- escolas_sp$localizacao_da_escola
soma_matriculados_regiao_sudeste <- c(matriculados_es, matriculados_mg, matriculados_rj, matriculados_sp)

freq = table(soma_matriculados_regiao_sudeste)
prop = round(prop.table(table(soma_matriculados_regiao_sudeste))*100,2)
kable(cbind(freq,prop),col.names = c("Freq","Prop"))

#Agrupamento por Região Sul
matriculados_pr <- escolas_pr$localizacao_da_escola
matriculados_rs <- escolas_rs$localizacao_da_escola
matriculados_sc <- escolas_sc$localizacao_da_escola
soma_matriculados_regiao_sul <- c(matriculados_pr, matriculados_rs, matriculados_sc)

freq = table(soma_matriculados_regiao_sul)
prop = round(prop.table(table(soma_matriculados_regiao_sul))*100,2)
kable(cbind(freq,prop),col.names = c("Freq","Prop"))




#####

######frequência tipo de dependência administriva#####
#Agrupamento por Região Norte
matriculados_ac <- escolas_ac$tipo_dependecia_administrativa
matriculados_ap <- escolas_ap$tipo_dependecia_administrativa
matriculados_am <- escolas_am$tipo_dependecia_administrativa
matriculados_pr <- escolas_pr$tipo_dependecia_administrativa
matriculados_ro <- escolas_ro$tipo_dependecia_administrativa
matriculados_rr <- escolas_rr$tipo_dependecia_administrativa
matriculados_to <- escolas_to$tipo_dependecia_administrativa
soma_matriculados_regiao_norte <- c(matriculados_ac, matriculados_ap, matriculados_am, matriculados_pr, matriculados_ro, matriculados_rr, matriculados_to)

freq = table(soma_matriculados_regiao_norte)
prop = round(prop.table(table(soma_matriculados_regiao_norte))*100,2)
kable(cbind(freq,prop),col.names = c("Freq","Prop"))

#Agrupamento por Região Centro-Oeste
matriculados_df <- escolas_df$tipo_dependecia_administrativa
matriculados_go <- escolas_go$tipo_dependecia_administrativa
matriculados_mt <- escolas_mt$tipo_dependecia_administrativa
matriculados_ms <- escolas_ms$tipo_dependecia_administrativa
soma_matriculados_regiao_centro_oeste <- c(matriculados_df, matriculados_go, matriculados_mt, matriculados_ms)

freq = table(soma_matriculados_regiao_centro_oeste)
prop = round(prop.table(table(soma_matriculados_regiao_centro_oeste))*100,2)
kable(cbind(freq,prop),col.names = c("Freq","Prop"))

#Agrupamento por Região Nordeste
matriculados_al <- escolas_al$tipo_dependecia_administrativa
matriculados_ba <- escolas_ba$tipo_dependecia_administrativa
matriculados_ce <- escolas_ce$tipo_dependecia_administrativa
matriculados_ma <- escolas_ma$tipo_dependecia_administrativa
matriculados_pb <- escolas_pb$tipo_dependecia_administrativa
matriculados_pe <- escolas_pe$tipo_dependecia_administrativa
matriculados_pi <- escolas_pi$tipo_dependecia_administrativa
matriculados_rn <- escolas_rn$tipo_dependecia_administrativa
matriculados_se <- escolas_se$tipo_dependecia_administrativa
soma_matriculados_regiao_nordeste <- c(matriculados_al, matriculados_ba, matriculados_ce, matriculados_ma, matriculados_pb, matriculados_pe, matriculados_pi, matriculados_rn, matriculados_se)

freq = table(soma_matriculados_regiao_nordeste)
prop = round(prop.table(table(soma_matriculados_regiao_nordeste))*100,2)
kable(cbind(freq,prop),col.names = c("Freq","Prop"))

#Agrupamento por Região Sudeste
matriculados_es <- escolas_es$tipo_dependecia_administrativa
matriculados_mg <- escolas_mg$tipo_dependecia_administrativa
matriculados_rj <- escolas_rj$tipo_dependecia_administrativa
matriculados_sp <- escolas_sp$tipo_dependecia_administrativa
soma_matriculados_regiao_sudeste <- c(matriculados_es, matriculados_mg, matriculados_rj, matriculados_sp)

freq = table(soma_matriculados_regiao_sudeste)
prop = round(prop.table(table(soma_matriculados_regiao_sudeste))*100,2)
kable(cbind(freq,prop),col.names = c("Freq","Prop"))

#Agrupamento por Região Sul
matriculados_pr <- escolas_pr$tipo_dependecia_administrativa
matriculados_rs <- escolas_rs$tipo_dependecia_administrativa
matriculados_sc <- escolas_sc$tipo_dependecia_administrativa
soma_matriculados_regiao_sul <- c(matriculados_pr, matriculados_rs, matriculados_sc)

freq = table(soma_matriculados_regiao_sul)
prop = round(prop.table(table(soma_matriculados_regiao_sul))*100,2)
kable(cbind(freq,prop),col.names = c("Freq","Prop"))




#####

#Agrupamento de escolas por porte escolar####
set.seed(6520)
#Agrupamento por estado Acre

e_1_a_30 = (ano_2015[porte_escolar == 'De 1 a 30 alunos'])
#Escolas entre 1 e 30 alunos
e_1_a_30 <- e_1_a_30[sample(nrow(e_1_a_30), 30), ]
soma_aprovados <- sum(e_1_a_30$taxa_de_aprovacao, na.rm=T)
quant_rows_aprovados <- length(e_1_a_30$taxa_de_aprovacao)
media_e_1_a_30 <- mean(soma_aprovados / quant_rows_aprovados)
media_e_1_a_30 = round(media_e_1_a_30, digits = 2)
mean(e_1_a_30$taxa_de_aprovacao)

e_31_a_60 = (ano_2015[porte_escolar == 'De 31 a 60 alunos'])
#Escolas entre 31 e 60 alunos
e_31_a_60 <- e_31_a_60[sample(nrow(e_31_a_60), 30), ]
soma_aprovados <- sum(e_31_a_60$taxa_de_aprovacao, na.rm=T)
quant_rows_aprovados <- length(e_31_a_60$taxa_de_aprovacao)
media_e_31_a_60 <- mean(soma_aprovados / quant_rows_aprovados)
media_e_31_a_60 = round(media_e_31_a_60, digits = 2)
mean(e_31_a_60$taxa_de_aprovacao)

e_61_a_90 = (ano_2015[porte_escolar == 'De 61 a 90 alunos'])
#Escolas entre 61 e 90 alunos
e_61_a_90 <- e_61_a_90[sample(nrow(e_61_a_90[porte_escolar %in% 'De 61 a 90 alunos']), 30), ]
soma_aprovados <- sum(e_61_a_90$taxa_de_aprovacao, na.rm=T)
quant_rows_aprovados <- length(e_61_a_90$taxa_de_aprovacao)
media_e_61_a_90 <- mean(soma_aprovados / quant_rows_aprovados)
media_e_61_a_90 = round(media_e_61_a_90, digits = 2)
mean(e_61_a_90$taxa_de_aprovacao)

e_maior_90 = (ano_2015[porte_escolar == 'Maior que 90 alunos'])
#Escolas acima de 90 alunos
e_maior_90 <- e_maior_90[sample(nrow(e_maior_90[porte_escolar %in% 'Maior que 90 alunos']), 30), ]
soma_aprovados <- sum(e_maior_90$taxa_de_aprovacao, na.rm=T)
quant_rows_aprovados <- length(e_maior_90$taxa_de_aprovacao)
media_maior_90 <- mean(soma_aprovados / quant_rows_aprovados)
media_maior_90 = round(media_maior_90, digits = 2)
mean(e_maior_90$taxa_de_aprovacao)

# Análise bivariada para duas variáveis quantitativas
temp<-data.table(porte=c("De 1 a 30 alunos","De 31 a 60 alunos","De 61 a 90 alunos","Maior que 90 alunos"),
                 taxas=c(media_e_1_a_30,media_e_31_a_60
                                    ,media_e_61_a_90,media_maior_90))

ggplot(temp, aes(x=porte, y=taxas)) +
  geom_bar(stat="identity",fill="steelblue") + 
  labs(x="Porte", y="Taxas", title="Taxas de aprovação") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.3))

table(dados$porte_escolar)
#####

#####Médias Totais por estado####
media_total_estado_ac = mean(escolas_ac$media_total, na.rm=T)
media_total_estado_ac = media_total_estado_ac/length(escolas_ac$media_total)

media_total_estado_al = sum(escolas_al$media_total, na.rm=T)
media_total_estado_al = media_total_estado_al/length(escolas_al$media_total)

media_total_estado_ap = sum(escolas_ap$media_total, na.rm=T)
media_total_estado_ap = media_total_estado_ap/length(escolas_ap$media_total)

media_total_estado_ba = sum(escolas_ba$media_total, na.rm=T)
media_total_estado_ba = media_total_estado_ba/length(escolas_ba$media_total)

media_total_estado_ce = sum(escolas_ce$media_total, na.rm=T)
media_total_estado_ce = media_total_estado_ce/length(escolas_ce$media_total)

media_total_estado_df = sum(escolas_df$media_total, na.rm=T)
media_total_estado_df = media_total_estado_df/length(escolas_df$media_total)

media_total_estado_es = sum(escolas_es$media_total, na.rm=T)
media_total_estado_es = media_total_estado_es/length(escolas_es$media_total)

media_total_estado_go = sum(escolas_go$media_total, na.rm=T)
media_total_estado_go = media_total_estado_go/length(escolas_go$media_total)

media_total_estado_ma = sum(escolas_ma$media_total, na.rm=T)
media_total_estado_ma = media_total_estado_ma/length(escolas_ma$media_total)

media_total_estado_mt = sum(escolas_mt$media_total, na.rm=T)
media_total_estado_mt = media_total_estado_mt/length(escolas_mt$media_total)

media_total_estado_ms = sum(escolas_ms$media_total, na.rm=T)
media_total_estado_ms = media_total_estado_ms/length(escolas_ms$media_total)

media_total_estado_mg = sum(escolas_mg$media_total, na.rm=T)
media_total_estado_mg = media_total_estado_mg/length(escolas_mg$media_total)

media_total_estado_pa = sum(escolas_pa$media_total, na.rm=T)
media_total_estado_pa = media_total_estado_pa/length(escolas_pa$media_total)

media_total_estado_pb = sum(escolas_pb$media_total, na.rm=T)
media_total_estado_pb = media_total_estado_pb/length(escolas_pb$media_total)

media_total_estado_pr = sum(escolas_pr$media_total, na.rm=T)
media_total_estado_pr = media_total_estado_pr/length(escolas_pr$media_total)

media_total_estado_pe = sum(escolas_pe$media_total, na.rm=T)
media_total_estado_pe = media_total_estado_pe/length(escolas_pe$media_total)

media_total_estado_pi = sum(escolas_pi$media_total, na.rm=T)
media_total_estado_pi = media_total_estado_pi/length(escolas_pi$media_total)

media_total_estado_rj = sum(escolas_rj$media_total, na.rm=T)
media_total_estado_rj = media_total_estado_rj/length(escolas_rj$media_total)

media_total_estado_rn = sum(escolas_rn$media_total, na.rm=T)
media_total_estado_rn = media_total_estado_rn/length(escolas_rn$media_total)

media_total_estado_rs = sum(escolas_rs$media_total, na.rm=T)
media_total_estado_rs = media_total_estado_rs/length(escolas_rs$media_total)

media_total_estado_ro = sum(escolas_ro$media_total, na.rm=T)
media_total_estado_ro = media_total_estado_ro/length(escolas_ro$media_total)

media_total_estado_rr = sum(escolas_rr$media_total, na.rm=T)
media_total_estado_rr = media_total_estado_rr/length(escolas_rr$media_total)

media_total_estado_sc = sum(escolas_sc$media_total, na.rm=T)
media_total_estado_sc = media_total_estado_sc/length(escolas_sc$media_total)

media_total_estado_sp = sum(escolas_sp$media_total, na.rm=T)
media_total_estado_sp = media_total_estado_sp/length(escolas_sp$media_total)

media_total_estado_se = sum(escolas_se$media_total, na.rm=T)
media_total_estado_se = media_total_estado_se/length(escolas_se$media_total)

media_total_estado_to = sum(escolas_to$media_total, na.rm=T)
media_total_estado_to = media_total_estado_se/length(escolas_to$media_total)


# Análise bivariada para duas variáveis quantitativas
temp<-data.table(sigla=c("AC","AL","AP","BA","CE", "ano_2013", "ES", "GO", "MA", "MT", "MS", "MG", "PA",
                         "PB", "PR", "PE", "PI", "RJ", "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
                 medias_por_estado=c(media_total_estado_ac, media_total_estado_al, media_total_estado_ap
                                     ,media_total_estado_ba, media_total_estado_ce, media_total_estado_df
                                     ,media_total_estado_es ,media_total_estado_go ,media_total_estado_ma
                                     ,media_total_estado_mt ,media_total_estado_ms ,media_total_estado_mg
                                     ,media_total_estado_pa ,media_total_estado_pb ,media_total_estado_pr
                                     ,media_total_estado_pe ,media_total_estado_pi ,media_total_estado_rj
                                     ,media_total_estado_rs ,media_total_estado_ro ,media_total_estado_rr
                                     ,media_total_estado_sc ,media_total_estado_sp ,media_total_estado_se
                                     ,media_total_estado_to)
)

ggplot(temp, aes(x=sigla, y=medias_por_estado)) +
  geom_bar(stat="identity",fill="steelblue")

escolas_ac[,min(media_total), by=data.table(media_total, num_escola_educacenso)]
(table(ano_2013$sigla_estado))
#####

######Menores médias por estado#####
str(dados)
escola_ac = sqldf("SELECT num_escola_educacenso, max(media_total), tipo_dependecia_administrativa  from escolas_ac")
escola_al = sqldf("SELECT num_escola_educacenso, max(media_total), tipo_dependecia_administrativa  from escolas_al")
escola_am = sqldf("SELECT num_escola_educacenso, max(media_total), tipo_dependecia_administrativa  from escolas_am")
escola_ap = sqldf("SELECT num_escola_educacenso, max(media_total), tipo_dependecia_administrativa  from escolas_ap")
escola_ba = sqldf("SELECT num_escola_educacenso, max(media_total), tipo_dependecia_administrativa  from escolas_ba")
escola_ce = sqldf("SELECT num_escola_educacenso, max(media_total), tipo_dependecia_administrativa  from escolas_ce")
escola_df = sqldf("SELECT num_escola_educacenso, max(media_total), tipo_dependecia_administrativa  from escolas_df")
escola_es = sqldf("SELECT num_escola_educacenso, max(media_total), tipo_dependecia_administrativa  from escolas_es")
escola_go = sqldf("SELECT num_escola_educacenso, max(media_total), tipo_dependecia_administrativa  from escolas_go")
escola_ma = sqldf("SELECT num_escola_educacenso, max(media_total), tipo_dependecia_administrativa  from escolas_ma")
escola_mg = sqldf("SELECT num_escola_educacenso, max(media_total), tipo_dependecia_administrativa  from escolas_mg")
escola_ms = sqldf("SELECT num_escola_educacenso, max(media_total), tipo_dependecia_administrativa  from escolas_ms")
escola_mt = sqldf("SELECT num_escola_educacenso, max(media_total), tipo_dependecia_administrativa  from escolas_mt")
escola_pa = sqldf("SELECT num_escola_educacenso, max(media_total), tipo_dependecia_administrativa  from escolas_pa")
escola_pb = sqldf("SELECT num_escola_educacenso, max(media_total), tipo_dependecia_administrativa  from escolas_pb")
escola_pe = sqldf("SELECT num_escola_educacenso, max(media_total), tipo_dependecia_administrativa  from escolas_pe")
escola_pi = sqldf("SELECT num_escola_educacenso, max(media_total), tipo_dependecia_administrativa  from escolas_pi")
escola_pr = sqldf("SELECT num_escola_educacenso, max(media_total), tipo_dependecia_administrativa  from escolas_pr")
escola_rj = sqldf("SELECT num_escola_educacenso, max(media_total), tipo_dependecia_administrativa  from escolas_rj")
escola_rn = sqldf("SELECT num_escola_educacenso, max(media_total), tipo_dependecia_administrativa  from escolas_rn")
escola_ro = sqldf("SELECT num_escola_educacenso, max(media_total), tipo_dependecia_administrativa  from escolas_ro")
escola_rr = sqldf("SELECT num_escola_educacenso, max(media_total), tipo_dependecia_administrativa  from escolas_rr")
escola_rs = sqldf("SELECT num_escola_educacenso, max(media_total), tipo_dependecia_administrativa  from escolas_rs")
escola_sc = sqldf("SELECT num_escola_educacenso, max(media_total), tipo_dependecia_administrativa  from escolas_sc")
escola_se = sqldf("SELECT num_escola_educacenso, max(media_total), tipo_dependecia_administrativa  from escolas_se")
escola_sp = sqldf("SELECT num_escola_educacenso, max(media_total), tipo_dependecia_administrativa  from escolas_sp")
escola_to = sqldf("SELECT num_escola_educacenso, max(media_total), tipo_dependecia_administrativa  from escolas_to")

# Análise bivariada para duas variáveis quantitativas
temp<-data.table(escola=c(  escola_ac$num_escola_educacenso, escola_al$num_escola_educacenso
                          , escola_am$num_escola_educacenso, escola_ap$num_escola_educacenso
                          , escola_ba$num_escola_educacenso, escola_ce$num_escola_educacenso
                          , escola_df$num_escola_educacenso, escola_es$num_escola_educacenso
                          , escola_go$num_escola_educacenso, escola_ma$num_escola_educacenso
                          , escola_mg$num_escola_educacenso, escola_ms$num_escola_educacenso
                          , escola_mt$num_escola_educacenso, escola_pa$num_escola_educacenso
                          , escola_pb$num_escola_educacenso, escola_pe$num_escola_educacenso
                          , escola_pi$num_escola_educacenso, escola_pr$num_escola_educacenso
                          , escola_rj$num_escola_educacenso, escola_rn$num_escola_educacenso
                          , escola_ro$num_escola_educacenso, escola_rr$num_escola_educacenso
                          , escola_rs$num_escola_educacenso, escola_sc$num_escola_educacenso
                          , escola_se$num_escola_educacenso, escola_sp$num_escola_educacenso
                          , escola_to$num_escola_educacenso),
                         notas=c(  escola_ac$`max(media_total)`, escola_al$`max(media_total)`, escola_am$`max(media_total)`
                                 , escola_ap$`max(media_total)`, escola_ba$`max(media_total)`, escola_ce$`max(media_total)`
                                 , escola_df$`max(media_total)`, escola_es$`max(media_total)`, escola_go$`max(media_total)`
                                 , escola_ma$`max(media_total)`, escola_mg$`max(media_total)`, escola_ms$`max(media_total)`
                                 , escola_mt$`max(media_total)`, escola_pa$`max(media_total)`, escola_pb$`max(media_total)`
                                 , escola_pe$`max(media_total)`, escola_pi$`max(media_total)`, escola_pr$`max(media_total)`
                                 , escola_rj$`max(media_total)`, escola_rn$`max(media_total)`, escola_ro$`max(media_total)`
                                 , escola_rr$`max(media_total)`, escola_rs$`max(media_total)`, escola_sc$`max(media_total)`
                                 , escola_se$`max(media_total)`, escola_sp$`max(media_total)`, escola_to$`max(media_total)`
                          )
)

ggplot(temp, aes(x=escola, y=notas)) +
  geom_bar(stat="identity",fill="steelblue") + 
  labs(x="Escola", y="Média", title="Maiores médias") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.3))
#####

######taxas de aprovação por estado#####

escola_ac = sqldf("SELECT sigla_estado, max(taxa_de_aprovacao), min(taxa_de_aprovacao), avg(taxa_de_aprovacao) from escolas_ac")
escola_al = sqldf("SELECT sigla_estado, max(taxa_de_aprovacao), min(taxa_de_aprovacao), avg(taxa_de_aprovacao) from escolas_al")
escola_am = sqldf("SELECT sigla_estado, max(taxa_de_aprovacao), min(taxa_de_aprovacao), avg(taxa_de_aprovacao) from escolas_am")
escola_ap = sqldf("SELECT sigla_estado, max(taxa_de_aprovacao), min(taxa_de_aprovacao), avg(taxa_de_aprovacao) from escolas_ap")
escola_ba = sqldf("SELECT sigla_estado, max(taxa_de_aprovacao), min(taxa_de_aprovacao), avg(taxa_de_aprovacao) from escolas_ba")
escola_ce = sqldf("SELECT sigla_estado, max(taxa_de_aprovacao), min(taxa_de_aprovacao), avg(taxa_de_aprovacao) from escolas_ce")
escola_df = sqldf("SELECT sigla_estado, max(taxa_de_aprovacao), min(taxa_de_aprovacao), avg(taxa_de_aprovacao) from escolas_df")
escola_es = sqldf("SELECT sigla_estado, max(taxa_de_aprovacao), min(taxa_de_aprovacao), avg(taxa_de_aprovacao) from escolas_es")
escola_go = sqldf("SELECT sigla_estado, max(taxa_de_aprovacao), min(taxa_de_aprovacao), avg(taxa_de_aprovacao) from escolas_go")
escola_ma = sqldf("SELECT sigla_estado, max(taxa_de_aprovacao), min(taxa_de_aprovacao), avg(taxa_de_aprovacao) from escolas_ma")
escola_mg = sqldf("SELECT sigla_estado, max(taxa_de_aprovacao), min(taxa_de_aprovacao), avg(taxa_de_aprovacao) from escolas_mg")
escola_ms = sqldf("SELECT sigla_estado, max(taxa_de_aprovacao), min(taxa_de_aprovacao), avg(taxa_de_aprovacao) from escolas_ms")
escola_mt = sqldf("SELECT sigla_estado, max(taxa_de_aprovacao), min(taxa_de_aprovacao), avg(taxa_de_aprovacao) from escolas_mt")
escola_pa = sqldf("SELECT sigla_estado, max(taxa_de_aprovacao), min(taxa_de_aprovacao), avg(taxa_de_aprovacao) from escolas_pa")
escola_pb = sqldf("SELECT sigla_estado, max(taxa_de_aprovacao), min(taxa_de_aprovacao), avg(taxa_de_aprovacao) from escolas_pb")
escola_pe = sqldf("SELECT sigla_estado, max(taxa_de_aprovacao), min(taxa_de_aprovacao), avg(taxa_de_aprovacao) from escolas_pe")
escola_pi = sqldf("SELECT sigla_estado, max(taxa_de_aprovacao), min(taxa_de_aprovacao), avg(taxa_de_aprovacao) from escolas_pi")
escola_pr = sqldf("SELECT sigla_estado, max(taxa_de_aprovacao), min(taxa_de_aprovacao), avg(taxa_de_aprovacao) from escolas_pr")
escola_rj = sqldf("SELECT sigla_estado, max(taxa_de_aprovacao), min(taxa_de_aprovacao), avg(taxa_de_aprovacao) from escolas_rj")
escola_rn = sqldf("SELECT sigla_estado, max(taxa_de_aprovacao), min(taxa_de_aprovacao), avg(taxa_de_aprovacao) from escolas_rn")
escola_ro = sqldf("SELECT sigla_estado, max(taxa_de_aprovacao), min(taxa_de_aprovacao), avg(taxa_de_aprovacao) from escolas_ro")
escola_rr = sqldf("SELECT sigla_estado, max(taxa_de_aprovacao), min(taxa_de_aprovacao), avg(taxa_de_aprovacao) from escolas_rr")
escola_rs = sqldf("SELECT sigla_estado, max(taxa_de_aprovacao), min(taxa_de_aprovacao), avg(taxa_de_aprovacao) from escolas_rs")
escola_sc = sqldf("SELECT sigla_estado, max(taxa_de_aprovacao), min(taxa_de_aprovacao), avg(taxa_de_aprovacao) from escolas_sc")
escola_se = sqldf("SELECT sigla_estado, max(taxa_de_aprovacao), min(taxa_de_aprovacao), avg(taxa_de_aprovacao) from escolas_se")
escola_sp = sqldf("SELECT sigla_estado, max(taxa_de_aprovacao), min(taxa_de_aprovacao), avg(taxa_de_aprovacao) from escolas_sp")
escola_to = sqldf("SELECT sigla_estado, max(taxa_de_aprovacao), min(taxa_de_aprovacao), avg(taxa_de_aprovacao) from escolas_to")

# Análise bivariada para duas variáveis quantitativas
temp<-data.table(estados=c(  escola_ac$sigla_estado, escola_al$sigla_estado
                            , escola_am$sigla_estado, escola_ap$sigla_estado
                            , escola_ba$sigla_estado, escola_ce$sigla_estado
                            , escola_df$sigla_estado, escola_es$sigla_estado
                            , escola_go$sigla_estado, escola_ma$sigla_estado
                            , escola_mg$sigla_estado, escola_ms$sigla_estado
                            , escola_mt$sigla_estado, escola_pa$sigla_estado
                            , escola_pb$sigla_estado, escola_pe$sigla_estado
                            , escola_pi$sigla_estado, escola_pr$sigla_estado
                            , escola_rj$sigla_estado, escola_rn$sigla_estado
                            , escola_ro$sigla_estado, escola_rr$sigla_estado
                            , escola_rs$sigla_estado, escola_sc$sigla_estado
                            , escola_se$sigla_estado, escola_sp$sigla_estado
                            , escola_to$sigla_estado),
                 taxas=c(escola_ac$`avg(taxa_de_aprovacao)`, escola_al$`avg(taxa_de_aprovacao)`, escola_am$`avg(taxa_de_aprovacao)`
                         , escola_ap$`avg(taxa_de_aprovacao)`, escola_ba$`avg(taxa_de_aprovacao)`, escola_ce$`avg(taxa_de_aprovacao)`
                         , escola_df$`avg(taxa_de_aprovacao)`, escola_es$`avg(taxa_de_aprovacao)`, escola_go$`avg(taxa_de_aprovacao)`
                         , escola_ma$`avg(taxa_de_aprovacao)`, escola_mg$`avg(taxa_de_aprovacao)`, escola_ms$`avg(taxa_de_aprovacao)`
                         , escola_mt$`avg(taxa_de_aprovacao)`, escola_pa$`avg(taxa_de_aprovacao)`, escola_pb$`avg(taxa_de_aprovacao)`
                         , escola_pe$`avg(taxa_de_aprovacao)`, escola_pi$`avg(taxa_de_aprovacao)`, escola_pr$`avg(taxa_de_aprovacao)`
                         , escola_rj$`avg(taxa_de_aprovacao)`, escola_rn$`avg(taxa_de_aprovacao)`, escola_ro$`avg(taxa_de_aprovacao)`
                         , escola_rr$`avg(taxa_de_aprovacao)`, escola_rs$`avg(taxa_de_aprovacao)`, escola_sc$`avg(taxa_de_aprovacao)`
                         , escola_se$`avg(taxa_de_aprovacao)`, escola_sp$`avg(taxa_de_aprovacao)`, escola_to$`avg(taxa_de_aprovacao)`)
)

summary(taxas)
sd(taxas)
var(taxas)

ggplot(temp, aes(x=estados, y=taxas)) +
  geom_bar(stat="identity",fill="steelblue") + 
  labs(x="Porte", y="Taxas", title="Taxas de Aprovação")
#####

######menores taxas de reprovação por escola#####
escola_ac = sqldf("SELECT num_municipio, max(taxa_de_reprovacao), min(taxa_de_reprovacao) from escolas_ac")
escola_al = sqldf("SELECT num_municipio, max(taxa_de_reprovacao), min(taxa_de_reprovacao) from escolas_al")
escola_am = sqldf("SELECT num_municipio, max(taxa_de_reprovacao), min(taxa_de_reprovacao) from escolas_am")
escola_ap = sqldf("SELECT num_municipio, max(taxa_de_reprovacao), min(taxa_de_reprovacao) from escolas_ap")
escola_ba = sqldf("SELECT num_municipio, max(taxa_de_reprovacao), min(taxa_de_reprovacao) from escolas_ba")
escola_ce = sqldf("SELECT num_municipio, max(taxa_de_reprovacao), min(taxa_de_reprovacao) from escolas_ce")
escola_df = sqldf("SELECT num_municipio, max(taxa_de_reprovacao), min(taxa_de_reprovacao) from escolas_df")
escola_es = sqldf("SELECT num_municipio, max(taxa_de_reprovacao), min(taxa_de_reprovacao) from escolas_es")
escola_go = sqldf("SELECT num_municipio, max(taxa_de_reprovacao), min(taxa_de_reprovacao) from escolas_go")
escola_ma = sqldf("SELECT num_municipio, max(taxa_de_reprovacao), min(taxa_de_reprovacao) from escolas_ma")
escola_mg = sqldf("SELECT num_municipio, max(taxa_de_reprovacao), min(taxa_de_reprovacao) from escolas_mg")
escola_ms = sqldf("SELECT num_municipio, max(taxa_de_reprovacao), min(taxa_de_reprovacao) from escolas_ms")
escola_mt = sqldf("SELECT num_municipio, max(taxa_de_reprovacao), min(taxa_de_reprovacao) from escolas_mt")
escola_pa = sqldf("SELECT num_municipio, max(taxa_de_reprovacao), min(taxa_de_reprovacao) from escolas_pa")
escola_pb = sqldf("SELECT num_municipio, max(taxa_de_reprovacao), min(taxa_de_reprovacao) from escolas_pb")
escola_pe = sqldf("SELECT num_municipio, max(taxa_de_reprovacao), min(taxa_de_reprovacao) from escolas_pe")
escola_pi = sqldf("SELECT num_municipio, max(taxa_de_reprovacao), min(taxa_de_reprovacao) from escolas_pi")
escola_pr = sqldf("SELECT num_municipio, max(taxa_de_reprovacao), min(taxa_de_reprovacao) from escolas_pr")
escola_rj = sqldf("SELECT num_municipio, max(taxa_de_reprovacao), min(taxa_de_reprovacao) from escolas_rj")
escola_rn = sqldf("SELECT num_municipio, max(taxa_de_reprovacao), min(taxa_de_reprovacao) from escolas_rn")
escola_ro = sqldf("SELECT num_municipio, max(taxa_de_reprovacao), min(taxa_de_reprovacao) from escolas_ro")
escola_rr = sqldf("SELECT num_municipio, max(taxa_de_reprovacao), min(taxa_de_reprovacao) from escolas_rr")
escola_rs = sqldf("SELECT num_municipio, max(taxa_de_reprovacao), min(taxa_de_reprovacao) from escolas_rs")
escola_sc = sqldf("SELECT num_municipio, max(taxa_de_reprovacao), min(taxa_de_reprovacao) from escolas_sc")
escola_se = sqldf("SELECT num_municipio, max(taxa_de_reprovacao), min(taxa_de_reprovacao) from escolas_se")
escola_sp = sqldf("SELECT num_municipio, max(taxa_de_reprovacao), min(taxa_de_reprovacao) from escolas_sp")
escola_to = sqldf("SELECT num_municipio, max(taxa_de_reprovacao), min(taxa_de_reprovacao) from escolas_to")

# Análise bivariada para duas variáveis quantitativas
temp<-data.table(cidade=c(  escola_ac$num_municipio, escola_al$num_municipio
                          , escola_am$num_municipio, escola_ap$num_municipio
                          , escola_ba$num_municipio, escola_ce$num_municipio
                          , escola_df$num_municipio, escola_es$num_municipio
                          , escola_go$num_municipio, escola_ma$num_municipio
                          , escola_mg$num_municipio, escola_ms$num_municipio
                          , escola_mt$num_municipio, escola_pa$num_municipio
                          , escola_pb$num_municipio, escola_pe$num_municipio
                          , escola_pi$num_municipio, escola_pr$num_municipio
                          , escola_rj$num_municipio, escola_rn$num_municipio
                          , escola_ro$num_municipio, escola_rr$num_municipio
                          , escola_rs$num_municipio, escola_sc$num_municipio
                          , escola_se$num_municipio, escola_sp$num_municipio
                          , escola_to$num_municipio),
                 taxas=c(escola_ac$`max(taxa_de_reprovacao)`, escola_al$`max(taxa_de_reprovacao)`, escola_am$`max(taxa_de_reprovacao)`
                                 , escola_ap$`max(taxa_de_reprovacao)`, escola_ba$`max(taxa_de_reprovacao)`, escola_ce$`max(taxa_de_reprovacao)`
                                 , escola_df$`max(taxa_de_reprovacao)`, escola_es$`max(taxa_de_reprovacao)`, escola_go$`max(taxa_de_reprovacao)`
                                 , escola_ma$`max(taxa_de_reprovacao)`, escola_mg$`max(taxa_de_reprovacao)`, escola_ms$`max(taxa_de_reprovacao)`
                                 , escola_mt$`max(taxa_de_reprovacao)`, escola_pa$`max(taxa_de_reprovacao)`, escola_pb$`max(taxa_de_reprovacao)`
                                 , escola_pe$`max(taxa_de_reprovacao)`, escola_pi$`max(taxa_de_reprovacao)`, escola_pr$`max(taxa_de_reprovacao)`
                                 , escola_rj$`max(taxa_de_reprovacao)`, escola_rn$`max(taxa_de_reprovacao)`, escola_ro$`max(taxa_de_reprovacao)`
                                 , escola_rr$`max(taxa_de_reprovacao)`, escola_rs$`max(taxa_de_reprovacao)`, escola_sc$`max(taxa_de_reprovacao)`
                                 , escola_se$`max(taxa_de_reprovacao)`, escola_sp$`max(taxa_de_reprovacao)`, escola_to$`max(taxa_de_reprovacao)`)
)

ggplot(temp, aes(x=cidade, y=taxas)) +
  geom_bar(stat="identity",fill="steelblue") + 
  labs(x="Porte", y="Taxas", title="Taxas de Reprovação") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.3))
#####

######Menores e maiores taxas de reprovação por estado#####
View(table(dados$num_municipio))
View(table(dados$tipo_dependecia_administrativa))
View(table(dados$localizacao_da_escola))
View(table(dados$taxa_de_abandono))



head(dados[ano == "2015"])
#Maiores taxas de reprovação
escola_ac_medias = sqldf("SELECT sigla_estado, media_ciencias_da_natureza from escolas_ac")
escola_al_medias = sqldf("SELECT sigla_estado, avg(media_ciencias_da_natureza), avg(media_ciencias_humanas), avg(media_lingua_portuguesa), avg(media_matematica), avg(media_redacao) from escolas_al")
escola_am_medias = sqldf("SELECT sigla_estado, avg(media_ciencias_da_natureza), avg(media_ciencias_humanas), avg(media_lingua_portuguesa), avg(media_matematica), avg(media_redacao) from escolas_am")
escola_ap_medias = sqldf("SELECT sigla_estado, avg(media_ciencias_da_natureza), avg(media_ciencias_humanas), avg(media_lingua_portuguesa), avg(media_matematica), avg(media_redacao) from escolas_ap")
escola_ba_medias = sqldf("SELECT sigla_estado, avg(media_ciencias_da_natureza), avg(media_ciencias_humanas), avg(media_lingua_portuguesa), avg(media_matematica), avg(media_redacao) from escolas_ba")
escola_ce_medias = sqldf("SELECT sigla_estado, avg(media_ciencias_da_natureza), avg(media_ciencias_humanas), avg(media_lingua_portuguesa), avg(media_matematica), avg(media_redacao) from escolas_ce")
escola_df_medias = sqldf("SELECT sigla_estado, avg(media_ciencias_da_natureza), avg(media_ciencias_humanas), avg(media_lingua_portuguesa), avg(media_matematica), avg(media_redacao) from escolas_df")
escola_es_medias = sqldf("SELECT sigla_estado, avg(media_ciencias_da_natureza), avg(media_ciencias_humanas), avg(media_lingua_portuguesa), avg(media_matematica), avg(media_redacao) from escolas_es")
escola_go_medias = sqldf("SELECT sigla_estado, avg(media_ciencias_da_natureza), avg(media_ciencias_humanas), avg(media_lingua_portuguesa), avg(media_matematica), avg(media_redacao) from escolas_go")
escola_ma_medias = sqldf("SELECT sigla_estado, avg(media_ciencias_da_natureza), avg(media_ciencias_humanas), avg(media_lingua_portuguesa), avg(media_matematica), avg(media_redacao) from escolas_ma")
escola_mg_medias = sqldf("SELECT sigla_estado, avg(media_ciencias_da_natureza), avg(media_ciencias_humanas), avg(media_lingua_portuguesa), avg(media_matematica), avg(media_redacao) from escolas_mg")
escola_ms_medias = sqldf("SELECT sigla_estado, avg(media_ciencias_da_natureza), avg(media_ciencias_humanas), avg(media_lingua_portuguesa), avg(media_matematica), avg(media_redacao) from escolas_ms")
escola_mt_medias = sqldf("SELECT sigla_estado, avg(media_ciencias_da_natureza), avg(media_ciencias_humanas), avg(media_lingua_portuguesa), avg(media_matematica), avg(media_redacao) from escolas_mt")
escola_pa_medias = sqldf("SELECT sigla_estado, avg(media_ciencias_da_natureza), avg(media_ciencias_humanas), avg(media_lingua_portuguesa), avg(media_matematica), avg(media_redacao) from escolas_pa")
escola_pb_medias = sqldf("SELECT sigla_estado, avg(media_ciencias_da_natureza), avg(media_ciencias_humanas), avg(media_lingua_portuguesa), avg(media_matematica), avg(media_redacao) from escolas_pb")
escola_pe_medias = sqldf("SELECT sigla_estado, avg(media_ciencias_da_natureza), avg(media_ciencias_humanas), avg(media_lingua_portuguesa), avg(media_matematica), avg(media_redacao) from escolas_pe")
escola_pi_medias = sqldf("SELECT sigla_estado, avg(media_ciencias_da_natureza), avg(media_ciencias_humanas), avg(media_lingua_portuguesa), avg(media_matematica), avg(media_redacao) from escolas_pi")
escola_pr_medias = sqldf("SELECT sigla_estado, avg(media_ciencias_da_natureza), avg(media_ciencias_humanas), avg(media_lingua_portuguesa), avg(media_matematica), avg(media_redacao) from escolas_pr")
escola_rj_medias = sqldf("SELECT sigla_estado, avg(media_ciencias_da_natureza), avg(media_ciencias_humanas), avg(media_lingua_portuguesa), avg(media_matematica), avg(media_redacao) from escolas_rj")
escola_rn_medias = sqldf("SELECT sigla_estado, avg(media_ciencias_da_natureza), avg(media_ciencias_humanas), avg(media_lingua_portuguesa), avg(media_matematica), avg(media_redacao) from escolas_rn")
escola_ro_medias = sqldf("SELECT sigla_estado, avg(media_ciencias_da_natureza), avg(media_ciencias_humanas), avg(media_lingua_portuguesa), avg(media_redacao) from escolas_ro")
escola_rr_medias = sqldf("SELECT sigla_estado, avg(media_ciencias_da_natureza), avg(media_ciencias_humanas), avg(media_lingua_portuguesa), avg(media_matematica), avg(media_redacao) from escolas_rr")
escola_rs_medias = sqldf("SELECT sigla_estado, avg(media_ciencias_da_natureza), avg(media_ciencias_humanas), avg(media_lingua_portuguesa), avg(media_matematica), avg(media_redacao) from escolas_rs")
escola_sc_medias = sqldf("SELECT sigla_estado, avg(media_ciencias_humanas), avg(media_lingua_portuguesa), avg(media_matematica), avg(media_redacao) from escolas_sc")
escola_se_medias = sqldf("SELECT sigla_estado, avg(media_ciencias_da_natureza), avg(media_ciencias_humanas), avg(media_lingua_portuguesa), avg(media_matematica), avg(media_redacao) from escolas_se")
escola_sp_medias = sqldf("SELECT sigla_estado, avg(media_ciencias_da_natureza), avg(media_ciencias_humanas), avg(media_lingua_portuguesa), avg(media_matematica), avg(media_redacao) from escolas_sp")
escola_to_medias = sqldf("SELECT sigla_estado, avg(media_ciencias_da_natureza), avg(media_ciencias_humanas), avg(media_lingua_portuguesa), avg(media_matematica), avg(media_redacao) from escolas_to")

ciencias_da_natureza = c(escola_ac_medias$`avg(media_ciencias_da_natureza)`, escola_al_medias$`avg(media_ciencias_da_natureza)`, escola_am_medias$`avg(media_ciencias_da_natureza)`
                         , escola_ap_medias$`avg(media_ciencias_da_natureza)`, escola_ba_medias$`avg(media_ciencias_da_natureza)`, escola_ce_medias$`avg(media_ciencias_da_natureza)`
                         , escola_df_medias$`avg(media_ciencias_da_natureza)`, escola_es_medias$`avg(media_ciencias_da_natureza)`, escola_go_medias$`avg(media_ciencias_da_natureza)`
                         , escola_ma_medias$`avg(media_ciencias_da_natureza)`, escola_mg_medias$`avg(media_ciencias_da_natureza)`, escola_ms_medias$`avg(media_ciencias_da_natureza)`
                         , escola_mt_medias$`avg(media_ciencias_da_natureza)`, escola_pa_medias$`avg(media_ciencias_da_natureza)`, escola_pb_medias$`avg(media_ciencias_da_natureza)`
                         , escola_pe_medias$`avg(media_ciencias_da_natureza)`, escola_pi_medias$`avg(media_ciencias_da_natureza)`, escola_pr_medias$`avg(media_ciencias_da_natureza)`
                         , escola_rj_medias$`avg(media_ciencias_da_natureza)`, escola_rn_medias$`avg(media_ciencias_da_natureza)`, escola_ro_medias$`avg(media_ciencias_da_natureza)`
                         , escola_rr_medias$`avg(media_ciencias_da_natureza)`, escola_rs_medias$`avg(media_ciencias_da_natureza)`, escola_sc_medias$`avg(media_ciencias_da_natureza)`
                         , escola_se_medias$`avg(media_ciencias_da_natureza)`, escola_sp_medias$`avg(media_ciencias_da_natureza)`, escola_to_medias$`avg(media_ciencias_da_natureza)`)
ciencias_da_natureza = round(ciencias_da_natureza, digits = 2)

ciencias_humanas = c(escola_ac_medias$`avg(media_ciencias_humanas)`, escola_al_medias$`avg(media_ciencias_humanas)`, escola_am_medias$`avg(media_ciencias_humanas)`
                         , escola_ap_medias$`avg(media_ciencias_humanas)`, escola_ba_medias$`avg(media_ciencias_humanas)`, escola_ce_medias$`avg(media_ciencias_humanas)`
                         , escola_df_medias$`avg(media_ciencias_humanas)`, escola_es_medias$`avg(media_ciencias_humanas)`, escola_go_medias$`avg(media_ciencias_humanas)`
                         , escola_ma_medias$`avg(media_ciencias_humanas)`, escola_mg_medias$`avg(media_ciencias_humanas)`, escola_ms_medias$`avg(media_ciencias_humanas)`
                         , escola_mt_medias$`avg(media_ciencias_humanas)`, escola_pa_medias$`avg(media_ciencias_humanas)`, escola_pb_medias$`avg(media_ciencias_humanas)`
                         , escola_pe_medias$`avg(media_ciencias_humanas)`, escola_pi_medias$`avg(media_ciencias_humanas)`, escola_pr_medias$`avg(media_ciencias_humanas)`
                         , escola_rj_medias$`avg(media_ciencias_humanas)`, escola_rn_medias$`avg(media_ciencias_humanas)`, escola_ro_medias$`avg(media_ciencias_humanas)`
                         , escola_rr_medias$`avg(media_ciencias_humanas)`, escola_rs_medias$`avg(media_ciencias_humanas)`, escola_sc_medias$`avg(media_ciencias_humanas)`
                         , escola_se_medias$`avg(media_ciencias_humanas)`, escola_sp_medias$`avg(media_ciencias_humanas)`, escola_to_medias$`avg(media_ciencias_humanas)`)
ciencias_humanas = round(ciencias_humanas, digits = 2)

lingua_portuguesa = c(escola_ac_medias$`avg(media_lingua_portuguesa)`, escola_al_medias$`avg(media_lingua_portuguesa)`, escola_am_medias$`avg(media_lingua_portuguesa)`
                     , escola_ap_medias$`avg(media_lingua_portuguesa)`, escola_ba_medias$`avg(media_lingua_portuguesa)`, escola_ce_medias$`avg(media_lingua_portuguesa)`
                     , escola_df_medias$`avg(media_lingua_portuguesa)`, escola_es_medias$`avg(media_lingua_portuguesa)`, escola_go_medias$`avg(media_lingua_portuguesa)`
                     , escola_ma_medias$`avg(media_lingua_portuguesa)`, escola_mg_medias$`avg(media_lingua_portuguesa)`, escola_ms_medias$`avg(media_lingua_portuguesa)`
                     , escola_mt_medias$`avg(media_lingua_portuguesa)`, escola_pa_medias$`avg(media_lingua_portuguesa)`, escola_pb_medias$`avg(media_lingua_portuguesa)`
                     , escola_pe_medias$`avg(media_lingua_portuguesa)`, escola_pi_medias$`avg(media_lingua_portuguesa)`, escola_pr_medias$`avg(media_lingua_portuguesa)`
                     , escola_rj_medias$`avg(media_lingua_portuguesa)`, escola_rn_medias$`avg(media_lingua_portuguesa)`, escola_ro_medias$`avg(media_lingua_portuguesa)`
                     , escola_rr_medias$`avg(media_lingua_portuguesa)`, escola_rs_medias$`avg(media_lingua_portuguesa)`, escola_sc_medias$`avg(media_lingua_portuguesa)`
                     , escola_se_medias$`avg(media_lingua_portuguesa)`, escola_sp_medias$`avg(media_lingua_portuguesa)`, escola_to_medias$`avg(media_lingua_portuguesa)`)
lingua_portuguesa = round(lingua_portuguesa, digits = 2)

matematica = c(escola_ac_medias$`avg(media_matematica)`, escola_al_medias$`avg(media_matematica)`, escola_am_medias$`avg(media_matematica)`
                      , escola_ap_medias$`avg(media_matematica)`, escola_ba_medias$`avg(media_matematica)`, escola_ce_medias$`avg(media_matematica)`
                      , escola_df_medias$`avg(media_matematica)`, escola_es_medias$`avg(media_matematica)`, escola_go_medias$`avg(media_matematica)`
                      , escola_ma_medias$`avg(media_matematica)`, escola_mg_medias$`avg(media_matematica)`, escola_ms_medias$`avg(media_matematica)`
                      , escola_mt_medias$`avg(media_matematica)`, escola_pa_medias$`avg(media_matematica)`, escola_pb_medias$`avg(media_matematica)`
                      , escola_pe_medias$`avg(media_matematica)`, escola_pi_medias$`avg(media_matematica)`, escola_pr_medias$`avg(media_matematica)`
                      , escola_rj_medias$`avg(media_matematica)`, escola_rn_medias$`avg(media_matematica)`, escola_ro_medias$`avg(media_matematica)`
                      , escola_rr_medias$`avg(media_matematica)`, escola_rs_medias$`avg(media_matematica)`, escola_sc_medias$`avg(media_matematica)`
                      , escola_se_medias$`avg(media_matematica)`, escola_sp_medias$`avg(media_matematica)`, escola_to_medias$`avg(media_matematica)`)
matematica = round(matematica, digits = 2)

redacao = c(escola_ac_medias$`avg(media_redacao)`, escola_al_medias$`avg(media_redacao)`, escola_am_medias$`avg(media_redacao)`
               , escola_ap_medias$`avg(media_redacao)`, escola_ba_medias$`avg(media_redacao)`, escola_ce_medias$`avg(media_redacao)`
               , escola_df_medias$`avg(media_redacao)`, escola_es_medias$`avg(media_redacao)`, escola_go_medias$`avg(media_redacao)`
               , escola_ma_medias$`avg(media_redacao)`, escola_mg_medias$`avg(media_redacao)`, escola_ms_medias$`avg(media_redacao)`
               , escola_mt_medias$`avg(media_redacao)`, escola_pa_medias$`avg(media_redacao)`, escola_pb_medias$`avg(media_redacao)`
               , escola_pe_medias$`avg(media_redacao)`, escola_pi_medias$`avg(media_redacao)`, escola_pr_medias$`avg(media_redacao)`
               , escola_rj_medias$`avg(media_redacao)`, escola_rn_medias$`avg(media_redacao)`, escola_sc_medias$`avg(media_redacao)`
               , escola_se_medias$`avg(media_redacao)`, escola_sp_medias$`avg(media_redacao)`, escola_to_medias$`avg(media_redacao)`)
redacao = round(redacao, digits = 2)


# Análise bivariada para duas variáveis quantitativas
temp<-data.table(estado=c(  escola_ac_medias$sigla_estado, escola_al_medias$sigla_estado
                          , escola_am_medias$sigla_estado, escola_ap_medias$sigla_estado
                          , escola_ba_medias$sigla_estado, escola_ce_medias$sigla_estado
                          , escola_df_medias$sigla_estado, escola_es_medias$sigla_estado
                          , escola_go_medias$sigla_estado, escola_ma_medias$sigla_estado
                          , escola_mg_medias$sigla_estado, escola_ms_medias$sigla_estado
                          , escola_mt_medias$sigla_estado, escola_pa_medias$sigla_estado
                          , escola_pb_medias$sigla_estado, escola_pe_medias$sigla_estado
                          , escola_pi_medias$sigla_estado, escola_pr_medias$sigla_estado
                          , escola_rj_medias$sigla_estado, escola_rn_medias$sigla_estado
                          , escola_ro_medias$sigla_estado, escola_rr_medias$sigla_estado
                          , escola_rs_medias$sigla_estado, escola_sc_medias$sigla_estado
                          , escola_se_medias$sigla_estado, escola_sp_medias$sigla_estado
                          , escola_to_medias$sigla_estado),
                 media=c(redacao)
)



ggplot(temp, aes(x=estado, y=media)) +
  geom_bar(stat="identity",fill="steelblue") + 
  labs(x="Porte", y="Taxas", title="Média Redação") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.3))

#####

######Menores médias por cidade#####

indicadorNivelSocioEconomico = na.omit(ano_2013$adequacao_da_formacao_docente)
adequacaoDaFormacaoDocente = na.omit(ano_2013$taxa_de_permanencia_escolar)
taxaDePermanenciaEscola = na.omit(ano_2013$num_participantes_com_necessidades)


str(indicadorNivelSocioEconomico)
ins = as.data.frame(indicadorNivelSocioEconomico)
View(ins)

str(adequacaoDaFormacaoDocente)
afd = as.data.frame(adequacaoDaFormacaoDocente)
View(afd)

str(taxaDePermanenciaEscola)
tdp = as.data.frame(taxaDePermanenciaEscola)
View(tdp)
ano_2013[!is.na(media_redacao),
   list(mean=mean(media_redacao),
        median = median(media_redacao),
        sd=sd(media_redacao),
        max = max(media_redacao),
        min = min(media_redacao)),
   by=media_redacao]


#####

#Agrupamento por tipo de localização####

View(ano_2015[localizacao_da_escola == "Privada"])
ano_2015[localizacao_da_escola == "Urbana"]

escolas_urbanas = (enem_2015[localizacao_da_escola == "Urbana"])
escolas_rurais = (enem_2015[localizacao_da_escola == "Privada"])

View(escolas_urbanas)
View(escolas_rurais)
#####
table(ano_2015[tipo_dependecia_administrativa == "Privada"]$sigla_estado)
table(dados[localizacao_da_escola == "Urbana"]$sigla_estado)

################################################################################
################################################################################
# Linear Regression####
set.seed(6525)
#Coleta de amostras
y <- sample(enem_2015$taxa_de_aprovacao)
x <- sample(enem_2015$media_redacao)

#Geração do modelo para avaliar a correlação das variáveis
modelo = lm(y ~ x)
modelo

#Observando se há significância ####
summary(modelo)
## Coeficientes do modelo (intercepto e beta)
anova(modelo)

# Linearidade #####

#Plotagem de gráfico de dispersão
plot(x,y,xlab="Média Total",ylab="Taxa de Aprovação", pch = 20, main = 'Gráfico de Dispersão')
abline(lm(y~x))
# Homocedasticidade dos resíduos####
par(mfrow = c(2,2))
plot(modelo, which = c(1:4), pch = 20)

# Normalidade dos Resíduos ####
hist(modelo$residuals, col = 'gray', xlab = 'Resíduos', ylab = 'Densidade de Probabilidade',
     probability = TRUE, main = 'Histógrama Residual')

# Shapiro-Wil####
shapiro.test(rstudent(modelo))

# Valores previstos pelo modelo
predict(modelo)   

# Resíduos do modelo
residuals(modelo) 

##########
##########
##########
# Predizendo valores utilizando o modelo de regressão
predito <- predict(modelo)

# Calculando a ANOVA do modelo 
anova(modelo)

cor(x, y)
# Intervalos de confiança para os coeficientes
confint(modelo)

# Intervalo de previsão
newdata=data.frame() # `data.frame` contém os dados das variáveis explicativas utilizados para calcular as previsões.
predict(modelo, newdata, interval="predict") 

# Resíduos padronizados
rstandard(modelo)
# Resíduos não padronizados
resid(modelo)
#####

# Logistic Regression####
#Tempos de reação a um estímulo(Y) e acuidade visual(Z) de 20 indivíduos, segundo o sexo(W) e a idade(X)
df<-data.frame(
  individuo=1:20,
  tempo_Y=c(96,92,106,100,98,104,110,101,116,106,109,100,112,105,118,108,113,112,127,117),
  sexo_W=factor(c("H","M","H","M","M","H","H","M","M","H","H","M","M","M","H","H","M","M","H","H")),
  idade_X=factor(c(20,20,20,20,25,25,25,25,30,30,30,30,35,35,35,35,40,40,40,40)),
  acuidade_Z=c(90,100,80,90,100,90,80,90,70,90,90,80,90,80,70,90,90,90,60,80))

df$n_idade_X<-as.numeric(as.character(df$idade_X))
plot(df$n_idade_X,df$tempo_Y, pch=20, xlab="Idade(x)", ylab="Estímulo(y)", col="darkblue")
abline(lm(df$tempo_Y~df$n_idade_X), lwd=2, col="red")


#Ajustando o modelo de regressão 
modelo <- lm(tempo_Y ~ n_idade_X,data = df)
summary(modelo)
modelo$residuals

# Predizendo valores utilizando o modelo de regressão
predito <- predict(modelo)

#Calculando os resíduos
residuos<-cbind(df[,1:3],resid=resid(modelo))

# Calculando a ANOVA do modelo 
anova(modelo)

# Intervalos de confiança para os coeficientes
confint(modelo)

# Intervalo de previsão
newdata=data.frame(n_idade_X=28) # `data.frame` contém os dados das variáveis explicativas utilizados para calcular as previsões.
predict(modelo, newdata, interval="predict") 

# Resíduos padronizados
rstandard(modelo)
# Resíduos não padronizados
resid(modelo)

cbind(df[3:4],zi=rstandard(modelo),resid = resid(modelo))


# Plotando os resíduos 
par(mfrow=c(1,2))
plot(df$n_idade_X,resid(modelo), cex=2, col="darkblue", pch=20, xlab="Idade", ylab="Resíduos",main="(a)")
abline(h=0)
plot(df$n_idade_X,rstandard(modelo), cex=2, col="darkblue", pch=20, xlab="Idade", ylab="Resíduos padronizados",main="(b)")
abline(h=0)

# Histograma dos resíduos 
hist(resid(modelo), xlab="Resíduos", ylab="", col="lightblue3", border="white", main="")
qqnorm(resid(modelo), cex=1.5, col="darkblue", xlab="Quantis da normal padrão", ylab="Quantis dos resíduos", pch=20)
qqline(resid(modelo), col="darkred")



par(mfrow=c(2,2))
plot(modelo)

# Logistics Regression
df_pos_neg = df[resultado_teste %in% c('NEGATIVO','POSITIVO') ]
glm(as.factor(resultado_teste) ~ as.factor(sintoma_febre) , data = df_pos_neg, familly = binomial)

idade=c(21,20,25,26,22,35,36,40,42,46,59,50,60,72,85,59,29,45,39,45,20,25,36,58,95,52,80,85,62,72)
renda=c(1,1,1,1,0,0,1,1,1,1,1,0,1,1,1,1,1,0,0,0,0,0,0,1,0,0,0,0,0,1)
saude=c(0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

modelo=glm(SURVIVED~ SEX + PCLASS,family=binomial, data=titanic)
summary(modelo)
#####
################################################################################
################################################################################
####
#código de terceiros####
library(ggplot2)
df<-read.csv("enem_por_escolas.csv", header = TRUE, sep = ',', dec = ',', stringsAsFactors = F)
#df<-read.csv2("enem_por_escolas.csv")
df<-as.data.table(df)
df_resumido = df[df$ano == 2015]

dados_organizados = df_resumido %>% 
  group_by(Nome_escola = num_escola_educacenso,
           Sigla_mun = sigla_estado,
           Porte_escola = porte_escolar,
           Taxa_aprovacao = taxa_de_aprovacao) %>%
  summarise(Media_total = 
              sum(as.numeric(
                c(media_ciencias_da_natureza,
                  media_ciencias_humanas,
                  media_lingua_portuguesa,
                  media_matematica,
                  media_redacao)
              ))/5)

vetor_porte = dados_organizados %>% 
  group_by(Porte_escola) %>% 
  summarise(Media = mean(!is.na(as.numeric(Taxa_aprovacao)))*100)

ggplot(vetor_porte, aes(x = vetor_porte$Porte_escola , y = vetor_porte$Media)) + 
  geom_bar(stat='identity') + 
  labs(x="Estado", y="Média", title="Média das notas no ENEM por Estado no periodo de 2013 a 2015") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.3)) 
#####

#Revisar - Distribuição binomial | Calulo de probabilidades###############################
# DISTRIBUIçã NORMAL
hist(ano_2015$num_participantes)

summary(ano_2015$num_participantes)
hist(log(ano_2015$num_participantes))

# para escolas acima de 90 alunos, qual a probabilidade
# da escola ter acima de 100 participantes
View(dados)
media = mean(ano_2015[!is.na(num_matriculas)]$num_matriculas)
desvio = sd(ano_2015[!is.na(num_matriculas)]$num_matriculas)

media = mean(ano_2015[!is.na(num_participantes)]$num_participantes)
desvio = sd(ano_2015[!is.na(num_participantes)]$num_participantes)

media = mean(ano_2015[!is.na(taxa_participacao)]$taxa_participacao)
desvio = sd(ano_2015[!is.na(taxa_participacao)]$taxa_participacao)

media = mean(ano_2015[!is.na(media_ciencias_da_natureza)]$media_ciencias_da_natureza)
desvio = sd(ano_2015[!is.na(media_ciencias_da_natureza)]$media_ciencias_da_natureza)

media = mean(ano_2015[!is.na(media_ciencias_humanas)]$media_ciencias_humanas)
desvio = sd(ano_2015[!is.na(media_ciencias_humanas)]$media_ciencias_humanas)

media = mean(ano_2015[!is.na(media_lingua_portuguesa)]$media_lingua_portuguesa)
desvio = sd(ano_2015[!is.na(media_lingua_portuguesa)]$media_lingua_portuguesa)

media = mean(ano_2015[!is.na(media_matematica)]$media_matematica)
desvio = sd(ano_2015[!is.na(media_matematica)]$media_matematica)

media = mean(ano_2015[!is.na(media_redacao)]$media_redacao)
desvio = sd(ano_2015[!is.na(media_redacao)]$media_redacao)

#-----

pnorm(100, mean = media, sd = desvio)

pnorm(100, mean=media,
      sd=desvio,
      lower.tail=F) 

# Qual a probabilidade do indivíduo ter entre 30 e 50 participantes?

pnorm(30, mean=media,
      sd=desvio,
      lower.tail=F) - 
  pnorm(50, mean=media,
        sd=desvio,
        lower.tail=F)
#-----
#####