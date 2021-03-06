#---Seguran�a ALimentar ----#

#Limpar a m�moria do R
#rm(list=ls(all=TRUE))

#Leitura da PNAD2013
library(lodown)

# lista todos os arquivos de microdados da PNAD dispon�veis
pnad_cat <-
  get_catalog( "pnad" ,
               output_dir = file.path( path.expand( "~" ) , "PNAD" ) )

# 2013 apenas
pnad_cat <- subset( pnad_cat , year == 2013)

# baixa os microdados para seu computador
pnad_cat <- lodown( "pnad" , pnad_cat )

# arquivo de microdados da pnad2013 (merge DOM e PES)

pnad2013 <- readRDS( pnad_cat[ 1 , 'output_filename' ] )

attach(pnad2013)

#Filtrando base de pnad2013DOM
pnad2013<-subset(pnad2013, select=c(uf,v0302,v8005,v0404,v0602,v9001,v4728,v4743
                                    ,v2101,v2103,v2105,v2107,v2109,v2113,v2115,v2117,v2121
                                    ,v2123,v2124,v2126,v2128,v2130,v2132,v2134,v2138,v2139
                                    ,v4618,v4617,v4609,v4610,v4611,v0102,v0103,v0401,one,region,pre_wgt))

str(pnad2013)

#Base somente com domic�lios (filtro pela pessoa de refer�ncia)

pnad2013DOM<-subset(pnad2013,v0401=="1")
str(pnad2013DOM) 

detach(pnad2013) 
attach(pnad2013DOM)

#Itens do EBIA (transformando sim=1, n�o=0)

pnad2013DOM$vv2103<-ifelse(is.na(v2103),0, ifelse(v2103==1,1,0))
pnad2013DOM$vv2105<-ifelse(is.na(v2105),0, ifelse(v2105==1,1,0))
pnad2013DOM$vv2107<-ifelse(is.na(v2107),0, ifelse(v2107==1,1,0))
pnad2013DOM$vv2109<-ifelse(is.na(v2109),0, ifelse(v2109==1,1,0))
pnad2013DOM$vv2113<-ifelse(is.na(v2113),0, ifelse(v2113==1,1,0))
pnad2013DOM$vv2115<-ifelse(is.na(v2115),0, ifelse(v2115==1,1,0))
pnad2013DOM$vv2117<-ifelse(is.na(v2117),0, ifelse(v2117==1,1,0))
pnad2013DOM$vv2121<-ifelse(is.na(v2121),0, ifelse(v2121==1,1,0))
pnad2013DOM$vv2124<-ifelse(is.na(v2124),0, ifelse(v2124==2,1,0))
pnad2013DOM$vv2126<-ifelse(is.na(v2126),0, ifelse(v2126==2,1,0))
pnad2013DOM$vv2128<-ifelse(is.na(v2128),0, ifelse(v2128==2,1,0))
pnad2013DOM$vv2130<-ifelse(is.na(v2130),0, ifelse(v2130==2,1,0))
pnad2013DOM$vv2132<-ifelse(is.na(v2132),0, ifelse(v2132==2,1,0))
pnad2013DOM$vv2134<-ifelse(is.na(v2134),0, ifelse(v2134==2,1,0))


attach(pnad2013DOM)

pnad2013DOM$segtotal<-c(vv2103+vv2105+vv2107+vv2109+vv2113+vv2115+vv2117+vv2121
                        +vv2124+vv2126+vv2128+vv2130+vv2132+vv2134)

table(pnad2013DOM$segtotal)
sum(pnad2013DOM$segtotal) #115560

attach(pnad2013DOM)
str(pnad2013DOM)

###Seguran�a alimentar###

#se n�o respondeu sim a nenhuma pergunta � seguro
#se tem menor de idade V2123=1
#se n�o tem menor de idade V2123=3


# Cria a variavel Classifica��o da Inseguran�a Alimentar
pnad2013DOM = transform(pnad2013DOM, CLASS =ifelse(segtotal==0,"Seguran�a Alimentar",
                                                   ifelse(segtotal>=1 & segtotal<=5 & v2123==1,"IA Leve",
                                                          ifelse(segtotal>=6 & segtotal<=9 & v2123==1,"IA Moderada",
                                                                 ifelse(segtotal>=10 & segtotal<=14 & v2123==1,"IA Grave",
                                                                        ifelse(segtotal>=1 & segtotal<=3 & v2123==3,"IA Leve",
                                                                               ifelse(segtotal>=4 & segtotal<=5 & v2123==3,"IA Moderada",
                                                                                      ifelse(segtotal>=6 & segtotal<=8 & v2123==3,"IA Grave",NA)))))))) 
str(pnad2013DOM)
colnames(pnad2013DOM)
summary(pnad2013DOM$CLASS) #116543 ok
head(pnad2013DOM$CLASS,200)

attach(pnad2013DOM)

#teste sem o peso amostral
table(CLASS) #tabela de seguran�a
pont.v2123.tb<-table(CLASS,v2123)
prop.table(pont.v2123.tb) #frequencia relativa


### Estima��o utilizando Teoria de Resposta ao Item (com base no Analysis.r do VoH) ###

### Carregando os pacotes necess�rios

#install.packages("eRm")
library(eRm)
#install.packages("psychomix")
library(psychomix)
#install.packages("weights")
library(weights)
#install.packages("RM.weights)
library(RM.weights)

# Se��o 1: Dados ------------------------------------------------------------

# Base de pnad2013DOM FIES para o BRASIL (segundo PNAD 2013)
# Leva em considera��o somente os 8 itens referentes a domic�lios com maiores de 18 anos

pnad2013DOMfies<- subset(pnad2013DOM, select=c("uf","v0302","v8005","v0404","v0602","v9001","v4728","v4743"
                                               ,"vv2103","vv2105","vv2107","vv2109","vv2113","vv2115","vv2117","vv2121","v2138","v2139"
                                               ,"v4618","v4617","v4609", "v4611","one","region","pre_wgt")) 

attach(pnad2013DOMfies)

#install.packages("plyr")
library(plyr)

data.FAO_Brasil<-rename(pnad2013DOMfies, c("vv2103"="WORRIED","vv2105"="RUNOUT","vv2107"="HEALTHY","vv2109"="FEWFOOD"
                                           ,"vv2113"="SKIPPED","vv2115"='ATELESS',"vv2117"="HUNGRY","vv2121"="WHLDAY"
                                           ,"v2138"="Atitude","v2139"="Outra atitude"))
names(data.FAO_Brasil)


head(pre_wgt) #peso b�sico do damico (=v4610)
head(v4611) #peso do domic�lio segundo o dicion�rio
head(one) #peso 1 (teste)

### Fixa a base de dados
attach(data.FAO_Brasil)

### Salvando a base do FIES e seus correspondentes pesos
XX.Brasil = data.FAO_Brasil[,9:16]
str(XX.Brasil)
wt.Brasil = data.FAO_Brasil$v4611

### Calculando os escores (n�mero individual de "sim" para os 8 itens)
rv.Brasil=rowSums(XX.Brasil)

### N�mero  de itens do FIES
k = ncol(XX.Brasil)


# Se��o 2: An�lise Psicom�trica ----------------------------------------------------------------
library(RM.weights)
#?RM.w
rr.Brasil = RM.w(XX.Brasil, wt.Brasil) 

# Resultados dentro do modelo...
str(rr.Brasil)
# Gravidade do Item 
rr.Brasil$b
# Erro padr�o da gravidade do Item
rr.Brasil$se.b
# Gravidade do domic�lio 
rr.Brasil$a
# Erro padr�o da gravidade do domic�lio
rr.Brasil$se.a
# Infit
rr.Brasil$infit
# Outfit
rr.Brasil$outfit
# Rasch reliability
rr.Brasil$reliab
# Rasch reliability (flat)
rr.Brasil$reliab.fl
# Person infit: observed and expected
quantile.seq = c(0,.01,.02,.05,.10,.25,.50,.75,.90,.95,.98,.99,1)
plot(quantile.seq, rr.Brasil$q.infit, type = "b", xlab = "Quantis", 
     ylab = "INFITs Observados", ylim = c(0, 6))
lines(quantile.seq, rr.Brasil$q.infit.theor, type = "b", col = 2)
# Correla��o Residual
rr.Brasil$res.cor

# Se��o 3: Salvando outputs ------------------------------------------------------------------

#rr.Brasil = RM.w(XX.Brasil, wt.Brasil, country = "Brasil", write.file = T) #24 minutos

# Se��o 4: An�lises Descritivas ------------------------------------------------------------------
#?tab.weight
## Raw score distribution
# Unweighted
table(rv.Brasil)
# Weighted
tab.weight(as.factor(rv.Brasil), wt.Brasil, XX.Brasil)$RS.abs.w
# Weighted percentage distribution
tab.weight(as.factor(rv.Brasil), wt.Brasil, XX.Brasil)$RS.rel.w*100

# Se��o 5: Atribui��o das probabilidades -------------------------------------
# Pr�-define os extremos para definir SA e IA
# IA moderada ou grave (-0.25) e IA grave (1.81) #no exemplo da FAO
sthresh = c(-0.25, 1.81)
pp.Brasil = prob.assign(rr.Brasil, sthres = sthresh)$sprob
# Prevalence of moderate or severe food insecurity in Brazil
pp.Brasil[1]*100
# Prevalence of severe food insecurity in Brazil
pp.Brasil[2]*100

#Para a equaliza��o (torna as medidas comparativas com outros pa�ses)
pp.Brasil2013=prob.assign(rr.Brasil, sthres = sthresh)$sprob

#####################################################################################################
#  Modelo Rasch utilizando package "mirt" ou "ltm" #

#install.packages("ltm")
library(ltm)
#install.packages("mirt")
library(mirt)

itensfies<-XX.Brasil #igual ao XX.Brasil
str(itensfies)

#Calibra��o do modelo

pnad2013DOMmirt<- mirt(itensfies, model=1,itemtype = "Rasch",constrained=F, IRT.param=T,survey.weights=wt.Brasil) #unidimensional com 1pl (Rasch)
coef(pnad2013DOMmirt, IRTPars = TRUE, simplify = TRUE)

#Curvas caracter�sticas dos itens
par(mfrow=c(1,1),ask=FALSE)
itemplot(pnad2013DOMmirt,item=1) #curvas caracter�stica do item 1
par(mfrow=c(1,1))

#curva de informa��o dos itens
par(mfrow=c(1,1),ask=FALSE)
itemplot(pnad2013DOMmirt,item=1,type="info")
par(mfrow=c(1,1))

#curva Caracteristica do Teste
print(plot(pnad2013DOMmirt))

#fun��o de informa��o do teste com linha do erro
plot(pnad2013DOMmirt, type = 'infoSE', main = "Fun��o de Informa��o do Teste e Erro Padr�o de Medida") 

#Estima��o de theta
coef(pnad2013DOMmirt)
base.est<-fscores(pnad2013DOMmirt, resp.patterns = itensfies) # calcula a habilidade de cada um dos sujeitos
length(base.est) # n=116543 

theta<-base.est
table(theta)

## Para fazer tabela de contig�ncia com a EBIA ###
#motiva��o: quais os valores que definem a escala? De quanto a quanto � leve, mod ou grave??? 
#thetanovo= forma de colocar o theta na mesma escala da EBIA

thetanovo<-((8-0)*(theta - min(theta)))/(max(theta)- min(theta))
summary(thetanovo)
table(thetanovo)

classmirt<-ifelse(thetanovo==0,"Seguran�a Alimentar",
                  ifelse(thetanovo>=0.01 & thetanovo<=3.99,"IA Leve",
                         ifelse(thetanovo>=4 & thetanovo<=5.99,"IA Moderada",
                                ifelse(thetanovo>=6 & thetanovo<=8 ,"IA Grave",NA))))

summary(classmirt)
table(classmirt)


#Tabela de Conting�ncia
table(CLASS,classmirt) #CLASS � da EBIA
table(CLASS)

################################# REFAZENDO com os 14 itens  ####################################################

### Estima��o utilizando Teoria de Resposta ao Item (com base no Analysis.r do VoH) ###

###  Carregando pacotes

#install.packages("eRm")
library(eRm)
#install.packages("psychomix")
library(psychomix)
#install.packages("weights")
library(weights)

library(RM.weights)

# Se��o 1: Dados ------------------------------------------------------------

# Base de pnad2013DOM FIES para o BRASIL (segundo PNAD 2013)
# Leva em considera��o somente os 8 itens referentes a domic�lios com maiores de 18 anos

pnad2013DOMfiesplus<- subset(pnad2013DOM, select=c("uf","v0302","v8005","v0404","v0602","v9001","v4728","v4743"
                                                   ,"vv2103","vv2105","vv2107","vv2109","vv2113","vv2115","vv2117","vv2121"
                                                   ,"vv2124", "vv2126", "vv2128", "vv2130","vv2132","vv2134", "v2138","v2139"
                                                   ,"v4618","v4617","v4609", "v4611","one","region","pre_wgt")) 

attach(pnad2013DOMfiesplus)

#install.packages("plyr")
library(plyr)

data.FAO_Brasilplus<-rename(pnad2013DOMfiesplus, c("vv2103"="WORRIED","vv2105"="RUNOUT","vv2107"="HEALTHY","vv2109"="FEWFOOD"
                                                   ,"vv2113"="SKIPPED","vv2115"='ATELESS',"vv2117"="HUNGRY","vv2121"="WHLDAY"
                                                   ,"vv2124"="V2124","vv2126"='V2126',"vv2128"="V2128","vv2130"="V2130","vv2132"= "V2132"
                                                   ,"vv2134"= "V2134","v2138"="Atitude","v2139"="Outra atitude"))
names(data.FAO_Brasilplus)


head(pre_wgt) #peso b�sico do damico (=v4610)
head(v4611) #peso do domic�lio segundo o dicion�rio
head(one) #peso 1 (teste)

### Fixando o banco de dados
attach(data.FAO_Brasilplus)

### Salvando os dados do FIES e pesos correspondentes
XX.Brasilplus = data.FAO_Brasilplus[,9:16]
str(XX.Brasilplus)
wt.Brasilplus = data.FAO_Brasilplus$v4611

### C�lculo dos escores (n�mero de sim para as oito quest�es)
rv.Brasilplus=rowSums(XX.Brasilplus)

### N�mero de itens do FIES
kplus = ncol(XX.Brasilplus)

# Se��o 2: An�lise Psicom�trica ----------------------------------------------------------------
library(RM.weights)
#?RM.w
rr.Brasilplus = RM.w(XX.Brasilplus, wt.Brasilplus) 
# Resultados dentro do modelo...
str(rr.Brasilplus)
# Gravidade do Item
rr.Brasilplus$b
# Erro padr�o da gravidade do item
rr.Brasilplus$se.b
# Gravidade do Domicilio
rr.Brasilplus$a
# Erro Padr�o da gravidade do domicilio
rr.Brasilplus$se.a
# Infit
rr.Brasilplus$infit
# Outfit
rr.Brasilplus$outfit
# Rasch reliability
rr.Brasilplus$reliab
# Rasch reliability (flat)
rr.Brasilplus$reliab.fl
# Person infit: observados e esperados
quantile.seqplus = c(0,.01,.02,.05,.10,.25,.50,.75,.90,.95,.98,.99,1)
plot(quantile.seqplus, rr.Brasilplus$q.infit, type = "b", xlab = "Quantiles", 
     ylab = "Observed infit", ylim = c(0, 6))
lines(quantile.seqplus, rr.Brasilplus$q.infit.theor, type = "b", col = 2)
# Correla��o Residual
rr.Brasilplus$res.cor

# Se��o 3: Salvando outputs ------------------------------------------------------------------

#rr.Brasilplus = RM.w(XX.Brasilplus, wt.Brasilplus, country = "Brasil", write.file = T) 

# Se��o 4: Descritivas ------------------------------------------------------------------
#?tab.weight
## Raw score distribution
# Unweighted
table(rv.Brasilplus)
# Weighted
tab.weight(as.factor(rv.Brasilplus), wt.Brasilplus, XX.Brasilplus)$RS.abs.w
# Weighted percentage distribution
tab.weight(as.factor(rv.Brasilplus), wt.Brasilplus, XX.Brasilplus)$RS.rel.w*100

# Se��o 5: Atribui��o das probabilidades -------------------------------------
# Limites pr�-definidos t
#moderada ou grave (-0.25) e grave (1.81) #no exemplo da FAO
sthresh = c(-0.25, 1.81)
pp.Brasilplus = prob.assign(rr.Brasilplus, sthres = sthresh)$sprob
# Prevalence of moderate or severe food insecurity in Brazil
pp.Brasilplus[1]*100
# Prevalence of severe food insecurity in Brazil
pp.Brasilplus[2]*100


###############################################################################################
#  Modelo Rasch utilizando package "mirt" ou "ltm" #
library(ltm)
#install.packages("mirt")
library(mirt)
itensfiesplus<-XX.Brasilplus #igual ao XX.Brasil
str(itensfiesplus)

#Calibra��o do modelo

pnad2013DOMmirtplus<- mirt(itensfiesplus, model=1,itemtype = "Rasch",constrained=F, IRT.param=T,survey.weights=wt.Brasilplus) #unidimensional com 1pl (Rasch)
coef(pnad2013DOMmirtplus, IRTPars = TRUE, simplify = TRUE)

#Curvas caracter�sticas dos itens
par(mfrow=c(1,1),ask=FALSE)
itemplot(pnad2013DOMmirtplus,item=1) #curvas caracter�stica do item 1
par(mfrow=c(1,1))

#curva de informa��o dos itens
par(mfrow=c(1,1),ask=FALSE)
itemplot(pnad2013DOMmirtplus,item=1,type="info")
par(mfrow=c(1,1))

#curva de informa��o do teste
print(plot(pnad2013DOMmirtplus))

#Estima��o de theta
coef(pnad2013DOMmirtplus)
base.estplus<-fscores(pnad2013DOMmirtplus, resp.patterns = itensfiesplus) # calcula a habilidade de cada um dos sujeitos
length(base.estplus) # N=116543 #UFA!
head(base.estplus)
thetaplus<-base.estplus
summary(thetaplus)
table(thetaplus)


##para fazer tabela de contig�ncia com a EBIA ###
#motiva��o: quais os valores que definem a escala? De quanto a quanto � leve, mod ou grave??? 
#thetanovo= forma de colocar o theta na mesma escala da EBIA

thetanovoplus<-((14-0)*(thetaplus - min(thetaplus)))/(max(thetaplus)- min(thetaplus))
summary(thetanovoplus)
table(thetanovoplus)

classmirtplus<-ifelse(thetanovoplus==0,"Seguran�a Alimentar",
                      ifelse(thetanovoplus>=0.01 & thetanovoplus<=5.99,"IA Leve",
                             ifelse(thetanovoplus>=6 & thetanovoplus<=9.99,"IA Moderada",
                                    ifelse(thetanovoplus>=10 & thetanovoplus<=14 ,"IA Grave",NA))))

summary(classmirtplus)
table(classmirtplus)


#Tabela de Conting�ncia
table(CLASS,classmirtplus) #CLASS � da EBIA
table(CLASS)
