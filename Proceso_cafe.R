# InstalaciÛn de librerÌas "Install.packages("nombredelpaquete")"

install.packages("RWeka")

library(tidyverse)
library(lubridate)
library(readxl)
library(arules)
library(inspectdf)


Dataset <- read_excel("C:/Users/Juan Valencia/Documents/QualitativeDatasetEnglish.xlsx")

#Dataset2 <- read_excel("C:/Users/Juan Valencia/Documents/DatasetRCualitativoConbioaggressors.xlsx")

View(Dataset)

qualitative_dataset <-as.data.frame(Dataset)
#qualitative_dataset <-as.data.frame(Dataset2)

## Clase de objeto y quÈ variables tiene y de quÈ tipo
str(qualitative_dataset)

## fechas con su formato
#qualitative_dataset$fecha_encuesta<- as.Date(qualitative_dataset$fecha_encuesta, format='%d/%m/%Y')
qualitative_dataset$survey_date<- as.Date(qualitative_dataset$survey_date, format='%d/%m/%Y')

## Dataframe
qualitative_dataset

view(qualitative_dataset)

# ExclusiÛn de variables
# Col 1.  id_caficultor 
# Col 2.  coffee_grower_name
# col 3.  Edad
# col 4.  Genero
# col 5.  crop_hectares
# Col 6.  fecha_encuesta 
# Col 7.  mes_de_encuesta
# Col 8.  semana_de_encuesta


#qualitative_dataset <- qualitative_dataset[,-c(1,2,3,4,6,7,8)]
qualitative_dataset <- qualitative_dataset[,-c(1,2,3,4,6,7,8)]

qualitative_dataset <- qualitative_dataset[,-c(31)]

# Las siguientes variables si no tienen una categoria por lo que se pueden excluir
# del analisis ya que estas no aportarian en reducir la varianza del dataset
# por que siempre seran iguales:

# "asociaciÛn"
# "ie_nacimientos_agua_cercanos" 


## VisualizaciÛn de frame
View(qualitative_dataset)

#which(apply(qualitative_dataset, 2, function(x){length(unique(x))})==1)
# Variables que no tienen mas de 1 dimensiÛn
#qualitative_dataset<- qualitative_dataset[,-which(apply(qualitative_dataset, 2, function(x){length(unique(x))})==1)]


## Ahora se categoriza la variable hectarea en 3 categorÌas diferentes
#qualitative_dataset$hectareas_cultivo <- discretize(qualitative_dataset$hectareas_cultivo, method = "interval",breaks = 3)
qualitative_dataset$crop_hectares <- discretize(qualitative_dataset$crop_hectares, method = "interval",breaks = 3)


view(qualitative_dataset)


# Se convierten las variables a factor(Dummificar)
for (i in 1:ncol(qualitative_dataset)) {
    qualitative_dataset[,i]<-as.factor(qualitative_dataset[,i])
}

str(qualitative_dataset)

## Gr·ficos y tablas

 # x11() Esto abre una ventana aparte de la consola y abre el gr·fico, se guardar· del tamano 
  #que tenga la ventana en el momento que se da guardar como. 

x11()
inspect_cat(qualitative_dataset) %>% show_plot()
inspect_cat(qualitative_dataset)[1:9,]$levels

inspect_cat(qualitative_dataset)


## Tablas cruzadas 

nombres<- unique(Dataset$coffee_grower_name)

variedades<-unique(Dataset$variety)

fenologia<-unique(Dataset$crop_phenology)

enfermedades<-unique(Dataset$bioaggressors)
# 
qualitative_dataset %>%
  filter(Dataset$coffee_grower_name==nombres[1])%>%
  inspect_cat()%>%
  show_plot()

qualitative_dataset %>%
  filter(Dataset$variety==variedades[1])%>%
  inspect_cat()%>%
  show_plot()

qualitative_dataset %>%
  filter(Dataset$crop_phenology==fenologia[1])%>%
  inspect_cat()%>%
  show_plot()


## Tablas cruzadas entre las variables y nombres de caficultores
install.packages("gmodels")

library(gmodels)

View(CrossTable( Dataset$coffee_grower_name,qualitative_dataset$bioaggressors)$t)

View(CrossTable( Dataset$crop_phenology,qualitative_dataset$bioaggressors)$t)

View(CrossTable( Dataset$variety,qualitative_dataset$bioaggressors)$t)

####################################################################################
############################# AplicaciÛn del ACM ###################################
####################################################################################

#El ACM al igual que otros m√©todos multivariados consiste en la descomposici√≥n 
#ortogonal de una matriz a partir del c√°lculo de sus valores propios y vectores
#propios. La idea consiste en disminuir la dimensi√≥n de la inercia total de las
#variables e individuos, representandolas en otras variables llamadas componentes
#o dimensiones, las cuales, se esperan que tengan mayor informaci√≥n retenida en
# menos dimensiones que las iniciales. generalmente se estudian las dos primeras 
#dimensiones a partir de un plano factorial. 

install.packages("factoextra")
library(factoextra)
library(FactoMineR)
library(FactoClass)
library(Factoshiny)

acm<-MCA(qualitative_dataset,graph = F, ncp=10)

# Se observan contribuciones de las variables
# es que porcentaje aporta cada variable a la construcciÛn de cada dimensiÛn

View(round(acm$var$contrib,2))
fviz_mca_biplot(acm,repel = T)
varn<-rownames(acm$var$coord)

#varn[c(25,26,27,28,29,30,31,32,53,54,55,60,61,64,65)] <- c("arvenses_agresivos","arvenses_no","arvenses_nobles","enfermedad_derrite o quema","enfermedad_mancha_h","enfermedad_no","plagabroca","plaga_no","otros_c_aguacate","otros_c_guamo","otros_c_no","conocimiento_anc_no","conocimiento_anc_ULCS","condicion_ext_clima","condicion_ext_no")

## Mapa de los individuos agrupado por nombre de cultivador
## los cultivadores cercanos son aquellos que tienen caracteristicas similares en sus cultivos
view(Dataset)

x11()
fviz_mca_ind(acm, col.ind = Dataset$variety)

x11()
fviz_mca_ind(acm, col.ind = Dataset$bioaggressors)

x11()
fviz_mca_biplot(acm,repel = T, col.ind = Dataset$variety)

x11()
fviz_mca_biplot(acm,repel = T, select_var = list(name = varn[c(1:8)]), col.ind = Dataset$variety)

x11()
fviz_mca_biplot(acm,repel = T, select_var = list(name = varn[c(1:8)]), col.ind = Dataset$bioaggressors)

x11()
fviz_mca_biplot(acm,repel = T, select_var = list(name = varn[c(1:8,8:40)]), col.ind = Dataset$variety)

## Mapas superpuesto para encontrar correspondencias
 
# todos los planos tienen las variables de interÈs asociadas a la apariciÛn de bioaggressors. 

# El primero contiene las variables asociadas al sitio del cultivo, el tamaÒo en hectareas, variedad cultivada y si tiene alrededor m·s tipos de cultivos. 
x11()
fviz_mca_biplot(acm,repel = T, select.var = list(name = varn[c(1:8,6:21)]),col.ind = qualitative_dataset_sin_presencia_ausencia$variety)

x11()
fviz_mca_biplot(acm,repel = T, select.var = list(name = varn[c(8:16,25:32)]),col.ind = Dataset$variety)

# El segundo contiene las variables asociadas al clima en general. 
x11()
fviz_mca_biplot(acm,repel = T,select.var = list(name = varn[c(16:24,14:19)]),col.ind = Dataset$variety)

# El tercero contiene las variables asociadas a IntercalaciÛn del cultivo, pr·ctica de zoqueo, si hay animales cerca al cultivo, si pasan rios cerca y si hace nutriciÛn del cultivo, conocimientos ancestrales, entre otras.
x11()
fviz_mca_biplot(acm,repel = T,select.var = list(name = varn[c(25:46,22:29)]),col.ind = Dataset$variety)

# El cuarto las contiene todas las variables
x11()
fviz_mca_biplot(acm,repel = T,col.ind = Dataset$variety)



####################################################################################
########################### Clustering (AgrupaciÛn) ################################
####################################################################################

# Realiza el proceso de clusterizaciÛn

res.MCA<-MCA(qualitative_dataset,ncp=2,graph=FALSE) # Se realiza de nuevo un ACM que solo guarde 2 dimensiones
res.HCPC<-HCPC(res.MCA,nb.clust=3,consol=FALSE,graph=FALSE) # Se realiza el proceso de agrupamiento
res.HCPC$data.clust$clust ## Extrae la variable cluster como un vector

res.HCPC$data.clust$bioaggressors ## Extrae la variable cluster como un vector

# Gr·ficos 
# Dendograma
x11() 
plot.HCPC(res.HCPC,choice='tree',title='Dendrograma')
# Mapa solo del cluster 
x11()
plot.HCPC(res.HCPC,choice='map',draw.tree=FALSE,title='Cluster in the factorial map')
## Mapa con cluster y adem·s todas las variables (MAPA M¡S COMPLETO)
x11()
fviz_mca_biplot(acm,repel = T,col.ind = res.HCPC$data.clust$variety, title = "Overlapping and clustered factorial plane", palette = c("black","green","blue"))

x11()
fviz_mca_biplot(acm,repel = T,col.ind = res.HCPC$data.clust$bioaggressors, title = "Overlapping and clustered factorial plane", palette = c("black","green","blue"))

# Herramienta web
Factoshiny(acm)

####################################################################################
########################### Arbol de decisiones C4.5 ###############################
####################################################################################
install.packages("rJava")
library(rJava)
install.packages("RWeka")
library("RWeka")

install.packages("lattice")
library(lattice)


install.packages("caret")
library(caret)

view(qualitative_dataset)

view(Dataset)

set.seed(1958)  # set a seed to get replicable results
train <- createFolds(qualitative_dataset$bioaggressors, k=10)
C45Fit <- train(bioaggressors ~., method="J48", data=qualitative_dataset,
                tuneLength = 5,
                trControl = trainControl(
                  method="cv", indexOut=train))

# fit model
fit <- J48(bioaggressors~., data=qualitative_dataset)

# summarize the fit
summary(fit)

# make predictions
predictions <- predict(fit, qualitative_dataset[,1:31])

# summarize accuracy
table(predictions, Dataset$bioaggressors)

# ************ Exportando datos a archivo procesado en csv. *******

write.csv(qualitative_dataset, file="DatasetRCualitativoConbioaggressors.csv",row.names = F)

####################################################################################
# ************ APLICACI”N ALGORITMO DE REGLAS DE ASOCIACI”N APRIORI *******
####################################################################################

# Leyendo el conjunto de datos cualitativo

getwd()


Dataset <- read.csv("~/QualitativeDatasetEnglishCSV.csv", header=T, colClasses="factor")
DatasetSelAttrib <- read.csv("~/QualitativeProcessedDataSelectAttribute.csv", header=T, colClasses="factor")
DatasetCC <- read.csv("~/CCQualitativeProcessedData.csv", header=T, colClasses="factor")
DatasetLP <- read.csv("~/LPQualitativeProcessedData.csv", header=T, colClasses="factor")
DatasetOC <- read.csv("~/OCQualitativeProcessedData.csv", header=T, colClasses="factor")

library(arules)

qualitative_dataset <-as.data.frame(Dataset)
qualitative_dataset_sel_attrib <-as.data.frame(DatasetSelAttrib)
qualitative_dataset_cc <-as.data.frame(DatasetCC)
qualitative_dataset_lp <-as.data.frame(DatasetLP)
qualitative_dataset_oc <-as.data.frame(DatasetOC)

view(qualitative_dataset)
view(qualitative_dataset_sel_attrib)
view(qualitative_dataset_cc)
view(qualitative_dataset_lp)
view(qualitative_dataset_oc)

summary(qualitative_dataset)

#Visualiza las variables 1 al 12
#labels(qualitative_dataset[1:12])

# Se muestran los 20 items que se repiten m·s veces ordenados de mayor a menor
#itemFrequencyPlot(qualitative_dataset, topN = 20)

# se aplica el algoritmo
rules <- apriori(qualitative_dataset, parameter = list(support=0.001,confidence=0.5),appearance = NULL,control = NULL)
rules <- apriori(qualitative_dataset, parameter = list(support=0.001,confidence=0.5),appearance = NULL,control = NULL)

rules <- apriori(qualitative_dataset, parameter = list(support=0.001,confidence=0.5, minlen=4,maxlen=6),appearance = NULL,control = NULL)
rules_broca <- apriori(qualitative_dataset, parameter = list(support=0.01,confidence=0.6, minlen=4,maxlen=6),appearance = list(rhs=c("bioaggressors=coffee berry borer")),control = NULL)
rules_mancha <- apriori(qualitative_dataset, parameter = list(support=0.01,confidence=0.6, minlen=4,maxlen=6),appearance = list(rhs=c("bioaggressors=Brown eye spot")),control = NULL)
rules_quema <- apriori(qualitative_dataset_sel_attrib, parameter = list(support=0.01,confidence=0.6, minlen=4,maxlen=6),appearance = list(rhs=c("bioaggressors=derrite o quema del fruto")),control = NULL)

#coffee berry borer
rules_broca_cc <- apriori(qualitative_dataset_cc, parameter = list(support=0.01,confidence=0.6, minlen=4,maxlen=6),appearance = list(rhs=c("bioaggressors=coffee berry borer")),control = NULL)
rules_broca_lp <- apriori(qualitative_dataset_lp, parameter = list(support=0.01,confidence=0.6, minlen=4,maxlen=6),appearance = list(rhs=c("bioaggressors=coffee berry borer")),control = NULL)
rules_broca_oc <- apriori(qualitative_dataset_oc, parameter = list(support=0.01,confidence=0.6, minlen=4,maxlen=6),appearance = list(rhs=c("bioaggressors=coffee berry borer")),control = NULL)

#Brown eye spot
rules_mancha_cc <- apriori(qualitative_dataset_cc, parameter = list(support=0.01,confidence=0.6, minlen=4,maxlen=6),appearance = list(rhs=c("bioaggressors=Brown eye spot")),control = NULL)
rules_mancha_lp <- apriori(qualitative_dataset_lp, parameter = list(support=0.01,confidence=0.6, minlen=4,maxlen=6),appearance = list(rhs=c("bioaggressors=Brown eye spot")),control = NULL)
rules_mancha_oc <- apriori(qualitative_dataset_oc, parameter = list(support=0.01,confidence=0.6, minlen=4,maxlen=6),appearance = list(rhs=c("bioaggressors=Brown eye spot")),control = NULL)

# Se organizan las reglas
rules_broca.sorted <- sort(rules_broca, by="lift")
rules_mancha.sorted <- sort(rules_mancha, by="lift")
rules_quema.sorted <- sort(rules_quema, by="lift")

rules_broca_cc.sorted <- sort(rules_broca_cc, by="lift")
rules_broca_lp.sorted <- sort(rulesbroca_lp, by="lift")
rules_mancha.sorted <- sort(rules_mancha, by="lift")
rules_quema.sorted <- sort(rules_quema, by="lift")

install.packages("arulesViz")
library(arulesViz)
x11()
plot(rules_broca.sorted)

# Se gr·fica reglas de asociaciÛn
x11()
plot(rules)

# Inspeccionar las reglas
inspectDT(rules_broca.sorted[1:30])


inspectDT(rules_mancha.sorted[1:30])

# Plot "Graph-based visualization" de las reglas coffee berry borer
plot(rules_broca.sorted[1:30], method = "graph", engine = "htmlwidget")


# Plot "Graph-based visualization" de las reglas Brown eye spot
plot(rules_mancha.sorted[1:30], method = "graph", engine = "htmlwidget")


# Plot "Graph-based visualization" de las reglas coffee berry borer
plot(itemsets[1:30], method = "graph", engine = "htmlwidget")

# Plot "Matrix" de las reglas
plot(rulesbroca.sorted[1:30], method = "matrix", engine = "htmlwidget")

# Plot "Grouped-matrix" de las reglas
plot(rulesbroca.sorted[1:30], method = "grouped matrix", engine = "interactive")

#guardar graficos hml
saveAsGraph(rulesbroca.sorted[1:30], file = "coffee berry borer_rules.graphml")


inspect(rulesbroca.sorted[1:10])
inspect(rules_mancha.sorted[1:10])
inspect(rules_quema.sorted[1:6])

inspect(rules.sorted[1:20])
inspect(rulesbroca[1:20])


subrules <- subset(rules, variables %in% "bioaggressors")
subrules

# Se inspecciona las reglas organizadas
inspect(rules.sorted[1:5])
inspect(rulesbroca[1:10])
inspect(rules_mancha.sorted[1:10])

# ************ ALGORITMO FP-GROWTH ***************
install.packages("rCBA")
install.packages("rJava")
library(rJava)
library(rCBA)
rulesFpGrowth <- rCBA::fpgrowth(qualitative_dataset, support = 0.01, confidence = 0.6, maxLength = 5, consequent = bioaggressors, verbose = TRUE, parallel = TRUE)

# ************ ALGORITMO ECLAT ***************
itemsets <- eclat(qualitative_dataset, parameter = list(support=0.01,maxlen=6),control = NULL)

rulesEclat <- ruleInduction(itemsets,qualitative_dataset,confidence=0.6)

rulesEclat

inspectDT(itemsets[1:30])
