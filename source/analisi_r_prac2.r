# Instal.lem / activem tots els packages necessaris per la nostra anàlisi

if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('corrplot')) install.packages('corrplot'); library('corrplot')
if (!require('caret')) install.packages('caret'); library('caret')
if (!require('randomForest')) install.packages('randomForest'); library('randomForest')

# Modificar segons la ruta local on es tingui el fitxer 
# setwd('C:\\Users\\Joan\\Desktop\\UOC Data Science\\Tipologia i Cicle de Vida de les Dades\\PRAC2\\Dataset proposat')

df <- read.csv('heart.csv', sep=',')

# ---------- APARTAT 1 ---------- #

summary(df)
nrow(df) # Tenim 303 registres al nostre joc de dades

# Mirem si tenim algun valor nul

any(is.na(df)) # Observem que no hi ha valors nuls al joc de dades

# Ens centrem en les dades categòriques. Observem que al no tenir valors nuls, ens hem de centrar en trobar

noms_columnes <- colnames(df)

# Variables qualitatives i categòriques. Exploracio de les distribucions

# Categoriques

# Sex

barplot(table((df$sex)))

ggplot(data=df, aes(x=sex)) + geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7) )


# CP

barplot(table(df$cp))
ggplot(data=df, aes(x=cp)) + geom_bar(color="black", fill=rgb(0.4,0.4,0.4,0.4) )

# Restecg

barplot(table(df$restecg))
ggplot(data=df, aes(x=restecg)) + geom_bar(color="black", fill=rgb(0.5,0.8,0.4,0.4) )

# slp

barplot(table(df$slp)) # Tenim 21 valors informats a 0, ?s a dir, nuls. Aprox 7% mostra
ggplot(data=df, aes(x=slp)) + geom_bar(color="black", fill="orange" )

# caa

barplot(table(df$caa))
ggplot(data=df, aes(x=caa)) + geom_bar(color="black", fill="grey" )

# thall

barplot(table(df$thall)) # Tenim dos valors faltants
ggplot(data=df, aes(x=thall)) + geom_bar(colour="black", fill="red")

# output

barplot(table(df$output))
ggplot(data=df, aes(x=as.factor(output))) + geom_bar(colour="black", fill="blue")

# fbs

barplot(table(df$fbs))
ggplot(data=df, aes(x=as.factor(fbs))) + geom_bar(colour="black", fill="purple")

# exng

barplot(table(df$exng))
ggplot(data=df, aes(x=as.factor(exng))) + geom_bar(colour="black", fill="black")

# Variables quantitatives

# Age

summary(df$age) # Ok, sense valors extrems

hist(df$age)

# trtbps

summary(df$trtbps) # Ok, sense valors extrems

hist(df$trtbps)

# chol

summary(df$chol) # "Ok"

ggplot(data = df, aes(x = chol)) + geom_histogram()

df[df$chol > 500,] # Mostrem l'outlier

# thalachh

summary(df$thalachh) # Ok

hist(df$thalachh)

# oldpeak

summary(df$oldpeak) # Ok

hist(df$oldpeak)

# Apartat 3 (NETEJA DE LES DADES)

######## Eliminem registre amb chol > 500, tal i com hem motivat al report

df_def <- df[df$chol < 500,]

# Apartat 2 (INTEGRACIÓ I SELECCIÓ DE LES DADES)

####### Eliminem variables descartades per a l'analisi

df_def <- df_def[!(colnames(df_def) %in% c('restecg', 'thall', 'slp', 'oldpeak'))]

###### Correlacio entre variables continues. Fem un plot de correlacio

cont_var <- c('age', 'trtbps', 'chol', 'thalachh')

# Apartat 4 (ANÀLISI DE LES DADES)

# Efectuem contrastos d'hipotesis per tal de veure si les variables continues escollides son distribuides
# normalment

for(var in cont_var){
  print(var)
  print(shapiro.test(df_def[, var]))
}

#### En cap cas podem suposar homogeneitat de la variancia. A continuacio hem de fer 
#### el test Fligner-Killeen per tal de comprovar homoscedasticitat de les variables
#### Ho compararem amb la variable target, 'output', i amb la variable 'sex'

for(var in cont_var){
  print(var)
  print(fligner.test(x=list(df_def[,var], df_def[,'output'])))
}

for(var in cont_var){
  print(var)
  print(fligner.test(x=list(df_def[,var], df_def[,'sex'])))
}

#### Hem d'aplicar el test de correlacio de Spearman, és a dir, no paramètric

cont.data.cor <- cor(df_def[,cont_var], method = c("spearman"))
print(cont.data.cor)


corrplot(cont.data.cor, method="pie")

ggplot(data=df_def, aes(x=thalachh, y=age)) + geom_point(size=2, shape=4, colour='blue') +
                                              geom_smooth(method=lm, se=FALSE, linetype="dashed", color="black")


#### Variables discretes
disc_var <- c("sex", "exng", "caa", "cp", "fbs", "output")




var_list <- list()
valid_test_list <- list()
p_val_list <- list()

for(i in seq_along(disc_var)){
  for(j in seq_along(disc_var)){
    var_1 <- df_def[, disc_var[i]]
    var_2 <- df_def[, disc_var[j]]
    test_1 <- chisq.test(var_1, var_2)
    p_val_list <- append(p_val_list, test_1$p.value)
    valid_test_list <- append(valid_test_list, 
                              ifelse(any(as.matrix(test_1$expected)<5),
                                     "VALID", "INVALID")) # Si les freq esperades no son >= 5 no podem aplicar-lo
    var_list[[length(var_list) + 1]] <- c(disc_var[i], disc_var[j])
  }
}

# Reconvertim les variables abans de desar-les a un dataframe

var_1 <- unlist(lapply(var_list, function(x){return(x[[1]])}))
var_2 <- unlist(lapply(var_list, function(x){return(x[[2]])}))

# Creem el dataframe

df_disc_var <- data.frame(var_1, var_2, unlist(valid_test_list), unlist(p_val_list))
colnames(df_disc_var) <- c("variable_1", "variable_2", "validesa_test", "p_valor")

# Eliminem els registres que comparen una mateixa variable

df_disc_var <- df_disc_var[!(df_disc_var$variable_1==df_disc_var$variable_2),]

# Ordenem segons p-valor per tal de poder eliminar els registres repetits
# (correlacio entre A i B es igual a correlacio entre B i A; per cada parell de variables
#  hi ha dos registres i amb un es suficient)

df_disc_var <- df_disc_var[(order(df_disc_var$p_valor, decreasing=TRUE)),]
row.names(df_disc_var) <- NULL
rows_to_del <- seq(from = 1, to = nrow(df_disc_var), by = 2)

df_disc_var <- df_disc_var[-(rows_to_del),]

df_disc_var_valid <- df_disc_var[(df_disc_var$validesa_test=="VALID"),]
df_disc_var_invalid <- df_disc_var[(df_disc_var$validesa_test=="INVALID"),]

# En el cas de les parelles de variables invalides, hem de fer el test exacte de Fisher
# Son els casos en q alguna freq esperada era < 5
p_valor_fisher <- list()
for(i in seq_along(df_disc_var_invalid$variable_1)){
  test_fisher <- fisher.test(table(df_def[,df_disc_var_invalid$variable_1[i]],
                                   df_def[,df_disc_var_invalid$variable_2[i]]))
  p_valor_fisher <- append(p_valor_fisher, test_fisher$p.value)
  
}

# Substituim p valors

df_disc_var_invalid$p_valor <- unlist(p_valor_fisher)

# Ja tenim l'analisi d'independencia de les variables discretes fet

df_disc_var_invalid$validesa_test <- rep("Test de Fisher", nrow(df_disc_var_invalid))
df_disc_var_valid$validesa_test <- rep("Test de Chi quadrat", nrow(df_disc_var_valid))
df_correl_disc_def <- rbind(df_disc_var_invalid, df_disc_var_valid)
row.names(df_correl_disc_def) <- NULL
write.csv(df_correl_disc_def, 'df_cor_disc.csv') # He desat aquest df en local per poder fer
# la taula per al report mes facilment.



# --------------------------------------------------------------------------------- #

# Comparació entre grups

# Categòriques (sex (2), cp (4), fbs (2), caa (4)) + output
# Contínues (age, trtbps, chol, thalachh)


# Realitzem el test de Kruskal Wallis per veure si hi ha diferències entre grups en les 
# variables categòriques respecte les contínues.


# Creem una taula per a emmagatzemar-hi els resultats
resultats_kruskal <- data.frame(variable_cat = character(),
                                variable_cont = character(),
                                p_value = numeric(),
                                stringsAsFactors = FALSE)

# Bucle per realitzar el test de Kruskal Wallis per totes les combinacions
for (var_cat in disc_var) {
  for (var_cont in cont_var) {
    formula <- as.formula(paste(var_cont, "~", var_cat))
    resultats <- kruskal.test(formula, data = df_def)
    p_value <- resultats$p.value
    nou_resultat <- data.frame(variable_cat = var_cat,
                               variable_cont = var_cont,
                               p_value = p_value,
                               stringsAsFactors = FALSE)
    
    resultats_kruskal <- rbind(resultats_kruskal, nou_resultat)
  }
}

# Afegim nova columna per veure si la diferència és significativa o no
resultats_kruskal$'Resultat test' <- ifelse(resultats_kruskal$p_value < 0.05, 
                                     "Diferències significatives entre grups", "H0 certa")


# Imprimim els resultats
resultats_kruskal


# Guardem la taula per extreure-la fàcilment i posar-la al report
write.csv(resultats_kruskal, 'kruskal.csv')




# Realitzem la prova post hoc amb l'ajust de Bonferroni per veure les diferencies entre grups per a variables categòriques amb més de 2 categories

resultats_bonferroni <- pairwise.t.test(df_def$age, df_def$caa, p.adjust.method = "bonferroni")

resultats_bonferroni

resultats_bonferroni <- pairwise.t.test(df_def$thalachh, df_def$caa, p.adjust.method = "bonferroni")

resultats_bonferroni

resultats_bonferroni <- pairwise.t.test(df_def$thalachh, df_def$cp, p.adjust.method = "bonferroni")

resultats_bonferroni

resultats_bonferroni <- pairwise.t.test(df_def$age, df_def$cp, p.adjust.method = "bonferroni")

resultats_bonferroni

resultats_bonferroni <- pairwise.t.test(df_def$chol, df_def$caa, p.adjust.method = "bonferroni")

resultats_bonferroni


# ------------------------------------------------------------------------------------

# Anem a fer regressió logística, que va bé per predir variables dicotòmiques com la nostra (output)

# Primer forcem totes les variables categòriques per tenir una regressió més acurada

df_def$sex <- as.factor(df_def$sex)
df_def$exng <- as.factor(df_def$exng)
df_def$caa <- as.factor(df_def$caa)
df_def$cp <- as.factor(df_def$cp)
df_def$output <- as.factor(df_def$output)


# Ara construirem el model amb totes les variables que estaven significativament
# relacionades amb la variable output, le nostre target.


model_log <- glm(output ~ age + sex + exng + cp + caa + chol + thalachh + trtbps, data = df_def, family = binomial(link = "logit"))

summary(model_log)


# Una vegada mirem el summary, es veu que la variable age no té una relació
# significativa amb la nostra variable dependent en el context d'aquesta 
# regressió logística.

# Primer anem a treure la variable age del model i a veure si millora l'indicador AIC
# en aquest primer model és de 245.64, per millorar hauria de baixar.


model_log <- glm(output ~ sex + exng + cp + caa + chol + thalachh + trtbps, data = df_def, family = binomial(link = "logit"))

summary(model_log)

# Podem veure que sí que ha baixat l'indicador AIC fins a 244.65, és per això
# que aquest segon és un millor model.


# Anem a veure com funciona predint l'output en el mateix dataset



predictions <- ifelse(predict(model_log, type = "response") > 0.5, 1, 0)


# Creem la matriu de confusió per veure com funciona
confusionMatrix(as.factor(predictions), as.factor(df_def$output))

# Està bastant bé, una accuracy de 0.8245


# Però és possible que sigui una mètrica bona perque el model esta overtfitted
# Estem fent un testeig sobre tota la mostra d'entrenament.

# Per això crearem dos datasets aleatoris (train i test) i entrenarem el model una altra vegada


set.seed(44)  # Per tal que sempre sigui igual

indices <- createDataPartition(df_def$output, p = 0.7, list = FALSE) # 30% test

train_data <- df_def[indices, ]
test_data <- df_def[-indices, ]


# Tornem entrenar el model només amb les dades de train
model_log <- glm(output ~ sex + exng + cp + caa + chol + thalachh + trtbps, data = train_data, family = binomial(link = "logit"))

summary(model_log)


# Provem a veure com ho ha fet el model amb les dades test
# Noti's que l'AIC baixa fins a 168.63

predictions <- ifelse(predict(model_log, newdata = test_data, type = "response") > 0.5, 1, 0)


# Creem la matriu de confusió

confusionMatrix(as.factor(predictions), as.factor(test_data$output))



# Continua predint bastant bé, una accuracy del 80%, això vol dir que no està overfitted



# -------------------------------------------------------------------------------



# Per últim anem a provar un mètode supervisat per tal de predir l'output

# Aprofitant que ja tenim dades train i test, anem al gra, a construir un model de classificació Random Forest
# Es farà amb l'ajuda d'una validació creuada:

set.seed(44)  # Per que sempre sigui igual
train_control <- trainControl(method = 'cv', number = 4) # Per la validació creuada

model_rf <-  train(output ~ ., data=train_data, method="rf", trControl = train_control)

predictions <- predict(model_rf, newdata=test_data)

confusionMatrix(as.factor(predictions), test_data$output)


# Ens surt una accuracy del 80%, bastant bé també



