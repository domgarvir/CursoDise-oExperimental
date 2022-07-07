library(openintro)
library(gmodels)

#Hacer tabla histograma para una columna, cd ocurrencia.
CrossTable(hsb2$prog)

#Hacer tabala cruzada, o de incidencia, vale con poner las dos columnas
CrossTable(hsb2$prog, hsb2$gender)


#Con tidyverse
library(tidyverse)
hsb2 %>% group_by(prog) %>% summarize(n = n())
hsb2 %>% group_by(prog, gender) %>% summarize(n = n())

#podemos transformar variables de chr a factor asi:
hsb2$gender2 <- as.factor(hsb2$gender)

### COMPARANDO PROPORCIONES
#1 MUESTRA
library(ggstatsplot)
ggpiestats(data=hsb2,x=gender, bf.message = FALSE)

#Tabla con gmodels
CrossTable(hsb2$gender)

#Prueba de proporciones apra 2 muestras no relacionadas
#con ggstats
ggbarstats(data=hsb2, x=gender, y=schtyp, bf.message = FALSE)

#congmodels y tablas de contingencia
CrossTable(x=hsb2$gender,y=hsb2$schtyp, prop.r = FALSE,prop.t = FALSE,prop.chisq = FALSE,chisq = TRUE)
#la correcion de YAtes es pq la chi² tiende a dar más falsos positivos, asi q debemos coregirlo. 
#Ojo, esto por supuesto siempre está sujeto a los supuestos de Chi²: 
#cada dato es independiencte (OK, es una persona distinta)
#frecuencia esperada mayor que 5 sino usar a) pruyeba exacta de Fisher, de esta forma, en lugar de la aprox que hemos usado antes.
CrossTable(x=hsb2$gender,y=hsb2$schtyp, prop.r = FALSE,prop.t = FALSE,prop.chisq = FALSE,digits = 2,fisher = TRUE)
#b) reagrupar, veamos un ejemplo
library(plyr)
hsb2$race2 <- mapvalues( hsb2$race, from = c("african american", "asian", "hispanic", "white"), to = c(rep("others", 3), "white" ))
table(hsb2$race2)
CrossTable(x=hsb2$schtyp,y=hsb2$race2,prop.r = FALSE,prop.t = FALSE,prop.c = FALSE,prop.chisq = FALSE)

#+2 muetras no relacionadas, por ejemplo por tipo de programa: x= vaiable q separamos, y=el numero de barras, aqui los diferentes programas.
ggbarstats(data=hsb2,x=schtyp,y=prog,bf.message = FALSE)

#con tabal de contingencia: por columnas #prop.c=TRUE
CrossTable(x=hsb2$schtyp,y=hsb2$prog,prop.r = FALSE,prop.t = FALSE,prop.chisq = FALSE,chisq = TRUE)

#ahora apra ve r las diferencias entre programas usaremos pruebas post-hoc
#Esto te compara las 3 proporciones, es decir als tres columnitas y solo ve diferncias entre la acadeimca y la vocacional
library(rstatix)
pairwise_prop_test(table(hsb2$schtyp,hsb2$prog))

#AHORA CON MUESTRAS RELACIONADAS
#ej, opinion del electorado sobre el presidnte antes y despues de aprobar el presupuesto
Performance <- matrix(c(794,86,150,570),nrow=2,dimnames = list("Survey1st"=c("App.","Dissap."), "Survey2nd"=c("App.","Dissap.")))
Performance
survey_df <- as.data.frame(as.table(Performance))
ggbarstats(data=survey_df,x=Survey1st,y=Survey2nd,counts=Freq,paired=TRUE,bf.message = FALSE)
#para obtener los datos disgregados, es con crosstable
CrossTable(Performance,prop.r = FALSE,prop.t = FALSE,prop.chisq = FALSE,mcnemar = TRUE)
#La prueba de McNemar se utiliza para determinar si existen diferencias en una variable dependiente dicotómica entre dos grupos relacionados. 

#PARA +2 MUESTRAS RELACIONADAS usaremos la Q de Cochran
respuesta <- c(1,0,1,0,0,1,0,1,0,0,1,1,1,0,1,0,1,0,1,0,1,1,1,0,0,1,0,1,0,1)
Sujeto <- c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,8,8,8,9,9,9,10,10,10)
Canal <- factor(rep(c("inicial","anuncio","internet")))
datos <- data.frame(Sujeto,Canal,respuesta)
datos
levels(datos$Canal) <- c("inicial","anuncio","internet")
ggbarstats(data=datos,x=respuesta,y=Canal,paired = TRUE,bf.message = FALSE)
#como este paquete aun no da los resultados los tenemos q sacar usando statrix
cochran_qtest(datos,respuesta ~Canal|Sujeto)
#como p es 0.90 no es estadisticamente significativo, si diese significativo tendriamos q mirar cuando, apra lo q usamos la de mcnemar
pairwise_mcnemar_test(datos,respuesta ~ Canal|Sujeto)

#############################################################

######### COMPARACIONES DE MEDIAS ###########################
#Comenzamos explorando los datos
hsb2 %>% get_summary_stats(write,type="mean_sd")
library(ggpubr)
ggboxplot(y="write", data=hsb2,add=c("mean"),add.params=list(color='red'))

#evaluemos los supuestos del modelo, ya q estamos con una prueba apramétrica
#outliers
hsb2 %>% identify_outliers(write)
#distribución normal (ok, p value no significativo)
hsb2 %>%  shapiro_test(write)
ggqqplot(hsb2,x="write")

#vamos a hacer un t-test
t_test(write ~1,data=hsb2, mu=50) #es significativamete diferente de 50 puntos, pero cuanto?
cohens_d(write ~1,data=hsb2,mu=50) #0.2, así que es pequeña

#vamos a ahcer la de Yuen (no tiene en cuenta outliers)
hsb2 %>% filter(between(write,quantile(write,0.1),quantile(write,0.9))) %>% get_summary_stats(write,type="mean_sd")
hsb2 %>% filter(between(write,quantile(write,0.1),quantile(write,0.9))) %>% ggboxplot(y="write",add=c("mean"),add.params=list(color='red'))
#si los comprobamos los datos siguen sin ser normales, asi que no mola mucho usar esta prueba
library(DescTools)
YuenTTest(x=hsb2$write,mu=50)
#de nuevo vemos que la medai recortando los 20% de datos extremos sigue si ser similar a 50 puntos
#el tamaño del efecto no existe apra la media recortada

#PRUEBA NO PARAMETRICA de WILCOXON para 1 MEUSTRA - comparamos la mediana
#ahora exploremos la mediana
hsb2 %>% get_summary_stats(write,type="median_iqr")
ggboxplot(y="write", data=hsb2,add=c("mean"),add.params=list(color='red'))
#veamos la asimetria de los datos con el histograma
gghistogram(hsb2, x="write", y="..density..",fill="steelblue",add_density = TRUE)
#ajustmos el modelo - rstatic
wilcox_test(write~1,data=hsb2,mu=50) #de nuevo nos da signficativamente diferente.
#esta pruab usa el orden y no los valores reales, por eso no es parametrica.
#calculamos el tamaño del efecto
wilcox_effsize(write ~1,data=hsb2,mu=50)#de nuevo da una diferencia pequeña
#representamos el resultado 
gghistostats(x=write,data=hsb2,test.value = 50,type = "nonparametric",centrality.parameter = "nonparametric",test.value.size=TRUE,test.value.line=TRUE,bf.message=FALSE_)

## Tsudent para 2 mustras independientes ###############
hsb2 %>% group_by(gender) %>% get_summary_stats(write, type="mean_sd") #get_summary_stats es de ggpub
ggboxplot(x="gender", y="write",data=hsb2, add=c("mean"), add.params = list(color="red"))
#evaluando los supuestos: una prueba paramétrica asume q no hay outliers, y normalidad
#identificamos los aoutliers así:
hsb2 %>% group_by(gender) %>% identify_outliers(write) #identify_outliers es de rstatix
#homogeneidad de varianza: levene test
hsb2 %>% levene_test(write ~ gender) #rstatix . El pvalues <0.05 la variabilidad no es homogenea en ambos grupos!
#tWelch vs Tstudent: cuando la varianza no es homogenea se usa mejor la t de welch
#chequo de normalidad de distribución
hsb2 %>% group_by(gender) %>% shapiro_test(write) #rstatix <0.05 ambas -> no normal
ggqqplot(hsb2, x="write", facet.by = "gender") # aqui se ve que no es mu normal 
#¿Cómo ajustar un modelo de evaluar tstudent? rstatix
t_test(write ~gender , data=hsb2, var.equal = FALSE)# p<0.05 -> no iguales
# Calculando el tamaño de efecto: prueba d cohen
cohens_d(write ~ gender, data = hsb2)# tamaño 0.5 es moderado
#para comunicar los resultados hacemos un daigrama de cajas modificado
ggbetweenstats(x=gender, y=write, data = hsb2, bf.message = FALSE) 

#Cuando hay OUTLIERS usaremos la prueba ROBUSTA de YUEN : pruebas apramétricas de medias recortadas
##vemos la diferencia en la media rcortada. Aqui la prueba va a ser BILATERAL (decimos simplemente
#+#q es DIFERENTE, sin especificar direccion
#+#exploracion
hsb2 %>%  group_by(gender) %>% filter(between(write,
                                              quantile(write,0.1),
                                              quantile(write,0.9))) %>% 
  get_summary_stats(write,type = "mean_sd")
#diagrama box
hsb2_recortado <- hsb2 %>%  group_by(gender) %>%  filter(between(write,
                                               quantile(write, 0.1),
                                               quantile(write, 0.9))) 
ggboxplot(data=hsb2_recortado,x="gender", y="write", add=c("mean"), add.params = list(color="red"))
#evaluamos de nuevo los supuestos del modelo.
#prueba de levene -> igualdad de varianzas entre dos variables
hsb2_filtrado <- hsb2 %>%  filter(between(write,quantile(write,0.1),quantile(write,0.9)))
#hsb2 %>%  filter(between(write,quantile(write,0.1),quantile(write,0.9))) %>% levene_test(write ~ gender)
hsb2_filtrado  %>% levene_test(write ~ gender) #ojo el levene mete la separacion en la fucion
#shapiro test: normalida d de la distribuciones. No se cumple
hsb2_recortado %>% shapiro_test(write) #para el shapiro tenemos q meterla antes
hsb2_filtrado %>% ggqqplot(x= "write", facet.by = "gender")
# Prueba de Yuen: nos dice si es significativamente difernte o no
YuenTTest(write ~ gender, data=hsb2) #esta te recorta por defecto el 20% #DescTools# resultado significativo
#Tamaño del efecto de la prueba de yuen
library(WRS2)
yuen.effect.ci(write ~gender, data=hsb2,boot=10) # resulta 0.35: efewcto moderado
#comunicación de resultado
ggbetweenstats(x=gender,y=write, data=hsb2, tr=0.2, type="r", bf.message = FALSE)
#hay un problema en el gráfico y me está poiendo delta_R^AKP=0.58 en lugar del eta=0.35 del efecto de tamaño calculado justo ants

#### PRUEBAS NO PARAMETRICAS #########
#cuando los datos no siguen distribución normal usamos pruebas no parametricas y tnemos 2 grupos
#se llama prueba U de MannWhitney
#xplorando los datos: calculamos la MEDIANA
hsb2 %>%  group_by(gender) %>% get_summary_stats(write, type="median")
#iqr es el rango inter cuartial, y da idea de la varianza
ggboxplot(x="gender", y="write", data=hsb2, add = c("median"), add.params = list(color="red"))
#evaluamos los supuestos: supoemos q las distribucioens son simetricas. lo miramos con el histograma
gghistogram(hsb2, x="write",add="median", rug = TRUE, bins=15, color="gender", fill="gender", palette="Kark2")
#el test lo hacemos con la funcion wilcox
wilcox_test(write ~gender,data=hsb2) #esto nos dice qu son DIFERENTES p>0.05
#tamaño del efecto
wilcox_effsize(write ~ gender, data=hsb2)# tamaó0.23, small
#resultados
ggbetweenstats(x=gender, y=write, data=hsb2, type="np", bf.message = FALSE)
# de nuevo me esta imprimiendo el r_biserial^rak(0.27) y o el effect size de yuen, q es 0.235

########## 2 MUESTRAS RELACIONADAS ###############
#veamos los resultados en lectura y escritura de los estudiantes (relacionadas pq hay 2 datos x estudiante)
#Buscamos saber si la media en lectura es diferent a la media en escritura
#PARAMÉTRICA
hsb2 %>% dplyr::select(write,read) %>% get_summary_stats(type="mean_sd")
#para plotar esto en ggplot hay q modificar los datos y ponerlos e formato long
hsb2_long <- hsb2 %>%  pivot_longer(c(write,read), names_to="test", values_to = "score") %>% arrange(test,id)
ggboxplot(x="test", y="score", data=hsb2_long, add=c("mean"), add.params = list(color="red"))
# buscamos aoutliers - recuerda q nos fijamos e la diferencia entre las notas write y read
hsb2 <- hsb2 %>% mutate(differences=read-write)
hsb2 %>% identify_outliers(differences)
# test de normalidad de shapiro
hsb2 %>% shapiro_test(differences) #no son diferentes a la normalidad
ggqqplot(hsb2,"differences")
# queda la de igualdad de dispersion, prueba de levene
#ojo! como son muetras de los mismos sujetos entendemos que tienen la mima varianza!!
#me aprece una mierda de suposición ya q al medir cosas diferentes puede estar distribuidas de forma diferente
hsb2_long %>% levene_test(score ~ test)
# el test: recuerda poner q estan relacionados
t_test(score ~test, data=hsb2_long, paired = TRUE ) # 0.3 No significativamente diferentes
#tamaño del efecto
cohens_d(score ~ test, data=hsb2_long, paired = TRUE) #negligible
#presentacion. Al ser relacionadas usamos ggwithinstats
ggwithinstats(x=test,y=score, data=hsb2_long, bf.message = FALSE)
#este es aun peor porque no me está escribiendo el mensaje!!

### PRUEBA ROBUSTA YUEN
#ara tenemos q filtrar el 20% de extremos
hsb2 %>%  dplyr::select(write, read) %>% filter_all(all_vars(between(.,quantile(.,.1),quantile(.,.9)))) %>% get_summary_stats()

hsb2_long %>% filter(between(score,
                        quantile(score, 0.1),
                        quantile(score, 0.9))) %>% ggboxplot(x="test", y="score", add=c("mean"), add.params = list(color="red"))
#test shapiro
hsb2 %>% filter(between(differences,
                        quantile(differences,0.1),
                        quantile(differences,0.9))) %>% shapiro_test(differences)
#qqplot
hsb2 %>% filter(between(differences,
                        quantile(differences,0.1),
                        quantile(differences,0.9))) %>% ggqqplot(x="differences") #no se cumple noramlidad (auqnue sea robusta la normalidad se pide
#test de yuen
YuenTTest(x=hsb2$read,y=hsb2$write, paired=TRUE)#YuenTTest(write ~ gender, data=hsb2) # p 0.009 -> diferentes
YuenTTest(score ~ test, data=hsb2_long, paired = TRUE)
#chapucerismo extremo, por que seescrib diferente ahora solo por ser relacionadas!?!?!?!?!
#tamaño del efecto
dep.effect(x=hsb2$read,y=hsb2$write,tr=0.2,nboot=10)#WRS2; 
#de aqui nos quedamos con el Est de AKP (-0.06), osea lo q sería un efecto de mierda, no?
#comunica resultados
ggwithinstats(x=test, y=score, data=hsb2_long, tr=0.2, type="r", bf.message = FALSE)
#este si me da bien, me da la delta AKP

###### 2 MUESTRAS RELACIONADAS NO PARAMETRICAS
hsb2 %>%  dplyr::select(write,read) %>% get_summary_stats(type = "median_iqr")
ggboxplot(x="test", y="score", data=hsb2_long, add=c("median"), add.params = list(color="red"))
#supuestos: no outliers, distribucion simetrica
hsb2 %>% gghistogram(x="differences", y="..density..", fill="steelblue", add_density = TRUE)
#test wilcox
wilcox_test(score ~ test, data=hsb2_long, paired = TRUE) #no significativo
#tamaño efecto
wilcox_effsize(score ~test, data=hsb2_long, paired = TRUE) # 0.06, small
#resultados
ggwithinstats(x=test,y=score, data=hsb2_long, ggstastplot.layer=FALSE, messages=FALSE, typ="np", bf.message = FALSE)
#de nuevo me sale el r_biserial_rank q es -0.08, en lugarr de0.06

############################
rm(list = ls())
library(ggstatsplot)
library(rstatix)
library(ggpubr)
library(DescTools)
library(WRS2)
library(tidyverse)
#######ejercicios obligatorios del T3
# 1 Impuestos
datos <- data.frame(name=c("impuestos","servicios"), count=c(624, 1200-624))
datos
ggpiestats(datos, x=name, counts=count, bf.message=FALSE)
#es el 52% suficientemente diferente del 50 con 1.200 datos?
#la estadistica no de ggpiestats nos dice q no es significativo (P=0.17). 
#La orueba apra proporcio de una muestra es : La prueba de proporción para una muestra 

#2 genetica y crimen
Convictions <-matrix(c(2, 10, 15, 3), 
                     nrow = 2, 
                     dimnames = list(c("Dizygotic", "Monozygotic"), 
                                     c("Convicted", "Not convicted")))
#La prueba de independencia Chi-cuadrado y la prueba de Fisher se utilizan para probar la relación 
#entre dos variables categóricas. O dicho de otro modo, examina si las filas y columnas de una tabla 
#de contingencia están asociadas de manera estadísticamente significativa.
as.data.frame(as.table(Convictions))
#para comparar las proporciones e dos muetras usamos gráficos de barras

ggbarstats( data = as.data.frame(as.table(Convictions)), 
            x = Var1, 
            y = Var2,
            counts = Freq)

#Otro ejemplo
M <- as.table(rbind(c(762, 327, 468), c(484,239,477)))
dimnames(M) <- list(gender=c("M","F"),
                    party=c("Democrat","Independent", "Republican"))
M
ggbarstats(data = as.data.frame(M),
           x= gender, y = party, counts = Freq,
           bf.message = FALSE)

C <- as.table(rbind(c(2,15),c(10,3)))
dimnames(C) <- list(twin=c("Dizygotic", "Monozygotic"),
                    Crime=c("Convicted", "Not convicted"))
ggbarstats( data = as.data.frame(C), 
            x = twin, y = Crime, counts = Freq)

#cuando tenemos pocos datos hay q hacer la prueba de Fisher, pero esa no está en ggstats
fisher_test(Convictions, alternative = "less")
#
#3. Abortos inducidos
data(infert, package = "datasets") 
head(infert)
I <- table(infert$education, infert$induced)
#tenemos dos variables cualitativas: educacion y abortos,m cada una con tres niveles.para esto se usa ggbar
ggbarstats(data=infert,x=induced,y=education)
#¿Cómo saber si ahy correlacion entre ambas variables con 3 categorias?
#Como queremos comparar las proporciones para más de 2 muestras, utilizamos la prueba de independencia de Chi-cuadrado
#existe erlacion significativa entre educacion y numero de abortos inducidos, aunque el efecto es pequeño (0.16)
#Para ver diferencias entre categorias ebemos hacer comparaciones multiples pareadas
pairwise_prop_test(M)
#

#4. Artitris
library(vcd)
data(Arthritis) 
head(Arthritis)
table(Arthritis[which(Arthritis$Treatment=="Treated"),5])
datos <- Arthritis %>%
  filter(Treatment == "Treated" & Improved != "Some") %>%
  mutate(Improved = droplevels(Improved)) #borra la categoría fantasma
head(datos)
summary(datos)
datos %>% group_by(Improved) %>% get_summary_stats(Age, type="mean_sd")
#outliers
datos %>% group_by(Improved) %>% identify_outliers(Age)
#encontramos 2 outliers, creo q tendremos q suar el robusto de yuen
#levene
#levene test
datos %>%  levene_test(Age ~ Improved) #es < 0.05 asi q es ligeramente significativa la diferencias de variabilidad
#no hemos chequeao si siguen siendo diferentes cuando recortamos los datos
datos %>% filter(between(Age,quantile(Age,0.1),quantile(Age,0.9))) %>% levene_test(Age ~ Improved) #ahora ya NO son diferentes!!
#shapiro
datos %>% group_by(Improved) %>% shapiro_test(Age) #idem
ggqqplot(datos, x="Age", facet.by = "Improved") # aqui se ve que no es mu normal 
# quitando outliers
datos %>% group_by(Improved) %>%  filter(between(Age,
                                                 quantile(Age, 0.1),
                                                 quantile(Age, 0.9)))  %>% shapiro_test(Age) # no significativos: normalidad


#yo diria q lo mejor es una de Yuen, q es aprametrica pero sin outliers
#esa se ahce asi:
YuenTTest(Age ~ Improved, data=datos) #esta te recorta por defecto el 20% #DescTools# resultado significativo
#Tamaño del efecto de la prueba de yuen
library(WRS2)
yuen.effect.ci(Age ~Improved, data=datos,boot=10) # resulta 0.35: efewcto moderado
#comunicación de resultado
ggbetweenstats(x=Improved,y=Age, data=datos, tr=0.2, type="r", bf.message = TRUE)
#hay un problema en el gráfico y me está poiendo delta_R^AKP=0.58 en lugar del eta=0.35 del efecto de tamaño calculado justo ants

#5. Cebada
library(MASS)
head(immer)
#tenemos ahora 2 muestras relacionadas, un mismo sitio con dos medidas cada uno. Hagamos test parametrico, robusto, y no parametrico
#primero cambiar a formato alrgo
immer_largo <- immer %>%   pivot_longer(Y1:Y2, names_to = "variable", values_to = "valor") %>% mutate(variable = as.factor(variable))
summary(immer_largo)
#las tres pruebas posibles apra dos muestras relacionadas son:
# outliers
immer <- immer %>% mutate(differences=Y2-Y1)
immer %>% identify_outliers(differences) #no hay
#levene: supuestamente no es necesario al darse por sentado q al ser del mismo sujeto son similares
immer_largo %>% levene_test(valor ~ variable)#efectivamente son similares p=0.43
#shaphiro
immer %>% shapiro_test(differences) #no son diferentes a la normalidad
ggqqplot(immer,"differences")
#puesto que no hay outliers, y cumple los supuestos de normalidad (shapiro P=0.079) optaremos por una prueba aprametrica ttest
ggwithinstats(data = immer_largo, 
              x = variable,
              y = valor,
              type = "p")  #INDICA
