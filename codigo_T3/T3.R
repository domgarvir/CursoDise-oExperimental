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

## Tsudent para 2 mustras independientes
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
#+   theme(text = element_text(size=8), plot.subtitle = element_text(size=8))
#+   #Cuando hay OUTLIERS usaremos la prueba ROBUSTA de YUEN : pruebas apramétricas de medias recortadas
##vemos la diferencia en la media rcortada. Aqui la prueba va a ser BILATERAL (decimos simplemente
#+#q es DIFERENTE, sin especificar direccion
#+#exploracion
hsb2 %>%  group_by(gender) %>% filter(between(write,
                                              quantile(write,0.1),
                                              quantile(write,0.9))) %>% 
  get_summary_stats(write,type = "mean_sd")














