library(openintro)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(DescTools)
library(WRS2)
library(ggstatsplot)
#ANOVA de 1 via

#La preguta que nos interesa es comparar la calificaion media en escritura de los grupos segun el tipo de programa del isntituto
#Hipotesis nula, todas las medias ighuales
#Hipotesis alterativa al menos una es diferente
data(hsb2)
hsb2 %>%  group_by(prog) %>% get_summary_stats(write, type ="mean_sd")
ggboxplot(hsb2, x="prog", y="write", orientation="horizontal")
#outliers
hsb2 %>%  group_by(prog) %>% identify_outliers(write) #encuentra 1
#normalidad
fit <- lm(write ~ prog, data=hsb2)
ggqqplot(residuals(fit))
shapiro_test(residuals(fit)) # signioficativamente diferente
#lo aplicamos a los residuos del anova o moelo lineal
#homogeneidad de varianza
plot(fit,1) #los residuos de cada grupo parece homogeneos
hsb2 %>% levene_test(write ~prog)#no es significativamente diferente por grupos
#anova test
hsb2 %>% anova_test(write ~prog) # es significativo. Alguno se diferencia del resto. El tamaño del efecto es 0.78(ges)
#¿Como saber cual es el grupo diferente? Post hoc multiple comparison
hsb2 %>% pairwise_t_test(write ~ prog, p.adjust.method = "bonferroni", pool.sd = TRUE)#hemos peusto varianzas iguales=TRUE
#presentar
ggbetweenstats(x=prog,y=write,data=hsb2, p.adjust.method = "bonferroni",bf.message = FALSE, var.equal = TRUE)
 #Ffisher es la prueba anova

#SI NO HAY HOMOGENEIDAD DE VARIANZA: Si esto no se da es un supuesto GRAVE. hay q usar otra prueba
#hay q usar la ANOVA DE WELCH
hsb2 %>%  welch_anova_test(write ~ prog)#el p valor tb es significativo. No = media
#para el post-hoc el cambio lo ahcemos en el pool.sd
hsb2 %>% pairwise_t_test(write ~ prog, p.adjust.method = "bonferroni", pool.sd = FALSE)#hemos peusto varianzas iguales=TRUE
#ahora ahy una que no da significativo!!
#represetar de nuevo con var.equal=FALSE
ggbetweenstats(x=prog,y=write, data=hsb2, p.adjust.method="none",bf.message = FALSE,var.equal = FALSE)   

#cuando no hay NORMALIDAD debemos hacer pruebas NO PARAMETRICAS: Aonva de KRUSKAL WALLIS
#pregunta comparar la distribucion de la calificacion escritura en funcion del tipo de programa
df <-  hsb2
df %>% group_by(prog) %>% get_summary_stats(write,type="median") 
df %>% ggboxplot(x="prog", y="write", orientation="horizontal")
#test kruskal
df %>% kruskal_test(write ~ prog) #significativo
df %>%  kruskal_effsize(write ~ prog)
# ahora las pruebas post hoc apra saber cuales difieren
df %>%  dunn_test(write ~prog, p.adjust.method = "bonferroni")
#resultados
ggbetweenstats(x=prog, y=write, data=df, type="np", bf.message = FALSE, p.adjust.method = "bonferroni")

#Cuando hay outliers: ANOVA ROBUSTA PARAMETRICA, comparamos las medias RECORTADAS
df %>%  group_by(prog) %>% filter(between(write,
                                              quantile(write,0.1),
                                              quantile(write,0.9))) %>% 
  get_summary_stats(write,type = "mean_sd")
df %>%  filter(between(write,
                       quantile(write,0.1),
                       quantile(write,0.9))) %>% ggboxplot(x="prog", y="write", orientation="horizontal")
#al ser prueba aprametrica tenermos q controlar la normalidad
df %>%  group_by(prog) %>% filter(between(write,
                                          quantile(write,0.1),
                                          quantile(write,0.9))) %>% shapiro_test(write) #ups 1 no es normal
  
df %>%  group_by(prog) %>% filter(between(write,
                                          quantile(write,0.1),
                                          quantile(write,0.9))) %>% ggqqplot("write", facet.by = "prog")  
#ahora dice q auque no exista homogeneidad de varianza no tenemos q preocuparnos demasiado???
t1way(write ~ prog, data=df )# da super significativo pero la pava pasa del tema
#para saber entre los grupos q hay diferencia
lincon(write ~ prog, data=df) #encuentra super diferencias entre todos
#resultado
ggbetweenstats(x=prog,y=write,data=df, p.adjust.method = "none", bf.message = FALSE, type="r", tr=0.2)


# ANOVAS PARA MUESTRAS RELACIONADSAS
#diferencias entre matematicas, escritura y lectura
df_long <- df %>% pivot_longer(c(write,read,math), names_to = "test", values_to= "score")
#explore
df %>% dplyr::select(write,read, math) %>% get_summary_stats(type="mean_sd")  
ggboxplot(x="test", y="score", data=df_long, add=c("mean"), add.params = list(color="red"))
# supuestos de pruebas parametricas
#outliers
df_long %>%  group_by(test) %>%  identify_outliers(score) #no hay
#normalidad
df_long %>%  group_by(test) %>%  shapiro_test(score)# no lo pasa
ggqqplot(df_long, "score", facet.by = "test")
#no ha hecho el test de igualdad de varianzas
# anova test de pareados
rs_aov <- anova_test(data=df_long, dv=score, wid=id, within=test) #is mantiene info del sujeto. Whiti e la variable compartida:test
rs_aov$`Mauchly's Test for Sphericity` #no rechaza esfericidad de datos:  existe variablidad similar e las tres
#el resultado del modelo
get_anova_table(rs_aov) # no significativa, el tamaño del efecto es ges, pequeño. #correction="auto" significa q en función de q se cumpla o no el supuesto de esfericidad se utiliza una correccion
#es una correccion de los grados de libertad
#como o es significativo no hay post-hoc
df_long %>% pairwise_t_test(score ~ test, paired = TRUE, p.adjust.method = "bonferroni")
#resultados
ggwithinstats(x=test,y=score, data=df_long,type = "p", bf.message = FALSE, p.adjust.method = "bonferroni")
#nada sale significativo

#La version no aprametrica del anova es la prueba de Friedman
#esto e cuando no cumplen normalidad, o tratamos con datos ordinales
df %>% dplyr::select(write,read, math) %>% get_summary_stats(type="median_iqr")  
#la pava no le pasa ni el shapiro test, pero vamos, q sabemos q no lo pasa
#tampoco aplica el test de esfericidad por ahora
#tst de fridman
df_long %>%  friedman_test(score ~ test |id) #no significativo
#effectsize
df_long %>%  friedman_effsize(score ~test |id) #mu bajo
#post hoc
df_long %>%  pairwise_wilcox_test(score ~ test, paired=TRUE, p.adjust.method = "bonferroni")
#presentacion
ggwithinstats(x=test,y=score, data=df_long, type="np", bf.message = FALSE, p.adjust.method = "bonferroni")

#ANOVA ROBUSTO +2 MUESTRAS RELACIONADAS
#vamoss a recortar los extremos
df %>% dplyr::select(write, read,math) %>% filter_all(all_vars(between(.,quantile(.,.1),quantile(.,.9)))) %>% get_summary_stats()
# supuesto de normalidad
df_long %>% group_by(test) %>% filter(between(score,quantile(score,.1),quantile(score,.9))) %>%  shapiro_test(score) #significativo
df_long %>% group_by(test) %>% filter(between(score,quantile(score,.1),quantile(score,.9))) %>% ggqqplot("score", facet.by = "test")
#el test rmanova del WRS2
rmanova(y=df_long$score, groups = df_long$test,blocks = df_long$id) #sale ligeramente significativo
#prubas post hoc
rmmcp(y=df_long$score, groups = df_long$test,blocks = df_long$id) #ninguna sale significativa
# q cojones, y esta dferencia??
#resultados
ggwithinstats(x=test,y=score,data=df_long,p.adjust.method = "none", bf.message = FALSE, type="r", tr=0.2)

######### ejercicios
library(faraway)
data(coagulation)
head(coagulation)
summary(coagulation)

c_df <- coagulation

c_df %>%  group_by(diet) %>% get_summary_stats(coag, type ="mean_sd")
ggboxplot(c_df, x="diet", y="coag")
#optaremos por uan anove ade 1 via. Veamos los supeustos
#outliers
c_df %>%  group_by(diet) %>% identify_outliers(coag) #encuentra 3, optaremos por pruebas robustas
c_df %>%  filter(between(coag,
                       quantile(coag,0.1),
                       quantile(coag,0.9))) %>% ggboxplot(x="diet", y="coag")
#normalidad
fit <- lm(coag ~ diet, data=c_df)
ggqqplot(residuals(fit))
shapiro_test(residuals(fit)) #normalidad ok
#homogeneidad de varianza
plot(fit,1) #los residuos de cada grupo parece homogeneos
c_df %>% levene_test(coag ~ diet)#no es significativamente diferente por grupos
#anova test
#prueba
t1way(coag ~ diet, data=c_df )#
#para saber entre los grupos q hay diferencia
lincon(coag ~ diet, data=c_df) #encuentra super diferencias entre todos
#resultado
ggbetweenstats(x=diet,y=coag,data=c_df, p.adjust.method = "none", bf.message = FALSE, type="r", tr=0.2)

#ejercicio 2
library(datarium)
data(selfesteem)
head(selfesteem)
summary(selfesteem)

s_df <- selfesteem
#primero tenemos que pasarlo a estructura tidy
s_long <- s_df %>% pivot_longer(c(t1,t2,t3), names_to = "time", values_to= "selfsteem")
#explore
s_df %>% dplyr::select(t1,t2,t3) %>% get_summary_stats(type="mean_sd")  
ggboxplot(x="time", y="selfsteem", data=s_long, add=c("mean"), add.params = list(color="red"))
# supuestos de pruebas parametricas
#outliers
s_long %>%  group_by(time) %>%  identify_outliers(selfsteem) #hay 2 pero no son extremos
#normalidad
s_long %>%  group_by(time) %>%  shapiro_test(selfsteem)# si lo pasa
ggqqplot(s_long, "selfsteem", facet.by = "time")
#normalidad de datos recortados
# supuesto de normalidad
s_long %>% group_by(time) %>% filter(between(selfsteem,quantile(selfsteem,.1),quantile(selfsteem,.9))) %>%  shapiro_test(selfsteem) # no significativo
s_long %>% group_by(time) %>% filter(between(selfsteem,quantile(selfsteem,.1),quantile(selfsteem,.9))) %>% ggqqplot("selfsteem", facet.by = "time")
#el test rmanova del WRS2
rmanova(y=s_long$selfsteem, groups = s_long$time,blocks = s_long$id) #sale  significativo
#prubas post hoc
rmmcp(y=s_long$selfsteem, groups = s_long$time ,blocks = s_long$id) #todas significativa
ggwithinstats(x=time,y=selfsteem ,data=s_long,p.adjust.method = "none", bf.message = FALSE, type="r", tr=0.2)
# q cojones, y esta dferencia??