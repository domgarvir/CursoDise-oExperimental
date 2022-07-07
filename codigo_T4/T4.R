library(openintro)
#ANOVA de 1 via

#La preguta que nos interesa es comparar la calificaion media en escritura de los grupos segun el tipo de programa del isntituto
#Hipotesis nula, todas las medias ighuales
#Hipotesis alterativa al menos una es diferente
library()
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
#hay q usar la de welch
hsb2 %>%  welch_anova_test(write ~ prog)#el p valor tb es significativo. No = media
#para el post-hoc el cambio lo ahcemos en el pool.sd
hsb2 %>% pairwise_t_test(write ~ prog, p.adjust.method = "bonferroni", pool.sd = FALSE)#hemos peusto varianzas iguales=TRUE
#ahora ahy una que no da significativo!!
#represetar de nuevo con var.equal=FALSE
ggbetweenstats(x=prog,y=write, data=hsb2, p.adjust.method="none",bf.message = FALSE,var.equal = FALSE)
