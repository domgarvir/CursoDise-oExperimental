library(car)
data("mtcars")
head(mtcars,2)
shapiro.test(mtcars$mpg)
hist(mtcars$mpg, freq = FALSE)
lines(density(mtcars$mpg))

qqPlot(mtcars$mpg,distribution="norm")


bartlett.test(mpg ~ factor(cyl), data=mtcars)
leveneTest(mpg ~ factor(cyl), data=mtcars) 
fligner.test(mpg ~ factor(cyl), data=mtcars)
boxplot(mpg ~ factor(cyl), data=mtcars)
