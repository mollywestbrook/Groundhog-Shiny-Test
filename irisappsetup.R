
iris <- force(iris)
iris <- setDT(iris)

irislong <- melt(iris, 
                 id.vars=c("Species"), 
                 measure.vars=c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
                 variable.name=c("MeasureType"),
                 value.name="Value")

setosa <- irislong[Species == "setosa", ]

ggplot(setosa, aes(x=MeasureType, y=Value), group=MeasureType)+
  geom_boxplot()+
  theme_classic()+
  theme(text=element_text(size=20))