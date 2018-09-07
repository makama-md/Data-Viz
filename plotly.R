d =datasets::mtcars
library(plotly)
library(ggplot2)
plot_ly(d,x =  ~wt, y =  ~mpg, color =  ~disp, size =  ~disp)
plot_ly(iris,x =  ~Petal.Length, y =  ~Sepal.Width, 
        color = ~Species,size = ~ Sepal.Length,alpha= 1)

data("iris")
head(iris)
summary(iris)
(iris$Species)
plot(iris)
plot(iris$Petal.Length,iris$Sepal.Width)
ggplot(iris,aes(x = Petal.Length, y = Sepal.Width))
ggplot(iris,aes(x = Petal.Length, y = Sepal.Width,shape = Species,
                alpha=Sepal.Length,size = Petal.Width,col=Species)) +
  geom_point()
ggplot(iris,aes(Species)) +
  geom_bar()
gplot=ggplot(iris,aes(Species, Sepal.Length, fill = Species)) +
  geom_bar(stat = "summary", fun.y = "mean",
                 fill = "blue", col = "red")+ 
  geom_point(position = position_jitter(),size = 3, shape = 21)
gplot + theme(panel.grid = element_blank(), 
              panel.background = element_rect(fill="white"),
              axis.line.y = element_line(color = "black", size = 0.2))
ggplot(iris,aes(Petal.Length))+ 
  geom_histogram(binwidth = 0.1)
ggplot(iris, aes(Species, Petal.Length))+ 
  geom_boxplot(fill = "orange", notch = TRUE) +
  geom_point()+
  ggtitle("Iris dataset Analysis")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold",colour = "cadetblue"))
ggsave("graphlast.pdf", width = 10, height = 10)
data("ToothGrowth")
head(ToothGrowth)
summary(ToothGrowth)
ggplot(ToothGrowth,aes(supp, len, fill = as.factor(dose))) +
  geom_bar(stat = "summary", fun.y ="mean", color = "black",position = "dodge")+
  geom_point(position = position_dodge(0.9))
ggplot(ToothGrowth, aes(as.factor(dose),len, col= supp, group=supp))+
geom_line(stat = "summary",fun.y = "mean")+
geom_smooth(method = lm)  

#ggplot and dplyr
df = data.frame(x = rnorm(1000),
                y = rnorm(1000),
                group=sample(letters[1:5],size = 1000, replace=TRUE),
                stringAsFactor= FALSE)
head(df)
graph = ggplot(df,aes(x=x, y=y, col=group))+
  geom_point()+ geom_smooth(se = FALSE) + 
  facet_grid(~group, scales = "free") + 
  theme(panel.background = element_rect(fill = "lemonchiffon1")) +
  theme(legend.background = element_rect("lightcyan4")) +
  theme(plot.background = element_rect(fill = "peachpuff"))

#ggplotly applied on Titanic dataset
ggplotly(graph)


#ggplot on Titanic dataset
t = read.csv("titanic.csv", stringsAsFactors = FALSE)
str(t)
t$pclass = as.factor(t$pclass)
t$survived = as.factor(t$survived)
t$embarked = as.factor(t$embarked)
t$sex = as.factor(t$sex)
str(t)
ggplot(t, aes(x =survived, fill = "red")) +
  geom_bar()
prop.table(table(t$survived))
ggplot(t, aes(x =survived, fill = "red")) +
  theme(panel.background = element_rect(fill = "palegreen")) +
  geom_bar(col ="black") +
  labs(y = "passenger count", 
       title= "Titanic Survival Rate") 
ggplot(t, aes(x =sex, fill = survived)) +
  theme_bw() +
  geom_bar(col ="black") +
  labs(y = "passenger count", 
       title= "Titanic Survival Rates by Sex")
ggplot(t, aes(x = sex, fill = survived)) +
  theme_bw() +
  facet_wrap(~ pclass) +
  geom_bar(col ="black") +
  labs(y = "passenger count", 
       title= "Titanic Survival Rates by Class")
ggplot(t, aes(x =age, fill = survived)) +
  theme_bw() +
  geom_histogram(binwidth = 5,col ="black", position = "fill") +
  labs(y = "passenger count", x = "age(binwidth=5)" ,
       title= "Titanic Survival Rates by Sex")
ggplot(t, aes(x = survived, y = age)) +
  theme_bw() +
  geom_boxplot() +
  labs(title= "Titanic Survival Rates by Sex")
ggplot(t, aes(x = age, fill = survived)) +
  theme_bw() +
  facet_wrap(sex ~ pclass) +
  geom_density(alpha = 0.5) +
  labs(y = "passenger count", 
       title= "Titanic Survival Rates by  Sex and Class")
q=ggplot(t, aes(x = age, fill = survived)) +
  theme_bw() +
  facet_wrap(sex ~ pclass) +
  geom_histogram(binwidth = 5)
  labs(y = "passenger count", 
       title= "Titanic Survival Rates by  Sex and Class")
ggplotly(q)

devtools::install_github('hadley/ggplot2')

#ggplotly applied on map
library(maps)
data(canada.cities, package = "maps")
viz <- ggplot(canada.cities, aes(long, lat)) +
  borders(regions = "canada") +
  coord_equal() +
  geom_point(aes(text = name, size = pop), colour = "red", alpha = 1/2)
ggplotly(viz, tooltip = c("text", "size"))


