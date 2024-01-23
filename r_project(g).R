library(ggplot2)
library(dplyr)
library(gridExtra)
library(GGally)
library(memisc)
library(pander)
library(corrplot)
library(qicharts)
library(qcc)

#Correlation table
#Putting a Cor test together
simple_cor_test <- function(x, y) {
  return(cor.test(x, as.numeric(y))$estimate)
}

correlations <- c(
  simple_cor_test(wine$fixed.acidity, wine$quality),
  simple_cor_test(wine$volatile.acidity, wine$quality),
  simple_cor_test(wine$citric.acid, wine$quality),
  simple_cor_test(log10(wine$residual.sugar), wine$quality),
  simple_cor_test(log10(wine$chlorides), wine$quality),
  simple_cor_test(wine$free.sulfur.dioxide, wine$quality),
  simple_cor_test(wine$total.sulfur.dioxide, wine$quality),
  simple_cor_test(wine$density, wine$quality),
  simple_cor_test(wine$pH, wine$quality),
  simple_cor_test(log10(wine$sulphates), wine$quality),
  simple_cor_test(wine$alcohol, wine$quality))

barplot(correlations , names.arg =c('fixed.acidity', 'volatile.acidity', 'citric.acid',
                                    'log10.residual.sugar',
                                    'log10.chlordies', 'free.sulfur.dioxide',
                                    'total.sulfur.dioxide', 'density', 'pH',
                                    'log10.sulphates', 'alcohol') , main = "Correlation of Wine Quality")
names(correlations) <- c('fixed.acidity', 'volatile.acidity', 'citric.acid',
                         'log10.residual.sugar',
                         'log10.chlordies', 'free.sulfur.dioxide',
                         'total.sulfur.dioxide', 'density', 'pH',
                         'log10.sulphates', 'alcohol')
print("Correlation data : ")
print(correlations)

#Multivariate

#With constant alcohol density does not seem to have much effect, confirming our old suspicion
v1 <- ggplot(data = wine,
             aes(y = density, x = alcohol,
                 color = quality)) +
  geom_point() +
  scale_color_brewer()


# Seems like for wines with higher alcohol content, having higher sulphate makes better wines
v2 <- ggplot(data = wine,
             aes(y = sulphates, x = alcohol,
                 color = quality)) +
  geom_point() +
  scale_y_continuous(limits=c(0.3,1.5)) +
  facet_wrap(~rating) +
  scale_color_brewer()

#If volatile acidity os less, better wine
v3 <- ggplot(data = wine,
             aes(y = volatile.acidity, x = alcohol,
                 color = quality)) +
  geom_point() +
  facet_wrap(~rating) +
  scale_color_brewer()


#Low pH and higher alcohol percent produces better wines
v4 <- ggplot(data = wine,
             aes(y = pH, x = alcohol,
                 color = quality)) +
  geom_point() +
  facet_wrap(~rating) +
  scale_color_brewer()


#Lower residual sugar produces better wine
v5 <- ggplot(data = wine,
             aes(y = residual.sugar, x = alcohol,
                 color = quality)) +
  geom_point() +
  facet_wrap(~rating) +
  scale_color_brewer()


#In general lower SO2 produces better wine even though some high outliers for better wine with high SO2
v6 <- ggplot(data = wine,
             aes(y = total.sulfur.dioxide, x = alcohol,
                 color = quality)) +
  geom_point() +
  facet_wrap(~rating) +
  scale_color_brewer()

#Comparing the acids

#Higher citric acid and low volatile acidity produces better wines
v7 <- ggplot(data = wine,
             aes(y = citric.acid, x = volatile.acidity,
                 color = quality)) +
  geom_point() +
  facet_wrap(~rating) +
  scale_color_brewer()

#citric acid and fixed acidity may be correlated. But the quality does not seem to be dependent here
v8 <- ggplot(data = wine,
             aes(y = citric.acid, x = fixed.acidity,
                 color = quality)) +
  geom_point() +
  facet_wrap(~rating) +
  scale_color_brewer()

#Cannot really distinguish Average from Good wine based on these two factors
v9 <- ggplot(data = wine,
             aes(y = fixed.acidity, x = volatile.acidity,
                 color = quality)) +
  geom_point() +
  facet_wrap(~rating) +
  scale_color_brewer()
grid.arrange(v1,v2,v3,v4,v5 , v6 ,v7,v8,v9,ncol = 3 , top = "Multi Variate Analysis")


wine <- read.csv('Winedataset.csv')
#Converting Wine quality into a ordered factor
wine$quality <- factor(wine$quality, ordered = T)

#Creating a new 'rating' variable into the dataframe for different quality range
wine$rating <- ifelse(wine$quality < 5, 'bad', ifelse(
  wine$quality < 7, 'average', 'good'))

wine$rating <- ordered(wine$rating,
                       levels = c('bad', 'average', 'good'))

#Summary of the dataframe
print(summary(wine))

#Quality and rating
q1<-ggplot(data = wine, aes(x = quality)) +
  stat_count(width = 1, color = 'black',fill = I('orange')) +ggtitle("Quality")

q2<-ggplot(data = wine, aes(x = rating)) +
  stat_count(width = 1, color = 'black',fill = I('blue'))+ggtitle("Rating")


grid.arrange(q1,q2, ncol =2)
#Bivariate analysis
#Fixed acidity : Doesn't seem to have much effect
a1<-ggplot(data = wine, aes(x = quality, y = fixed.acidity)) +
  geom_boxplot() +ggtitle("Fixed Acidty")

#Volatile Acidity : Seems to have negative effect. With increase, quality seems to go down
a2<-ggplot(data=wine, aes(x = quality, y = volatile.acidity)) +
  geom_boxplot()

#Citric acid (Better wines tend to have higher citric acid)
a3<-ggplot(data=wine, aes(x=quality, y=citric.acid)) +
  geom_boxplot()

#Residual Sugar(Almost has no effect to quality. This is contrary to previous assumption)

a4<-ggplot(data=wine, aes(x=quality, y=residual.sugar)) +
  geom_boxplot()

#Chlorides

a5<-ggplot(data=wine, aes(x=quality, y=chlorides)) +
  geom_boxplot()

#Free SO2(We see too little and we get a poor wine and too much : we get an average wine)

a6<-ggplot(data=wine, aes(x=quality, y=free.sulfur.dioxide)) +
  geom_boxplot()

#Total SO2(Just like free SO2)

a7<-ggplot(data=wine, aes(x=quality, y=total.sulfur.dioxide)) +
  geom_boxplot()

#Density(Better wines tend to have lower densities but is it due to alcohol content?)

a8<-ggplot(data=wine, aes(x=quality, y=density)) +
  geom_boxplot()

#pH(Better wines seems to be more acidic. Now let's see contribution of each acid on pH)

a9<-ggplot(data=wine, aes(x=quality, y=pH)) +
  geom_boxplot()

grid.arrange(a1,a2,a3,a4,a5,a6,a7,a8,a9,ncol =3 , top = "BiVariate Analysis")


#Contribution of each acid to pH(We see all of them has negative correlation on pH except
#volatile acidity. But how's that possible! Is it possible that there is a Simson's effect?)


c_1<-ggplot(data = wine, aes(x = fixed.acidity, y = pH)) +
  geom_point() +
  scale_x_log10(breaks=seq(5,15,1)) +
  xlab("log10(fixed.acidity)") +
  geom_smooth(method="lm")

c_2<-ggplot(data = wine, aes(x = volatile.acidity, y = pH)) +
  geom_point() +
  scale_x_log10(breaks=seq(.1,1,.1)) +
  xlab("log10(volatile.acidity)") +
  geom_smooth(method="lm")

c_3<-ggplot(data = subset(wine, citric.acid > 0), aes(x = citric.acid, y = pH)) +
  geom_point() +
  scale_x_log10() +
  xlab("log10(citric.acid)") +
  geom_smooth(method="lm")

grid.arrange(c_1,c_2,c_3 , ncol = 3 , top = "Correlatio on Ph Effect")


#Sulphates(better wines seems to have higher sulphates. Although medium wines have many outliers)

print(ggplot(data=wine, aes(x=quality, y=sulphates)) +
        geom_boxplot()+ggtitle("Sulphates quality analysis"))

#Alcohol(Better wines have higher alcohol)
print(ggplot(data=wine, aes(x=quality, y=alcohol)) +
        geom_boxplot()+ggtitle("Alcohol quality analysis"))


p <- qcc(wine$free.sulfur.dioxide,wine$total.sulfur.dioxide,type = "p" ,data.name = "Sulfur Content")
np_chart <- qcc(wine$free.sulfur.dioxide,wine$total.sulfur.dioxide, type = "np", data.name = "Sulfur Content")

print("P CHART : ")
summary(p)
print("NP CHART : ")
summary(np_chart)
