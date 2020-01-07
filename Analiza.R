remove(list = ls())
library(ggplot2)

install.packages('ggplot2movies')
library(ggplot2movies)


options(scipen = 9999)

?movies

data <- data(movies)

movies$mpaa[movies$mpaa == ''] <- NA

sum(is.na(movies$mpaa))

ggplot()+
  geom_bar(data = movies, mapping = aes(x = year), col = 'black', fill = 'blue')+
  ggtitle('Iloœæ filmów na przestrzeni lat')

ggplot()+
  geom_bar(data = movies, mapping = aes(x = length), col = 'darkgreen',
           fill = 'blue', binwidth = 5)+xlim(0,300)+
  ggtitle('Histogram d³ugoœci filmu')


ggplot()+
  geom_bar(data = movies, mapping = aes(x = rating),
           col = 'black', fill = 'darkblue', binwidth = 0.5)+
  ggtitle('Histogram g³osów')

ggplot()+
  geom_histogram(data = movies, mapping = aes(x = budget),
                 fill = 'red', col = 'black')+
  xlim(0,100000000)+ylim(0,850)+ggtitle('Histogram bud¿etu')



movies$rodzaj <- ifelse(movies$Action == 1, "Akcji",
                        ifelse(movies$Animation == 1, "Animowany",
                               ifelse(movies$Comedy == 1, 'Komedia',
                                      ifelse(movies$Drama == 1, 'Dramat', 
                                             ifelse(movies$Documentary == 1, "Dokumentalny",
                                                    ifelse(movies$Romance == 1, "Romantyczny",
                                                           'Krótki'))))))




movies$dlugosc <- ifelse(movies$length < 60, 'krótszy ni¿ 1h',
                         ifelse(movies$length < 120, 'pomiêdzy 1h a 2h',
                                'd³u¿szy ni¿ 2h'))

movies$dlugosc <- factor(movies$dlugosc, levels = c('krótszy ni¿ 1h',
                         'pomiêdzy 1h a 2h',
                         'd³u¿szy ni¿ 2h'))


ggplot(data = movies, mapping = aes(x = year, fill = rodzaj))+
  geom_bar()+
  facet_wrap(rodzaj~dlugosc, ncol = 3)




ggplot()+
  geom_bar(data = movies, mapping = aes(x = year, fill = dlugosc))+
  facet_wrap(~dlugosc)+theme(legend.position = 'none')+
  ggtitle('Iloœæ filmów na przestrzeni lat w podziale na d³ugoœæ filmu')


ggplot()+
  geom_density(data = movies, mapping = aes(x = rating, fill = dlugosc))+
  facet_wrap(~dlugosc)+theme(legend.position = 'none')+
  ggtitle('Density ocen w podziale na d³ugoœæ filmu')


