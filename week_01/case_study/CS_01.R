#load iris dataset
data(iris)
#calculate the mean value of one column
petal_length_mean <- mean(iris$Petal.Length)
#draw the histogram
#hist(x=iris$Petal.Length, col='red')

ggplot(iris,aes(x=Petal.Length))+
geom_histogram(color='black')
