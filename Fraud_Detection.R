#CreditCard verilerinin [6,21] verisi için CreditCard2 oluşturlmustur.
#CreditCard2

CreditCard2$Class[[2]] <- 0
CreditCard2$Class

x1 <-  matrix(CreditCard2$Time)
x1
x2 <- list(CreditCard2$Class)
View(x2)

x2=as.matrix(x2)
plot(CreditCard2)
plot(x1)
plot(x2)

colSums(is.na(CreditCard2)) # Verilerde boş veri veya NA tarzı veri olup olmadığını belirlemek için.

install.packages(ggplot2)
library(ggplot2)

Time_Class1 <- CreditCard2[,c("Time","Class")]
plot(Time_Class1)
line = ggplot(Time_Class1,aes(x=time,y=class))
View(line)
line+geom_line()

summary(CreditCard2)

Class_Amount=cbind(CreditCard2$Class,
                  CreditCard2$Amount)
as.matrix(Class_Amount)
plot(Class_Amount)



Ortalama = mean(CreditCard2$Amount)
Ortalama
Medyan = median(CreditCard2$Amount)
Medyan
summary(CreditCard2)

p <- ggplot(CreditCard2, aes(x = Class, y = Amount)) + geom_boxplot() + ggtitle("Distribution of transaction amount by class")
print(p)

CreditCard2$Class <- as.numeric(CreditCard2$Class)
corr_plot <- corrplot(cor(CreditCard2[,c("Time")]), method = "circle", type = "upper")

#CreditCard
class <- as.matrix(CreditCard$Class)
plot(Class)

Time_Class <- CreditCard[,c("Time","Class")]  # Sadece Time ve Class alınarak karşılaştırma yapıldı 
plot(Time_Class)


fig(14, 8)
ggplot(CreditCard, aes(x = factor(Class), y = Amount)) + geom_boxplot() + 
  labs(x = 'Class', y = 'Amount') +
  ggtitle("Distribution of transaction amount by class") # ggplot gafiği ile yapılmaya çalışıldı


install.packages("corrplot")
CreditCard$Class <- as.numeric(CreditCard$Class)
corr_plot <- corrplot(cor(CreditCard[,c("Amount")]), method = "circle", type = "upper") # corrplot grafiği ile Amount ve Class nitelikleri bakıldı.

a1 <- summary(mean(CreditCard$Amount))
a1
a2 <- summary(mean(CreditCard$Class))
a2

table(CreditCard$Class)


prop.table(table(CreditCard$Class))


