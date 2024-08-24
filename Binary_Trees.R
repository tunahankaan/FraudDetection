install.packages("mlbench")
install.packages("xgboost")
install.packages("pROC")
install.packages("DiagrammeR")
library(xgboost)
library(mlbench)
library(pROC)
library(DiagrammeR)
# CreditCard verimizde toplam 31 değişken var bunlardan class değişenini tahmin etmeye çalışıyoruz

set.seed(123) # eğitim ve test kümelerini rastgele örneklem seçimiyle oluşturuyoruz.
data.matrix()
orneklem <- sample.int(n = nrow(CreditCard), size = floor(.8*nrow(CreditCard)), replace = F)

egitim <- data.matrix(CreditCard[orneklem, ])
test <- data.matrix(CreditCard[-orneklem, ]) #Eğitim ve test kümeleri rastgele örneklem seçimiyle oluşturuldu

hedef <- egitim[,31] # Hedef Değişken
library(xgboost)

kararmodeli <- xgboost(data=egitim[,-31], objective="binary:logistic",
                       label=hedef,
                       nround=5)   #Karar Ağacı modeli

tahmin <- predict(kararmodeli, test[,-31])

# 0.5 esik degeri ile tahminlerin dogruluk matrisi olusturulur. Belli bir eşik değerin üstündekiler ussulsüzlük yapılma olasılığı fazla olanlar.
# Burada satirlar tahmin, sutunlar gercek degerler.
table(as.numeric(tahmin > 0.5), test[,31])

# Degisken etkileri matrisini olustur
degisken_etkileri <- xgb.importance(feature_names =
                                      colnames(egitim[,-31]), model = kararmodeli)

print(xgb.plot.importance(importance_matrix = degisken_etkileri))


