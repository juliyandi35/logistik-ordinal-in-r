##=====================##
# INPUT DATA
##=====================##
library(readxl)
dataku<-read_excel("Ordinal Data.xlsx")
dim(dataku)
dataku <- data.frame(dataku)
head(dataku)

mental.degree<-factor(dataku$mental.degree)
ses<-factor(dataku$ses)
life<-dataku$life

##=====================## 
# FITTING MODEL 
##=====================##
library('foreign')
library('MASS')
library('nnet')
model<-polr(mental.degree~ses+life, method="logistic")
summary(model)

#A. Bandingkan hasilnya dengan output SAS pada buku Agresti tersebut serta berikan interpretasi pada tiap nilai dugaan parameter model.
##=====================## 
# FITTING MODEL 
##=====================##

model<-polr(mental.degree~ses+life, method="logistic")
summary(model)

##Interpretasi prameter#
coefmodel<-c(-model$coefficients,model$zeta)
data.frame(coefmodel,expcoef=exp(coefmodel)) 

# B. Berdasarkan hasil pada poin (a) diatas, tentukan nilai dugaan 𝑃(𝑌)=1,𝑃(𝑌)=3 , dan 𝑃(𝑌)>2

##=====================## 
# predict model
##=====================## 
new<-data.frame(ses=factor(c(1)),life=4) 
predik<-predict(model, newdata=new, type="probs") #peluang point
peluang<-data.frame(cumulatif=cumsum(predik),point=predik) 
peluang

#peluang p(>2)
1-peluang[1,1] 

# C. Tentukan model terbaik
# Menguji pengaruh interaksi life score dan ses terhadap gangguan mental
#model interaksi life*ses
model1<-polr(mental.degree~life*ses,method="logistic") 
summary(model1)

library(lmtest)
lrtest(model1,model)#cek interaksi

# Menguji pengaruh ses terhadap gangguan mental
#model hanya peubah life 
model2<-polr(mental.degree~life,method="logistic") 
summary(model2)

lrtest(model2,model)#cek ses

# Menguji pengaruh life score terhadap gangguan mental
#model hanya peubah ses
model3<-polr(mental.degree~ses,method="logistic") 
summary(model3)

lrtest(model3,model)#cek ses

# D. Misalkan seorang individu diketahui bahwa Life Events (x1=8) dan SES (x2=1), berdasarkan model pada poin (c) tentukan dugaan “Mental Impairment”
#prediksi data baru
new=data.frame(life=8,ses=as.factor("1"))
predict(model3,newdata=new,"probs")

#Berdasarkan hasil output R di atas maka dugaan mental impairment-nya adalah well karena memiliki peluang paling besar

