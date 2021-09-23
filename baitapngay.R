install.packages("ISwR")
library(ISwR) 
igfdata <- juul$igf1
head(igfdata)

#Chuong 9: tr55-71: Phan tich thong ke mo ta
#gia tri trung binh cua age
mean(igfdata, na.rm = TRUE)
#phuong sai
var(igfdata,na.rm = TRUE)
#do lech chuan
sd(igfdata, na.rm = TRUE)
#Tat ca thong tin thong ke
summary(igfdata)
#Ham tinh sai so chuan
desc <- function(x)
{
  av <- mean(x, na.rm = TRUE)
  sd <- sd(x, na.rm = TRUE)
  se <- sd/sqrt(length(x))
  c(MEAN=av, SD=sd, SE=se)
}
desc(igfdata)
#tom luoc theo age
by(juul, juul$age, summary)
#Ve do thi phan phoi cho bien so igfdata
hist(igfdata)
length(juul)
#Thong ke mo ta theo tung nhom
#Tinh trung binh cho mot bien so cho moi nam va nu (theo 1 va 2)
tapply(igfdata, list(juul$sex), mean, na.rm = TRUE)

#Kiem dinh t 1 mau
#VD 10
#Tinh gia tri t cho 100 mau voi do tin cay la 95%
qt(0.95, 100)
#nhanh gon hon bang t.test (mu: gt gia thuyet, age: bien so can kiem dinh)
t.test(age, mu=30)

#Kiem dinh t hai mau
#VD 11
#xem xet muc do khac biet trung binh giua hai nhom va do lech chuan cua do
#khac biet.
t.test(igfdata~ juul$sex)
#co li do de cho rang hai nhom co cung phuong sai, chung ta chi thay doi mot
#thong so trong ham t voi var.equal=TRUE
t.test(igfdata~ juul$sex, var.equal=TRUE)

#Kiem dinh Wilcoxon cho hai mau (wilcox.test)
#Kiem din? phan phoi cho igfdata
shapiro.test(igfdata)
#kiem dinh Wilcoxon khong tuy thuoc vao gia dinh phan phoi chuan
wilcox.test(igfdata~ juul$sex)

#Kiem dinh t cho cac bien so theo cap (paired t-test, t.test)
#VD 12
# nhap du kien
before <- c(180, 140, 160, 160,220, 185, 145, 160, 160, 170)
after <- c(170, 145, 145, 125, 205, 185, 150, 150, 145, 155)
bp <- data.frame(before, after)
# kiem dinh t
t.test(before, after, paired=TRUE)
#Chu y neu chung ta phan tich sai bang kiem dinh thong ke cho hai nhom doc lap duoi day
#thi tri so p = 0.32 cho biet muc do giam ap suat khong co y nghia thong ke!
t.test(before, after)

#Kiem dinh Wilcoxon cho cac bien so theo cap (wilcox.test)
wilcox.test(before, after, paired=TRUE)

#Tan so (frequency)
#Ham table cho chung ta biet ve tan so cua mot bien so
#mang tinh phan loai nhu sex, tanner
table(juul$sex)
table(juul$tanner)
#bang thong ke 2 chieu
table(juul$sex, juul$tanner)
#tinh so phan tram, chung ta su dung ham prop.table
freq <- table(juul$sex, juul$tanner)
#xem ket qua 
margin.table(freq, 1)
margin.table(freq, 2)
#Tinh so phan tram
#Tinh tanner cho tung sex
prop.table(freq, 1)
#tinh sex cho tung tanner
prop.table(freq, 2)

#Kiem dinh ti le (proportion test, prop.test, binom.test)
#VD 13
#1: 621, 2: 713, vay ti le 1 la 0.4655 -> kiem dinh xem ti le nay co that su khac voi ti le 0.5 hay khong
#ta dung prop.test(x, n, ??)
prop.test(46.55, 100, 0.50)
#cach tinh chinh xac hon kiem dinh ti le la kiem dinh nhi phan bionom.test(x, n, ??)
binom.test(47, 100, 0.50)

#So sanh hai ti le (prop.test, binom.test)
#VD 14
#kiem dinh xem 2 ti le co that su khac nhau, chung ta co the su dung ham
#prop.test(x, n, ??)
fracture <- c(7, 20)
total <- c(100, 110)
prop.test(fracture, total)

#So sanh nhieu ti le (prop.test, chisq.test)
table(juul$sex, juul$tanner)
female <- c( 291, 55, 34, 41, 124)
total <- c(515, 103, 72, 81, 328)
prop.test(female, total)

#Kiem dinh chi binh phuong (Chi squared test, chisq.test)
chisq.test(juul$sex, juul$tanner)

#Kiem dinh Fisher ((Fisher’s exact test, fisher.test)
fisher.test(juul$sex, juul$tanner, simulate.p.value=TRUE)

#Phan tich hoi quy tuyen tinh
#VD 15
age <- c(46,20,52,30,57,25,28,36,22,43,57,33,22,63,40,48,28,49)
bmi <-c(25.4,20.6,26.2,22.6,25.4,23.1,22.7,24.9,19.8,25.3,23.2,
        21.8,20.9,26.7,26.4,21.2,21.2,22.8)
chol <- c(3.5,1.9,4.0,2.6,4.5,3.0,2.9,3.8,2.1,3.8,4.1,3.0,
          2.5,4.6,3.2, 4.2,2.3,4.0)
data <- data.frame(age, bmi, chol)
plot(chol ~ age, pch=16)