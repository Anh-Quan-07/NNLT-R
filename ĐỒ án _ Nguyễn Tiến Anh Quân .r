library(stats)
library(dplyr)
library(data.table)
library(ggplot2)
library(plotly)
library(ggcharts)


url <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/02-19-2021.csv'
covid <- read.csv(url, header = TRUE)
url2 = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/02-20-2021.csv'
data2 <- read.csv(url2, header = TRUE)
df <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/03-17-2021.csv')
setwd("C:\\Users\\DELL\\Desktop\\Baithi\\R\\US")
#Doc tat ca cac file vao mot dataframe
files <- list.files(pattern = ".csv")
temp <- lapply(files, fread, sep=",")
data <- rbindlist( temp, fill=TRUE)

TX <- data[Province_State=="Texas"]
ALK <- data[Province_State=="Alaska"]
Ohio <- data[Province_State=="Ohio"]

####################
#bd1 - BIEU DO NGANG - TOP 15 NUOC CO SO CA NHIEM LON NHAT 14/3/2021
a1 <- plot_ly(covid %>% top_n(15, Confirmed),
  type = 'bar',
  x = ~Confirmed,
  y = ~Country_Region,
  textposition =  'outside',
  marker = list(color = 'blue',                                                    
                  line = list(color = 'red)',
                              width = 2))) 

a1 <- a1  %>% layout(title = "TOP 15 NUOC CO SO CA NHIEM LON NHAT",uniformtext=list(minsize=8, mode='hide'))
a1

#bd2 - BIEU DO DOC - TOP 15 NUOC CO SO CA NHIEM LON NHAT 24/3/2021
a2 <- plot_ly(covid %>% top_n(15, Confirmed),
  type='bar',
  x = ~Country_Region,
  y = ~Confirmed,
  textposition = 'outside',
  marker = list(color = '#blue',
          line = list(color = 'red)',
                    width = 2)))  
a1 <- a2   %>% layout(title = "TOP 15 NUOC CO SO CA NHIEM LON NHAT",uniformtext=list(minsize=8, mode='hide'))
a2 



#Texas
#bd3  - BIEU DO DIEM - SO CA TU VONG TAI Texas
p = ggplot(data=TX, aes(x=Deaths, y=Last_Update))
p+
  geom_point(aes(colour = Deaths), colour = "red") +
  xlab("SO NGUOI TU VONG")+
  ylab("NGAY")+
  labs(title="DO THI SO CA TU VONG TAI Texas")
#bd4 - BIEU DO DUONG - SO CA HOI PHUC TAI Texas
p = ggplot(TX, aes(Last_Update, Recovered))
p+
  xlab("SO NGUOI HOI PHUC")+
  ylab("NGAY")+
  geom_line(aes(color = "Recovered",size = 0.5),colour="red")+
  labs(title="DO THI SO CA HOI PHUC TAI Texas")


#ALASKA
#bd5 - BIEU DO TRON - TY LE KIEM TRA ALASKA
p <- plot_ly(ALK,
          type='pie',
          labels = ~Last_Update,
          values = ~Testing_Rate,
          textposition = 'inside')

p <- p %>% layout(title = "TY LE KIEM TRA CUA ALASKA",uniformtext=list(minsize=2, mode='hide'))
p

#bd6  - BIEU DO DIEM - SO CA NHIEM BENH TAI ALASKA
p = ggplot(data=ALK, aes(x=Confirmed, y=Last_Update))
p+
  geom_point(aes(colour = Confirmed), colour = "blue") +
  xlab("SO NGUOI NHiEM")+
  ylab("NGAY")+
  labs(title="DO THI SO CA NHIEM TAI ALASKA")

#bd7 - BIEU DO COT - SO LAN XAY RA SU CO CAC KHU VUC O MY NGAY 2021-01-10
ggplot(df[1:10,],
       aes(x=Incident_Rate,
           y=Province_State))+
  geom_col(aes(x=Incident_Rate,
               y=Province_State,
               fill = Province_State))+ 
  labs(title="SO LAN XAY RA SU CO CAC KHU VUC O MY NGAY 2021-01-10 ",x = "Su Co", y="Khu Vuc")


#bd8  - BIEU DO TRON - SO CA NHIEM BENH CAC KHU VUC O MY
a3 <- plot_ly(data,
               type='pie',
               labels = ~Province_State,
               values = ~Confirmed,
               text = ~Country_Region,
               textposition = 'inside')

a3 <- a3 %>% layout(title = "SO CA NHIEM BENH GIUA CAC KHU VUC O MY ",uniformtext=list(minsize=8, mode='hide'))
a3

#bd9 - BIEU DO TRON - SO CA TU VONG CAC KHU VUC O MY 02-20-2021
a4 <- plot_ly(data2,
                type='pie',
                labels = ~Province_State,
                values = ~Deaths,
                text = ~Country_Region,
                textposition = 'inside')

a4 <- a4 %>% layout(title = "SO CA TU VONG CAC KHU VUC O MY 02-20-2021 ",uniformtext=list(minsize=8, mode='hide'))
a4


#bd10  - BIEU DO DIEM - 10 KHU VUC CO SO CA SONG SOT CAO NHAT
ggplot(data2 %>% top_n(10,Active),
       aes(x=Active,
           y=Province_State,
           fill= Active))+ 
  geom_point(aes(colour = Active))+ 
  labs(title="BIEU DO SO CA HOI PHUC CUA CAC KHU VUC O MY 02-20-2021",x = "SONG SOT", y="KHU VUC")

#bd11  - BIEU DO TRON - SO NGUOI SONG O CAC KHU VUC O MY 02-20-2021
a5 <- plot_ly(data2,
                type='pie',
                labels = ~Province_State,
                values = ~Active,
                text = ~Country_Region,
                textposition = 'inside')
a5 <- a5 %>% layout(title = "SO NGUOI SONG CAC KHU VUC O MY 02-20-2021 ",uniformtext=list(minsize=8, mode='hide'))
a5


#bd12  - BIEU DO DUONG - SO SANH TY LE TU VONG GIUA ALASKA VA Texas
tx_rec <- c(TX$Case_Fatality_Ratio)
alk_rec <- c(ALK$Case_Fatality_Ratio)

plot(tx_rec,type = "o",col = "red",
     xlab = "NGAY",main = "SO SANH TY LE TU VONG GIUA ALASKA VA Texas ")
par(new=TRUE)
plot(alk_rec,type = "o",col = "blue",xlab = "NGAY")
legend("topright", legend=c("Texas Case Fatality Ratio", "ALASKA Case Fatality Ratio"),
       col=c("red", "blue"), lty=1:2, cex=0.8)

#bd13  - BIEU DO NGANG - TOP CAC NUOC CO SO CA TU VONG CAO NHAT
a6 <- plot_ly(covid %>% top_n(20, Deaths),
                type = 'bar',
                x = ~Deaths,
                y = ~Country_Region,
                textposition =  'outside',
                marker = list(color = '#19D3F3',                                                    
                              line = list(color = '#D62728)',
                                          width = 2))) 

a6 <- a6  %>% layout(title = "TOP CAC NUOC CO SO CA TU VONG CAO NHAT",uniformtext=list(minsize=8, mode='hide'))
a6

#bd14  - BIEU DO COT - DO THI SO NGUOI TU VONG PHUC CAC KHU VUC O Ohio
ggplot(Ohio, aes(x=Last_Update,
                 color=Deaths)) +
  geom_col(aes(x=Last_Update,
               y=Deaths,
               fill =Deaths)) + 
  theme_grey() +
  labs(title="DO THI SO NGUOI TU VONG PHUC CAC KHU VUC O Ohio")





#bd15  - BIEU DO TRON - TY LE SU CO CAC NGAY O Ohio
p <- plot_ly(Ohio,
             type='pie',
             labels = ~Last_Update,
             values = ~Incident_Rate,
             textposition = 'inside')

p <- p %>% layout(title = "TY LE SU CO QUA CAC NGAY O Ohio",uniformtext=list(minsize=2, mode='hide'))
p

#bd16  - BIEU DO TRON - TY LE KIEM TRA  O CAC KHU VUC O MY 02-20-2021

a7 <- plot_ly(data2, type='pie', labels = ~Province_State, values = ~Testing_Rate, text = ~Country_Region, textposition = 'inside')
a7 <- fig8 %>% layout(title = "TY LE KIEM TRA  O CAC KHU VUC O MY 02-20-2021 ",uniformtext=list(minsize=8, mode='hide'))
a7 

#bd17  - BIEU DO DUONG DIEM - SO SANH SO CA NHIEM BENH GIUA Texas VA Ohio
TX_con <- c(TX$Confirmed)
Ohio_con <- c(Ohio$Confirmed)

plot(TX_con,type = "o",col = "red",xlab = "NGAY",main = "SO SANH TY LE NHIEM BENH GIUA Ohio VA Texas ")
par(new=TRUE)
plot(Ohio_con,type = "o",col = "blue",xlab = "NGAY")
legend("topright", legend=c("Texas Confirmed", "Ohio Confirmed"),
       col=c("red", "blue"), lty=1:2, cex=0.8)


