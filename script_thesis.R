library(tidyverse)
library(foreign)
library(readr)  # for read_csv
library(knitr)
library(rvest)
library(xml2)
library(stargazer)
library(tikzDevice)
library(xtable)

R.version
rm(list = ls())
covidurl<-("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/09-23-2020.csv")
cases<-read_csv(covidurl)
cases<-cases %>% filter(Country_Region=="US")

sum(cases$Deaths)
co_est2019_alldata_1_ <- read_csv("Downloads/co-est2019-alldata (1).csv")
county_pop<-co_est2019_alldata_1_ %>% select(county=CTYNAME,state=STNAME,
                                             pop=POPESTIMATE2019)
county_pop$county<-gsub(" County", "", county_pop$county)
county_pop<-county_pop %>% filter(pop!=19453561)

rm(co_est2019_alldata_1_)
load("/home/marges/Downloads/countypres_2000-2016.RData")
pres_16<-x %>% filter(year=="2016")
pres_16<-pres_16 %>% filter(party=="republican"|party=="democrat")
rm(x)
difference<-numeric(6316)
difference<-diff(pres_16$candidatevotes)*-1

for (i in 1:6316) {
  if(i%%2==0) {
    difference[i]<- 0-difference[i-1]
  }
}
pres_16$difference<-difference
press_16winner<-pres_16 %>% filter(party=="republican") %>% select(state,county,difference,party)
press_16winner<-press_16winner %>% mutate(repwin=numeric(3158))
Repwin<-press_16winner %>% filter(difference>0)
Replose<-press_16winner %>% filter(difference<1)
Repwin$repwin<-1
press_16winner<-rbind(Repwin,Replose)
press_16winner$party<-NULL
press_16winner$difference<-NULL
cases1<-cases %>% select(county=Admin2,state=Province_State,Confirmed,Deaths)

cases_parties<-merge(cases1,press_16winner,by.x=c("county","state"),by.y=c("county","state"))
final_db1<-merge(cases_parties,county_pop,by.x=c("county","state"),by.y=c("county","state"))
url_timeseries<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
cases_ts<-read_csv(url_timeseries)
cases_ts$daysfrom100<-numeric(nrow(cases_ts))
for (u in 6:(nrow(cases_ts))) {
  
  for (i in 12:(ncol(cases_ts)-1)) {
    
    if(cases_ts[u,i]>=100) {
      cases_ts$daysfrom100[u]<-(ncol(cases_ts)-1)-i
      break
    }
    
  }
}
ds100<-cases_ts %>% select(county=Admin2,state=Province_State,daysfrom100)


final_db2<-merge(final_db1,ds100,by.x=c("county","state"),by.y=c("county","state"))
Metrop <- read_delim("Documents/seminar_econ/data/Metrop.csv",";", escape_double = FALSE, trim_ws = TRUE)
Metrop$`County/County Equivalent`<- gsub(" County", "", Metrop$`County/County Equivalent`)

final_db3<-merge(final_db2,Metrop,by.x=c("county","state"),by.y=c("County/County Equivalent","State Name"))
sum(final_db3$Deaths)

colnames(Metrop)
final_db3[final_db3=="Micropolitan Statistical Area"]<-"0"
final_db3[final_db3=="Metropolitan Statistical Area"]<-"1"
final_db3$`Metropolitan/Micropolitan Statistical Area`<-as.numeric(final_db3$`Metropolitan/Micropolitan Statistical Area`)
colnames(final_db3)[8]<-"Metropol_area"

Income <- read_delim("Documents/seminar_econ/data/Income.csv", ";", escape_double = FALSE, trim_ws = TRUE)
Income<-Income %>% select(County,State,Income=`2018`)
final_db4<-merge(final_db3,Income,by.x=c("county","state"),by.y=c("County","State"))
final_db4_1<-merge(final_db2,Income,by.x=c("county","state"),by.y=c("County","State"))
sum(final_db4$Deaths)
sum(final_db4_1$Deaths)

#creating 2 new columns; deaths per 10 thousands residents and cases per 10 thousands residents( taking population into account)
final_db4<-final_db4 %>% mutate(cases_10ths=(Confirmed/pop)*10000,deaths_10ths=(Deaths/pop)*10000)
final_db4_1<-final_db4_1 %>% mutate(cases_10ths=(Confirmed/pop)*10000,deaths_10ths=(Deaths/pop)*10000)

final_db5<-final_db4 %>% filter(Confirmed>80)
sum(final_db5$Deaths)
final_db5_1<-final_db4_1 %>% filter(Confirmed>0&Deaths>0)
sum(final_db5_1$Deaths)
##adding the party of the state gouvernor for each county
gouvernors_url <- "https://en.wikipedia.org/wiki/List_of_United_States_governors#State_governors"
html <- read_html(gouvernors_url)
states_nodes <- html_nodes(html, ".center a")
states<-html_text(states_nodes)
states<-states[seq(2,100,2)]
party_nodes<-html_nodes(html,"td:nth-child(5)")
parties<-html_text(party_nodes)
parties<-parties[1:50]
gouvernors_db<-data.frame(states,parties,stringsAsFactors = FALSE)
gouvernors_db[]
parties
names(gouvernors_db)<-c("States","gouvern_party")
gouvernors_db$gouvern_party[48]<-"Republican\n"
gouvernors_db
Rep_states<-gouvernors_db$States[gouvernors_db$gouvern_party=="Republican\n"]
sort(Rep_states)
as.character(gouvernors_db$gouvern_party)
final_db5$rep_gouv<-ifelse(final_db5$state%in%Rep_states,1,0)
final_db5_1$rep_gouv<-ifelse(final_db5_1$state%in%Rep_states,1,0)

final_db6<-final_db5 %>% mutate(death_rate=Deaths/Confirmed)
final_db7<- final_db6 %>% filter(Deaths>0)

fn_db6_1<-final_db6 %>% filter(daysfrom100<190)
fn_db6_2<-final_db6 %>% filter(daysfrom100<80)

metrop_db<-final_db7 %>% filter(Metropol_area==1)
microp_db<-final_db7 %>% filter(Metropol_area==0)



## paper reference , estimation strategy
sum(final_db2$pop[final_db2$Confirmed<80])/sum(final_db2$pop)  #proportion of people living in counties with less than 80 cases, 1.1% of USA population lives in these counties
length(final_db2$pop[final_db2$Confirmed<80])
length(final_db2$pop)  #proportion of counties having less than 80 cases, nearly 18% of the counties 




#building models
first_model<-lm(log(Confirmed)~repwin+daysfrom100+Metropol_area+log(pop), data = final_db6)
second_model<-lm(log(Deaths)~repwin+daysfrom100+Metropol_area+log(pop), data = final_db6)

third_model<-lm(log(cases_10ths)~repwin+daysfrom100+Metropol_area+log(Income), data = final_db6)
third_model_nny<-lm(log(cases_10ths)~repwin+daysfrom100+Metropol_area+log(Income), data = fn_db6_1)
third_model_2w<-lm(log(cases_10ths)~repwin+daysfrom100+Metropol_area+log(Income), data = fn_db6_2)

fourth_model<-lm((deaths_10ths)~repwin+daysfrom100+Metropol_area+log(Income), data = final_db6)
fourth_model_nny<-lm(deaths_10ths~repwin+daysfrom100+Metropol_area+log(Income), data = fn_db6_1)
fourth_model_2w<-lm(deaths_10ths~repwin+daysfrom100+Metropol_area+log(Income), data = fn_db6_2)

fifth_model<-lm((death_rate)*100~repwin+daysfrom100+Metropol_area+log(Income),data = final_db6)
fifth_model_nny<-lm((death_rate)*100~repwin+daysfrom100+Metropol_area+log(Income),data = fn_db6_1)
fifth_model_2w<-lm(death_rate~repwin+daysfrom100+Metropol_area+log(Income),data = fn_db6_2)
sixth_model<-lm(log(cases_10ths)~rep_gouv+daysfrom100+Metropol_area+log(Income),data = final_db6)
seventh_model<-lm(deaths_10ths~rep_gouv+daysfrom100+Metropol_area+log(Income),data = final_db6)
sixth_model_nny<-lm(cases_10ths~rep_gouv+daysfrom100+Metropol_area+log(Income),data = fn_db6_1)
seventh_model_nny<-lm(deaths_10ths~rep_gouv+daysfrom100+Metropol_area+log(Income),data = fn_db6_1)
eighth_model<-lm(((death_rate)*100)~rep_gouv+daysfrom100+Metropol_area+log(Income),data = final_db6)

summary(first_model)
summary(second_model)
summary(third_model)
summary(fourth_model)
summary(third_model_nny)
summary(fourth_model_nny)
summary(third_model_2w)
summary(fourth_model_2w)

summary(fifth_model)
summary(fifth_model_nny)
summary(fifth_model_2w)

summary(sixth_model)
summary(sixth_model_nny)

summary(seventh_model)
summary(seventh_model_nny)

summary(eighth_model)



### Tables in the thesis
stargazer(final_db6)

ds<-final_db6 %>% group_by(repwin) %>% select(cases_10ths,deaths_10ths,death_rate) %>% summarise(across(cases_10ths:deaths_10ths:death_rate,mean,na.rm=TRUE))
print(xtable(ds)) #table 2

stargazer(first_model,second_model)
stargazer(third_model,third_model_nny,third_model_2w)
stargazer(fourth_model,fourth_model_nny,fourth_model_2w)
stargazer(fifth_model,fifth_model_nny,fifth_model_2w)
stargazer(sixth_model,seventh_model)
stargazer(sixth_model_nny,seventh_model_nny)
stargazer(eighth_model)

#figure 1 Thesis  
sums<-(colSums(cases_ts[seq(60,205,20)]))/1000
figure_1<-plot(seq(10,165,20),sums,main = "Growth in Coronavirus cases in USA ", sub = "Time interval starting from February (Data from JHU)",ylab = "Confirmed Cases/1000",xlab = "Days")+
  lines(seq(10,165,20),sums)

#figure 2 Thesis
(figure_2<-cases %>%ggplot(aes(x=Province_State,y=Confirmed/1000),color="black")+geom_bar(stat = "identity")+ 
    theme(axis.text.x = element_text(angle = 45))+labs(title = "Covid-19 cases by US State",
                                                       caption = "Data source: JHU, own visualization"))
(figure_3<-cases %>%ggplot(aes(x=Province_State,y=Deaths),color="black")+geom_bar(stat = "identity")+ 
    theme(axis.text.x = element_text(angle = 45))+labs(title = "Covid-19 Deaths by US State",
                                                       caption = "Data source: JHU, own visualization"))                                                  

#figure 4 day average
cases_ts$increase<-cases_ts[ncol(cases_ts)-2]-cases_ts[ncol(cases_ts)-10]
increase<-cases_ts %>% select(Admin2,Province_State,increase)
increase<-na.omit(increase)
figure_4<-increase %>% group_by(Province_State) %>% summarise(sum(increase)) %>% ggplot(aes(x=Province_State,(y=`sum(increase)`)/8))+geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 45))+labs(title = "daily cases averaged for the last 8 days for each state", caption = "Data source: JHU, own visualization")+ylab("cases")


sum(final_db4$Deaths)
final_db4 %>% ggplot(aes(x=daysfrom200,y=mean(Deaths)))+geom_line()
mean(final_db4$cases_10ths[final_db4$repwin==1])
mean(final_db4$cases_10ths[final_db4$repwin==0])
mean(final_db4$deaths_10ths[final_db4$repwin==1])
mean(final_db4$deaths_10ths[final_db4$repwin==0])
mean(final_db4$cases_10ths[final_db4$Metropol_area==1])

mean(final_db4$cases_10ths[final_db4$Metropol_area==0])
mean(final_db4$deaths_10ths[final_db4$Metropol_area==1])
mean(final_db4$deaths_10ths[final_db4$Metropol_area==0])
cases_ts %>% ggplot(aes(x=seq(10,110,20),y=sum(cases_ts[seq(65,155,20)])))+geom_line(stat = "identity")
length(final_db4$cases_10ths[final_db4$Metropol_area==1])
length(final_db4$cases_10ths[final_db4$Metropol_area==0])
length(final_db4$cases_10ths[final_db4$repwin==1])
length(final_db4$cases_10ths[final_db4$repwin==0])
sum(cases_ts[155])
sums<-colSums(cases_ts[seq(55,155,20)])

stargazer





mean(final_db6$death_rate[final_db6$repwin==1])
mean(final_db6$death_rate[final_db6$repwin==0])

##Calculating the average death rates for all counties
sum(final_db5$Deaths[final_db5$repwin==1])/sum(final_db5$Confirmed[final_db5$repwin==1]) 
sum(final_db5$Deaths[final_db5$repwin==0])/sum(final_db5$Confirmed[final_db5$repwin==0])
sum(final_db5$Deaths)/sum(final_db5$Confirmed)

final_db6$county[which.max(final_db6$death_rate)]
final_db6$death_rate[which.max(final_db6$death_rate)]
final_db6$Confirmed[which.max(final_db6$death_rate)]

head(final_db6[order(-final_db6$Deaths),])
sum(final_db6$repwin==1)/length(final_db6$repwin)
sum(cases_parties$repwin==1)/length(cases_parties$repwin)

## reached the 100th case after March 17th
final_db2 %>% ggplot(aes(repwin,Confirmed/1000,fill=(daysfrom100<190)))+geom_bar(stat = "identity") + scale_fill_grey() +
  labs(title = "Counties which reached their 100th case after 1st of April", caption = "Data source: JHU, own visualization") +
  xlab("Republican majority=1, Democrat majority =0")+ylab("Confirmed cases in thousands people") + theme_bw()


sum(final_db2$Deaths)
sum(final_db2$Confirmed)
sum(final_db2$Deaths[final_db2$repwin==0])
sum(final_db2$Confirmed[final_db2$repwin==0])