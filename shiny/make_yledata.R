# Load some R libraries
# You may have to install some of them first: install.packages("glmnet"), install.packages("plyr"), ...

library(Rmisc)
library(plyr)
library(dplyr)
library(glmnet)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(corrplot)
library(tidyr)
library(pxweb)
library(pxR)
library(RCurl)
library(curl)
library(caret)
library(here)

### some mapping functions
f2num<-function(x){as.numeric(levels(x)[x])}

# Function for plottin average with CI
ci.plot <-function(data, attr, group="puolue.lyh", ci=.95, y.lim=c(-1,1)) {
  d<-select_(data,y=attr,x=group)  
  group.CI(y ~ x, d, ci=ci) %>% 
    ggplot(data=.,aes(x=x, y=y.mean, ymin=y.lower, ymax=y.upper))+
    geom_linerange()+geom_point(size=2)+
    coord_cartesian(ylim=y.lim)+
    ggtitle(attr)+
    ylab(paste("Average with confindence interval=",ci))+
    xlab(group)
}

# Functions that maps party names to convenient acronyms 
map.puolue<-function(x) revalue(x,c(
  "Perussuomalaiset"="PS", 
  "Vihreä liitto"="VIHR",
  "Vasemmistoliitto"="VAS", 
  "Suomen Kommunistinen Puolue"="SKP",
  "Suomen Kristillisdemokraatit (KD)"="KD",
  "Suomen ruotsalainen kansanpuolue"="RKP", 
  "Itsenäisyyspuolue"="IP",
  "Kansallinen Kokoomus"="KOK",
  "Liberaalipuolue - Vapaus valita"="LIB",
  "Feministinen puolue"="FP",
  "Piraattipuolue"="PIR",                    
  "Suomen Keskusta"="KESK",
  "Suomen Sosialidemokraattinen Puolue"="SDP"))

# Function for mapping YLE Likert-scale answers to a heuristic quantitative scale
# We'll this is wrong, of course...

yle.map<-function(x){
  as.numeric(mapvalues(as.character(x),
                       c("",
                         "jokseenkin eri mieltä",
                         "jokseenkin samaa mieltä",
                         "ohita kysymys",
                         "täysin eri mieltä",
                         "täysin samaa mieltä"),
                       c(NA,-0.5,0.5,0,-1,1)))
}

# Load population of cities on 2/2017
# make log-transform
#kunta.vakiluku<- read.px(textConnection(getURL("http://pxnet2.stat.fi/PXWeb/Resources/PX/Databases/StatFin/vrm/vamuu/001_vamuu_tau_107.px"))) %>% 
#  as.data.frame %>% 
#  filter(Sukupuoli=="Sukupuolet yhteensä" & Alue!="KOKO MAA" & Kuukausi=="Helmikuu") %>%
#  select(-Kuukausi, -Sukupuoli, kunta=Alue, vakiluku=value) %>% 
#  mutate(vakiluku.lg10=log10(vakiluku), kunta=as.character(kunta))
#
#Does not exist any more must load from vakiluku.rds


# Load YLE election candidate opinion poll data
# If the does not wok out, load the csv file, unzip it to your R working dir
# you'll locate that by 'getwd()' and load it from there:
# yle.orig<-read.csv("candidate_answer_data_kokomaa11042017.csv",sep=";",stringsAsFactors = FALSE)

# Get YLE data zip file if not in data

if (!file.exists(here::here("data","yle_orig.rds"))) { 
zip.file <- paste(tempdir(),"/","candidate_answer_data_kokomaa11042017.zip",sep="")
curl_download("https://vaalit.beta.yle.fi/avoindata/candidate_answer_data_kokomaa11042017.zip", zip.file)
unzip(zip.file, exdir=tempdir())  

# load from tempdir
yle.orig<-read.csv(paste(tempdir(),"/","candidate_answer_data_kokomaa11042017.csv",sep=""),sep=";",stringsAsFactors = FALSE)

saveRDS(file=here::here("data","yle_orig.rds"), yle.orig)
} else 
  yle.orig <- readRDS(file=here::here("data","yle_orig.rds"))

# Most of the variables are just for later use, if you wish to do some data mining...
# The questions are marked X... and do not have "kommentti at the end"

yle <- select(yle.orig, 
             kunta, id, sukunimi, etunimi, sukupuoli, puolue, valittu, ikä, sitoutumaton, Äidinkieli, Työnantaja, 
             Ammattiasema, Käytän.vaaleihin.rahaa, Ulkopuolisen.rahoituksen.osuus, Vuositulot, Lapsia,
             Uskonnollinen.yhteisö, 
             starts_with("X"), -ends_with("kommentti"))


####
# Recode Likert scale answers to numbers on a scale ... which is a bit wrong but... never mind
# the "lautakunta" question 

yle <- select(yle.orig, 
             kunta, 
             id, 
             sukunimi, 
             etunimi, 
             puolue, 
             valittu, 
             ikä, 
             Äidinkieli, 
             Työnantaja, 
             Ammattiasema, 
             Käytän.vaaleihin.rahaa, 
             Ulkopuolisen.rahoituksen.osuus, 
             Vuositulot, 
             Uskonnollinen.yhteisö, 
             starts_with("X"), -ends_with("kommentti")) %>% 
  rename_(lautakunta = "X1810.Minkä.lautakunnan.työ.on.mielestäsi.tärkeintä.omassa.kunnassasi.tulevalla.vaalikaudella..Valitse.yksi.") %>% 
  mutate_at(vars(starts_with("X")), funs(yle.map)) %>%
  rename(X.Lautakunta = lautakunta,
         Z.Äidinkieli = Äidinkieli,
         Z.Usk.yhteisö = Uskonnollinen.yhteisö,
         Z.Työnantaja = Työnantaja,
         Z.Vuositulot = Vuositulot) %>%
  mutate(one=1) %>% spread(X.Lautakunta, one, fill=0, sep=".") %>%   
  mutate(one=1) %>% spread(Z.Äidinkieli, one, fill=0, sep=".") %>%
  mutate(one=1) %>% spread(Z.Usk.yhteisö, one, fill=0, sep=".") %>%
  mutate(one=1) %>% spread(Z.Työnantaja, one, fill=0, sep=".") %>% 
  mutate(Z.Vuosiulot.puuttuu=ifelse(Z.Vuositulot=="",1,0),
         Z.Vuositulot=as.numeric(mapvalues(Z.Vuositulot,
                                           c("", "alle 20 000 euroa", "20 000-30 000 euroa",
                                             "30 000-50 000 euroa", "50 000-70 000 euroa",
                                             "70 000-100 000 euroa", "yli 100 000 euroa"),
                                           c(0,0,20,30,50,70,100)/100)))

yle<-mutate(yle, kunta=iconv(kunta,to="UTF-8"))

# `Tarkein.lautakunta` (what is the most important board for you (social, construction, ...) in the city)
#  must be one-hot-coded for this model

# Count the missing data for the actual questions

yle$missing<-select(yle,starts_with("X")) %>% 
  mutate_all(function(x) is.na(x)+0) %>% 
  rowSums

# Combine rare parties and local coalitions with few candidates
# discard people with too few answers 

yle<-group_by(yle, puolue) %>%  
  mutate(N=n()) %>% ungroup %>%
  mutate(puolue.lyh=map.puolue(puolue) %>% as.character,
         puolue.lyh=ifelse(N < 40, "MUU", puolue.lyh),
         ok = missing <=15) %>% 
  select(-N) 

# Remove candidates with too many missing values

yle <- filter(yle, ok) %>% 
  mutate_at(vars(starts_with("X")), funs(ifelse(is.na(.),0,.))) %>% 
  select(-ok, -missing, -puolue) %>%
  mutate_all(funs(ifelse(.=="","puuttuu",.)))

# ratio 
#ggplot(count(yle,puolue.lyh) %>% mutate(N=sum(n)), aes(x=puolue.lyh,y=n/N))+geom_bar(stat="identity")

## A diversion: Just for curiosity (we are not going to use these in the classification, we could of course)
## One can plot the data for example with ggplot in various ways
## position "identity" sets relative absolute scale (but )
## position "fill" sets relative scale between 0 to 1 

#ggplot(yle,aes(x=puolue.lyh,fill=Äidinkieli))+geom_bar(position="fill")

# Another example of vis
#ggplot(yle,aes(x=ikä))+geom_density()
# Oh, sombody claims to be 400 years old? <18 can't be canfidaets...
yle <- mutate(yle,ikä.korjattu=ifelse(ikä>90|ikä<18,NA,ikä))

# Let's demostrate "boxplot" medians & 25/75 quartiles + outliers
#ggplot(yle,aes(y=ikä.korjattu,x=puolue.lyh))+geom_boxplot(notch=TRUE)

##########

# Let's make an "engineered feature": Extremism

yle$xtrem<-select(yle,starts_with("X"), -starts_with("X.lautakunta")) %>% 
  mutate_each(funs(abs(.)==1+0)) %>% 
  rowSums(.,na.rm=TRUE) 
yle <- mutate(yle,xtrem=xtrem/33)

# inhabitants in commune
yle <-left_join(yle, readRDS(file=here::here("data","vakiluku.rds")), by="kunta")
  

saveRDS(file=here::here("data","yle.rds"), yle)

# Statplot: means with 95% conf interval
# the function is based on ggplot... 
#ci.plot(yle,"xtrem","puolue.lyh",y.lim=c(0,1), ci=.95)

# Plot. NB: THIS is a bit WRONG since Likert scale is not continuous!
# change the attr to see other attributes

# Statplot: means with 95% conf interval... for one variable
#ci.plot(yle,"X128.Kuntien.tulee.tarjota.lasten.päivähoidon.varhaiskasvatus.ilmaiseksi.kaikille.lapsille.","puolue.lyh", ci=.95)

