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

kunta.vakiluku<- read.px(textConnection(getURL("http://pxnet2.stat.fi/PXWeb/Resources/PX/Databases/StatFin/vrm/vamuu/001_vamuu_tau_107.px"))) %>% 
  as.data.frame %>% 
  filter(Sukupuoli=="Sukupuolet yhteensä" & Alue!="KOKO MAA" & Kuukausi=="Helmikuu") %>%
  select(-Kuukausi, -Sukupuoli, kunta=Alue, vakiluku=value) %>% 
  mutate(vakiluku.lg10=log10(vakiluku), kunta=as.character(kunta))

# Load YLE election candidate opinion poll data
# If the does not wok out, load the csv file, unzip it to your R working dir
# you'll locate that by 'getwd()' and load it from there:
# yle.orig<-read.csv("candidate_answer_data_kokomaa11042017.csv",sep=";",stringsAsFactors = FALSE)

# Get YLE data zip file
zip.file <- paste(tempdir(),"/","candidate_answer_data_kokomaa11042017.zip",sep="")
curl_download("https://vaalit.beta.yle.fi/avoindata/candidate_answer_data_kokomaa11042017.zip", zip.file)
unzip(zip.file, exdir=tempdir())  

# load from tempdir
yle.orig<-read.csv(paste(tempdir(),"/","candidate_answer_data_kokomaa11042017.csv",sep=""),sep=";",stringsAsFactors = FALSE)


# Most of the variables are just for later use, if you wish to do some data mining...
# The questions are marked X... and do not have "kommentti at the end"

yle<- select(yle.orig, 
             kunta, id, sukunimi, etunimi, sukupuoli, puolue, valittu, ikä, sitoutumaton, Äidinkieli, Työnantaja, 
             Ammattiasema, Käytän.vaaleihin.rahaa, Ulkopuolisen.rahoituksen.osuus, Vuositulot, Lapsia,
             Uskonnollinen.yhteisö, 
             starts_with("X"), -ends_with("kommentti"))


####
# Recode Likert scale answers to numbers on a scale ... which is a bit wrong but... never mind
# the "lautakunta" question 

yle<- select(yle.orig, 
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

yle<-mutate(yle,kunta=iconv(kunta,to="UTF-8"))

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
yle<-mutate(yle,ikä.korjattu=ifelse(ikä>90|ikä<18,NA,ikä))

# Let's demostrate "boxplot" medians & 25/75 quartiles + outliers
#ggplot(yle,aes(y=ikä.korjattu,x=puolue.lyh))+geom_boxplot(notch=TRUE)

##########

# Let's make an "engineered feature": Extremism

yle$xtrem<-select(yle,starts_with("X"), -starts_with("X.lautakunta")) %>% 
  mutate_each(funs(abs(.)==1+0)) %>% 
  rowSums(.,na.rm=TRUE) 
yle<-mutate(yle,xtrem=xtrem/33)

# inhabitants in commune
yle<-left_join(yle,kunta.vakiluku,by="kunta")

# Statplot: means with 95% conf interval
# the function is based on ggplot... 
#ci.plot(yle,"xtrem","puolue.lyh",y.lim=c(0,1), ci=.95)

# Plot. NB: THIS is a bit WRONG since Likert scale is not continuous!
# change the attr to see other attributes

# Statplot: means with 95% conf interval... for one variable
#ci.plot(yle,"X128.Kuntien.tulee.tarjota.lasten.päivähoidon.varhaiskasvatus.ilmaiseksi.kaikille.lapsille.","puolue.lyh", ci=.95)

# Ok, now the model 1:
# glmnet for multinomial requires data in matrix and target in factor


make.classifier.data <- function(classifier, yle, X) {
# Ugly staff for geting the cross validated results for best lambda

  C <-list(glmnet=classifier$glmnet.fit,
           lambda.min=classifier$lambda.min,
           lambda=classifier$lambda,
           classnames=classifier$glmnet.fit$classnames)  
  
  # cv.luokka
  ennuste<-tolower(C$classnames[apply(classifier$fit.preval[,,which(C$lambda.min==C$lambda)],1,which.max)])

response <- classifier$fit.preval[,,which(classifier$lambda.min==classifier$lambda)];

colnames(response) <- C$classnames

C$cv <- data.frame(id=yle$id, 
                        oikea=yle$puolue.lyh,
                        ennuste,
                        posterior=response)
                       
# Confusion matrix

C$confusion.matrix <- table(C$cv$ennuste,C$cv$oikea)

## Coefficients!

puolue<-colnames(C$confusion.matrix)

j<-data.frame(matrix(0,dim(X)[2]+1,length(puolue)))

#names(j)<-puolue
#for (i in puolue) j[i]<-coef(classifier)[[i]] %>% as.numeric
#j$question<-rownames(coef(classifier)$SDP)

names(j)<-puolue

for (i in puolue) j[i]<-predict(C$glmnet,s=C$lambda.min,type="coef")[[i]] %>% 
  unlist %>% as.numeric
j$question<-rownames(coef(classifier)$SDP)

C$kertoimet<-melt(j, id.vars="question") %>%
  filter(question != "intercept") %>% 
  rename(puolue=variable, kysymys=question) %>%
  mutate(txt=ifelse(value==0,"0",sprintf("%1.2f", value))) %>% 
  arrange(kysymys) 

## Store best classifier

# Classification for best classifier
posterior <- predict(C$glmnet, newx=X, type="response")[,,C$lambda.min==C$lambda] %>% 
  data.frame
posterior$id <- as.numeric(rownames(posterior))

results<-list(C=C, 
              posterior=posterior, 
              X=X)

return(results)
}

X<-select(yle, matches("^X|xtrem")) %>% select(-X.Lautakunta.) %>% as.matrix

row.names(X)<-yle$id
X[is.na(X)]<-0
y=factor(yle$puolue.lyh)

# cv.glmnet makes the modelling, finds penalization weight by cross-validation. We'll
# use the results from the optimal lambda, classification is taken from the cv (unseen folds)
# Let's try just LASSO (alpha=1) penalization - you can check for Ridge -penalization alpha=0 which gives a bit different results 
# or something in between like, like alpha=0.7

classifier<-cv.glmnet(X,y,family="multinomial", 
                      keep=TRUE, 
                      type.measure="class", 
                      standardize=TRUE, 
                      intercept=TRUE, 
                      alpha=0.5, 
                      nfold=5)

C1 <- make.classifier.data(classifier=classifier, X, yle=yle)

####

# Malli 2

X <- select(yle, matches("^X|xtrem|Z.Äid|Z.Usk|Z.Vuositulot|Z.Työ|vakiluku.lg10")) %>%
               select(-one_of(c("Z.Äidinkieli.suomi","Z.Usk.yhteisö.","Z.Työnantaja."))) %>% 
  as.matrix

row.names(X)<-yle$id
X[is.na(X)]<-0
y=factor(yle$puolue.lyh)

# cv.glmnet makes the modelling, finds penalization weight by cross-validation. We'll
# use the results from the optimal lambda, classification is taken from the cv (unseen folds)
# Let's try just LASSO (alpha=1) penalization - you can check for Ridge -penalization alpha=0 which gives a bit different results 
# or something in between like, like alpha=0.7

classifier<-cv.glmnet(X,y, family="multinomial", 
                      keep=TRUE, 
                      type.measure="class", 
                      standardize=TRUE, 
                      intercept=TRUE, 
                      alpha=0.5, 
                      nfold=5)

C2 <- make.classifier.data(classifier=classifier, X, yle=yle)

ehdokkaat <- select(yle, etunimi, sukunimi, id, kunta, oikea=puolue.lyh)

ehdokkaat$malli1 <- names(select(C1$posterior,-id))[apply(select(C1$posterior,-id),1,which.max)] %>% 
  tolower

ehdokkaat$malli2 <- names(select(C2$posterior,-id))[apply(select(C2$posterior,-id),1,which.max)] %>% 
  tolower

C1$C$var.imp<-varImp(C1$C$glmnet, lambda=C1$C$lambda.min)
C1$C$var.imp$kysymys<-rownames(C1$C$var.imp)

C2$C$var.imp<-varImp(C2$C$glmnet, lambda=C2$C$lambda.min)
C2$C$var.imp$kysymys<-rownames(C2$C$var.imp)

# Move the file to the proper place then...
save(file="malli.RData", C1, C2, ehdokkaat, yle)



