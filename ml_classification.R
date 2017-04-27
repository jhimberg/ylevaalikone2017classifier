# Load some R libraries
# You may have to install some of them first: install.packages("glmnet"), install.packages("plyr"), ...

library(glmnet)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(corrplot)
library(tidyr)
library(Rmisc)
library(curl)

### some mapping functions
f2num<-function(x){as.numeric(levels(x)[x])}

# Function for plottin average with CI
ci.plot <-function(data, attr, group="puolue.lyh", ci=.95, y.lim=c(-1,1)) {
d<-select_(data,y=attr,x=group)  
group.CI(y ~ x, d,ci=ci) %>% 
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

# Select variables (reject city spcific and free-text answers)
# Make some recoding: we are going to use multinomial (logistic) regression: 
# everything must be numeric at the end
# 

# NOTE: you have to run R in RStudio IDE to get View to work
View(yle.orig)


# Most of the variables are just for later use, if you wish to do some data mining...
# The questions are marked X... and do not have "kommentti at the end"

yle<- select(yle.orig, 
             kunta, id, sukunimi, etunimi, sukupuoli, puolue, valittu, ikä, sitoutumaton, Äidinkieli, Työnantaja, 
             Ammattiasema, Käytän.vaaleihin.rahaa, Ulkopuolisen.rahoituksen.osuus, Vuositulot, Lapsia,
             Uskonnollinen.yhteisö, 
             starts_with("X"), -ends_with("kommentti"))

# let's see the data now 
View(yle) 

####
# Recode Likert scale answers to numbers on a scale ... which is a bit worng but... never mind
# the "lautakunta" question 

yle<-rename_(yle,lautakunta="X1810.Minkä.lautakunnan.työ.on.mielestäsi.tärkeintä.omassa.kunnassasi.tulevalla.vaalikaudella..Valitse.yksi.") %>% 
mutate_at(vars(starts_with("X")), funs(yle.map)) %>%
rename(X.lautakunta=lautakunta)

# `Tarkein.lautakunta` (what is the most important board for you (social, construction, ...) in the city)
#  must be one-hot-coded for this model

yle<-mutate(yle,one=1)  
yle<-spread(yle,X.lautakunta,one, fill=0,sep=".")  


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
  select(-ok, -sitoutumaton, -missing, -puolue) %>%
  mutate_all(funs(ifelse(.=="","puuttuu",.)))

# ratio 
ggplot(count(yle,puolue.lyh) %>% mutate(N=sum(n)), aes(x=puolue.lyh,y=n/N))+geom_bar(stat="identity")

## A diversion: Just for curiosity (we are not going to use these in the classification, we could of course)
## One can plot the data for example with ggplot in various ways
## position "identity" sets relative absolute scale (but )
## position "fill" sets relative scale between 0 to 1 

ggplot(yle,aes(x=puolue.lyh,fill=Äidinkieli))+geom_bar(position="fill")

# Another example of vis
ggplot(yle,aes(x=ikä))+geom_density()
# Oh, sombody claims to be 400 years old? <18 can't be canfidaets...

# Let's demostrate "boxplot" medians & 25/75 quartiles + outliers
yle<-mutate(yle,ikä.korjattu=ifelse(ikä>122|ikä<18,NA,ikä))
ggplot(yle,aes(y=ikä.korjattu,x=puolue.lyh))+geom_boxplot(notch=TRUE)

##########

# Let's make an "engineered feature": Extremism

yle$xtrem<-select(yle,starts_with("X"), -starts_with("X.lautakunta")) %>% 
  mutate_each(funs(abs(.)==1+0)) %>% 
  rowSums(.,na.rm=TRUE) 
yle<-mutate(yle,xtrem=xtrem/33)

# Statplot: means with 95% conf interval
# the function is based on ggplot... 
ci.plot(yle,"xtrem","puolue.lyh",y.lim=c(0,1), ci=.95)

# Plot. NB: THIS is a bit WRONG since Likert scale is not continuous!
# change the attr to see other attributes

# Statplot: means with 95% conf interval... for one variable

ci.plot(yle,"X128.Kuntien.tulee.tarjota.lasten.päivähoidon.varhaiskasvatus.ilmaiseksi.kaikille.lapsille.","puolue.lyh", ci=.95)

# Ok, now the model
# glmnet for multinomial requires data in matrix and target in factor

X<-select(yle,starts_with("X"),xtrem) %>% as.matrix
row.names(X)<-yle$id
X[is.na(X)]<-0
y=factor(yle$puolue.lyh)

# cv.glmnet makes the modelling, finds penalization weight by cross-validation. We'll
# use the results from the optimal lambda, classification is taken from the cv (unseen folds)
# Let's try just LASSO (alpha=1) penalization - you can check for Ridge -penalization alpha=0 which gives a bit different results 
# or something in between like, like alpha=0.5

classifier<-cv.glmnet(X,y,family="multinomial", 
                      keep=TRUE, 
                      type.measure="class", 
                      standardize=TRUE, 
                      intercept=TRUE, 
                      alpha=1, 
                      nfold=5)


# Ugly stuff for getting the cross-validated output from optimal regularization ####

lab<-classifier$glmnet.fit$classnames
response<-classifier$fit.preval[,,which(classifier$lambda.min==classifier$lambda)]
r<-tolower(classifier$glmnet.fit$classnames[apply(response,1,which.max)])
colnames(response)<-classifier$glmnet.fit$classnames
r<-classifier$fit.preval[,,which(classifier$lambda.min==classifier$lambda)];
colnames(r)<-classifier$glmnet.fit$classnames

posterior <- data.frame(id=yle$id, 
                        puolue=yle$puolue.lyh, 
                        r,
                        puolue.e=tolower(classifier$glmnet.fit$classnames[apply(r,1,which.max)]))


posterior <-mutate(posterior,correct=ifelse(puolue==toupper(puolue.e),1,0))

## Confusion matrix (there's a R package for this...)

p<-table(posterior$puolue,posterior$puolue.e)
p<-p[match(colnames(p),tolower(rownames(p))),]

# See the confusion matrix
# Capital letters (rows) eg. SDP => the actual party
# lowcase letters (columns) eg. sdp => the predicted party

p

# Plot the confusion scaled 100% row-wise; how many percents of actual parties go into the predicted class
# for example sdp-SDP cell shows how many percents of actual SDP members will be labeled as members of SDP by the classifier
corrplot(prop.table(p,1),method="shade",is.corr=FALSE,addCoef.col=2,
         addCoefasPercent=TRUE,col=colorRampPalette(c("white","white","black"),1)(80))

# Plot the confusion in another way 100% coumn-wise...
# for example sdp-SDP cell shows how many percents candidates labeled as SDP members by the classifier are acutally from SDP
corrplot(prop.table(p,2),method="shade",is.corr=FALSE,addCoef.col=2,
         addCoefasPercent=TRUE,col=colorRampPalette(c("white","white","black"),1)(80))

## Let's check some candidates!! What's the probability (as seen by the classifier!!) to belong to a party
ehdokkaat<-left_join(select(yle,etunimi,sukunimi,id,valittu,kunta),posterior,by="id")

View(filter(ehdokkaat,sukunimi %in% c("Orpo","Andersson","Niinistö","Lohela") & etunimi %in% c("Li","Petteri","Maria","Ville")))

####
# Even more ugly thing for getting the coefficients and plotting them

puolue<-rownames(p)
j<-data.frame(matrix(0,41,14))
names(j)<-puolue
for (i in puolue) j[i]<-coef(classifier)[[i]] %>% as.numeric
j$question<-rownames(coef(classifier)$SDP)

k<-melt(j) %>%
  filter(question!="intercept") %>% 
  rename(puolue=variable, kysymys=question) %>%
  mutate(txt=ifelse(value==0,"0",sprintf("%1.2f", value)))

k<-arrange(k,kysymys) 

ggplot(data=k,aes(y=kysymys, x=puolue,fill = value, label = txt)) + 
  geom_tile() + geom_text(size=4, colour = "black") +
  scale_fill_gradient2(low = "#c51b7d", high = "#4d9221",mid="#fdfdfd",  midpoint=0) +
  ylim(rev(sort(unique(k$kysymys))))









