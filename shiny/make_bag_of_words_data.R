library(tidyverse)
library(stringr)
library(purrr)
library(purrrlyr)
library(lubridate)
library(RCurl)
library(httr)
library(irlba)
library(jsonlite)
library(glmnet)

stopwords <- c(
  "olla",
  "kuka",
  "joka",
  "tulla",
  "nämä",
  "nuo",
  "ne",
  "minä",
  "sinä",
  "me",
  "he",
  "hän",
  "ei",
  "tämä",
  "kaikki",
  "kanssa",
  "tuo",
  "mikä",
  "voida",
  "te",
  "se",
  "eräs",
  "mikään",
  "jokin",
  "jo",
  "ellei",
  "vielä",
  "vain",
  "vaikka",
  "vai",
  "vaan",
  "tuota",
  "tuosta",
  "tuossa",
  "tuonne",
  "tuolta",
  "tuolla",
  "tuohon",
  "tuntua",
  "tosin",
  "toki",
  "todella",
  "tietysti",
  "täysin",
  "täten",
  "tätä",
  "tästä",
  "tässä",
  "tänne",
  "tai",
  "tähän",
  "taas",
  "täältä",
  "täällä",
  "sitten",
  "siten",
  "sitä",
  "sinut",
  "sinne",
  "silti",
  "silloin",
  "sillä",
  "siksi",
  "siitä",
  "siis",
  "siinä",
  "siihen",
  "sieltä",
  "siellä",
  "sen",
  "sekä",
  "nyt",
  "n",
  "noita",
  "noin",
  "niin",
  "näissä",
  "näin",
  "myöskin",
  "myöskään",
  "myös",
  "muun",
  "muuanne",
  "muualta",
  "muualle",
  "muualla",
  "mutta",
  "monta",
  "monesti",
  "miten",
  "mitä",
  "mistä",
  "missä",
  "minne",
  "minkä",
  "milloin",
  "miksi",
  "mikin",
  "mikäli",
  "mihin",
  "kuten",
  "kun",
  "kumpikin",
  "kumpikaan",
  "kumpi",
  "kumpainenkaan",
  "kumpainen",
  "kukin",
  "kukaan",
  "kuitenkin",
  "kuitenkaan",
  "kuinka",
  "kuin",
  "koskaan",
  "koska",
  "kenties",
  "kai",
  "juuri",
  "jotta",
  "jotenkin",
  "joten",
  "jotain",
  "jota",
  "josta",
  "joskus",
  "jos",
  "jopa",
  "joo",
  "jonne",
  "jompikumpi",
  "jolloin",
  "jolla",
  "joku",
  "johon",
  "ja",
  "ettei",
  "että",
  "eli", "eri", "muu", "osa", "mm", "koko", "esim", "voi", 
  "myötä", "vasta", "no", "usea", "yli",
  "det","är","att","och","i","en", "ett", "fi", "som", "vi", "ni", "jag", "han", "hon", 
  "har", "på", "men", "så", "om", "av", "sig", "mig", "dig", "er", "oss", 
  "vill", "får", "sku", "andra", "då", "många")

# Install milankinen/seco/las from dockerhub
# docker run --rm -p 19990:9000 -t -i milankinen/seco-las 
# curl 'http://localhost:19990/las/baseform?text=Terve+maailmaan!&locale=fi'
# =>
# "terve maailma !"
#

# lemmatize text
seco.lemmatization <- function(txt, seco.url="http://localhost:19990/las/baseform", locale="fi") {
  # txt: text will be set to lowercase, punctuation is removed
  # Seco Language analysis server ()
  r <- POST(seco.url, body = list(text = txt, locale = locale), encode = "form")
  txt<-content(r, "text") %>% 
    gsub("[[:punct:]]","",.) %>% 
    gsub(" +"," ",.) %>% 
    gsub("^ | $","",.)
  return(txt)
}

#udpipe.lemmatization <- function(txt, udmodel) {
#  txt<-udpipe_annotate(udmodel, txt) %>% as.data.frame %>% 
#    filter(upos!="PUNCT")$lemma
#  return(txt)
#}

# recognize language
seco.language <- function(txt, seco.url="http://localhost:19990/las/identify") {
  # txt: text will be set to lowercase, punctuation is removed
  # Seco Language analysis server ()
  r <- POST(seco.url, body = list(text = txt), encode = "form")
  lang <- content(r, "text") %>% fromJSON %>% .$locale
  return(lang)
}

# Map party names to acronyms
map.puolue<-function(x) plyr::revalue(x,c(
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


# Load data 

yle<-reaRDS(file=here::here("data","yle.rds"))

# Concatenate text fields
# Remove candidates with too many missing values 

corpus <- select(yle.orig,id, starts_with("Vaalilupaus"), ends_with("kommentti"),
          Miksi.juuri.sinut.kannattaisi.valita.kunnanvaltuustoon., 
          Mitä.asioita.haluat.edistää.tai.ajaa.tulevalla.vaalikaudella.) %>% 
  gather(., label, txt, -id) %>%
  group_by(id) %>% 
  summarise(txt=paste(txt, collapse=" ") %>% 
              gsub("[0-9]","",.) %>% 
              gsub(" +"," ",.))

# Lemmatise
corpus$lemma <- ""
for (i in seq(1, dim(yle.orig)[1])) {
  print(i)
  corpus$lemma[i] <- seco.lemmatization(corpus$txt[i])
}

# Detect languaage; using loop & filtering since map_chr(x, ) crashed all the time 
lang <- c()
for (i in 1:length(corpus$txt)) {
  print(i)
  if (str_length(gsub(" ","", stringi::stri_enc_toutf8(corpus$txt[i], validate=TRUE))) < 5)
    tmp <- "NA" 
  else 
    tmp <- seco.language(stringi::stri_enc_toutf8(X$txt[i], validate=TRUE))
  lang[i]<-tmp
}
corpus$language<-lang

## copurs stores concatenated texts, lemmatised texts, text length, and language
corpus <- mutate(corpus, l.text=str_length(txt))

saveRDS(corpus, file=here::here("data","corpus.rds"))

# Change the data into long format: person_id1, word1; person_id1, word2; ..., person_id1, wordK
# person_id2, word1, ..

lemmas <- NULL
for (i in seq(1, dim(corpus)[1])) {
  print(i)
  tmp <- data.frame(id = corpus$id[i], lemma = corpus$lemma[i] %>% 
                      str_split(" ") %>% 
                      unlist, stringsAsFactors = FALSE)
  lemmas <- bind_rows(lemmas, tmp)
}

saveRDS(lemmas, file=here::here("data","lemmas_all.rds"))

# filter out stopwords & count words for each document
lemmas <- mutate(lemmas, lemma = str_trim(tolower(lemma))) %>% 
  filter(!(lemma %in% stopwords)) %>% 
  count(id, lemma) 

# filter out very short texts <= 10 chars, and words appearing in less than 6 times
# make words & ids into factors and filter out lemmas less than 3 chars
# recode into a sparse matrix

lemmas <- left_join(lemmas, select(yle, puolue.lyh, id), by="id") %>% 
  left_join(select(corpus, l.text, id, language),by="id") %>% 
  filter(l.text > 10) %>% 
  group_by(lemma) %>% 
  mutate(N=sum(n)) %>% 
  ungroup %>% 
  filter(N > 5)

lemmas <- mutate(lemmas, 
                id=factor(id), 
                lemma=factor(lemma)) %>% 
  filter(str_length(lemma) >=3) %>% 
  droplevels


saveRDS(lemmas, file=here::here("data","BOW_lemmas.rds"))

Lemmas <-Matrix::sparseMatrix(i=as.numeric(lemmas$id), 
                        j=as.numeric(lemmas$lemma), 
                        x=lemmas$n, 
                        dims = c(max(as.numeric(lemmas$id)), max(as.numeric(lemmas$lemma))),
                        dimnames = list(id=levels(lemmas$id), lemma=levels(lemmas$lemma)))

saveRDS(Lemmas, file=here::here("data","BOW_Lemmas_matrix.rds"))

