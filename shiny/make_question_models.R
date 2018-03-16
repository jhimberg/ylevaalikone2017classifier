
yle <- readRDS(file=here::here("data","yle.rds"))

# Ok, now the model 1:
# glmnet for multinomial requires data in matrix and target in factor

# Model 1

X <-select(yle, matches("^X|xtrem")) %>% select(-X.Lautakunta.) %>% as.matrix

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

source(here::here("make_classifier_data.R"))
C1 <- make.classifier.data(classifier=classifier, X, yle=yle)

####

# Model 2

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

classifier<-cv.glmnet(X, y, family="multinomial", 
                      keep=TRUE, 
                      type.measure="class", 
                      standardize=TRUE, 
                      intercept=TRUE, 
                      alpha=0.5, 
                      nfold=5)

source(here:here("make_classifier_data.R"))

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

## load 

save(file=here::here("data","malli.RData"), C1, C2, ehdokkaat, yle)
load
saveRDS(yle, file=here::here("data","yle.rds"))
saveRDS(yle.orig, file=here::here("data","yle_orig.rds"))




