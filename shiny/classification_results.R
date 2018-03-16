# An ugly function for making a classifier and storing results
#
make.classifier.data <- function(classifier, X, yle=yle) {
  # Ugly staff for geting the cross validated results for best lambda
  
  C <-list(glmnet=classifier$glmnet.fit,
           lambda.min=classifier$lambda.min,
           lambda=classifier$lambda,
           classnames=classifier$glmnet.fit$classnames)  
  
  # cv.luokka
  ennuste<-tolower(C$classnames[apply(classifier$fit.preval[,,which(C$lambda.min==C$lambda)], 1, which.max)])
  ennuste <- as.data.frame(ennuste) %>% 
    mutate(id=rownames(X)) 
  
  response <- classifier$fit.preval[,,which(classifier$lambda.min==classifier$lambda)] %>% as.data.frame
  names(response) <- C$classnames
  response<-mutate(response, id=rownames(X))
  
  C$cv <- transmute(yle, id=as.character(id), oikea=puolue.lyh) %>%
    left_join(., response, by="id") %>%
    left_join(., ennuste, by="id")
  
  # Confusion matrix
  
  C$confusion.matrix <- table(C$cv$ennuste, C$cv$oikea)
  
  ## Coefficients!
  
  puolue<-colnames(C$confusion.matrix)
  
  j<-data.frame(matrix(0,dim(X)[2]+1,length(puolue)))
  
  #names(j)<-puolue
  #for (i in puolue) j[i]<-coef(classifier)[[i]] %>% as.numeric
  #j$question<-rownames(coef(classifier)$SDP)
  
  names(j)<-puolue
  
  for (i in puolue) j[i]<-predict(C$glmnet, s=C$lambda.min,type="coef")[[i]] %>% 
    unlist %>% 
    as.numeric
  
  j$question<-rownames(coef(classifier)$SDP)
  
  C$kertoimet <- melt(j, id.vars="question") %>%
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
