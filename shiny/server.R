# Load the ggplot2 package which provides
# the 'mpg' dataset.

library(ggplot2)
library(dplyr)
library(glmnet)
library(corrplot)
library(reshape2)

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


posterior <-mutate(posterior, correct=ifelse(puolue==toupper(puolue.e),1,0))

confusion.matrix<-table(posterior$puolue,posterior$puolue.e)
confusion.matrix<-confusion.matrix[match(colnames(confusion.matrix),tolower(rownames(confusion.matrix))),]

puolue<-rownames(confusion.matrix)
j<-data.frame(matrix(0,dim(X)[2]+1,length(unique(ehdokkaat$puolue))))
names(j)<-puolue
for (i in puolue) j[i]<-coef(classifier)[[i]] %>% as.numeric
j$question<-rownames(coef(classifier)$SDP)

k<-melt(j,id.vars="question") %>%
  filter(question != "intercept") %>% 
  rename(puolue=variable, kysymys=question) %>%
  mutate(txt=ifelse(value==0,"0",sprintf("%1.2f", value)))

k<-arrange(k,kysymys) 

confusion.plot<-function(confusionmatrix, margin=1,order="AOE") {
  corrplot(prop.table(confusionmatrix,margin), method="shade", is.corr=FALSE, addCoef.col=2,
           addCoefasPercent=TRUE,col=colorRampPalette(c("white","white","black"),1)(80),
           order=order)
}

y <- select(ehdokkaat, etunimi, sukunimi, kunta, puolue, puolue.e) %>% 
  rename(ennustettu=puolue.e, oikea=puolue)

function(input, output, session) {
  observe({
    updateRadioButtons(session, "q1", selected = X[input$table_rows_selected,1])
    updateRadioButtons(session, "q2", selected = X[input$table_rows_selected,2])
    updateRadioButtons(session, "q3", selected = X[input$table_rows_selected,3])
    updateRadioButtons(session, "q4", selected = X[input$table_rows_selected,4])
    updateRadioButtons(session, "q5", selected = X[input$table_rows_selected,5])
    updateRadioButtons(session, "q6", selected = X[input$table_rows_selected,6])
    updateRadioButtons(session, "q7", selected = X[input$table_rows_selected,7])
    updateRadioButtons(session, "q8", selected = X[input$table_rows_selected,8])
    updateRadioButtons(session, "q9", selected = X[input$table_rows_selected,9])
    updateRadioButtons(session, "q10", selected = X[input$table_rows_selected,10])
    updateRadioButtons(session, "q11", selected = X[input$table_rows_selected,11])
    updateRadioButtons(session, "q12", selected = X[input$table_rows_selected,12])
    updateRadioButtons(session, "q13", selected = X[input$table_rows_selected,13])
    updateRadioButtons(session, "q14", selected = X[input$table_rows_selected,14])
    updateRadioButtons(session, "q15", selected = X[input$table_rows_selected,15])
    updateRadioButtons(session, "q16", selected = X[input$table_rows_selected,16])
    updateRadioButtons(session, "q17", selected = X[input$table_rows_selected,17])
    updateRadioButtons(session, "q18", selected = X[input$table_rows_selected,18])
    updateRadioButtons(session, "q19", selected = X[input$table_rows_selected,19])
    updateRadioButtons(session, "q20", selected = X[input$table_rows_selected,20])
    updateRadioButtons(session, "q21", selected = X[input$table_rows_selected,21])
    updateRadioButtons(session, "q22", selected = X[input$table_rows_selected,22])
    updateRadioButtons(session, "q23", selected = X[input$table_rows_selected,23])
    updateRadioButtons(session, "q24", selected = X[input$table_rows_selected,24])
    updateRadioButtons(session, "q25", selected = X[input$table_rows_selected,25])
    updateRadioButtons(session, "q26", selected = X[input$table_rows_selected,26])
    updateRadioButtons(session, "q27", selected = X[input$table_rows_selected,27])
    updateRadioButtons(session, "q28", selected = X[input$table_rows_selected,28])
    updateRadioButtons(session, "q29", selected = X[input$table_rows_selected,29])
    updateRadioButtons(session, "q30", selected = X[input$table_rows_selected,30])
    updateRadioButtons(session, "q31", selected = X[input$table_rows_selected,31])
    updateRadioButtons(session, "q32", selected = X[input$table_rows_selected,32])
    updateRadioButtons(session, "q33", selected = X[input$table_rows_selected,33])
    updateRadioButtons(session, "z1", selected = X.lautakunta[X[input$table_rows_selected,34:39]*seq(6)][1])
  })
  
  predicted.party <- reactive({
    x<-as.numeric(c(
      input$q1,
      input$q2,
      input$q3,
      input$q4,
      input$q5,
      input$q6,
      input$q7,
      input$q8,
      input$q9,
      input$q10,
      input$q11,
      input$q12,
      input$q13,
      input$q14,
      input$q15,
      input$q16,
      input$q17,
      input$q18,
      input$q19,
      input$q20,
      input$q21,
      input$q22,
      input$q23,
      input$q24,
      input$q25,
      input$q26,
      input$q27,
      input$q28,
      input$q29,
      input$q30,
      input$q31,
      input$q32,
      input$q33,
      (input$z1=="ohi")+0, 
      (input$z1=="eli")+0,
      (input$z1=="kas")+0,
      (input$z1=="kul")+0,
      (input$z1=="sos")+0,
      (input$z1=="ymp")+0))
    
    x[40]<-sum(abs(x[1:33])==1)/33
    x<-t(as.matrix(x))
    
    predict(classifier$glmnet.fit, newx=x, type="response")[1,,classifier$lambda.min==classifier$lambda] 
    
  })
  
  output$table <- DT::renderDataTable(y,
                                      selection = 'single', 
                                      server = TRUE, 
                                      rownames = FALSE)
  
  output$plot <- renderPlot({
    i<-predicted.party() %>% data.frame
    i$puolue <- rownames(i)
    i<-rename_(i,"p"=".")
    p.bar<-ggplot(i,aes(x=puolue,y=p))+
      geom_bar(stat="identity")+
      ggtitle("Mallin ennuste")
    p.bar
  })
  
  output$confusion <- renderPlot({
    confusion.matrix<-table(posterior$puolue.e, posterior$puolue)
    confusion.plot(confusion.matrix, margin=2, order="original")
  })
  
  output$coef <- renderPlot({
    ggplot(data=k, aes(y=kysymys, x=puolue, fill = value, label = txt)) + geom_tile() + 
      geom_text(size=4, colour = "black") +
      scale_fill_gradient2(low = "#c51b7d", high = "#4d9221",mid="#fdfdfd",  midpoint=0) +
      ylim(rev(sort(unique(k$kysymys))))
  })
  
}