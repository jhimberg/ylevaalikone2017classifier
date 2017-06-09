# Ks. global.R!

library(plyr)
library(dplyr)
library(glmnet)
library(reshape2)
library(ggplot2)
library(corrplot)

# Ehdokkaista luokat (datamatriisi on X ja sen pitää olla oikeassa järjestykses!)

shinyServer(function(input, output, session) {
  observe({
    updateRadioButtons(session, "q1", selected = C1$X[input$table_rows_selected,1])
    updateRadioButtons(session, "q2", selected = C1$X[input$table_rows_selected,2])
    updateRadioButtons(session, "q3", selected = C1$X[input$table_rows_selected,3])
    updateRadioButtons(session, "q4", selected = C1$X[input$table_rows_selected,4])
    updateRadioButtons(session, "q5", selected = C1$X[input$table_rows_selected,5])
    updateRadioButtons(session, "q6", selected = C1$X[input$table_rows_selected,6])
    updateRadioButtons(session, "q7", selected = C1$X[input$table_rows_selected,7])
    updateRadioButtons(session, "q8", selected = C1$X[input$table_rows_selected,8])
    updateRadioButtons(session, "q9", selected = C1$X[input$table_rows_selected,9])
    updateRadioButtons(session, "q10", selected = C1$X[input$table_rows_selected,10])
    updateRadioButtons(session, "q11", selected = C1$X[input$table_rows_selected,11])
    updateRadioButtons(session, "q12", selected = C1$X[input$table_rows_selected,12])
    updateRadioButtons(session, "q13", selected = C1$X[input$table_rows_selected,13])
    updateRadioButtons(session, "q14", selected = C1$X[input$table_rows_selected,14])
    updateRadioButtons(session, "q15", selected = C1$X[input$table_rows_selected,15])
    updateRadioButtons(session, "q16", selected = C1$X[input$table_rows_selected,16])
    updateRadioButtons(session, "q17", selected = C1$X[input$table_rows_selected,17])
    updateRadioButtons(session, "q18", selected = C1$X[input$table_rows_selected,18])
    updateRadioButtons(session, "q19", selected = C1$X[input$table_rows_selected,19])
    updateRadioButtons(session, "q20", selected = C1$X[input$table_rows_selected,20])
    updateRadioButtons(session, "q21", selected = C1$X[input$table_rows_selected,21])
    updateRadioButtons(session, "q22", selected = C1$X[input$table_rows_selected,22])
    updateRadioButtons(session, "q23", selected = C1$X[input$table_rows_selected,23])
    updateRadioButtons(session, "q24", selected = C1$X[input$table_rows_selected,24])
    updateRadioButtons(session, "q25", selected = C1$X[input$table_rows_selected,25])
    updateRadioButtons(session, "q26", selected = C1$X[input$table_rows_selected,26])
    updateRadioButtons(session, "q27", selected = C1$X[input$table_rows_selected,27])
    updateRadioButtons(session, "q28", selected = C1$X[input$table_rows_selected,28])
    updateRadioButtons(session, "q29", selected = C1$X[input$table_rows_selected,29])
    updateRadioButtons(session, "q30", selected = C1$X[input$table_rows_selected,30])
    updateRadioButtons(session, "q31", selected = C1$X[input$table_rows_selected,31])
    updateRadioButtons(session, "q32", selected = C1$X[input$table_rows_selected,32])
    updateRadioButtons(session, "q33", selected = C1$X[input$table_rows_selected,33])
    updateRadioButtons(session, "z1", selected = X.lautakunta[C1$X[input$table_rows_selected,34:38]*seq(5)][1])
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
      #(input$z1=="ohi")+0, 
      (input$z1=="eli")+0,
      (input$z1=="kas")+0,
      (input$z1=="kul")+0,
      (input$z1=="sos")+0,
      (input$z1=="ymp")+0))
    
    x[39]<-sum(abs(x[1:33])==1)/33
    x<-t(as.matrix(x))
    
    predict(C1$C$glmnet, newx=x, type="response")[1,,C1$C$lambda.min==C1$C$lambda] 

  })
  
  output$kysymykset.puolue <- renderPlot({
    p.var<-ggplot(yle, aes_string(x="vakiluku.lg10", y=input$kysymykset))+
      geom_smooth(method="gam",formula=y~s(x,k=4), size=0.2,color="black")+
      geom_jitter(alpha=0.05, size=.5, color="red")+facet_wrap(~puolue.lyh)+
      ggtitle(input$kysymykset)+xlab("log10(kunnan väkiluku)")+ylab("Vastaukset -1...1")
    p.var
  })

  output$kysymykset.kaikki <- renderPlot({
    p.var<-ggplot(yle, aes_string(x="vakiluku.lg10", y=input$kysymykset))+
      geom_smooth(method="gam",formula=y~s(x,k=4), size=0.2,color="black")+
      geom_jitter(alpha=0.05, size=.5, color="red")+
      ggtitle(input$kysymykset)+xlab("log10(kunnan väkiluku)")+ylab("Vastaukset -1...1")
    p.var
  })
  
  output$table <- DT::renderDataTable(select(ehdokkaat, -malli2),
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
  
  output$confusion.m1 <- renderPlot({
    confusion.plot(C1$C$confusion.matrix, margin=2, order="original")
  })

  output$confusion.m2 <- renderPlot({
    confusion.plot(C2$C$confusion.matrix, margin=2, order="original")
  })
  
  output$quality.m1 <- renderText({
    round(mean(as.character(C1$C$cv$oikea)==toupper(as.character(C1$C$cv$ennuste)))*1000)/10
  })
  
  output$quality.m2 <- renderText({
    round(mean(as.character(C2$C$cv$oikea)==toupper(as.character(C2$C$cv$ennuste)))*1000)/10
  })

  output$coef.m1 <- renderPlot({
    d <- C1$C$kertoimet %>% filter(kysymys != "(Intercept)")
    if (input$puolueet == "Aakkosjärjestys") {
      ord.y <- rev(sort(unique(d$kysymys)))
      ord.x <- puolueet
    } else {
      ord.x <-puolueet
      #ord.x <- C1$C$confusion.matrix[, input$puolueet] %>% prop.table %>% sort %>% rev %>% names %>% toupper
      ord.y <- arrange_(C1$C$var.imp[c(input$puolueet, "kysymys")], input$puolueet)$kysymys
    }
    ggplot(data = d, aes(
      y = kysymys,
      x = puolue,
      fill = value,
      label = txt
    )) +
      geom_tile() +
      geom_text(size = 4, colour = "black") +
      scale_fill_gradient2(
        low = "#c51b7d",
        high = "#4d9221",
        mid = "#fdfdfd",
        midpoint = 0
      ) +
      ylim(ord.y) +
      xlim(ord.x)
  })
  
  output$coef.m2 <- renderPlot({
    d <- C2$C$kertoimet %>% filter(kysymys != "(Intercept)")
    if (input$puolueet == "Aakkosjärjestys") {
      ord.y <- rev(sort(unique(d$kysymys)))
      ord.x <- puolueet
    } else {
      ord.x <-puolueet
      #ord.x <- C2$C$confusion.matrix[, input$puolueet] %>% prop.table %>% sort %>% rev %>% names %>% toupper
      ord.y <- arrange_(C2$C$var.imp[c(input$puolueet, "kysymys")], input$puolueet)$kysymys
    }
    ggplot(data = d, aes(
      y = kysymys,
      x = puolue,
      fill = value,
      label = txt
    )) +
      geom_tile() +
      geom_text(size = 4, colour = "black") +
      scale_fill_gradient2(
        low = "#c51b7d",
        high = "#4d9221",
        mid = "#fdfdfd",
        midpoint = 0
      ) +
      ylim(ord.y) +
      xlim(ord.x)
  })
})

