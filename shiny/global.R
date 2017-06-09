load("/home/rstudio/ShinyApps/sample-apps/test/malli.RData")

## Ristiinvalidoinnin antama luokittelu sekaannusmatriisiin!

## sekaannusmatriisi
confusion.plot<-function(confusionmatrix, margin=1,order="AOE") {
  corrplot(prop.table(confusionmatrix,margin), method="shade", is.corr=FALSE, addCoef.col=2,
           addCoefasPercent=TRUE,col=colorRampPalette(c("white","white","black"),1)(80),
           order=order)
}

puolueet <- unique(as.character(ehdokkaat$oikea))
kunnat <- unique(ehdokkaat$kunta)

X.choice.values <- c(-1,-.5, 0, .5, 1)
X.choice.labels <- c("--", "-", "o", "+", "++")
X.lautakunta <- c("eli","kas","kul","sos","ymp")

question<-gsub("\\."," ",colnames(C1$X))

# 
ex <- c(rep(0,39),1)
z1="ohi"



