load("/home/rstudio/ShinyApps/sample-apps/test/ehdokkaat.RData")

puolueet <- unique(as.character(ehdokkaat$puolue))
kunnat <- unique(ehdokkaat$kunta)

X.choice.values <- c(-1,-.5, 0, .5, 1)
X.choice.labels <- c("--", "-", "o", "+", "++")
X.lautakunta <- c("ohi","eli","kas","kul","sos","ymp")

question<-gsub("\\."," ",colnames(X))

# 
ex <- c(rep(0,39),1)
z1="ohi"



