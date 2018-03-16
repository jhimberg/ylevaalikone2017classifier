
yle <- readRDS(here::here(file="data","yle.rds"))
Lemmas <- readRDS("data/BOW_Lemmas_matrix.rds") 

## Topic model 
topics<- LDA(Lemmas, 
             k=10, 
             method = "VEM", 
             control = list(alpha=1, verbose=1))


saveRDS(topics, file=here::here("BOW_topics.rds"))

order.columns <- function(df, first.names) select(df, one_of(c(first.names, setdiff(names(df), first.names))))

relevancy <- function(topics, N.terms, lambda = NA) {
  if (!is.na(lambda) & (lambda < 0 | lambda > 1)) error("lambda must be [0,1]")
  Terms <- as.data.frame(posterior(topics)$terms %>% t) 
  names(Terms) <- paste0("T", names(Terms))
  Terms$term<-rownames(Terms)
  
  if (typeof(N.terms)=="double") {
    N.terms <- as.data.frame(N.terms)
    names(N.terms) <- "N"
    N.terms$term <- row.names(N.terms)
  }
  
  Terms <- left_join(Terms, N.terms, by="term") %>%
    mutate(p.term = N/sum(N))
  
  Terms$sum.topics <- select(Terms, -N, -term, -p.term) %>% rowSums
  if (!is.na(lambda)) Terms<-mutate_at(Terms, vars(matches("^T[0-9]*$")), .funs=funs(mod.lift = lambda * log(. / p.term) + (1-lambda)*log(.)))
  Terms<-mutate_at(Terms, vars(matches("^T[0-9]*$"), -ends_with("_mod.lift")), funs(relevancy=log(N)*(. /sum.topics)))
  Terms<-select(Terms, -sum.topics)
  
  return(order.columns(Terms, c("term","N","p.term")))
}




