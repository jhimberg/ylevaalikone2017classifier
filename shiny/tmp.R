Topics <- as.data.frame(posterior(topics)$topics)
names(Topics) <- paste0("T",names(Topics))
#Topics <- bind_cols(select(filter(Malli.3$N, date=="2017-09-30" & ryhma=="Normaali" & ajoneuvonkaytto=="Yksityinen"), 
#                             ajoneuvonkaytto, ryhma, date, pono.2, pono.3, sum.N), Topics)
#Topics.2 <- group_by(Topics.3, ajoneuvonkaytto, ryhma, date, pono.2) %>% summarise_at(., .vars=vars(matches("T[0-9]")), .fun=funs(sum(. * sum.N)/sum(sum.N))) 

## Termit ja mallistatistiikat

Terms <- as.data.frame(posterior(topics)$terms %>% t) 
names(Terms) <- paste0("T",names(Terms))

Terms <- mutate(Terms, malli=row.names(Terms)) 

Terms <- left_join(Terms, S, by = "malli") %>% 
  mutate(p.malli = N.malli/sum(N.malli))

Terms$sum.Topic <- select(Terms,-N.malli,-malli,-p.malli) %>% rowSums

# Sinkkosen relevassi: funs=funs(sinkkonen=log(N.malli)*(. /sum.Topic))
# tavallinen Lift = p.malli|topic / p.malli
#%>% mutate_at(., vars(matches("T[0-9]$")), .funs=funs(lift=./p.malli))

Terms <- mutate_at(Terms, vars(matches("T[0-9]$")), .funs=funs(relevanssi=log(N.malli)*(. /sum.Topic))) %>%
  #select(-matches("T[0-9]$"), -p.malli, -sum.Topic) %>% 
  select(-p.malli, -sum.Topic) %>% 
  mutate(date="2017-09-30", ryhma="Normaali", ajoneuvonkaytto="Yksityinen")

d <- CBOW$C$kertoimet %>% filter(kysymys != "(Intercept)")
  ord.y <- rev(sort(unique(d$kysymys)))
  ord.x <- puolueet

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


lemmas.b <- NULL
for (i in seq(1, dim(corpus)[1])) {
  print(i)
  tmp <- data.frame(
    id = corpus$id[i],
    lemma = corpus$lemma[i] %>% 
      str_split(" ") %>% unlist,
    original = corpus$txt[i] %>% gsub("[[:punct:]]"," ",.) %>% gsub(" *"," ",.) %>% str_split(" ") %>% unlist,
    stringsAsFactors = FALSE
    )
  lemmas.b <- bind_rows(lemmas.b, tmp)
}