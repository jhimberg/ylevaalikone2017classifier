
yle <- readRDS(file=here::here("data","yle.rds"))
Lemmas <- readRDS(file=here::here("data","BOW_Lemmas_matrix.rds"))

# Store original and predicted classes
Class <- data.frame(rownames(Lemmas)) 
names(Class) <- "id"
Class <- mutate(Class, id=as.numeric(as.character(id)))
Class <- left_join(Class, select(yle, id, puolue.lyh), by="id") %>% 
  mutate(correct=factor(puolue.lyh))


# logistic elasticnet model 
model.L <- cv.glmnet(Lemmas, Class$correct, family="multinomial", 
          keep=TRUE, 
          type.measure="class", 
          standardize=TRUE, 
          intercept=TRUE, 
          alpha=0.25, 
          nfold=5)

# predict class for each with the best model 
Class$predicted = as.character(predict(model.L, L, type="class"))

saveRDS(model.L, file=here::here("data","BOW_glmmodel.rds")) 
saveRDS(Class, file=here::here("data","BOW_classes.rds")) 





