library(keras)

num_classes <- nlevels(C1$C$cv$oikea)

# the data, shuffled and split between train and test sets

N.X<-dim(C1$X)[1]
data.sample <- rep("train", N.X)
data.sample[sample.int(N.X, size=round(N.X/6), replace=FALSE)] <- "test"

x_train <- C1$X[data.sample=="train",]
y_train <- to_categorical(as.numeric(C1$C$cv$oikea)[data.sample=="train"]-1, 
                         num_classes=nlevels(C2$C$cv$oikea))

x_test <- C1$X[data.sample=="test",]
y_test <- to_categorical(as.numeric(C1$C$cv$oikea)[data.sample=="test"]-1, 
                         num_classes=nlevels(C1$C$cv$oikea))


cat(dim(x_train)[1], 'train samples\n')
cat(dim(x_test)[1], 'test samples\n')

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 150, activation = 'relu', input_shape = c(39)) %>% 
  layer_dense(units = 150, activation = 'relu') %>%
  layer_dense(units = 14, activation = 'softmax')

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adam(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train,
  batch_size = 3000,
  epochs = 600,
  verbose = 1,
  validation_split = 0.3
)

score <- model %>% evaluate(
  x_test, y_test,
  verbose = 0
)

cat('Test loss:', score[[1]], '\n')
cat('Test accuracy:', score[[2]], '\n')

