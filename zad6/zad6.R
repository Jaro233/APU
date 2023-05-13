setwd("C:/Users/jaro9/OneDrive/Desktop/apu/zad6")

install.packages("keras")
library("keras")
install.packages("tensorflow")
library("tensorflow")
tensorflow::install_tensorflow()
library(reticulate)
library(keras)
virtualenv_create("myenv")
install_keras(method="virtualenv", envname="myenv")
#load data cifar 10
cifar <- dataset_cifar10()

x_train <- cifar$train$x
x_test <- cifar$test$x
y_train <- cifar$train$y
y_test <- cifar$test$y

#--------------------------------------------------- wersja liniowa

#set up data
#change matrix shape
x_train <- array_reshape(x_train, c(nrow(x_train), 3072))
#normalize
x_train <- x_train / 255                                  

x_test <- array_reshape(x_test, c(nrow(x_test), 3072))
x_test <- x_test / 255

#set classes
y_train <- to_categorical(y_train, num_classes = 10)
y_test <- to_categorical(y_test, num_classes = 10)

#256 neurons, dropout rate 0.25
model <- keras_model_sequential() %>%
  layer_dense(units = 256, activation = "relu", input_shape = c(3072)) %>%   
  layer_dropout(rate = 0.25) %>%                                            
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 10, activation = "relu")

summary(model)

#set model parameters
model %>% compile(
  loss = "categorical_crossentropy",     #calculate loss
  optimizer = optimizer_adam(),          #optimization
  metrics = c("accuracy")                #accuracy
)

#train model
history <- model %>%
  fit(
    x_train, y_train,          #input
    epochs = 50,               
    batch_size = 128,          #128 pictures
    validation_split = 0.15
  )

#check quality
model %>% evaluate(x_test, y_test)

#------------------------------------------ wersja sp³aszczona
cifar <- dataset_cifar10()

x_train <- cifar$train$x
x_test <- cifar$test$x
y_train <- cifar$train$y
y_test <- cifar$test$y

#set up data
#normalize
x_train <- x_train / 255                                  

x_test <- x_test / 255

#set classes
y_train <- to_categorical(y_train, num_classes = 10)
y_test <- to_categorical(y_test, num_classes = 10)

#create model
model <- keras_model_sequential() %>%
  layer_flatten(input_shape = c(32, 32, 3)) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 10, activation = "softmax")

#print model
summary(model)

#set model parameters
model %>% compile(
  loss = "categorical_crossentropy",      #calculate loss
  optimizer = optimizer_adam(),           #optimization
  metrics = c("accuracy")                 #accuracy
)

#train model
history <- model %>%
  fit(
    x_train, y_train,
    epochs = 50,
    batch_size = 128,
    validation_split = 0.15
  )

#check model quality 
model %>% evaluate(x_test, y_test)

#predict
model %>% predict(x_test) %>% k_argmin()
