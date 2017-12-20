

# Statistics 440
# Case Study 3 Code
# Brad Smallwood, 301228034
# In collaboration with Barinder Thind

# Librarys 
library(pls)
library(ggplot2)
library(tidyverse)
library(jsonlite)
library(stringr)
library(stringi)
library(SpatialNP)
library(rlist)
library(mixtools)
library(ripa)
library(adimpro)
library(glmnet)
library(caret)
library(randomForest)
library(neuralnet)
library(xgboost)
library(h2o)
library(mxnet)
library(Metrics)
library(RJSONIO)
library(abind)
library(kohonen)

# Cleaning Functions

make.image.matrix = function(pic_string){
  pic = pic_string
  
  ## How many pixels (number of commas plus 1)
  Npix = 1 + str_count(pic,",") ## 5625 pixels, this is a 75x75 image
  img_size = sqrt(Npix)
  
  ## cleaning and splitting
  pic = str_replace(pic, "c\\(","")
  pic = str_replace(pic, "\\)","")
  pixels = str_split_fixed(pic,",",Npix)
  pixels = as.numeric(pixels)
  
  ## Formatting into a matrix for image()
  pixels_mat = matrix(pixels,nrow=75,ncol=75)
  return(pixels_mat)
  
}

# Will be used in noiseReduction
sq.loglik.ratio = function(x, mu1,mu2,sig1,sig2){
  LL1 = dnorm(x,mu1,sig1)
  LL2 = dnorm(x,mu2,sig2)
  output = log(LL1/LL2)^2
  return(output)
  
}

dataStructuring <- function(data){
  # Take both pictures from each observation
  # Put into a list of lists structure
  outer_list <- list() # Initialize List.
  for(i in 1:nrow(data)){ # Iterate through each observation.
    pixel_pic1 = make.image.matrix(as.character(data[i,2])) # Split out the first image
    pixel_pic2 = make.image.matrix(as.character(data[i,3])) # Split out the second image
    inner_list<- list(pixel_pic1, pixel_pic2) # Combine the two pictures into a list
    outer_list <- list.append(outer_list, inner_list) # Append that list to the big list of all observations
    print(i) # Cuz I like knowing how much longer it is going to take.
  }
  return(outer_list) # Return a nested list where each element is a list for an observation and contains two dataframes for the images.
}

# Combine the Images
combineImages <- function(list){
  
}

noiseReduction <- function(list){ # Only useable after dataStructuring()
  # Take the list created above and reduce the background noise in each picture.
  for(i in 1:length(list)){
    for(j in 1:2){
      
      # Filter 1: Noise Reduction
      # Applying mixture models and Expecation Maximaization Algorithm
      
      placeholder <- normalmixEM(list[[i]][[j]], lambda = .5, mu = c(50, 80), sigma = 5)
      mu1 = placeholder$mu[1] # Where first distribution is centered
      mu2 = placeholder$mu[2] # Where second distribution is centered
      sig1 = placeholder$sigma[1] # Spread of first distribtuion
      sig2 = placeholder$sigma[2] # Spread of the second distribtuion
      
      cutoff = optim(par=(mu1+mu2)/2,sq.loglik.ratio, mu1=mu1,mu2=mu2,sig1=sig1,sig2=sig2)$par # Find the optimal cut off point
      
      list[[i]][[j]] = pmax(list[[i]][[j]], cutoff) # Remove everything before cutoff and save over list input
      # Change placeholder2 back to list[[i]][[j]] to revert back to old version
      print(i)
    }
  }
  return(list) # Return the list that was used as argument but without the noise.
}


createDF <- function(list){
  df <- data.frame()
  for(i in 1:length(list)){
    for(j in 1:length(list[i])){
      vec = as.vector(list[[i]][[j]])
      df <- rbind(df, vec)
    }
    print(i)
  }
  # Change column names for convinience 
  colnames(df) <- 1:ncol(df)
  colnames(df) = paste0("Pixel", 1:ncol(df))
  
  # Add labelled reponse
  df <- cbind(df, dat$is_iceberg)
  
  return(df)
}


createDF2 <- function(list){
  df <- data.frame()
  for(i in 1:length(list)){
    for(j in 1:length(list[i])){
      vec = as.vector(list[[i]][[j]])
      df <- rbind(df, vec)
    }
    print(i)
  }
  # Change column names for convinience 
  colnames(df) <- 1:ncol(df)
  colnames(df) = paste0("Pixel", 1:ncol(df))
  
  return(df)
}


# Subset dataframe

df_subset <- function(list){
  
  startCut_rows = 0.4*75 # 30
  endCut_rows = 0.6:75 # 45
  
  startCut_col = 0.4*75 # 30
  endCut_col = 0.6*75 # 45
  
  
  for(i in 1:length(list)){
    for(j in 1:length(list[i])){
      df = list[[i]][[j]]
      sub_df = df[30:45, 30:45]
      list[[i]][[j]] = sub_df
    }
  }
  return(list)
}

df_subset2 <- function(list){
  
  # Instead of taking the same cuts each time lets actually make sure that the ship/iceberg is in the frame
  
  list_max <- list()
  for(i in 1:length(list)){
    for(j in 1:length(list[i])){
      df = list[[i]][[j]]
      inner_list <- list()
      inner_list[j] <-  which.max(df)
    }
    list_max <- list.append(list_max, inner_list)
  }
  
  for(i in 1:length(list)){
    for(j in 1:length(list[i])){
      image_max <- append(list, as.numeric(list_max[[i]][[j]]))
    }
  }
  
  for(i in 1:length(list)){
    for(j in 1:length(list[i])){
      df = list[[i]][[j]]
      vect <- as.vector(list[[i]][[j]])
      image_max <- as.numeric(list_max[[i]][[j]])
      print(image_max)
      
      startCut_rows = image_max - 8 # 30
      endCut_rows = image_max + 7 # 45
      
      startCut_col = image_max - 8 # 30
      endCut_col = image_max + 7 # 45
      
      sub_df = df[startCut_rows:endCut_rows, startCut_col:endCut_col]
      list[[i]][[j]] = sub_df
    }
  }
  return(list)
}

dataNoise_Cleaning <- function(list){
  for(i in 1:lenghth(list)){
    for(j in 1:length(list[i])){
      df <- list[[i]][[j]]
      df = df[,-257]
      
    }
  }
}

# Convolution Specific Packages

install.packages('devtools')
#installing keras
devtools::install_github("rstudio/keras") 
devtools::install_github("rstudio/reticulate")

#Loading the keras package
library(keras)

setwd("C:/Users/Brad_/Desktop/SFU/Statistics/Statistics 440/Case Studies/Case Study 3")

source("functionFile.R")

trainData = fromJSON("train.json")
x = trainData %>% lapply(
  function(x){
    x2 <- c(x$band_1,x$band_2,apply(cbind(x$band_1,x$band_2),1,mean))
    placeholder <- normalmixEM(x2, lambda = .5, mu = c(50, 80), sigma = 5)
    mu1 = placeholder$mu[1] # Where first distribution is centered
    mu2 = placeholder$mu[2] # Where second distribution is centered
    sig1 = placeholder$sigma[1] # Spread of first distribtuion
    sig2 = placeholder$sigma[2] # Spread of the second distribtuion
    
    cutoff = optim(par=(mu1+mu2)/2,sq.loglik.ratio, mu1=mu1,mu2=mu2,sig1=sig1,sig2=sig2)$par # Find the optimal cut off point
    
    x3 = pmax(x2, cutoff) # Remove everything before cutoff and save
    
    #Apply Smoothing
    # Remove smoothing by commenting out this setion.  Skip to return x3

    # x4 = x3[(5625*2):length(x3)]
    #x5 = matrix(x4, nrow = 75, ncol = 75)
    #x6 = as.im(x5)
    #smoothedImage = blur(x6, sigma = 2)
    #x7 = as.vector(smoothedImage)
    
    
    #ret <- c(x$band_1,x$band_2,x7)
    
    # Smoothing screws everything up.  Skip over.
    
    return(x3)
  }) %>%
  unlist %>%
  array(dim=c(75,75,3,1604)) %>%
  aperm(c(4,1,2,3))

y = kohonen::classvec2classmat(
  unlist(
    lapply(train,
           function(x){
             x$is_iceberg
             })))

inc_angle = unlist(lapply(train,
                          function(x){
                            x$inc_angle
                            }))
inc_angle[inc_angle == "na"] = mean(as.numeric(inc_angle[inc_angle != "na"]))
inc_angle = as.numeric(inc_angle)
dim(inc_angle) = c(length(inc_angle),1)
train = NULL

# Plotting 
plot_image = function(data,i){
  for(j in seq(dim(data)[4])){
    image(data[i,,,j],main=paste0("image ",i,", band ",j,", is_iceberg = ",y[i,2]))
  }
}

set.seed(123)
samp_plot = 8
par(mfrow=c(samp_plot/2,dim(x)[4]*2),xaxt = "n",yaxt="n",mar=c(0.5,0.5,2,0.5),cex=0.3)
for(i in sample(seq(dim(x)[1]),samp_plot)){
  plot_image(x,i)
}

# Splitting into train and validaiton sets 

set.seed(123)
sampleRows = sample(seq_len(nrow(data)),
                    size = floor(0.8*nrow(data)))

train <- list(
  x[sampleRows, , , ],
  inc_angle[sampleRows,,drop = FALSE],
  y[sampleRows,])

val <- list(
  x[-sampleRows, , , ],
  inc_angle[-sampleRows, , drop = FALSE],
  y[-sampleRows,])


# First step of setting up the model
kernel_size = c(5,5)
input_img = layer_input(shape = dim(x)[2:4],name="img") # Layer to be used as an entry point into a graph
input_img_norm = input_img %>%
  layer_batch_normalization(momentum = 0.99) # Normalizes the activation of the previous layer for each batch
  # Momentum is momentum for the moving average of the layers.  0.99 is standard.


# Input Layer
input_CNN = input_img_norm %>%
  layer_conv_2d(32, kernel_size = kernel_size,padding = "same") %>% # Slider over the image.  Takes dot product and generates layer
  layer_batch_normalization(momentum = 0.975) %>% # Normalizes the layer.
  layer_activation_elu() %>% # Exponential Linear Unit Activation.  Takes care of vanishing gradient problem.  Vanishing gradient screws up weights of layers.
  layer_max_pooling_2d(c(2,2)) %>% # Pooling layer and size of it.  2x2 
  layer_dropout(0.25) %>% # Drop out one of 1/4 layers to prevent overfitting.
  
  # Does same thing as above but bigger "slider"
  layer_conv_2d(64, kernel_size = kernel_size,padding = "same") %>%
  layer_batch_normalization(momentum = 0.975) %>%
  layer_activation_elu() %>%
  layer_max_pooling_2d(c(2,2)) %>% # Pooling is the same size
  layer_dropout(0.25) 

# First Residual Layer
# Adding residual layers helps it learn. Similar idea as adding hidden layers in regualar nnet
# For the most part the same as the input layer except we want to use a bigger convolution layer this time.
# Add layers to identify more specific features of the data.  First layer handles "high level" features.
# Each new residual layer goes more in depth.
input_CNN_residual = input_CNN %>%
  layer_batch_normalization(momentum = 0.975) %>%
  layer_conv_2d(128, kernel_size = kernel_size,padding = "same") %>% # Want the slider to be even bigger this time.  
  layer_batch_normalization(momentum = 0.975) %>%
  layer_activation_elu() %>%
  layer_dropout(0.25) %>%
  layer_conv_2d(64, kernel_size = kernel_size,padding = "same") %>%
  layer_batch_normalization(momentum = 0.975) %>%
  layer_activation_elu()

input_CNN_residual = layer_add(list(input_CNN_residual,input_CNN))

# Second Residual Layer
# Exact same thing as above.  Don't know enough to change.
input_CNN_residual = input_CNN_residual %>%
  layer_batch_normalization(momentum = 0.975) %>%
  layer_conv_2d(128, kernel_size = kernel_size,padding = "same") %>%
  layer_batch_normalization(momentum = 0.975) %>%
  layer_activation_elu() %>%
  layer_dropout(0.25) %>%
  layer_conv_2d(64, kernel_size = kernel_size,padding = "same") %>%
  layer_batch_normalization(momentum = 0.975) %>%
  layer_activation_elu()

input_CNN_residual = layer_add(list(input_CNN_residual,input_CNN))

# Third Residual Layer
# Computational time increases each residual layer added.  Adding layers seems to improve model but takes longer.
# Limit residual layers to 3 for now.
input_CNN_residual = input_CNN_residual %>%
  layer_batch_normalization(momentum = 0.975) %>%
  layer_conv_2d(128, kernel_size = kernel_size,padding = "same") %>%
  layer_batch_normalization(momentum = 0.975) %>%
  layer_activation_elu() %>%
  layer_dropout(0.25) %>%
  layer_conv_2d(64, kernel_size = kernel_size,padding = "same") %>%
  layer_batch_normalization(momentum = 0.975) %>%
  layer_activation_elu()

input_CNN_residual = layer_add(list(input_CNN_residual,input_CNN))

# Top Layer
top_CNN = input_CNN_residual %>%
  layer_conv_2d(128, kernel_size = kernel_size,padding = "same") %>%
  layer_batch_normalization(momentum = 0.975) %>%
  layer_activation_elu() %>%
  layer_max_pooling_2d(c(2,2)) %>%
  layer_conv_2d(256, kernel_size = kernel_size,padding = "same") %>%
  layer_batch_normalization(momentum = 0.975) %>%
  layer_activation_elu() %>%
  layer_dropout(0.25) %>%
  layer_max_pooling_2d(c(2,2)) %>%
  layer_conv_2d(512, kernel_size = kernel_size,padding = "same") %>%
  layer_batch_normalization(momentum = 0.975) %>%
  layer_activation_elu() %>%
  layer_dropout(0.25) %>%
  layer_max_pooling_2d(c(2,2)) %>%
  layer_global_max_pooling_2d()

# Output Layer
outputs = top_CNN %>%
  layer_dense(512,activation = NULL) %>%
  layer_batch_normalization(momentum = 0.975) %>%
  layer_activation_elu() %>%
  layer_dropout(0.5) %>%
  layer_dense(256,activation = NULL) %>%
  layer_batch_normalization(momentum = 0.975) %>%
  layer_activation_elu() %>%
  layer_dropout(0.25) %>%
  layer_dense(2,activation = "softmax")


# Put model all together.  Get all layers stacked together.

model <- keras_model(inputs = list(input_img), outputs = list(outputs))

model %>% compile(optimizer=optimizer_adam(lr = 0.001),
                  loss="binary_crossentropy",
                  metrics = c("accuracy"))

summary(model)

# Change These
batch_size = 32 # Trade off used to control overfitting.  Change with steps.  Can't run model long enough to overfit.  
epochs = 40 # Higher the better
steps_per_epoch=2^12/batch_size # Higher the better


# Use so you don't waste your time on bad models.  If nothing is changing it will eventually stop.
# Best strategy seems to be to run model until it stops itself.  If model completes without early stop then start it again from where it left off.
early_stopping <- callback_early_stopping(monitor = "val_loss",
                                          patience = 10)


# Will use to pull out weights later.
check_point = callback_model_checkpoint("weights.hdf5",
                                        monitor = "val_loss",
                                        verbose = 0,
                                        save_best_only = T)


# Able to internally do some manipulate of images.  
# Have no reason to change.  
gen_images <- image_data_generator(
  horizontal_flip = TRUE,
  vertical_flip = TRUE,
  width_shift_range = 0.30,
  height_shift_range = 0.30,
  zoom_range = 0.1,
  rotation_range = 20
)
# Runs model
# If model stops from early stop just run this again to get model to continue from where it left off.
history = model %>% fit_generator(
  flow_images_from_data(x = train[[1]],
                        y = train[[3]],
                        generator = gen_images,
                        batch_size=batch_size,
                        seed = 123),
    steps_per_epoch = steps_per_epoch,
    epochs = epochs,
    view_metrics = T,
    validation_data = list(val[[1]],val[[3]]),
    callbacks = c(check_point,early_stopping)
)

# Evaluation
model_wghts = load_model_hdf5("weights.hdf5") ## load the best model
pred = predict(model_wghts,x=val[[1]])[,2]

confusion_Matrix_Table = table(round(pred),val[[3]][,2])
confusion_Matrix_Table

# Accuracy of Model
sum(diag(confusion_Matrix_Table))/sum(confusion_Matrix_Table)

# Log Loss of model
Metrics::logLoss(val[[3]][,2], pred)

# Save Model
save(model_wghts, file = "CNN7_final.rda")
str(model)

###########################################################################################################
############################################# MODEL STACKING ##############################################
###########################################################################################################

setwd("C:/Users/Brad_/Desktop/SFU/Statistics/Statistics 440/Case Studies/Case Study 3/Submitions/my sub")


# The better CNN predictions.  
sub1 <- read.csv("answer_cnn1 (2459).csv", as.is = TRUE, header = TRUE)
sub2 <- read.csv("answer_cnn5 (2206).csv", as.is = TRUE, header = TRUE)
sub3 <- read.csv("answer_cnn2_0.csv", as.is = TRUE, header = TRUE)
sub4 <- read.csv("avg_subs1 (2097).csv", as.is = TRUE, header = TRUE)
sub5 <- read.csv("new_sub2 (3159).csv", as.is = TRUE, header = TRUE)
sub6 <- read.csv("new_sub3 (2219).csv", as.is = TRUE, header = TRUE)
sub7 <- read.csv("answer_cnn6 (1962).csv", as.is = TRUE, header = TRUE)
sub8 <- read.csv("answer_cnn7 (1918).csv", as.is = TRUE, header = TRUE)


newSub <- data.frame(cbind(sub1[,2], sub2[,2], sub3[,2],sub4[,2], sub6[,2], sub7[,2], sub8[,2]))

test <- map_dbl(newSub, mean)
is_iceberg <- rowMeans(newSub)
is_iceberg

new_sub2 <- newSub[,-10]
head(is_iceberg)
subm_mean <- data.frame(cbind(id = sub1$id, is_iceberg))
head(subm_mean)
str(subm_mean[,2])

write.csv(subm_mean, "avg_subs1.csv", row.names = F)

subm_mean2 = subm_mean
head(subm_mean$is_iceberg)
subm_mean2_col = as.numeric(as.character(subm_mean2$is_iceberg))



subm_mean2_col <- ifelse(subm_mean2_col[i] > 0.95, subm_mean2_col[i] + 0.01, subm_mean2_col[i] + 0)
subm_mean2_col

# Bad idea
for(i in 1:length(subm_mean2_col)){
  subm_mean2_col[i] <- ifelse(subm_mean2_col[i] < 0.05, subm_mean2_col[i] - 0.01, subm_mean2_col[i] + 0)
}


for(i in 1:length(subm_mean2_col)){
  subm_mean2_col[i] <- ifelse(subm_mean2_col[i] > 1, subm_mean2_col[i] - 0.01, subm_mean2_col[i] + 0)
}

for(i in 1:length(subm_mean2_col)){
  subm_mean2_col[i] <- ifelse(subm_mean2_col[i] < 0, subm_mean2_col[i] + 0.01, subm_mean2_col[i] + 0)
}

View(subm_mean2_col)

subm_mean2 <- data.frame(cbind(id = sub1$id, subm_mean2_col))
View(subm_mean2)

difference = subm_mean2_col - as.numeric(as.character(subm_mean2$is_iceberg))
difference

write.csv(subm_mean, "avg_subs1_6.csv", row.names = F)


# Stacking Method

# Create dataframe of submissions

sub_df <- data.frame(cbind(id = sub1$id, newSub))
View(sub_df)
head(sub_df)

cor(sub_df[,-1])
sub_df_max = apply(sub_df[,-1], 1, max)
sub_df_min = apply(sub_df[,-1], 1, min)
sub_df_mean = apply(sub_df[,-1], 1, mean)
sub_df_median = apply(sub_df[,-1], 1, median)

lower_cutoff = 0.70
upper_cutoff = 0.30

sub_stack_max <- data.frame(cbind(id = sub1$id, sub_df_max))
sub_stack_min <- data.frame(cbind(id = sub1$id, sub_df_min))
sub_stack_mean <- data.frame(cbind(id = sub1$id, sub_df_mean))
sub_stack_median <- data.frame(cbind(id = sub1$id, sub_df_median))

# Building MinMax Stack off of sub8


base_preds = sub8[,2] # The base of the stack
base_preds = as.numeric(as.character(base_preds))
base_preds

stack_max = sub_df_max
stack_min = sub_df_min
stack_mean = sub_df_mean
stack_median = sub_df_median


sub_df_full <- data.frame(cbind(id = sub1[,1],
                                CNN_Pred1 = sub1[,2], 
                                CNN_Pred2 = sub2[,2], 
                                CNN_Pred3 = sub3[,2],
                                CNN_Pred4 = sub4[,2], 
                                CNN_Pred6 = sub6[,2], 
                                CNN_Pred7 = sub7[,2], 
                                CNN_Pred8 = sub8[,2],
                                stack_max,
                                stack_min,
                                stack_mean,
                                stack_median))
View(sub_df_full)

for(i in 1:length(base_preds)){
  if(base_preds[i] > lower_cutoff){ # For predictions we are highly confident in.
    base_preds[i] = stack_max[i] # Boost them up to the max of the stack
  }else{
    if(base_preds[i] < upper_cutoff){ 
      base_preds[i] = stack_min[i] # Likewise when we are highly confident that they are ships
    }else{
      base_preds[i] = base_preds[i] # For predictions were aren't confident in, use the base.
    }
  }
}

stack_baseMinMax = base_preds
stack_baseMinMax
sub_stack_baseMinMax <- data.frame(cbind(id = sub1$id, is_iceberg = stack_baseMinMax))
View(sub_stack_baseMinMax)
write.csv(sub_stack_baseMinMax, "sub_stack_baseMinMax4.csv", row.names = F)



for(i in 1:length(base_preds)){
  if(base_preds[i] > lower_cutoff){ # For predictions we are highly confident in.
    base_preds[i] = stack_max[i] # Boost them up to the max of the stack
  }else{
    if(base_preds[i] < upper_cutoff){ 
      base_preds[i] = stack_min[i] # Likewise when we are highly confident that they are ships
    }else{
      base_preds[i] = stack_median[i] # For predictions were aren't confident in, use the base.
    }
  }
}

stack_medianMinMax = base_preds
sub_stack_medianMinMax <- data.frame(cbind(id = sub1$id, is_iceberg = stack_medianMinMax))
View(sub_stack_medianMinMax)
write.csv(sub_stack_baseMinMax, "sub_stack_medianMinMax.csv", row.names = F)

# Barinder's Prediction Code

setwd("C:/Users/Brad_/Desktop/SFU/Statistics/Statistics 440/Case Studies/Case Study 3/Submitions")

sub1 <- read.csv("iceberg_kaggle_submit_03.csv", as.is = TRUE, header = TRUE)
sub2 <- read.csv("new_sub.csv", as.is = TRUE, header = TRUE)
sub3 <- read.csv("new_sub2 (1).csv", as.is = TRUE, header = TRUE)
sub4 <- read.csv("new_sub3 (1).csv", as.is = TRUE, header = TRUE)
sub5 <- read.csv("sub_fcn.csv", as.is = TRUE, header = TRUE)
sub6 <- read.csv("subm_fix.csv", as.is = TRUE, header = TRUE)
sub7 <- read.csv("submission.csv", as.is = TRUE, header = TRUE)
sub8 <- read.csv("submission3-1.csv", as.is = TRUE, header = TRUE)
sub9 <- read.csv("submission3.csv", as.is = TRUE, header = TRUE)


# 

newSub <- data.frame(cbind(sub1[,2], sub2[,2], sub3[,2], sub4[,2], sub5[,2], sub6[,2], sub7[,2], sub8[,2], sub9[,2],
                           new2))

test <- map_dbl(newSub, mean)
is_iceberg <- rowMeans(new_sub2)

new_sub2 <- newSub[,-10]
head(is_iceberg)
subm_mean <- data.frame(cbind(id = sub1$id, is_iceberg))
head(subm_mean)

write.csv(subm_mean, "subm_mean2.csv", row.names = F)

head(newSub)


for (i in 1:9){
  for (j in 1:8424){
    newSub[j,i] <- ifelse((newSub[j,i] > 0.5), 1, 0)
  }
}

dim(newSub)


newSub[1, 1] <- ifelse(newSub[1, 1] > 0.5, 1, 0)


head(newSub)

new2 <- rowSums(newSub)

head(new2)

preds <- NU

for (i in 1:9){
  for (j in 1:8424){
    if (newSub[j, i] > 0.5){
      newSub[j, i] <- ifelse(newSub[j, 10] > 4, newSub[j, i], 0)
    } else {
      newSub[j, i] <- ifelse(newSub[j, 10] <= 4, newSub[j, i], 0)
    }
  }
}


head(newSub)

new2[]
superFUN <- function(data){
  for(i in 1:nrow(newSub)){
    
  }
}

###########################################################################################################
###########################################################################################################
############################################## Fox n'friends < CNN    #####################################
############################################## Other models we tried  #####################################
###########################################################################################################
###########################################################################################################

# Data and Directory
setwd("C:/Users/Brad_/Desktop/SFU/Statistics/Statistics 440/Case Studies/Case Study 3")
json = readLines("train.json")  ## Be patient, 45MB
dat = jsonlite::fromJSON(json)  # Need to reload the jsonlite package.  The other json packages read it in a different format.
View(dat)
###############################################################################################
json2 = readLines("test.json") ## Have the patience of a saint...1.4GB...ughhhh
submition_test = fromJSON(json2)

dat_List2 <- dataStructuring(submition_test)
dat_Reduced2 <- noiseReduction(dat_List2)
dat_sub2 <- df_subset(dat_Reduced2)
dat_df2 <- createDF2(dat_sub2)

write.csv(dat_df2, "never_again.csv")
View(dat_df2)

###############################################################################################

# Source File
# Contains Functions
source("functionFile.R")

# Clean Data
dat_List <- dataStructuring(dat)
dat_Reduced <- noiseReduction(dat_List)
dat_sub <- df_subset(dat_Reduced)
dat_df <- createDF(dat_Reduced)
data = dat_df


image(dat_Reduced[[1]][[1]])
image(dat_Reduced[[11]][[1]])


# write.csv(submition_test, "never_again.csv")

# write.csv(data, "cleaned_data2.csv")

# Read Data
data = read.csv("cleaned_data2.csv", header = TRUE)
View()
dim(data)

##################################################################
# Adding Angles
# Shouldn't need to do again.  Added into CSV
inc_angle <- as.numeric(dat$inc_angle)
hist(inc_angle)

inc_angle <- ifelse(is.na(inc_angle), rnorm(1, 39, 3), inc_angle)

data <- cbind(data, inc_angle)
##################################################################



# Split into Train and Validation datasets
set.seed(123)
sampleRows = sample(seq_len(nrow(data)), size = floor(0.75*nrow(data)))
train <- data[sampleRows, ]
test <- data[-sampleRows, ]

# write.csv(data, "cleaned_data2.csv")


dim(train)
train[,257]
############### Models ###################

# Model 1: LASSO Regression

lambdas <- 10^{seq(from=-2,to=2,length=100)}
matX <- as.matrix(train[,-257])
Y = as.factor(train[,257])
cv.lafit <- cv.glmnet(matX, Y, alpha=1, lambda=lambdas, family = "binomial")

plot(cv.lafit) 
lasso.best.lam <- cv.lafit$lambda.min
lasso.best.lam

fit1 <- glmnet(matX,Y,alpha=1,lambda=lasso.best.lam, family = "binomial")
fit1
summary(fit1)
coefs = coef(fit1)
coefs


matX_test = as.matrix(test[,-257])
pred1 <- predict.glmnet(fit1, newx = matX_test)
pred1.5 = ifelse(pred1 >= 0.5, 1, 0)
pred1.5

AccVect = (pred1.5 == test[,257])
sum(AccVect)/length(AccVect)
# 64% Accuracy

#Model 2: Random Forest

# Train Model
numMtry = c(14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)

rf_testResults = list()
for(i in 1:length(numMtry)){
  rf.1 <- randomForest(as.factor(data[,257]) ~.,
                       data = data[,-257],
                       ntree = 600,
                       mtry = numMtry[i],
                       importance = TRUE,
                       proximity = TRUE)
  
  rf_testResults[i] = mean(rf.1$err.rate[,1])
  print(i)
}

rf_testResults
# See that ntree ~ 600 minimizes out of bag error.

tuneRF(as.factor(train[,-257]), train[,257], ntreeTry = 600)

rf.1 <- randomForest(as.factor(train[,257]) ~.,
                     data = train[,-257],
                     ntree = 500,
                     mtry = 15,
                     do.trace = TRUE,
                     importance = TRUE,
                     proximity = TRUE)

rf.1
summary(rf.1)
plot(rf.1,
     main = "Random Forest Model")
varImpPlot(rf.1)

importance(rf.1)


pred2 <- predict(rf.1, newdata = test[,-257])
pred2

AccVect = (pred2 == test[,257])
sum(AccVect)/length(AccVect)
# 75% Accuracy

pred2_prob <- predict(rf.1, newdata = test[,-257], type = "prob")
pred2_prob

# Running on Test Set

pred.rf_test <- predict(rf.1, newdata = dat_df2, type = "prob")

dim(pred.rf_test)

answer.rf <- data.frame(submition_test$id ,pred.rf_test[,2])
colnames(answer.rf)<- c("id","is_iceberg")
write.csv(answer.rf, "answer_rf.csv")

# Model 3: Gradient Boost

#Beginning specification.  Important!
fitControl <- trainControl(method = "repeatedcv", number = 20, repeats = 10)
fitControl

# Bunch of magic numbers.  Play around with them.
gbmGrid <-  expand.grid(interaction.depth = 1, 
                        n.trees = 100, 
                        shrinkage = 0.1,
                        n.minobsinnode = 10)



# Train the model
xnam <- paste("train[,",1:256, sep="")
xnam2 <- paste(xnam,"]", sep = "")
fmla <- as.formula(paste("train[,257] ~ ", paste(xnam2, collapse= "+")))
fmla


gbmFit1 <- train(matX, Y, data = train, method = "gbm", trControl = fitControl,verbose = FALSE)
gbmFit1


# Model 4: Neural Network

xnam <- paste("train[,",1:256, sep="")
xnam2 <- paste(xnam,"]", sep = "")
fmla <- as.formula(paste("train[,257] ~ ", paste(xnam2, collapse= "+")))

net_iceberg <- neuralnet(fmla,
                         train,
                         hidden=0, 
                         rep=10,
                         err.fct="ce",
                         linear.output=FALSE,
                         stepmax = 1e7)

net_iceberg
summary(net_iceberg)

save(net_iceberg, file = "neural1.rda")
load( "neural1.rda")


### Compute the results on the test data
pred_nn <- compute(net_iceberg,test[,-257]) 

pred_nn_train <- compute(net_iceberg, train[,-257])

pred_nn2 <- round(pred_nn$net.result)
table(pred_nn2, test[,257])

nn_misclass <- (87 + 75)/(87 + 75 + 134 + 105)
nn_misclass

# Logloss

nn_logloss <- logLoss(test[,257], pred_nn$net.result)
nn_logloss


pred_nn_train <- compute(net_iceberg, train[,-257])
pred_nn2_train <- round(pred_nn_train$net.result)
table(pred_nn2_train, train[,257])

(135+139)/(503+135+139+426)

# Model 4.5: Neural Network (With 2 Hidden Layers)

# Setting Up Formula
xnam <- paste("train[,",1:256, sep="")
xnam2 <- paste(xnam,"]", sep = "")
fmla <- paste("train[,257] ~ ", paste(xnam2, collapse= "+"))
fmla <- as.formula(paste0(fmla, " + train[,258]"))
fmla

nnetGrid <- expand.grid(.size = 7, .decay = c(0, .01, .1, .2, .3, .4, .5, 1, 2))
ctrl <- trainControl(method = "repeatedcv", repeats = 5, classProbs = TRUE)



net2_iceberg <- neuralnet(fmla,
                          train,
                          hidden=2, 
                          rep=10,
                          err.fct="ce",
                          linear.output=FALSE,
                          stepmax = 1e7)


save(net2_iceberg, file = "neural2.rda")
net2_iceberg
pred2_nn2 <- compute(net2_iceberg,test[,-257]) 

pred2_nn2_train <- compute(net2_iceberg, train[,-257])

pred2_nn2 <- round(pred2_nn2$net.result)
table(pred2_nn2, test[,257])

(18 + 139)/(70 + 18 + 139 + 174)

nn2_logloss <- logLoss(test[,257], pred2_nn2$net.result)
nn2_logloss

# Model 5: Extreme Gradient Boost

matX <- as.matrix(train[,-257])
Y = as.factor(train[,257])
dtrain <- xgb.DMatrix(data = matX, label = as.numeric(as.character(Y)))
dtest <- xgb.DMatrix(data = as.matrix(test[,-257]), label = as.numeric(as.character(test[,257])))

watchlist <- list(train = dtrain, test = dtest) # Used for cross validation in param

# Basic xgboost
xgb1 <- xgboost(data = dtrain,
                objective = "binary:logistic", 
                eta = 0.3, # Step size shrinkage
                max.depth = 3, # maximum depth of tree
                nthread = 2, # Number of Cores
                gamma = 1, # Regular Reugluarization
                lambda = 0, # L2 Regularization (Ridge) } Choose between alpha and lambda
                alpha = 1, # L1 Regularization (LASSO)  }
                colsample_tree = 0.2,
                nround = 1000)


# Cross Validation
# Set Xgboost parameters
param <- list("objective" = "binary:logistic", 
              "eval_metric" = "logloss", # Error function.
              "eta" = 0.15, # Step size shrinkage
              "max.depth" = 3, # maximum depth of tree
              "watchlist" = "watchlist", # The train and test sets for cross validation.  Set above
              "nthread" = 2, # Number of Cores
              "gamma" = 1, # Regular Reugluarization
              "lambda" = 0, # L2 Regularization (Ridge) } Choose between alpha and lambda
              "alpha" = 1, # L1 Regularization (LASSO)  }
              "colsample_tree" = 0.2,
              "nround" = 1000) # Number of threads to be used

xgb.cv = xgb.cv(param=param, data = dtrain, nfold = 10, nrounds = 150)
summary(xgb.cv)
plot(xgb.cv$evaluation_log)

xgb.pred1 <- predict(xgb1, newdata = dtest, type = "prob")
xgb.pred1

# Model 5.5: XGBoost on all pixels

# Split into Train and Validation datasets
set.seed(123)
sampleRows = sample(seq_len(nrow(data)), size = floor(0.75*nrow(data)))
train_full <- data[sampleRows, ]
test_full <- data[-sampleRows, ]

matX <- as.matrix(train_full[,-5626])
Y = as.factor(train_full[,5626])
dtrain_full <- xgb.DMatrix(data = matX, label = as.numeric(as.character(Y)))
dtest_full <- xgb.DMatrix(data = as.matrix(test_full[,-2626]), label = as.numeric(as.character(test_full[,5626])))

watchlist <- list(train = dtrain_full, test = dtest_full) # Used for cross validation in param

# Basic xgboost
xgb2 <- xgboost(data = dtrain,
                objective = "binary:logistic", 
                eta = 0.15, # Step size shrinkage
                max.depth = 3, # maximum depth of tree
                nthread = 2, # Number of Cores
                gamma = 2, # Regular Reugluarization
                lambda = 0, # L2 Regularization (Ridge) } Choose between alpha and lambda
                alpha = 1, # L1 Regularization (LASSO)  }
                colsample_tree = 0.2,
                nround = 1000)


# Cross Validation
# Set Xgboost parameters
param <- list("objective" = "binary:logistic", 
              "eval_metric" = "logloss", # Error function.
              "eta" = 0.25, # Step size shrinkage
              "max.depth" = 4, # maximum depth of tree
              "watchlist" = "watchlist", # The train and test sets for cross validation.  Set above
              "nthread" = 2, # Number of Cores
              "gamma" = 2, # Regular Reugluarization
              "lambda" = 0, # L2 Regularization (Ridge) } Choose between alpha and lambda
              "alpha" = 1, # L1 Regularization (LASSO)  }
              "colsample_tree" = 0.2,
              "nround" = 1000) # Number of threads to be used

xgb_full.cv = xgb.cv(param=param, data = dtrain_full, nfold = 10, nrounds = 150)
summary(xgb.cv)
plot(xgb.cv$evaluation_log)

xgb.pred1 <- predict(xgb1, newdata = dtest, type = "prob")
xgb.pred1

# Histogram Plot
ggplot(data = x, aes(x = x, fill = ..x..)) +
  geom_histogram(binwidth = 0.5, bins = 60) + 
  scale_fill_gradient(low = "red", high = "yellow", "Legend") + 
  xlab("Pixel Value") +
  ylab("Count") +
  ggtitle("Histogram of Pixel Values for an Image") + 
  theme(plot.title = element_text(hjust = 0.5))


