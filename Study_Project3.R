
#R project for Study project
#
#Nahid Hasan Ronnie
#matriculation 3944438
#



#checking working directory
getwd()
setwd("E:/Thesis/Program file on RStudio")
#Library

library(dplyr)
library(tidyverse)
library(countrycode)
library(purrr)

library(stars)
library(rnaturalearth)
library(patchwork)

library(caret)
library(pdp)
library(vip)
library(rpart)
library(rpart.plot)


#importing data
cropdata <- 
  read.csv("Data.csv", header = TRUE, sep = ",",stringsAsFactors=T)%>% na.omit
#variable types

glimpse(cropdata)
cropdata$NT_effect <- cropdata$NT_effect %>% fct_rev()

cropdata1 <- cropdata[-which(cropdata$Yield_change > quantile(cropdata$Yield_change, 0.975)),]
cropdata1$continent = countrycode(sourcevar = cropdata1 [, "Site.country"],
                                 origin ="country.name",
                                 destination ="continent")

cropdata_cleaned <-
  cropdata1%>% dplyr::select(-Yield_NT,-NT_effect, -Site.country, -Location, -continent)

#####For all the crops


'create_subsets <- function(your_croptype){
  
  GlobalCotton=filter(cropdata_cleaned, Crop==your_croptype) %>% dplyr::select(-Crop)
  
}
cot <- create_subsets("cotton")'

GlobalCotton=filter(cropdata_cleaned, Crop=="cotton") %>% dplyr::select(-Crop)
Globalrice=filter(cropdata_cleaned, Crop=="rice")%>% dplyr::select(-Crop)
GlobalSoybean=filter(cropdata_cleaned, Crop=="soybean")%>% dplyr::select(-Crop)
Globalsorghum=filter(cropdata_cleaned, Crop=="sorghum")%>% dplyr::select(-Crop)
GlobalSunflower=filter(cropdata_cleaned, Crop=="sunflower")%>% dplyr::select(-Crop)
Globalwheat.winter=filter(cropdata_cleaned, Crop=="wheat.winter")%>% dplyr::select(-Crop)
Globalwheat.spring=filter(cropdata_cleaned, Crop=="wheat.spring")%>% dplyr::select(-Crop)
Globalbarley.winter=filter(cropdata_cleaned, Crop=="barley.winter")%>% dplyr::select(-Crop)
Globalbarley.spring=filter(cropdata_cleaned, Crop=="barley.spring")%>% dplyr::select(-Crop)
Globalmaize=filter(cropdata_cleaned, Crop=="maize")%>% dplyr::select(-Crop)

#Map with number of factors

world <- ne_countries(scale="small", returnclass = "sf")
map_global_star <- world$Cumulative %>% st_as_stars()

##############for Cotton
world_map_cot <-
  ggplot(data = world) +
  geom_sf() +
  geom_point(data=GlobalCotton, aes(x=Longitude, y=Latitude), size=1, color="darkolivegreen4")+
  ggtitle("World map distribution of Cotton")+
  theme_bw(base_size = 14)+
  theme(plot.title = element_text(size = 12, hjust = 0.5))

##############for rice
world_map_Rice <-
  ggplot(data = world) +
  geom_sf() +
  geom_point(data=Globalrice, aes(x=Longitude, y=Latitude), size=1, color="blue")+
  ggtitle("World map distribution of Rice")+
  theme_bw(base_size = 14)+
  theme(plot.title = element_text(size = 12, hjust = 0.5)) 

##############for soybean
world_map_Soybean <-
  ggplot(data = world) +
  geom_sf() +
  geom_point(data=GlobalSoybean, aes(x=Longitude, y=Latitude), size=1, color="orange")+
  ggtitle("World map distribution of Soybean")+
  theme_bw(base_size = 14)+
  theme(plot.title = element_text(size = 12, hjust = 0.5)) 

##############for maize
world_map_maize <-
  ggplot(data = world) +
  geom_sf() +
  geom_point(data=Globalmaize, aes(x=Longitude, y=Latitude), size=1, color="yellow")+
  ggtitle("World map distribution of Maize")+
  theme_bw(base_size = 14)+
  theme(plot.title = element_text(size = 12, hjust = 0.5))

##############for maize
world_map_sunflower <-
  ggplot(data = world) +
  geom_sf() +
  geom_point(data=GlobalSunflower, aes(x=Longitude, y=Latitude), size=1, color="brown")+
  ggtitle("World map distribution of sunflower")+
  theme_bw(base_size = 14)+
  theme(plot.title = element_text(size = 12, hjust = 0.5))

##############for sorghum
world_map_sorghum <-
  ggplot(data = world) +
  geom_sf() +
  geom_point(data=Globalsorghum, aes(x=Longitude, y=Latitude), size=1, color="green")+
  ggtitle("World map distribution of sorghum")+
  theme_bw(base_size = 14)+
  theme(plot.title = element_text(size = 12, hjust = 0.5))

##############for wheat.spring
world_map_wheat.sp <-
  ggplot(data = world) +
  geom_sf() +
  geom_point(data=Globalwheat.spring, aes(x=Longitude, y=Latitude), size=1, color="purple")+
  ggtitle("World map distribution of Wheat in Spring")+
  theme_bw(base_size = 14)+
  theme(plot.title = element_text(size = 12, hjust = 0.5)) 

##############for wheat.winter
world_map_wheat.win <-
  ggplot(data = world) +
  geom_sf() +
  geom_point(data=Globalwheat.winter, aes(x=Longitude, y=Latitude), size=1, color="pink2")+
  ggtitle("World map distribution of Wheat in Winter")+
  theme_bw(base_size = 14)+
  theme(plot.title = element_text(size = 12, hjust = 0.5)) 

##############for barley.spring
world_map_bar.sp <-
  ggplot(data = world) +
  geom_sf() +
  geom_point(data=Globalbarley.spring, aes(x=Longitude, y=Latitude), size=1, color="cyan2")+
  ggtitle("World map distribution of Barley in Spring")+
  theme_bw(base_size = 14)+
  theme(plot.title = element_text(size = 12, hjust = 0.5)) 

##############for barley.winter
world_map_bar.win <-
  ggplot(data = world) +
  geom_sf() +
  geom_point(data=Globalbarley.winter, aes(x=Longitude, y=Latitude), size=1, color="red")+
  ggtitle("World map distribution of Barley in Winter")+
  theme_bw(base_size = 14)+
  theme(plot.title = element_text(size = 12, hjust = 0.5))

Fig01 <- (world_map_cot / world_map_Rice / world_map_Soybean / world_map_maize/ world_map_sorghum /world_map_sunflower 
          / world_map_wheat.sp / world_map_wheat.win / world_map_bar.sp / world_map_bar.win) +
  
  plot_layout(ncol=2,nrow = 5, widths = c(2,2), heights = c(2,2)) + 
  plot_annotation(tag_levels = 'a')


Fig01


#Histrogram analysis

world_hist_cot <-
  ggplot(data=GlobalCotton, aes(y=Yield_change)) +
  geom_histogram(bins=30)+
  coord_flip() + 
  ylab("Relative change in crop yield") +
  ggtitle("Histrogram analysis for Cotton")+
  theme_bw(
    base_size = 14
  )

world_hist_Rice <-
  ggplot(data=Globalrice, aes(y=Yield_change)) +
  geom_histogram(bins=30) +
  coord_flip() + 
  ylab("Relative change in crop yield") +
  ggtitle("Histrogram analysis for Rice") +
  theme_bw(
    base_size = 14
  )

world_hist_Soybean <-
  ggplot(data=GlobalSoybean, aes(y=Yield_change)) +
  geom_histogram(bins=30) +
  coord_flip() + 
  ylab("Relative change in crop yield") +
  ggtitle("Histrogram analysis for Soybean") +
  theme_bw(
    base_size = 14
  )

world_hist_maize <-
  ggplot(data=Globalmaize, aes(y=Yield_change)) +
  geom_histogram(bins=30) +
  coord_flip() + 
  ylab("Relative change in crop yield") +
  ggtitle("Histrogram analysis for Maize") +
  theme_bw(
    base_size = 14
  )
world_hist_sorghum <-
  ggplot(data=Globalsorghum, aes(y=Yield_change)) +
  geom_histogram(bins=30) +
  coord_flip() + 
  ylab("Relative change in crop yield") +
  ggtitle("Histrogram analysis for sorghum") +
  theme_bw(
    base_size = 14
  )

world_hist_sunflower <-
  ggplot(data=GlobalSunflower, aes(y=Yield_change)) +
  geom_histogram(bins=30) +
  coord_flip() + 
  ylab("Relative change in crop yield") +
  ggtitle("Histrogram analysis for Sunflower") +
  theme_bw(
    base_size = 14
  )

world_hist_wheat.sp <-
  ggplot(data=Globalwheat.spring, aes(y=Yield_change)) +
  geom_histogram(bins=30) +
  coord_flip() + 
  ylab("Relative change in crop yield") +
  ggtitle("Histrogram analysis for Wheat in spring") +
  theme_bw(
    base_size = 14
  )

world_hist_wheat.win <-
  ggplot(data=Globalwheat.winter, aes(y=Yield_change)) +
  geom_histogram(bins=30) +
  coord_flip() + 
  ylab("Relative change in crop yield") +
  ggtitle("Histrogram analysis for Wheat in winter") +
  theme_bw(
    base_size = 14
  )

world_hist_bar.sp <-
  ggplot(data=Globalbarley.spring, aes(y=Yield_change)) +
  geom_histogram(bins=30) +
  coord_flip() + 
  ylab("Relative change in crop yield") +
  ggtitle("Histrogram analysis for Barley in spring") +
  theme_bw(
    base_size = 14
  )

world_hist_bar.win <-
  ggplot(data=Globalbarley.winter, aes(y=Yield_change)) +
  geom_histogram(bins=30) +
  coord_flip() + 
  ylab("Relative change in crop yield") +
  ggtitle("Histrogram analysis for Barley in winter") +
  theme_bw(
    base_size = 14
  )

Fig02 <- (world_hist_cot / world_hist_Rice / world_hist_Soybean / world_hist_maize / world_hist_sorghum / world_hist_sunflower 
          / world_hist_wheat.sp / world_hist_wheat.win / world_hist_bar.sp / world_hist_bar.win) +
  
  plot_layout(ncol=2,nrow = 5, widths = c(1,1), heights = c(1,1)) + 
  plot_annotation(tag_levels = 'i')


Fig02



#cross validation
#5-fold
#random
#train : 80%, test : 20%

set.seed(111) #to get same number every time


train_id_cotton <- sample(c(1:nrow(GlobalCotton)),
                   size = nrow(GlobalCotton)*0.8,
                   replace=F)
####view(train_id_cotton)

test_id_cotton <- setdiff(c(1:nrow(GlobalCotton)), train_id_cotton)
#View(test_id_cotton)

train_cotton <- GlobalCotton %>% slice(train_id_cotton)
test_cotton <- GlobalCotton %>% slice(test_id_cotton)
#View(train_cotton)
#View(test_cotton)

#
set.seed(111)
train_id_soybean <- sample(c(1:nrow(GlobalSoybean)),
                          size = nrow(GlobalSoybean)*0.8,
                          replace=F)
#View(train_id_soybean)

test_id_soybean <- setdiff(c(1:nrow(GlobalSoybean)), train_id_soybean)
#View(test_id_soybean)

train_soybean <- GlobalSoybean %>% slice(train_id_soybean)
test_soybean <- GlobalSoybean %>% slice(test_id_soybean)
#View(train_soybean)
#View(test_soybean)

#
set.seed(111)
train_id_rice <- sample(c(1:nrow(Globalrice)),
                        size = nrow(Globalrice)*0.8,
                        replace=F)
#View(train_id_rice)

test_id_rice <- setdiff(c(1:nrow(Globalrice)), train_id_rice)
#View(test_id_rice)

train_rice <- Globalrice %>% slice(train_id_rice)
test_rice <- Globalrice %>% slice(test_id_rice)
#View(train_rice)
#View(test_rice)

#
set.seed(111)
train_id_maize <- sample(c(1:nrow(Globalmaize)),
                        size = nrow(Globalmaize)*0.8,
                        replace=F)
#View(train_id_maize)

test_id_maize <- setdiff(c(1:nrow(Globalmaize)), train_id_maize)
#View(test_id_maize)

train_maize <- Globalmaize %>% slice(train_id_maize)
test_maize <- Globalmaize %>% slice(test_id_maize)
#View(train_maize)
#View(test_maize)

#
set.seed(111)
train_id_sorghum <- sample(c(1:nrow(Globalsorghum)),
                         size = nrow(Globalsorghum)*0.8,
                         replace=F)
#View(train_id_sorghum)

test_id_sorghum <- setdiff(c(1:nrow(Globalsorghum)), train_id_sorghum)
#View(test_id_sorghum)

train_sorghum <- Globalsorghum %>% slice(train_id_sorghum)
test_sorghum <- Globalsorghum %>% slice(test_id_sorghum)
#View(train_sorghum)
#View(test_sorghum)

#
set.seed(111)
train_id_sunflower <- sample(c(1:nrow(GlobalSunflower)),
                           size = nrow(GlobalSunflower)*0.8,
                           replace=F)
#View(train_id_sunflower)

test_id_sunflower <- setdiff(c(1:nrow(GlobalSunflower)), train_id_sunflower)
#View(test_id_sunflower)

train_sunflower <- GlobalSunflower %>% slice(train_id_sunflower)
test_sunflower <- GlobalSunflower %>% slice(test_id_sunflower)
#View(train_sunflower)
#View(test_sunflower)

#
set.seed(111)
train_id_barley.spring <- sample(c(1:nrow(Globalbarley.spring)),
                          size = nrow(Globalbarley.spring)*0.8,
                          replace=F)
#View(train_id_barley.spring)

test_id_barley.spring <- setdiff(c(1:nrow(Globalbarley.spring)), train_id_barley.spring)
#View(test_id_barley.spring)

train_barley.spring <- Globalbarley.spring %>% slice(train_id_barley.spring)
test_barley.spring <- Globalbarley.spring %>% slice(test_id_barley.spring)
#View(train_barley.spring)
#View(test_barley.spring)

#
set.seed(111)
train_id_barley.winter <- sample(c(1:nrow(Globalbarley.winter)),
                                 size = nrow(Globalbarley.winter)*0.8,
                                 replace=F)
##View(train_id_barley.winter)

test_id_barley.winter <- setdiff(c(1:nrow(Globalbarley.winter)), train_id_barley.winter)
#View(test_id_barley.winter)

train_barley.winter <- Globalbarley.winter %>% slice(train_id_barley.winter)
test_barley.winter <- Globalbarley.winter %>% slice(test_id_barley.winter)
#View(train_barley.winter)
#View(test_barley.winter)

#
set.seed(111)
train_id_wheat.spring <- sample(c(1:nrow(Globalwheat.spring)),
                                 size = nrow(Globalwheat.spring)*0.8,
                                 replace=F)
#View(train_id_wheat.spring)

test_id_wheat.spring <- setdiff(c(1:nrow(Globalwheat.spring)), train_id_wheat.spring)
#View(test_id_wheat.spring)

train_wheat.spring <- Globalwheat.spring %>% slice(train_id_wheat.spring)
test_wheat.spring <- Globalwheat.spring %>% slice(test_id_wheat.spring)
#View(train_wheat.spring)
#View(test_wheat.spring)

#
set.seed(111)
train_id_wheat.winter <- sample(c(1:nrow(Globalwheat.winter)),
                                size = nrow(Globalwheat.winter)*0.8,
                                replace=F)
#View(train_id_wheat.winter)

test_id_wheat.winter <- setdiff(c(1:nrow(Globalwheat.winter)), train_id_wheat.winter)
#View(test_id_wheat.winter)

train_wheat.winter <- Globalwheat.winter %>% slice(train_id_wheat.winter)
test_wheat.winter <- Globalwheat.winter %>% slice(test_id_wheat.winter)
#View(train_wheat.winter)
#View(test_wheat.winter)

#

# Machine learning algorithm implementation -------------------------------


tc = trainControl(method = "cv", number = 5) # caret

#
set.seed(111)
model.lm.cotton   = caret::train(Yield_change ~., data=train_cotton, method="glmStepAIC", trControl=tc)

library(partykit)

set.seed(111)

default_minsplit <- rpart.control()$minsplit
print(default_minsplit)


'ctree_control <- ctree_control(minsplit = 5)
print(ctree_control)
model.cart.cotton1 = caret::train(Yield_change ~., data=train_cotton, method="ctree", trControl=tc, control = ctree_control)
plot(model.cart.cotton1$finalModel)'


set.seed(111)
cart_control <- rpart.control(minsplit = 5, cp = 0.0001)
model.cart.cotton = caret::train(Yield_change ~., data=train_cotton, method="rpart", trControl=tc, control = cart_control)
rpart.plot(model.cart.cotton$finalModel)


set.seed(111)
model.rf.cotton   = caret::train(Yield_change ~., data=train_cotton, method="rf", trControl=tc)

set.seed(111)
model.gbm.cotton  = caret::train(Yield_change ~., data=train_cotton, method="gbm", trControl=tc,
                               tuneGrid=expand.grid(n.trees=(1:5)*10, interaction.depth=(1:5)*1,
                                               shrinkage=0.5, n.minobsinnode=10))

#

set.seed(111)
model.lm.rice   = caret::train(Yield_change ~., data=train_rice, method="glmStepAIC", trControl=tc)

set.seed(111)
model.cart.rice = caret::train(Yield_change ~., data=train_rice, method="rpart", trControl=tc, control = cart_control)
rpart.plot(model.cart.rice$finalModel)

set.seed(111)
model.rf.rice   = caret::train(Yield_change ~., data=train_rice, method="rf", trControl=tc)

set.seed(111)
model.gbm.rice  = caret::train(Yield_change ~., data=train_rice, method="gbm", trControl=tc,
                               tuneGrid=expand.grid(n.trees=(1:5)*10, interaction.depth=(1:5)*1,
                                                shrinkage=0.5, n.minobsinnode=10))
#

set.seed(111)
model.lm.soybean   = caret::train(Yield_change ~., data=train_soybean, method="glmStepAIC", trControl=tc)

set.seed(111)
model.cart.soybean = caret::train(Yield_change ~., data=train_soybean, method="rpart", trControl=tc, control = cart_control
                                )
rpart.plot(model.cart.soybean$finalModel)


cp_values <- model.cart.soybean$results$cp
second_lowest_cp <- cp_values[order(cp_values)][2]

model.cart.soybean_second <- caret::train(Yield_change ~., data=train_soybean, method="rpart", trControl=tc, control = cart_control, cp = second_lowest_cp)

set.seed(111)
model.rf.soybean   = caret::train(Yield_change ~., data=train_soybean, method="rf", trControl=tc)

set.seed(111)
model.gbm.soybean  = caret::train(Yield_change ~., data=train_soybean, method="gbm", trControl=tc,
                                  tuneGrid=expand.grid(n.trees=(1:5)*10, interaction.depth=(1:5)*1,
                                               shrinkage=0.5, n.minobsinnode=10))
#

set.seed(111)
model.lm.maize   = caret::train(Yield_change ~., data=train_maize, method="glmStepAIC", trControl=tc)

set.seed(111)
model.cart.maize = caret::train(Yield_change ~., data=train_maize, method="rpart", trControl=tc, control = cart_control)
rpart.plot(model.cart.maize$finalModel)

set.seed(111)
model.rf.maize   = caret::train(Yield_change ~., data=train_maize, method="rf", trControl=tc)

set.seed(111)
model.gbm.maize  = caret::train(Yield_change ~., data=train_maize, method="gbm", trControl=tc,
                                  tuneGrid=expand.grid(n.trees=(1:5)*10, interaction.depth=(1:5)*1,
                                                       shrinkage=0.5, n.minobsinnode=10))
#

set.seed(111)
model.lm.barley.spring   = caret::train(Yield_change ~., data=train_barley.spring, method="glmStepAIC", trControl=tc)

set.seed(111)
cart_control1 <- rpart.control(minsplit = 5)
model.cart.barley.spring = caret::train(Yield_change ~., data=train_barley.spring, method="rpart", trControl=tc, control = cart_control1
                                        )
rpart.plot(model.cart.barley.spring$finalModel)

'rmse_values <- model.cart.barley.spring$results$RMSE
second_lowest_rmse <- sort(unique(rmse_values))[2]

model.cart.barley.spring_second <- caret::train(Yield_change ~ ., data = train_barley.spring, method = "rpart", trControl = tc, 
                                                control = cart_control1, 
                                                cp = cp_values[rmse_values == second_lowest_rmse])
rpart.plot(model.cart.barley.spring_second$finalModel)'

cp_values <- model.cart.barley.spring$results$cp
second_lowest_cp <- cp_values[order(cp_values)][2]

model.cart.barley.spring_second <- caret::train(Yield_change ~., data=train_barley.spring, method="rpart", trControl=tc, control = cart_control1, 
cp = second_lowest_cp)

rpart.plot(model.cart.barley.spring_second$finalModel)

set.seed(111)
model.rf.barley.spring   = caret::train(Yield_change ~., data=train_barley.spring, method="rf", trControl=tc)

set.seed(111)
model.gbm.barley.spring  = caret::train(Yield_change ~., data=train_barley.spring, method="gbm", trControl=tc,
                                tuneGrid=expand.grid(n.trees=(1:5)*10, interaction.depth=(1:5)*1,
                                                     shrinkage=0.5, n.minobsinnode=10))

#
set.seed(111)
model.lm.barley.winter   = caret::train(Yield_change ~., data=train_barley.winter, method="glmStepAIC", trControl=tc)

set.seed(111)
model.cart.barley.winter = caret::train(Yield_change ~., data=train_barley.winter, method="rpart", trControl=tc, control = cart_control)
rpart.plot(model.cart.barley.winter$finalModel)

set.seed(111)
model.rf.barley.winter   = caret::train(Yield_change ~., data=train_barley.winter, method="rf", trControl=tc)

set.seed(111)
model.gbm.barley.winter  = caret::train(Yield_change ~., data=train_barley.winter, method="gbm", trControl=tc,
                                        tuneGrid=expand.grid(n.trees=(1:5)*10, interaction.depth=(1:5)*1,
                                                             shrinkage=0.5, n.minobsinnode=10))
#

set.seed(111)
model.lm.wheat.spring   = caret::train(Yield_change ~., data=train_wheat.spring, method="glmStepAIC", trControl=tc)

set.seed(111)
model.cart.wheat.spring = caret::train(Yield_change ~., data=train_wheat.spring, method="rpart", trControl=tc, control = cart_control)
rpart.plot(model.cart.wheat.spring$finalModel)

set.seed(111)
model.rf.wheat.spring   = caret::train(Yield_change ~., data=train_wheat.spring, method="rf", trControl=tc)

set.seed(111)
model.gbm.wheat.spring  = caret::train(Yield_change ~., data=train_wheat.spring, method="gbm", trControl=tc,
                                       tuneGrid=expand.grid(n.trees=(1:5)*10, interaction.depth=(1:5)*1,
                                                            shrinkage=0.5, n.minobsinnode=10))
#

set.seed(111)
model.lm.wheat.winter   = caret::train(Yield_change ~., data=train_wheat.winter, method="glmStepAIC", trControl=tc)

set.seed(111)
model.cart.wheat.winter = caret::train(Yield_change ~., data=train_wheat.winter, method="rpart", trControl=tc, control = cart_control)
rpart.plot(model.cart.wheat.winter$finalModel)

set.seed(111)
model.rf.wheat.winter   = caret::train(Yield_change ~., data=train_wheat.winter, method="rf", trControl=tc)

set.seed(111)
model.gbm.wheat.winter  = caret::train(Yield_change ~., data=train_wheat.winter, method="gbm", trControl=tc,
                                        tuneGrid=expand.grid(n.trees=(1:5)*10, interaction.depth=(1:5)*1,
                                                             shrinkage=0.5, n.minobsinnode=10))
#


# -------------------------------------------------------
# performance evaluation

# with test
set.seed(111)
pred.lm.cotton <- predict(model.lm.cotton, test_cotton)

pred.cart.cotton <- predict(model.cart.cotton,test_cotton)

pred.rf.cotton <- predict(model.rf.cotton, test_cotton)

pred.gbm.cotton <- predict(model.gbm.cotton, test_cotton)



# R2: observation vs prediction
set.seed(111)
r2.lm.cotton   <- cor(pred.lm.cotton, test_cotton$Yield_change)^2 %>% round(.,4)
r2.cart.cotton <- cor(pred.cart.cotton, test_cotton$Yield_change)^2 %>% round(.,4)
r2.rf.cotton   <- cor(pred.rf.cotton, test_cotton$Yield_change)^2 %>% round(.,4)
r2.gbm.cotton  <- cor(pred.gbm.cotton, test_cotton$Yield_change)^2 %>% round(.,4)


r2.cotton <- data.frame(r2 = c(r2.lm.cotton,r2.cart.cotton,r2.rf.cotton,r2.gbm.cotton),
                      algorithm = c("Linear model","Decision Tree","Random Forests","Gradient Boosting") 
                      %>% factor(.,levels=c("Linear model","Decision Tree","Random Forests","Gradient Boosting")))

#RMSE
set.seed(111)
rmse.lm.cotton <- sqrt(mean((pred.lm.cotton - test_cotton$Yield_change)^2))
rmse.lm.cotton <- round(rmse.lm.cotton, 4)

rmse.cart.cotton <- sqrt(mean((pred.cart.cotton - test_cotton$Yield_change)^2))
rmse.cart.cotton <- round(rmse.cart.cotton, 4)

rmse.rf.cotton <- sqrt(mean((pred.rf.cotton - test_cotton$Yield_change)^2))
rmse.rf.cotton <- round(rmse.rf.cotton, 4)

rmse.gbm.cotton <- sqrt(mean((pred.gbm.cotton - test_cotton$Yield_change)^2))
rmse.gbm.cotton <- round(rmse.gbm.cotton, 4)

rmse.cotton <- data.frame(rmse = c(rmse.lm.cotton,rmse.cart.cotton,rmse.rf.cotton,rmse.gbm.cotton),
                        algorithm = c("Linear model","Decision Tree","Random Forests","Gradient Boosting") 
                        %>% factor(.,levels=c("Linear model","Decision Tree","Random Forests","Gradient Boosting")))
#
set.seed(111)
pred.lm.rice <- predict(model.lm.rice, test_rice)

pred.cart.rice <- predict(model.cart.rice,test_rice)

pred.rf.rice <- predict(model.rf.rice, test_rice)

pred.gbm.rice <- predict(model.gbm.rice, test_rice)



# R2: observation vs prediction
set.seed(111)
r2.lm.rice   <- cor(pred.lm.rice, test_rice$Yield_change)^2 %>% round(.,4)
r2.cart.rice <- cor(pred.cart.rice, test_rice$Yield_change)^2 %>% round(.,4)
r2.rf.rice   <- cor(pred.rf.rice, test_rice$Yield_change)^2 %>% round(.,4)
r2.gbm.rice  <- cor(pred.gbm.rice, test_rice$Yield_change)^2 %>% round(.,4)


r2.rice <- data.frame(r2 = c(r2.lm.rice,r2.cart.rice,r2.rf.rice,r2.gbm.rice),
                      algorithm = c("Linear model","Decision Tree","Random Forests","Gradient Boosting") 
                      %>% factor(.,levels=c("Linear model","Decision Tree","Random Forests","Gradient Boosting")))
#RMSE
set.seed(111)
rmse.lm.rice <- sqrt(mean((pred.lm.rice - test_rice$Yield_change)^2))
rmse.lm.rice <- round(rmse.lm.rice, 4)

rmse.cart.rice <- sqrt(mean((pred.cart.rice - test_rice$Yield_change)^2))
rmse.cart.rice <- round(rmse.cart.rice, 4)

rmse.rf.rice <- sqrt(mean((pred.rf.rice - test_rice$Yield_change)^2))
rmse.rf.rice <- round(rmse.rf.rice, 4)

rmse.gbm.rice <- sqrt(mean((pred.gbm.rice - test_rice$Yield_change)^2))
rmse.gbm.rice <- round(rmse.gbm.rice, 4)

rmse.rice <- data.frame(rmse = c(rmse.lm.rice,rmse.cart.rice,rmse.rf.rice,rmse.gbm.rice),
                      algorithm = c("Linear model","Decision Tree","Random Forests","Gradient Boosting") 
                      %>% factor(.,levels=c("Linear model","Decision Tree","Random Forests","Gradient Boosting")))
#
set.seed(111)
pred.lm.soybean <- predict(model.lm.soybean, test_soybean)

pred.cart.soybean <- predict(model.cart.soybean_second,test_soybean)

pred.rf.soybean <- predict(model.rf.soybean, test_soybean)

pred.gbm.soybean <- predict(model.gbm.soybean, test_soybean)



# R2: observation vs prediction
set.seed(111)
r2.lm.soybean   <- cor(pred.lm.soybean, test_soybean$Yield_change)^2 %>% round(.,4)
r2.cart.soybean <- cor(pred.cart.soybean, test_soybean$Yield_change)^2 %>% round(.,4)
r2.rf.soybean   <- cor(pred.rf.soybean, test_soybean$Yield_change)^2 %>% round(.,4)
r2.gbm.soybean  <- cor(pred.gbm.soybean, test_soybean$Yield_change)^2 %>% round(.,4)


r2.soybean <- data.frame(r2 = c(r2.lm.soybean,r2.cart.soybean,r2.rf.soybean,r2.gbm.soybean),
                      algorithm = c("Linear model","Decision Tree","Random Forests","Gradient Boosting") 
                     %>% factor(.,levels=c("Linear model","Decision Tree","Random Forests","Gradient Boosting")))

#RMSE
set.seed(111)
rmse.lm.soybean <- sqrt(mean((pred.lm.soybean - test_soybean$Yield_change)^2))
rmse.lm.soybean <- round(rmse.lm.soybean, 4)

rmse.cart.soybean <- sqrt(mean((pred.cart.soybean - test_soybean$Yield_change)^2))
rmse.cart.soybean <- round(rmse.cart.soybean, 4)

rmse.rf.soybean <- sqrt(mean((pred.rf.soybean - test_soybean$Yield_change)^2))
rmse.rf.soybean <- round(rmse.rf.soybean, 4)

rmse.gbm.soybean <- sqrt(mean((pred.gbm.soybean - test_soybean$Yield_change)^2))
rmse.gbm.soybean <- round(rmse.gbm.soybean, 4)


rmse.soybean <- data.frame(rmse = c(rmse.lm.soybean,rmse.cart.soybean,rmse.rf.soybean,rmse.gbm.soybean),
                         algorithm = c("Linear model","Decision Tree","Random Forests","Gradient Boosting") 
                         %>% factor(.,levels=c("Linear model","Decision Tree","Random Forests","Gradient Boosting")))

#
set.seed(111)
pred.lm.maize <- predict(model.lm.maize, test_maize)

pred.cart.maize <- predict(model.cart.maize,test_maize)

pred.rf.maize <- predict(model.rf.maize, test_maize)

pred.gbm.maize <- predict(model.gbm.maize, test_maize)



# R2: observation vs prediction
set.seed(111)
r2.lm.maize   <- cor(pred.lm.maize, test_maize$Yield_change)^2 %>% round(.,4)
r2.cart.maize <- cor(pred.cart.maize, test_maize$Yield_change)^2 %>% round(.,4)
r2.rf.maize   <- cor(pred.rf.maize, test_maize$Yield_change)^2 %>% round(.,4)
r2.gbm.maize  <- cor(pred.gbm.maize, test_maize$Yield_change)^2 %>% round(.,4)


r2.maize <- data.frame(r2 = c(r2.lm.maize,r2.cart.maize,r2.rf.maize,r2.gbm.maize),
                      algorithm = c("Linear model","Decision Tree","Random Forests","Gradient Boosting") 
                      %>% factor(.,levels=c("Linear model","Decision Tree","Random Forests","Gradient Boosting")))
#
#RMSE
set.seed(111)
rmse.lm.maize <- sqrt(mean((pred.lm.maize - test_maize$Yield_change)^2))
rmse.lm.maize <- round(rmse.lm.maize, 4)

rmse.cart.maize <- sqrt(mean((pred.cart.maize - test_maize$Yield_change)^2))
rmse.cart.maize <- round(rmse.cart.maize, 4)

rmse.rf.maize <- sqrt(mean((pred.rf.maize - test_maize$Yield_change)^2))
rmse.rf.maize <- round(rmse.rf.maize, 4)

rmse.gbm.maize <- sqrt(mean((pred.gbm.maize - test_maize$Yield_change)^2))
rmse.gbm.maize <- round(rmse.gbm.maize, 4)

rmse.maize <- data.frame(rmse = c(rmse.lm.maize,rmse.cart.maize,rmse.rf.maize,rmse.gbm.maize),
                       algorithm = c("Linear model","Decision Tree","Random Forests","Gradient Boosting") 
                       %>% factor(.,levels=c("Linear model","Decision Tree","Random Forests","Gradient Boosting")))
#
set.seed(111)
pred.lm.barley.spring <- predict(model.lm.barley.spring, test_barley.spring)

pred.cart.barley.spring <- predict(model.cart.barley.spring_second,test_barley.spring)

pred.rf.barley.spring <- predict(model.rf.barley.spring, test_barley.spring)

pred.gbm.barley.spring <- predict(model.gbm.barley.spring, test_barley.spring)



# R2: observation vs prediction
set.seed(111)
r2.lm.barley.spring   <- cor(pred.lm.barley.spring, test_barley.spring$Yield_change)^2 %>% round(.,4)
r2.cart.barley.spring <- cor(pred.cart.barley.spring, test_barley.spring$Yield_change)^2 %>% round(.,4)
r2.rf.barley.spring   <- cor(pred.rf.barley.spring, test_barley.spring$Yield_change)^2 %>% round(.,4)
r2.gbm.barley.spring  <- cor(pred.gbm.barley.spring, test_barley.spring$Yield_change)^2 %>% round(.,4)


r2.barley.spring <- data.frame(r2 = c(r2.lm.barley.spring,r2.cart.barley.spring,r2.rf.barley.spring,r2.gbm.barley.spring),
                       algorithm = c("Linear model","Decision Tree","Random Forests","Gradient Boosting") 
                       %>% factor(.,levels=c("Linear model","Decision Tree","Random Forests","Gradient Boosting")))
#

#RMSE
set.seed(111)
rmse.lm.barley.spring <- sqrt(mean((pred.lm.barley.spring - test_barley.spring$Yield_change)^2))
rmse.lm.barley.spring <- round(rmse.lm.barley.spring, 4)

rmse.cart.barley.spring <- sqrt(mean((pred.cart.barley.spring - test_barley.spring$Yield_change)^2))
rmse.cart.barley.spring <- round(rmse.cart.barley.spring, 4)

rmse.rf.barley.spring <- sqrt(mean((pred.rf.barley.spring - test_barley.spring$Yield_change)^2))
rmse.rf.barley.spring <- round(rmse.rf.barley.spring, 4)

rmse.gbm.barley.spring <- sqrt(mean((pred.gbm.barley.spring - test_barley.spring$Yield_change)^2))
rmse.gbm.barley.spring <- round(rmse.gbm.barley.spring, 4)

rmse.barley.spring <- data.frame(rmse = c(rmse.lm.barley.spring,rmse.cart.barley.spring,rmse.rf.barley.spring,rmse.gbm.barley.spring),
                               algorithm = c("Linear model","Decision Tree","Random Forests","Gradient Boosting") 
                               %>% factor(.,levels=c("Linear model","Decision Tree","Random Forests","Gradient Boosting")))

#
set.seed(111)
pred.lm.barley.winter <- predict(model.lm.barley.winter, test_barley.winter)

pred.cart.barley.winter <- predict(model.cart.barley.winter,test_barley.winter)

pred.rf.barley.winter <- predict(model.rf.barley.winter, test_barley.winter)

pred.gbm.barley.winter <- predict(model.gbm.barley.winter, test_barley.winter)



# R2: observation vs prediction
set.seed(111)
r2.lm.barley.winter   <- cor(pred.lm.barley.winter, test_barley.winter$Yield_change)^2 %>% round(.,4)
r2.cart.barley.winter <- cor(pred.cart.barley.winter, test_barley.winter$Yield_change)^2 %>% round(.,4)
r2.rf.barley.winter   <- cor(pred.rf.barley.winter, test_barley.winter$Yield_change)^2 %>% round(.,4)
r2.gbm.barley.winter  <- cor(pred.gbm.barley.winter, test_barley.winter$Yield_change)^2 %>% round(.,4)


r2.barley.winter <- data.frame(r2 = c(r2.lm.barley.winter,r2.cart.barley.winter,r2.rf.barley.winter,r2.gbm.barley.winter),
                               algorithm = c("Linear model","Decision Tree","Random Forests","Gradient Boosting") 
                               %>% factor(.,levels=c("Linear model","Decision Tree","Random Forests","Gradient Boosting")))
#RMSE
set.seed(111)
rmse.lm.barley.winter <- sqrt(mean((pred.lm.barley.winter - test_barley.winter$Yield_change)^2))
rmse.lm.barley.winter <- round(rmse.lm.barley.winter, 4)

rmse.cart.barley.winter <- sqrt(mean((pred.cart.barley.winter - test_barley.winter$Yield_change)^2))
rmse.cart.barley.winter <- round(rmse.cart.barley.winter, 4)

rmse.rf.barley.winter <- sqrt(mean((pred.rf.barley.winter - test_barley.winter$Yield_change)^2))
rmse.rf.barley.winter <- round(rmse.rf.barley.winter, 4)

rmse.gbm.barley.winter <- sqrt(mean((pred.gbm.barley.winter - test_barley.winter$Yield_change)^2))
rmse.gbm.barley.winter <- round(rmse.gbm.barley.winter, 4)

rmse.barley.winter <- data.frame(rmse = c(rmse.lm.barley.winter,rmse.cart.barley.winter,rmse.rf.barley.winter,rmse.gbm.barley.winter),
                               algorithm = c("Linear model","Decision Tree","Random Forests","Gradient Boosting") 
                               %>% factor(.,levels=c("Linear model","Decision Tree","Random Forests","Gradient Boosting")))
#
set.seed(111)
pred.lm.wheat.spring <- predict(model.lm.wheat.spring, test_wheat.spring)

pred.cart.wheat.spring <- predict(model.cart.wheat.spring,test_wheat.spring)

pred.rf.wheat.spring <- predict(model.rf.wheat.spring, test_wheat.spring)

pred.gbm.wheat.spring <- predict(model.gbm.wheat.spring, test_wheat.spring)


# R2: observation vs prediction
set.seed(111)
r2.lm.wheat.spring   <- cor(pred.lm.wheat.spring, test_wheat.spring$Yield_change)^2 %>% round(.,4)
r2.cart.wheat.spring <- cor(pred.cart.wheat.spring, test_wheat.spring$Yield_change)^2 %>% round(.,4)
r2.rf.wheat.spring   <- cor(pred.rf.wheat.spring, test_wheat.spring$Yield_change)^2 %>% round(.,4)
r2.gbm.wheat.spring  <- cor(pred.gbm.wheat.spring, test_wheat.spring$Yield_change)^2 %>% round(.,4)


r2.wheat.spring <- data.frame(r2 = c(r2.lm.wheat.spring,r2.cart.wheat.spring,r2.rf.wheat.spring,r2.gbm.wheat.spring),
                              algorithm = c("Linear model","Decision Tree","Random Forests","Gradient Boosting") 
                              %>% factor(.,levels=c("Linear model","Decision Tree","Random Forests","Gradient Boosting")))
#RMSE
set.seed(111)
rmse.lm.wheat.spring <- sqrt(mean((pred.lm.wheat.spring - test_wheat.spring$Yield_change)^2))
rmse.lm.wheat.spring <- round(rmse.lm.wheat.spring, 4)

rmse.cart.wheat.spring <- sqrt(mean((pred.cart.wheat.spring - test_wheat.spring$Yield_change)^2))
rmse.cart.wheat.spring <- round(rmse.cart.wheat.spring, 4)

rmse.rf.wheat.spring <- sqrt(mean((pred.rf.wheat.spring - test_wheat.spring$Yield_change)^2))
rmse.rf.wheat.spring <- round(rmse.rf.wheat.spring, 4)

rmse.gbm.wheat.spring <- sqrt(mean((pred.gbm.wheat.spring - test_wheat.spring$Yield_change)^2))
rmse.gbm.wheat.spring <- round(rmse.gbm.wheat.spring, 4)

rmse.wheat.spring <- data.frame(rmse = c(rmse.lm.wheat.spring,rmse.cart.wheat.spring,rmse.rf.wheat.spring,rmse.gbm.wheat.spring),
                              algorithm = c("Linear model","Decision Tree","Random Forests","Gradient Boosting") 
                              %>% factor(.,levels=c("Linear model","Decision Tree","Random Forests","Gradient Boosting")))

#
set.seed(111)
pred.lm.wheat.winter <- predict(model.lm.wheat.winter, test_wheat.winter)

pred.cart.wheat.winter <- predict(model.cart.wheat.winter,test_wheat.winter)

pred.rf.wheat.winter <- predict(model.rf.wheat.winter, test_wheat.winter)

pred.gbm.wheat.winter <- predict(model.gbm.wheat.winter, test_wheat.winter)



# R2: observation vs prediction
set.seed(111)
r2.lm.wheat.winter   <- cor(pred.lm.wheat.winter, test_wheat.winter$Yield_change)^2 %>% round(.,4)
r2.cart.wheat.winter <- cor(pred.cart.wheat.winter, test_wheat.winter$Yield_change)^2 %>% round(.,4)
r2.rf.wheat.winter   <- cor(pred.rf.wheat.winter, test_wheat.winter$Yield_change)^2 %>% round(.,4)
r2.gbm.wheat.winter  <- cor(pred.gbm.wheat.winter, test_wheat.winter$Yield_change)^2 %>% round(.,4)


r2.wheat.winter <- data.frame(r2 = c(r2.lm.wheat.winter,r2.cart.wheat.winter,r2.rf.wheat.winter,r2.gbm.wheat.winter),
                               algorithm = c("Linear model","Decision Tree","Random Forests","Gradient Boosting") 
                               %>% factor(.,levels=c("Linear model","Decision Tree","Random Forests","Gradient Boosting")))


#RMSE
set.seed(111)
rmse.lm.wheat.winter <- sqrt(mean((pred.lm.wheat.winter - test_wheat.winter$Yield_change)^2))
rmse.lm.wheat.winter <- round(rmse.lm.wheat.winter, 4)

rmse.cart.wheat.winter <- sqrt(mean((pred.cart.wheat.winter - test_wheat.winter$Yield_change)^2))
rmse.cart.wheat.winter <- round(rmse.cart.wheat.winter, 4)

rmse.rf.wheat.winter <- sqrt(mean((pred.rf.wheat.winter - test_wheat.winter$Yield_change)^2))
rmse.rf.wheat.winter <- round(rmse.rf.wheat.winter, 4)

rmse.gbm.wheat.winter <- sqrt(mean((pred.gbm.wheat.winter - test_wheat.winter$Yield_change)^2))
rmse.gbm.wheat.winter <- round(rmse.gbm.wheat.winter, 4)

rmse.wheat.winter <- data.frame(rmse = c(rmse.lm.wheat.winter,rmse.cart.wheat.winter,rmse.rf.wheat.winter,rmse.gbm.wheat.winter),
                              algorithm = c("Linear model","Decision Tree","Random Forests","Gradient Boosting") 
                              %>% factor(.,levels=c("Linear model","Decision Tree","Random Forests","Gradient Boosting")))


###Figures

#R2
Fig03.1 <-
  ggplot(r2.cotton, aes(x=algorithm, y=r2, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ggtitle("R-Squared values for cotton")+
  xlab("IML algorithm")+
  ylab("R-squred") +
  scale_fill_manual(values=c("Linear model"="lightgreen",
                             "Decision Tree"="orange",
                             "Random Forests"="red",
                             "Gradient Boosting"="blue")) +
  theme_bw(
    base_size = 14
  )  +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))


Fig03.2 <-
  ggplot(r2.rice, aes(x=algorithm, y=r2, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ggtitle("R-Squared values for Rice")+
  xlab("IML algorithm")+
  ylab("R-squred") +
  scale_fill_manual(values=c("Linear model"="lightgreen",
                             "Decision Tree"="orange",
                             "Random Forests"="red",
                             "Gradient Boosting"="blue")) +
  theme_bw(
    base_size = 14
  )  +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

Fig03.3 <-
  ggplot(r2.soybean, aes(x=algorithm, y=r2, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ggtitle("R-Squared values for Soybean")+
  xlab("IML algorithm")+
  ylab("R-squred") +
  scale_fill_manual(values=c("Linear model"="lightgreen",
                             "Decision Tree"="orange",
                             "Random Forests"="red",
                             "Gradient Boosting"="blue")) +
  theme_bw(
    base_size = 14
  )  +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

Fig03.4 <-
  ggplot(r2.maize, aes(x=algorithm, y=r2, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ggtitle("R-Squared values for maize")+
  xlab("IML algorithm")+
  ylab("R-squred") +
  scale_fill_manual(values=c("Linear model"="lightgreen",
                             "Decision Tree"="orange",
                             "Random Forests"="red",
                             "Gradient Boosting"="blue")) +
  theme_bw(
    base_size = 14
  )  +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

Fig03.5 <-
  ggplot(r2.barley.spring, aes(x=algorithm, y=r2, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ggtitle("R-Squared values for barley in spring")+
  xlab("IML algorithm")+
  ylab("R-squred") +
  scale_fill_manual(values=c("Linear model"="lightgreen",
                             "Decision Tree"="orange",
                             "Random Forests"="red",
                             "Gradient Boosting"="blue")) +
  theme_bw(
    base_size = 14
  )  +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))


Fig03.6 <-
  ggplot(r2.barley.winter, aes(x=algorithm, y=r2, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ggtitle("R-Squared values for barley.winter")+
  xlab("IML algorithm")+
  ylab("R-squred") +
  scale_fill_manual(values=c("Linear model"="lightgreen",
                             "Decision Tree"="orange",
                             "Random Forests"="red",
                             "Gradient Boosting"="blue")) +
  theme_bw(
    base_size = 14
  )  +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

Fig03.7 <-
  ggplot(r2.wheat.spring, aes(x=algorithm, y=r2, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ggtitle("R-Squared values for wheat.spring")+
  xlab("IML algorithm")+
  ylab("R-squred") +
  scale_fill_manual(values=c("Linear model"="lightgreen",
                             "Decision Tree"="orange",
                             "Random Forests"="red",
                             "Gradient Boosting"="blue")) +
  theme_bw(
    base_size = 14
  )  +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))


Fig03.8 <-
  ggplot(r2.wheat.winter, aes(x=algorithm, y=r2, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ggtitle("R-Squared values for wheat.winter")+
  xlab("IML algorithm")+
  ylab("R-squred") +
  scale_fill_manual(values=c("Linear model"="lightgreen",
                             "Decision Tree"="orange",
                             "Random Forests"="red",
                             "Gradient Boosting"="blue")) +
  theme_bw(
    base_size = 14
  )  +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))


Fig03 <- (Fig03.1 / Fig03.2 / Fig03.3 / Fig03.4 / Fig03.5 / Fig03.6 / Fig03.7 / Fig03.8) +
  
  plot_layout(ncol=2,nrow = 4, widths = c(1,1), heights = c(1,1)) + 
  plot_annotation(tag_levels = 'i')


Fig03

#RMSE
Fig04.1 <-
  ggplot(rmse.cotton, aes(x=algorithm, y=rmse, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ggtitle("RMSE values for cotton")+
  xlab("IML algorithm")+
  ylab("RMSE") +
  scale_fill_manual(values=c("Linear model"="lightgreen",
                             "Decision Tree"="orange",
                             "Random Forests"="red",
                             "Gradient Boosting"="blue")) +
  theme_bw(
    base_size = 14
  )  +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))
Fig04.2 <-
  ggplot(rmse.rice, aes(x=algorithm, y=rmse, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ggtitle("RMSE values for rice")+
  xlab("IML algorithm")+
  ylab("RMSE") +
  scale_fill_manual(values=c("Linear model"="lightgreen",
                             "Decision Tree"="orange",
                             "Random Forests"="red",
                             "Gradient Boosting"="blue")) +
  theme_bw(
    base_size = 14
  )  +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

Fig04.3 <-
  ggplot(rmse.soybean, aes(x=algorithm, y=rmse, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ggtitle("RMSE values for soybean")+
  xlab("IML algorithm")+
  ylab("RMSE") +
  scale_fill_manual(values=c("Linear model"="lightgreen",
                             "Decision Tree"="orange",
                             "Random Forests"="red",
                             "Gradient Boosting"="blue")) +
  theme_bw(
    base_size = 14
  )  +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))


Fig04.4 <-
  ggplot(rmse.maize, aes(x=algorithm, y=rmse, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ggtitle("RMSE values for maize")+
  xlab("IML algorithm")+
  ylab("RMSE") +
  scale_fill_manual(values=c("Linear model"="lightgreen",
                             "Decision Tree"="orange",
                             "Random Forests"="red",
                             "Gradient Boosting"="blue")) +
  theme_bw(
    base_size = 14
  )  +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))


Fig04.5 <-
  ggplot(rmse.barley.spring, aes(x=algorithm, y=rmse, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ggtitle("RMSE values for barley in spring")+
  xlab("IML algorithm")+
  ylab("RMSE") +
  scale_fill_manual(values=c("Linear model"="lightgreen",
                             "Decision Tree"="orange",
                             "Random Forests"="red",
                             "Gradient Boosting"="blue")) +
  theme_bw(
    base_size = 14
  )  +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))


Fig04.6 <-
  ggplot(rmse.barley.winter, aes(x=algorithm, y=rmse, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ggtitle("RMSE values for barley in winter")+
  xlab("IML algorithm")+
  ylab("RMSE") +
  scale_fill_manual(values=c("Linear model"="lightgreen",
                             "Decision Tree"="orange",
                             "Random Forests"="red",
                             "Gradient Boosting"="blue")) +
  theme_bw(
    base_size = 14
  )  +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))


Fig04.7 <-
  ggplot(rmse.wheat.spring, aes(x=algorithm, y=rmse, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ggtitle("RMSE values for wheat in spring")+
  xlab("IML algorithm")+
  ylab("RMSE") +
  scale_fill_manual(values=c("Linear model"="lightgreen",
                             "Decision Tree"="orange",
                             "Random Forests"="red",
                             "Gradient Boosting"="blue")) +
  theme_bw(
    base_size = 14
  )  +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))


Fig04.8 <-
  ggplot(rmse.wheat.winter, aes(x=algorithm, y=rmse, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ggtitle("RMSE values for wheat in winter")+
  xlab("IML algorithm")+
  ylab("RMSE") +
  scale_fill_manual(values=c("Linear model"="lightgreen",
                             "Decision Tree"="orange",
                             "Random Forests"="red",
                             "Gradient Boosting"="blue")) +
  theme_bw(
    base_size = 14
  )  +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))


Fig04 <- (Fig04.1 / Fig04.2 / Fig04.3 / Fig04.4 / Fig04.5 / Fig04.6 / Fig04.7 / Fig04.8) +
  
  plot_layout(ncol=2,nrow = 4, widths = c(1,1), heights = c(1,1)) + 
  plot_annotation(tag_levels = 'i')
Fig04

'# main title
Fig02 + theme(plot.title = element_text("Ariel", "bold", "black", "12"))
# x axis title 
Fig02 + theme(axis.title.x = element_text("Ariel", "bold", "black", "12"))
# y axis title
Fig02 + theme(axis.title.y = element_text("Ariel", "bold", "black", "12"))
ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5))'
