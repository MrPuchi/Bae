library(GGally)
library(reshape2)
library(psych)
library(readr)
library(devtools)
library(ggplot2)
install_github("vqv/ggbiplot")
library(ggbiplot)


## Getting the DataSet into R using the import dataset function

ecommerce_data <- read_csv("ecommerce_data.csv", 
                           col_types = cols(date = col_date(format = "%Y-%m-%d"), 
                                            dt = col_date(format = "%Y-%m-%d"), 
                                            time = col_character(), user_reg_tm = col_date(format = "%Y-%m-%d")))

#Preparing the Data
  #type 4 means purchase, so we subset by type == 4
  
  purchase.data = as.data.frame(subset(x = ecommerce_data, type == '4'))
  names(purchase.data)
  
  summary(purchase.data)
  names(purchase.data)[14]='date_action'
  names(purchase.data)[13]='date_registered'
  names(purchase.data)[12]='user_activity_level'
  names(purchase.data)[11]='bad_comment_distribution'
  
  #Subsetting with relevant columns: userid skuid cate brand bad_comment_distribution user_activity_level date_registered date_action
  
  purchase.data.type4 = purchase.data[,c(1,2,6,7,11,12,13,14)]
  unique(purchase.data.type4$cate)
  
  write.csv(purchase.data.type4,'purchase-data.csv')



#creating the matrix for categories and shopping carts
  cate.matrix = SC.cate.matrix[,c(2:9)]
  shopping.cart.cate = purchase.data.type4[,c(1,8,3)]
  shopping.cart.cate$shopping.id = gsub(pattern = '-','',
                                        x = paste(shopping.cart.cate$userid,shopping.cart.cate$date_action,sep=''),
                                        ignore.case = TRUE)
  shopping.cart.cate = shopping.cart.cate[,c(4,3)]
  SC.cate.matrix= dcast(data = shopping.cart.cate, shopping.id~cate)
  
#Correlation plots
  ggcorr(SC.cate.matrix[,c(2:9)], label = TRUE)
  pairs.panels(SC.cate.matrix[,-1])
  
  
#PCA ANALYSIS  
  pca <- prcomp(cate.matrix, center = TRUE, scale.=TRUE) 
  pca$center  #avg of each category
  pca$scale   #std of each category

## make a scree plot to check for 
  summary(pca)

#plot component loadings
  plot(pca$rotation[,1],pca$rotation[,2]
       , main= "Component Loadings",
       xlab= "PC1",
       ylab= "PC2",
       col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2)
  text(pca$rotation[,1], pca$rotation[,2], labels=rownames(pca$rotation), cex= 0.7, pos= 2)

## PCA Graph  with categories
rownames(pca$rotation)

library(ggplot2)

pca.data <- data.frame(Sample=rownames(pca$x),
                       X=pca$x[,1],
                       Y=pca$x[,2])
ggplot(data=pca.data, aes(x=X, y=Y, label=Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("My PCA Graph")

## get the name of the top 10 measurements
## most to pc1.
loading_scores <- pca$rotation[,1]
sc_scores <- abs(loading_scores) ## get the magnitudes
sc_ranked <- sort(sc_scores, decreasing=TRUE)
top_10_sc <- names(sc_scores[1:10])

top_10_sc ## show the names of the top 10 purchases

pca$rotation[top_10_sc,1] ## show the scores (and +/- sign)


#PCA from purchases perspective (OPTION 2)

pca <- prcomp(t(cate.matrix), center = TRUE, scale.=TRUE) 

#The graph with the purchases plotted
g <- ggbiplot(pca,
              obs.scale = 1,
              var.scale = 1,
              ellipse = TRUE,
              circle = TRUE,
              ellipse.prob = 0.68)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)


