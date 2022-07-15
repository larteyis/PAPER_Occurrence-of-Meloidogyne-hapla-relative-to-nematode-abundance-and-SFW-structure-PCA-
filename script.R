
######PCA############

# Data#
#Import .csv file into R
my_data <- read.csv(file.choose())
View(my_data)

str(my_data)
summary(my_data)

# Partition Data
set.seed(111)
ind <- sample(2, nrow(my_data),
              replace = TRUE,
              prob = c(0.8, 0.2))
training <- my_data[ind==1,]
testing <- my_data[ind==2,]

# Scatter Plot & Correlations
library(psych)
pairs.panels(training[1:3,12:20],
             gap = 0,
             bg = c("3", "16", "17","18","19","20")[training$Locations],
             pch=21)

# Principal Component Analysis
pc <- prcomp(training[,13:21],
             center = TRUE,
             scale. = TRUE) #scale normalises the dependent variables before pCA is done

attributes(pc)
pc$center
pc$scale
print(pc)
summary(pc)

# Orthogonality of PCs
pairs.panels(pc$x,
             gap=0,
             bg = c("3", "16", "17","18","19","20")[training$Locations],
             pch=21) # Milticolineaity has been taken care of. this is evident in the zero values shown in the output
#bg = c("red", "yellow", "blue","green","purple","black")

# Bi-Plot
#install.packages("remotes")
#remotes::install_github("vqv/ggbiplot")
library(ggbiplot)
g <- ggbiplot(pc,
              obs.scale = 1,
              var.scale = 1,
              groups = training$Locations)#,
#ellipse = TRUE,#
#circle = TRUE,#
#ellipse.prob = 0.70)#
g <- g + geom_point(aes(shape = groups), size =5) +
  scale_shape_manual(values = c(1,2,3,4,11,8))
g <- g + theme_classic()+theme(legend.direction = 'horizontal',
               legend.position = 'top',text = element_text(size=16),legend.text=element_text(size=20))
print(g)





