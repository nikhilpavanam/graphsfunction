install.packages("MASS")
library(MASS)

data("Boston")
View(Boston)
?Boston


data("mtcars")
View(mtcars)


Graphs <- function(data)
{
  if(!is.data.frame(data))
  {
    stop("The given object is not a dataframe")
  }
  for(i in 1:ncol(data))
  {
    if(is.numeric(data[,i]))
    {
      png(paste(names(data)[i],".png",sep = ""))
      
      par(mfrow = c(2,1))
      
      boxplot(data[,i],main = paste("BoxPlot of",names(data)[i]),col = "lightgreen",
              border = 'grey5', horizontal = T)
      
      hist(data[,i],main = paste("Histogram of",names(data)[i]), xlab = names(data)[i],
           col = "skyblue2", ylab = "No Of Houses")
      
      dev.off()
    }
  }
}

setwd("C:\\Users\\lenovo\\Documents\\Graphs Folder")
getwd()

Graphs(Boston)


#SUGGESTION 1:
#Often we are not required the graphs for all the numeric variables. Try to improve the code
#by adding an additional parameter "variable" that can take a vector of variable index and 
#return the graphs for only those variables.
#
#Example: Graphs(Boston, c(1,3,4))
#Will generate the graphics for only the numerical variables among the variables 1,3 & 4
#in the data Boston

Graphs <- function(data,newvar)
{
  if(!is.data.frame(data))
  {
    stop("The given object is not a dataframe")
  }
  
  data = data[,newvar] # Subsetting only the variables we want, passed in "newvar" parameter.
  
  for(i in 1:ncol(data))
  {
    if(is.numeric(data[,i]))
    {
      png(paste(names(data)[i],".png",sep = ""))
      
      par(mfrow = c(2,1))
      
      boxplot(data[,i],main = paste("BoxPlot of",names(data)[i]),col = "lightgreen",
              border = 'grey5', horizontal = T)
      
      hist(data[,i],main = paste("Histogram of",names(data)[i]), xlab = names(data)[i],
           col = "skyblue2", ylab = "No Of Houses")
      
      dev.off()
    }
  }
}

Boston[,c(1,3,4)]

Graphs(Boston, c(1,3,4))



#SUGGESTION 2:
#Improve the code in suggestion 1 such that if the argument variable is ignored then it will
#return the graphs of all the numeric variables in the data by default.



Graphs <- function(data, newvar = 1:ncol(data))
{
  if(!is.data.frame(data))
  {
    stop("The given object is not a dataframe")
  }
  
  data = data[,newvar] # Subsetting only the variables we want passed in "newvar" parameter.
  
  for(i in 1:ncol(data))
  {
    if(is.numeric(data[,i]))
    {
      png(paste(names(data)[i],".png",sep = ""))
      
      par(mfrow = c(2,1))
      
      boxplot(data[,i],main = paste("BoxPlot of",names(data)[i]),col = "lightgreen",
              border = 'grey5', horizontal = T)
      
      hist(data[,i],main = paste("Histogram of",names(data)[i]), xlab = names(data)[i],
           col = "skyblue2", ylab = "No Of Houses")
      
      dev.off()
    }
  }
}

Graphs(Boston)

Graphs(Boston, c(1,3,4))



#SUGGESTION 3:
#We ignored the cateorical variables in our discussion. Make some improvement in your codes
#in suggestion 2 such that the function will take the argument "data" and "variable" and will
#return boxplots & histograms for the numerical variables and barplots and pie charts for
#the categorical variables.
#
#Example:
#Graphs(mtcars)
#will get the necessary graphics for all numeric variables and categorical variables in the
#data

car <- mtcars
car$am <- as.factor(car$am)
car$vs <- as.factor(car$vs)

Boston1 <- Boston
str(Boston1)
Boston1$chas <- as.factor(Boston$chas)
Boston1$rad <- as.factor(Boston$rad)


Graphs <- function(data, newvar = 1:ncol(data))
{
  if(!is.data.frame(data))
  {
    stop("The given object is not a dataframe")
  }
  
  
  data = data[,newvar] # Subsetting only the variables we want passed in "newvar" parameter.
  
  for(i in 1:ncol(data))
  {
    {
      if(is.numeric(data[,i]))
      {
        png(paste(names(data)[i],".png",sep = ""))
        
        par(mfrow = c(2,1))
        
        boxplot(data[,i],main = paste("BoxPlot of",names(data)[i]),col = "lightgreen",
                border = 'grey5', horizontal = T)
        
        hist(data[,i],main = paste("Histogram of",names(data)[i]), xlab = names(data)[i],
             col = "skyblue2", ylab = "No Of Houses")
        
        dev.off()
      }
      else
      {
        png(paste(names(data)[i],".png",sep = ""))
        
        par(mfrow = c(1,1))
        
        barplot(table(data[,i]),main = paste("Barplot of",names(data)[i]),
                xlab = names(data)[i],col = "orange",ylab = "No Of Houses")
        
        dev.off()
      }
    }
    
  }
}

Graphs(car)
Graphs(Boston1)


#SUGGESTION 4:
#Probably you need not want to mess up your working directory with so many image files...
#Create an additional argument for the function "dir" (directory), such that the function
#exports all the files to your specified folder (which need not necessaryly be your working
#directory).
#
#Example:
#Graphs(Boston, Variable = c(1,3,4), dir = ".../Praxis/LearntSometingNew/Graphs")
#will generate the necessary graphics for the variables 1, 3 and 4 in the specified location
#in your system i.e. ".../Praxis/LearntSometingNew/Graphs"

Graphsfunction <- function(data, newvar = 0, graphsdir = 0)
{
  options(warn=-1)
  if(!is.data.frame(data))
  {
    stop("The given object is not a data frame") 
  }
    
  
  {if (graphsdir == 0) #set export directory as working directory if no argument is mentioned
  {
    direct=setwd()
  }
    else
    {
      direct=setwd(graphsdir)
    }}
  
  
  {if (newvar == 0)
  {
    d=data
  }
    else
    {
      d=data[,newvar]
    }}
  
  for(i in 1:ncol(d))
  {
    
    {if(is.numeric(d[,i]))
    {
      png(paste(names(data)[i], ".png", sep=""))
      
      par(mfrow=c(2,1))
      
      boxplot(d[,i], main = paste("Boxplot of", names(d)[i]),
              ylab = names(d)[i], col = "lightgreen", border = "grey5", horizontal = T)
      
      hist(d[,i], main = paste("Histogram of", names(d)[i]),
           xlab = names(d)[i], ylab = "Frequency", col = "skyblue2", border=F)
      
      dev.off()
    }
      else
        
      {
        
        d[,i]=as.factor(d[,i])
        slices <- as.numeric(unname(table(d[,i])))
        lbls <- names(table(d[,i]))
        
        png(paste(names(data)[i], ".png", sep=""))
        
        par(mfrow=c(2,1))
        
        barplot(table(d[,i]), freq=T,main = paste("Barplot of", names(d)[i]),
                xlab = names(d)[i], ylab = "Frequency",col = "orange",
                border = "black", horizontal = T)
        
        pie(slices,labels = lbls, col=rainbow(length(lbls)),
            main = paste("PieChart of", names(d)[i]))
        
        dev.off()
        
      }}
  }
}

Graphsfunction(Boston1,c(1,5,9),"C:\\Users\\lenovo\\Documents\\Graphs Folder")






