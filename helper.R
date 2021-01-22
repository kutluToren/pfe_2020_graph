library(readxl)
library(rlist)
library(ggplot2)

tmp <- data.frame(read_excel("PFE-2020.xlsx"))
list_of_sectors <- c()

for (i in colnames(tmp)){
  if(!startsWith(i,"Dernier")&&!startsWith(i,"Date")){
    list_of_sectors <- c(list_of_sectors,i)
  }
  if(startsWith(i,"Dernier")){
    tmp[(which(colnames(tmp)==i))]<-lapply(tmp[(which(colnames(tmp)==i))], as.numeric)
  }
}

#return the minimum and maximum value for slider
find_min_max <- function(sector){
  x <-na.omit(tmp[which(colnames(tmp)==sector)+2])
  
  return (c(min(x),
            max(x)))
}

#return the minimum and maximum value for slider
date_min_max <- function(sector){
  x <-na.omit(tmp[which(colnames(tmp)==sector)+1])
  
  return (c(min(as.Date(x[,1])),
            max(as.Date(x[,1]))))
}

#return plot with adjusted values
plot_value_over_date <- function(sector,val_min=0,val_max=150,date_min="2007-01-21",date_max="2020-12-27"){
  x <-na.omit(tmp[which(colnames(tmp)==sector)+1])
  y <-na.omit(tmp[which(colnames(tmp)==sector)+2])
  
  x1 <- as.Date(vector())
  y1 <- vector()
  for (i in 1:length(x$Dat)){
    if((y$Der[i]>=val_min)&&(y$Der[i]<=val_max)){
      x1 <- c(x1,as.Date(x$Dat[i][1]))
      y1 <- c(y1,y$Der[i])
    }
  }
  result <- data.frame(
    x_axis = x1,
    y_axis = y1
  )
  
  return(plot(result$y_axis~result$x_axis,type="l",
        xlab="Date",ylab="Price"))
}




