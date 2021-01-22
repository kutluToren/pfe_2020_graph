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

plot_value_over_date <- function(sector){
  x <-na.omit(tmp[which(colnames(tmp)==sector)+1])
  y <-na.omit(tmp[which(colnames(tmp)==sector)+2])

  plot(y$Der~x$Dat,type="l",
       xlab="Date",ylab="Price")
}



# observe({
#   
#   val <- input$sector_selector
#   
#   updateSliderInput(session,"range",value = val,
#                     min = find_min_max(input$var)[1], 
#                     max = find_min_max(input$var)[2]
#   )
# })

  

