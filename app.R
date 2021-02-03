library(shiny)
library(readxl)
library(rlist)
library(plotly)
library(tsbox)
library(xts)



#data read
tmp <- data.frame(read_excel("PFE-2020.xlsx"))


# finding list of sectors in the excel file
list_of_sectors <- c()
for (i in colnames(tmp)){
  if(!startsWith(i,"Dernier")&&!startsWith(i,"Date")){
    list_of_sectors <- c(list_of_sectors,i)
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

plot_value_over_date <- function(sector,val_min=0,val_max=150,
                                 date_min=as.Date("2007-01-21"),date_max=as.Date("2020-12-27")){
  x <-na.omit(tmp[which(colnames(tmp)==sector)+1])
  y <-na.omit(tmp[which(colnames(tmp)==sector)+2])
  
  x1 <- as.Date(vector())
  y1 <- vector()
  for (i in 1:length(x$Dat)){
    if((y$Der[i]>=val_min)&&(y$Der[i]<=val_max)&&
       (as.Date(x$Dat[i][1])>=date_min) && (as.Date(x$Dat[i][1])<=date_max)){
      
      x1 <- c(x1,as.Date(x$Dat[i][1]))
      y1 <- c(y1,y$Der[i])
    }
  }
  result <- data.frame(
    x_axis = x1,
    y_axis = y1
  )
  
  # linear trend + confidence interval
  p3 <- ggplot(result, aes(x=x_axis, y=y_axis)) +
    geom_point() +
    geom_smooth(color="red", fill="#69b3a2", se=TRUE) +
    labs(title = "ETF over Time",
         caption = "Fiting with Red, Confidence interval light blue")
  

  
  return(p3)
  
}

value_over_date <- function(sector,val_min=0,val_max=150,
                                 date_min=as.Date("2007-01-21"),date_max=as.Date("2020-12-27")){
  x <-na.omit(tmp[which(colnames(tmp)==sector)+1])
  y <-na.omit(tmp[which(colnames(tmp)==sector)+2])
  
  x1 <- as.Date(vector())
  y1 <- vector()
  for (i in 1:length(x$Dat)){
    if((y$Der[i]>=val_min)&&(y$Der[i]<=val_max)&&
       (as.Date(x$Dat[i][1])>=date_min) && (as.Date(x$Dat[i][1])<=date_max)){
      
      x1 <- c(x1,as.Date(x$Dat[i][1]))
      y1 <- c(y1,y$Der[i])
    }
  }
  result <- data.frame(
    x_axis = x1,
    y_axis = y1
  )

  xts <- xts(result$y_axis,result$x_axis)
  ts <- ts_ts(xts)
  
  return(ts)
  
}





ui <- fluidPage(
  titlePanel("PFE"),
  
  tabsetPanel(id = "tabs",
    tabPanel("Main",
      sidebarLayout(
        sidebarPanel(
          helpText("Select Values for Graph."),
          
          selectInput("sector_selector", 
                      label = "Choose a sector for graph",
                      choices = list_of_sectors,
                      selected = list_of_sectors[1]),
          
          
          textOutput("kt"),
          
          uiOutput("slider"),
          
          
          textOutput("selected_var"),
          textOutput("min_max"),
          
          #static date selector
          dateInput("date1","From",value = "2006-09-03"),
          dateInput("date2","To")
          
        ),
        mainPanel(
          titlePanel("Main"),
          plotlyOutput("plot1"))
      )),
    
  tabPanel("ARIMA",
       sidebarLayout(
         sidebarPanel(
           helpText("Select Values for Graph."),
           
           selectInput("sector_selector2", 
                       label = "Choose a sector for graph",
                       choices = list_of_sectors,
                       selected = list_of_sectors[1]),
           
         ),
         mainPanel(
           titlePanel("Model"),
           plotOutput("plot2"))
       )))
      
    )


server <- function (input,output,session){
  

  
  output$selected_var <- renderText({
    paste("You have selected", input$sector_selector)
  })
  
  output$min_max <- renderText({
    paste("You have chosen a range that goes from", input$inSlider[1], "to", input$inSlider[2])
  })
  

  
  output$plot1 <- renderPlotly({
    plot_value_over_date(input$sector_selector,
                         val_min =input$inSlider[1],
                         val_max = input$inSlider[2],
                         date_min = input$date1,
                         date_max = input$date2)
                         #date_min = as.Date(input$daterange[1]),
                         #date_max = as.Date(input$daterange[2]))
  })
  
  output$plot2 <- renderPlot({
    tm <-value_over_date(input$sector_selector2)
    plot(decompose(tm))  

  })
  
  
  
  

  output$slider <- renderUI({
    sliderInput("inSlider", "Filter value for Graph", min=find_min_max(input$sector_selector)[1],
                max=find_min_max(input$sector_selector)[2],
                value=c(find_min_max(input$sector_selector)[1],find_min_max(input$sector_selector)[2]))
  })
  

  output$kt <- renderText({
    paste("Data available from",date_min_max(input$sector_selector)[1])
  })

}


shinyApp(ui=ui, server = server)
