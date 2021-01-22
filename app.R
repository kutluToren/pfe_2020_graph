library(shiny)
library(readxl)
library(rlist)



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


ui <- fluidPage(
  titlePanel("PFE"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select Values for Graph."),
      
      selectInput("sector_selector", 
                  label = "Choose a sector for graph",
                  choices = list_of_sectors,
                  selected = list_of_sectors[1]),
      
      uiOutput("slider"),
      
      uiOutput("date_range")
    ),
    mainPanel(
      textOutput("selected_var"),
      textOutput("min_max"),
      textOutput("control_date_range"),
      plotOutput("plot1")
    )
    
  )
)



server <- function (input,output,session){
  

  
  output$selected_var <- renderText({
    paste("You have selected", input$sector_selector)
  })
  output$min_max <- renderText({
    paste("You have chosen a range that goes from", input$inSlider[1], "to", input$inSlider[2])
  })
  output$control_date_range <- renderText({
    paste("You have chosen a range that goes from", input$daterange[1], "to", input$daterange[2])
  })
  output$plot1 <- renderPlot({
    plot_value_over_date(input$sector_selector,
                         val_min =input$inSlider[1],
                         val_max = input$inSlider[2])
  })

  output$slider <- renderUI({
    sliderInput("inSlider", "Filter value for Graph", min=find_min_max(input$sector_selector)[1],
                max=find_min_max(input$sector_selector)[2],
                value=c(find_min_max(input$sector_selector)[1],find_min_max(input$sector_selector)[2]))
  })
  
  output$date_range <- renderUI({
    dateRangeInput("daterange", "Filter Dates for Graph:",
                   start = date_min_max(input$sector_selector)[1],
                   end = date_min_max(input$sector_selector)[2])
  })
  
  
  
}



shinyApp(ui=ui, server = server)