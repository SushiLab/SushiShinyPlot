#' Package for plotting through the construction of Shiny Apps
#'
#' The package \pkg{SushiShinyPlot} provides pre-built Shiny Apps for the most common plots based on \pkg{ggplot2} and \pkg{plotly}.
#' 
#' To see the preferable citation of the package, type citation("SushiShinyPlot").
#'@docType package
#'@name SushiShinyPlot
#'@author Guillem Salazar <guillems@@ethz.ch>

NULL


#' Shiny App for a Scatterplot.
#'
#' @param dat Data frame.
#' @keywords SushiShinyPlot
#' @return Shiny App.
#' @export
#'@author Guillem Salazar <guillems@@ethz.ch>
#' @examples
#' shinyScatter(newdat)

shinyScatter<-function(dat){
  require(shiny)
  require(plotly)
  
  # Get the variable names
  nms <- colnames(dat)
  nms.numeric<-nms[sapply(dat,is.numeric)]
  nms.factor<-c(nms[sapply(dat,is.factor)],nms[sapply(dat,is.character)])
  
  # Generate the UI
  ui <- fluidPage(
    
    headerPanel("Scatterplot Explorer"),
    sidebarPanel(
      selectInput('x', 'X variable', choices = nms.numeric, selected = nms.numeric[1]),
      selectInput('y', 'Y variable', choices = nms.numeric, selected = nms.numeric[2]),
      selectInput('color', 'Color by variable', choices = c("None",nms), selected = "None"),
      selectInput('smooth', 'Smoothing', choices = c("None","lm","loess"), selected = "None"),
      selectInput('facet_row', 'Facet Row', c(None = '.', nms.factor), selected = "None"),
      selectInput('facet_col', 'Facet Column', c(None = '.', nms.factor)),
      sliderInput('plotHeight', 'Height of plot (in pixels)',min = 100, max = 2000, value = 600)
    ),
    mainPanel(
      plotlyOutput('trendPlot', height = "900px")
    )
  )
  
  # Generate the SERVER
  server <- function(input, output) {
    
    #add reactive data information. Dataset = built in dat data
    dataset <- reactive({
      dat
    })
    
    output$trendPlot <- renderPlotly({
      
      # build graph with ggplot syntax
      if (input$color=="None") {
        p <- ggplot(dataset(), aes_string(x = input$x, y = input$y)) + 
          geom_point()}
      else {
        p <- ggplot(dataset(), aes_string(x = input$x, y = input$y, color = input$color)) + 
          geom_point()}
      if (input$smooth!="None") p<-p + geom_smooth(method=input$smooth)
      
      # if at least one facet column/row is specified, add it
      facets <- paste(input$facet_row, '~', input$facet_col)
      if (facets != '. ~ .') p <- p + facet_grid(facets)
      
      ggplotly(p,height = input$plotHeight, autosize=TRUE)
      
    })
    
  }
  
  # Launch Shiny App
  shinyApp(ui, server)
}

#' Shiny App for a Violinplot.
#'
#' @param dat Data frame.
#' @keywords SushiShinyPlot
#' @return Shiny App.
#' @export
#'@author Guillem Salazar <guillems@@ethz.ch>
#' @examples
#' shinyViolin(newdat)

shinyViolin<-function(dat){
  require(shiny)
  require(plotly)
  
  # Get the variable names
  nms <- colnames(dat)
  nms.numeric<-nms[sapply(dat,is.numeric)]
  nms.factor<-c(nms[sapply(dat,is.factor)],nms[sapply(dat,is.character)])
  
  # Generate the UI
  ui <- fluidPage(
    
    headerPanel("Violinplot Explorer"),
    sidebarPanel(
      selectInput('x', 'X variable', choices = nms.factor, selected = nms.factor[1]),
      selectInput('y', 'Y variable', choices = nms.numeric, selected = nms.numeric[2]),
      selectInput('color', 'Color by variable', choices = c("None",nms), selected = "None"),
      
      selectInput('facet_row', 'Facet Row', c(None = '.', nms.factor), selected = "None"),
      selectInput('facet_col', 'Facet Column', c(None = '.', nms.factor)),
      sliderInput('plotHeight', 'Height of plot (in pixels)', 
                  min = 100, max = 2000, value = 600)
    ),
    mainPanel(
      plotlyOutput('trendPlot', height = "900px")
    )
  )
  
  # Generate the SERVER
  server <- function(input, output) {
    
    #add reactive data information. Dataset = built in dat data
    dataset <- reactive({
      dat
    })
    
    output$trendPlot <- renderPlotly({
      
      # build graph with ggplot syntax
      if (input$color=="None") {p <- ggplot(dataset(), aes_string(x = input$x, y = input$y)) + 
        geom_violin(scale = "width",draw_quantiles = 0.5)} else
        {p <- ggplot(dataset(), aes_string(x = input$x, y = input$y, fill = input$color)) + 
          geom_violin(scale = "width",draw_quantiles = 0.5,position = "dodge")}
      
      # if at least one facet column/row is specified, add it
      facets <- paste(input$facet_row, '~', input$facet_col)
      if (facets != '. ~ .') p <- p + facet_grid(facets)
      
      ggplotly(p,height = input$plotHeight, autosize=TRUE)
      
    })
    
  }

  # Launch Shiny App
  shinyApp(ui, server)
}

#' Shiny App for an Histogram.
#'
#' @param dat Data frame.
#' @keywords SushiShinyPlot
#' @return Shiny App.
#' @export
#'@author Guillem Salazar <guillems@@ethz.ch>
#' @examples
#' shinyHistogram(newdat)

shinyHistogram<-function(dat){
  require(shiny)
  require(plotly)
  
  # Get the variable names
  nms <- colnames(dat)
  nms.numeric<-nms[sapply(dat,is.numeric)]
  nms.factor<-c(nms[sapply(dat,is.factor)],nms[sapply(dat,is.character)])
  
  # Generate the UI
  ui <- fluidPage(
    
    headerPanel("Histogram Explorer"),
    sidebarPanel(
      selectInput('x', 'X variable', choices = nms.numeric, selected = nms.numeric[1]),
      sliderInput('bins', 'Number of bins',min = 10, max = 1000, value = 30),
      selectInput('color', 'Color by variable', choices = c("None",nms), selected = "None"),
      selectInput('facet_row', 'Facet Row', c(None = '.', nms.factor), selected = "None"),
      selectInput('facet_col', 'Facet Column', c(None = '.', nms.factor)),
      sliderInput('plotHeight', 'Height of plot (in pixels)',min = 100, max = 2000, value = 600),
      sliderInput('alpha', 'Transparency',min = 100, max = 1000, value = 700)
    ),
    mainPanel(
      plotlyOutput('trendPlot', height = "900px")
    )
  )
  
  # Generate the SERVER
  server <- function(input, output) {
    
    #add reactive data information. Dataset = built in dat data
    dataset <- reactive({
      dat
    })
    
    output$trendPlot <- renderPlotly({
      
      # build graph with ggplot syntax
      if (input$color=="None") {
        p <- ggplot(dataset(), aes_string(x = input$x)) + 
          geom_histogram(alpha=input$alpha/1000,bins=input$bins)}
      else {
        p <- ggplot(dataset(), aes_string(x = input$x,fill = input$color)) + 
          geom_histogram(alpha=input$alpha/1000,bins=input$bins)}

      # if at least one facet column/row is specified, add it
      facets <- paste(input$facet_row, '~', input$facet_col)
      if (facets != '. ~ .') p <- p + facet_grid(facets)
      
      ggplotly(p,height = input$plotHeight, autosize=TRUE)
      
    })
    
  }
  
  # Launch Shiny App
  shinyApp(ui, server)
}

#' Shiny App for a Densityplot.
#'
#' @param dat Data frame.
#' @keywords SushiShinyPlot
#' @return Shiny App.
#' @export
#'@author Guillem Salazar <guillems@@ethz.ch>
#' @examples
#' shinyDensity(newdat)

shinyDensity<-function(dat){
  require(shiny)
  require(plotly)
  
  # Get the variable names
  nms <- colnames(dat)
  nms.numeric<-nms[sapply(dat,is.numeric)]
  nms.factor<-c(nms[sapply(dat,is.factor)],nms[sapply(dat,is.character)])
  
  # Generate the UI
  ui <- fluidPage(
    
    headerPanel("Density Explorer"),
    sidebarPanel(
      selectInput('x', 'X variable', choices = nms.numeric, selected = nms.numeric[1]),
      sliderInput('bandwidth', 'Bandwith',min = 0.1, max = 10, value = 1),
      selectInput('color', 'Color by variable', choices = c("None",nms), selected = "None"),
      selectInput('facet_row', 'Facet Row', c(None = '.', nms.factor), selected = "None"),
      selectInput('facet_col', 'Facet Column', c(None = '.', nms.factor)),
      sliderInput('plotHeight', 'Height of plot (in pixels)',min = 100, max = 2000, value = 600),
      sliderInput('alpha', 'Transparency',min = 100, max = 1000, value = 700)
    ),
    mainPanel(
      plotlyOutput('trendPlot', height = "900px")
    )
  )
  
  # Generate the SERVER
  server <- function(input, output) {
    
    #add reactive data information. Dataset = built in dat data
    dataset <- reactive({
      dat
    })
    
    output$trendPlot <- renderPlotly({
      
      # build graph with ggplot syntax
      if (input$color=="None") {
        p <- ggplot(dataset(), aes_string(x = input$x)) + 
          geom_density(alpha=input$alpha/1000,adjust=input$bandwidth,fill="gray")}
      else {
        p <- ggplot(dataset(), aes_string(x = input$x,fill = input$color)) + 
          geom_density(alpha=input$alpha/1000,adjust=input$bandwidth)}
      
      # if at least one facet column/row is specified, add it
      facets <- paste(input$facet_row, '~', input$facet_col)
      if (facets != '. ~ .') p <- p + facet_grid(facets)
      
      ggplotly(p,height = input$plotHeight, autosize=TRUE)
      
    })
    
  }
  
  # Launch Shiny App
  shinyApp(ui, server)
}

#' Shiny App for an Barplot.
#'
#' @param dat Data frame.
#' @keywords SushiShinyPlot
#' @return Shiny App.
#' @export
#'@author Guillem Salazar <guillems@@ethz.ch>
#' @examples
#' shinyBar(newdat)

shinyBar<-function(dat){
  require(shiny)
  require(plotly)
  
  # Get the variable names
  nms <- colnames(dat)
  nms.numeric<-nms[sapply(dat,is.numeric)]
  nms.factor<-c(nms[sapply(dat,is.factor)],nms[sapply(dat,is.character)])
  
  # Generate the UI
  ui <- fluidPage(
    
    headerPanel("Barplot Explorer"),
    sidebarPanel(
      selectInput('x', 'X variable', choices = nms.factor, selected = nms.factor[1]),
      selectInput('y', 'Y variable', choices = nms.numeric, selected = nms.numeric[2]),
      selectInput('color', 'Color by variable', choices = c("None",nms), selected = "None"),
      selectInput('pos', 'Bar position', choices = c("Stacked","Side-by-side"), selected = "Stacked"),
      selectInput('flip', 'Flip axis?', choices = c("No","Yes"), selected = "No"),
      selectInput('facet_row', 'Facet Row', c(None = '.', nms.factor), selected = "None"),
      selectInput('facet_col', 'Facet Column', c(None = '.', nms.factor)),
      sliderInput('plotHeight', 'Height of plot (in pixels)',min = 100, max = 2000, value = 600)
    ),
    mainPanel(
      plotlyOutput('trendPlot', height = "900px")
    )
  )
  
  # Generate the SERVER
  server <- function(input, output) {
    
    #add reactive data information. Dataset = built in dat data
    dataset <- reactive({
      dat
    })
    
    output$trendPlot <- renderPlotly({
      
      # build graph with ggplot syntax
      if (input$color=="None") {
        p <- ggplot(dataset(), aes_string(x = input$x, y = input$y)) + 
          geom_bar(stat = "identity")}
      else
        {
          p <- ggplot(dataset(), aes_string(x = input$x, y = input$y, fill = input$color)) + 
          geom_bar(stat="identity")}
      
      if (input$pos=="Side-by-side" & input$color=="None") {
        p <- ggplot(dataset(), aes_string(x = input$x, y = input$y)) + 
          geom_bar(stat="identity",position="dodge")
      }
      else if (input$pos=="Side-by-side" & input$color!="None"){
        p <- ggplot(dataset(), aes_string(x = input$x, y = input$y, fill = input$color)) + 
          geom_bar(stat="identity",position="dodge")
      }
      
      if (input$flip=="Yes") {
        p <- p + coord_flip()
      }
      
      # if at least one facet column/row is specified, add it
      facets <- paste(input$facet_row, '~', input$facet_col)
      if (facets != '. ~ .') p <- p + facet_grid(facets)
      
      ggplotly(p,height = input$plotHeight, autosize=TRUE)
      
    })
    
  }
  
  # Launch Shiny App
  shinyApp(ui, server)
}

