# Load libraries needed
library(shiny)
library(purrr)
library(rootSolve)
library(ggplot2)
library(plotly)
source("helperfunctions.r")
library(rootSolve)
library(car)
library(rgl)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Shiny App"),

  sidebarLayout(
    sidebarPanel(
      fileInput("dataset", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

      selectInput(
        "x_input",
        label = h5("X Variable"),
        ""
      ),

      selectInput(
        "y_input",
        label = h5("Y Variable"),
        ""
      ),

      selectInput(
        "z_input",
        label = h5("Z Variable"),
        ""
      ),

      checkboxInput(
        "Linear_Regression",
        label = "Linear Regression"
      ),

      sliderInput("radians",
                  "Choose how much to rotate the axis:",
                  min = 0,
                  max = round(pi/2,4),
                  value = 0,
                  step=0.001),
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("multivariable_plot", click ="plot_click"),
      textOutput("drop_one"),
      tableOutput("data"),
      plotOutput("rotated_graph"),
      textOutput("theta_value"),
      rglwidgetOutput("threeD"),
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  data <- reactive({
    req(input$dataset)

    ext <- tools::file_ext(input$dataset$name)
    switch(ext,
           csv = vroom::vroom(input$dataset$datapath, delim = ","),
           validate("Invalid file; Please upload a .csv file")
    )
  })


  output$checkbox <- renderUI({
    updateSelectizeInput(inputId = "select_var",
                         label = "Select variables",
                         choices = names(data()))
  })

  observe({
    updateSelectInput(
      session,
      "y_input",
      choices=names(data()))

    updateSelectInput(
      session,
      "x_input",
      choices=names(data()))

    updateSelectInput(
      session,
      "z_input",
      choices=names(data()))

  })

  output$multivariable_plot <- renderPlot({
    if (is.null(data())==FALSE) {
      if (input$Linear_Regression){
        x = unlist(data()[, colnames(data()) %in% input$x_input])
        y = unlist(data()[, colnames(data()) %in% input$y_input])

        df_lm = data.frame(x, y)
        linear_model = lm(y~x, data=df_lm)
        intercept = round(linear_model$coefficients[1],4)
        slope = round(linear_model$coefficients[2],4)


        annotations <- data.frame(
          xpos = c(-Inf),
          ypos =  c(Inf),
          annotateText = c(paste(input$y_input, "=", slope, "*", input$x_input, "+", intercept)),
          hjustvar = c(-1) ,
          vjustvar = c(1))


        g = ggplot(data = data(), aes_string(x=input$x_input, y=input$y_input), colors()) +
          geom_point() +
          labs(title= paste(input$y_input, "vs.", input$x_input), y=input$y_input, x=input$x_input) +
          geom_smooth(method = "lm") + geom_text(data=annotations,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText))
        g
      }
      else{
        req(input$x_input, input$y_input)
        g = ggplot(data = data(), aes_string(x=input$x_input, y=input$y_input)) +
          geom_point() +
          labs(title= paste(input$y_input, "vs.", input$x_input), y=input$y_input, x=input$x_input)
        g
      }
    }

  })


  output$drop_one <- renderText({
    if (is.null(data())==FALSE) {
      clicked_point = nearPoints(data(), input$plot_click, xvar = input$x_input, yvar = input$y_input)
      x_val = as.numeric(clicked_point[,input$x_input])
      y_val = as.numeric(clicked_point[,input$y_input])
      x = data()[, colnames(data()) %in% input$x_input]
      y = data()[, colnames(data()) %in% input$y_input]
      x = x[x != x_val]
      y = y[y != y_val]
      paste("Drop-one Correlation:", round(cor(x,y),4))
    }
  })


  output$rotated_graph <- renderPlot({
    if (input$Linear_Regression){
      x = unlist(data()[, colnames(data()) %in% input$x_input])
      y = unlist(data()[, colnames(data()) %in% input$y_input])
      transformed_values = my.tilde(x, y, input$radians)
      xt1 = transformed_values$x1t
      yt1 = transformed_values$x2t
      new_df = data.frame(xt=xt1, yt=yt1)
      correlation = round(cor(xt1, yt1),4)

      df_lm = data.frame(xt1, yt1)
      linear_model = lm(yt1~xt1, data=df_lm)
      intercept = round(linear_model$coefficients[1],4)
      slope = round(linear_model$coefficients[2],4)

      annotations <- data.frame(
        xpos = c(-Inf, Inf),
        ypos =  c(Inf, -Inf),
        annotateText = c(paste(input$y_input, "' =", slope, "*", input$x_input, "'", " +", intercept), paste("correlation:", correlation)),
        hjustvar = c(-1, 1.25) ,
        vjustvar = c(1, -1))


      g = ggplot(data = new_df, aes_string(x=colnames(new_df)[1], y=colnames(new_df)[2])) +
        geom_point() +
        ylab(paste(input$y_input, "'")) + xlab(paste(input$x_input, "'"))  +
        ggtitle(paste(input$y_input, "'  vs. ", input$x_input, "'")) +
        geom_text(data=annotations,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText)) +
        geom_smooth(method = "lm")
      g
    }

    else{
      x = unlist(data()[, colnames(data()) %in% input$x_input])
      y = unlist(data()[, colnames(data()) %in% input$y_input])
      transformed_values = my.tilde(x, y, input$radians)
      xt1 = transformed_values$x1t
      yt1 = transformed_values$x2t
      new_df = data.frame(xt=xt1, yt=yt1)
      correlation = round(cor(xt1, yt1),4)

      annotations <- data.frame(
        xpos = c(Inf),
        ypos =  c(-Inf),
        annotateText = c(paste("correlation:", correlation)),
        hjustvar = c(1.25) ,
        vjustvar = c(-1))


      g = ggplot(data = new_df, aes_string(x=colnames(new_df)[1], y=colnames(new_df)[2])) +
        geom_point() +
        ylab(paste(input$y_input, "'")) + xlab(paste(input$x_input, "'"))  +
        ggtitle(paste(input$y_input, "'  vs. ", input$x_input, "'")) +
        geom_text(data=annotations,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText))
      g
    }
  })

  output$theta_value <- renderText({
    x = as.vector(data()[, colnames(data()) %in% input$x_input])
    y = as.vector(data()[, colnames(data()) %in% input$y_input])
    transformed_values = my.tilde(x, y, input$radians)
    xt1 = as.vector(transformed_values$x1t)
    yt1 = as.vector(transformed_values$x2t)

    new_df = data.frame(xt=xt1, yt=yt1)
    correlation = round(cor(xt1, yt1),4)
    s11 = var(x)
    s22 = var(y)
    s12 = cov(x, y)

    myfun=function(x) as.numeric(s12)*(cos(x)^2 - sin(x)^2) - (as.numeric(s11) - as.numeric(s22))*(cos(x)*sin(x))
    root = round(rootSolve::uniroot.all(myfun, interval = c(0, pi/2)), 4)
    paste("The first quadrant value of theta to make the covariance term 0 is: ", root)
  })


  output$threeD <- renderRglwidget({
    if (is.null(data())==FALSE) {
      x = unlist(data()[, colnames(data()) %in% input$x_input])
      y = unlist(data()[, colnames(data()) %in% input$y_input])
      z = unlist(data()[, colnames(data()) %in% input$z_input])
      df = data.frame(x, y, z)

      rgl.open(useNULL=T)
      scatter3d(x=df$x, y=df$y, z=df$z, surface=TRUE, xlab = input$x_input, ylab = input$y_input, zlab = input$z_input)
      rglwidget()
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
