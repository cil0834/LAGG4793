# Load libraries needed
library(shiny)
library(ggplot2)
library(ggforce)
source("helper_functions.r")
library(DT)
library(EnvStats)

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
        "x_i",
        label = h5("x_i"),
        ""
      ),

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
        "boxcox",
        label = h5("BoxCox Variable"),
        ""
      ),

      sliderInput("alpha",
                  "Alpha:",
                  min = 0,
                  max = 1,
                  value = 0.5,
                  step=0.01),

      sliderInput("outlier",
                  "outlier_alpha:",
                  min = 0,
                  max = 0.05,
                  value = 0.025,
                  step=0.001),

      sliderInput("lambda",
                  "BoxCox Lambda:",
                  min = -2,
                  max = 2,
                  value = 0,
                  step=0.01),
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("barplot"),
      plotOutput("qq_plot"),
      DT::dataTableOutput("qq_table"),
      plotOutput("multivariable_plot"),
      plotOutput("chi_plot", click ="plot_click"),
      plotOutput("chi_plot_removed"),
      DT::dataTableOutput("chisquare_df"),
      DT::dataTableOutput("z_table"),
      plotOutput("box_cox"),
      plotOutput("trans_plot"),
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

  observe({
    quant = c()
    col_names = names(data())
    for (i in 1:length(col_names)){
      x = unlist(data()[, colnames(data()) %in% col_names[i]])
      test_variable = class(x[1])
      if (test_variable == "integer" || test_variable == "numeric"){
        quant = c(quant, col_names[i])
      }
    }
    updateSelectInput(
      session,
      "x_i",
      choices=quant)
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
      "boxcox",
      choices=names(data()))
  })


  output$barplot <- renderPlot({
    if (is.null(data())==FALSE) {
      means = c()
      sd = c()
      result = c()
      col_names = colnames(data())
      for(i in 1:length(col_names)){
        x = unlist(data()[, colnames(data()) %in% col_names[i]])
        means = c(means, mean(x))
        sd = c(sd, sd(x))

        df = data.frame(x)
        names(df)[names(df) == "x"] <- col_names[i]
        out = proportion_norm(df)
        r_1 = out$result_1
        r_2 = out$result_2
        if(r_1 == "passed" && r_2 == "passed"){
          results = "green"
        }
        else if(r_1 == "failed" && r_2 == "passed"){
          results = "yellow"
        }
        else if(r_1 == "passed" && r_2 == "failed"){
          results = "orange"
        }
        else{
          results = "red"
        }

        result = c(result, results)
      }
      df <- data.frame("Group" = col_names, "mean" = means, "sd" = sd, "results" = result)

      ggplot(df,aes(x=Group) ) +
        geom_boxplot(fill = df$results ,aes(lower=mean-sd,upper=mean+sd,middle=mean,ymin=mean-3*sd,ymax=mean+3*sd),stat="identity") +
        labs(title = "Boxplot")
    }
  })

  output$chisquare_df <- DT::renderDataTable({
    req(input$plot_click)
    d = c()
    co = solve(cov(data()))
    m = colMeans(data())
    x = unlist(data()[, colnames(data()) %in% input$x_i])
    n = length(x)
    p = dim(data())[2]
    for(i in 1:n){
      vec = as.matrix((data()[i,]-m))
      vec = vec%*%co%*%t(vec)
      d = c(d, vec)
    }
    d = sort(d)
    qs = qnorm((1:n- .5)/n, 0, 1)
    df = data.frame("d" = d, "qs" = qs)
    point = nearPoints(df, input$plot_click, xvar = "qs", yvar = "d")

    qs_point = as.numeric(unlist(point))[2]
    d_point = as.numeric(unlist(point))[1]

    d_point = round(d_point, 4)

    mat = as.matrix(df)
    point = round(d_point,4)
    a = round(c(mat[,1]),4)

    row = match(point, a)
    row

    mat = round(mat[-row,],4)
    df = as.data.frame(mat)
  })

  output$qq_plot <- renderPlot({
    if (is.null(data())==FALSE) {
      x = unlist(data()[, colnames(data()) %in% input$x_i])
      qq_info = qq_measurements(x)
      qs = qq_info$qs
      observations = qq_info$observations
      rq = round(r_q(observations, qs),4)

      m = mean(qs)
      std_1 = sd(x) + mean(x)
      std_2 = 2*sd(x) + mean(x)

      ms = c(m, m)
      p_value = round(shapiro.test(x)$p.value,4)
      p_value
      df_1 = data.frame("qs" = qs, "observations" = observations)
      ggplot(data = df_1, aes(x = qs, y = observations)) + geom_point() + geom_smooth(method='lm', formula= y~x) +
        labs(title = paste(input$x_i, " QQ-Plot")) + geom_text(x=m, y=std_2, label=paste("P-value", p_value)) +
        geom_text(x=m, y= std_1, label=paste("R", rq))
    }
  })

  output$qq_table = DT::renderDataTable({
    col_names = colnames(data())
    rqs = c()
    for(i in 1:length(col_names)){
      x = unlist(data()[, colnames(data()) %in% col_names[i]])
      qq = qq_measurements(x)
      q = qq$qs
      ob = qq$observations
      rq = round(r_q(ob, q),4)

      rqs = c(rqs, rq)
    }

    df = data.frame("variables" = col_names, "rqs" = rqs)
    df
  })

  output$chi_plot <-renderPlot({
    if (is.null(data())==FALSE) {
      d = c()
      co = solve(cov(data()))
      m = colMeans(data())
      x = unlist(data()[, colnames(data()) %in% input$x_i])
      n = length(x)
      p = dim(data())[2]
      for(i in 1:n){
        vec = as.matrix((data()[i,]-m))
        vec = vec%*%co%*%t(vec)
        d = c(d, vec)
      }
      d = sort(d)
      qs = qnorm((1:n- .5)/n, 0, 1)
      outlier_chi = qchisq(input$outlier, p, lower.tail = FALSE)




      df = data.frame("d" = d, "qs" = qs)
      g = ggplot(data = df, aes(x = qs, y = d)) +
        geom_point(aes(x =qs , y =d , colour = outlier_chi < d)) +
        geom_smooth(method='lm', formula= y~x) + labs(title="Chi-sq Plot") +
        scale_colour_discrete("Outlier")
      g
      }

  })

  output$chi_plot_removed <-renderPlot({
    req(input$plot_click)
    d = c()
    co = solve(cov(data()))
    m = colMeans(data())
    x = unlist(data()[, colnames(data()) %in% input$x_i])
    n = length(x)
    p = dim(data())[2]
    for(i in 1:n){
      vec = as.matrix((data()[i,]-m))
      vec = vec%*%co%*%t(vec)
      d = c(d, vec)
    }
    d = sort(d)
    qs = qnorm((1:n- .5)/n, 0, 1)
    df = data.frame("d" = d, "qs" = qs)
    point = nearPoints(df, input$plot_click, xvar = "qs", yvar = "d")

    qs_point = as.numeric(unlist(point))[2]
    d_point = as.numeric(unlist(point))[1]

    d_point = round(d_point, 4)

    mat = as.matrix(df)
    point = round(d_point,4)
    a = round(c(mat[,1]),4)

    row = match(point, a)
    row

    mat = mat[-row,]
    df = as.data.frame(mat)
    names(df) = c("d", "qs")

    g = ggplot(data = df, aes(x = qs, y = d)) +
      geom_point() +
      geom_smooth(method='lm', formula= y~x) + labs(title="Chi-sq Plot Removed")
    g

  })

  output$z_table = DT::renderDataTable({
    if (is.null(data())==FALSE) {
      df = z_mat(data())
      df
    }
  })

  output$box_cox <- renderPlot({
    x = unlist(data()[, colnames(data()) %in% input$boxcox])
    box = boxcox(x, lambda = seq(-2, 2, by = 0.01))
    lam = box$lambda
    obj = box$objective

    inflection_point_y = max(obj)
    index = match(inflection_point_y, obj)
    inflextion_point_x = lam[index]

    df = data.frame("lam" = lam, "obj" = obj)

    ggplot(df, aes(x = lam, y = obj)) +
      geom_point(colour = "blue") +
      labs(title = "l(\u03BB) vs. \u03BB", y = "l(\u03BB)", x = "\u03BB") +
      geom_text(x=inflextion_point_x, y=(inflection_point_y - 0.02), label=paste("Critical \u03BB:", round(inflextion_point_x,4)))
  })

  output$trans_plot <- renderPlot({
    if (is.null(data())==FALSE) {
      x = unlist(data()[, colnames(data()) %in% input$boxcox])
      x = box_cox(x, lambda = input$lambda)
      qq_info = qq_measurements(x)
      qs = qq_info$qs
      observations = qq_info$observations
      rq = round(r_q(observations, qs),4)

      m = mean(qs)
      std_1 = sd(x) + mean(x)
      std_2 = 2*sd(x) + mean(x)

      ms = c(m, m)
      p_value = round(shapiro.test(x)$p.value,4)
      df_1 = data.frame("qs" = qs, "observations" = observations)
      ggplot(data = df_1, aes(x = qs, y = observations)) + geom_point() + geom_smooth(method='lm', formula= y~x) +
        labs(title = paste(input$boxcox, " QQ-Plot BoxCox")) + geom_text(x=m, y=std_2, label=paste("P-value", p_value)) +
        geom_text(x=m, y= std_1, label=paste("R", rq))
    }
  })



  output$multivariable_plot <- renderPlot({
    if (is.null(data())==FALSE) {
        x = unlist(data()[, colnames(data()) %in% input$x_input])
        y = unlist(data()[, colnames(data()) %in% input$y_input])
        dt = data.frame("x"=x, "y"=y)
        m = colMeans(dt)
        co_i = solve(cov(dt))
        n = dim(dt)[1]
        p = dim(dt)[2]
        m_vec = as.matrix(dt)
        chis = c()

        for(i in 1:n){
          chi = t(m_vec[i,]-m)%*%co_i%*%(m_vec[i,] - m)
          chis = round(c(chis, chi),2)
        }

        dt$chis = chis

        chi_sq = qchisq(input$alpha, 2, lower.tail = FALSE)
        mat = matrix(c(x, y), byrow = FALSE, ncol = 2)
        me = colMeans(mat)
        co = cov(mat)
        e_val = eigen(co)$values
        e_vec = eigen(co)$vectors
        es = sqrt(e_val)
        major = es[1]*sqrt(chi_sq)
        minor = es[2]*sqrt(chi_sq)

        eigens = eigen(co)
        c = eigens$vectors[1,1]
        s = eigens$vectors[2,1]
        angle = atan(s/c)

        ggplot(data = data(), aes_string(x=input$x_input, y=input$y_input), colors()) +
        geom_point(aes(x = x, y = y), colour = "blue") +
        geom_point(aes(x = x, y = y, colour = chis < chi_sq, colour = "red")) +
        geom_ellipse(aes(x0 = me[1], y0 = me[2], a = major, b = minor, angle = angle)) +
        labs(title= paste(input$y_input, "vs.", input$x_input, "Ellipse"), y=input$y_input, x=input$x_input) +
        scale_colour_discrete("Inside Ellipse")
    }

  })
}

# Run the application
shinyApp(ui = ui, server = server)
