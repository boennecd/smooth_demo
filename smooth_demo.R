#####
# Code to run demo
# width and height are the width and height of the plot
smooth_demo <- function(x, y, width = 500, height = 400){
  if(!require(shiny, quietly = T))
    stop("smooth_demo requires that you have installed the package shiny")
  
  # Get initial objects name of passed objects
  xlab <- deparse(substitute(x))
  ylab <- deparse(substitute(y))
  
  # Will be used for x-range slider
  x_vals <-  signif(x, digits = 3)
  
  # Define server logic
  server <- function(input, output) {
    
    # Compute the smooth
    smoother_xy <- sim_input <- reactive({
      if(input$smoother == "smooth.spline"){
        fit <- smooth.spline(x, y, spar = input$smooth.spline_spar)
        x <- fit$x
        y <- fit$y
        
      } else if(input$smoother == "ksmooth"){
        fit <- ksmooth(x, y, input$ksmooth_kernel, 
                       exp(input$ksmooth_log_bandwidth))
        x <- fit$x
        y <- fit$y
        
      } else if(input$smoother == "loess"){
        fit <- loess(y ~ x, enp.target = exp(input$loess_enp.target), 
                     degree = input$loess_degree)
        x <- seq(min(x), max(x), length.out = 1000)
        y <- predict(fit, newdata = x)
        
      }
      list(x = x, y = y)
    })
    
    # Make plot
    output$out_plot <- renderPlot({
      par(cex = .6, pch = 16, mar = c(5, 4, 1, 1))
      args <- list(xlab = xlab, ylab = ylab, frame = F, 
                   col = rgb(0, 0, 0, input$col_opacity), 
                   xlim = input$xlim)
      
      # Create plot
      if(input$col_opacity == 0){
        do.call(plot, c(list(range(x), range(y), type = "n"), args))
      } else
        do.call(plot, c(list(x, y), args))
      
      # Add smooth
      x_y <- smoother_xy()
      lines(x_y$x, x_y$y, col = "red", lwd = 1.5)
      
      # Add rug/color density
      if(input$dens_opt == "Rug"){
        set.seed(6390753)
        rug(jitter(x), side = 1, col = rgb(0, 0, 0, .5))
        rug(jitter(y), side = 2, col = rgb(0, 0, 0, .5))
        
      } else if(input$dens_opt == "Color density"){
        col_dens(x, 1)
        col_dens(y, 2)
      }
    }, res = 144, width = width, height = height)
  }
  
  # Define UI for application
  ui <- fluidPage(
    
    # Application title
    titlePanel("Smoothing demonstration"),
    
    fluidRow(
      
      # The column for output
      column(12 - 3, 
             plotOutput("out_plot"), 
             style = paste0("max-width: ", width * 1.15 ,"px;", 
                            "min-width: ", width ,"px;")),
      
      # The column for input
      column(
        3, style = "max-width: 250px;", 
             
        # Smoother input
        wellPanel(
          style = "height: 350px;", 
          
          radioButtons("smoother",
                       "Choose smoother",
                       choices = c("smooth.spline", "loess", "ksmooth"),
                       selected = "smooth.spline"),
          
          # Conditional panel if smooth.spline is selected
          conditionalPanel(
            "input.smoother == 'smooth.spline'",
            
            sliderInput("smooth.spline_spar",
                        "Select spar argument",
                        min = 0,
                        max = 1,
                        step = .01,
                        value = .5, 
                        ticks = F)
            ),
          
          # Conditional panel if ksmooth is selected
          conditionalPanel(
            "input.smoother == 'ksmooth'",
            
            sliderInput("ksmooth_log_bandwidth",
                        "Select log bandwidth",
                        min = -4,
                        max = 6,
                        step = .1,
                        value = -.5, 
                        ticks = F),
            
            radioButtons("ksmooth_kernel",
                         "Choose kernel",
                         choices = c("normal", "box"),
                         selected = "normal")
          ),
          
          # Conditional panel if loess is selected
          conditionalPanel(
            "input.smoother == 'loess'",
            
            sliderInput("loess_enp.target",
                        "Select log enp.target",
                        min = 0,
                        max = 4,
                        step = .1,
                        value = 3, 
                        ticks = FALSE),
            
            sliderInput("loess_degree",
                        "Select degree",
                        min = 0,
                        max = 2,
                        step = 1,
                        value = 2,
                        ticks = F)
          )),
          
          # Plot settings
          wellPanel(
            sliderInput("col_opacity",
                        "Select opacity",
                        min = 0,
                        max = 1,
                        step = .01,
                        value = .25,
                        ticks = F),
            
            selectInput("dens_opt",
                        "Choose density illustration",
                        choices = c("Color density", "Rug", "None"),
                        selected = "smooth.spline"),
            
            sliderInput("xlim", "Select x range:",
                        min = min(x_vals), max = max(x_vals), value = range(x_vals), 
                        ticks = F, round  = T)
          ))
      ))
  
  # Run the application 
  shinyApp(ui = ui, server = server) 
}

#####
# Rug like function to illustrate distribution on axis
# Faster than the rug function for larger data sets
col_dens <- function(vals, side = 1){
  hst <- hist(vals, plot = F, breaks = 100)
  hst$col_scale <- .8 * hst$density / max(hst$density)
  
  cols <- rgb(0, 0, 0, hst$col_scale)
  if(side == 1){
    y0 <- par("usr")[3]
    y1 <- y0 + .03 * diff(par("usr")[3:4])
    
    rect(head(hst$breaks, -1), rep(y0, length(hst$breaks) - 1),
         hst$breaks[-1], rep(y1, length(hst$breaks) - 1), 
         col = cols, border = NA) 
    
  } else{
    x0 <- par("usr")[1]
    x1 <- x0 +  .015 * diff(par("usr")[1:2])
    
    rect(rep(x0, length(hst$breaks) - 1), head(hst$breaks, -1),
         rep(x1, length(hst$breaks) - 1), hst$breaks[-1], 
         col = cols, border = NA) 
  }
}
