
explore <- function(dt){
	require(shiny)
	require(ggplot2)
	require(gridExtra)
	require(data.table)

	shinyApp(
		ui = fluidPage(
		  title = "Explore DataFrame Web App",
		  fluidRow(
		    column(3, wellPanel(

				h4("Select X,Y"),

				uiOutput("x_var_select"),

				uiOutput("y_var_select"),

				uiOutput("pct_slider"),

				h4("Options"),
				
				#transformation inputs		      
				conditionalPanel(
				  condition = "input.margins != 'y'",
				  selectInput("xtrans", "X axis transformation", 
				  	c("none", "log", "square", "cube", "log10", "sqrt"))
				),

				conditionalPanel(
				  condition = "input.margins != 'x'",
				  selectInput("ytrans", "Y axis transformation", 
				  	c("none", "log", "square", "cube", "log10", "sqrt"))
				),

				numericInput("xcap", "Cap Max X-value", NULL),
				numericInput("ycap", "Cap Max Y-value", NULL),

				sliderInput("scatdim",
		            "Ratio of Scatter Height", 1, 10, 5, 1),

				sliderInput("scatalpha",
		            "Scatter Alpha", 0, 1, 0.5, 0.05),

				checkboxInput("lmcheck", "Show LM Fit", value = TRUE),

				checkboxInput("loesscheck", "Show Loess Fit", value = TRUE),

				checkboxInput("densitycheck", "Show Density Fits", value = TRUE),

				verbatimTextOutput("xysummary")
			)), #close settings wellpanel
		    
		    column(9,
		      plotOutput("jointplot", height = "700px"),
		      plotOutput("xhist"),
		      plotOutput("yhist"),
		      plotOutput("scatterxy"), 
		      verbatimTextOutput("lmsummary")
		    )  
		  )
		), 

		server = function(input, output, session) {

		  #render UI elements
		  output$pct_slider <- renderUI({
		  	bigData <- 1000000
			sliderInput("datapct",
	            "Sample Pct of Data", 0, 1, 
	            ifelse(nrow(dt) < bigData, 1, 
	            bigData / nrow(dt) %/% 0.05 * 0.05), 0.05)		  	
		  })

		  output$x_var_select <- renderUI({
		    selectInput("x_var", "X variable",
		        colnames(dt), colnames(dt)[1])
		  })
		  
		  output$y_var_select <- renderUI({
		    selectInput("y_var", "Y variable",
		    	colnames(dt), colnames(dt)[2])
		  })

		  #scan for null input
		  scatDimR <- reactive({
		  	if(is.null(input$scatdim)){
		  		6
		  	} else {
		  		input$scatdim
		  	}
		  })

		  #downsample large datasets
		  cleandtR <- reactive({
		  	if(input$datapct < 1){
			  	set.seed(333)
			  	out <- dt[sort(sample.int(nrow(dt), 
			  		ceiling(nrow(dt) * input$datapct),
			  		replace = FALSE)),]
			} else {
				out <- dt
			}

			out <- data.table(out)

			#convert non numeric to numeric
			nonnum <- names(out)[sapply(out, function(i) !is.numeric(i))]
			out[, (nonnum) := lapply(.SD, function(i) as.integer(as.factor(i))), 
				.SDcols = nonnum]

			out
		  })

		  #transform columns 
		  dtR <- reactive({
		  	out <- cleandtR()[, c(input$y_var, input$x_var), with = FALSE]

		  	if(!is.null(input$xcap)){
		  		out[get(input$x_var) >= input$xcap, (input$x_var) := input$xcap]
		  	}
		  	if(!is.null(input$ycap)){
		  		out[get(input$y_var) >= input$ycap, (input$y_var) := input$ycap]
		  	}

		  	if(input$xtrans == "log"){
		  		out[, (input$x_var) := log(.SD), .SDcols = input$x_var]
		  	} else if(input$xtrans == "square"){
		  		out[, (input$x_var) := (.SD ^ 2), .SDcols = input$x_var]
		  	} else if(input$xtrans == "cube"){
		  		out[, (input$x_var) := (.SD ^ 3), .SDcols = input$x_var]
		  	} else if(input$xtrans == "log10"){
		  		out[, (input$x_var) := log10(.SD), .SDcols = input$x_var]
		  	} else if(input$xtrans == "square"){
		  		out[, (input$x_var) := (.SD ^ 0.5), .SDcols = input$x_var]
		  	} 

		  	if(input$ytrans == "log"){
		  		out[, (input$y_var) := log(.SD), .SDcols = input$y_var]
		  	} else if(input$ytrans == "square"){
		  		out[, (input$y_var) := (.SD ^ 2), .SDcols = input$y_var]
		  	} else if(input$ytrans == "cube"){
		  		out[, (input$y_var) := (.SD ^ 3), .SDcols = input$y_var]
		  	} else if(input$ytrans == "log10"){
		  		out[, (input$y_var) := log10(.SD), .SDcols = input$y_var]
		  	} else if(input$ytrans == "square"){
		  		out[, (input$y_var) := (.SD ^ 0.5), .SDcols = input$y_var]
		  	} 

		  	out
		  })

		  output$lmsummary <- renderPrint({
		  	summary(lm(as.formula(paste(input$y_var, "~", input$x_var)), 
		  		data = dtR()))
		  })

		  output$xysummary <- renderPrint({
		  	data <- dtR()
		  	rbind(data[, .(metric = "min", x_var = min(get(input$x_var)), 
		  			y_var = min(get(input$y_var)))], 
		  		data[, .(metric = "mean", x_var = mean(get(input$x_var)), 
		  			y_var = mean(get(input$y_var)))], 
		  		data[, .(metric = "median", x_var = median(get(input$x_var)), 
		  			y_var = median(get(input$y_var)))], 
		  		data[, .(metric = "max", x_var = max(get(input$x_var)), 
		  			y_var = max(get(input$y_var)))], 
		  		data[, .(metric = "naPct", 
		  			x_var = .SD[is.na(get(input$x_var)), .N] / .N, 
		  			y_var = .SD[is.na(get(input$y_var)), .N] / .N)]
		  	)
		  })

		  #hist of x_var
		  histxR <- reactive({
		  	#set ranges
			minx <- min(dtR()[[input$x_var]], na.rm = TRUE)
			maxx <- max(dtR()[[input$x_var]], na.rm = TRUE)
			rangex <- maxx - minx

			#x-var hist
			if(input$densitycheck){
				p <- ggplot(dtR(), aes_string(x = input$x_var)) + 
					geom_histogram(aes(y = ..density..), 
						binwidth = rangex / 30,
						colour = "white") +
					geom_density(colour = "blue", size = 1) +
					scale_x_continuous(limits = c(minx, maxx))
			} else {
				p <- ggplot(dtR(), aes_string(x = input$x_var)) + 
					geom_histogram(binwidth = rangex / 30,
						colour = "white") +
					scale_x_continuous(limits = c(minx, maxx))
			}

			p
		  })

		  #hist of y_var
		  histyR <- reactive({
		  	#set ranges
		  	miny <- min(dtR()[[input$y_var]], na.rm = TRUE)
			maxy <- max(dtR()[[input$y_var]], na.rm = TRUE)
			rangey <- maxy - miny

			#y-var hist
			if(input$densitycheck){
				p <- ggplot(dtR(), aes_string(x = input$y_var)) + 
					geom_histogram(aes(y = ..density..), 
						binwidth = rangey / 30,
						colour = "white") +
					geom_density(colour = "blue", size = 1) + 
					scale_x_continuous(limits = c(miny, maxy))
			} else {
				p <- ggplot(dtR(), aes_string(x = input$y_var)) + 
					geom_histogram(binwidth = rangey / 30,
						colour = "white") +
					scale_x_continuous(limits = c(miny, maxy))
			}

			p
		  })

		  scatterxyR <- reactive({
		  	#set ranges
			minx <- min(dtR()[[input$x_var]], na.rm = TRUE)
			maxx <- max(dtR()[[input$x_var]], na.rm = TRUE)
			miny <- min(dtR()[[input$y_var]], na.rm = TRUE)
			maxy <- max(dtR()[[input$y_var]], na.rm = TRUE)

			#x,y scatter
			p <- ggplot(dtR(), aes_string(input$x_var, input$y_var)) + 
				geom_point(alpha = input$scatalpha) +
				scale_x_continuous(limits = c(minx, maxx)) +
				scale_y_continuous(limits = c(miny, maxy)) 

			if(input$lmcheck){
				p <- p + geom_smooth(method = "lm", se = FALSE, colour = "blue")
			}
			
			if(input$loesscheck){
				p <- p + geom_smooth(method = "loess", se = FALSE, colour = "red")
			}

			p
		  })

		  output$xhist <- renderPlot({
		  	# make sure the x and y variable select boxes have been updated
		    if (is.null(input$x_var) || is.null(input$y_var) ||
		        !input$x_var %in% colnames(dt) ||
		        !input$y_var %in% colnames(dt)) {
		      return(NULL)
		    }

		  	histxR()
		  })

		  output$yhist <- renderPlot({
		  	# make sure the x and y variable select boxes have been updated
		    if (is.null(input$x_var) || is.null(input$y_var) ||
		        !input$x_var %in% colnames(dt) ||
		        !input$y_var %in% colnames(dt)) {
		      return(NULL)
		    }

		  	histyR()
		  })

		  output$scatterxy <- renderPlot({
		  	# make sure the x and y variable select boxes have been updated
		    if (is.null(input$x_var) || is.null(input$y_var) ||
		        !input$x_var %in% colnames(dt) ||
		        !input$y_var %in% colnames(dt)) {
		      return(NULL)
		    }

		  	scatterxyR()
		  })

		  output$jointplot <- renderPlot({   
		    # make sure the x and y variable select boxes have been updated
		    if (is.null(input$x_var) || is.null(input$y_var) ||
		        !input$x_var %in% colnames(dt) ||
		        !input$y_var %in% colnames(dt)) {
		      return(NULL)
		    }

			#render scatter and histogram plots
			hist_top <- histxR() +
				theme(axis.ticks = element_blank(), 
				  panel.background = element_blank(), 
				  axis.text = element_blank(), 
				  axis.line = element_blank(),           
				  axis.title = element_blank()
				)

			empty <- ggplot() + 
				geom_point(aes(1,1), colour="white") +
				theme(axis.ticks = element_blank(), 
				  panel.background = element_blank(), 
				  axis.text = element_blank(), 
				  axis.line = element_blank(),           
				  axis.title = element_blank()
				)

			scatter <- scatterxyR()

			hist_right <-  histyR() +
				coord_flip() +
				theme(axis.ticks = element_blank(), 
				  panel.background = element_blank(), 
				  axis.text = element_blank(), 
				  axis.line = element_blank(),           
				  axis.title = element_blank()
				)

			#arrange jointplot grid
			p <- grid.arrange(hist_top, empty, scatter, hist_right, 
				ncol=2, nrow=2, widths=c(scatDimR(), 1), 
				heights=c(1, scatDimR()))

			#return output plot
			p
		  }) #close plot generation
		} #close the server function
	) #close the shinyApp call
}

explore(iris)


