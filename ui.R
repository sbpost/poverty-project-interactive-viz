ui <- fluidPage(sidebarLayout(
			      sidebarPanel(
		# growth period control
		radioButtons("growth_period", "Which years should be used to get historical growth rates?",
			     choices = c("1998-2018" = "1998-2018",
					 "2008-2018 (pre COVID-19 projections)" = "2008-2018precovid",
					 "2008-2018 (post COVID-19 projections)" = "2008-2018",
					 "2013-2018" = "2013-2018"),
			     selected = "2008-2018"),
		# added growth control
		sliderInput("extra_growth", "How many percentage points should growth be changed from the the historical annual rates?",
			    min = -2,
			    max = 2,
			    step = 1,
			    value = 0),
		# inequality change control
		sliderInput("gini_alpha", "How any percent should the Gini-coefficient change each year?",
			    min = -2,
			    max = 2,
			    step = 1,
			    value = 0),
		# GIC shape control
		radioButtons("gic", "Should the change in Gini-coefficient be distributed by a linear or a convex growth incidence curve (GIC)?",
			     choices = c("Linear GIC" = "l",
			     		 "Convex GIC" = "c"),
			     selected = "c"),
		# passthrough rate control
		radioButtons("passthrough_rate", "How should the passthrough rate from growth in GDP to household welfare be decided?",
			     choices = c("Model-based recursive partioning (MBRP)" = "mbrp",
					 "Global value of 85%" = "glob"),
			     selected = "mbrp")
		),
			      mainPanel(tableOutput("test_table"))
			      )
)

server <- function(input, output, session) {

	original_data <- reactive({
		haven::read_dta("./data/Projections.dta")
	})


	# Filtering reactive
	filtered_data <- reactive({

		original_data() %>% 
			# Add growth period control
			filter(growth %in% c("", input$growth_period)) %>%
			# Add extragrowth control
			filter(extragrowth %in% c(NA, input$extra_growth)) %>%
			# Add gini control
			filter(alpha %in% c(NA, input$gini_alpha)) %>%
			# Add GIC control
			filter(gic %in% c("", input$gic)) %>%
			# Add passthrough control 
			filter(passthrough %in% c("", input$passthrough_rate))

	})

	# Global plots



}

shinyApp(ui, server)

