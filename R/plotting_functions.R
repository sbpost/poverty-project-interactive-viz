# Dev prep
input <- list()
input$growth_period <- "2008-2018"
input$extra_growth <- 0
input$gini_alpha <- 0
input$gic <- "l"
input$passthrough_rate <- "mbrp"
input$poverty_line <- "FGT0_19"

prepared_data <- function() {
		haven::read_dta("../data/Projections.dta") %>%
		select(-c(
			  region_pcn,
			  FGT1_19,
			  FGT1_32,
			  FGT1_55,
			  FGT2_19,
			  FGT2_32,
			  FGT2_55
			  )) %>%
		# Gather poverty_lines into single col 
		gather(c(FGT0_19, FGT0_32, FGT0_55), key = poverty_line, val = poverty_in_perc) %>%
		# Create "number of poor" 
		mutate(number_of_poor = pop * (poverty_in_perc / 100))
}

filtered_data <- function(){
prepared_data() %>% 
			# Add poverty line control
			filter(poverty_line == input$poverty_line) %>%
			# Add growth period control
			filter(growth %in% c("", input$growth_period)) %>%
			# Add extragrowth control
			filter(is.na(extragrowth) | extragrowth == input$extra_growth) %>%
			# Add gini control
			filter(is.na(alpha) | alpha == input$gini_alpha) %>%
			# Add GIC control
			filter(gic %in% c("", input$gic)) %>%
			# Add passthrough control 
			filter(passthrough %in% c("", input$passthrough_rate))
}


# Prepare global data
regional_poverty_tbl <-
	filtered_data() %>% 
	group_by(region_wb, year) %>%
	summarize(region_number_of_poor = sum(number_of_poor))

# Plot global data
plot <- 
regional_poverty_tbl %>%
	ggplot(aes(x = year, y = region_number_of_poor, fill = region_wb)) +
	geom_area()

ggplotly(plot)
