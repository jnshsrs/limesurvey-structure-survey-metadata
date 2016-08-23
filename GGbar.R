# data: data.frame,  containing bar labels and response values
# X: sting, column name containing bar labels (question text)
# value: string, column name containing response values

#' Visualising the relative frequencies with paired bar plots
#'
#' @param data A data.frame containing bar labels and response values.
#' @param X String, column name mapping to the column name containing bar labels(question type)
#' @param value String, column name mapping to the column of data which contains response values.
#' @return The function returns a horizonal, filled, colored, ggplot2 barchart with relative frequencies
#' @export


library(ggplot2)
library(dplyr)
library(scales)
library(ggvis)
library(rCharts)


GGbar <- function(data, X, value, colour = "grey") { 
    
    data_filtered <- data %>% filter(subquestion_internal_id == value)
    
    percent_values <- data_filtered %>% group_by(response_label) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
    
    n <- percent_values %>% summarise(sum(n)) %>% as.character()
    
    plot_title <- paste0(data_filtered$question_text[1], "\n" , data_filtered$subquestion_text[1], " [n=", n, "]")
    
    p <- ggplot(data = data_filtered, aes(x = response_label, fill = response_label)) + 
            geom_bar(aes(y = (..count..)/sum(..count..)), width = .5, fill = "grey50", col = "grey") + 
            coord_flip() + theme(axis.title = element_blank()) +
            ggtitle(plot_title) +
            scale_y_continuous(breaks = seq(0,1, .1), labels = scales::percent, limits = c(0,1)) +
            geom_text(data = percent_values, aes(y = freq + .15, label = sprintf("%.1f%%", 100 * freq))) + 
            theme(axis.title = element_blank(),
                  axis.text.y = element_text(size = 12),
                  axis.text.x = element_text(size = 10),
                  axis.ticks.y = element_blank(),
                  legend.title = element_blank(),
                  legend.text = element_text(size = 8),
                  legend.position = "none",
                  panel.background = element_blank(),
                  panel.grid.major.x = element_line(colour = "grey"),
                  axis.ticks.x = element_line(colour = "grey"))
    return(p)
}

#' Visualising the relative frequencies with paired bar plots
#'
#' @param data A data.frame containing bar labels and response values.
#' @param X String, column name mapping to the column name containing bar labels(question type)
#' @param value String, column name mapping to the column of data which contains response values.
#' @return The function returns a horizonal, filled, colored, ggvis barchart with relative frequencies
#' @export

GGVIS_bar <- function(data, X, value, colour = "grey") {

    data_filtered <- data %>% filter(subquestion_internal_id == value)
    
    percent_values <- data_filtered %>% group_by(response_label) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
    
    n <- percent_values %>% summarise(sum(n)) %>% as.character()
    
    plot_title <- paste0(data_filtered$question_text[1], "\n" , data_filtered$subquestion_text[1], " [n=", n, "]")
    
    p <- data_filtered %>% 
        group_by(response_label) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n / sum(n)) %>% 
        droplevels() %>% 
        ggvis(x = ~freq, x2 = 0,  y = ~response_label, height = band()) %>% 
        layer_rects()  %>% 
        add_axis(type = "y", title = "") %>% 
        add_axis(type = "x", title = "Umsetzungsgrad in Prozent")
    return(p)
    
}

#' Visualising the relative frequencies with bar plot
#'
#' @param data A data.frame containing bar labels and response values.
#' @param X String, column name mapping to the column name containing bar labels(question type)
#' @param value String, column name mapping to the column of data which contains response values.
#' @return The function returns a horizonal, filled, colored, rChart discrete barchart with relative frequencies
#' @export

rChart_bar <-  function(data, X, value) { 
    
    data_filtered <- data %>% filter(subquestion_internal_id == value)
    
    percent_values <- data_filtered %>% group_by(response_label) %>% summarise(n = n()) %>% mutate(freq = n / sum(n))
    n <- percent_values %>% summarise(sum(n)) %>% as.character()
    
    n2 = nPlot(x = "response_label", y = "freq", data = percent_values, type = "discreteBarChart")
    n2$chart(color = c('darkgreen', 'green', 'seagreen', 'palegreen'))
    n2$yAxis(tickFormat = "#! function(d) {
             //will change to .1% to show how to get one decimal
             return d3.format('.1%')(d)
             } !#")
    
    # n2$xAxis(height = 4000)
    n2$set(width = 600, height = 500)
    n2$chart(forceY = c(0, 1))
    n2$xAxis(axisLabel = NULL, rotateLabels = -45)
    n2$set(title = "Gibt es in Ihrem Krankenhaus eine Funktion für...")
    # n2$xAxis(staggerLabels = TRUE)

    n2$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle.html"
    prefix <- sub(pattern = "\\.{3}\\?", replacement = "", data_filtered$question_text[1])
    n <- percent_values %>% summarise(sum(n)) %>% as.character()
    n2$set(title = paste0(prefix, " ", data_filtered$subquestion_text[1], "? [n=", n, "]"))
    n2$chart(margin = list(bottom = 250), 
             showValues = TRUE)
    n2$chart(valueFormat = "#! function(d) {
             //will change to .1% to show how to get one decimal
             return d3.format('.1%')(d)
    } !#")
    
    return(n2)
}

