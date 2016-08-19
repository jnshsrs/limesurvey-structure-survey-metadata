# data: data.frame,  containing bar labels and response values
# X: sting, column name containing bar labels (question text)
# value: string, column name containing response values

#' Visualising the relative frequencies with paired bar plots
#'
#' @param data A data.frame containing bar labels and response values.
#' @param X String, column name mapping to the column name containing bar labels(question type)
#' @param value String, column name mapping to the column of data which contains response values.
#' @return The function returns a horizonal, filled, colored, bar chart with relative frequencies
#' @export


library(ggplot2)
library(dplyr)
library(scales)


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
                  axis.text.y = element_text(size = 14),
                  axis.text.x = element_text(size = 12),
                  axis.ticks.y = element_blank(),
                  legend.title = element_blank(),
                  legend.text = element_text(size = 10),
                  legend.position = "none",
                  panel.background = element_blank(),
                  panel.grid.major.x = element_line(colour = "grey"),
                  axis.ticks.x = element_line(colour = "grey"))
    return(p)
}
GGbar(data, X = "response_label", value = "KD1_SQ001")


