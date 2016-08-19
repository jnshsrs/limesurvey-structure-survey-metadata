# data: data.frame,  containing bar labels and response values
# X: sting, column name containing bar labels (question text)
# value: string, column name containing response values

GGbars <- function(data, X, value, category = NULL, plot_color = "Greens", plot_title = NULL) {
    data <- data.frame(X = data[[X]], value = data[[value]])
    # Parameter für Plot-Function
    
    data <- data %>%
        mutate(text_2 = X) %>%
        group_by(text_2) %>%
        mutate(count = n())  %>%
        mutate(text = paste0(text_2, " [n=", count, "]"))
    
    data$text_2 <- NULL
    data$count <- NULL
    data$X <- NULL
    
    cont_table <- prop.table(table(data$text, data$value), margin = 1)
    values <- as.data.frame.matrix(cont_table)
    values$text <- row.names(values)
    values <- gather(values, key = "key", value = "percent", -text) %>% select(-key) %>% arrange(text)
    
    # get order
    x_label_order <- cont_table[,c(3,4)] %>% rowSums() %>% sort %>% names
    
    text_position <- t(apply(cont_table, 1, cumsum)) - (cont_table / 2)
    text_position <- as.data.frame.matrix(text_position)
    text_position$text <- row.names(text_position)
    text_position <- gather(text_position, key = "key", value = "value", -text) %>% select(-key) %>% arrange(text)
    
    text_position <- cbind(text_position, percent = values$percent)
    
    p <- data %>% ggplot(., aes(x = text, fill = value)) + geom_bar(position = "fill") +
        scale_fill_brewer(palette = plot_color) +
        scale_y_continuous(breaks = seq(0,1, .1), labels = scales::percent) +
        ggtitle(plot_title) +
        theme(axis.title = element_blank(),
              axis.text.y = element_text(size = 14),
              axis.text.x = element_text(size = 12),
              axis.ticks.y = element_blank(),
              legend.title = element_blank(),
              legend.text = element_text(size = 10),
              legend.position = "right",
              panel.background = element_blank()) +
        annotate("text", x = text_position$text, y=text_position$value, label = sprintf("%.1f%%", 100 * text_position$percent)) +
        coord_flip()
    return(p)
}


