# data: data.frame,  containing bar labels and response values
# X: sting, column name containing bar labels (question text)
# value: string, column name containing response values

GGbars <- function(data, X, value, plot_color = "Greens", plot_title = NULL) {
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



# data = dataframe containing it-functions of one group (e.g. documentation functions)
# X = response label (implementation status)
# value = "subquestion_text" - name of function

rChart_implementation_bars <- function(data, X, value){
    data <- data.frame(X = data[[X]], value = data[[value]])
    
    data <- data %>% 
        group_by(X, value) %>% 
        summarise(n = n()) %>% 
        group_by(value) %>% 
        mutate(abs = sum(n)) %>% 
        mutate(freq = n / abs)
    
    n2 <- nPlot(freq ~ value, group = "X", data = data, type = "multiBarChart")
        
    data_2 <- data_2 %>% group_by(response_label, subquestion_text) %>% mutate(abs = sum(count))
    data_2 %>% mutate(freq = count / abs) %>% filter(subquestion_text == "Pflegedokumentation")
    n2$chart(color = c('darkgreen', 'green', 'seagreen', 'palegreen'))
    n2$yAxis(tickFormat = "#! function(d) {
                    //will change to .1% to show how to get one decimal
                    return d3.format('.1%')(d)
                    } !#")
        
        # n2$xAxis(height = 4000)
    n2$set(width = 600, height = 500)
    n2$chart(forceY = c(0, 1))
    n2$xAxis(axisLabel = NULL, rotateLabels = -45)
    # n2$set(title = "Gibt es in Ihrem Krankenhaus eine Funktion für...")
    # n2$xAxis(staggerLabels = TRUE)
        
    # n2$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle.html"
    # prefix <- sub(pattern = "\\.{3}\\?", replacement = "", data_filtered$question_text[1])
    # n <- percent_values %>% summarise(sum(n)) %>% as.character()
    # n2$set(title = paste0(prefix, " ", data_filtered$subquestion_text[1], "? [n=", n, "]"))
    n2$chart(margin = list(bottom = 250)) # , showValues = TRUE)
    # n2$chart(valueFormat = "#! function(d) {
    #          //will change to .1% to show how to get one decimal
    #          return d3.format('.1%')(d)
    #          } !#")
    n2$chart(reduceXTicks = F)
    return(n2)
}

source("./simulate-cio-data.R")
# process data before given into function as argument
data <- data %>% filter(question_internal_id == "KD1") %>% droplevels()

# data = dataframe containing it-functions of one group (e.g. documentation functions)
# X = response label (implementation status)
# value = "subquestion_text" - name of function

rChart_implementation_bars(data = data, X = "response_label", value = "subquestion_text")
