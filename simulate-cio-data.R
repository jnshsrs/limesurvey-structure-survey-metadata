library(shiny)
library(dplyr)
library(tidyr)
library(showtext)
library(RCurl)
library(stringi)
library(RSQLite)
library(RMySQL)
library(purrr)
library(rCharts)

get_data <- function(qry) {
    path <-  "cio-survey-structure-it-report-16.sqlite"
    con <- dbConnect(drv = SQLite(), path)
    qry_statement <- qry
    res <- dbSendQuery(con, qry_statement)
    df <- dbFetch(res, n = -1)
    df <- map_df(df, stri_encode, from = "UTF-8", to  = "UTF-8")
    dbDisconnect(con)
    return(df)
}

qry <- 'SELECT * FROM question 
            INNER JOIN subquestion ON question.id = subquestion.question_id 
            WHERE question.question_text LIKE \'%Existiert in Ihrer Einrichtung%\''

df <- get_data(qry)

funktion_gruppe <- c("Klinische Dokumentationsfunktionen", "Leistungsanforderung und Befundrückmeldung", "Entscheidungsunterstützung", "Patientensicherheit")
question_internal_id <- c("KD1", "LB1", "EN1", "P1")
funktionsgruppen <- data.frame(question_internal_id, funktion_gruppe, stringsAsFactors = F) 

df <- left_join(df, funktionsgruppen, by = "question_internal_id")

qry <- 'SELECT response_label, question_id FROM response LEFT JOIN question_response ON response.id = question_response.response_id'
responses <- get_data(qry)


df <- left_join(df, responses, by = c("question_id" = "question_id")) %>% 
    select(-question_type) %>% 
    filter(grepl(pattern = "^[A-Z]{0,2}1|^Tel", x = question_internal_id))

idx <- sample(nrow(df), 1000, replace = T)
data <- df[idx, ]

value_levels <- df$response_label %>% unique() %>% sort
value_levels <- value_levels[c(5,2,3,1,4)]

data$response_label <- factor(x = data$response_label, 
                              levels = value_levels,
                              ordered = T)

qry <- "select subquestion.subquestion_text, subquestion.subquestion_internal_id, question.question_internal_id 
            from subquestion 
            left join question on subquestion.question_id = question.id"
subquestion <- get_data(qry) 
question_internal_id <- c("KD1", "LB1", "EN1", "P1", "Tel", "VS1", "S1")
funktionsgruppen <- data.frame(question_internal_id, stringsAsFactors = F) 

subquestion <- left_join(funktionsgruppen, subquestion, by = c("question_internal_id" = "question_internal_id"))

# create mapped list from two columns of a dataframe
map_values <- function(data, x, y){
    x <- data[[x]]
    y <- data[[y]]
    l <-  list()
    
    for(i in 1:length(x)){
        l <- c(l, x[i])
    }
    
    names(l) <- y
    return(l)
}

# Dieser Analyseschritt kann erstellt die Liste für das SubDrownMenu
filter_subcategory <- function(data, category){
    data  %>% filter(question_internal_id == category) %>% 
        map_values(data = ., "subquestion_internal_id" , "subquestion_text")
}



