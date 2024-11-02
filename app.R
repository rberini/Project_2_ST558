library(shiny)
library(bslib)
library(tidyverse)
library(janitor)
library(ggalluvial)
library(ggmosaic)

#generate data set
user_behavior_dataset <-
  read_csv("user_behavior_dataset.csv") |>
  clean_names() |>
  mutate(across(c(device_model, operating_system, gender, user_behavior_class), as.factor)) |>
  mutate(user_id = as.character(user_id))

#establish numeric_variable_choices
num_var_choices <- names(select(user_behavior_dataset, where(is.numeric)))

#ui section
ui <- fluidPage(
  titlePanel("Mobile Behavior Data Exploration"),
  sidebarLayout(
    sidebarPanel(
      h2("Select how to subset mobile behavior data:"),
      #Choose (at least) two categorical variables they can subset from. If there are groups of categories that make sense to have them choose for a given variable, that’s fine. That is, they don’t need to be able to choose any level of the each variable to subset. The user should be able to select all levels as well.
      selectInput("cat_device",
                  "Device models",
                  choices = levels(user_behavior_dataset$device_model),
                  multiple = T),
      selectInput("cat_behavior",
                  "User behavior classes",
                  choices = levels(user_behavior_dataset$user_behavior_class),
                  multiple = T),
      selectInput("num_one",
                  "First numeric variable",
                  choices = num_var_choices),
      uiOutput("slider_num_one"),
      selectInput("num_two",
                  "Second numeric variable",
                  choices = num_var_choices),
      uiOutput("slider_num_two"),
      actionButton("subset_data",
                   "Update for Subset")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("About",
                 fluidRow(
                   column(8,
                 "The Mobile Device Usage and User Behavior Dataset provides a comprehensive analysis of mobile device usage patterns and user behavior classification. It contains 700 samples of user data, including metrics such as app usage time, screen-on time, battery drain, and data consumption. Each entry is categorized into one of five user behavior classes, ranging from light to extreme usage, allowing for insightful analysis and modeling.

Data dictiomnary:

`user_id`: Unique identifier for each user.
`device_model`: Model of the user's smartphone.
`operating_system`: The OS of the device (iOS or Android).
`app_usage_time_min_day`: Daily time spent on mobile applications, measured in minutes.
`screen_on_time_hours_day`: Average hours per day the screen is active.
`battery_drain_m_ah_day`: Daily battery consumption in mAh.
`number_of_apps_installed`: Total apps available on the device.
`data_usage_mb_day`: Daily mobile data consumption in megabytes.
`age`: Age of the user.
`gender`: Gender of the user (Male or Female).
`user_behavior_class`: Classification of user behavior based on usage patterns (1 to 5)."),
                 column(4,
                   img(
                     src = "devices.jpeg",
                     height = 200,
                     width = 200,
                     alt = "Picture of mobile devices"
                   ))
                 ),
        tabsetPanel(
          tabPanel("Data Download"),
          tabPanel("Data Exploration",
                 tabsetPanel(
                   tabPanel("Summary"),
                   tabPanel("Plot"))))
      )
    )
    )
  )
)


#server section
server <- function(input, output, session) {
  observeEvent(input$num_one, {
    updateSelectInput(session, "num_two",
                      choices = num_var_choices[!num_var_choices %in% input$num_one])
  })
  
  
  output$slider_num_one <- renderUI({
    req(input$num_one)
    min = min(user_behavior_dataset[input$num_one])
    max = max(user_behavior_dataset[input$num_one])
    sliderInput("dynamic",
                "Range",
                min = min,
                max = max,
                value = c(min, max))
  })
  
  output$slider_num_two <- renderUI({
    req(input$num_two)
    min = min(user_behavior_dataset[input$num_two])
    max = max(user_behavior_dataset[input$num_two])
    sliderInput("dynamic",
                "Range",
                min = min,
                max = max,
                value = c(min, max))
  })
  
  ubd_subset <- reactive({
    user_behavior_dataset |>
      select(input$cat_device) |>
      select(input$cat_behavior) |>
      select(input$num_one) |>
      filter(between(
        input$num_one,
        left = input$slider_num_one[1],
        right = input$slider_num_one[2])) |>
      select(input$num_two) |>
      filter(between(
        input$num_two,
        left = input$slider_num_two[1],
        right = input$slider_num_two[2])) 
  })
}

#run section
shinyApp(ui, server)