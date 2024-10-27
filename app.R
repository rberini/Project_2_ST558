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

#ui section
ui <- fluidPage(
  titlePanel("Mobile Behavior Data Exploration"),
  sidebarLayout(
    sidebarPanel(
      h2("Select Variables ... :"),
      h2("..."),

      h2("...")
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
server <- function(input, output, session) {}

#run section
shinyApp(ui, server)