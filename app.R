library(shiny)
library(bslib)
library(tidyverse)
library(janitor)
library(DT)
library(ggalluvial)
library(ggmosaic)

#generate data set
user_behavior_dataset <-
  read_csv("user_behavior_dataset.csv") |>
  clean_names(case = "big_camel") |>
  mutate(across(c(DeviceModel, OperatingSystem, Gender, UserBehaviorClass), as.factor)) |>
  mutate(UserId = as.character(UserId))

#establish variable_choices
all_var_choices <- names(user_behavior_dataset[-1])
num_var_choices <- names(select(user_behavior_dataset, where(is.numeric)))
cat_var_choices <- append(all_var_choices[!all_var_choices %in% num_var_choices], "None")
mod_var_choices <- append(all_var_choices, "None")

#ui section
ui <- fluidPage(
  titlePanel("Mobile Behavior Data Exploration"),
  sidebarLayout(
    sidebarPanel(
      h2("Select how to subset the mobile behavior data:"),
      #Choose (at least) two categorical variables they can subset from. If there are groups of categories that make sense to have them choose for a given variable, that’s fine. That is, they don’t need to be able to choose any level of the each variable to subset. The user should be able to select all levels as well.
      selectInput("cat_device",
                  "Device models",
                  choices = levels(user_behavior_dataset$DeviceModel),
                  multiple = T),
      selectInput("cat_behavior",
                  "User behavior classes",
                  choices = levels(user_behavior_dataset$UserBehaviorClass),
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
                   "Apply Subset")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("About",
                 fluidRow(
                   column(8,
                 "The Mobile Device Usage and User Behavior Dataset provides a comprehensive analysis of mobile device usage patterns and user behavior classification. It contains 700 samples of user data, including metrics such as app usage time, screen-on time, battery drain, and data consumption. Each entry is categorized into one of five user behavior classes, ranging from light to extreme usage, allowing for insightful analysis and modeling.

Data dictionary:

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
`user_behavior_class`: Classification of user behavior based on usage patterns (1 to 5).
                 Orientation of app:
                 Data Download Tab: ...
                 Data Exploration Tab: ..."),
                 column(4,
                   img(
                     src = "devices.jpeg",
                     height = 200,
                     width = 200,
                     alt = "Picture of mobile devices"
                   ))
                 )),
                 tabPanel("Data Download",
                          fluidRow(
                            column(12,
                                   downloadButton("download_subset",
                                      "Download Subset Data"),
                                   DTOutput("ubd_subset_dt")
                                   ))),
                 tabPanel("Data Exploration",
                          tabsetPanel(
                            tabPanel("Summary",
                                     fluidRow(
                                       column(6,
                                              selectInput("summ_var",
                                                          "Select variable for summary",
                                                          choices = all_var_choices)),
                                       column(6,
                                              selectInput("group_var",
                                                          "Select grouping variable",
                                                          choices = cat_var_choices,
                                                          selected = "None"))
                                     ),
                                     fluidRow(
                                       column(12,
                                              DTOutput("summ_table")))
                                     ),
                            tabPanel("Plot",
                                     fluidRow(
                                       column(6, 
                                              selectInput("axis1_var",
                                                          "Select variable for first axis",
                                                          choices = all_var_choices)),
                                       column(6,
                                              selectInput("axis2_var",
                                                          "Select variable for second axis",
                                                          choices = mod_var_choices,
                                                          selected = "None")),
                                     ),
                                     fluidRow(
                                       column(6,
                                              selectInput("fill_var",
                                                          "Select variable fill or color",
                                                          choices = cat_var_choices,
                                                          selected = "None")),
                                       column(6,
                                              selectInput("facet_var",
                                                          "Select variable for faceting",
                                                          choices = cat_var_choices,
                                                          selected = "None")),
                                     plotOutput("plot")))
                          )
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
  
  observeEvent(input$summ_var, {
    updateSelectInput(session, "group_var",
                      choices = cat_var_choices[!cat_var_choices %in% input$summ_var])
  })
  
  output$slider_num_one <- renderUI({
    req(input$num_one)
    min_val <- min(user_behavior_dataset[[input$num_one]], na.rm = T)
    max_val <- max(user_behavior_dataset[[input$num_one]], na.rm = T)
    sliderInput("dyn_slider_one",
                "Range",
                min = min_val,
                max = max_val,
                value = c(min_val, max_val))
  })
  
  output$slider_num_two <- renderUI({
    req(input$num_two)
    min_val <- min(user_behavior_dataset[[input$num_two]], na.rm = T)
    max_val <- max(user_behavior_dataset[[input$num_two]], na.rm = T)
    sliderInput("dyn_slider_two",
                "Range",
                min = min_val,
                max = max_val,
                value = c(min_val, max_val))
  })
  
  ubd_subset <- eventReactive(input$subset_data, {
    user_behavior_dataset |>
      filter(DeviceModel %in% input$cat_device,
             UserBehaviorClass %in% input$cat_behavior) |>
      filter(between(!!sym(input$num_one),
                     input$dyn_slider_one[1],
                     input$dyn_slider_one[2])) |>
      filter(between(!!sym(input$num_two),
                     input$dyn_slider_two[1],
                     input$dyn_slider_two[2]))
  })
  
  output$ubd_subset_dt <- renderDT(datatable(ubd_subset()))
  
  output$download_subset <- downloadHandler(
    filename = function() {
      paste('ubd-subset-', Sys.Date(), '.csv', sep='')
      },
    content = function(con) {
      write.csv(ubd_subset(), con)
      }
    )
  
  summaries <- eventReactive(c(input$summ_var, input$group_var), {
    req(input$summ_var, input$group_var)
    data <- ubd_subset()
    if(input$group_var == "None") {
      if(input$summ_var %in% num_var_choices) {
        data |>
        summarise(mean = mean(.data[[input$summ_var]], na.rm = T),
                  median = median(.data[[input$summ_var]]), na.rm = T,
                  sd = sd(.data[[input$summ_var]]), na.rm = T,
                  IQR = IQR(.data[[input$summ_var]]), na.rm = T,
                  .groups = "drop") |>
        round(2)
      } else {
        data |>
          group_by(.data[[input$summ_var]]) |>
          summarise(count = n())
        }
    } else {
      if(input$summ_var %in% num_var_choices) {
        data |>
          select(.data[[input$group_var]], .data[[input$summ_var]]) |>
          group_by(.data[[input$group_var]]) |>
          summarise(across(where(is.numeric),
                           list("mean" = mean, "median" = median, "stdev" = sd, "IQR" = IQR),
                           .names = "{.fn}"))|>
          mutate(across(where(is.numeric), round, 2))
      } else {
      data |>
          group_by(.data[[input$summ_var]], .data[[input$group_var]]) |>
          summarise(count = n()) |>
          pivot_wider(names_from = .data[[input$group_var]], values_from = count)
      }
    }
    }
    )
  
  output$summ_table <- renderDT(datatable(summaries(), rownames = F))

  graph <- eventReactive(c(input$axis1_var, input$axis2_var, input$fill_var, input$facet_var), {
    req(input$axis1_var, input$axis2_var, input$fill_var, input$facet_var)
    data <- ubd_subset()
    g <- ggplot(data) +
      scale_fill_brewer(palette="Set1")
    if(input$axis1_var %in% num_var_choices & input$axis2_var %in% num_var_choices) {
      g <- g +
        aes_string(x = input$axis1_var,
                   y = input$axis2_var,
                   color = if (input$fill_var != "None") input$fill_var else NULL
                   ) +
        geom_point() +
        ggtitle(paste0("Relationship between ", input$axis1_var, " and ", input$axis2_var)) +
        labs(subtitle = paste0("Considering effects of ", input$fill_var, " and ", input$facet_var),
             color = input$fill_var)
    } else if (input$axis1_var %in% num_var_choices & input$axis2_var == "None") {
      g <- g +
        aes_string(x = input$axis1_var) +
        geom_histogram()
    } else if (input$axis1_var %in% num_var_choices | input$axis2_var %in% num_var_choices) {
      g <- g +
        aes_string(x = input$axis1_var,
                   y = input$axis2_var) +
        geom_boxplot() +
        theme(legend.position = "none") +
        ggtitle(paste0("Distribution relative to ", input$axis1_var, " and ", input$axis2_var))
    } else if (input$axis2_var != "None") {
      g <- g +
        geom_mosaic(
          aes_string(x = product(!!sym(input$axis1_var)),
                     fill = input$axis2_var) 
        ) +
        theme_mosaic() +
        theme(legend.position = "none") +
        ggtitle(paste0("Distribution relative to ", input$axis1_var, " and ", input$axis2_var))
    } else {
      g <- g +
        aes_string(x = input$axis1_var,
                   fill = if (input$fill_var != "None") input$fill_var else NULL
                   ) +
        geom_bar(position = "dodge") +
        labs(fill = input$fill_var) +
        ggtitle(paste0("Distribution relative to ", input$axis1_var, " and ", input$fill_var))
    }
    if (input$facet_var != "None") {
      g <- g + facet_wrap(as.formula(paste("~", input$facet_var)))
    }
    
    return(g)
    }
  )
  
  output$plot <- renderPlot(graph())
  
  
  }
#run section
shinyApp(ui, server)