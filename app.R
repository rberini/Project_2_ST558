#load all required packages
library(shiny)
library(bslib)
library(tidyverse)
library(janitor)
library(DT)
library(ggmosaic)
library(shinyvalidate)
library(shinyalert)
library(shinycssloaders)

#generate data set with automatic renaming of columns in upper camel case for improve readability across the app
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
      #Choose two categorical variables the user can subset from. The user should be able to select any or all levels.
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
      tabsetPanel(id = "tab_switch",
        tabPanel("About",
                 value = 1,
                 fluidRow(
                   column(8,
                          tags$div(
                            tags$h3("Purpose and Overview of this Shiny App"),
                            tags$p("This app allows the user to interactively explore the Mobile Device Usage and User Behavior Dataset (described below). The user can subset the data, download that subset, and generate numeric and graphical summaries of variables, or combinations of variables, in that subset"),
                            tags$h3("Navigating the App"),
                            tags$ul(
                              tags$li("Left Sidebar Panel: Asks the user to subset the data"),
                              tags$li("Data Download Tab: Allows the user to view and download the subsetted data"),
                              tags$li("Data Exploration Tab: Allows the user to generate numeric and/or graphical summaries of the subsetted data based on combinations of variables selected")
                            ),
                            tags$h3("About the Dataset"),
                            tags$p("The Mobile Device Usage and User Behavior Dataset provides a comprehensive analysis of mobile device usage patterns and user behavior classification. It contains 700 samples of user data. Available metrics are outlined in data dictionary below."),
                          tags$a(href = "https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset",
                                 "Click here to access dataset on kaggle",
                                 target = "_blank",
                                 rel = "noopener noreferrer"),
                          tags$p(""),
                          tags$p("Data dictionary:"),
                          tags$ul(
                            tags$li("UserId: Unique identifier for each user"),
                            tags$li("DeviceModel: Model of the user's smartphone"),
                            tags$li("OperatingSystem: The OS of the device (iOS or Android)"),
                            tags$li("AppUsageTimeMinDay: Daily time spent on mobile applications, measured in minutes"),
                            tags$li("ScreenOnTimeHoursDay: Average hours per day the screen is active"),
                            tags$li("BatteryDrainMAhDay: Daily battery consumption in mAh"),
                            tags$li("NumberOfAppsInstalled: Total apps available on the device"),
                            tags$li("DataUsageMbDay: Daily mobile data consumption in megabytes"),
                            tags$li("Age: Age of the user"),
                            tags$li("Gender: Gender of the user (Male or Female)"),
                            tags$li("UserBehaviorClass: Classification of user behavior based on usage patterns (1 to 5)")
                          )
                 )
                 ),
                 column(4,
                   img(
                     src = "devices.jpeg",
                     height = 300,
                     width = 300,
                     alt = "Picture of mobile devices"
                   ))
                 )),
                 tabPanel("Data Download",
                          value = 2,
                          fluidRow(
                            column(12,
                                   DTOutput("ubd_subset_dt"),
                                   downloadButton("download_subset",
                                      "Download Subset Data")
                                   ))),
                 tabPanel("Data Exploration",
                          value = 3,
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
                                                          "Select variable for fill or color",
                                                          choices = cat_var_choices,
                                                          selected = "None")),
                                       column(6,
                                              selectInput("facet_var",
                                                          "Select variable for faceting",
                                                          choices = cat_var_choices,
                                                          selected = "None")),
                                     plotOutput("plot") |>
                                       withSpinner(type = 5)
                                     ))
                          )
                          )
        )
      )
  )
)

#server section
server <- function(input, output, session) {
  
  #validate at least one level selection made for each categorical variable
  iv <- InputValidator$new()
  iv$add_rule("cat_device", sv_required())
  iv$add_rule("cat_behavior", sv_required())
  iv$enable()
  
  #remove first numeric variable selected from options for second numeric variable
  observeEvent(input$num_one, {
    updateSelectInput(session, "num_two",
                      choices = num_var_choices[!num_var_choices %in% input$num_one])
  })
  
  #if categorical variable selected for summary statistics, remove that variable from options for grouping variable
  observeEvent(input$summ_var, {
    updateSelectInput(session, "group_var",
                      choices = cat_var_choices[!cat_var_choices %in% input$summ_var],
                      selected = "None")
  })
  
  #display error if user tries to apply subset without selected at least one level for each categorical variable
  observeEvent(input$subset_data, {
    if(is.null(input$cat_device) | is.null(input$cat_behavior)) {
      shinyalert("Please select at least one device model and at least one user behavior class")
    }
  })
  
  #display error if user navigates to Data Download or Data Exploration without first subsetting the data
  observeEvent(input$tab_switch, {
    if((is.null(input$cat_device) |
       is.null(input$cat_behavior) |
       input$subset_data == 0) &
       input$tab_switch > 1) {
      shinyalert("Please first subset the dataset, then press 'Apply Subset' in left sidebar")
    }
  })
  
  #remove first variable selected from options for second variable
  observeEvent(input$axis1_var, {
    updateSelectInput(session, "axis2_var",
                      choices = mod_var_choices[!mod_var_choices %in% input$axis1_var],
                      selected = "None")
  })
  
  #change slider one values based upon variable selected
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
  
  #change slider two values based upon variable selected
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
  
  #create the data subset when action button is pressed
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
  
  #setup the csv file for data download if user presses download button
  output$download_subset <- downloadHandler(
    filename = function() {
      paste('ubd-subset-', Sys.Date(), '.csv', sep='')
      },
    content = function(con) {
      write.csv(ubd_subset(), con)
      }
    )
  
  #automatically generate appropriate summary based upon types and combinations of variables selected
  summaries <- eventReactive(c(input$summ_var, input$group_var), {
    req(input$summ_var, input$group_var)
    data <- ubd_subset()
    if(input$group_var == "None") {
      if(input$summ_var %in% num_var_choices) {
        data |>
        summarise(mean = mean(.data[[input$summ_var]], na.rm = T),
                  median = median(.data[[input$summ_var]], na.rm = T),
                  sd = sd(.data[[input$summ_var]], na.rm = T),
                  IQR = IQR(.data[[input$summ_var]], na.rm = T),
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
          drop_na(.data[[input$group_var]], .data[[input$summ_var]]) |>
          summarise(across(where(is.numeric),
                           list("mean" = mean, "median" = median, "stdev" = sd, "IQR" = IQR),
                           .names = "{.fn}"))|>
          mutate(across(where(is.numeric), round, 2))
      } else {
      data |>
          group_by(.data[[input$summ_var]], .data[[input$group_var]]) |>
          drop_na(.data[[input$summ_var]], .data[[input$group_var]]) |>
          summarise(count = n()) |>
          pivot_wider(names_from = .data[[input$group_var]], values_from = count)
      }
    }
    }
    )
  
  output$summ_table <- renderDT(datatable(summaries(), rownames = F))
  
  #automatically generate appropriate graph based upon types and combinations of variables selected
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
        ggtitle(paste0("Relationship between ", input$axis1_var, " and ", input$axis2_var))
      if(input$fill_var != "None") {
        g <- g +
          labs(subtitle = paste0("Considering effects of ", input$fill_var),
             color = input$fill_var)}
    } else if (input$axis1_var %in% num_var_choices
               & input$axis2_var == "None"
               & input$fill_var != "None") {
      g <- g +
        aes_string(x = input$axis1_var,
                   fill = input$fill_var) +
        geom_density(alpha = 0.5) +
        ggtitle(paste0("Distribution of ", input$axis1_var)) +
        labs(subtitle = paste0("Considering effects of ", input$fill_var),
             fill = input$fill_var)
    } else if (input$axis1_var %in% num_var_choices & input$axis2_var == "None") {
      g <- g +
        aes_string(x = input$axis1_var) +
        geom_density(fill = "grey60") +
        ggtitle(paste0("Distribution of ", input$axis1_var))
    } else if (input$axis1_var %in% num_var_choices | input$axis2_var %in% num_var_choices) {
      g <- g +
        aes_string(x = input$axis1_var,
                   y = input$axis2_var,
                   fill = if (input$fill_var != "None") input$fill_var else NULL) +
        geom_boxplot() +
        labs(fill = input$fill_var) +
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