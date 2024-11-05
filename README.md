# Project_2_ST558

## About the shiny app

This app allows the user to interactively explore the **Mobile Device Usage and User Behavior Dataset** (described below). The user can subset the data, download that subset, and generate numeric and graphical summaries of variables, or combinations of variables, in that subset. It features three areas for working with the data:

-   Left Sidebar Panel: Asks the user to subset the data based upon levels of two categorical variables and value ranges of two numerical variables
-   Data Download Tab: Allows the user to view and download the subsetted data
-   Data Exploration Tab: Allows the user to generate numeric and/or graphical summaries of the subsetted data based on combinations of variables selected

## About the data set

The **Mobile Device Usage and User Behavior Dataset** provides a comprehensive analysis of mobile device usage patterns and user behavior classification. It contains 700 samples of user data, including metrics such as app usage time, screen-on time, battery drain, and data consumption. Each entry is categorized into one of five user behavior classes, ranging from light to extreme usage, allowing for insightful analysis and modeling.

Data dictionary:

-   `UserId`: Unique identifier for each user
-   `DeviceModel`: Model of the user's smartphone
-   `OperatingSystem`: The OS of the device (iOS or Android)
-   `AppUsageTimeMinDay`: Daily time spent on mobile applications, measured in minutes
-   `ScreenOnTimeHoursDay`: Average hours per day the screen is active
-   `battery_drain_m_ah_day`: Daily battery consumption in mAh
-   `NumberOfAppsInstalled`: Total apps available on the device
-   `DataUsagMbDay`: Daily mobile data consumption in megabytes
-   `Age`: Age of the user
-   `Gender`: Gender of the user (Male or Female)
-   `User_behavior_class`: Classification of user behavior based on usage patterns (1 to 5)
