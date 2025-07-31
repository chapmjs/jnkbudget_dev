# Budget Tracking Tool - R Shiny Application
# File: app.R
# With MySQL database and password protection

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)
library(shinycssloaders)
library(RMySQL)
library(pool)

# Database configuration
DB_CONFIG <- list(
  host = "mysql.bbfarm.org",
  port = 3307,
  dbname = "budget_tracker",
  username = "budget_user",
  password = Sys.getenv("DB_PASSWORD")  # Set this environment variable
)

# Define budget categories
BUDGET_CATEGORIES <- c(
  "Housing", "Food", "Clothing", "Education", "Transportation", 
  "Communications", "Health", "Recreation", "Other", "Debt", 
  "Fast Offering", "Tithing", "Income"
)

# Create database connection pool
create_db_pool <- function() {
  tryCatch({
    pool <- dbPool(
      drv = MySQL(),
      host = DB_CONFIG$host,
      port = DB_CONFIG$port,
      dbname = DB_CONFIG$dbname,
      username = DB_CONFIG$username,
      password = DB_CONFIG$password
    )
    
    # Test connection and create table if it doesn't exist
    poolWithTransaction(pool, function(conn) {
      dbExecute(conn, "
        CREATE TABLE IF NOT EXISTS transactions (
          id INT AUTO_INCREMENT PRIMARY KEY,
          date DATE NOT NULL,
          description VARCHAR(255) NOT NULL,
          amount DECIMAL(10,2) NOT NULL,
          vendor VARCHAR(255) NOT NULL,
          budget_category VARCHAR(50) NOT NULL,
          buyer VARCHAR(100) NOT NULL,
          created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
      ")
    })
    
    return(pool)
  }, error = function(e) {
    warning("Could not connect to database: ", e$message)
    return(NULL)
  })
}

# Generate demo data for non-authenticated users
generate_demo_data <- function() {
  set.seed(42)  # For consistent demo data
  
  dates <- seq(from = as.Date("2024-01-01"), to = Sys.Date(), by = "day")
  demo_dates <- sample(dates, 50)
  
  descriptions <- c(
    "Grocery shopping", "Gas station", "Coffee shop", "Restaurant dinner",
    "Electric bill", "Water bill", "Internet bill", "Phone bill",
    "Clothing store", "Online purchase", "Medical appointment", "Pharmacy",
    "Movie tickets", "Gym membership", "Book purchase", "Software subscription",
    "Rent payment", "Car insurance", "Health insurance", "Parking fee"
  )
  
  vendors <- c(
    "Walmart", "Safeway", "Shell", "Starbucks", "Olive Garden",
    "PG&E", "City Water", "Comcast", "Verizon", "Target",
    "Amazon", "Kaiser", "CVS", "AMC Theaters", "Planet Fitness",
    "Barnes Noble", "Adobe", "Property Manager", "State Farm", "Geico"
  )
  
  buyers <- c("John", "Sarah", "Mike", "Emma")
  
  data.frame(
    Date = demo_dates,
    Description = sample(descriptions, 50, replace = TRUE),
    Amount = round(runif(50, 5, 500), 2),
    Vendor = sample(vendors, 50, replace = TRUE),
    Budget_Category = sample(BUDGET_CATEGORIES[BUDGET_CATEGORIES != "Income"], 50, replace = TRUE),
    Buyer = sample(buyers, 50, replace = TRUE),
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(Date))
}

# Load data from database
load_data_from_db <- function(pool) {
  if (is.null(pool)) {
    return(generate_demo_data())
  }
  
  tryCatch({
    data <- pool %>% tbl("transactions") %>% 
      select(date, description, amount, vendor, budget_category, buyer) %>%
      collect() %>%
      rename(
        Date = date,
        Description = description,
        Amount = amount,
        Vendor = vendor,
        Budget_Category = budget_category,
        Buyer = buyer
      ) %>%
      mutate(Date = as.Date(Date)) %>%
      arrange(desc(Date))
    
    if (nrow(data) == 0) {
      return(data.frame(
        Date = as.Date(character()),
        Description = character(),
        Amount = numeric(),
        Vendor = character(),
        Budget_Category = character(),
        Buyer = character(),
        stringsAsFactors = FALSE
      ))
    }
    
    return(data)
  }, error = function(e) {
    warning("Could not load data from database: ", e$message)
    return(generate_demo_data())
  })
}

# Save data to database
save_data_to_db <- function(pool, new_transaction) {
  if (is.null(pool)) {
    return(FALSE)
  }
  
  tryCatch({
    poolWithTransaction(pool, function(conn) {
      dbExecute(conn, "
        INSERT INTO transactions (date, description, amount, vendor, budget_category, buyer)
        VALUES (?, ?, ?, ?, ?, ?)
      ", list(
        new_transaction$Date,
        new_transaction$Description,
        new_transaction$Amount,
        new_transaction$Vendor,
        new_transaction$Budget_Category,
        new_transaction$Buyer
      ))
    })
    return(TRUE)
  }, error = function(e) {
    warning("Could not save to database: ", e$message)
    return(FALSE)
  })
}

# Login UI
login_ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        font-family: 'Arial', sans-serif;
        height: 100vh;
        margin: 0;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      .login-container {
        background: white;
        padding: 40px;
        border-radius: 10px;
        box-shadow: 0 15px 35px rgba(0,0,0,0.1);
        width: 100%;
        max-width: 400px;
        text-align: center;
      }
      .login-title {
        color: #333;
        margin-bottom: 30px;
        font-size: 28px;
        font-weight: 300;
      }
      .form-group {
        margin-bottom: 20px;
        text-align: left;
      }
      .form-control {
        border: 2px solid #f1f1f1;
        border-radius: 5px;
        padding: 12px;
        font-size: 16px;
        width: 100%;
        box-sizing: border-box;
      }
      .form-control:focus {
        border-color: #667eea;
        outline: none;
      }
      .btn-login {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        border: none;
        color: white;
        padding: 12px 30px;
        border-radius: 5px;
        font-size: 16px;
        cursor: pointer;
        width: 100%;
        margin-bottom: 20px;
      }
      .btn-login:hover {
        opacity: 0.9;
      }
      .demo-notice {
        background: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 5px;
        padding: 15px;
        margin-top: 20px;
        font-size: 14px;
        color: #6c757d;
      }
      .error-message {
        color: #dc3545;
        margin-top: 10px;
        font-size: 14px;
      }
    "))
  ),
  div(class = "login-container",
      h2("Budget Tracker", class = "login-title"),
      passwordInput("password", "Enter Password:", 
                    placeholder = "Password", 
                    width = "100%"),
      actionButton("login", "Login", class = "btn-login"),
      actionButton("demo", "View Demo", class = "btn btn-secondary", 
                   style = "width: 100%; margin-bottom: 10px;"),
      textOutput("login_error", class = "error-message"),
      div(class = "demo-notice",
          strong("Demo Mode:"), br(),
          "Click 'View Demo' to explore the app with sample data. ",
          "No password required for demo access."
      )
  )
)

# Main app UI (same as before but updated)
main_ui <- dashboardPage(
  dashboardHeader(
    title = "Budget Tracker",
    tags$li(class = "dropdown",
            actionButton("logout", "Logout", 
                         style = "margin-top: 8px; margin-right: 10px;",
                         class = "btn btn-warning btn-sm")
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Add Transaction", tabName = "add", icon = icon("plus")),
      menuItem("View Transactions", tabName = "view", icon = icon("table")),
      menuItem("Monthly Summary", tabName = "summary", icon = icon("chart-bar")),
      menuItem("Analytics", tabName = "analytics", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 5px;
        }
        .btn-success {
          background-color: #28a745;
          border-color: #28a745;
        }
        .demo-banner {
          background-color: #fff3cd;
          border: 1px solid #ffeaa7;
          border-radius: 4px;
          padding: 10px;
          margin-bottom: 20px;
          text-align: center;
          color: #856404;
        }
      "))
    ),
    
    # Demo mode banner
    conditionalPanel(
      condition = "output.is_demo_mode",
      div(class = "demo-banner",
          icon("info-circle"), " ",
          strong("DEMO MODE:"), " You are viewing sample data. Login with password to access real data."
      )
    ),
    
    tabItems(
      # Add Transaction Tab
      tabItem(tabName = "add",
              fluidRow(
                box(
                  title = "Add New Transaction", status = "primary", solidHeader = TRUE,
                  width = 12,
                  conditionalPanel(
                    condition = "output.is_demo_mode",
                    div(style = "background-color: #f8d7da; padding: 10px; border-radius: 4px; margin-bottom: 15px; color: #721c24;",
                        icon("lock"), " Demo mode: New transactions cannot be saved. Please login to add real data."
                    )
                  ),
                  fluidRow(
                    column(6,
                           dateInput("date", "Date:", value = Sys.Date()),
                           textInput("description", "Description:", placeholder = "Enter transaction description"),
                           numericInput("amount", "Amount ($):", value = 0, step = 0.01)
                    ),
                    column(6,
                           textInput("vendor", "Vendor:", placeholder = "Enter vendor name"),
                           selectInput("category", "Budget Category:", choices = BUDGET_CATEGORIES),
                           textInput("buyer", "Buyer:", placeholder = "Who made this purchase?")
                    )
                  ),
                  br(),
                  actionButton("add_transaction", "Add Transaction", class = "btn btn-success"),
                  br(), br(),
                  verbatimTextOutput("add_status")
                )
              )
      ),
      
      # View Transactions Tab
      tabItem(tabName = "view",
              fluidRow(
                box(
                  title = "Transaction History", status = "primary", solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(4,
                           dateRangeInput("date_range", "Date Range:",
                                          start = floor_date(Sys.Date(), "month"),
                                          end = Sys.Date())
                    ),
                    column(4,
                           selectInput("filter_category", "Filter by Category:",
                                       choices = c("All", BUDGET_CATEGORIES),
                                       selected = "All")
                    ),
                    column(4,
                           br(),
                           actionButton("refresh_data", "Refresh Data", class = "btn btn-info")
                    )
                  ),
                  br(),
                  withSpinner(DT::dataTableOutput("transactions_table")),
                  br(),
                  downloadButton("download_data", "Download CSV", class = "btn btn-secondary")
                )
              )
      ),
      
      # Monthly Summary Tab
      tabItem(tabName = "summary",
              fluidRow(
                box(
                  title = "Monthly Category Totals", status = "primary", solidHeader = TRUE,
                  width = 8,
                  fluidRow(
                    column(6,
                           selectInput("summary_month", "Select Month:",
                                       choices = NULL)
                    ),
                    column(6,
                           numericInput("summary_year", "Year:", value = year(Sys.Date()),
                                        min = 2020, max = 2030)
                    )
                  ),
                  withSpinner(DT::dataTableOutput("monthly_summary"))
                ),
                box(
                  title = "Category Breakdown", status = "info", solidHeader = TRUE,
                  width = 4,
                  withSpinner(plotlyOutput("category_pie_chart"))
                )
              ),
              fluidRow(
                box(
                  title = "Monthly Spending Trend", status = "success", solidHeader = TRUE,
                  width = 12,
                  withSpinner(plotlyOutput("monthly_trend"))
                )
              )
      ),
      
      # Analytics Tab
      tabItem(tabName = "analytics",
              fluidRow(
                box(
                  title = "Spending Analytics", status = "primary", solidHeader = TRUE,
                  width = 6,
                  h4("Total Spending by Category"),
                  withSpinner(plotlyOutput("category_bar_chart"))
                ),
                box(
                  title = "Buyer Analysis", status = "info", solidHeader = TRUE,
                  width = 6,
                  h4("Spending by Buyer"),
                  withSpinner(plotlyOutput("buyer_analysis"))
                )
              ),
              fluidRow(
                box(
                  title = "Daily Spending Pattern", status = "success", solidHeader = TRUE,
                  width = 12,
                  withSpinner(plotlyOutput("daily_spending"))
                )
              )
      )
    )
  )
)

# Main UI logic
ui <- fluidPage(
  uiOutput("page")
)

# Server
server <- function(input, output, session) {
  # Reactive values
  values <- reactiveValues(
    authenticated = FALSE,
    demo_mode = FALSE,
    db_pool = NULL,
    budget_data = NULL
  )
  
  # Initialize database connection
  observe({
    values$db_pool <- create_db_pool()
  })
  
  # Page routing
  output$page <- renderUI({
    if (values$authenticated || values$demo_mode) {
      main_ui
    } else {
      login_ui
    }
  })
  
  # Login logic
  observeEvent(input$login, {
    # Replace "your_password_here" with your actual password
    correct_password <- "budget2025"  # Change this to your desired password
    
    if (input$password == correct_password) {
      values$authenticated <- TRUE
      values$demo_mode <- FALSE
      values$budget_data <- load_data_from_db(values$db_pool)
      output$login_error <- renderText("")
    } else {
      output$login_error <- renderText("Incorrect password. Try demo mode to explore the app.")
    }
  })
  
  # Demo mode
  observeEvent(input$demo, {
    values$authenticated <- FALSE
    values$demo_mode <- TRUE
    values$budget_data <- generate_demo_data()
    output$login_error <- renderText("")
  })
  
  # Logout
  observeEvent(input$logout, {
    values$authenticated <- FALSE
    values$demo_mode <- FALSE
    values$budget_data <- NULL
    updateTextInput(session, "password", value = "")
  })
  
  # Demo mode indicator
  output$is_demo_mode <- reactive({
    values$demo_mode
  })
  outputOptions(output, "is_demo_mode", suspendWhenHidden = FALSE)
  
  # Update month choices for summary
  observe({
    if (values$authenticated || values$demo_mode) {
      updateSelectInput(session, "summary_month",
                        choices = month.name,
                        selected = month.name[month(Sys.Date())])
    }
  })
  
  # Add transaction
  observeEvent(input$add_transaction, {
    if (!values$authenticated && !values$demo_mode) return()
    
    if (input$description == "" || input$amount == 0 || input$vendor == "" || input$buyer == "") {
      output$add_status <- renderText("Please fill in all fields.")
      return()
    }
    
    if (values$demo_mode) {
      output$add_status <- renderText("Demo mode: Cannot save transactions. Please login to add real data.")
      return()
    }
    
    new_transaction <- data.frame(
      Date = input$date,
      Description = input$description,
      Amount = input$amount,
      Vendor = input$vendor,
      Budget_Category = input$category,
      Buyer = input$buyer,
      stringsAsFactors = FALSE
    )
    
    # Save to database
    if (save_data_to_db(values$db_pool, new_transaction)) {
      # Reload data from database
      values$budget_data <- load_data_from_db(values$db_pool)
      
      # Clear inputs
      updateTextInput(session, "description", value = "")
      updateNumericInput(session, "amount", value = 0)
      updateTextInput(session, "vendor", value = "")
      updateTextInput(session, "buyer", value = "")
      
      output$add_status <- renderText("Transaction added successfully!")
    } else {
      output$add_status <- renderText("Error: Could not save transaction to database.")
    }
  })
  
  # Refresh data
  observeEvent(input$refresh_data, {
    if (values$authenticated) {
      values$budget_data <- load_data_from_db(values$db_pool)
    } else if (values$demo_mode) {
      values$budget_data <- generate_demo_data()
    }
  })
  
  # Filter data for display
  filtered_data <- reactive({
    if (is.null(values$budget_data)) return(data.frame())
    
    data <- values$budget_data
    if (nrow(data) == 0) return(data)
    
    # Filter by date range
    if (!is.null(input$date_range)) {
      data <- data[data$Date >= input$date_range[1] & data$Date <= input$date_range[2], ]
    }
    
    # Filter by category
    if (!is.null(input$filter_category) && input$filter_category != "All") {
      data <- data[data$Budget_Category == input$filter_category, ]
    }
    
    return(data)
  })
  
  # Transactions table
  output$transactions_table <- DT::renderDataTable({
    data <- filtered_data()
    if (nrow(data) == 0) {
      return(data.frame(Message = "No transactions found"))
    }
    
    data$Amount <- paste0("$", formatC(data$Amount, format = "f", digits = 2))
    data
  }, options = list(pageLength = 15, scrollX = TRUE))
  
  # Monthly summary
  output$monthly_summary <- DT::renderDataTable({
    if (is.null(values$budget_data)) return(data.frame())
    
    data <- values$budget_data
    if (nrow(data) == 0) {
      return(data.frame(Category = character(), Total = character()))
    }
    
    if (is.null(input$summary_month)) return(data.frame())
    
    selected_month <- match(input$summary_month, month.name)
    
    monthly_data <- data %>%
      filter(month(Date) == selected_month, year(Date) == input$summary_year) %>%
      group_by(Budget_Category) %>%
      summarise(Total = sum(Amount, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(Total))
    
    if (nrow(monthly_data) == 0) {
      return(data.frame(Category = "No data", Total = "$0.00"))
    }
    
    monthly_data$Total <- paste0("$", formatC(monthly_data$Total, format = "f", digits = 2))
    colnames(monthly_data) <- c("Category", "Total")
    monthly_data
  }, options = list(pageLength = 15, dom = 't'))
  
  # Category pie chart
  output$category_pie_chart <- renderPlotly({
    if (is.null(values$budget_data) || is.null(input$summary_month)) return(NULL)
    
    data <- values$budget_data
    if (nrow(data) == 0) return(NULL)
    
    selected_month <- match(input$summary_month, month.name)
    
    pie_data <- data %>%
      filter(month(Date) == selected_month, year(Date) == input$summary_year, Amount > 0) %>%
      group_by(Budget_Category) %>%
      summarise(Total = sum(Amount, na.rm = TRUE), .groups = 'drop')
    
    if (nrow(pie_data) == 0) return(NULL)
    
    p <- plot_ly(pie_data, labels = ~Budget_Category, values = ~Total, type = 'pie',
                 textposition = 'inside', textinfo = 'label+percent') %>%
      layout(showlegend = FALSE, margin = list(l = 0, r = 0, t = 0, b = 0))
    p
  })
  
  # Monthly trend
  output$monthly_trend <- renderPlotly({
    if (is.null(values$budget_data)) return(NULL)
    
    data <- values$budget_data
    if (nrow(data) == 0) return(NULL)
    
    trend_data <- data %>%
      filter(Amount > 0) %>%
      mutate(YearMonth = floor_date(Date, "month")) %>%
      group_by(YearMonth) %>%
      summarise(Total = sum(Amount, na.rm = TRUE), .groups = 'drop') %>%
      arrange(YearMonth)
    
    if (nrow(trend_data) == 0) return(NULL)
    
    p <- plot_ly(trend_data, x = ~YearMonth, y = ~Total, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Monthly Spending Trend",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Total Spending ($)"))
    p
  })
  
  # Category bar chart
  output$category_bar_chart <- renderPlotly({
    if (is.null(values$budget_data)) return(NULL)
    
    data <- values$budget_data
    if (nrow(data) == 0) return(NULL)
    
    cat_data <- data %>%
      filter(Amount > 0) %>%
      group_by(Budget_Category) %>%
      summarise(Total = sum(Amount, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(Total))
    
    if (nrow(cat_data) == 0) return(NULL)
    
    p <- plot_ly(cat_data, x = ~reorder(Budget_Category, Total), y = ~Total, type = 'bar') %>%
      layout(xaxis = list(title = "Category"),
             yaxis = list(title = "Total Spending ($)")) %>%
      layout(xaxis = list(categoryorder = "total ascending"))
    p
  })
  
  # Buyer analysis
  output$buyer_analysis <- renderPlotly({
    if (is.null(values$budget_data)) return(NULL)
    
    data <- values$budget_data
    if (nrow(data) == 0) return(NULL)
    
    buyer_data <- data %>%
      filter(Amount > 0) %>%
      group_by(Buyer) %>%
      summarise(Total = sum(Amount, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(Total))
    
    if (nrow(buyer_data) == 0) return(NULL)
    
    p <- plot_ly(buyer_data, x = ~reorder(Buyer, Total), y = ~Total, type = 'bar') %>%
      layout(xaxis = list(title = "Buyer"),
             yaxis = list(title = "Total Spending ($)")) %>%
      layout(xaxis = list(categoryorder = "total ascending"))
    p
  })
  
  # Daily spending pattern
  output$daily_spending <- renderPlotly({
    if (is.null(values$budget_data)) return(NULL)
    
    data <- values$budget_data
    if (nrow(data) == 0) return(NULL)
    
    daily_data <- data %>%
      filter(Amount > 0) %>%
      group_by(Date) %>%
      summarise(Total = sum(Amount, na.rm = TRUE), .groups = 'drop') %>%
      arrange(Date)
    
    if (nrow(daily_data) == 0) return(NULL)
    
    p <- plot_ly(daily_data, x = ~Date, y = ~Total, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Daily Spending Pattern",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Daily Total ($)"))
    p
  })
  
  # Download data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("budget_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (!is.null(values$budget_data)) {
        write.csv(values$budget_data, file, row.names = FALSE)
      }
    }
  )
  
  # Clean up database connection on session end
  session$onSessionEnded(function() {
    if (!is.null(values$db_pool)) {
      poolClose(values$db_pool)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
