
# - Packages: shiny, bslib, dplyr, tidyr, lubridate, tsibble,
#             fable, fabletools, ggplot2, readr


library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(lubridate)
library(tsibble)
library(fable)
library(fabletools)
library(ggplot2)
library(readr)

# LOAD & WRANGLE

wines_raw <- read_csv("AustralianWines.csv")

# Clean Month and fix Rose column, create tsibble
wines_clean <- wines_raw %>%
  mutate(
    Month = yearmonth(my(Month)),  # "Jan-80" -> yearmonth
    Rose  = as.numeric(Rose)       # coerce Rose to numeric 
  )

wines_long <- wines_clean %>%
  pivot_longer(
    cols = -Month,
    names_to  = "Varietal",
    values_to = "Sales"
  ) %>%
  as_tsibble(index = Month, key = Varietal)

# ontrols
all_varietals <- sort(unique(wines_long$Varietal))
min_month <- min(wines_long$Month)
max_month <- max(wines_long$Month)

# About tab text
about_html <- HTML(
  "
<h3>About This App</h3>
<p>
This Shiny application was created for ADS-506 to explore and forecast monthly Australian wine sales by varietal.
The dataset, <code>AustralianWines.csv</code>, contains monthly sales volumes (in thousands of liters)
for six varietals: Dry White, Fortified, Red, Rose, Sparkling, and Sweet White, spanning the 1980s and early 1990s.
</p>

<h4>Modeling Approach</h4>
<p>
To compare forecasting performance across varietals, the app automatically fits three time-series models
using the <strong>fable</strong> framework:
</p>
<ul>
<li><strong>TSLM</strong> – a time series linear model with trend and seasonal predictors;</li>
<li><strong>ETS</strong> – exponential smoothing with automatic selection of error, trend, and seasonality components;</li>
<li><strong>ARIMA</strong> – autoregressive integrated moving average with automatic order selection.</li>
</ul>
<p>
Data are converted into a tsibble keyed by varietal, split into training and validation periods based on the
selected date range and training proportion. Forecasts include 80% and 95% prediction intervals to quantify uncertainty.
</p>

<h4>How to Use the App</h4>
<ol>
<li>Select one or more wine varietals from the sidebar.</li>
<li>Adjust the date range and the training window percentage.</li>
<li>Choose a forecast horizon (in months).</li>
<li>Select which models (TSLM, ETS, ARIMA) to display.</li>
</ol>
<p>
The app will update:
</p>
<ul>
<li>An overview plot of historical sales;</li>
<li>A forecast plot with prediction intervals;</li>
<li>Accuracy metrics (RMSE, MAE, MAPE) for both Training and Validation windows;</li>
<li>A table of model specifications (ETS form and ARIMA orders) per varietal.</li>
</ul>

<h4>Reproducibility</h4>
<p>
All figures and metrics are generated using the R packages <code>tsibble</code>, <code>fable</code>, <code>fabletools</code>,
<code>ggplot2</code>, <code>dplyr</code>, and <code>shiny</code>. To reproduce these results outside the app, follow the
same steps: read the CSV, form a tsibble (index = Month, key = Varietal), split into training/validation,
fit TSLM / ETS / ARIMA models, generate forecasts, and compute accuracy using <code>fabletools::accuracy()</code>.
</p>


# UI

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("Australian Wines Forecasting App"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Varietal selection 
      selectInput(
        "varietal",
        "Select varietal(s):",
        choices  = all_varietals,
        selected = all_varietals[1:2],
        multiple = TRUE
      ),
      
      # Date range selection for analysis window
      sliderInput(
        "date_range",
        "Date range:",
        min   = as.Date(min_month),
        max   = as.Date(max_month),
        value = c(as.Date(min_month), as.Date(max_month)),
        timeFormat = "%Y-%m"
      ),
      
      # Training proportion slider
      sliderInput(
        "train_prop",
        "Training window (% of selected period):",
        min   = 50,
        max   = 90,
        value = 80,
        step  = 5
      ),
      
      # Forecast horizon
      numericInput(
        "h",
        "Forecast horizon (months):",
        value = 12,
        min   = 3,
        max   = 60
      ),
      
      # Extra UX: user chooses which models to display
      checkboxGroupInput(
        "model_choices",
        "Models to display:",
        choices  = c("TSLM", "ETS", "ARIMA"),
        selected = c("TSLM", "ETS", "ARIMA")
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Overview",
          br(),
          plotOutput("overview_plot"),
          br(),
          p("Overview of monthly Australian wine sales by varietal over the selected date range.")
        ),
        tabPanel(
          "Forecasts & Accuracy",
          br(),
          plotOutput("forecast_plot"),
          br(),
          h4("Model Accuracy (Training vs Validation)"),
          p("Metrics are computed using fabletools::accuracy() for both the training and validation windows."),
          tableOutput("metrics_table")
        ),
        tabPanel(
          "Model Specs",
          br(),
          p("ETS component forms and ARIMA orders per varietal and model."),
          tableOutput("specs_table")
        ),
        tabPanel(
          "About",
          br(),
          uiOutput("about_tab")
        )
      )
    )
  )
)


# SERVER

server <- function(input, output, session) {
  

  # Filtered data (by varietal + date range)
  filtered_data <- reactive({
    req(input$varietal)
    
    wines_long %>%
      filter(
        Varietal %in% input$varietal,
        Month >= yearmonth(input$date_range[1]),
        Month <= yearmonth(input$date_range[2])
      ) %>%
      arrange(Varietal, Month)
  })
  

  # Overview plot
  output$overview_plot <- renderPlot({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = as.Date(Month), y = Sales, color = Varietal)) +
      geom_line() +
      facet_wrap(~ Varietal, scales = "free_y") +
      labs(
        title = "Australian Wine Sales by Varietal",
        x = "Month",
        y = "Sales (thousand liters)",
        color = "Varietal"
      ) +
      theme_minimal()
  })

  # Train / Validation Split
  split_data <- reactive({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    # Ordered unique time index within filtered window
    idx <- df %>%
      distinct(Month) %>%
      arrange(Month) %>%
      pull(Month)
    
    # Training proportion (from slider, 50–90%)
    pos <- floor(length(idx) * input$train_prop / 100)
    pos <- max(1, min(pos, length(idx)))  # guard rails
    
    train_end <- idx[pos]
    
    training <- df %>%
      filter(Month <= train_end)
    
    validation <- df %>%
      filter(Month > train_end)
    
    list(
      training   = training,
      validation = validation,
      train_end  = train_end
    )
  })
  

  # Model, Forecast, Accuracy

  models_and_forecasts <- reactive({
    sp   <- split_data()
    train <- sp$training
    valid <- sp$validation
    
    req(nrow(train) > 0, nrow(valid) > 0)
    
    # Ensure tsibble structure
    train_ts <- train %>%
      as_tsibble(index = Month, key = Varietal)
    
    valid_ts <- valid %>%
      as_tsibble(index = Month, key = Varietal)
    
    # Fit models per varietal
    wine_models <- train_ts %>%
      model(
        TSLM  = TSLM(Sales ~ trend() + season()),
        ETS   = ETS(Sales),
        ARIMA = ARIMA(Sales)
      )
    
    # Forecast:  future horizon & validation window
    fc_future <- wine_models %>%
      forecast(h = input$h)
    
    fc_valid <- wine_models %>%
      forecast(new_data = valid_ts)
    
    # Accuracy: training & validation
    acc_train <- wine_models %>%
      accuracy() %>%
      mutate(Window = "Training")
    
    acc_valid <- fc_valid %>%
      accuracy(valid_ts) %>%
      mutate(Window = "Validation")
    
    acc_all <- bind_rows(acc_train, acc_valid) %>%
      select(Varietal, .model, Window, RMSE, MAE, MAPE)
    
    list(
      train      = train_ts,
      valid      = valid_ts,
      models     = wine_models,
      fc_future  = fc_future,
      fc_valid   = fc_valid,
      acc        = acc_all
    )
  })
  

  # Forecast plot with prediction intervals

  output$forecast_plot <- renderPlot({
    mf <- models_and_forecasts()
    
    train <- mf$train
    valid <- mf$valid
    fc_future <- mf$fc_future
    
    # Filter forecasts by selected models
    fc_filtered <- fc_future %>%
      filter(.model %in% input$model_choices)
    
    # Combine train + validation for actuals
    hist_data <- bind_rows(
      train %>% mutate(Set = "Training"),
      valid %>% mutate(Set = "Validation")
    )
    
    autoplot(fc_filtered, hist_data, level = c(80, 95)) +
      facet_wrap(~ Varietal, scales = "free_y") +
      labs(
        title  = "Forecasts by Model with Prediction Intervals",
        x      = "Month",
        y      = "Sales (thousand liters)",
        colour = "Model",
        fill   = "Interval"
      ) +
      theme_minimal()
  })
  

  # Accuracy table (Training vs Validation)

  output$metrics_table <- renderTable({
    mf <- models_and_forecasts()
    
    mf$acc %>%
      filter(.model %in% input$model_choices) %>%
      arrange(Varietal, .model, Window)
  })
  

  # Model specs table (ETS form, ARIMA order)

  output$specs_table <- renderTable({
    mf <- models_and_forecasts()
    models <- mf$models
    
    specs <- models %>%
      glance() %>%
      as_tibble() %>%
      mutate(
        ETS_form    = ifelse(.model == "ETS", method, NA_character_),
        ARIMA_order = ifelse(.model == "ARIMA", arima, NA_character_)
      ) %>%
      filter(.model %in% input$model_choices) %>%
      select(Varietal, .model, ETS_form, ARIMA_order)
    
    specs
  })
  

  # About tab

  output$about_tab <- renderUI({
    about_html
  })
}


# Run app

shinyApp(ui, server)
