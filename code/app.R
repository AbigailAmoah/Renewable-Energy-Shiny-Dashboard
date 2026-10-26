library(shiny)
library(bslib)
library(tidyverse)
library(scales)
library(plotly)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

load("project_data.RData")
df <- project_data %>% filter(!is.na(continent))

continents <- sort(unique(df$continent))
year_min   <- min(df$year, na.rm = TRUE)
year_max   <- max(df$year, na.rm = TRUE)

world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(name, geometry)

metric_choices <- c(
  "Renewable Energy Consumption (%)" = "renewable_energy",
  "Renewable Electricity Output (%)" = "renewable_electricity",
  "CO2 Emissions (million tonnes)"   = "co2",
  "Electricity Access (%)"           = "electricity_access",
  "GDP per Capita (USD)"             = "gdp_per_capita"
)

metric_label <- function(val) names(metric_choices)[metric_choices == val]


# UI
ui <- page_navbar(
  title = "World Sustainability Explorer",
  theme = bs_theme(bootswatch = "flatly"),
  id    = "navbarID",
  
# Introduction
  nav_panel(
    title = "Introduction",
    br(),
    p(strong("Welcome to the World Sustainability Explorer.")),
    p("This app allows you to interactively explore global sustainability
       indicators across 170+ countries from 2000 to 2018. Use the tabs
       above to navigate between visualizations and discover how the world
       has changed across energy, emissions, and economic dimensions."),
    p(em("How to use: Select a tab above, then use the sidebar panel on the
          left to choose your metric, year, and regions of interest.
          All charts are interactive — hover over any point, bar, or country
          to see detailed values.")),
    br(),
    h4("Key Visualizations"),
    tags$ul(
      tags$li(strong("Global Map:"),
              " A choropleth world map. Choose any sustainability metric and year
          to compare countries side by side. Grey countries have no data
          for the selected metric and year."),
      tags$li(strong("Trends Over Time:"),
              " Line chart of continent-level averages from 2000 to 2018.
          Filter by metric and continent to reveal regional patterns.
          Hover over any point to see the exact value."),
      tags$li(strong("Country Scatter:"),
              " Bubble plot comparing GDP per Capita against any chosen metric,
          with bubble size representing CO2 emissions. Wealthier countries
          tend to emit more CO2 — use the log scale option for a clearer view.")
    ),
    br(),
    h4("Variables Used"),
    tableOutput("var_table"),
    br(),
    h4("Data Source"),
    tags$a(
      "World Sustainability Dataset — Kaggle",
      href   = "https://www.kaggle.com/datasets/truecue/worldsustainabilitydataset",
      target = "_blank"
    )
  ),
  
  # Global Map
  nav_panel(
    title = "Global Map",
    layout_sidebar(
      sidebar = sidebar(
        h5("Map Controls"),
        p(em("Select a metric and year to update the map.",
             style = "font-size:12px; color:#888;")),
        selectInput(
          inputId  = "map_metric",
          label    = "Sustainability Metric:",
          choices  = metric_choices,
          selected = "renewable_energy"
        ),
        sliderInput(
          inputId = "map_year",
          label   = "Year:",
          min = year_min, max = year_max,
          value = year_max, step = 1, sep = ""
        ),
        hr(),
        helpText("Grey = no data for the selected metric and year.
                  Hover over a country to see its value.")
      ),
      h5(textOutput("map_title")),
      plotlyOutput("map_plot", height = "500px")
    )
  ),
  
  # Trends Over Time
  nav_panel(
    title = "Trends Over Time",
    layout_sidebar(
      sidebar = sidebar(
        width = 220,
        h5("Trend Controls"),
        p(em("Select a metric and toggle continents to compare regional trends.",
             style = "font-size:12px; color:#888;")),
        selectInput(
          inputId  = "trend_metric",
          label    = "Sustainability Metric:",
          choices  = metric_choices,
          selected = "renewable_energy"
        ),
        checkboxGroupInput(
          inputId  = "trend_continents",
          label    = "Continents to Display:",
          choices  = continents,
          selected = continents
        ),
        hr(),
        helpText("Lines show the continent average. Hover over any point
                  to see the exact value.")
      ),
      h5(textOutput("trend_title")),
      plotlyOutput("trend_plot", height = "500px")
    )
  ),
  
  # Country Scatter
  nav_panel(
    title = "Country Scatter",
    layout_sidebar(
      sidebar = sidebar(
        h5("Scatter Controls"),
        p(em("Compare GDP against any metric. Bubble size shows CO2 emissions.",
             style = "font-size:12px; color:#888;")),
        selectInput(
          inputId  = "scatter_metric",
          label    = "Metric (Y-Axis):",
          choices  = metric_choices[metric_choices != "gdp_per_capita"],
          selected = "renewable_energy"
        ),
        sliderInput(
          inputId = "scatter_year",
          label   = "Year:",
          min = year_min, max = year_max,
          value = year_max, step = 1, sep = ""
        ),
        checkboxGroupInput(
          inputId  = "scatter_continents",
          label    = "Filter by Continent:",
          choices  = continents,
          selected = continents
        ),
        checkboxInput(
          inputId = "scatter_log",
          label   = "Log scale for GDP (recommended)",
          value   = TRUE
        ),
        hr(),
        helpText("Bubble size = CO2 emissions. Hover over any bubble
                  to see full country details.")
      ),
      h5(textOutput("scatter_title")),
      plotlyOutput("scatter_plot", height = "480px")
    )
  )
)


# SERVER 
server <- function(input, output) {
  
  # Introduction table
  output$var_table <- renderTable({
    tibble(
      Variable = c(
        "Renewable Energy Consumption",
        "Renewable Electricity Output",
        "CO2 Emissions",
        "Electricity Access",
        "GDP per Capita"
      ),
      Unit   = c("%", "%", "Million tonnes", "%", "USD (current)"),
      Source = c(
        "World Bank / IEA",
        "World Bank / IEA",
        "Our World in Data",
        "World Bank",
        "World Bank"
      )
    )
  }, striped = TRUE, bordered = TRUE)
  
  #Global Map
  output$map_title <- renderText({
    paste(metric_label(input$map_metric), "—", input$map_year)
  })
  
  output$map_plot <- renderPlotly({
    metric <- input$map_metric
    yr     <- input$map_year
    
    country_data <- df %>%
      filter(year == yr) %>%
      select(country, val = all_of(metric))
    
    map_df <- world_sf %>%
      left_join(country_data, by = c("name" = "country")) %>%
      mutate(tooltip = paste0(
        "<b>", name, "</b><br>",
        metric_label(metric), ": ",
        ifelse(is.na(val), "No data", round(val, 2))
      ))
    
    p <- ggplot(map_df) +
      geom_sf(aes(fill = val, text = tooltip),
              color = "white", linewidth = 0.1) +
      scale_fill_viridis_c(
        name     = metric_label(metric),
        na.value = "#d3d3d3",
        labels   = comma
      ) +
      labs(caption = "Source: World Bank / Our World in Data") +
      theme_void() +
      theme(
        legend.position  = "bottom",
        legend.key.width = unit(2.5, "cm"),
        plot.background  = element_rect(fill = "white", color = NA)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(margin = list(l = 0, r = 0, t = 10, b = 0)) %>%
      config(displayModeBar = FALSE)
  })
  
  # Trends Over Time
  output$trend_title <- renderText({
    paste(metric_label(input$trend_metric), "by Continent, 2000–2018")
  })
  
  output$trend_plot <- renderPlotly({
    req(length(input$trend_continents) > 0)
    metric <- input$trend_metric
    
    trend_data <- df %>%
      filter(continent %in% input$trend_continents) %>%
      group_by(continent, year) %>%
      summarise(
        avg = mean(.data[[metric]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(avg)) %>%
      mutate(tooltip = paste0(
        "<b>", continent, "</b> (", year, ")<br>",
        metric_label(metric), ": ", round(avg, 2)
      ))
    
    validate(need(nrow(trend_data) > 0, "No data for selected continents."))
    
    p <- ggplot(trend_data,
                aes(x = year, y = avg, color = continent,
                    group = continent, text = tooltip)) +
      geom_line(linewidth = 1.6) +
      geom_point(size = 3.5) +
      scale_x_continuous(breaks = seq(2000, 2018, 2), expand = c(0.02, 0.02)) +
      scale_y_continuous(labels = comma, expand = c(0.05, 0.05)) +
      scale_color_brewer(palette = "Set1") +
      labs(
        x       = "Year",
        y       = metric_label(metric),
        color   = "Continent",
        caption = "Source: World Bank / Our World in Data"
      ) +
      theme_bw(base_size = 14) +
      theme(
        legend.position  = "bottom",
        panel.grid.minor = element_blank(),
        plot.caption     = element_text(color = "#888888", size = 9)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.1, y = -0.2)) %>%
      config(displayModeBar = FALSE)
  })
  
  # Country Scatter
  output$scatter_title <- renderText({
    paste(metric_label(input$scatter_metric), "vs GDP per Capita —",
          input$scatter_year)
  })
  
  output$scatter_plot <- renderPlotly({
    req(length(input$scatter_continents) > 0)
    metric <- input$scatter_metric
    yr     <- input$scatter_year
    
    scatter_data <- df %>%
      filter(
        year == yr,
        continent %in% input$scatter_continents,
        !is.na(gdp_per_capita),
        !is.na(.data[[metric]]),
        !is.na(co2)
      ) %>%
      mutate(tooltip = paste0(
        "<b>", country, "</b><br>",
        "Continent: ", continent, "<br>",
        "GDP per Capita: $", comma(round(gdp_per_capita)), "<br>",
        metric_label(metric), ": ", round(.data[[metric]], 2), "<br>",
        "CO2 Emissions: ", round(co2, 1), " million tonnes"
      ))
    
    validate(need(nrow(scatter_data) > 0, "No data for the selected filters."))
    
    p <- ggplot(scatter_data,
                aes(x = gdp_per_capita, y = .data[[metric]],
                    size = co2, color = continent, text = tooltip)) +
      geom_point(alpha = 0.7) +
      scale_size_continuous(range = c(2, 18), guide = "none") +
      scale_color_brewer(palette = "Set1") +
      scale_y_continuous(labels = comma) +
      labs(
        x       = ifelse(input$scatter_log,
                         "GDP per Capita — log scale (USD)",
                         "GDP per Capita (USD)"),
        y       = metric_label(metric),
        color   = "Continent",
        caption = "Bubble size = CO2 emissions. Source: World Bank"
      ) +
      theme_bw() +
      theme(legend.position = "bottom")
    
    if (input$scatter_log) p <- p + scale_x_log10(labels = dollar)
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.1, y = -0.2)) %>%
      config(displayModeBar = FALSE)
  })
}

shinyApp(ui = ui, server = server)
