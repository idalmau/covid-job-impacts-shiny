# --- autor: Iñigo Dalmau (i.dalmau@outlook.com)
# --- propósito: 'covid-job-impacts' shiny.io app script, módulo de visualización máster Data Science (UCM)


###################################
#                                 #
# -------- PASOS PREVIOS -------- #
#                                 #
###################################

# --- librerías ---
# -- instalación
if (!"tidyverse" %in% installed.packages()) install.packages("tidyverse")
if (!"plotly" %in% installed.packages()) install.packages("plotly")
if (!"hrbrthemes" %in% installed.packages()) install.packages("hrbrthemes")
if (!"magrittr" %in% installed.packages()) install.packages("magrittr")
if (!"zoo" %in% installed.packages()) install.packages("zoo")
if (!"usmap" %in% installed.packages()) install.packages("usmap")
if (!"gghighlight" %in% installed.packages()) install.packages("gghighlight")
if (!"shinyWidgets" %in% installed.packages()) install.packages("shinyWidgets")
# -- carga
library(tidyverse)        # múltiples paquetes
library(plotly)           # gráficos interactivos
library(hrbrthemes)       # temáticas alternativas para gráficos
library(magrittr)         # manipulación de datos
library(zoo)              # manipulación de fechas
library(usmap)            # gráficos con mapas de EEUU
library(gghighlight)      # destacar visuales para gráficos
# -- librería shiny
library(shiny)
library(shinyWidgets)

# --- funciones ---
monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

# --- lectura de archivos
tot <- read_csv('https://greenwichhr-covidjobimpacts.s3.us-east-2.amazonaws.com/overall.csv.part_00000') %>%
  mutate(industry = "ALL") %>%
  select(post_date, industry, count_id_indexed)
ind <- read_csv('https://greenwichhr-covidjobimpacts.s3.us-east-2.amazonaws.com/industry.csv.part_00000') %>% 
  filter(industry != 'NA') 
geo <- read_csv('https://greenwichhr-covidjobimpacts.s3.us-east-2.amazonaws.com/geography.csv.part_00000') %>% 
  filter(state != 'NA') %>% 
  mutate(month = as.Date(as.yearmon(post_date))) %>%
  mutate(state = str_replace_all(state,"[^[:graph:]]", " ")) %>%
  filter(toupper(state) %in% state.abb)

# --- modificación archivos
# -- crear dfs con la media del índice
# por día #
ind_by_day <- inner_join(
  x = ind,
  y = ind %>% 
    group_by(industry) %>%
    summarise(avg_index = mean(count_id_indexed)),
  by = "industry"
  )
tot_by_day <-  inner_join(
  x = tot,
  y = tot %>% 
    group_by(industry) %>%
    summarise(avg_index = mean(count_id_indexed)),
  by = "industry"
)
# juntar dataframes con totales
ind_tot_by_day <- rbind(tot_by_day, ind_by_day)

# por mes #
ind_by_month <- ind %>%
  mutate(month = as.Date(as.yearmon(post_date))) %>%
  group_by(industry, month) %>%
  summarise(avg_index = mean(count_id_indexed)) %>%
  mutate(avg_index_tot = mean(avg_index))
geo_by_month <- geo %>%
  group_by(month, state) %>%
  summarise(avg_index = mean(count_id_indexed))

# por estado #
geo_tot <- geo %>%
  group_by(state) %>%
  summarise(avg_index = mean(count_id_indexed))


###################################
#                                 #
# ------------- APP ------------- #
#                                 #
###################################

# definimos la lógica de la UI
ui <- navbarPage("Covid Job Impacts",
    
    # panel de Serie Temporal
    tabPanel("Serie Temporal",
        
        # título del panel
        titlePanel("COVID19: Creación de empleo (USA)"),
        
        # sidebar con diferentes inputs del usuario
        sidebarLayout(
          sidebarPanel(
            # decisión de incluir totales
            checkboxInput("total_ts", "Mostrar Total", FALSE),
            # decisión de elegir el rango de fecha
            dateRangeInput("daterange_ts", "Rango: Fecha de Inicio y Final:",
                           start  = min(ind_tot_by_day$post_date),
                           end    = max(ind_tot_by_day$post_date),
                           min    = min(ind_tot_by_day$post_date),
                           max    = max(ind_tot_by_day$post_date),
                           format = "dd/mm/yyyy",
                           separator = " - "
            ),
            # botón para reiniciar rango de fechas
            actionButton("reset_daterange_ts","Reiniciar Rango"),
            # linebreak
            htmlOutput("br_ts_sidebar"),
            # decisión de elegir destacar las categorías mayores o menores que el valor del índice seleccionado
            selectInput("upper_lower_choice","Criterio para destacar categorías por Índice Medio:",
                        choices = c("Mayor (>)", "Menor (<)"),
                        selected = "Mayor (>)"
            ),
            # decisión de valor del índice
            sliderInput("index_threshold", "Índice medio:", 
                        value = 1, 
                        min = plyr::round_any(min(ind_tot_by_day$avg_index), 0.1, f = ceiling), 
                        max = plyr::round_any(max(ind_tot_by_day$avg_index), 0.1, f = floor),
                        step = 0.025
            )
          ),
          
          # panel principal 1
          mainPanel(
            # texto
            htmlOutput("txt_ts"),
            # gráfico
            plotlyOutput("ts")
            
          )
        )
    ),
    
    tabPanel("Boxplot",
            
        # título de panel
        titlePanel("COVID19: Creación de empleo (USA)"),
        
        # sidebar con diferentes inputs del usuario
        sidebarLayout(
          sidebarPanel(
            # decisión para elegir el rango de meses
            airDatepickerInput("daterange_box", "Rango: Mes de Inicio y Final:",
                               range = TRUE,
                               minDate = min(ind_by_month$month) + 30,
                               maxDate = max(ind_by_month$month) + 15,
                               value = c(min(ind_by_month$month) + 30, max(ind_by_month$month) + 15),
                               dateFormat = "mm/yyyy",
                               separator = " - ",
                               autoClose = TRUE,
                               view = "months",
                               minView = "months",
                               language = 'es',
                               clearButton = TRUE,
                               toggleSelected = TRUE,
                               addon = "left"
            ),
            # botón para reiniciar rango de fechas
            actionButton("reset_daterange_box","Reiniciar Rango"),
            # linebreak
            htmlOutput("br_box_sidebar"),
            # decisión para elegir categorías
            pickerInput(
              inputId = "industry_choice",
              label = "Industria:", 
              choices = unique(ind_by_month$industry),
              options = pickerOptions('actionsBox' = TRUE,
                             liveSearch = TRUE,
                             selectedTextFormat = "count > 3",
                             virtualScroll = TRUE,
                             selectAllText = "Seleccionar Todas",
                             deselectAllText = "Deseleccionar Todas",
                             noneSelectedText = "Ninguna seleccionada"),
              multiple = TRUE,
              selected = unique(ind_by_month$industry)
            )
          ),
          
          # panel principal 2
          mainPanel(
            # texto
            htmlOutput("txt_box"),
            # gráfico
            plotlyOutput("boxplot")
          )
        )
    ),
    
    tabPanel("Mapa",
        
        # título del panel
        titlePanel("COVID19: Creación de empleo (USA)"),
        
        # sidebar con diferentes inputs del usuario
        sidebarLayout(
          sidebarPanel(
            # texto sidebar
            h5("El gráfico puede ser pintado por índice medio total o por mes."),
            htmlOutput("txt_map_sidebar"),
            # decisión de cambiar el gráfico a meses
            selectInput("date_map_choice", "Índice Medio", 
                        choices = c("Total", "Por Mes"),
                        selected = "Total"),
            # decisión de mes
            airDatepickerInput("date_map", "Mes:",
                      minDate = min(geo$month) + 30,
                      maxDate = max(geo$month) + 15,
                      dateFormat = "mm/yyyy",
                      value = NULL,
                      clearButton = TRUE,
                      view = "months",
                      minView = "months",
                      autoClose = TRUE,
                      language = 'es',
                      addon = "left"
            )
          ),
          
          # panel principal
          mainPanel(
            # texto
            htmlOutput("txt_map"),
            # gráfico
            plotlyOutput("map")
          )
        )
    )
)

# definimos la lógica del servidor
server <- function(input, output, session) {
 
# -------------------- SERIE TEMPORAL -------------------- 
     
  # reiniciar rango de fechas Serie Temporal
  observeEvent( input$reset_daterange_ts, {
    updateDateRangeInput(session, "daterange_ts", 
                       start  = min(ind_tot_by_day$post_date),
                       end    = max(ind_tot_by_day$post_date))
  })
  
  # dataframe para la Serie Temporal
    # en función de si el usuario quiere incluir el total o no
    filter_choice <-  reactive({
      # mensaje en caso de input erróneo
      validate(need(input$daterange_ts[1] < input$daterange_ts[2], "Error: la fecha de inicio debe ser menor que la de final"))
      # dataframe a escoger
      if (input$total_ts) {ind_tot_by_day} else {ind_by_day} %>% 
        # opción de rango de fecha de inicio y final
        filter(post_date >= input$daterange_ts[1], 
               post_date <= input$daterange_ts[2])
    })
    
    # gráfico Serie Temporal
    output$ts <- renderPlotly({
      
      # datos ya filtrados
      myfiltered_df <- filter_choice()
      
      # pintamos el gráfico
      p <- ggplot(myfiltered_df,
                  aes(x=post_date, y=count_id_indexed, 
                      color=industry, 
                      group=industry) ) +
        geom_line(size = 0.5) + 
        labs(title = "Índice de oferta de empleo durante la pandemia, por industria",
             subtitle = "Referencia para el índice: 01/03/2020",
             colour = "Industria") +
        xlab(NULL) + 
        ylab("Índice diaro por industria") +
        theme_bw() +
        theme(plot.title = element_text(face = "bold")) +
        geom_hline(yintercept = 1, color = "red", lty = 5, lwd = 0.8, alpha = 0.4) +
        gghighlight(
          if(input$upper_lower_choice == "Mayor (>)") {
            avg_index > input$index_threshold
          } else {
            avg_index < input$index_threshold
          },
          use_direct_label = FALSE)
      # interactivo
      ggplotly(p,
               height = 500, 
               width = 800)
      
    })
    
    # texto Serie Temporal
    output$txt_ts <- renderUI({
      str1 <- "Los datos muestran la evolución del índice de creación de empleo en Estados Unidos durante la pandemia COVID-19."
      str2 <- "El índice se basa en el número de puestos de trabajo disponibles por industria en el momento de registro, con carácter <b>diario</b>."
      str3 <- "Referencia para el ínidice: <b>01/03/2020</b>."
      tag <- tags$a(href = "https://registry.opendata.aws/us-hiring-rates-pandemic/", 
             "Fuente: Covid Job Impacts (Greenwich.HR)", target = "_blank")
      br <- "<br/>"
      HTML(paste(str1, str2, str3, tag, br, sep = '<br/>'))
      
    })
    
    # linebreak Serie Temporal sidebar
    output$br_ts_sidebar <- renderUI({
      HTML(paste("<br/>","<br/>"))
    })
    
# -------------------- BOXPLOT -------------------- 
    
  # reiniciar rango de fechas Boxplot
  observeEvent( input$reset_daterange_box , {
    updateAirDateInput(session, "daterange_box", value = c(min(ind_by_month$month) + 30, max(ind_by_month$month) + 15))
  })
  
  # dataframe para el Boxplot
    filter_box <-  reactive({
      # mensaje en caso de input de fecha erróneo
      validate(need(input$daterange_box[1] < input$daterange_box[2], "Error: la fecha de inicio debería ser menor que la de final"))
      # mensaje en caso de input de categoría erróneo
      validate(need(length(input$industry_choice) >= 1, "Error: se necesita al menos una industria para pintar el gráfico"))
      # opción de categorías
      ind_by_month %>% 
        filter(industry %in% input$industry_choice) %>%
        # selección rango de fecha inicio y final
        filter(month >= monthStart(input$daterange_box[1]), 
               month <= monthStart(input$daterange_box[2]))
    })
    
    # gráfico Boxplot
    output$boxplot <- renderPlotly({
      
      # datos ya filtrados
      myfiltered_df <- filter_box()
      
      # pintamos el gráfico 
      p <- ggplot(myfiltered_df, 
                  aes(x=fct_reorder(industry, avg_index, .fun = mean, .desc = T), 
                      y=avg_index,
                      fill=avg_index_tot)) + 
        geom_boxplot() + 
        scale_x_discrete(guide = guide_axis(angle = 45)) +
        labs(title = "Índice de oferta de empleo durante la pandemia, por industria",
             subtitle = "Referencia para el índice: 01/03/2020",
        ) +
        ylab("Índice medio por mes") + 
        xlab(NULL) +
        theme_bw(base_size = 12) +
        theme(plot.title = element_text(face = "bold")) +
        scale_fill_gradient2(
          low = "#A7171A",
          high = "#1AA717",
          midpoint = 1,
          name = "Índice\nMedio"
        ) +
        geom_hline(yintercept = 1, color = "blue", lty = 5, lwd = 0.8, alpha = 0.4)
      # interactivo
      ggplotly(p, 
               height = 500, 
               width = 770) %>% 
        layout(xaxis = list(tickangle=315))
      
    })
    
    # texto Boxplot
    output$txt_box <- renderUI({
      str1 <- "Cada valor del gráfico representa el índice medio de creación de empleo <b>por mes</b>, para cada industria, ordenadas de mayor a menor índice medio."
      str2 <- "Un mayor valor de la mediana refleja un mayor número de meses con más empleos disponibles que antes de la pandemia."
      str3 <- "Referencia para el ínidice: <b>01/03/2020</b>."
      tag <- tags$a(href = "https://registry.opendata.aws/us-hiring-rates-pandemic/", 
                    "Fuente: Covid Job Impacts (Greenwich.HR)", target = "_blank")
      br <- "<br/>"
      HTML(paste(str1, str2, str3, tag, br, sep = '<br/>'))
    })
    
    # linebreak Boxplot sidebar
    output$br_box_sidebar <- renderUI({
      HTML(paste("<br/>","<br/>"))
    })
    
# -------------------- MAPA -------------------- 
    
  # dataframe para el Mapa  
    # en función de si el usuario quiere incluir representar mensualidad o no
    filter_map <-  reactive({
      # mensaje en caso de seleccionar fecha y total a la vez
      if (input$date_map_choice == "Total") {
        validate(need(length(input$date_map) == 0, "Error: no se puede especificar fecha para valores totales -> seleccione índice medio 'Por Mes' o borre la selección de mes"))
      }
      if(input$date_map_choice == "Por Mes"){
        # mensaje en caso de input erróneo
        validate(need(length(input$date_map) > 0, "Error: mes no seleccionado -> seleccione un mes o cambie a gráfico de totales"))
        # opción de mes
        geo_by_month %>%
          filter(month == monthStart(input$date_map))
      } else {
        # opción de totales (sin mes)
        geo_tot
      }

    })
    
    # gráfico Mapa
    output$map <- renderPlotly({
      
      # datos ya filtrados
      myfiltered_df <- filter_map()
      
      # pintamos el gráfico 
      p <- plot_usmap(data = myfiltered_df, values = "avg_index", regions = "state", color = "black", include = ) + 
        scale_fill_gradient2(
          low = "#A7171A",
          high = "#1AA717",
          midpoint = 1,
          name = "Índice\nMedio",
          label = scales::comma
        ) +
        labs(title = "Índice de oferta de empleo durante la pandemia, por estado", 
             subtitle = "Referencia para el índice: 01/03/2020") +
        theme(plot.title = element_text(face = "bold"),
              legend.position = "right")
      # interactivo
      ggplotly(p, 
               height = 500)
    })
    
    # texto Mapa sidebar
    output$txt_map_sidebar <- renderUI({
      HTML(paste("Pulsa en el botón 'Limpiar' dentro del seleccionador de fecha para borrar la selección de mes. <br/>", "<br/>"))
    })
    
    # texto Mapa
    output$txt_map <- renderUI({
      str1 <- "Cada estado representa el índice medio de creación de empleo <b>total</b> durante la pandemia, coloreados de mayor a menor índice medio."
      str2 <- "También se puede explorar el índice medio por mes, activando el checkbox. Si este no está activado, se muestra el gráfico de totales."
      str3 <- "Referencia para el ínidice: <b>01/03/2020</b>."
      tag <- tags$a(href = "https://registry.opendata.aws/us-hiring-rates-pandemic/", 
                    "Fuente: Covid Job Impacts (Greenwich.HR)", target = "_blank")
      br <- "<br/>"
      HTML(paste(str1, str2, str3, tag, br, sep = '<br/>'))
      
    })
    
  }

# activamos la aplicación
shinyApp(ui = ui, server = server)
