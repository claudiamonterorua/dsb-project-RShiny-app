# ==========================================
# LIBRERÍAS
# ==========================================
library(shiny)
library(shinyjs)
library(mongolite)
library(DT)
library(dplyr)
library(lubridate)
library(stringr)
library(jsonlite)
library(rmarkdown)

# ==========================================
# CONEXIONES
# ==========================================
con <- mongo(
  collection = "patient-collection",
  db = "hospital_db",
  url = "mongodb+srv://team:Group4@dsb-cluster.0auppui.mongodb.net/"
)

log_con <- mongo(
  collection = "audit-log",
  db = "hospital_db",
  url = "mongodb+srv://team:Group4@dsb-cluster.0auppui.mongodb.net/"
)

# ==========================================
# 🔹 MODULE: DATA
# ==========================================
mod_data_ui <- function(id){
  ns <- NS(id)

  tagList(
    downloadButton(ns("report"), "Generate Report"),
    br(), br(),
    DTOutput(ns("tabla"))
  )
}

mod_data_server <- function(id, data){

  moduleServer(id, function(input, output, session){

    output$tabla <- renderDT({

      df <- data()

      datatable(
        df %>%
          select(
            `Patient ID`,
            `Birth Date`,
            `Age`,
            `Sex`,
            `Weight (kg)`,
            `Height (m)`,
            `Blood Type`,
            `Diagnosis Code`,
            `Dosage (mg)`,
            `Smoker`,
            `Doctor`
          )
      )

    })

    # ==========================================
    # REPORT
    # ==========================================
    output$report <- downloadHandler(

      filename = function(){
        paste0("report_", Sys.Date(), ".html")
      },

      content = function(file){

        df <- data()

        temp_rmd <- tempfile(fileext = ".Rmd")
        temp_html <- tempfile(fileext = ".html")

        writeLines(c(

          "---",
          "title: 'Database Summary Report'",
          "output: html_document",
          "params:",
          "  data: NA",
          "---",

          "```{r setup, include=FALSE}",
          "library(dplyr)",
          "library(ggplot2)",
          "df <- params$data",
          "```",

          "# 📊 Overview",

          "Total patients: `r nrow(df)`",

          "",

          "# 📋 Global Quality Metrics",

          "```{r, echo=FALSE}",
          "",
          "cols <- c(",
          "  'Patient ID',",
          "  'Birth Date',",
          "  'Age',",
          "  'Sex',",
          "  'Weight (kg)',",
          "  'Height (m)',",
          "  'Blood Type',",
          "  'Diagnosis Code',",
          "  'Dosage (mg)',",
          "  'Smoker',",
          "  'Doctor'",
          ")",
          "",
          "total <- nrow(df)",
          "",
          "# ==========================================",
          "# COMPLETENESS GLOBAL",
          "# ==========================================",
          "completeness_values <- sapply(cols, function(col){",
          "",
          "  errors <- sum(",
          "    is.na(df[[col]]) |",
          "    df[[col]] == 'NA'",
          "  )",
          "",
          "  round((1 - errors / total) * 100, 2)",
          "",
          "})",
          "",
          "completeness <- round(",
          "  mean(completeness_values),",
          "  2",
          ")",
          "",
          "# ==========================================",
          "# CONSISTENCY",
          "# ==========================================",
          "get_consistency <- function(df, col){",
          "",
          "  if(col == 'Age'){",
          "",
          "    df %>%",
          "      filter(",
          "        !is.na(Age),",
          "        !is.na(`Birth Date`)",
          "      ) %>%",
          "      mutate(",
          "        calculated_age = floor(",
          "          time_length(",
          "            interval(as.Date(`Birth Date`), Sys.Date()),",
          "            'years'",
          "          )",
          "        )",
          "      ) %>%",
          "      filter(Age != calculated_age)",
          "",
          "  } else {",
          "",
          "    data.frame()",
          "",
          "  }",
          "",
          "}",
          "",
          "consistency_values <- sapply(cols, function(col){",
          "",
          "  errors <- nrow(get_consistency(df, col))",
          "",
          "  round((1 - errors / total) * 100, 2)",
          "",
          "})",
          "",
          "consistency <- round(",
          "  mean(consistency_values),",
          "  2",
          ")",
          "",
          "# ==========================================",
          "# ACCURACY",
          "# ==========================================",
          "get_accuracy <- function(df, col){",
          "",
          "  if(col == 'Age'){",
          "",
          "    df %>% filter(Age > 120)",
          "",
          "  } else if(col == 'Weight (kg)'){",
          "",
          "    df %>% filter(`Weight (kg)` > 300)",
          "",
          "  } else if(col == 'Height (m)'){",
          "",
          "    df %>% filter(`Height (m)` > 3)",
          "",
          "  } else if(col == 'Dosage (mg)'){",
          "",
          "    df %>% filter(`Dosage (mg)` > 1000)",
          "",
          "  } else {",
          "",
          "    data.frame()",
          "",
          "  }",
          "",
          "}",
          "",
          "accuracy_values <- sapply(cols, function(col){",
          "",
          "  errors <- nrow(get_accuracy(df, col))",
          "",
          "  round((1 - errors / total) * 100, 2)",
          "",
          "})",
          "",
          "accuracy <- round(",
          "  mean(accuracy_values),",
          "  2",
          ")",
          "",
          "# ==========================================",
          "# EXTRA METRICS",
          "# ==========================================",
          "duplicate_ids <- sum(",
          "  duplicated(df$`Patient ID`)",
          ")",
          "",
          "invalid_ages <- nrow(",
          "  get_accuracy(df, 'Age')",
          ")",
          "",
          "invalid_weights <- nrow(",
          "  get_accuracy(df, 'Weight (kg)')",
          ")",
          "",
          "invalid_heights <- nrow(",
          "  get_accuracy(df, 'Height (m)')",
          ")",
          "",
          "invalid_dosages <- nrow(",
          "  get_accuracy(df, 'Dosage (mg)')",
          ")",
          "",
          "# ==========================================",
          "# FINAL TABLE",
          "# ==========================================",
          "quality_table <- data.frame(",
          "",
          "  Metric = c(",
          "    'Completeness',",
          "    'Consistency',",
          "    'Accuracy',",
          "    'Duplicate IDs',",
          "    'Invalid Ages',",
          "    'Invalid Weights',",
          "    'Invalid Heights',",
          "    'Invalid Dosages'",
          "  ),",
          "",
          "  Value = c(",
          "    paste0(completeness, '%'),",
          "    paste0(consistency, '%'),",
          "    paste0(accuracy, '%'),",
          "    duplicate_ids,",
          "    invalid_ages,",
          "    invalid_weights,",
          "    invalid_heights,",
          "    invalid_dosages",
          "  )",
          "",
          ")",
          "",
          "knitr::kable(quality_table)",
          "```",

          "",

          "## Age distribution",

          "```{r, echo=FALSE, warning=FALSE}",
          "df_clean <- df %>%",
          "  filter(!is.na(Age), Age <= 120)",
          "",
          "ggplot(df_clean, aes(x=Age)) +",
          "  geom_histogram(bins=20, fill='#A8DADC', color='white') +",
          "  theme_minimal()",
          "```",

          "",

          "## Sex distribution",

          "```{r, echo=FALSE}",
          "df_plot <- df",
          "df_plot$Sex[is.na(df_plot$Sex)] <- 'NA'",
          "",
          "df_plot$Sex <- factor(",
          "  df_plot$Sex,",
          "  levels = c('Male','Female','NA')",
          ")",
          "",
          "ggplot(df_plot, aes(x=Sex, fill=Sex)) +",
          "  geom_bar() +",
          "  scale_x_discrete(drop = FALSE) +",
          "  scale_fill_manual(values=c(",
          "    'Male'='#BDE0FE',",
          "    'Female'='#FFC8DD',",
          "    'NA'='#D3D3D3'",
          "  )) +",
          "  theme_minimal() +",
          "  guides(fill='none')",
          "```",

          "",

          "## Weight vs Height",

          "```{r, echo=FALSE, warning=FALSE}",
          "df_clean <- df %>%",
          "  filter(",
          "    !is.na(`Height (m)`),",
          "    !is.na(`Weight (kg)`),",
          "    `Height (m)` <= 3,",
          "    `Weight (kg)` <= 300",
          "  )",
          "",
          "ggplot(df_clean, aes(x=`Height (m)`, y=`Weight (kg)`)) +",
          "  geom_point(alpha=0.6, color='#90DBF4') +",
          "  theme_minimal()",
          "```",

          "",

          "## Blood type distribution",

          "```{r, echo=FALSE}",
          "df_plot <- df",
          "df_plot$`Blood Type`[is.na(df_plot$`Blood Type`)] <- 'NA'",
          "",
          "df_plot$`Blood Type` <- factor(",
          "  df_plot$`Blood Type`,",
          "  levels = c(",
          "    'A+','A-','B+','B-','AB+','AB-','O+','O-','NA'",
          "  )",
          ")",
          "",
          "ggplot(df_plot, aes(x=`Blood Type`, fill=`Blood Type`)) +",
          "  geom_bar() +",
          "  scale_x_discrete(drop = FALSE) +",
          "  scale_fill_manual(values=c(",
          "    'A+'='#FFF3B0',",
          "    'A-'='#FFE5B4',",
          "    'B+'='#FFD6A5',",
          "    'B-'='#FFADAD',",
          "    'AB+'='#E2C2FF',",
          "    'AB-'='#CDB4DB',",
          "    'O+'='#BDB2FF',",
          "    'O-'='#A0C4FF',",
          "    'NA'='#D3D3D3'",
          "  )) +",
          "  theme_minimal() +",
          "  guides(fill='none')",
          "```",

          "",

          "## Diagnosis distribution",

          "```{r, echo=FALSE}",
          "df_plot <- df",
          "df_plot$`Diagnosis Code`[is.na(df_plot$`Diagnosis Code`)] <- 'NA'",
          "",
          "diag_levels <- sort(unique(df_plot$`Diagnosis Code`))",
          "diag_levels <- c(diag_levels[diag_levels != 'NA'], 'NA')",
          "",
          "df_plot$`Diagnosis Code` <- factor(",
          "  df_plot$`Diagnosis Code`,",
          "  levels = diag_levels",
          ")",
          "",
          "diag_colors <- rep('#90DBF4', length(diag_levels))",
          "diag_colors[length(diag_colors)] <- '#D3D3D3'",
          "names(diag_colors) <- diag_levels",
          "",
          "ggplot(df_plot, aes(x=`Diagnosis Code`, fill=`Diagnosis Code`)) +",
          "  geom_bar() +",
          "  scale_x_discrete(drop = FALSE) +",
          "  scale_fill_manual(values=diag_colors) +",
          "  theme_minimal() +",
          "  theme(axis.text.x = element_text(angle=45, hjust=1)) +",
          "  guides(fill='none')",
          "```",

          "",

          "## Dosage distribution",

          "```{r, echo=FALSE}",
          "df_clean <- df %>%",
          "  filter(!is.na(`Dosage (mg)`), `Dosage (mg)` <= 1000)",
          "",
          "ggplot(df_clean, aes(x=`Dosage (mg)`)) +",
          "  geom_histogram(bins=20, fill='#CDEAC0', color='white') +",
          "  theme_minimal()",
          "```",

          "",

          "## Smoker distribution",

          "```{r, echo=FALSE}",
          "df_plot <- df",
          "df_plot$Smoker[is.na(df_plot$Smoker)] <- 'NA'",
          "",
          "df_plot$Smoker <- factor(",
          "  df_plot$Smoker,",
          "  levels = c('Yes','No','NA')",
          ")",
          "",
          "ggplot(df_plot, aes(x=Smoker, fill=Smoker)) +",
          "  geom_bar() +",
          "  scale_x_discrete(drop = FALSE) +",
          "  scale_fill_manual(values=c(",
          "    'Yes'='#FFD6A5',",
          "    'No'='#BDE0FE',",
          "    'NA'='#D3D3D3'",
          "  )) +",
          "  theme_minimal() +",
          "  guides(fill='none')",
          "```",

          "",

          "## Doctor distribution",

          "```{r, echo=FALSE}",
          "df_plot <- df",
          "df_plot$Doctor[is.na(df_plot$Doctor)] <- 'NA'",
          "",
          "doctor_levels <- sort(unique(df_plot$Doctor))",
          "doctor_levels <- c(doctor_levels[doctor_levels != 'NA'], 'NA')",
          "",
          "df_plot$Doctor <- factor(",
          "  df_plot$Doctor,",
          "  levels = doctor_levels",
          ")",
          "",
          "doctor_colors <- rep('#6FA8DC', length(doctor_levels))",
          "doctor_colors[length(doctor_colors)] <- '#D3D3D3'",
          "names(doctor_colors) <- doctor_levels",
          "",
          "ggplot(df_plot, aes(x=Doctor, fill=Doctor)) +",
          "  geom_bar() +",
          "  scale_x_discrete(drop = FALSE) +",
          "  scale_fill_manual(values=doctor_colors) +",
          "  theme_minimal() +",
          "  guides(fill='none')",
          "```"

        ), temp_rmd)

        rmarkdown::render(
          input = temp_rmd,
          output_file = temp_html,
          params = list(data = df),
          envir = new.env(parent = globalenv())
        )

        file.copy(temp_html, file)

      }

    )

  })

}

# ==========================================
# 🔹 MODULE: QUALITY
# ==========================================
mod_quality_ui <- function(id){

  ns <- NS(id)

  tagList(

    h3("Global Quality Metrics"),
    tableOutput(ns("global_metrics")),

    hr(),

    h4("Completeness"),

    selectInput(ns("comp_col"), "Column:", choices = NULL),

    tableOutput(ns("comp_metrics")),
    tableOutput(ns("comp_table")),

    hr(),

    h4("Consistency"),

    selectInput(ns("cons_col"), "Column:", choices = NULL),

    tableOutput(ns("cons_metrics")),
    tableOutput(ns("cons_table")),

    hr(),

    h4("Accuracy"),

    selectInput(ns("acc_col"), "Column:", choices = NULL),

    tableOutput(ns("acc_metrics")),
    tableOutput(ns("acc_table"))

  )

}

mod_quality_server <- function(id, data){

  moduleServer(id, function(input, output, session){

    observe({

      cols <- c(
        "Patient ID",
        "Birth Date",
        "Age",
        "Sex",
        "Weight (kg)",
        "Height (m)",
        "Blood Type",
        "Diagnosis Code",
        "Dosage (mg)",
        "Smoker",
        "Doctor"
      )

      updateSelectInput(session, "comp_col", choices = cols)
      updateSelectInput(session, "cons_col", choices = cols)
      updateSelectInput(session, "acc_col", choices = cols)

    })

    # ==========================================
    # GLOBAL QUALITY METRICS
    # ==========================================
    output$global_metrics <- renderTable({

      df <- data()

      cols <- c(
        "Patient ID",
        "Birth Date",
        "Age",
        "Sex",
        "Weight (kg)",
        "Height (m)",
        "Blood Type",
        "Diagnosis Code",
        "Dosage (mg)",
        "Smoker",
        "Doctor"
      )

      total <- nrow(df)

      # ==========================================
      # COMPLETENESS GLOBAL
      # ==========================================
      completeness_values <- sapply(cols, function(col){

        errors <- sum(
          is.na(df[[col]]) |
            df[[col]] == "NA"
        )

        round((1 - errors / total) * 100, 2)

      })

      completeness <- round(
        mean(completeness_values),
        2
      )

      # ==========================================
      # CONSISTENCY GLOBAL
      # ==========================================
      consistency_values <- sapply(cols, function(col){

        errors <- nrow(get_consistency(df, col))

        round((1 - errors / total) * 100, 2)

      })

      consistency <- round(
        mean(consistency_values),
        2
      )

      # ==========================================
      # ACCURACY GLOBAL
      # ==========================================
      accuracy_values <- sapply(cols, function(col){

        errors <- nrow(get_accuracy(df, col))

        round((1 - errors / total) * 100, 2)

      })

      accuracy <- round(
        mean(accuracy_values),
        2
      )

      # ==========================================
      # EXTRA METRICS
      # ==========================================
      duplicate_ids <- sum(
        duplicated(df$`Patient ID`)
      )

      invalid_ages <- nrow(
        get_accuracy(df, "Age")
      )

      invalid_weights <- nrow(
        get_accuracy(df, "Weight (kg)")
      )

      invalid_heights <- nrow(
        get_accuracy(df, "Height (m)")
      )

      invalid_dosages <- nrow(
        get_accuracy(df, "Dosage (mg)")
      )

      # ==========================================
      # FINAL TABLE
      # ==========================================
      data.frame(

        Metric = c(
          "Completeness",
          "Consistency",
          "Accuracy",
          "Duplicate IDs",
          "Invalid Ages",
          "Invalid Weights",
          "Invalid Heights",
          "Invalid Dosages"
        ),

        Value = c(
          paste0(completeness, "%"),
          paste0(consistency, "%"),
          paste0(accuracy, "%"),
          duplicate_ids,
          invalid_ages,
          invalid_weights,
          invalid_heights,
          invalid_dosages
        )

      )

    })

    # ==========================================
    # COMPLETENESS
    # ==========================================
    output$comp_metrics <- renderTable({

      df <- data()

      total <- nrow(df)

      errors <- sum(
        is.na(df[[input$comp_col]]) |
          df[[input$comp_col]] == "NA"
      )

      data.frame(
        Total = total,
        Nulls = errors,
        Percentage = paste0(round(errors/total*100,2), "%")
      )

    })

    output$comp_table <- renderTable({

      df <- data()

      res <- df %>%
        filter(
          is.na(.data[[input$comp_col]]) |
            .data[[input$comp_col]] == "NA"
        )

      if(nrow(res) == 0) return(NULL)

      res

    })

    # ==========================================
    # CONSISTENCY
    # ==========================================
    get_consistency <- function(df, col){

      if(col == "Age"){

        df %>%
          filter(
            !is.na(Age),
            !is.na(`Birth Date`)
          ) %>%
          mutate(
            calculated_age =
              floor(
                time_length(
                  interval(as.Date(`Birth Date`), Sys.Date()),
                  "years"
                )
              )
          ) %>%
          filter(Age != calculated_age) %>%
          select(-calculated_age)

      } else {

        data.frame()

      }

    }

    output$cons_metrics <- renderTable({

      df <- data()

      errors <- nrow(get_consistency(df, input$cons_col))

      data.frame(
        Total = nrow(df),
        Errors = errors,
        Percentage = paste0(round(errors/nrow(df)*100,2), "%")
      )

    })

    output$cons_table <- renderTable({
      get_consistency(data(), input$cons_col)
    })

    # ==========================================
    # ACCURACY
    # ==========================================
    get_accuracy <- function(df, col){

      if(col == "Age"){

        df %>% filter(Age > 120)

      } else if(col == "Weight (kg)"){

        df %>% filter(`Weight (kg)` > 300)

      } else if(col == "Height (m)"){

        df %>% filter(`Height (m)` > 3)

      } else if(col == "Dosage (mg)"){

        df %>% filter(`Dosage (mg)` > 1000)

      } else {

        data.frame()

      }

    }

    output$acc_metrics <- renderTable({

      df <- data()

      errors <- nrow(get_accuracy(df, input$acc_col))

      data.frame(
        Total = nrow(df),
        Outliers = errors,
        Percentage = paste0(round(errors/nrow(df)*100,2), "%")
      )

    })

    output$acc_table <- renderTable({
      get_accuracy(data(), input$acc_col)
    })

  })

}

# ==========================================
# 🔹 MODULE: MANAGE PATIENT
# ==========================================
mod_manage_ui <- function(id){

  ns <- NS(id)

  tagList(

    useShinyjs(),

    div(

      style = "
      max-width:600px;
      margin:auto;
      padding:20px;
      border:1px solid #ddd;
      border-radius:10px;
      background-color:#fafafa;
      ",

      h3("Manage Patient"),

      selectInput(
        ns("action"),
        NULL,
        c("CREATE","MODIFY","DELETE")
      ),

      textInput(ns("id"), "Patient ID *"),

      dateInput(
        ns("birth"),
        "Birth Date",
        value = NULL,
        max = Sys.Date()
      ),

      numericInput(ns("age"), "Age", value = NA),

      selectInput(
        ns("sex"),
        "Sex",
        c("NA","Male","Female"),
        selected = "NA"
      ),

      numericInput(
        ns("weight"),
        "Weight (kg)",
        value = NA
      ),

      numericInput(
        ns("height"),
        "Height (m)",
        value = NA
      ),

      selectInput(
        ns("blood"),
        "Blood Type",
        c("NA","A+","A-","B+","B-","O+","O-","AB+","AB-"),
        selected = "NA"
      ),

      selectInput(
        ns("diagnosis"),
        "Diagnosis Code",
        choices = c("NA"),
        selected = "NA"
      ),

      numericInput(
        ns("dosage"),
        "Dosage (mg)",
        value = NA
      ),

      selectInput(
        ns("smoker"),
        "Smoker",
        c("NA","Yes","No"),
        selected = "NA"
      ),

      selectInput(
        ns("doctor"),
        "Doctor",
        c("NA","Dr. House","Dr. Strange","Dr. Who"),
        selected = "NA"
      ),

      br(),

      conditionalPanel(
        condition = sprintf(
          "input['%s'] == 'CREATE'",
          ns("action")
        ),
        actionButton(ns("create"), "Register")
      ),

      conditionalPanel(
        condition = sprintf(
          "input['%s'] == 'MODIFY'",
          ns("action")
        ),
        actionButton(ns("update"), "Update")
      ),

      conditionalPanel(
        condition = sprintf(
          "input['%s'] == 'DELETE'",
          ns("action")
        ),
        actionButton(ns("delete"), "Erase")
      ),

      verbatimTextOutput(ns("msg"))

    )

  )

}

mod_manage_server <- function(id, data){

  moduleServer(id, function(input, output, session){

    user_id <- session$request$REMOTE_ADDR

    # ==========================================
    # ACTIONS
    # ==========================================
    observeEvent(input$action, {

      all_fields <- c(
        "birth",
        "age",
        "sex",
        "weight",
        "height",
        "dosage",
        "blood",
        "diagnosis",
        "doctor",
        "smoker"
      )

      if(input$action == "CREATE"){

        updateTextInput(session, "id", value = "")

        updateDateInput(session, "birth", value = NULL)

        updateNumericInput(session, "age", value = NA)
        updateNumericInput(session, "weight", value = NA)
        updateNumericInput(session, "height", value = NA)
        updateNumericInput(session, "dosage", value = NA)

        updateSelectInput(session, "sex", selected = "NA")
        updateSelectInput(session, "blood", selected = "NA")
        updateSelectInput(session, "diagnosis", selected = "NA")
        updateSelectInput(session, "smoker", selected = "NA")
        updateSelectInput(session, "doctor", selected = "NA")

        lapply(all_fields, shinyjs::enable)

        shinyjs::disable("age")

      } else if(input$action == "MODIFY"){

        lapply(all_fields, shinyjs::enable)

      } else if(input$action == "DELETE"){

        lapply(all_fields, shinyjs::disable)

      }

    })

    # ==========================================
    # DIAGNOSIS
    # ==========================================
    observe({

      df <- data()

      if("Diagnosis Code" %in% names(df)){

        vals <- sort(unique(df$`Diagnosis Code`))

        vals <- vals[!is.na(vals)]

        updateSelectInput(
          session,
          "diagnosis",
          choices = c("NA", vals),
          selected = "NA"
        )

      }

    })

    # ==========================================
    # AUTOFILL
    # ==========================================
    observeEvent(input$id, {

      req(input$action != "CREATE")

      if(nchar(input$id) != 6) return()

      df <- data()

      if(!(input$id %in% df$`Patient ID`)) return()

      row <- df %>%
        filter(`Patient ID` == input$id) %>%
        slice(1)

      safe_text <- function(x){
        if(is.na(x) || x == "") "NA" else as.character(x)
      }

      updateDateInput(
        session,
        "birth",
        value = as.Date(row$`Birth Date`)
      )

      updateNumericInput(
        session,
        "age",
        value = row$Age
      )

      updateSelectInput(
        session,
        "sex",
        selected = safe_text(row$Sex)
      )

      updateNumericInput(
        session,
        "weight",
        value = row$`Weight (kg)`
      )

      updateNumericInput(
        session,
        "height",
        value = row$`Height (m)`
      )

      updateSelectInput(
        session,
        "blood",
        selected = safe_text(row$`Blood Type`)
      )

      updateSelectInput(
        session,
        "diagnosis",
        selected = safe_text(row$`Diagnosis Code`)
      )

      updateNumericInput(
        session,
        "dosage",
        value = row$`Dosage (mg)`
      )

      updateSelectInput(
        session,
        "smoker",
        selected = safe_text(row$Smoker)
      )

      updateSelectInput(
        session,
        "doctor",
        selected = safe_text(row$Doctor)
      )

    })

    # ==========================================
    # AGE AUTO CALC
    # ==========================================
    observeEvent(input$birth, {

      if(input$action == "CREATE"){

        age_calc <- floor(
          time_length(
            interval(input$birth, Sys.Date()),
            "years"
          )
        )

        updateNumericInput(
          session,
          "age",
          value = age_calc
        )

      }

    })

    # ==========================================
    # BUILD DATA
    # ==========================================
    build_data <- function(){

      to_na <- function(x){

        if(is.null(x) || x == "NA" || x == ""){
          return(NA)
        }

        return(x)

      }

      data.frame(

        "Patient ID" = input$id,

        "Birth Date" =
          ifelse(is.null(input$birth), NA, as.character(input$birth)),

        "Age" =
          ifelse(is.na(input$age), NA, input$age),

        "Sex" =
          to_na(input$sex),

        "Weight (kg)" =
          ifelse(is.na(input$weight), NA, input$weight),

        "Height (m)" =
          ifelse(is.na(input$height), NA, input$height),

        "Blood Type" =
          to_na(input$blood),

        "Diagnosis Code" =
          to_na(input$diagnosis),

        "Dosage (mg)" =
          ifelse(is.na(input$dosage), NA, input$dosage),

        "Smoker" =
          to_na(input$smoker),

        "Doctor" =
          to_na(input$doctor),

        check.names = FALSE

      )

    }

    # ==========================================
    # CREATE
    # ==========================================
    observeEvent(input$create, {

      if(input$action != "CREATE") return()

      df <- data()

      if(input$id == ""){

        output$msg <- renderText("❌ ID empty")
        return()

      }

      if(!str_detect(input$id, "^P-\\d{4}$")){

        output$msg <- renderText("❌ ID format P-XXXX")
        return()

      }

      if(input$id %in% df$`Patient ID`){

        output$msg <- renderText("❌ ID already exists")
        return()

      }

      if(!is.na(input$age) && input$age > 120){

        output$msg <- renderText("❌ Age must be <= 120 years")
        return()

      }

      if(!is.na(input$weight) && input$weight > 300){

        output$msg <- renderText("❌ Weight must be <= 300 kg")
        return()

      }

      if(!is.na(input$height) && input$height > 3){

        output$msg <- renderText("❌ Height must be <= 3 m")
        return()

      }

      if(!is.na(input$dosage) && input$dosage > 1000){

        output$msg <- renderText("❌ Dosage must be <= 1000 mg")
        return()

      }

      con$insert(build_data())

      log_con$insert(data.frame(
        action = "CREATE",
        patient_id = input$id,
        changes = "new record",
        user = user_id,
        timestamp = Sys.time()
      ))

      data(con$find())

      output$msg <- renderText("✅ Created")

    })

    # ==========================================
    # MODIFY
    # ==========================================
    observeEvent(input$update, {

      if(input$action != "MODIFY") return()

      df <- data()

      if(!(input$id %in% df$`Patient ID`)){

        output$msg <- renderText("❌ ID does not exist")
        return()

      }

      if(!is.na(input$age) && input$age > 120){

        output$msg <- renderText("❌ Age must be <= 120 years")
        return()

      }

      if(!is.na(input$weight) && input$weight > 300){

        output$msg <- renderText("❌ Weight must be <= 300 kg")
        return()

      }

      if(!is.na(input$height) && input$height > 3){

        output$msg <- renderText("❌ Height must be <= 3 m")
        return()

      }

      if(!is.na(input$dosage) && input$dosage > 1000){

        output$msg <- renderText("❌ Dosage must be <= 1000 mg")
        return()

      }

      old <- df %>%
        filter(`Patient ID` == input$id)

      clean_val <- function(x){

        if(is.null(x) || x == "NA" || x == ""){
          return(NA)
        }

        return(x)

      }

      new_vals <- list(

        "Birth Date" =
          ifelse(is.null(input$birth), NA, as.character(input$birth)),

        "Age" =
          ifelse(is.na(input$age), NA, input$age),

        "Sex" =
          clean_val(input$sex),

        "Weight (kg)" =
          ifelse(is.na(input$weight), NA, input$weight),

        "Height (m)" =
          ifelse(is.na(input$height), NA, input$height),

        "Blood Type" =
          clean_val(input$blood),

        "Diagnosis Code" =
          clean_val(input$diagnosis),

        "Dosage (mg)" =
          ifelse(is.na(input$dosage), NA, input$dosage),

        "Smoker" =
          clean_val(input$smoker),

        "Doctor" =
          clean_val(input$doctor)

      )

      changes <- list()
      changes_text <- c()

      for(col in names(new_vals)){

        old_val <- old[[col]][1]
        new_val <- new_vals[[col]]

        if(is.na(old_val) && is.na(new_val)) next

        if(isTRUE(all.equal(old_val, new_val))) next

        changes[[col]] <- new_val

        changes_text <- c(
          changes_text,
          paste0(col, ": ", old_val, " → ", new_val)
        )

      }

      if(length(changes) == 0){

        output$msg <- renderText("⚠️ No changes detected")
        return()

      }

      con$update(

        query = paste0(
          '{"Patient ID":"',
          input$id,
          '"}'
        ),

        update = paste0(
          '{"$set":',
          toJSON(changes, auto_unbox = TRUE),
          '}'
        ),

        multiple = FALSE

      )

      log_con$insert(data.frame(
        action = "MODIFY",
        patient_id = input$id,
        changes = paste(changes_text, collapse = " | "),
        user = user_id,
        timestamp = Sys.time()
      ))

      data(con$find())

      output$msg <- renderText("✏️ Updated")

    })

    # ==========================================
    # DELETE
    # ==========================================
    observeEvent(input$delete, {

      if(input$action != "DELETE") return()

      df <- data()

      if(!(input$id %in% df$`Patient ID`)){

        output$msg <- renderText("❌ ID does not exist")
        return()

      }

      con$remove(
        paste0(
          '{"Patient ID":"',
          input$id,
          '"}'
        )
      )

      log_con$insert(data.frame(
        action = "DELETE",
        patient_id = input$id,
        changes = "record deleted",
        user = user_id,
        timestamp = Sys.time()
      ))

      data(con$find())

      output$msg <- renderText("🗑️ Deleted")

    })

  })

}

# ==========================================
# 🔹 MODULE: AUDIT LOG
# ==========================================
mod_log_ui <- function(id){

  ns <- NS(id)

  tagList(

    h3("Audit Log"),

    DTOutput(ns("log_table"))

  )

}

mod_log_server <- function(id){

  moduleServer(id, function(input, output, session){

    output$log_table <- renderDT({

      logs <- log_con$find()

      if(nrow(logs) == 0) return(NULL)

      logs <- logs %>%
        select(
          action,
          patient_id,
          changes,
          user,
          timestamp
        ) %>%
        arrange(desc(timestamp))

      datatable(
        logs,
        options = list(pageLength = 10)
      )

    })

  })

}

# ==========================================
# UI
# ==========================================
ui <- fluidPage(

  titlePanel("Hospital Data System"),

  tabsetPanel(

    tabPanel(
      "Data",
      mod_data_ui("data")
    ),

    tabPanel(
      "Quality",
      mod_quality_ui("quality")
    ),

    tabPanel(
      "Manage Patient",
      mod_manage_ui("manage")
    ),

    tabPanel(
      "Audit Log",
      mod_log_ui("log")
    )

  )

)

# ==========================================
# SERVER
# ==========================================
server <- function(input, output, session){

  data <- reactiveVal(con$find())

  mod_data_server("data", data)
  mod_quality_server("quality", data)
  mod_manage_server("manage", data)
  mod_log_server("log")

}

# ==========================================
# APP
# ==========================================
shinyApp(ui, server)