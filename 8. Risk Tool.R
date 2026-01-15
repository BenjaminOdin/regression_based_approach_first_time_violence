library(shiny)
# --- Load models (ensure these files exist in app directory) ---
models <- list(
  "10-16 Year Olds: Aggregate Offences" = readRDS("Risk_Calculator_7.rds"),
  "10-13 Year Olds: Aggregate Offences" = readRDS("Risk_Calculator_8.rds"),
  "14-16 Year Olds: Aggregate Offences" = readRDS("Risk_Calculator_12.rds")
)

# --- Predictor lists for each model ---
predictors_list <- list(
  "10-16 Year Olds: Aggregate Offences" = c("Gender", "Missing_Frequency", "Sexual_Assault_Frequency",
                                            "Physical_Assault_Frequency", "NV_Offence_Frequency_victim",
                                            "Neglect_Frequency", "NV_Offence_Frequency",
                                            "Witness_Offence_Frequency", "NV_Offence_Frequency_Sexual"),
  "10-13 Year Olds: Aggregate Offences" = c("Gender", "Missing_history_freq_Pre_14", "Sexual_history_freq_Pre_14",
                                            "Physical_Assault_Frequency_Pre_14", "nv_victim_history_freq_Pre_14",
                                            "Neglect_history_freq_Pre_14", "NV_history_freq_Pre_14",
                                            "witness_freq_Pre_14", "sexual_history_freq_Pre_14"),
  "14-16 Year Olds: Aggregate Offences" = c("Gender", "Missing_Frequency", "Sexual_Assault_Frequency",
                                            "Physical_Assault_Frequency", "NV_Offence_Frequency_victim",
                                            "Neglect_Frequency", "NV_Offence_Frequency",
                                            "Witness_Offence_Frequency", "NV_Offence_Frequency_Sexual")
)

# --- Human-readable labels ---
predictor_labels <- list(
  "Gender" = tags$span("Gender", title = "Biological Sex"),
  "Missing_Frequency" = tags$span("Missing Events", title ="where the subject has been reported as missing from home"),
  "Missing_history_freq_Pre_14" = tags$span("Missing Events", title = "where the subject has been reported as missing from home"),
  "Sexual_Assault_Frequency" = tags$span("Sexual Victimisation", title = "where the subject has been a victim of a sexual offence e.g. upskirting or rape"),
  "Sexual_history_freq_Pre_14" = tags$span("Sexual Victimisation", title = "where the subject has been a victim of a sexual offence e.g. upskirting or rape"),
  "Physical_Assault_Frequency" = tags$span("Physical Victimisation", title = "where the subject has been a victim of a physical offence e.g. ABH or Robbery"),
  "Physical_Assault_Frequency_Pre_14" = tags$span("Physical Victimisation", title = "where the subject has been a victim of a physical offence e.g. ABH or Robbery"),
  "NV_Offence_Frequency_victim" = tags$span("Non-Violent Victimisation", title = "where the subject has been a victim of a non-violent offence e.g. theft or fraud"),
  "nv_victim_history_freq_Pre_14" = tags$span("Non-Violent Victimisation", title = "where the subject has been a victim of a non-violent offence e.g. theft or fraud"),
  "Neglect_Frequency" = tags$span("Neglect History", title = "where the subject has been a victim of neglect either emotional or physical"),
  "Neglect_history_freq_Pre_14" = tags$span("Neglect History", title = "where the subject has been a victim of neglect either emotional or physical"),
  "NV_Offence_Frequency" = tags$span("Non-Violent Offence History", title = "where the subject has committed a non-violent offence e.g. theft or fraud"),
  "NV_history_freq_Pre_14" = tags$span("Non-Violent Offence History", title = "where the subject has committed a non-violent offence e.g. theft or fraud"),
  "Witness_Offence_Frequency" = tags$span("Witness History (Violent)", title = "where the subject has witnessed a violent incident e.g. ABH or GBH"),
  "witness_freq_Pre_14" = tags$span("Witness History (Violent)", title = "where the subject has witnessed a violent incident e.g. ABH or GBH"),
  "NV_Offence_Frequency_Sexual" = tags$span("Sexual Offence History", title = "where the subject has committed a sexual offence e.g. upskirting or rape"),
  "sexual_history_freq_Pre_14" = tags$span("Sexual Offence History", title = "where the subject has committed a sexual offence e.g. upskirting or rape")
)

nice_name <- function(x) {
  if (x %in% names(predictor_labels)) predictor_labels[[x]] else x
}

# --- Shrinkage metrics ---
shrinkage_metrics <- data.frame(
  Scenario = c("10-16 Year Olds: Aggregate Offences","10-13 Year Olds: Aggregate Offences", "14-16 Year Olds: Aggregate Offences"),
  
  Calibration_Slope = c(0.727,0.694,0.654),
  Calibration_in_the_Large = c(-0.524, -0.587, -0.665),
  stringsAsFactors = FALSE
)

decision_criteria <- data.frame(
  Criteria = c(
    "Is the data subject aged appropriately?",
    "Is the data subject from an appropriate nation?",
    "Has the data subject confirmed their exposure(s)?",
    "Is there a need to use this tool?",
    "Has the user read the overview?",
    "Does the data subject have at least one listed exposure?",
    "Does the user require a general or age-specific understanding of risk?",
    "Is the user concerned that the data subject is at increased risk?",
    "Has the user considered their own risk threshold?",
    "Does the user appreciate outputs are not diagnostic?",
    "Does the user require an indication or exact estimate?",
    "If the calculator is used, is the justification valid?",
    "Does the justification risk the subject’s welfare?",
    "Does the data subject have a right to know the outputs?",
    "Will outputs be shared on a need-to-know basis?",
    "Has the user considered generalising the results?"
  ),
  Description = c(
    "Applicable only to those aged 10–16.",
    "Subject must have lived in England (preferably Thames Valley).",
    "Exposure must be confirmed or clearly evident.",
    "If intervention can occur without this tool, do not use it.",
    "The overview explains tool scope and valid inputs.",
    "The tool is not useful without at least one exposure.",
    "Choose general (10–16) or age-specific (10–13 / 14–16).",
    "If there is no concern, the tool should not be used.",
    "If none is set, use the tool’s suggested thresholds.",
    "Outputs indicate risk only and are not diagnostic.",
    "Indicator tab preferred unless numeric estimate is justified.",
    "Calculator use must be justified beyond preference.",
    "If welfare may be harmed, do not proceed.",
    "Subjects have the right to access their own information.",
    "Outputs should remain confidential unless necessary.",
    "Generalise results where population mismatch exists."
  ),
  Action = c(
    "YES = Continue / NO = Stop",
    "YES = Continue / NO = Stop",
    "YES = Continue / NO = Stop",
    "YES = Continue / NO = Stop",
    "YES = Continue / NO = Stop",
    "YES = Continue / NO = Stop",
    "General = Observation period / Age-specific = Development stage",
    "YES = Continue / NO = Stop",
    "NA",
    "YES = Continue / NO = Stop",
    "Indicator = Indicator tab / Exact = Calculator tab",
    "YES = Continue / NO = Stop / NA = Continue",
    "YES = Continue / NO = Stop / NA = Continue",
    "YES = Continue / NO = Stop",
    "YES = Continue / NO = Stop",
    "YES = Continue / NO = Stop"
  ),
  stringsAsFactors = FALSE
)


# --- UI ---
ui <- fluidPage(
  titlePanel("Risk Identification Tool: First-Time Violent Offending in 10–16 Year Olds"),
  tabsetPanel(
    tabPanel("Overview",
             fluidRow(
               column(12,
                      h2("About this Risk Tool"),
                      p("This tool provides estimates of the risk of first-time violence based on different characteristics and incidents identified in routinely collected police data"),
                      p("The population used to base the calculations on consists of a total of 30,853 data subjects (15,854 males and 14,999 females) from the Thames Valley Region"),
                      p("Inclusion in the population required that the data subject was aged 16 by 25th March 2021"),
                      p("Probabilities are based on a single population cohort and must be interpreted with caution"),
                      p("If the user is working with an individual outside of the population identified above, it is recommended that they use the 'Generalise' option when calculating results"),
                      p("Unless for a specific quantitative purpose, the user is recommended to use the 'Risk Indicator' tab to identify changes in risk for your data subject"),
                      p("Prior to engaging with the tool, the user is recommended to use the 'Pre-Use Decision Checklist' (next tab) to ensure that the tool is appropriate for use for their intended purpose"),
                      
                      p("Characteristics and Incidents:"),
                      tags$ul(
                        tags$li("The following lists the characteristics and incidents that are included for selection in this tool"),
                        tags$li("Characteristic: Gender (Biological Sex)"),
                        tags$li("Incident: Missing Events"),
                        tags$li("Incident: Sexual Victimisation (All types)"),
                        tags$li("Incident: Physical Victimisation (All types)"),
                        tags$li("Incident: Non-Violent Victimisation (All Types)"),
                        tags$li("Incident: Neglect History (Emotional and Physical)"),
                        tags$li("Incident: Non-Violent Offence History (All Types)"),
                        tags$li("Incident: Witnessing Violence (All Types)"),
                        tags$li("Incident: Sexual Offence History (All Types)")
                      ),
                      p("The tool provides the option to select between three specific groups:"),
                      tags$ul(
                        tags$li("Early and Middle Adolescence: Those aged between 10 and 16 years of age"),
                        tags$li("Early Adolescence: Those aged between 10 and 13 years of age"),
                        tags$li("Middle Adolescence: Those aged between 14 and 16 years of age")
                      ),
                      p("The tool considers violent offences as a combined group:"),
                      tags$ul(
                        tags$li("Aggregate Offences: Assault and Battery, ABH, GBH, Robbery, Manslaughter and Murder")
                      ),
                      p("The outputs are based on a specific outcome definition used in the modelling:"),
                      tags$ul(
                        tags$li("An offence is deemed to have occurred where the outcome indicator in the police dataset did not explicitly indicate that an offence had not taken place or that there was evidence that the suspect was innocent"),
                        tags$li("The decision to use this definition is to maximise sensitivity so that as many true positives are identified as possible without introducing averse conditions to the assumption i.e. where there is clear evidence of no crime"),
                        tags$li("A stricter definition requiring that there was sufficient evidence for the suspect to be summonsed or charged was tested via a main effect logistic regression"),
                        tags$li("The outcome of the testing was that several exposures had changes in their directional effects on the outcome")
                      ),
                      p("For more information, contact Benjamin Odin at bgeodin1@sheffield.ac.uk")
               )
             )
             
    ),
    
    tabPanel(
      "Pre-Use Decision Checklist",
      fluidRow(
        column(
          12,
          h2("Decision Criteria Before and During Tool Use"),
          p("This checklist must be followed to ensure appropriate, ethical, and valid use of the risk identification tool."),
          tableOutput("decision_criteria_table"),
          br(),
          p(strong("Important:"), 
            "If any step results in a 'Stop' decision, the tool should not be used further.")
        )
      )
    ),
    
    tabPanel("Instructions and Evaluation",
             fluidRow(
               column(12,
                      h2("How to use this tool"),
                      tags$ul(
                        tags$li("The 'Risk Indicator' Tab is recommended for all users"),
                        tags$li("If using the 'Risk Calculation' tab, ensure that the 'Overview' tab has been read and understood thoroughly alongside the 'Evaluative Measures' tab"),
                        tags$li("The functionality between the Indicator and Calculator tab is the same"),
                        tags$li("To adjust the estimated probability so that it is generalisable, use the 'Generalise' checkbox"),
                        tags$li("The 'Generalise' checkbox applies a logistic recalibration adjustment to the estimates")
                      ),
                      tags$ul(
                        tags$li("Decide whether a general overview of the change in Risk Profile (Indicator) or actual numeric estimates (Calculator) is required"),
                        tags$li("Decide whether an average-based estimate (10-16 years) or an age-specific estimate (10-13 years or 14-16 years) is required"),
                        tags$li("Select the Gender of the data subject of interest"),
                        tags$li("Select whether the data subject's risk will be compared to the male or female baseline (baseline = risk with 0 exposures)"),
                        tags$li("Fill in the incident values on the left, inputting the number of times each incident has occurred"),
                        tags$li("If using a mobile phone or a tablet, these values must be entered manually"),
                        tags$li("If an incident has not taken place, leave as 0 (blank if using a mobile/tablet)"),
                        tags$li("Once all information regarding incidents has been entered, select 'Check Indicator' / 'Calculate Risk' to view the outputs"),
                        tags$li("Select the 'Generalise' checkbox to adjust the risk estimate for wider application"),
                        tags$li("If using a computer, for more information on each exposure, hover over the name of the exposure"),
                        tags$li("Calculator only: To download results as a CSV, use the Download Results button")
                      ),
                      p("Consider the next section 'Model Evaluation' carefully before using the tool")
               )
               
             ),
             
             tabPanel("Evaluative Measures",
                      fluidRow(
                        column(12,
                               h2("Model Evaluation"),
                               tags$ul(
                                 tags$li("Evaluation of the models underpinning this tool has been performed"),
                                 tags$li("The following outputs provide the user with insights into the discriminatory ability, accuracy, sensitivity, and specificity"),
                                 tags$li("Subjective thresholds determining when a data subject is truly at risk are discouraged. Using the Baseline Risks provided to set the threshold pertinent to the data subject is recommended"),
                                 h3("Baseline Thresholds"),
                                 tableOutput("Baseline_Risk_Thresholds"),
                                 h3("Discrimination and Accuracy"),
                                 tableOutput("Discrimination_Accuracy_Calibration"),
                                 h3("Sensitivity"),
                                 tableOutput("Sensitivity"),
                                 h3("Specificity"),
                                 tableOutput("Specificity"),
                                 tags$ul(
                                   tags$li("Baseline Risks have been calculated for pre-recalibration and post-recalibration and are shown as a range"),
                                   tags$li("Brier Scores have been calculated for pre-recalibration and post-recalibration and are shown as a range (AUC has not changed pre and post recalibration)"),
                                   tags$li("Sensitivity and Specificity have been calculated for pre-recalibration and post-recalibration and are shown as a range"),
                                   tags$li("All values are presented in order of pre-recalibration - post-recalibration")
                                 ),
                                 p("For more information, contact Benjamin Odin at bgeodin1@sheffield.ac.uk")
                               )
                        )
                      )
             )
    ),
    
    tabPanel("Risk Indicator",
             sidebarLayout(
               sidebarPanel(
                 selectInput("indicator_model_select", "Age-Range and Offence Grouping:", choices = names(models)),
                 radioButtons("indicator_baseline_type", "Baseline Comparator:",
                              choices = c("Male Baseline", "Female Baseline"), selected = "Male Baseline"),
                 uiOutput("indicator_dynamic_inputs"),
                 actionButton("indicator_calc_btn", "Check Indicator"),
                 br(), br(),
                 checkboxInput("indicator_apply_shrinkage", "Generalise", value = FALSE)
               ),
               mainPanel(h3("Indicator Result"), htmlOutput("indicator_message"))
             )
    ),
    
    tabPanel("Risk Calculation",
             sidebarLayout(
               sidebarPanel(
                 selectInput("model_select", "Age-Range and Offence Grouping:", choices = names(models)),
                 radioButtons("baseline_type", "Baseline Comparator:", choices = c("Male Baseline", "Female Baseline"), selected = "Male Baseline"),
                 uiOutput("dynamic_inputs"),
                 actionButton("calc_btn", "Calculate Risk"),
                 br(), br(),
                 checkboxInput("apply_shrinkage", "Generalise", value = FALSE),
                 br(), br(),
                 downloadButton("download_btn", "Download Results")
               ),
               mainPanel(
                 h3("Results"),
                 fluidRow(
                   column(4, strong("Predicted Probability:"), textOutput("prob_numeric")),
                   column(4, strong("Change from Baseline:"), textOutput("change_numeric")),
                   column(4, strong("Relative Risk:"), htmlOutput("rr_numeric"))
                 ),
                 br(),
                 htmlOutput("pred_prob"),
                 htmlOutput("change_from_baseline"),
                 htmlOutput("rr_sentence"),
                 br(),
                 h4("Explanation of Measures"),
                 p("'Predicted Probability' shows the estimated chance (%) that the subject will go on to commit a first violet offence, given the selected characteristics"),
                 p("'Change from Baseline' shows the absolute difference in risk compared to the selected baseline (male or female with no exposures)"),
                 p("'Relative Risk' shows how many times higher (or lower) the subject’s risk is compared to the chosen baseline risk")
               )
             )
    )
  )
)



# --- SERVER ---
server <- function(input, output, session) {
  
  output$decision_criteria_table <- renderTable({
    decision_criteria
  }, sanitize.text.function = function(x) x)
  
  # --- Metric Tables ---
  Baseline_Risk_Thresholds <- data.frame(
    Scenario = c("Aggregate Offences (ages 10-16)","Aggregate Offences (ages 10-13)", "Aggregate Offences (ages 14-16)"),
    Male_Baseline_TVP = c(10.70,6.07,4.48),
    Female_Baseline_TVP = c(7.74,4.54, 3.13),
    Male_Baseline_SHR = c(11.24, 7.67, 6.50),
    Female_Baseline_SHR = c(8.90, 6.29,5.16)
  )
  output$Baseline_Risk_Thresholds <- renderTable({
    
    df <- Baseline_Risk_Thresholds
    
    df$Male_Baseline <- paste0(
      formatC(df$Male_Baseline_TVP, format = "f", digits = 2),
      "–",
      formatC(df$Male_Baseline_SHR, format = "f", digits = 2)
    )
    
    df$Female_Baseline <- paste0(
      formatC(df$Female_Baseline_TVP, format = "f", digits = 2),
      "–",
      formatC(df$Female_Baseline_SHR, format = "f", digits = 2)
    )
    
    df <- df[, c("Scenario", "Male_Baseline", "Female_Baseline")]
    
    colnames(df) <- c(
      "Scenario",
      "Male Baseline Risk (%)",
      "Female Baseline Risk (%)"
    )
    
    df
  }, sanitize.text.function = function(x) x)
  
  
  Discrimination_Accuracy_Calibration <- data.frame(
    Scenario = c(
      "Aggregate Offences (ages 10-16)",
      "Aggregate Offences (ages 10-13)",
      "Aggregate Offences (ages 14-16)"
    ),
    AUC = c(0.767, 0.777, 0.778),
    Brier_Pre  = c(0.101, 0.061, 0.049),
    Brier_Post = c(0.103, 0.066, 0.051),
    stringsAsFactors = FALSE
  )
  
  output$Discrimination_Accuracy_Calibration <- renderTable({
    df <- Discrimination_Accuracy_Calibration
    
    df$AUC <- formatC(df$AUC, format = "f", digits = 3)
    
    df$Brier_Score <- paste0(
      formatC(df$Brier_Pre, format = "f", digits = 3),
      "–",
      formatC(df$Brier_Post, format = "f", digits = 3)
    )
    
    df <- df[, c("Scenario", "AUC", "Brier_Score")]
    colnames(df) <- c("Scenario", "AUC", "Brier Score (Pre–Post)")
    
    df
  }, sanitize.text.function = function(x) x)
  
  
  # --- Sensitivity Table (Pre/Post as Range) ---
  Sensitivity <- data.frame(
    Scenario = c("Aggregate Offences (ages 10-16)","Aggregate Offences (ages 10-13)", "Aggregate Offences (ages 14-16)"),
    Threshold_0.05 = c(0.999,0.775, 0.512),
    Threshold_0.10 = c(0.773,0.288, 0.248),
    Threshold_0.15 = c(0.382,0.148, 0.148),
    Threshold_0.20 = c(0.233,0.092, 0.108),
    Threshold_0.25 = c(0.179,0.067, 0.049),
    Threshold_0.30 = c(0.134,0.049, 0.069),
    Threshold2_0.05 = c(0.992, 0.694, 0.492),
    Threshold2_0.10 = c(0.760, 0.247, 0.226),
    Threshold2_0.15 = c(0.370, 0.126, 0.124),
    Threshold2_0.20 = c(0.222, 0.075, 0.085),
    Threshold2_0.25 = c(0.169, 0.053, 0.057),
    Threshold2_0.30 = c(0.119, 0.038, 0.049),
    stringsAsFactors = FALSE
  )
  
  output$Sensitivity <- renderTable({
    df <- Sensitivity
    for (col in c("Threshold_0.05","Threshold_0.10","Threshold_0.15","Threshold_0.20","Threshold_0.25","Threshold_0.30")) {
      df[[col]] <- paste0(formatC(df[[col]], format="f", digits=3), "–", 
                          formatC(df[[paste0("Threshold2_", sub("Threshold_", "", col))]], format="f", digits=3))
    }
    df <- df[, c("Scenario", "Threshold_0.05","Threshold_0.10","Threshold_0.15","Threshold_0.20","Threshold_0.25","Threshold_0.30")]
    colnames(df) <- c("Scenario","5% Range","10% Range","15% Range","20% Range","25% Range","30% Range")
    df
  }, sanitize.text.function = function(x) x)
  
  # --- Specificity Table (Pre/Post as Range) ---
  Specificity <- data.frame(
    Scenario = c("Aggregate Offences (ages 10-16)","Aggregate Offences (ages 10-13)", "Aggregate Offences (ages 14-16)"),
    Threshold_0.05 = c(0.003,0.416, 0.766),
    Threshold_0.10 = c(0.413,0.923, 0.946),
    Threshold_0.15 = c(0.858,0.968, 0.976),
    Threshold_0.20 = c(0.941,0.982, 0.987),
    Threshold_0.25 = c(0.962,0.988, 0.992),
    Threshold_0.30 = c(0.976,0.992, 0.995),
    Threshold2_0.05 = c(0.002, 0.472, 0.766),
    Threshold2_0.10 = c(0.413, 0.927, 0.942),
    Threshold2_0.15 = c(0.855, 0.967, 0.974),
    Threshold2_0.20 = c(0.940, 0.980, 0.985),
    Threshold2_0.25 = c(0.960, 0.985, 0.990),
    Threshold2_0.30 = c(0.975, 0.988, 0.993),
    stringsAsFactors = FALSE
  )
  
  output$Specificity <- renderTable({
    df <- Specificity
    for (col in c("Threshold_0.05","Threshold_0.10","Threshold_0.15","Threshold_0.20","Threshold_0.25","Threshold_0.30")) {
      df[[col]] <- paste0(formatC(df[[col]], format="f", digits=3), "–", 
                          formatC(df[[paste0("Threshold2_", sub("Threshold_", "", col))]], format="f", digits=3))
    }
    df <- df[, c("Scenario", "Threshold_0.05","Threshold_0.10","Threshold_0.15","Threshold_0.20","Threshold_0.25","Threshold_0.30")]
    colnames(df) <- c("Scenario","5% Range","10% Range","15% Range","20% Range","25% Range","30% Range")
    df
  }, sanitize.text.function = function(x) x)
  
  
  # --- Dynamic Inputs ---
  output$dynamic_inputs <- renderUI({
    req(input$model_select)
    preds <- predictors_list[[input$model_select]]
    input_list <- lapply(preds, function(pred) {
      label <- nice_name(pred)
      if(pred == "Gender") {
        radioButtons("Gender", label, choices = list("Female"="2","Male"="3"), selected="3")
      } else numericInput(pred,label=label,value=0,min=0)
    })
    do.call(tagList, input_list)
  })
  
  output$indicator_dynamic_inputs <- renderUI({
    req(input$indicator_model_select)
    preds <- predictors_list[[input$indicator_model_select]]
    input_list <- lapply(preds,function(pred){
      label <- nice_name(pred)
      if(pred=="Gender") radioButtons("indicator_Gender", label, choices=list("Female"="2","Male"="3"), selected="3")
      else numericInput(paste0("indicator_",pred), label=label,value=0,min=0)
    })
    do.call(tagList,input_list)
  })
  
  # --- Reactive Inputs ---
  input_data <- reactive({
    req(input$model_select)
    preds <- predictors_list[[input$model_select]]
    vals <- lapply(preds,function(pred){
      if(pred=="Gender") factor(input$Gender, levels=c("2","3"))
      else as.numeric(ifelse(is.null(input[[pred]]),0,input[[pred]]))
    })
    names(vals) <- preds
    as.data.frame(vals, stringsAsFactors=FALSE)
  })
  
  baseline_data <- reactive({
    req(input$model_select)
    preds <- predictors_list[[input$model_select]]
    baseline_gender <- if(input$baseline_type=="Female Baseline") "2" else "3"
    vals <- lapply(preds,function(pred){
      if(pred=="Gender") factor(baseline_gender, levels=c("2","3")) else 0
    })
    names(vals) <- preds
    as.data.frame(vals, stringsAsFactors=FALSE)
  })
  
  indicator_input_data <- reactive({
    req(input$indicator_model_select)
    preds <- predictors_list[[input$indicator_model_select]]
    vals <- lapply(preds,function(pred){
      if(pred=="Gender") factor(input$indicator_Gender, levels=c("2","3"))
      else as.numeric(ifelse(is.null(input[[paste0("indicator_",pred)]]),0,input[[paste0("indicator_",pred)]]))
    })
    names(vals) <- preds
    as.data.frame(vals, stringsAsFactors=FALSE)
  })
  
  indicator_baseline_data <- reactive({
    req(input$indicator_model_select)
    preds <- predictors_list[[input$indicator_model_select]]
    baseline_gender <- if(input$indicator_baseline_type=="Female Baseline") "2" else "3"
    vals <- lapply(preds,function(pred){
      if(pred=="Gender") factor(baseline_gender, levels=c("2","3")) else 0
    })
    names(vals) <- preds
    as.data.frame(vals, stringsAsFactors=FALSE)
  })
  
  # --- Results Calculator ---
  results <- eventReactive(input$calc_btn,{
    model <- models[[input$model_select]]
    newdata <- input_data()
    base_data <- baseline_data()
    pred_prob <- as.numeric(predict(model,newdata=newdata,type="response"))
    base_prob <- as.numeric(predict(model,newdata=base_data,type="response"))
    change <- pred_prob - base_prob
    rr <- if(base_prob>0) pred_prob/base_prob else NA_real_
    list(probability=pred_prob, baseline=base_prob, change=change, rr=rr)
  })
  
  # --- Results Indicator ---
  indicator_results <- eventReactive(input$indicator_calc_btn,{
    model <- models[[input$indicator_model_select]]
    newdata <- indicator_input_data()
    base_data <- indicator_baseline_data()
    pred_prob <- as.numeric(predict(model,newdata=newdata,type="response"))
    base_prob <- as.numeric(predict(model,newdata=base_data,type="response"))
    change <- pred_prob - base_prob
    list(probability=pred_prob, baseline=base_prob, change=change)
  })
  
  # --- Shrinkage Application (Calculator) ---
  current_results <- reactive({
    req(results())
    res <- results()
    if (input$apply_shrinkage) {
      scenario <- input$model_select
      metrics <- shrinkage_metrics[shrinkage_metrics$Scenario == scenario, ]
      
      logit <- log(res$probability / (1 - res$probability))
      shrunken_logit <- metrics$Calibration_in_the_Large + metrics$Calibration_Slope * logit
      shrunken_prob <- exp(shrunken_logit) / (1 + exp(shrunken_logit))
      
      baseline_logit <- log(res$baseline / (1 - res$baseline))
      shrunken_baseline <- exp(metrics$Calibration_in_the_Large + metrics$Calibration_Slope * baseline_logit) /
        (1 + exp(metrics$Calibration_in_the_Large + metrics$Calibration_Slope * baseline_logit))
      
      res$probability <- shrunken_prob
      res$baseline <- shrunken_baseline
      res$change <- shrunken_prob - shrunken_baseline
      res$rr <- ifelse(shrunken_baseline > 0, shrunken_prob / shrunken_baseline, NA_real_)
    }
    res
  })
  
  # --- Shrinkage Application (Indicator) ---
  current_indicator_results <- reactive({
    req(indicator_results())
    res <- indicator_results()
    if (input$indicator_apply_shrinkage) {
      scenario <- input$indicator_model_select
      metrics <- shrinkage_metrics[shrinkage_metrics$Scenario == scenario, ]
      
      logit <- log(res$probability / (1 - res$probability))
      shrunken_logit <- metrics$Calibration_in_the_Large + metrics$Calibration_Slope * logit
      shrunken_prob <- exp(shrunken_logit) / (1 + exp(shrunken_logit))
      
      baseline_logit <- log(res$baseline / (1 - res$baseline))
      shrunken_baseline <- exp(metrics$Calibration_in_the_Large + metrics$Calibration_Slope * baseline_logit) /
        (1 + exp(metrics$Calibration_in_the_Large + metrics$Calibration_Slope * baseline_logit))
      
      res$probability <- shrunken_prob
      res$baseline <- shrunken_baseline
      res$change <- shrunken_prob - shrunken_baseline
    }
    res
  })
  
  # --- Outputs ---
  output$prob_numeric <- renderText({
    req(current_results())
    paste0(formatC(current_results()$probability*100, format="f", digits=2), "%")
  })
  output$change_numeric <- renderText({
    req(current_results())
    paste0(formatC(current_results()$change*100, format="f", digits=2), "%")
  })
  output$rr_numeric <- renderUI({
    req(current_results())
    rr <- current_results()$rr
    if (is.na(rr)) return(HTML("<span style='color:black;'>N/A</span>"))
    
    if (rr >= 30) {
      color <- "red"
      rr_text <- "&gt; 30&nbsp;times higher"
    } else if (rr <= 1/30) {
      color <- "green"
      rr_text <- "&gt; 30&nbsp;times lower"
    } else if (abs(rr - 1) < 0.005) {
      color <- "blue"
      rr_text <- "No Change"
    } else if (rr > 1) {
      color <- "red"
      rr_text <- paste0(formatC(rr, format = "f", digits = 2), " times higher")
    } else {
      color <- "green"
      rr_text <- paste0(formatC(1/rr, format = "f", digits = 2), " times lower")
    }
    
    HTML(paste0("<span style='color:", color, ";'>", rr_text, "</span>"))
  })
  
  output$indicator_message <- renderUI({
    req(current_indicator_results())
    change <- current_indicator_results()$change
    msg <- if (abs(change) < 0.005) {
      "No Difference in Risk vs Selected Baseline Risk"
    } else if (change > 0) {
      "Potential Increase in Risk vs Selected Baseline Risk"
    } else {
      "Potential Decrease in Risk vs Selected Baseline Risk"
    }
    color <- if (abs(change) < 0.005) "blue" else if (change > 0) "red" else "green"
    HTML(paste0("<h4 style='color:", color, ";'>", msg, "</h4>"))
  })
  
  # --- Download CSV ---
  output$download_btn <- downloadHandler(
    filename = function() paste0("risk_results_", Sys.Date(), ".csv"),
    content = function(file) {
      req(current_results())
      df <- data.frame(
        Model = input$model_select,
        Probability = formatC(current_results()$probability,format="f",digits=4),
        Baseline_Probability = formatC(current_results()$baseline,format="f",digits=4),
        Change_from_Baseline = formatC(current_results()$change,format="f",digits=4),
        Relative_Risk = ifelse(is.na(current_results()$rr), NA, formatC(current_results()$rr,format="f",digits=4)),
        stringsAsFactors = FALSE
      )
      write.csv(df, file, row.names=FALSE)
    }
  )
  
}

# --- Launch the App ---
shinyApp(ui, server)

