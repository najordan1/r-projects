library(lavaan)
library(dplyr)
library(likert)
library(patchwork)
library(ggplot2)

#' Makes a likert graph displaying responses to question by a group,
#' which should be a factor variable
#' for a given question
#' @author Nathan Jordan
#'
#' @param question - the survey question to plot
#' @param group - which grouping should be used
#'
#' @return a likert plot
create_likert <- function(question, group) {
  # Determine correct (ish) labels and colors for responses
  # Obviously, should be changed to correct labels in a given setting, could
  # potentially refactor to be a parameter
  # Color palette from https://colorbrewer2.org/#type=diverging&scheme=RdBu&n=5
  num_levels <- length(data |> select(question) |> na.omit() |> distinct() |> pull())
  if (num_levels == 5) {
    levels <- c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree", "Agree", "Strongly Agree")
    colors <- c("#ca0020", "#f4a582", "#f7f7f7", "#92c5de", "#0571b0")
  } else {
    levels <- c("Not Important", "Unimportant", "Important", "Very Important")
    colors <- c("#ca0020", "#f4a582", "#92c5de", "#0571b0")
  }
  
  # Change responses to factor
  items <- data |>
    mutate(q = levels[eval(sym(question))],
           q = factor(q, levels = levels))
  
  grouping <- data$group
  items <- items |> select(q)
  # rename to the written out question
  names(items) <- c(questions[question])
  
  # create and return the plot
  likert_object <- likert(as.data.frame(items), grouping=grouping)
  return_plot <- likert.bar.plot(likert_object, colors=colors) + theme_minimal()
  
  return(return_plot)
}

#' Use patchwork package to make and group multiple likert graphs together
#' by underlying concept
#' @author Nathan Jordan
#'
#' @param concept - which concept or latent variable group to visualize
#' @param group - which grouping should be used ('technocrats' or 'agencies')
#'
#' @return a patchwork of likert graphs, 1 for each question in the concept
create_graphs_by_concept <- function(concept, group) {
  # Get questions relating to the concept
  # example format for the questions and concepts objects:
  #    questions <- c(Q1 = "Question 1", etc)
  #    concepts <- c(democracy = c(Q1, Q2, Q3, Q4))
  questions <- concepts[[concept]]
  # Make a graph per question, saving it by question name
  for (question in questions) {
    plot <- create_likert(question, group)
    # By saving the graph by the iteration variable name, I can easily
    # access it in the patchwork step by string concatenation
    assign(question, plot)
  }
  # Creating and returning the 'patchwork' of graphs, sharing a title and legend if possible
  title <- paste0("plot_annotation(title='", sym(concept), "')")
  eval(
    parse(
      text = paste(
        paste(questions, collapse = " + "),
        "plot_layout(guides='collect')",
        title, sep = " + "
      )
    )
  )
}

#' Create a multi-group confirmatory factor analysis model
#' @author Nathan Jordan
#'
#' @param concept - the concept representing the latent variable
#' @param level - the invariance level ('configural', 'metric', 'scalar', 'strict')
#'
#' @return a lavaan object containing the model
create_lavaan_model <- function(concept, level) {
  # See comment in first function for definition of concepts
  model <- paste(sym(concept), '=~', paste(concepts[[concept]], collapse = " + "))
  if (level == 'configural') {
    lavObject <- lavaan::cfa(model, data, group = 'group',
                             std.lv=T, estimator = "DWLS")
  } else {
    lavObject <- lavaan::cfa(model, data, group = 'group', std.lv=T,
                             group.equal=equality_constraints[[level]],
                             estimator = "DWLS")
  }
  return(lavObject)
}

#' Tests measurement invariance of a model on a data frame and group variable
#' @author Nathan Jordan
#'
#' @param concept - the concept representing the latent variable
#' 
#' @return a list with the following attributes:
#' model - a lavaan object, or NULL if configural fails
#' level - the level of invariance established
invariance_tester <- function(concept) {
  # Run and print configural invariance test
  configural <- create_lavaan_model(concept, 'configural')
  configuralFits <- fitMeasures(configural, fit_indices)
  cat("Configural Invariance Test:\n")
  print(configuralFits)
  
  # Return null if fails to pass configural
  if (configuralFits['cfi'] < 0.9 | configuralFits['rmsea'] > 0.08) {
    cat("\nFailed to establish configural invariance")
    return(list(model = NULL, level = 'none'))
  }
  
  # Run and print metric invariance test
  metric <- create_lavaan_model(concept, 'metric')
  metricFits <- fitMeasures(metric, fit_indices)
  cat("\nMetric Invariance Test:\n")
  print(metricFits)
  
  # If fails metric, return configural model
  if (configuralFits[['cfi']] - metricFits[['cfi']] > 0.1 | metricFits[['rmsea']] - configuralFits[['rmsea']] > 0.1) {
    cat("\nFailed to establish metric invariance")
    return(list(model = configural, level = 'configural'))
  }
  
  # Run and print scalar invariance test
  scalar <- create_lavaan_model(concept, 'scalar')
  scalarFits <- fitMeasures(scalar, fit_indices)
  cat("\nScalar Invariance Test:\n")
  print(scalarFits)
  # If fails scalar, return metric model
  if (metricFits[['cfi']] - scalarFits[['cfi']] > 0.1 | scalarFits[['rmsea']] - metricFits[['rmsea']] > 0.1) {
    cat("\nFailed to establish scalar invariance")
    return(list(model = metric, level = 'metric'))
  }
  
  # Return scalar
  return(list(model = scalar, level = 'scalar'))
}

#' Explores latent variables by testing for measurement invariance, and if passing,
#' visualizing and summarizing the weighted sum score of the variable across the groups
#' @author Nathan Jordan
#'
#' @param concept - the latent concept to explore
#'
#' @return
latent_variable_explorer <- function(concept){
  cfa_object <- invariance_tester(concept)
  if (cfa_object$level != 'scalar') {
    return()
  }
  # Get questions and corresponding factor loadings
  concept_questions <- concepts[[concept]]
  num_questions <- length(concept_questions)
  factor_loadings <- parameterestimates(cfa_object$model)[['est']][1:num_questions]
  cat("\nFactor loadings:\n")
  cat(factor_loadings)
  # Create a weighted sum score of responses by factor loading
  sum <- factor_loadings[[1]] * data[[concept_questions[[1]]]]
  for (i in 2:num_questions) {
    sum <- sum + (factor_loadings[[i]] * data[[concept_questions[[i]]]])
  }
  weighted_sum <- sum / num_questions
  # Add weighted sum score to copy of the data
  df <- data
  df[[sym(concept)]] <- weighted_sum
  # Print summary statistics
  cat("\n\n\n")
  cat("Summary across all respondents:\n")
  print(summary(df[[sym(concept)]]))
  cat("\nPolitical technocrats:\n")
  print(summary((df |> filter(group == "Political Technocrats"))[[sym(concept)]]))
  cat("\nOther respondents:\n")
  print(summary((df |> filter(group == "Rest"))[[sym(concept)]]))
  df |> select(group, sym(concept)) |> na.omit() |>
    ggplot(aes(eval(sym(concept)), group)) +
    geom_jitter(alpha = 0.15, size = 2.5, color = "#2c7fb8", height=0.1) +
    theme_minimal() + labs(y="", x=sym(concept))
}