library(lavaan)
library(dplyr)
library(likert)
library(patchwork)
library(ggplot2)

# These are the 4 levels of measurement invariance testing
equality_constraints <- list(
  configural = c(),
  metric = c("loadings"),
  scalar = c("loadings", "intercepts"),
  strict = c("loadings", "intercepts", "residuals")
)

# The fitness metrics we're interested in
fit_indices <- c("chisq", "df", "pvalue", "cfi", "rmsea")

#' Makes a likert graph displaying responses to question by a group,
#' which should be a factor variable for a given question
#' @author Nathan Jordan
#'
#' @param df - the dataframe
#' @param question - the survey question to plot
#' @param group - a factor variable in the data indicating group
#' Ex: group = factor(group, levels = c("Political Technocrats", "Bureacratic Technocrats", "Rest"))
#' @param labels - a list mapping column name to full label for display in the plot
#' Ex. labels = c(QA1 = "What is your gender?")
#'
#' @return a likert plot
create_likert <- function(df, question, group, labels) {
  # Determine the number of likert levels exist in this question
  num_levels <- length(
    df |>
      select(question)|>
      na.omit() |>
      distinct() |>
      pull()
    )
  
  # Determine correct (ish) labels and colors for the likert scale items
  # TODO: refactor to a parameter
  # Color palette from https://colorbrewer2.org/#type=diverging&scheme=RdBu&n=5
  if (num_levels == 5) {
    levels <- c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree", "Agree", "Strongly Agree")
    colors <- c('#a6611a','#dfc27d','#f5f5f5','#80cdc1','#018571')
  } else {
    levels <- c("Not Important", "Unimportant", "Important", "Very Important")
    colors <- c('#a6611a','#dfc27d','#80cdc1','#018571')
  }
  
  # Change responses to factor
  items <- df |>
    mutate(
      q = levels[eval(sym(question))],
      q = factor(q, levels = levels)
    )
  
  grouping <- df$group
  items <- items |> select(q)
  # rename to the written out question
  names(items) <- c(labels[question])
  
  # create and return the plot
  likert_object <- likert(as.data.frame(items), grouping=grouping)
  return_plot <- likert.bar.plot(likert_object, colors=colors) + theme_minimal()
  
  return(return_plot)
}

#' Use patchwork package to make and group multiple likert graphs together
#' by underlying concept
#' @author Nathan Jordan
#'
#' @param df - the dataframe
#' @param concept - which concept or latent variable group to visualize
#' @param questions - a list of the questions/cols for this concept
#' @param group - a factor variable in the data indicating group
#' Ex: group = factor(group, levels = c("Political Technocrats", "Bureacratic Technocrats", "Rest"))
#' @param labels - a list mapping column name to full label for display in the plot
#' Ex. labels = c(QA1 = "What is your gender?")
#'
#' @return a patchwork of likert graphs, 1 for each question in the concept
create_graphs_by_concept <- function(df, concept, questions, group, labels) {
  # Make a graph per question, saving it by question name
  for (question in questions) {
    plot <- create_likert(df, question, group, labels)
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
#' @param df - the dataframe
#' @param concept - the concept representing the latent variable
#' @param questions - a list of the questions/cols for this concept
#' @param level - the invariance level ('configural', 'metric', 'scalar', 'strict')
#'
#' @return a lavaan object containing the model
create_lavaan_model <- function(df, concept, questions, level) {
  # Format the model for lavaan, which expects LV =~ Q1 + ... + Qn
  model <- paste(sym(concept), '=~', paste(questions, collapse = " + "))
  
  if (level == 'individual') {
    lavObject <- lavaan::cfa(
      model,
      df,
      std.lv = T,
      estimator = "DWLS"
    )
  } else if (level == 'configural') {
    lavObject <- lavaan::cfa(
      model,
      df,
      group = 'group',
      std.lv = T,
      estimator = "DWLS" #this is for likert survey questions, should generalize with param
    )
  } else {
    lavObject <- lavaan::cfa(
      model,
      df,
      group = 'group',
      std.lv=T,
      group.equal=equality_constraints[[level]],
      estimator = "DWLS"
    )
  }
  return(lavObject)
}

#' Tests measurement invariance of a model on a data frame and group variable
#' @author Nathan Jordan
#'
#' @param df - the dataframe
#' @param concept - the concept representing the latent variable
#' @param questions - a list of the questions/cols for this concept
#' 
#' @return a list with the following attributes:
#' model - a lavaan object, or NULL if configural fails
#' level - the level of invariance established
invariance_tester <- function(df, concept, questions) {
  
  # Run and print configural invariance test
  configural <- create_lavaan_model(df, concept, questions, 'configural')
  configuralFits <- fitMeasures(configural, fit_indices)
  cat("Configural Invariance Test:\n")
  print(configuralFits)
  
  # Return null if fails to pass configural
  if (configuralFits['cfi'] < 0.9 | configuralFits['rmsea'] > 0.08) {
    cat("\nFailed to establish configural invariance")
    return(list(model = NULL, level = 'none'))
  }
  
  # Run and print metric invariance test
  metric <- create_lavaan_model(df, concept, questions, 'metric')
  metricFits <- fitMeasures(metric, fit_indices)
  cat("\nMetric Invariance Test:\n")
  print(metricFits)
  
  # If fails metric, return configural model
  if (configuralFits[['cfi']] - metricFits[['cfi']] > 0.1 | metricFits[['rmsea']] - configuralFits[['rmsea']] > 0.1) {
    cat("\nFailed to establish metric invariance")
    return(list(model = configural, level = 'configural'))
  }
  
  # Run and print scalar invariance test
  scalar <- create_lavaan_model(df, concept, questions, 'scalar')
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
#' @param df - the dataframe
#' @param concept - the latent concept to explore
#' @param questions - a list of the questions/cols for this concept
#'
#' @return
latent_variable_explorer <- function(df, concept, questions){
  # Run invariance tests, return if scalar not established
  # TODO: create optional param that sets what level we're ok with
  cfa_object <- invariance_tester(df, concept, questions)
  if (cfa_object$level != 'scalar') {
    return()
  }
  
  # Print factor loadings of model
  # Get corresponding factor loading for each question
  num_questions <- length(questions)
  factor_loadings <- parameterestimates(cfa_object$model)[['est']][1:num_questions]
  cat("\nFactor loadings:\n")
  cat(factor_loadings)
  
  # Create a weighted sum score of responses by factor loading
  # This is assuming that the factor loading for the first column is a 1.
  # There are some optional configs in lavaan thatd can change this, so needs
  # generalizing
  sum <- factor_loadings[[1]] * df[[questions[[1]]]]
  for (i in 2:num_questions) {
    sum <- sum + (factor_loadings[[i]] * df[[questions[[i]]]])
  }
  weighted_sum <- sum / num_questions
  # Add weighted sum score to copy of the data
  df_copy <- df
  df_copy[[sym(concept)]] <- weighted_sum
  
  # Print summary statistics overall
  cat("\n\n\n")
  cat("Summary across all respondents:\n")
  print(summary(df_copy[[sym(concept)]]))
  
  # Print summary statistics for each group
  num_groups <- levels(df$group)
  
  for (i in 1:num_groups) {
    level <- levels(df$group)[[i]]
    cat(paste("\n", level, ":\n"))
    print(summary((df_copy |> filter(group == level))[[sym(concept)]]))
  }

  df_copy |> select(group, sym(concept)) |> na.omit() |>
    ggplot(aes(eval(sym(concept)), group)) +
    geom_jitter(alpha = 0.15, size = 2.5, color = "#2c7fb8", height=0.1) +
    theme_minimal() + labs(y="", x=sym(concept))
}