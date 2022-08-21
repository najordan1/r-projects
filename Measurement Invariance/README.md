# Survey Data and Latent Variable Tools

Often in political science research, we need to quickly and easily explore responses to survey questions. Additionally, when considering underlying latent variables in our data, a quality measurement invariance test must be run in order to compare scores on latent variables/weighted sum scores among different groups in the data. This code contains 5 functions, outlined below, that I used to efficiently explore survey data.
  - `create_likert(df, question, group, labels)`: Creates a likert graph of responses to a survey question, showing distribution of responses by sample subgrouping.
  - `create_graphs_by_concept(df, concept, questions, group, labels)`: Creates a 'patchwork' of likert graphs by a predefined concept. For example, if 4 survey questions relate to meritocracy, it displays all 4 likert graphs on one plot.
   - `create_lavaan_model(df, concept, questions, level)`: Creates a lavaan object of a latent variable model, constraining subgroupings to be equal at one of the 4 levels of invariance testing (configural, metric, scalar, residual).
   - `invariance_tester(df, concept, questions)`: Tests whether or not a latent variable passes measurement invariance testing, displaying fit measures of each level on console output.
   - `latent_variable_explorer(df, concept, questions)`:  Runs measurement invariance testing on a given concept, and returns a graph displaying weighted sum scores by grouping.
