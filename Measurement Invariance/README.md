<h2>Survey Data and Latent Variable Tools</h2>
<p>
    Often in political science research, we need to quickly and easily explore responses to survey questions. Additionally, when considering underlying latent variables in our data, a quality measurement invariance test must be run in order to compare scores on latent variables/weighted sum scores among different groups in the data. This code contains 5 functions, outlined below, that I used to efficiently explore survey data.
</p>
<ul>
    <li>
        <code>create_likert(question, group)</code>: Creates a likert graph of responses to a survey question, showing distribution of responses by sample subgrouping.
    </li>
    <li>
        <code>create_graphs_by_concept(concept, group)</code>: Creates a 'patchwork' of likert graphs by a predefined concept. For example, if 4 survey questions relate to meritocracy, it displays all 4 likert graphs on one plot.
    </li>
    <li>
        <code>create_lavaan_model(concept, level)</code>: Creates a lavaan object of a latent variable model, constraining subgroupings to be equal at one of the 4 levels of invariance testing (configural, metric, scalar, residual).
    </li>
    <li>
        <code>invariance_tester(concept)</code>: Tests whether or not a latent variable passes measurement invariance testing, displaying fit measures of each level on console output.
    </li>
    <li>
        <code>latent_variable_explorer(concept)</code>: Runs measurement invariance testing on a given concept, and returns a graph displaying weighted sum scores by grouping.
    </li>
</ul>