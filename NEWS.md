## version 1.1.0

---

This release added some basic package files, added customizable options to the plots in seqmeans and atsmeans functions, run unit tests of major functions, and made some user-facing and non-user-facing changes to the code in each function.

### Documentation changes:

* Added SMARTAR.R file for calling the function - help(SMARTAR)
* Added NEWS.md file to record changing history of package for calling the function - news(package="SMARTAR")
* Added vignettes file for demonstrating major functionality that runs successfully locally
* An open-source license was declared.

### Changes in functions:

* Added customizable options to the plots in seqmeans and atsmeans functions
  + xlab, ylab: Specification for the labels of x and y axis of the graphs.
  + title, ti: Specification for the title of the graphs.
  + lwd: An integer indicating the line width.
  + cex:  An integer indicating the amount by which plotting symbols should be magnified.
  + pch: An integer indicating shape of points in the graphs.
  + reference: If TRUE, add a reference line (mean of outcome) to boxplot of primary outcome, otherwise do not add reference line.
  + legend: Characters indicating the legend of design diagram of SMART.
  + color: Specification for the color of the graphs.
  + xtext: Specification for the text in x axis of the graphs.
* Changed margins of the plots in seqmeans and atsmeans functions.
* Deleted unnecessary rounding in the code.
* Changed simulated dataset in example code of each function by codiacs dataset.
* Limited the options of `family` and `method` arguments in `seqmeans` and
`atsmeans` function.
* Added summary() function to summarize the data frame got from `seqmeans` and
`atsmeans` function.
* Added `SD` column to the data frame got from `seqmeans`.
* Replaced the simulated data by CODIACS data in example codes of each function.

## version 1.0.0

---

First commit.

Primary data analysis for sequential multiple assignment randomization trial (SMART) and calibration tools for clinical trial planning purposes, which involves five major functions:

  + seqmeans - design diagram, descriptive statistics, and summarized graphs at sequence level;
  + atsmeans - descriptive statistics and summarized graphs at adaptive treatment strategy level;
  + smartest - results of global test and pairwise tests; output simultaneous CIs for ATS comparison;
  + smartsize - results of sample size calculation;
  + getncp - value of non-centralized parameter for the chi-square distribution.

