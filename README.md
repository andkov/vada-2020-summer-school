![header](libs/images/header-1.png)

A repository to accompany selected applied sessions of the [2019 VADA Program Summer School](http://vada.cs.umanitoba.ca/program/program/summer-school/summer-school-2019/), June 10-14, at the George and Fay Yee Centre for Healthcare Innovation, Winnipeg, Manitoba. [Full program](libs/materials/Full-Week-Schedule-and-Descriptions.pdf) 

# Analytic Products

 - [technique demo][demo_complete] -  demonstrate a sequential development of a reproducible graphing system using `prep-plot-print` technique

[demo_complete]:https://raw.githack.com/andkov/vada-2019-summer-school/master/analysis/prep-plot-print/prep-plot-print.html

## Techniques for Reproducible Visualisation in R: Graph as a Sequence of Three Functions.   
Date : _2019-06-12-Wednesday_  
Time : _13:00 - 15:00_  

The session demonstrates a technique for organizing workflows that generate reproducible data visualisations. Rarely do applied data science projects produce any given graph only once.The need to generate plots of the same form using different inputs, different options, and in multiple contexts requires the analyst to structure the operations involved in graph production as customizable functions. The technique demonstrated in this session divides the production of data visualizations into three sections ( prep - plot - print ), each governed by a dedicated custom function. The first function prepares the data for graphing, the second plots the graphic, and the third prints the image to disk. This applied session will walk the learner through the stages of developing such a chain of functions and demonstrate the advantages of such an approach in reproducible projects. Data and starter scripts will be provided. 
Software in focus: RStudio, R, specifically _dplyr_ and _ggplot2_ packages. 

## Using github for data science: version control, project management, promotion  
Date: _2019-06-13-Thursday_  
Time: _8:30 - 10:30_  

The sessions offers a hands-on walk through a minimalistic project designed to introduce the learner to basic operational utility of git and github in data science projects. While git(hub) is best known for providing version control and streamlining collaboration in software development, its applications in data analytic projects can enhance the robustness of the produced solutions and help make results more visible and accessible to the community. The topics to be covered include: 1) project kickoff from a standard stencil (https://github.com/wibeasley/RAnalysisSkeleton ),  2) using Github client for version control 3) using Github Issues for structuring tasks in project development 3) team control and communication and 4) designing an effective README page that faces the public. Data and starter scripts will be provided. Software in focus: R, RStudio, github client. 

# Getting ready for sessions

To prepare the environment for our sessions, please follow the "required" section of the  [Installation Resources](https://github.com/OuhscBbmc/RedcapExamplesAndPatterns/blob/master/DocumentationGlobal/ResourcesInstallation.md) of the [Biomedical and Behavioral Methodology Core](https://ouhsc.edu/bbmc/) (BBMC) at the Oklahoma University Health Sciences Center (OUHSC). 

The data science lead of BBMC is [Will Beasley](https://github.com/wibeasley), my old friend from graduate school, whose ideas about data visualization, reproducible research and applied analytics continue to shape my practice. We will operate in the framework he developed: [RAnalysisSkeleton](https://github.com/wibeasley/ranalysisskeleton), my starting point for any data science project. Our `2019-06-13` session will focus on applying this template to publish on GitHub the results of our `2019-06-12` session on reproducible graphing.   

To prepare for the latter, please study the examples of _ggplot_ graphs Will has developed to illustrate this [textbook for introductory statistics](https://github.com/OuhscBbmc/DeSheaToothakerIntroStats/blob/master/thumbnails/thumbnails.md). Pay attention to the organization of the script in definition of _ggplot_ graphs. Specifically, notice typing conventions, the order of lines, and indentations. 


# How to reproduce

 1. Clone the repo   
 2. Save the excel files as `.csv` into `./data-unshared/raw/` folder  
 3. Run the `./manipulation/0-greeter.R` which would save the data transfer project (dto) into `./data-unshared/derived/` 
 4. Select the file from `./analysis/prep-plot-print/` that corresponds to the objective you would like to focus on
 
- `prep-plot-print-00-application.R` - demonstrates the application of the final sequence for graphing.
- `prep-plot-print.R` - demonstrates how the tool was built, progressing through development phase by phase.  

The technique for reproducible graphing demonstrated here procedes in the following sequence of phases:  

- PHASE 0 - explore the data  
- PHASE 1 - build the graph  
- PHASE 2 - build the `plot` function  
- PHASE 3 - split into `prep` and `plot` functions  
- PHASE 4 - add the `print` function   
- PHASE 5 - serialize graph production  
- PHASE 6 - `place` graphs onto canvas  


# Quick Links
- [Installation Resources](https://github.com/OuhscBbmc/RedcapExamplesAndPatterns/blob/master/DocumentationGlobal/ResourcesInstallation.md)
- [RAnalysisSkeleton](https://github.com/wibeasley/ranalysisskeleton)
- [Textbook for introductory statistics](https://github.com/OuhscBbmc/DeSheaToothakerIntroStats/blob/master/thumbnails/thumbnails.md)
- [Data Science Practices Style Guide](https://ouhscbbmc.github.io/data-science-practices-1/style-guide.htm)

