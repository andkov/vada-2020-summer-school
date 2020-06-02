

A repository to accompany selected applied sessions of the [2020 VADA Program Summer School](http://vada.cs.umanitoba.ca/program/program/summer-school/)

![](./libs/images/header-1.png)
<!--[](./libs/images/summer-school-banner.png)-->

Visualizing the pandemic: comparing trajectories of COVID-19 across countries
-------------------

Wrangling and visualizing temporal data presents unique challenges to data analysists. This 1-hour workshop addresses the key tasks involved in exploration of timeseries and walks the participants through an applied example of obtaining COVID-19 mortality data from a live source (https://opendata.ecdc.europa.eu/covid19), computing relative timelines and various temporal metrics for individual countries, and designing  information displays for understanding global trends. Data and scripts are provided. Software requirements: R, RStudio, and tidyverse packages. 

# Learning Objectives

After this workshop participants should be able to:
 
1. Plot time series of COVID-19 cases using `ggplot2` package
2. Add interactive highlights to trajectories using `plotly` package
3. Compute indicators for key epidemiological events in each country (e.g. day of the first death)
4. Construct country-specific timelines relative to key epidemiological events
5. Visualize the sequence of key events for a group of countries

# Visualization Goals
During the session, we will walk through creating three graphs:
|Goal 1| Goal 2| Goal 3|
|---|---|---|  
|Timeseries with interactive highlights|Trajectories with relative timelines| Sequence of key epidemiological events|
|![][g1]|![][g2]|![][g3]|



# Getting started
Please open `./analysis/live-in-session/live-in-session.R` script in RStudio. Depending on your familiarity with programming, you have two options to do so: 
  - Opion 1. Launch RStudio, start a new R script and  copy-paste the contents of [this file](https://raw.githubusercontent.com/andkov/vada-2020-summer-school/master/analysis/live-in-session/live-in-session.R)   
  - Option 2. Clone this repo, launch the project in RStudio and open `./analysis/live-in-session/live-in-session.R`. If this instruction confuses you, please use Option 1.   


# Data  
The data comes from [European Centre for Disease Prevention and Control](https://www.ecdc.europa.eu/en), with the source available from [here](https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide). I demonstrate the preparation of this data for analysis in  [./manipulation/ellis-covid.R](https://github.com/andkov/vada-2020-summer-school/blob/master/manipulation/ellis-covid.R) script of this repository. 

# Bonus tracks  

If you are considering an anlytical project involving world-wide COVID-19, consider cloning the repository and studying the following scripts
- I have prepared an [narrated version of this workshop](https://raw.githack.com/andkov/vada-2020-summer-school/master/analysis/covid-trajectory/covid-trajectory-1.html), which you can study later on your own.
- [./manipulation/ellis-geography](./manipulation/ellis-geography.R) - prepares a reference table with exhaustive list of countries, their two- and three-letter codes, continents, and other useful info. If you are doing cross-country comparisons, consider investing time studying this data prep script, it may save you a lot time and frustration.
- Check out my [workshopw from the VADA 2019 summer school](https://github.com/andkov/vada-2019-summer-school/), in which I demonstrate the technique for functionalizing graphing functions. 

# About the speaker

![](./libs/images/akoval.jpg)
Andriy Koval is an Assistant Professor in the Department of Health Management and Informatics at the University of Central Florida. Dr. Koval earned degrees in Quantitative Methods (Ph.D., Vanderbilt), Psychology (M.A., MTSU) and Mass Communication (B.S., MTSU). His research combines longitudinal modeling, reproducible analytics and  data visualization to study how people engage health systems and services over the course of their lives. He lives in Orlando, FL. 


[g1]:./libs/images/goal_1.PNG
[g2]:./libs/images/goal_2.PNG
[g3]:./libs/images/goal_3.PNG

# Resources
Check out my [blogpost](http://andriy.rbind.io/post/2020-05-21-online-course-and-textbooks/) with online books and courses on `rstats` that I find particulary useful. 
