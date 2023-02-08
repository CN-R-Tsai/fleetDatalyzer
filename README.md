# f-Datalyzer Web Application
Updated on February 08, 2023 Author: C.N. Roger T.

👋 Hello there! Thank you for taking the time to visit my Github repo. :pray:

## Introduction
This web application is developed to help to improve company's installed based equipment performance for fleet productivity, uptime, throughput and quality optimization via the analysis(analytical, statistical, and machine learning methods) of equipment sensor data from semiconductor manufacturing equipment. The goal is to build a `centralized dashboard application` which can providing information and comprehensive insights of data analysis.

## Impacts of f-Datalyzer Web Application
System failures and process variation are able to quickly identify from hundreds of sensors and process steps) on a fleet ( > 100+ processing chambers), leading to a reduction in Mean Time To repair (MTTR) by 35%, an increase in tool productivity by up to 30%, and an elimination in production wafer scrap by 15%.

## Application Overview
`f-Datalyzer` provides useful analytics to improve fleet productivity. Besides, machine learning methods are implemented for anomaly detection in time-series sensor data to predict system failures.

It is designed as two main dashboards as shown below. 
| Analytics | Fleet Insight |
| --- | --- |
| CSV file upload | Fleet traffic |
| Reduced dimension plot (Principle Component Analysis, PCA) | Chamber ranking |
| Data visualization (scatter plot, box plot , etc) | Detail view|
| Outlier detection (MD, ln. MD) ||
| Feature importance ranking ||
| Regression analysis ||


## Technologies Used
The `f-Datalyzer` app is built using,

- R
    - `shiny`[^1]
    - `ggplot2` 
    - `tidyverse`
    - `etc`

- HTML/CSS/Javascript

- PostgreSQL

- Web server
    - `Shiny server`

- Docker

[^1]:(https://shiny.rstudio.com/): Shiny is an R package to build interactive web apps, running upon on R programming.

## Why R/Shiny?
- Do the job and delivery business value
- Provide frontend GUI (Shiny)
   -  Framework that allows users to create interactive web apps
     - Shiny abstracts the key technologies underlying modern web design (HTML, CSS, JavaScript)
- Tons of packages (ggplot2, tidyverse, etc.) are available & well-maintained  
- Save results to database (PostgreSQL, SQLite) and provides a nice dashboard for business to see value
- I am a shiny lover :heartpulse:, proficient in R 


## 🏗️ Using Shiny Modules to simplify complex application
Since the web application is expected to be a large-scale application, at the beginning of the design process, I take advantage of a technique called Shiny modules to solve the complexities and to manage the code easily.

Modules are one of the most powerful tools for building shiny applications in a maintainable and sustainable manner.
- Compose complex apps out of smaller, more understandable pieces.
- The app is divided up into pieces and each piece has a name. Naming the pieces means that the names of the controls can be simpler. ← namespacing
- Organize code into logical and easy-to-understand components.
- Reuse code is possible.
- Module is black box (input & output). It simplifier the structure to the whole app (no more Spaghetti code).
- Facilitates collaboration! 👍

## Screenshots
Here is a snap from the application shows The images were removed due to intellectual property rights.
> ## Analytics Dashboard Demo 
<img src='login_page.gif' art='switch' width="500" height="400" />

> ## Fleet Insight Dashboard Demo
<img src='tool_productivity_ranking.gif' art='switch' width="500" height="400" />
