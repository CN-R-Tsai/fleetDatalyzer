# f-Datalyzer Web App V2 
*Updated on 06/06/22*

👋 Hello there! Thank you for visiting my Github repo. :pray:

> This web apps is aim to help field engineers to find out the system failure (from hundreds of sensors) on a fleet ( > 60+ processing chambers) when issue happened. To speed up the Root Cause Analysis and Corrective Action (RCCA). The ultimate goal is to build a `centralized dashboard app` which can providing information and comprehensive insights of data analysis. The app is called, `f-Datalyzer`. 

## Technologies Used

The `f-Datalyzer` app is built using,

- [Shiny](https://shiny.rstudio.com/): Shiny is an R package that makes it easy to build interactive web apps straight from R.

## Why R?
1. Provide frontend GUI (Shiny)
   -  Framework that allows users to create interactive web apps
     - Shiny abstracts the key technologies underlying modern web design (HTML, CSS, JavaScript)
2. Save results to database (PostgreSQL, SQLite) and provides a nice dashboard for business to see value
3. I am a shiny lover :heartpulse:, proficient in R 

## Apps Overview
`f-Datalyzer` provides some basic but useful analytics to understand the productivity of tools and process variation.
Two main functions are shown as below. 

| Analytics | Fleet Insight |
| --- | --- |
| CSV file upload (Standalone[^1]) | Fleet traffic |
| Reduced dimension plot (Principle Component Analysis, PCA) | Chamber ranking |
| Data visualization (scatter plot, box plot , etc) | Detail view|
| Outlier detection (MD, ln. MD) ||
| Feature importance ranking ||
| Regression analysis ||

[^1]: Partial data is extracted from SQLite database

Here is a snap from the app shows, it is user login page. 

The logo images are removed to avoid intellectual property violations. 
<img src='login_page.JPG' art='switch' width="600" height="600" />

# 🏗️ ## Using Shiny Modules to simplify complex apps
> Modules are one of the most powerful tools for building shiny applications in a maintainable and sustainable manner.
- Compose complex apps out of smaller, more understandable pieces
- The app is divided up into pieces and each piece has a name. Naming the pieces means that the names of the controls can be simpler. ← namespacing
Organize code into logical and easy-to-understand components
- Reuse code is possible
- Module is black box(input & output).  It  simplifier the structure to the whole app (no more Spaghetti code)
- Facilitates collaboration!👍



## Directory Structure

```
myapp                         
    └── i18n
        └── en.js
        └── es.js
        └── de.js
        └── fr.js
        └── index.js                
    └── node_modules                    
```




## Analytics Tab 🔗

## Fleet Insight Tab 🔗
<img src='tool_productivity_ranking.gif' art='switch' />


