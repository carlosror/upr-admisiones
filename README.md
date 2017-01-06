## URL
https://carlosgg.shinyapps.io/admisiones-upr/

## Motivation
Given that the vast majority of Puertorrican high school graduates that pursue a higher education do so in 
the University of Puerto Rico (UPR) system, I thought it would be nice for a potential applicant to get an idea 
of what it would take for her to get admitted to a given campus, in terms of GPA and IGS. The target audience 
is a student in the ninth or tenth grade that wants to starts shaping her plans for the not-too-distant future.

## Data source
The data was obtained from [Portal de Interconexión de Datos Abiertos de Puerto Rico]
(https://data.pr.gov/en/Educaci-n/Admitidos-a-la-Universidad-de-Puerto-Rico-desde-20/uaij-e68c#column-menu).
It consists of approximately 70,000 records of admitted students to the UPR system from 2009 to 2013.

## Technologies

[R](https://leanpub.com/rprogramming), [R Studio](https://www.rstudio.com/), [Shiny R server](http://shiny.rstudio.com/articles/)

## Features

### Campus-wide visualizations
The "Campus" tab shows GPS/IGS data for students accepted to the campus on the acdemic year, both chosen 
on the left panel of the app. Below we can see a scatterplot, a boxplot, and a barplot for the Cayey campus 
of students admitted in the academic year 2013-2014.

### Popular majors
The tabs "Populares-F" and "Populares-M" show the most popular majors among incoming female and male students in a given academic year.
Here are the most popular majors among incoming male students to the Humacao campus and the corresponding average IGS, 
in the 2009-2010 academic year.

### Most competitive majors
The "Selectivos" tab shows the most selective majors in a given campus on a given year, as well as the average IGS of students admitted 
into those programs. For example, the most selective programs at UPR-Mayagüez, a.k.a. R.U.M., in the 2012-2013 academic year.

### High school provenance
The "Escuelas" tab shows the high schools with the largest number of admitted students to a given campus on a given year. Here 
are the hish schools with the most accepted students to the Río Piedras campus in 2013-2014.

### Top high schools
"Escuelas Destacadas" shows the high schools with the highest average IGS on a given academic year. 
The top high schools in the island of Puerto Rico! Here they are for the academic year 2011-2012.

### Table
Interactive table that allows the user to filter the results.

## References
1. Montijo, Adrián. (2014). ***Admitidos a la Universidad de Puerto Rico desde 2009 hasta 2013*** [CSV file]. Retrieved from https://data.pr.gov/en/Educaci-n/Admitidos-a-la-Universidad-de-Puerto-Rico-desde-20/uaij-e68c#column-menu

2. Adler, Joseph. ***R in a Nutshell, Second Edition***. Sebastopol: O'Reilly Media, 2012. PDF.

3. Chang, Winston. ***Cookbook for R***. Sebastopol: O'Reilly Media, 2013. Retrieved from http://www.cookbook-r.com/

4. Mine Çetinkaya-Rundel. ***Data Analysis and Statistical Inference***, Spring 2014. Coursera.

5. Chi Yau. ***R tutorials: Percentile***. Retrieved from http://www.r-tutor.com/elementary-statistics/numerical-measures/percentile

6. Forester, James. ***Reorder factor levels***. Retrieved from http://quantitative-ecology.blogspot.com/2007/10/reorder-factor-levels.html

7. Shiny Articles. Retrieved from http://shiny.rstudio.com/articles/

8. soosus and sgibb. ***The condition has length > 1 and only the first element will be used***. Retrieved from http://stackoverflow.com/questions/23316161/the-condition-has-length-1-and-only-the-first-element-will-be-used

9. DuBois, Christopher and Chang, Jonathan. ***Rotating and spacing axis labels in ggplot2*** Retrieved from http://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2
