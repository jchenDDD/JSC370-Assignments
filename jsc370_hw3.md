---
title: "Assignment 3"
author: "John Chen"
date: "March 01, 2024"
output: 
  html_document:
    code_folding: hide
    theme: cerulean
    highlight: tango
    css: styles.css
    fig_caption: true
    self_contained: true
    keep_md: true
link-citations: true
---

# Introduction

This report aims to answer the question: When and where do most wild fires occur? The dataset I will be using includes 1.8 million recorded wild fires in the US from 1992 to 2015. The interest for this investigation was sparked by an sudden increase of wild fires around the world. I wish to examine the pattern in frequencies, as well as, the irregularities in frequencies of occurrences, in hopes to gain insight into finding the general 






```r
db <- dbConnect(RSQLite::SQLite(), "FPA_FOD_20170508.sqlite")
query <- "SELECT * FROM Fires"
result <- dbGetQuery(db, query)
dbDisconnect(db)
```




# Methods
