# Clean workspace
rm(list=ls())


setwd("~/Desktop/Eigth Semester/Stochastic Modelling/NFL_Project/All_Results")
stats2009 <- read.csv(file="Pij_4R_2009.csv", header=TRUE, sep=",") %>% select(-X)
stats2010 <- read.csv(file="Pij_4R_2010.csv", header=TRUE, sep=",") %>% select(-X)
stats2011 <- read.csv(file="Pij_4R_2011.csv", header=TRUE, sep=",") %>% select(-X)
stats2012 <- read.csv(file="Pij_4R_2012.csv", header=TRUE, sep=",") %>% select(-X)
stats2013 <- read.csv(file="Pij_4R_2013.csv", header=TRUE, sep=",") %>% select(-X)
stats2014 <- read.csv(file="Pij_4R_2014.csv", header=TRUE, sep=",") %>% select(-X)
stats2015 <- read.csv(file="Pij_4R_2015.csv", header=TRUE, sep=",") %>% select(-X)
stats2016 <- read.csv(file="Pij_4R_2016.csv", header=TRUE, sep=",") %>% select(-X)
stats2017 <- read.csv(file="Pij_4R_2017.csv", header=TRUE, sep=",") %>% select(-X)
stats2018 <- read.csv(file="Pij_4R_2018.csv", header=TRUE, sep=",") %>% select(-X)

min(colSums(stats2017))
