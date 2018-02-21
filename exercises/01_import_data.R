
#set working directory
setwd("D:/...")

trees_1 <- read.table("trees_tab.txt", header = T)
str(trees_1)
head(trees_1)

trees_2 <- read.csv("tree_comma.csv")
head(trees_2)

trees_3 <- read.table("tree_semicolon.txt", header = T, sep = ";")
head(trees_3)
