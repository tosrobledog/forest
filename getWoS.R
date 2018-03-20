# Ambiente de trabajo
library(bibliometrix)
setwd("C:/Users/Unalman/Downloads")

# Subir archivos

D <- readFiles("D1.txt", "D2.txt", "D3.txt", "D4.txt")

M <- convert2df(D, dbsource = "isi", format = "plaintext")
View(M)


