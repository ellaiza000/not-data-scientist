

# Read in Impulsivity1.csv file from:
# https://drive.google.com/drive/folders/0B5AwHsCad1WCdkRJVHNtX296dW8?resourcekey=0-4X9xX2xWo_rUh_IshnN33g
#
# Creating vectors of each column:
# Repodoxin or not:


TRT1 <- c("Repodoxin","Repodoxin",
          "Repodoxin","Repodoxin",
          "Repodoxin","Repodoxin",
          "Repodoxin","Repodoxin",
          "Repodoxin","Repodoxin",
          "Repodoxin","Repodoxin",
          "Repodoxin","Repodoxin",
          "Repodoxin","Repodoxin",
          "Repodoxin","Control",
          "Control","Control",
          "Control","Control",
          "Control","Control",
          "Control","Control",
          "Control")
# Gender of subject:
Gender1 <- c("Female","Male","Female",
             "Male","Male","Female",
             "Female","Male","Male",
             "Female","Female","Female",
             "Male","Male","Female",
             "Male","Male","Female",
             "Female","Male","Male",
             "Female","Male","Female",
             "Male","Female","Male")
# Subject number:
Subject1 <- c(24, 25, 26, 27, 28, 29,
              30, 31, 32, 33, 34, 35,
              36, 37, 38, 39, 40, 01,
              02, 03, 04, 05, 06, 07,
              08, 09, 10)
# Time1
T1 <- c(44, 42, 34, 45, 42, 37,
        41, 42, 44, 43, 38, 42,
        42, 48, 40, 41, 47, 59,
        58, 59, 54, 60, 62, 59,
        53, 56, 57)
# Time2
T2 <- c(44, 44, 44, 42, 39, 44,
        46, 47, 41, 41, 45, 43,
        43, 42, 37, 44, 42, 57,
        56, 66, 54, 57, 58, 56,
        51, 59, 64)
# Time3
T3 <- c(48, 41, 44, 35, 44, 45,
        46, 42, 38, 44, 46, 46,
        41, 42, 41, 43, 45, 62,
        49, 64, 59, 58, 64, 58,
        52, 55, 61)
# Combining all into one dataset:
Impulsivity1 <- data.frame( TRT = TRT1, # TRT = Column name and TRT1 = the vector it's retrieving data from
                         Gender = Gender1,
                         Subject = Subject1,
                         Time1 = T1,
                         Time2 = T2,
                         Time3 = T3)
# Subsetting by treatment (Repodoxin or not):
#          this is subsetting by row
#           [row, column]...... space after comma is empty because
#                                             we want all columns
Repodoxin <- Impulsivity1[ Impulsivity1$TRT == "Repodoxin", ]
Control <- Impulsivity1[ Impulsivity1$TRT == "Control", ]

# Subsetting by gender (Male or Female):
Male <- Impulsivity1[ Impulsivity1$Gender == "Male", ]
Female <- Impulsivity1[ Impulsivity1$Gender == "Female", ]

# Finding summaries for each of the five datasets:
# AND CREATING DATAFRAMES OF SUMMARIES AT THE SAME TIME
# Summary function:
mysummary1 <- function(x1, name1 = "x"){
  xmean1 <- mean(x1) 
  xsd1 <- sd(x1)  
  xmedian1 <- median(x1) #xmedian1 is a local variable. only exists in the function, not R.
  xQ1 <- quantile( x1,0.25)
  xQ3 <- quantile( x1,0.75)
  xmax1 <- max (x1)
  xmin1 <- min(x1)
  output1 <- data.frame( mean = xmean1,
                         sd = xsd1,
                         median = xmedian1,
                         Q1 = xQ1,
                         Q3 = xQ3,
                         max = xmax1,
                         min = xmin1)
  rownames( output1) <- name1
  return(output1)
}
# Impulsivity1 summary: mean impulsivity increased from Time1 to Time3
Impulsivity1.summary <- rbind(mysummary1(Impulsivity1$Time1, "Time1"),
                      mysummary1(Impulsivity1$Time2, "Time2"),
                      mysummary1(Impulsivity1$Time3, "Time3"))
# Repodoxin summary: mean impulsivity increased from Time1 to Time3
Repodoxin.summary <- rbind(mysummary1(Repodoxin$Time1, "Time1"),
                              mysummary1(Repodoxin$Time2, "Time2"),
                              mysummary1(Repodoxin$Time3, "Time3"))
# Control summary: mean impulsivity increased from Time1 to Time3
Control.summary <- rbind(mysummary1(Control$Time1, "Time1"),
                              mysummary1(Control$Time2, "Time2"),
                              mysummary1(Control$Time3, "Time3"))
# Male summary: mean impulsivity decreased from Time1 to Time3
Male.summary <- rbind(mysummary1(Male$Time1, "Time1"),
                              mysummary1(Male$Time2, "Time2"),
                              mysummary1(Male$Time3, "Time3"))
# Female summary: mean impulsivity increased from Time1 to Time3
Female.summary <- rbind(mysummary1(Female$Time1, "Time1"),
                              mysummary1(Female$Time2, "Time2"),
                              mysummary1(Female$Time3, "Time3"))

# EXPORTING
# Install Rio package
install.packages("writexl")

# exporting Impulsivity1.summary
library("writexl")
write_xlsx( Impulsivity1.summary, "~/Desktop/Impulsivity1.summary.xlsx")

# exporting Repodoxin.summary
library("writexl")
write_xlsx( Repodoxin.summary, "~/Desktop/Repodoxin.summary.xlsx")

# exporting Control.summary
library("writexl")
write_xlsx( Control.summary, "~/Desktop/Control.summary.xlsx")

# exporting Male.summary
library("writexl")
write_xlsx( Male.summary, "~/Desktop/Male.summary.xlsx")

# exporting Female.summary
library("writexl")
write_xlsx( Female.summary, "~/Desktop/Female.summary.xlsx")



































