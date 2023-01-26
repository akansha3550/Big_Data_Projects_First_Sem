#AKANSHA -
#STUDENT ID : A124058

#Project-1

sizeReport <- function(path, patt = ".*", dironly = FALSE, level = Inf) {
  # Create an empty data frame to store the report
  report <- data.frame(path = character(), size = numeric(), stringsAsFactors = FALSE)
  
  # Use the list.files() function to get a list of items in the directory
  items <- list.files(path, recursive = level, full.names = TRUE, pattern = patt)
  
  # Iterate through the items and check if they are a directory or a file
  for (item in items) {
    if (dironly == TRUE && !dir.exists(item)) {
      # Skip the item if it's not a directory and dironly is set to TRUE
      next
    }
    # Get the size of the item in bytes
    size <- file.info(item)$size
    # Add the item and its size to the report data frame
    report <- rbind(report, data.frame(path = item, size = size))
  }
  
  # Return the final report
  return(report)
}

report <- sizeReport(path = "C:/Users/akans/ASB_PYTHON" )
print(report)


options(width = 200)
report <- sizeReport(path = "C:/Users/akans/Advanced Analytics- Big data Course/Basic R Programming")
print(report)


#The level is restricted only to the zero level, the report contains only the initial directory.
sizeReport(path = "C:/Users/akans/Advanced Analytics- Big data Course/Data Mining", level= 0)


#The level is restricted only to the first level. Thus, the report contains only direct elements of the initial directory.
sizeReport(path = "C:/Users/akans/Advanced Analytics- Big data Course/Data Mining", level= 1)


#This report is identical to the first one because there are only two levels for the initial directory.
sizeReport(path = "C:/Users/akans/Advanced Analytics- Big data Course/Data Mining", level= 2)


#we restrict the report to directories only. The recursion level is not restricted, so all sub-directories are searched.
sizeReport(path = "C:/Users/akans/PycharmProjects", dironly = TRUE)


#we restrict listed items only to these ending with "png".
report <- sizeReport(path = "C:/Users/akans/Downloads", patt = "png$" )
print(report)

#The following example restricts listed items only to the ones containing one 
#of the strings pdf
report <- sizeReport(path = "C:/Users/akans/Advanced Analytics- Big data Course", patt = "pdf" )
print(report)


report <- sizeReport(path = "C:/Users/akans/Advanced Analytics- Big data Course", patt = "fig[1-4]")
print(report)


#The following example restricts listed items only to the ones containing 
#the string "Rplot" not followed by 1 through 4.

report <-sizeReport(path = "C:/Users/akans/Downloads", patt = "Rplot[^1-4]")
print(report)

#We list only directories on the first level of recursion containing the 
#string bank-additional-full.csv.
report <- sizeReport(path = "C:/Users/akans/Advanced Analytics- Big data Course/Credit scoring", patt = "software-20220930T155336Z-001", dironly = TRUE, level = 1)
print(report)

#here we restrict the path by giving dironly = FALSE 
report <- sizeReport(path = "C:/Users/akans/Advanced Analytics- Big data Course/Credit scoring", patt = "abt_app - good", dironly = FALSE, level = 1)
print(report)








