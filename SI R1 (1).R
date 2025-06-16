install.packages("ggplot2") 
install.packages("dplyr")
install.packages("ggthemes")
install.packages("readxl")
install.packages("writexl")
install.packages("tidylog")
install.packages("tidyverse")
install.packages("GGally")
install.packages("performance")
install.packages("latex2exp")
install.packages("gtsummary")
install.packages("tinytex")
install.packages("tableone")
install.packages("kableExtra")
install.packages("splitstackshape")
install.packages("stargazer")
install.packages("olsrr")
install.packages("tableone")
install.packages("latex2exp")
install.packages("patchwork")
install.packages("lmtest")
install.packages("gridExtra")
install.packages("reshape2")
install.packages("epiR")
install.packages("broom")
install.packages("corrplot")
install.packages("carData")
install.packages("car")
install.packages("gridExtra")
install.packages("stringr")
install.packages("arulesViz")
install.packages("knitr")
install.packages("coefplot")
install.packages("cowplot")
install.packages("egg")
install.packages("ggpubr")
install.packages("ggtext")
install.packages("stringi")
install.packages("stringdist")
install.packages("plotly")
install.packages("orca")
install.packages("kaleido")
install.packages("networkD3")
install.packages("googlesheets4")

#update versions of packages

update.packages("plotly")
update.packages("ggplot2")
update.packages()


R.version
# to get detailed information about my R session 
#including the version of R, the operating system, and the installed packages.
sessionInfo()

#clean the working environment
rm(list = ls())
?rm

library(ggplot2); library(dplyr); library(readxl); library (writexl); 
library(ggthemes); library(tidylog); library(tidyverse); library(GGally);
library(performance); library(tinytex); library(tableone); 
library(kableExtra) ; library(splitstackshape); 
library(stargazer) ; library(olsrr); library(tableone); library(latex2exp);
library(gtsummary); library(patchwork);library(ggridges); 
library(googlesheets4); library(lmtest); library(gridExtra); 
library(reshape2); library(epiR); library(broom); library(corrplot);
library(car); library(carData); library(gridExtra); library(stringr); 
library(arulesViz); library(knitr); library(coefplot);
library(cowplot); library(egg); library(ggpubr); library(ggtext);
library(stringi); library(stringdist) ; library(plotly); library(orca);
library(networkD3)

########################################################################
########################################################################

#########  Work with cities 200K and up  ########

#clean the working environment
rm(list = ls())
?rm


# Load necessary libraries
library(dplyr)
library(readxl)
library(writexl)

# Define the file paths
input_file <- "C:/Users/galia/Documents/GIS - CG/Cities - global view/Cities up to 200K - ready for R.xlsx"
output_file <- "C:/Users/galia/Documents/GIS - CG/Cities - global view/List of cities after R.xlsx"

# Load the data from Excel file
data <- read_excel(input_file)

# Step B: Keep only the row with the highest population for each source
data <- data %>%
  group_by(Source) %>%
  filter(Population == max(Population)) %>%
  ungroup()

# Step C: Identify rows where the city name contains non-English alphabet characters
data <- data %>%
  mutate(Country = ifelse(grepl("[^a-zA-Z ]", City), "Unknown", Country))

# Step D: Identify cities with the same name but different source, and mark "???" in the Country column
data <- data %>%
  group_by(City) %>%
  mutate(Country = ifelse(n() > 1 & n_distinct(Source) > 1, "???", Country)) %>%
  ungroup()

# Save the modified data to a new Excel file
write_xlsx(data, output_file)

# Print a message to confirm the process is done
cat("The updated file has been saved to", output_file)


###################################################################

######### Work with cities 100K and up ########

#clean the working environment
rm(list = ls())
?rm


# Load necessary libraries
library(dplyr)
library(readxl)
library(writexl)

# Define the file paths
input_file <- "C:/Users/galia/Documents/GIS - CG/Cities - global view/Cities 100K - rew data from wikidata.xlsx"
output_file <- "C:/Users/galia/Documents/GIS - CG/Cities - global view/Cities 100K - after R.xlsx"

# Load the data from Excel file
data <- read_excel(input_file)

# Step B: Keep only the row with the highest population for each source
data <- data %>%
  group_by(Source) %>%
  filter(population == max(population)) %>%
  ungroup()

# Step C: Keep only the row with the highest area for each source
data <- data %>%
  group_by(Source) %>%
  filter(area == max(area)) %>%
  ungroup()

# Step D: Identify rows where the city name contains non-English alphabet characters
data <- data %>%
  mutate(remarks = ifelse(grepl("[^a-zA-Z ]", cityLabel), "Unknown", remarks))

# Step E: Identify cities with the same name but different source, and mark "???" in the Country column
data <- data %>%
  group_by(cityLabel) %>%
  mutate(remarks = ifelse(n() > 1 & n_distinct(Source) > 1, "???", remarks)) %>%
  ungroup()

# Save the modified data to a new Excel file
write_xlsx(data, output_file)

# Print a message to confirm the process is done
cat("The updated file has been saved to", output_file)

