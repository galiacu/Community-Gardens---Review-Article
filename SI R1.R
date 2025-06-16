### should be install once with the current R

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



### Stacked area of -  number of publications per year in CGs and UA



rm(list = ls())

library(ggplot2)
library(dplyr)
library(readxl)
library(ggthemes)
library(writexl)

dat <- read_excel("C:/Users/galia/Documents/Files used in R/SI dataset3.xlsx")
x_axis_labels <- unique(dat$Year)

# Reorder levels of the "Subject" factor to have "UA" first and "CGs" second
dat$Subject <- factor(dat$Subject, levels = c("UA", "CGs"))

# Calculate total for each year
dat_total <- dat %>%
  group_by(Year) %>%
  summarise(Total = sum(Articles, na.rm = TRUE),
            Subject = "Total")  # Add a Subject column with value "Total" for each row

# Create a stacked area plot with UA at the bottom and CGs on top
g <- ggplot(dat, aes(x = Year, y = Articles, fill = Subject)) +
  geom_area(alpha = 0.7) +
  geom_line(data = dat_total, aes(x = Year, y = Total, linetype = "Total"), 
            color = "black", size = 0.8) +  # Add a black line for the total
  ylab('Articles') +
  theme_minimal() +
  theme(
    legend.position = c(.1, 1.0),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(4, 4, 4, 4),
    text = element_text(size = 14.00),
    legend.title = element_blank(),
    axis.text.x = element_text(size = 12, angle = 45, vjust = 0.99, hjust = 0.9),
    axis.text.y = element_text(size = 12)
  ) +
  scale_fill_manual(values = c("UA" = "#999999", "CGs" = "#009E73")) +
  scale_linetype_manual(values = c("Total" = "solid"), guide = guide_legend(title.position = "top")) +  # Set linetype for Total and adjust the legend position
  #scale_linetype_manual(values = c("Total" = "solid"), guide = "legend") +  # Set linetype for Total
  scale_x_continuous(expand = c(0, 0), labels = x_axis_labels[x_axis_labels %% 2 == 1], breaks = x_axis_labels[x_axis_labels %% 2 == 1]) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 250), breaks = seq(0, 250, 50)) + 
  guides(fill = guide_legend(override.aes = list(alpha = 1)))  # Override alpha in legend

print(g)




#######################################################################
#######################################################################





### lines plot of subjects of research by year in UA and CG ####


rm(list = ls())

library(dplyr); library(readxl); library(ggthemes);
library(writexl); library(ggplot2)


# Load the data 
dat_research = read_excel("C:/Users/galia/Documents/Files used in R/SI dataset4.xlsx")
names(dat_research) = c("keynote", "Count", "Year", "Field of Research", "Subject")

# Unit testing - Make sure there are not NA values
if(any(is.na(dat_research))) warning("There are NA values - do not preceed before fixing")

# UA

freq_table_UA = dat_research %>% filter(`Field of Research` == 'UA') %>% group_by(Subject, Year) %>% summarise(Frequency = sum(Count))
freq_table_UA_by_year = freq_table_UA %>% group_by(Year) %>%
  summarise(Total.Frequency.By.Year = sum(Frequency))
freq_table_UA = left_join(freq_table_UA, freq_table_UA_by_year, by = 'Year')
freq_table_UA = freq_table_UA %>% 
  mutate(Proportion = round(Frequency / Total.Frequency.By.Year *100, 1))

# Remove unwanted subjects
freq_table_UA = freq_table_UA %>%  filter(Subject %in% c('Economy', 'Food', 'Social',
                                                         'Environment', 'Health', 'Urbanism'))

# Save the UA data frame
library(writexl)
# write_xlsx(freq_table_UA, path = "D:/Users/galiacu/Google Drive/My Drive/PhD/Data bibiliometric research/Data for UA graph.xlsx")


# Create a plot for the desired subjects (UA)

x_axis_labels = unique(freq_table_UA$Year)
g_UA = ggplot(freq_table_UA, aes(x = Year, y = Proportion,
                                 colour = Subject)) + 
  # fill = Subject)) +
  geom_line(lwd = 1, alpha = 1.5) +
  geom_point(size = 1.5) +
  ggtitle('UA') +    # name of the panel - instead of b 
  ylab('Proportion (%)') +
  theme_minimal()  + 
  theme(legend.position = 'none',  # Remove legend
        text = element_text(size = 14),
        plot.title = element_text(size = 14, hjust = 0),
        axis.text.x = element_text(size = 12, angle = 45),
        legend.spacing.x = unit(0.5, 'cm'),
        legend.spacing.y = unit(-0.5, 'cm')) + 
  scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels, 
                     limits = c(2000, 2021), expand = c(0, 0)) +
  # the code expand = c(o,0) in the scale_x_continuous is to start the x-numbers closer to y-line
  labs(x = NULL) +  # Remove x-axis label  
  # scale_color_brewer(palette='Dark2') + 
  scale_colour_manual(values = c(Economy = '#5282d6',
                                 Food = '#a1d846',
                                 Social = '#B364D2',
                                 Environment = '#ffa624', 
                                 Health = '#c6361a', 
                                 Urbanism = '#009e73')) + 
  ylim(0, 50)

g_UA



#### CG

freq_table_CG = dat_research %>% filter(`Field of Research` == 'CG') %>% group_by(Subject, Year) %>% summarise(Frequency = sum(Count))
freq_table_CG_by_year = freq_table_CG %>% group_by(Year) %>%
  summarise(Total.Frequency.By.Year = sum(Frequency))
freq_table_CG = left_join(freq_table_CG, freq_table_CG_by_year, by = 'Year')
freq_table_CG = freq_table_CG %>% 
  mutate(Proportion = round(Frequency / Total.Frequency.By.Year *100, 1))

# Remove unwanted subjects
freq_table_CG = freq_table_CG %>%  filter(Subject %in% c('Economy', 'Food', 'Social',
                                                         'Environment', 'Health', 'Urbanism'))
# Save the CG data frame
library(writexl)
# write_xlsx(freq_table_CG, path = 'H:\\My Drive\\PhD\\Data bibiliometric research\\Data for CG graph.xlsx')


# Create a plot for the desired subjects (CGs)

x_axis_labels = unique(freq_table_CG$Year)
g_CG = ggplot(freq_table_CG, aes(x = Year, y = Proportion,
                                 colour = Subject)) + 
  # fill = Subject)) +
  geom_line(lwd = 1, alpha = 1.5) +
  geom_point(size = 1.5) +
  ggtitle('CGs') +  # name of the panel - instead of a 
  ylab('Proportion (%)') +
  theme_minimal()  + 
  theme(legend.text = element_text(size = 14),
        legend.position = 'bottom',
        legend.key.size = unit(3,"line"),
        legend.direction="horizontal", legend.title = element_blank(),
        text = element_text(size = 14),
        plot.title = element_text(size = 14, hjust = 0),
        axis.text.x = element_text(size = 12, angle = 45),
        legend.spacing.x = unit(0.5, 'cm'),
        legend.spacing.y = unit(-1, 'cm')) +
  
   
 # theme(legend.position = "bottom", 
 #     legend.justification = "center",
 #   legend.spacing.x = unit(0.1, 'cm'),  # Adjust the spacing between legend items
 #    legend.key.width = unit(1, "mm"),  # Adjust the width of the color boxes
 #  legend.key.height = unit(1, "mm")  # Adjust the height of the color boxes
 #  ) +
  
  guides(colour = guide_legend(byrow = TRUE)) +
  scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
  scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels, 
                     limits = c(2000, 2021), expand = c(0, 0)) +
  # expand = c(o,0) in the scale_x_continuous is the code to start the x-numbers closer to y-line
  scale_colour_manual(values = c(Economy = '#5282d6',
                                 Food = '#a1d846',
                                 Social = '#B364D2',
                                 Environment = '#ffa624', 
                                 Health = '#c6361a', 
                                 Urbanism = '#009e73')) + 
  ylim(0, 50) 

g_CG



### Combine the two plots with a legend below the second plot

combined_plot <- g_UA / g_CG +
  theme(legend.position = "none")  # Remove the legend from the first plot

# Create a separate legend for the second plot
legend_b <- get_legend(g_CG)

# Combine the plots and the legend with adjusted spacing
final_plot <- plot_grid(
  combined_plot,
  legend_b,
  ncol = 1,
  rel_heights = c(1, 0.1)  # Adjust the height of the legend
) +  
  theme(legend.position = "bottom", 
        legend.justification = "center",
        legend.spacing.x = unit(0.1, 'cm'),  # Adjust the spacing between legend items
        legend.key.width = unit(1, "mm"),  # Adjust the width of the color boxes
        legend.key.height = unit(1, "mm")  # Adjust the height of the color boxes
  ) +
  plot_annotation(title = NULL)  # Remove the title

# Display the combined plot
print(final_plot)




##########################################################################
##########################################################################     






#### Geographic locations - continents in colones and lines for countries



R.version
# to get detailed information about my R session 
#including the version of R, the operating system, and the installed packages.
sessionInfo()

#update versions of packages
update.packages()

#clean the working environment
rm(list = ls())
?rm

library(ggplot2); library(dplyr); library(readxl); library(ggthemes);
library(writexl); library(tidylog); library(tidyverse); library(GGally);
library(performance); library(tinytex); library(tableone); 
library(kableExtra) ; library(splitstackshape); library(stargazer) ;
library(stargazer) ; library(olsrr); library(tableone); library(latex2exp);
library(gtsummary); library(patchwork);library(ggridges); 
library(googlesheets4); library(lmtest); library(gridExtra); 
library(reshape2); library(epiR); library(broom); library(corrplot);
library(car); library(carData); library(gridExtra); library(stringr); 
library(arulesViz); library(knitr); library(coefplot);
library(cowplot); library(egg); library(ggpubr); library(ggtext);
library(stringi); library(stringdist) ; library(plotly); library(orca) ;
library(networkD3)




# Set the working directory to the folder containing the Excel files
setwd("C:/Users/galia/Documents/Files used in R")


# Read data from excel file
geo_dat <- read_excel("SI dataset5.xlsx")

geo_dat = geo_dat %>% arrange(Year)


### CG calculation

geo_dat_CG = geo_dat %>% filter(Domain == "CGs")
plot_dat_CG = geo_dat_CG %>% group_by(Continent, Domain) %>% summarise(N = sum(Times))
plot_dat_CG = plot_dat_CG %>% mutate(Percentage = N / sum(plot_dat_CG$N) *100)


### UA calculation

geo_dat_UA = geo_dat %>% filter(Domain == "UA")
plot_dat_UA = geo_dat_UA %>% group_by(Continent, Domain) %>% summarise(N = sum(Times))
plot_dat_UA = plot_dat_UA %>% mutate(Percentage = N / sum(plot_dat_UA$N) *100)


### CG + UA calculation

plot_dat = rbind(plot_dat_CG, plot_dat_UA)
plot_dat = plot_dat %>% arrange(Continent)


# Need to sum plot_dat$N by Domain (CG/UA) and continent

plot_dat$Continent = c("Africa \n [S.Africa]",  "Africa \n [S.Africa]", 
                       "Americas \n [USA]", "Americas \n [USA]", 
                       "Asia \n [China]", "Asia \n [China]",
                       "Australia", "Australia",
                       "Europe", "Europe")


#### CG

seg_dat_CG = geo_dat %>% filter(Domain == "CGs") %>%
  group_by(`Continent + extra`) %>% summarise(N = sum(Times))
seg_dat_CG = seg_dat_CG %>% mutate(Percentage = N / sum(seg_dat_CG$N) *100)


#### UA

seg_dat_UA = geo_dat %>% filter(Domain == "UA") %>%
  group_by(`Continent + extra`) %>% summarise(N = sum(Times))
seg_dat_UA = seg_dat_UA %>% mutate(Percentage = N / sum(seg_dat_UA$N) *100)



ggplot(data = plot_dat, aes(x = Continent, y = Percentage, fill = Domain)) + 
  ylim(0, 60) +
  ylab("Percentage (%)") + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_segment(x = 0.55,
               y = seg_dat_CG$Percentage[which(seg_dat_CG$`Continent + extra` == "South Africa")],
               xend = 1,
               yend = seg_dat_CG$Percentage[which(seg_dat_CG$`Continent + extra` == "South Africa")]) +
  geom_segment(x = 1,
               y = seg_dat_UA$Percentage[which(seg_dat_UA$`Continent + extra` == "South Africa")],
               xend = 1.45,
               yend = seg_dat_UA$Percentage[which(seg_dat_UA$`Continent + extra` == "South Africa")]) +
  geom_segment(x = 1.55,
               y = seg_dat_CG$Percentage[which(seg_dat_CG$`Continent + extra` == "USA")],
               xend = 2,
               yend = seg_dat_CG$Percentage[which(seg_dat_CG$`Continent + extra` == "USA")]) + 
  geom_segment(x = 2,
               y = seg_dat_UA$Percentage[which(seg_dat_UA$`Continent + extra` == "USA")],
               xend = 2.45,
               yend = seg_dat_UA$Percentage[which(seg_dat_UA$`Continent + extra` == "USA")]) +
  geom_segment(x = 2.55,
               y = seg_dat_CG$Percentage[which(seg_dat_CG$`Continent + extra` == "China")],
               xend = 3,
               yend = seg_dat_CG$Percentage[which(seg_dat_CG$`Continent + extra` == "China")]) + 
  geom_segment(x = 3,
               y = seg_dat_UA$Percentage[which(seg_dat_UA$`Continent + extra` == "China")],
               xend = 3.45,
               yend = seg_dat_UA$Percentage[which(seg_dat_UA$`Continent + extra` == "China")]) + 
  geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25, size = 4) +
  theme_minimal()  + theme(text = element_text(size = 14)) + 
  
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) + 
  
  theme(legend.title = element_blank()) + 
  # Reverse the order of the legend
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_cartesian(ylim = c(2.7, 60)) + 
  scale_fill_manual("legend", values = c("CGs" = "#009E73", "UA" = "#999999")) 









#########################################################################
#########################################################################


R.version
# to get detailed information about my R session 
#including the version of R, the operating system, and the installed packages.
sessionInfo()

#clean the working environment
rm(list = ls())
?rm

library(ggplot2); library(dplyr); library(readxl); library(ggthemes);
library(writexl); library(tidylog); library(tidyverse); library(GGally);
library(performance); library(tinytex); library(tableone); library(kableExtra) ;
library(splitstackshape); library(stargazer) ; library(stargazer) ; library(olsrr); library(tableone); library(latex2exp); library(gtsummary); library(patchwork);
library(lmtest); library(gridExtra); library(reshape2); library(epiR); library(broom); library(corrplot);
library(car); library(carData); library(gridExtra); library(stringr); library(arulesViz); library(knitr); library(coefplot);
library(cowplot); library(egg); library(ggpubr); library(ggtext); library(ggplot2) ;
library(stringi); library(stringdist) ; library(plotly); library(orca) ;library(networkD3);library(dplyr)




#########################################################################################
##############################################################################


####### Geographic over time #######



# Set the working directory to the folder containing the Excel files
setwd("C:/Users/galia/Documents/Files used in R")

library(readxl)

# Read data from excel file
CG_UA_data <- read_excel("SI dataset6.xlsx")
colnames(CG_UA_data)
str(CG_UA_data)



### Original colors from Galia chart line
original_colors <- c("#999999", "#5282d6", "#009e73","#fa7346","#c6361a")


### Define color palette = Change bar colors
# stacked bar chart color - option 1
colors_1 <- c( "#A3E4D7", "#76D7C4", "#5DADE2")

# stacked bar chart color - option 2
colors_2 <- c( "#6afff6", "#1fe4d8", "#1cbab0")

# stacked bar chart color - option 3
colors_3 <- c( "#F7F708", "#FABD0B", "#F70F08")

# stacked bar chart color - option 4
colors_4 <- c( "#d9cdfa", "#a18cd9", "#665399")


########################################################################


### UA data

# Split data by subject - the UA data
ua_data <- CG_UA_data %>% filter(subject == "UA")

# Reshape the data
ua_data_long <- ua_data %>%
  gather(key = "period", value = "count", -continent, -subject)

# Define custom order for the continents and reverse it
custom_order <- rev(c("Africa", "Americas", "Asia", "Australia", "Europe"))

# Apply custom order to the continent variable
ua_data_long$continent <- factor(ua_data_long$continent, levels = custom_order)

# Reorder the levels of the "period" variable so that "1992-2001" comes first
ua_data_long$period <- factor(ua_data_long$period, levels = rev(c("1992-2001", "2002-2011", "2012-2021")))

# Set common x-axis limits
common_x_limits <- c(0, 350)

# Define width and gap for the bars
bar_width <- 0.9  # Adjust this value as needed


### UA figure

# Create horizontal stacked bar chart with labels for ua_data
ua_data_plot <- ggplot(data = ua_data_long, aes(x = count, y = continent, fill = period, label = count)) +
  geom_bar(stat = "identity", color = NA, position = position_stack(vjust = 0), width = bar_width) +
  geom_text(data = subset(ua_data_long, count > 0.5),  # Filter out rows where count is 0
            position = position_stack(vjust = 0.65),
            color = "black", size = 3) +
  # Customize labels and title
  labs(
    title = "UA",
    x = "",
    y = "",
    fill = ""  #legend title
  ) +
  # Customize theme
  theme_minimal() +
  # Change bar colors
  scale_fill_manual(values = colors_3) +
  # Set x-axis units and limits
  scale_x_continuous(breaks = seq(0, 350, by = 50), 
                     limits = common_x_limits , expand = c(0.01, 0))+
  # Set y-axis limits
  scale_y_discrete(limits = custom_order) +
  # Set the size of title and axis labels
  theme(
    plot.title = element_text(size = 10), #face = "bold"),  # "plain"/"italic" / "bold.italic"
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    axis.text = element_text(size = 10, color = "#494b4b"),  # Adjust the color of the axis labels
    legend.text = element_text(size = 10, color = "#494b4b"),
    legend.position = "bottom" 
  ) +
  # Reverse the order of the legend
  guides(fill = guide_legend(reverse = TRUE)) +
  # Suppress the legend
  guides(fill = FALSE)

print (ua_data_plot)


#########################################################################

### CG_data

# Split data by subject - the CG data
cg_data <- CG_UA_data %>% filter(subject == "CG")

# Reshape the data
cg_data_long <- cg_data %>%
  gather(key = "period", value = "count", -continent, -subject)

# Define custom order for the continents and reverse it
custom_order <- rev(c("Africa", "Americas", "Asia", "Australia", "Europe"))

# Apply custom order to the continent variable
cg_data_long$continent <- factor(cg_data_long$continent, levels = custom_order)

# Reorder the levels of the "period" variable so that "1992-2001" comes first
cg_data_long$period <- factor(cg_data_long$period, levels = rev(c("1992-2001", "2002-2011", "2012-2021")))

# Set common x-axis limits
common_x_limits <- c(0, 350)

# Define width and gap for the bars
bar_width <- 0.9  # Adjust this value as needed



### CG figure

# Create horizontal stacked bar chart with labels for CG_data
cg_data_plot <- ggplot(data = cg_data_long, aes(x = count, y = continent, fill = period, label = count)) +
  geom_bar(stat = "identity", color = NA, position = "stack", width = bar_width) +
  geom_text(data = subset(cg_data_long, count > 0.5),  
            position = position_stack(vjust = 0.65),
            color = "black", size = 3) +
  labs(
    title = "CGs",
    x = "References",
    y = "",
    fill = ""
  ) +
  theme_minimal() +
  scale_fill_manual(values = colors_3) +
  
  # Set x-axis units and limits
  scale_x_continuous(breaks = seq(0, 350, by = 50), limits = c(0, 350), 
                     expand = c(0.01, 0)) +  # Adjust the expand parameter
  # Set y-axis limits
  scale_y_discrete(limits = custom_order) +
  
  # Set the size of title and axis labels
  theme(
    plot.title = element_text(size = 10), #face = "bold"),
    axis.title.x = element_text(size = 12, hjust = 0.5),# Adjust "References" for the middle side
    axis.title.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    axis.text = element_text(size = 10, color = "#494b4b"),
    legend.text = element_text(size = 10, color = "#494b4b"),
    legend.box = "horizontal",
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.margin = margin(t = 0, r = 0, b = 0, l = 5)
  ) +
  # Reverse the order of the legend
  guides(fill = guide_legend(reverse = TRUE))

# Suppress the legend
# guides(fill = FALSE)


print (cg_data_plot)


#####################################################################


### Combine the two plots with a legend below the second plot

combined_plot <- ua_data_plot / cg_data_plot +
  theme(legend.position = "none")  # Remove the legend from the first plot

# Create a separate legend for the second plot
legend_b <- get_legend(cg_data_plot)

# Combine the plots and the legend
final_plot <- plot_grid(
  combined_plot,
  legend_b,
  ncol = 1,
  rel_heights = c(1, 0.1)  # Adjust the height of the legend
) +  
  theme(legend.position = "bottom", 
        legend.justification = "center",
        legend.key.width = unit(3, "mm"),  # Adjust the width of the color boxes
        legend.key.height = unit(3, "mm"),  # Adjust the height of the color boxes
  ) +
  plot_annotation(title = NULL)  # Remove the title

# Display the combined plot

print(final_plot)




####################################################################
####################################################################
