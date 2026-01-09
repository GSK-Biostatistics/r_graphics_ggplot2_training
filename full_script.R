



## 
## # For systems using Posit Package Manager
## install.packages("gskRtraining")
## library("gskRtraining")
## setup_training()



library(tidyverse)
library(mapproj)
theoph <- read_csv("data/theoph.csv")

# tidyverse loads the dplyr package for filter and select (and the pipe)
library(tidyverse) 
# Start with the theoph data
head(theoph)

# Method 1: nesting (this is horrible)
bl_conc <- select(filter(theoph, TIME == 0), SUBJID, CONC)

# Method 2: traditional stepwise approach
# (better, but leaves us with an object, bl_rows, that we may not want)
bl_rows <- filter(theoph, TIME == 0)
bl_conc <- select(bl_rows, SUBJID, CONC)

# Now let's look at the data
head(bl_conc)

bl_conc <- theoph %>%      # Take the theoph data, then ...
  filter(TIME == 0) %>%   # ... filter the rows, then ... 
  select(SUBJID, CONC)    # ... select columns

# Now let's look at the data
bl_conc %>% head



library(tidyverse); theme_set(theme_bw(base_size = 12))
library(knitr)
pk <- tibble(SUBJID = as.character(rep(1:2, each = 5)),
             TIME = rep(c(0, 1, 6, 12, 24), 2), 
             CONC = c(0, 10, 7, 6, 3, 0, 8, 6, 3, 1))

## pk <- tibble(SUBJID = as.character(rep(1:2, each = 5)),

##              TIME = rep(c(0, 1, 6, 12, 24), 2),

##              CONC = c(0, 10, 7, 6, 3, 0, 8, 6, 3, 1))




library(ggplot2) # Or `library(tidyverse)` for full tidyverse functionality
ggplot() # Note that the function is named "ggplot" and not "ggplot2"

library(tidyverse)
pk <- tibble(SUBJID = as.character(rep(1:2, each = 5)),
             TIME = rep(c(0, 1, 6, 12, 24), 2), 
             CONC = c(0, 10, 7, 6, 3, 0, 8, 6, 3, 1))

## ggplot(data = pk,
##        aes(x = TIME, y = CONC))

# Extract subject 1's data
subj1 <- pk %>% filter(SUBJID == 1)

# Plot subject 1's concentrations over time
ggplot(data = subj1,
       aes(x = TIME, y = CONC)) +
  geom_point()

# A line plot
ggplot(data = subj1,
       aes(x = TIME, y = CONC)) +
  geom_line()

ggplot(data = subj1,
       aes(x = TIME, y = CONC)) +
  geom_line() +
  ggtitle("Concentration over Time for Subject 1",
          subtitle = "Using Simulated Data")

## # Exercise Answers: Layering
## 
## # 1
## eff_fev <- efficacy %>%
##   filter(VISITNUM %in% c(20,40,60))
## ggplot(data = eff_fev,
##        aes(x = VISITNUM, y = FEV1)) +
##   geom_point()
## # 1a
## ggplot(data = eff_fev,
##        aes(x = VISITNUM, y = FEV1, colour = ARM)) +
##   geom_point()
## # 2
## ggplot(data = eff_fev,
##        aes(x = VISITNUM, y = FEV1, colour = USUBJID)) +
##   geom_line()
## # 2a
## ggplot(data = eff_fev,
##        aes(x = VISITNUM, y = FEV1, colour = USUBJID)) +
##   geom_line() +
##   ggtitle("FEV1 values over time")
## # 3
## week12 <- efficacy %>%
##     filter(VISITNUM == 40)
## ggplot(data = week12,
##        aes(y = ACTTOT, x = ACTBL)) +
##     geom_point() +
##     ggtitle("ACT Total Score at Week 12 against Baseline ACT Total Score")
## # 3a
## ggplot(data = week12,
##        aes(y = ACTTOT, x = ACTBL)) +
##     geom_jitter() +
##     ggtitle("ACT Total Score at Week 12 against Baseline ACT Total Score")
## 

# Create a plot called my_plot - nothing is printed at this point!
my_plot <- ggplot(data = pk,
                  aes(x = TIME, y = CONC, colour = SUBJID)) +
  geom_line()

# Print/plot the graph
my_plot # equivalent to `print(my_plot)`

my_plot + geom_point(size = 4, shape = 'x')

# Note the `my_plot <- ` at the start
my_plot <- my_plot + geom_point(size = 4, shape = 'x')



library(ggplot2)
library(tidyverse)
library(haven)
theoph <- read_csv("data/theoph.csv")
dm <- read_sas( "data/dm.sas7bdat")
vs <- read_sas("data/vs.sas7bdat")

ggplot(data = theoph,
       aes(x = TIME, y = CONC, colour = factor(SUBJID))) +
  geom_line()



ggplot(data = quakes,
       aes(x = long, y = lat, alpha = -depth, colour = stations, size = mag)) +
  geom_point()



## # Create a plot
## bar_plot <- ggplot(data = dm,
##                    aes(x = ARM, fill = SEX)) +
##   geom_bar(position = "dodge") # As opposed to the default, "stack"
## # Without applying a scale
## bar_plot
## 
## # Apply a scale to change the default colours
## bar_plot +
##   scale_fill_discrete("Sex") +
##   scale_x_discrete("Treatment") +
##   scale_y_continuous("Number of Subjects",
##                      breaks= seq(0, 14, by = 2))



## # Apply a manual colour scale
## bar_plot +
##   scale_fill_manual("Sex", values = c("navy", "darkgreen")) +
##   scale_x_discrete("Treatment") +
##   scale_y_continuous("Number of Subjects", breaks = 0:8)

ggplot(data = vs,
       aes(x = WEIGHT)) +
  geom_histogram(binwidth = 10, fill = "tan")

# First 20 colours
colours()[1:20]

# Specify red, green and blue as a proportion
my_col <- rgb(0.1, 0.9, 0.7)

# Specify red, green and blue on a standard 0 to 255 range
my_col <- rgb(26, 230, 179, maxColorValue=255)

# Draw a histogram
ggplot(data = vs,
       aes(x = WEIGHT)) +
  geom_histogram(binwidth = 10, fill = my_col)





## # Exercise Answers: Aesthetics
## # 1
## act_W24 <- act_full %>%
##   filter(VISITNUM == 60)
## # 1a
## ggplot(data = act_W24,
##        aes(x = ACTBL, y = ACTCHGBL, colour = ARM)) +
##   geom_point()
## # 1b
## ggplot(data = act_W24,
##        aes(x = ACTBL, y = ACTCHGBL, colour = ARM,
##                                  shape = factor(ACTRESP))) +
##   geom_point()
## # 2
## ggplot(data = dm,
##        aes(x = AGE)) +
##   geom_histogram(binwidth = 5, fill = "hotpink")
## # 3
## ggplot(data = act_W24,
##        aes(x = ACTBL, y = ACTCHGBL, colour = ARM)) +
##   geom_point(size = 2)
## # 4
## ggplot(data = act_W24,
##        aes(x = ACTBL, y = ACTCHGBL, colour = ARM)) +
##   geom_point() +
##   scale_colour_manual("Treatment", values = c("orange", "navy"))
## # 5
## base_Q4 <- ggplot(data = theoph,
##                   aes(x = TIME, y = CONC, colour = SUBJID)) +
##   geom_line()
## # 5a
## base_Q4 +
##   scale_x_continuous(limits = c(4, NA))
## # 5b
## base_Q4 +
##   scale_x_continuous(limits = c(4, NA))  +
##   scale_y_continuous(trans = "log", breaks = 1:10)
## # 6
## visit_labels <- c("Screening", "Baseline", paste("Week", seq(6, 24, by = 6)), "EW")
## base_Q5 <- ggplot(data = act_full, aes(x = factor(VISITNUM, labels = visit_labels), fill = ARM)) +
##   geom_bar(position = "dodge")
## # 6a
## base_Q5 +
##   scale_y_continuous("Number of Subjects")
## # 6b
## base_Q5 +
##   scale_y_continuous("Number of Subjects") +
##   scale_x_discrete("Visit", limits = visit_labels[-c(1, length(visit_labels))])
## # 6c
## base_Q5 +
##   scale_y_continuous("Number of Subjects") +
##   scale_x_discrete("Visit", limits = visit_labels[-c(1, length(visit_labels))]) +
##   scale_fill_discrete("Treatment")
## 

## # No grouping
## ggplot(data = theoph,
##        aes(x = TIME, y = CONC)) +
##   geom_line() +
##   ggtitle("No Grouping")
## 
## 
## # Grouping
## ggplot(data = theoph,
##        aes(x = TIME, y = CONC, group = SUBJID)) +
##   geom_line() +
##   ggtitle("Grouping")
## 



## # Get some data (map of USA with state boundaries) from the map package
## library(maps)
## states <- map_data("state")
## head(states)
## 
## # Plot the data - this data has a "group" variable which enables the
## # addition of state boundaries.
## ggplot(states,
##        aes(long, lat, group = group, fill = region)) +
##   geom_polygon(colour = "black")



## # Exercise Answers: Aesthetics
## 
## # 1
## ggplot(data = act_full,
##        aes(x = VISITNUM, y = ACTTOT, group = USUBJID, colour = ARM))+
##   geom_line(alpha = .4, size = 1.5)
## # 2
## # Get some data (map of USA with state boundaries) from the map package
## library(maps)
## states <- map_data("state")
## #regions <- unique(states$region)
## centres = c("new york", "new jersey", "north carolina", "florida", "california")
## states_which <- states %>%
##   mutate(recruit = if_else(region %in% centres, TRUE, FALSE))
## 
## # Plot the data - this data has a "group" variable which enables the addition
## # of state boundaries.
## ggplot(states_which,
##        aes(long, lat, group = group)) +
##   geom_polygon(aes(fill = recruit), colour = "black") +
##   scale_fill_manual("Centres", values = c("white", "darkgreen"), guide = "none")



library(ggplot2)
library(tidyverse)
library(haven)

dm <- read_sas( "data/dm.sas7bdat")
act_full <- read_sas("data/actFull.sas7bdat")
vs <- read_sas("data/vs.sas7bdat")
pk <- tibble(SUBJID = as.character(rep(1:2, each = 5)),
             TIME = rep(c(0, 1, 6, 12, 24), 2), 
             CONC = c(0, 10, 7, 6, 3, 0, 8, 6, 3, 1))



ggplot(data = dm,
       aes(x = AGE)) +
  geom_histogram(bins = 6, fill = "orange", alpha = .5, colour = "black")

ggplot(data = dm,
       aes(x = AGE)) +
  geom_density(fill = "blue", alpha = .5)

# Count subjects "manually"
arm_count <- dm %>%
  group_by(ARM) %>%
  summarise(`Number of Subjects` = length(USUBJID))
ggplot(data = arm_count,
       aes(x = ARM, y = `Number of Subjects`)) +
  geom_bar(stat = "identity", fill = "gold")

## base_plot <- ggplot(data = dm,
##                     aes(x = ARM, fill = SEX))
## 
## base_plot + geom_bar(position = "dodge", width = .4)
## base_plot + geom_bar(position = "stack", width = .4)
## base_plot + geom_bar(position = "fill", width = .4)

base_plot <- ggplot(data = dm,
                    aes(x = ARM, fill = SEX)) +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom")
  
p1 <- base_plot + geom_bar(position = "dodge") + ggtitle("Dodge")
p2 <- base_plot + geom_bar(position = "stack") + ggtitle("Stack")
p3 <- base_plot + geom_bar(position = "fill") + ggtitle("Fill")

gridExtra::grid.arrange(p1, p2, p3, ncol = 3)

base_plot + geom_bar(position = position_dodge(width = .5), width = .4)

# Subset to planned visits
act_full_planned <- act_full %>% filter(20 <= VISITNUM, VISITNUM <= 60)

ggplot(data = act_full_planned,
       aes(x = VISITNUM, y = ACTTOT)) +
  geom_boxplot()

# Create some nicer visit labels
visit_labels <- c("Baseline", paste("Week", seq(6, 24, 6)))

# An on-the-fly fix
ggplot(data = act_full_planned, 
       aes(x = factor(VISITNUM, labels = visit_labels), y = ACTTOT)) +
  geom_boxplot()

# Create some nicer visit labels
visit_labels <- c("Baseline", paste("Week", seq(6, 24, 6)))

# Fill by treatment
ggplot(data = act_full_planned, 
       aes(x = factor(VISITNUM, labels = visit_labels), 
           y = ACTTOT, fill = ARM)) +
  geom_boxplot()

## random_data <- tibble(x = c(4,6,2,4,2,4,1),
##                      y = c(7,2,1,6,3,8,5))
## base_plot <- ggplot(data = random_data,
##                     aes(x = x, y = y))
## 
## # Path
## base_plot + geom_path() +
##   ggtitle("Path Plot of \"Random\" Data")
## 
## # Line
## base_plot + geom_line() +
##   ggtitle("Line Plot of \"Random\" Data")



# Import the lung data from the survival package
library(survival)
library(broom)
head(lung)

# Create a KM fit
cox_mod <- survfit(Surv(time, status) ~ ph.ecog , data=lung)
km_lung <- tidy(cox_mod)

# Plot the data
ggplot(data = km_lung, 
       aes(x = time, y = estimate, colour = strata)) +
  geom_step() +
  # Censoring - more on multiple layers coming up
  geom_point(aes(shape = factor(n.censor)), size = 4) +
  # Mark deaths with a + but don't add to the legend
  scale_shape_manual(values = c("", "+"), guide = 'none')

## # Exercise Answers: Graph Types: Geoms
## 
## # 1
## ggplot(data = dm,
##        aes(x = AGE)) +
##   geom_density()
## #1a
## ggplot(data = dm,
##        aes(x = AGE, fill = ARM)) +
##   geom_density()
## #1b
## ggplot(data = dm,
##        aes(x = AGE, fill = ARM)) +
##   geom_density(alpha = .5)
## #2
## act_W24 <- act_full %>%
##   filter(VISITNUM == 60)
## 
## ggplot(data = act_W24,
##        aes(x = SEX, y = ACTCHGBL)) +
##   geom_boxplot()
## # 2a
## ggplot(data = act_W24,
##        aes(x = SEX, y = ACTCHGBL, fill = ARM)) +
##   geom_boxplot()
## # 2b
## ggplot(data = act_W24,
##        aes(x = SEX, y = ACTCHGBL, fill = ARM)) +
##   geom_boxplot(outlier.shape=1)
## # 3
## ggplot(data = act_W24,
##        aes(x = SEX, y = ACTCHGBL, fill = ARM)) +
##   geom_violin()
## # 4
## ggplot(data = filter(act_full, VISITNUM <= 60),
##        aes(x = VISITNUM, fill = ARM)) +
##   geom_bar(position = "dodge")
## # 5
## ggplot(data = act_W24,
##        aes(x = SEX, y = ACTCHGBL, fill = ARM)) +
##   geom_boxplot(position = position_dodge(width = .5), width = .4)

ggplot(data = pk,
       aes(x = TIME, y = CONC, group = SUBJID)) +
  geom_line() +
  geom_point(colour = "red")

## ggplot(data = pk,
##        aes(x = TIME, y = CONC, group = SUBJID)) +
##   geom_line() +
##   geom_point(colour = "red")

## ggplot(data = pk,
##        aes(x = TIME, y = CONC)) +
##   geom_line(aes(group = SUBJID)) +
##   geom_point(colour = "red")

# Find the average at each time of our two pk subjects
pk_ave <- pk %>%
  group_by(TIME) %>%
  summarise(CONC = median(CONC))

# Plot the original data and the average
ggplot() +
  geom_point(data = pk,
             aes(x = TIME, y = CONC)) +
  geom_line(data = pk_ave,
            aes(x = TIME, y = CONC), colour = "red") 


# Summarise the ACT data
act_post_bl_summary <- act_full %>%
  # Post BL data
  filter(30 <= VISITNUM, VISITNUM <= 60) %>%
  # Mean and standard errors for each visit
  group_by(VISITNUM) %>%
  summarise(Mean = mean(ACTCHGBL),
            N = length(USUBJID),
            SE = sd(ACTCHGBL) / sqrt(N),
            LowerSE = Mean - SE,
            UpperSE = Mean + SE)

# Now the plotting bit
errorbar_eg <- ggplot(data = act_post_bl_summary, 
                      aes(x = VISITNUM, ymin = LowerSE, ymax = UpperSE)) +
  geom_errorbar(width = 0.8) # NOTE the 'width' argument
errorbar_eg

# Add lines to our previous example
errorbar_eg +
  geom_line(aes(y = Mean)) +
  geom_point(aes(y = Mean), colour = "orange", size = 2) +
  scale_y_continuous("Mean (+/- SE)")

# Add a bar chart underneath the error bars 
# Note: this puts bars on top of the error bars
#       in practice swap the order
errorbar_eg +
  geom_bar(stat = "identity", aes(y = Mean), alpha = .5, fill = "maroon") +
  scale_y_continuous("Mean (+/- SE)")

ribbon_plot <- ggplot(data = act_post_bl_summary, 
       aes(x = VISITNUM, ymin = LowerSE, ymax = UpperSE)) +
  geom_ribbon(fill = "lightblue", alpha = .2) +
  geom_line(aes(y = Mean)) +
  geom_point(aes(y = Mean), colour = "orange", size = 2) +
  scale_y_continuous("Mean (+/- SE)")

ribbon_plot

# Add a red, dotted reference line to the ribbon plot
ribbon_plot +
  geom_hline(yintercept = 3, linetype = 3, colour = "red", size = 1.5)

# Create a scatter plot of weight against height.
scat <- ggplot(data = vs,
               aes(x = HEIGHT, y = WEIGHT)) +
  geom_point()

# Use a linear model to get a best-fit line
a_model <- lm(data = vs, WEIGHT ~ HEIGHT)
int <- coef(a_model)["(Intercept)"]
slope <- coef(a_model)["HEIGHT"]

# Add a reference line
scat + 
  geom_abline(intercept=int, slope = slope, colour = "darkgreen", 
              size = 1.5)

# Create a scatter plot of weight against height.
ggplot(data = vs,
       aes(x = HEIGHT, y = WEIGHT)) +
  geom_point() +
  geom_smooth(colour = "hotpink3", size = 1.5)

ribbon_plot +
  geom_hline(yintercept = 3, linetype = 3, colour = "red") +
  geom_text(x = 60, y = 3, colour = "red",
            label = "Clinically\nmeaningful\ndifference",
            hjust = 1, vjust = 1)

ggplot(data = pk,
       aes(x = TIME, y = CONC, group = SUBJID)) +
  geom_line(alpha = .4) +
  geom_point() +
  geom_text(aes(label = SUBJID), nudge_x = .5, nudge_y = .5)

## # Exercise Answers: Graph Types: Geoms 2
## 
## # 1
## # Summarise the ACT data
## act_post_bl_summary <- act_full %>%
##   # Post BL data
##   filter(30 <= VISITNUM, VISITNUM <= 60) %>%
##   # Mean and standard errors for each visit
##   group_by(ARM, VISITNUM) %>%
##   summarise(Mean = mean(ACTCHGBL),
##             N = length(USUBJID),
##             SE = sd(ACTCHGBL) / sqrt(N),
##             LowerSE = Mean - SE,
##             UpperSE = Mean + SE)
## ggplot(data = act_post_bl_summary,
##        aes(x = VISITNUM, ymin = LowerSE, ymax = UpperSE, colour = ARM)) +
##   geom_errorbar(width = 0.8) # NOTE the 'width' argument
## # 1a
## ggplot(data = act_post_bl_summary,
##        aes(x = VISITNUM, ymin = LowerSE, ymax = UpperSE, y = Mean,
##            colour = ARM)) +
##   geom_errorbar(width = 1, position = position_dodge(width = .8)) +
##   geom_point(position = position_dodge(width = .8)) +
##   geom_line(position = position_dodge(width = .8))
## # 2
## week24 <- act_full %>%
##   filter(VISITNUM == 60) %>%
##   group_by(SEX) %>%
##   # Function to identify outliers based on the boxplot methodology for ggplot2
##   mutate(isOutlier = ACTCHGBL < quantile(ACTCHGBL, 0.25) - 1.5 * IQR(ACTCHGBL) |
##            ACTCHGBL > quantile(ACTCHGBL, 0.75) + 1.5 * IQR(ACTCHGBL))
## 
## ggplot(data = week24,
##        aes(x = SEX, y = ACTCHGBL)) +
##   geom_boxplot(outlier.shape=NA) +
##   geom_text(data = filter(week24, isOutlier), aes(label = USUBJID, colour = isOutlier)) +
##   scale_colour_manual(values ="black", guide = "none")
## 
## # 3
## act_mod <- lm(data = act_full, ACTCHGBL ~ ARM + AGE + SEX + VISIT)
## resid_data <- augment(act_mod)
## 
## ggplot(data = resid_data) +
##   geom_qq(aes(sample = .resid))



library(ggplot2)
library(tidyverse)
library(haven)

dm <- read_sas( "data/dm.sas7bdat")
theoph <- read_csv("data/theoph.csv")
act_full <- read_sas("data/actFull.sas7bdat")

ggplot(data = dm,
       aes(x = AGE)) +
  geom_histogram(binwidth = 10, fill = "lightblue") +
  facet_grid(. ~ SEX)

ggplot(data = dm,
       aes(x = AGE)) +
  geom_histogram(binwidth = 5, fill = "lightblue") +
  facet_grid(ARM ~ SEX)

ggplot(data = dm,
       aes(x = AGE)) +
  geom_histogram(binwidth = 5, fill = "lightblue") +
  facet_grid(ARM + SEX ~ .)

ggplot(data = theoph,
       aes(x = TIME, y = CONC, group = SUBJID)) +
  geom_line() +
  facet_wrap(~ DOSE)

ggplot(data = theoph,
       aes(x = TIME, y = CONC, group = SUBJID)) +
  geom_line() +
  facet_wrap(~ DOSE, labeller = label_both)

## # Exercise Answers: Panelling
## 
## # 1a
## profiles <- ggplot(data = act_full,
##                    aes(x = VISITNUM, y = ACTTOT, group = USUBJID,
##                        colour = ARM)) +
##   geom_path(alpha = .5) +
##   geom_point()
## # 1b
## profiles +
##   facet_grid(. ~ ACTRSP24)
## # 2
## ggplot(data = act_full,
##        aes(x = ACTTOT)) +
##   geom_histogram(bins = 6) +
##   facet_grid(. ~ VISITNUM)
## # 3
## act_full %>%
##   filter(VISITNUM == 60) %>%
## ggplot(aes(x = ACTRSP24, fill = ARM)) +
##   geom_bar() +
##   facet_grid(SMOKSTAT ~ SEX)
## 



library(ggplot2)
library(tidyverse)

pk <- tibble(SUBJID = as.character(rep(1:2, each = 5)),
             TIME = rep(c(0, 1, 6, 12, 24), 2), 
             CONC = c(0, 10, 7, 6, 3, 0, 8, 6, 3, 1))
theoph <- read_csv("data/theoph.csv")


args(theme) 

# Create a plot
pk_plot <- ggplot(data = pk,
                  aes(x = TIME, y = CONC)) +
  facet_wrap( ~ SUBJID) +
  geom_line() +
  geom_point()

# Draw the plot but modify some theme elements
pk_plot +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.text = element_text(colour = "white"),
        strip.background = element_rect(fill = "black"))

# Set the global theme using a theme in ggthemes
theme_set(ggthemes::theme_excel(base_size = 12))

# Draw the plot (in this case one that we created earlier)
pk_plot

# Update the theme
theme_update(panel.background = element_rect(fill = "orange"))

# Re-draw the plot again
pk_plot

# Set the theme back to something a bit nicer
theme_set(theme_bw(base_size = 12))

ggplot(data = theoph,
       aes(x = TIME, y = CONC, 
                          colour = DOSE, group = SUBJID)) + 
  geom_line() +
  geom_point() +
  theme(legend.position = "bottom") +
  guides(colour = 
           guide_legend(title = "Dose", title.position = "left", nrow = 2, 
                        direction = "vertical",
                        title.theme = element_text(face = "bold", angle = 0)))



library(ggplot2)
library(tidyverse)
library(haven)

pk <- tibble(SUBJID = as.character(rep(1:2, each = 5)),
             TIME = rep(c(0, 1, 6, 12, 24), 2), 
             CONC = c(0, 10, 7, 6, 3, 0, 8, 6, 3, 1))
dm <- read_sas( "data/dm.sas7bdat")
act_full <- read_sas("data/actFull.sas7bdat")

my_plot <- ggplot(data = pk,
                  aes(x = TIME, y = CONC, colour = SUBJID)) +
  geom_line()
my_plot +
  ggtitle("Concentration over Time for 2 Subjects",
          subtitle = "Using Simulated Data") +
  xlab("Time (days)") + 
  ylab("Concentration (units)") +
  # *lim functions require a vector of lower and upper limits
  xlim(c(0, 30)) +
  ylim(c(0, NA)) # No upper limit defined, use data
  

# Create some baseline data
bl_data <- act_full %>% filter(VISITNUM == 20)

# Quickly plot it
qplot(data = bl_data, x = AGE, y = ACTTOT)

qplot(data = bl_data, x = AGE, y = ACTTOT, 
      main = "Baseline ACT Total Score against Age",
      xlab = "Age", ylab = "Total Score",
      xlim = c(15, 65), ylim = c(0, 25),
      facets = ~ ARM)

qplot(data = bl_data, x = ARM, y = ACTTOT, fill = ARM,
      geom = "boxplot",
      main = "Boxplot of ACT Total Score By Treatment at Baseline",
      xlab = "Treatment", ylab = "ACT Total Score")

# Make it blue!
qplot(data = bl_data, x = AGE, y = ACTTOT, colour = I("blue"))

## # Exercise Answers: quick plots
## 
## # 1
## week24 <- filter(act_full, VISITNUM == 60)
## qplot(data = week24, x = ACTBL, y = ACTTOT)
## # 1a
## qplot(data = week24, x = ACTBL, y = ACTTOT,
##       colour = TRT)
## # 1b
## qplot(data = week24, x = ACTBL, y = ACTTOT,
##       colour = TRT, shape = factor(ACTRSP24))
## # 1c
## qplot(data = week24, x = ACTBL, y = ACTTOT,
##       colour = TRT, shape = factor(ACTRSP24),
##       geom = "jitter")
## # 2a
## qplot(data = dm, geom = "density", x = AGE, fill = TRT)
## # 2b
## qplot(data = dm, geom = "density", x = AGE, fill = TRT,
##       alpha = I(.5), colour = I("green"))
## # 3a
## act_completer <- act_full %>%
##   filter(VISITNUM != 70)
## plot3a <- qplot(data = act_completer, x = VISITNUM, y = ACTCHGBL,
##       facets = ~ USUBJID,
##       geom = c("line", "point"),
##       colour = ARM)
## plot3a
## # 3b
## plot3a +
##   theme(legend.position="bottom") +
##   guides(colour = guide_legend(ncol = 2))
## # 4
## my_theme <- theme_bw()
## my_theme <- my_theme +
##   theme(panel.grid=element_blank(),
##         strip.text=element_text(face="bold"))
## plot3a +
##   my_theme



library(ggplot2)
library(tidyverse)
library(haven)
library(gridExtra, warn.conflicts = FALSE)
library(readr)

pk <- tibble(SUBJID = as.character(rep(1:2, each = 5)),
             TIME = rep(c(0, 1, 6, 12, 24), 2), 
             CONC = c(0, 10, 7, 6, 3, 0, 8, 6, 3, 1))

subj1 <- pk |>
    filter(SUBJID == 1)
act_full <- read_sas("data/actFull.sas7bdat")
theoph <- read_csv("theoph.csv")

# Create two plots
act_bl <- act_full |>
         filter(VISITNUM == 40)

base_info <- ggplot(data = act_bl,
                    aes(y = ACTTOT)) +
  theme(legend.position = "bottom")
plot1 <- base_info + geom_boxplot(aes(x = SEX, fill = ARM)) 
plot2 <- base_info + geom_point(aes(x = WEIGHT, colour = ARM))

# Arrange side by side
library(gridExtra)
grid.arrange(plot1, plot2, ncol = 2)


## 
## # Load package
## library(docorator)
## 
## # Create figure
## myplot <- ggplot(data = theoph, aes(x = TIME, y = CONC, group = SUBJID)) +
##   geom_line()
## 
## 
## # Export display
## myplot |>
##   as_docorator(
##     display_name = "myplot",
##     display_loc = "my/file/path",
##     header = fancyhead(
##       fancyrow(left = "My Study", center = NA, right = doc_pagenum()),
##       fancyrow(left = "My Population", center = NA, right = NA),
##       fancyrow(left = NA, center = "My Plot", right = NA)
##     ),
##     footer = fancyfoot(
##       fancyrow(left = doc_path(display_name, display_loc), center = NA, right = "Data as of 2025-01-01")
##     )
##   ) |>
##   render_pdf()
## 



sessionInfo()
