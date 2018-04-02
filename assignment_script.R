#install and load the gridExtra and tidyverse library
install.packages("gridExtra")
library(gridExtra)
library(tidyverse)
install.packages("ggpubr")
library("ggpubr")

#load the data
surveys <- read.csv("data/portal_data_joined.csv") 

#clean the data from NA and blank value
surveys_complete <- surveys %>%
  filter(!is.na(hindfoot_length)) %>%
  filter(!is.na(weight)) %>%
  filter(sex !="") %>%
  filter(species_id !="")

#piechart of the records
count_all_records <- surveys %>% tally()
count_complete <- surveys %>%
  filter(!is.na(hindfoot_length)) %>%
  filter(!is.na(weight)) %>%
  filter(sex !="") %>%
  filter(species_id !="") %>%
  tally()

count_unclomplete <- count_all_records - count_complete

slices <- c(count_complete[1,1],count_unclomplete[1,1])
lbls <- c("complete records", "uncomplete records")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Data Completeness")

#plotting the number of species and assign it to variable
species_plot <- ggplot(data = surveys_complete) + geom_bar(mapping = aes(x = species_id, fill = sex)) +
  theme(axis.text.x = element_text(size = 8, angle = 90))

#plotting the number of genus and assign it to variable
genus_plot <- ggplot(data = surveys_complete) + geom_bar(mapping = aes(x = genus, fill = sex)) + 
  theme(axis.text.x = element_text(angle = 30, size = 8))

#combine species plot and genus plot together
grid.arrange(species_plot, genus_plot, ncol=2, widths=c(6,6))

#weight and hindfoot_length distribution 
weight_distribution <- ggplot(data = surveys_complete, aes(x = weight)) + 
  geom_histogram(stat = "bin", binwidth = 3)
hlength_distribution <- ggplot(data = surveys_complete, aes(x = hindfoot_length)) + 
  geom_histogram(stat = "bin", binwidth = 2)

#mean weight and hindfoot length for each species
weight_sum <- aggregate(surveys_complete["weight"], surveys_complete["species_id"], FUN = mean)
rbind(head(weight_sum), tail(weight_sum))
weight_sum_plot <- ggplot(weight_sum, aes(x = species_id, y = weight)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(size = 8, angle = 90))

hlength_sum <- aggregate(surveys_complete["hindfoot_length"], surveys_complete["species_id"], FUN = mean)
rbind(head(hlength_sum), tail(hlength_sum))
hlength_sum_plot <- ggplot(hlength_sum, aes(x = species_id, y = hindfoot_length)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(size = 8, angle = 90))

#combine four plots together
grid.arrange(weight_distribution, hlength_distribution, weight_sum_plot, 
             hlength_sum_plot, ncol=2, widths=c(6,6))

#plotting the relationship between weight and hindfoot length for all species
plot_1 <- ggplot(surveys_complete, aes(x = weight, y = hindfoot_length, color = genus)) +
  geom_point(shape = 2, size = 1)
plot_2 <- plot_1 + geom_smooth(mapping = aes(linetype = "r2"), 
                               method = "lm",
                               formula = y ~ x + log(x), se = FALSE,
                               color = "red") 
plot_3 <- plot_2 + ggtitle("Relationship between weight and hindfoot length for all species") +
  scale_x_continuous(name = "Weight") + scale_y_continuous(name = "Hindfoot Length")
plot_4 <- plot_3 + theme_minimal() + 
  theme(text = element_text(color = "gray20"), 
        legend.position = c("right"), 
        legend.text = element_text(size = 8, color = "gray20"),
        axis.text = element_text(face = "italic"), 
        axis.title.x = element_text(size = 9, face = "bold", color = "gray20" ),
        axis.title.y = element_text(size = 9, face = "bold", color = "gray20"),
        title = element_text(size = 10, face = "bold", color = "gray20")) 
mR2 <- summary(lm(hindfoot_length ~ weight + log(weight), data = surveys_complete))$r.squared
mR2 <- paste0(format(mR2, digits = 3))
W_L_realtion_plot_all_species <- plot_4 + 
  scale_linetype(name = "", breaks = "r2",
                 labels = list(bquote(R^2==.(mR2))), 
                 guide = guide_legend(override.aes = list(linetype = 1, size = 2, color = "red")))

#list of species with n >= 2500
species_count <- surveys_complete %>%
  group_by(species) %>%
  tally() %>%
  filter(n >= 2500)

#filter of the common sample (>= 2500)
surveys_common <- surveys_complete %>%
  filter(species %in% species_count$species)
  
#summary of common sample's weight by species 
surveys_common %>%
  group_by(species) %>%
  summarize(mean_weight = mean(weight),
            max_weight = max(weight),
            min_weight = min(weight))

species_vs_weight <- select(surveys_common, species, weight)
plot(species_vs_weight)
ggplot(surveys_common, aes(x = species, y = weight)) + geom_boxplot()+xlab("Species")


#summary of common sample's length of hindfoot by species 
surveys_common %>%
  group_by(species) %>%
  summarize(mean_length = mean(hindfoot_length),
            max_length = max(hindfoot_length),
            min_length = min(hindfoot_length))

species_vs_length <- select(surveys_common, species, hindfoot_length)
plot(species_vs_length)
ggplot(surveys_common, aes(x = species, y = hindfoot_length)) + geom_boxplot()+xlab("Species")


# Linier Model Analysis between weight and hindfoot legth on the most common species

#create object for analysis : 
most_common_data <- surveys_common %>%
  filter(species == "merriami")

weight_vs_length <- select(most_common_data, weight, hindfoot_length)

#plot the data in scatter
plot(weight_vs_length)

#fitting linier model between weight and length of hindfoot
lm_weight_vs_length <- lm(hindfoot_length ~ weight, data=most_common_data)
summary(lm_weight_vs_length)
plot(lm_weight_vs_length)

#testing the mean weight between sex to see the different 
#Is the mean weight of female is significantly different compared to the male?
##compute the summary statistic
surveys_complete %>% group_by(sex) %>%
  summarise(count = n(),
            mean = mean(weight),
            sd = sd(weight))
##visualize the data using box plots
ggboxplot(surveys_complete, x = "sex", y = "weight", color = "sex", ylab = "Weight", xlab = "Sex")
##do the two population have the same variances? use "f-test". answer = no because p-value < 0.05
result.ftest <- var.test(weight ~ sex, data = surveys_complete)
##is there any significant difference between female and male weights? use "t-test".
##answer = no, because p value > 0.05. (female and male = same)
t.test(weight ~ sex, data = surveys_complete, var.equal = FALSE)
