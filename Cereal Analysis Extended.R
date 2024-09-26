'''
1.Introduction
This report presents a statistical analysis of a cereal dataset, exploring various nutritional aspects of different cereal brands. The analysis aims to provide insights into the nutritional content of cereals and their potential impact on consumer health and marketing strategies.

1.1	Business Problem:
Cereal manufacturers face the challenge of producing products that are both appealing to consumers and nutritionally balanced. This analysis will help understand the current landscape of cereal products in terms of their nutritional content, which can inform product development and marketing strategies.

1.2	Key Business Questions:
1.	What is the distribution of key nutritional elements across different cereal brands?
2.	Is there a relationship between a cereal sugar content and its consumer rating?	
3.	How do different manufacturers compare in terms of the nutritional content of their cereals?

1.3	Hypothesis:

H0: There is no significant correlation between a cereals sugar content and its consumer rating.
H1:  There is a significant correlation between a cereals sugar content and its consumer rating.

H0: There is no significant difference in the mean calorie content among cereals from different manufacturers.

H1: There is a significant difference in the mean calorie content among cereals from different manufacturers.
'''''
options(repos = c(CRAN = "https://cloud.r-project.org"))
'''''
2.Data Preparation and Exploratory Data Analysis
a.	Loading and inspecting the data:
'''''''
# Install necessary packages
install.packages(c("readr", "ggplot2", "cluster", "factoextra"))
library(tidyverse)
library(ggplot2)
getwd()
# Import the dataset
cereals <- read.csv("Cereals nutritional data.csv", stringsAsFactors = FALSE)
# View the first few rows of the dataset
head(cereals)
# Structure of the dataset
str(cereals)
# Summary statistics
summary(cereals)
'''
b.	Data Cleaning:
Removing the rows with negative values.
'''
# Remove rows with negative values
cereals_clean <- cereals %>%
  filter_all(all_vars(. >= 0))
# Check for any remaining NA values
sum(is.na(cereals_clean))
# Convert manufacturer and type to factors
cereals_clean$mfr <- as.factor(cereals_clean$mfr)
cereals_clean$type <- as.factor(cereals_clean$type)
# Display summary of cleaned data
summary(cereals_clean)
par(mfrow=c(2,2))
'''
c.Exploratory Data Analysis:
i.	Distribution of Key Nutritional Elements:
Creating a histogram for key nutritional elements:
'''
hist(cereals_clean$calories, main="Distribution of Calories", xlab="Calories")
hist(cereals_clean$protein, main="Distribution of Protein", xlab="Protein (g)")
hist(cereals_clean$fiber, main="Distribution of Fiber", xlab="Fiber (g)")
hist(cereals_clean$sugars, main="Distribution of Sugar", xlab="Sugar (g)")
'''
Interpretation
-Calories: Most cereals contain between 100-110 calories per serving, with a few low-calorie options around 50-70	calories and some high-calorie options up to 160 calories.
-	Protein: The majority of cereals contain 2-3 gram of protein per serving, with a few high-protein options containing up to 6 grams.
-	Fiber: Most cereals have low fiber content (0-2 grams), but there are some high-fiber options with up to 14 grams per serving.
-	Sugar: There’s a wide distribution of sugar content, with peaks around 3 grams and 11 grams, suggesting two main categories: low sugar and high-sugar cereals
'''
plot(cereals_clean$sugars, cereals_clean$rating, 
     main="Sugar Content vs Rating", 
     xlab="Sugar Content (g)", ylab="Rating")
abline(lm(rating ~ sugars, data=cereals_clean), col="red")

nutritional_elements <- cereals_clean %>%
  select(calories, protein, fiber, sugars)

nutritional_long <- nutritional_elements %>%
  gather(key = "nutrient", value = "amount")

ggplot(nutritional_long, aes(x = amount)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~ nutrient, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Key Nutritional Elements in Cereals",
       x = "Amount", y = "Count")

'''
3.Inferential Statistics
a.	Correlation between sugar content and rating:
'''
# Correlation test
cor.test(cereals_clean$sugars, cereals_clean$rating)
cor_test <- cor.test(cereals_clean$sugars, cereals_clean$rating)
print(cor_test)
boxplot(calories ~ mfr, data=cereals_clean, 
        main="Calorie Content by Manufacturer", 
        xlab="Manufacturer", ylab="Calories")
aggregate(calories ~ mfr, data=cereals_clean, mean)

'''
There are differences in calorie content among manufacturers:
-	Manufacturer R has the highest average calorie content (110 calories).
-	Manufacturer N has the lowest average calorie content (96.7 calories).
-	Most manufacturers have average calorie contents between 100-110 calories.
'''
anova_result <- aov(calories ~ mfr, data=cereals_clean)
summary(anova_result)

# Post-hoc test (if ANOVA is significant)
if(summary(anova_result)[[1]]$`Pr(>F)`[1] < 0.05) {
  TukeyHSD(anova_result)
}
install.packages(c("cluster", "factoextra"))

# Load required libraries
library(cluster)
library(factoextra)

# Assuming you've already performed the k-means clustering and have km_result and scaled_data

if (!requireNamespace("cluster", quietly = TRUE)) install.packages("cluster")
if (!requireNamespace("factoextra", quietly = TRUE)) install.packages("factoextra")
library(cluster)
library(factoextra)

# Prepare data for clustering
cluster_data <- cereals_clean[, c("calories", "protein", "fat", "sodium", "fiber", "carbo", "sugars")]
# Scale the data
scaled_data <- scale(cluster_data)

# Determine optimal number of clusters
elbow_plot <- fviz_nbclust(scaled_data, kmeans, method = "wss") + 
  labs(title = "Elbow Method for Optimal k")
print(elbow_plot)
# Perform k-means clustering
set.seed(123)
km_result <- kmeans(scaled_data, centers = 3, nstart = 25)
# Visualize clusters
fviz_cluster(km_result, data = scaled_data,
             geom = "point",
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())
#
'''
The elbow method suggests that 3 clusters would be optimal for this dataset. The cluster analysis reveals 3 distinct clusters of cereals based on their nutritional profiles:
Cluster 1: Low-calorie, high-fiber cereals.
Cluster 2: Medium-calorie, balanced-nutrient cereals.
Cluster 3: High-calorie, high-sugar cereals.

This supports our hypothesis that there are distinct clusters of cereals based on their nutritional profiles.


4.	Discussion and Recommendations:
Based on the analysis, we can draw several insights and make recommendations:
a.	Sugar content is strongly negatively correlated with consumer ratings. Manufacturers should consider reducing sugar content in their cereals to improve consumer satisfaction and health perception. 
These could involve:

-	Developing the new low-level sugar cereal options.
-	 Gradually reducing sugar content in existing popular cereals.
-	 Exploring natural sweetness or flavor enhancer to maintain taste while reducing sugar.

b.	 Despite the lack of statistically significant differences in calorie content among manufacturers, there are still variations that could be leveraged:
-	Manufacturers with low average calorie content, for example N and Q, could highlight this in their marketing strategies to appeal to health-conscious consumers. 
-	Manufacturers with higher calorie content could focus on other nutritional benefits, for example high protein or fiber, to differentiate their products.

c.	The distribution of nutritional elements suggests opportunities for product development:
-	There is a gap in the market for high-protein cereals (>6g per serving).
-	High-fiber cereals (>5g per serving) are relatively uncommon and could be a point of differentiation.
-	Consider developing cereals that balance multiple nutritional benefits (e.g., high protein, high fiber, low sugar) to create unique selling propositions.

d.	 Given the wide range of sugar content (0-15g per serving), consider implementing a clear labelling system to help consumers make informed choices:
  -	Use a traffic light system (green, amber, red) to indicate low, medium, and high sugar content.
-	Highlight cereals that meet certain nutritional criteria (e.g., "high fiber",
"low sugar") on packaging and in marketing materials.

e.	For future product development, focus on the factors that contribute to higher ratings:
  -	Analyse the top-rated cereals to identify common characteristics beyond just low sugar content.
-	Consider consumer taste tests to ensure that reducing sugar does not negatively impact taste and acceptance.

f.	 Educational marketing campaigns could be beneficial:
-	Inform consumers about the importance of various nutritional elements in cereals (e.g., the benefits of fiber, the role of protein in satiety).
-	Provide guidance on how to interpret nutritional information on cereal packaging.


5.	Limitations and future work:

While this analysis provides valuable insights, it has several limitations that
could be addressed in future work:
1	Limited scope of nutritional information: The dataset doesnt include information on other important nutritional elements like vitamins and minerals.
Future studies could incorporate a more comprehensive nutritional profile.
2	Lack of temporal data: This analysis is based on a snapshot of cereal nutritional content. A longitudinal study could reveal trends in how cereal nutrition has changed over time.
3	No consumer demographic information: The ratings dont provide insight into which consumer groups prefer which types of cereals. Future studies could include consumer demographic data to allow for more targeted recommendations.
4	Absence of price data: Including price information could provide insights into the relationship between nutritional quality and cost, which could be valuable for both consumers and manufacturers.
5	Limited manufacturer information: A more detailed breakdown of manufacturers and their market share could provide additional context for the analysis.

Future work could address these limitations by:
⁃	Collecting more comprehensive nutritional data, including micronutrients.
⁃	Conducting a longitudinal study of cereal nutrition and consumer preferences.
⁃	Incorporating consumer demographic data to segment preferences.
⁃	Including price data to analyse the cost-nutrition relationship.
⁃	Gathering more detailed manufacturer and market share information.

Additionally, future studies could explore:
⁃	The impact of packaging and marketing on cereal ratings and sales.
⁃	The relationship between cereal nutrition and broader dietary patterns.
⁃	Cross-cultural comparisons of cereal preferences and nutritional content

6.	Conclusion: 
This analysis of the cereal’s dataset has revealed several key insights:

a.	There is a strong negative correlation between sugar content and cereal ratings, suggesting that consumers prefer cereals with lower sugar content.

b.	While there are no statistically significant differences in calorie content among manufacturers, there are variations that could be leveraged in marketing and product development strategies.

c.	There are opportunities in the market for cereals with specific nutritional profiles, particularly high-protein and high-fiber options.

d.	 The wide range of sugar content across cereals highlights the need for clear labeling and consumer education.

-	These findings have important implications for cereal manufacturers, marketers, and health-conscious consumers. By focusing on developing and promoting cereals with balanced nutritional profiles - particularly those lower in sugar - manufacturers can potentially improve both the healthfulness of their products and consumer satisfaction.

-	The cereal industry is at a crossroads, with increasing consumer awareness of
nutrition coming up against traditional preferences for taste. This analysis suggests that there is room for innovation in creating cereals that are both nutritious and appealing to consumers. By leveraging these insights, cereal manufacturers can position themselves to meet evolving consumer demands while promoting healthier eating habits.


'''


