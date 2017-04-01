install.packages("gapminder")
data(comics)
getwd()
setwd("/home/yanyan/R_essential")
df=read.csv("comics.csv")
dim(df)
#contigency table
table(df$ALIGN,df$SEX)

# Load dplyr
library(dplyr)

# Remove genderfluid characters level
df <- df %>%
  filter(SEX != "Genderfluid Characters") %>%
  droplevels()


# Load ggplot2
library(ggplot2)

#Create a side-by-side barchart with align on the x-axis 
#and gender as the fill aesthetic.
#Create another side-by-side barchart with gender on 
#the x-axis and align as the fill aesthetic. 
#Rotate the axis labels 90 degrees to help readability.
names(df)

# Create side-by-side barchart of gender by alignment
ggplot(df, aes(x = ALIGN, fill = SEX)) + 
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90))

# Create side-by-side barchart of alignment by gender
#dodge tell it that the bar should not be stacked
ggplot(df, aes(x = SEX, fill = ALIGN)) + 
  geom_bar(position="dodge") +
  theme(axis.text.x = element_text(angle = 90))

tab = table(df$ALIGN, df$SEX)
options(scipen=999, digits=3)  #Print fewer digits
prop.table(tab)   #Joint proportions
prop.table(tab,2)   #conditional on columns
prop.table(tab,1)

#Create a stacked barchart of gender counts with align on the x-axis
# Plot of gender by align
ggplot(df, aes(x = ALIGN, fill = SEX)) +
  geom_bar()


#By adding position = fill to geom_bar(), 
#you are saying you want the bars to fill the entire height 
#of the plotting window, thus displaying proportions and not raw counts.
# Plot proportion of gender, conditional on align
ggplot(df, aes(x = ALIGN, fill = SEX)) + 
  geom_bar(position = "fill")

levels(df$ALIGN)
# Change the order of the levels in align
df$ALIGN <- factor(df$ALIGN, 
                       levels = c("", "Bad Characters", "Good Characters","Neutral Characters"))

# Create plot of align
ggplot( df, aes(x = ALIGN)) + 
  geom_bar()


## Plot of alignment broken down by gender
ggplot(df, aes(x =ALIGN)) + 
  geom_bar() +
  facet_wrap(~ SEX)

library(ggplot2)

data(mtcars)
names(mtcars)
mtcars$am
mtcars$am=as.factor(mtcars$am)


library(ggplot2)

mtcars$hp
# Create faceted histogram
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(bins =20) +
  facet_grid(. ~ am)

?geom_density()

data("mpg")
dim(mpg)
head(mpg)

str(mpg)
table(mpg$cyl)
mpg$cyl = as.factor(mpg$cyl)
levels(mpg$cyl)
table(mpg$cyl)

# Filter cars with 4, 6, 8 cylinders, drop one level, the rows will be reduced
mpg <- mpg %>%   
  filter(cyl != 5) %>%
  droplevels()


# Create box plots of city mpg by ncyl
ggplot(mpg, aes(x = as.factor(cyl), y =cty)) +
  geom_boxplot()

# Create overlaid density plots for same data
ggplot(mpg, aes(x = cty, fill = as.factor(cyl))) +
  geom_density(alpha = .3)

names(mtcars)

mtcars %>%
  ggplot(aes(x=hp)) +
  geom_histogram(bins=10) +
  ggtitle("hist of horsepower")

# Create hist of horsepwr for affordable cars
mtcars %>% 
  filter(cyl<5) %>%
  ggplot(aes(x=hp)) +
  geom_histogram() +
  ggtitle("hist of affordable cars")

data("diamonds")

names(diamonds)

ggplot(diamonds, aes(x = price, fill = as.factor(color))) +
  geom_density(alpha = .3)

data("mpg")
names(mpg)

data("mtcars")


# Create hist of horsepwr with binwidth of 3
mtcars %>%
  ggplot(aes(hp)) +
  geom_histogram(binwidth =3) +
  ggtitle("hist of horsepwr with binwidth of 3")


mtcars %>%
  ggplot(aes(hp)) +
  geom_histogram(binwidth =30) +
  ggtitle("hist of horsepwr with binwidth of 30")


mtcars%>%
  ggplot(aes(hp)) +
  geom_histogram(binwidth =30) +
  ggtitle("hist of horsepwr with binwidth of 30")

summary(mtcars$hp)

# Construct box plot of msrp
mtcars %>%
  ggplot(aes(x = 1, y = hp)) +
  geom_boxplot()

# Exclude outliers from data
cars_no_out <- mtcars %>%
  filter(hp<60)

## higher dimentionality data
mtcars$mpg
names(mtcars)
mtcars$cyl = as.factor(mtcars$cyl)
mtcars$am = as.factor(mtcars$am)

mtcars %>%
  ggplot(aes(mpg))+
  geom_density()+
  facet_grid(cyl~am, labeller = label_both)

mtcars$cyl = as.numeric(mtcars$cyl)

mtcars$cyl

mtcars= mtcars %>%
  mutate (large_cyl = cyl %in% c(2,3))

mtcars %>%
  group_by(large_cyl)  %>%
  summarise(mean(mpg),
            median(mpg))

dim(mtcars)

mtcars %>%
  slice (10:20) %>%
  group_by(large_cyl) %>%
  summarize(mean(mpg))


library("gapminder")
data("gapminder")
# Create dataset of 2007 data
gap2007 <- filter(gapminder, year=="2007")
names(gap2007)

# Compute groupwise mean and median lifeExp
gap2007 %>%
  group_by(continent) %>%
  summarize(mean(lifeExp),
            median(lifeExp))

# Generate box plots of lifeExp for each continent
gap2007 %>%
  ggplot(aes(x =continent, y = lifeExp)) +
  geom_boxplot()

var(gapminder$lifeExp)  # variance
sd(gapminder$lifeExp)   #standard deviation
summary(gapminder$lifeExp)
IQR(gapminder$lifeExp)


#For each continent in gap2007, 
#summarize life expectancies using the sd(), the IQR(), 
#and the count of countries, n(). No need to name the 
#new columns produced here. 
#The n() function within your summarize() call does not take any arguments.

# Compute groupwise measures of spread
gap2007 %>%
  group_by(continent) %>%
  summarize(sd(lifeExp),
            IQR(lifeExp),
            n())

# Generate overlaid density plots
gap2007 %>%
  ggplot(aes(x = lifeExp, fill = continent)) +
  geom_density(alpha = 0.3)



# Create density plot of old variable
gap2007 %>%
  ggplot(aes(x = pop)) +
  geom_density()

# Transform the skewed pop variable
gap2007 <- gap2007 %>%
  mutate(log_pop = log(pop))

# Create density plot of new variable
gap2007 %>%
  ggplot(aes(x = log_pop)) +
  geom_density()


summary(diamonds$price)
data(diamonds)
diamonds = diamonds %>%
  mutate(is_outlier = price>15000)

diamonds %>%
  filter(is_outlier) %>%
  arrange(desc(price))

diamonds %>%
  filter(!is_outlier) %>%
  ggplot(aes(x=price, fill=color))+
  geom_density(alpha=0.3)



# Filter for Asia, add column indicating outliers
gap_asia <- gap2007 %>%
  filter(continent == "Asia") %>%
  mutate(is_outlier = lifeExp <50)

# Remove outliers, create box plot of lifeExp
gap_asia %>%
  filter(!is_outlier) %>%
  ggplot(aes(x = 1, y = lifeExp)) +
  geom_boxplot()

data(mtcars)
names(mtcars)
mtcars$cyl
mtcars$am =as.factor(mtcars$am)
mtcars %>%
  mutate(high_cyl = cyl>6) %>%
  ggplot(aes(x = high_cyl, fill = am)) +    # this fill is conditional on am
  geom_bar(position = "fill")    # this fill is for porportion

mtcars %>%
  mutate(high_cyl = cyl>6) %>%
  ggplot(aes(x = high_cyl, fill = am)) +    # this fill is conditional on am
  geom_bar()    # this fill is for porportion


# data integrity

mtcars$cyl<0

sum(mtcars$cyl<0)


data(diamonds)
names(diamonds)

# this is the way to change the order of levels
diamonds$color =factor(diamonds$color, levels=c("J","I","H","G","F","E","D"))

diamonds%>%
  ggplot(aes(color))+
  geom_bar()+
  facet_grid(.~cut)

diamonds%>%
  ggplot(aes(cut))+
  geom_bar()+
  facet_grid(.~color)


data("diamonds") 
diamonds%>%
  ggplot(aes(x=cut,y=price,color=factor(color)))+
  geom_point()

names(diamonds)

ggplot(data = diamonds, aes(x =carat , y = price)) +
  geom_point() + 
  coord_trans(x = "log10", y = "log10")

# Scatterplot with scale_x_log10() and scale_y_log10()
ggplot(data = diamonds, aes(x = carat, y = price)) +
  geom_point(alpha=0.5, position = "jitter") +  # add alpha and position to geom_point
  scale_x_log10() + scale_y_log10()