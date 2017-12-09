library(stringr)
library(car)
library(ggplot2)

data = read.csv("listings.csv")

# We count how many NA's each variable has
count_NA = 0
for (i in 1:ncol(data)){
  
  count_NA[i] = sum(is.na(data[,i]))
  
}

# exclude the variables where the majority of values are NA's
data = data[, -which(count_NA > 14000)]

# Some variables might not be very important. I included them anyway to test whether or not
# they could be useful
variables = c('id',
              'neighbourhood_cleansed',
              'property_type',
              'room_type',
              'accommodates',
              'bathrooms',
              'bedrooms',
              'beds',
              'bed_type',
              'amenities',
              'price',
              'cleaning_fee',
              'availability_30',
              'number_of_reviews',
              'review_scores_rating',
              'reviews_per_month')

#########################################  Pre-Processing #####################

data_filtered = data[ ,variables] # filter the data set based on variables
data_filtered = (na.omit(data_filtered)) # omit NA values

# convert price variable to numeric
data_filtered$price = as.character(data_filtered$price)
data_filtered$price = str_replace(data_filtered$price, ",", "")
data_filtered$price = as.numeric(str_sub(data_filtered$price, 2))

cat('Cleaning fee has a linear relationship with price.')
# converting security deposit from factor to numeric
data_filtered$cleaning_fee = as.character(data_filtered$cleaning_fee)
data_filtered$cleaning_fee = str_replace(data_filtered$cleaning_fee, ",", "")
data_filtered$cleaning_fee = as.numeric(str_sub(data_filtered$cleaning_fee, 2))

ind=which(is.na(data_filtered$cleaning_fee))
data_filtered[ind,"cleaning_fee"]=0

#################################################################################

cat('I use the log transformation in most cases for the plots. We can try other transformations too.')

# There are three exteme outliers that make the plots difficult to interpret
# We exclude them (price > 1000)
data_filtered = data_filtered[-which(data_filtered$price > 1000), ]

plot(data_filtered$neighbourhood_cleansed, log(data_filtered$price)) # plot neighborhood_cleansed vs price

plot(data_filtered$property_type, log(data_filtered$price)) # plot property_type vs price

table(data_filtered$property_type)
cat('There are many levels in property_type with too few observations.\n
    I drop levels with less than 5 observations.')

data_filtered = data_filtered[!as.numeric(data_filtered$property_type) %in% 
                                which(table(data_filtered$property_type) <= 5), ]    

data_filtered$property_type = droplevels(data_filtered$property_type) # drop levels of 0 observations

plot(data_filtered$property_type, log(data_filtered$price)) # plot again

plot(data_filtered$room_type, log(data_filtered$price)) # plot room_type vs price

cat('We are considering the property_type variable, but there are properties such as
    (for example) "Camper/RV" or "Cabin" that do not really present a linear relationship between
    bathrooms/bedrooms and price, maybe due to the fact that there are too few obsevations.
    Generally, we could discard factor levels of property_types that do not present a linear trend,
    because they *might* result to a model with less accuracy. The following code constructs
    plots between beds/bedrooms/bathrooms/accommodates and price for different levels of
    property_type. I splitted them into two groups "property_type1", "property_type2" so the plots
    can be interpretable. By examining the plots, we can choose which levels are important for the model.
    We can also experiment with different transformations of x and y, just by applying the transformation
    in the aes() argument.')

property_type1 = data_filtered[which(data_filtered$property_type == 'Apartment' | 
                                       data_filtered$property_type == 'Bed & Breakfast' |
                                       data_filtered$property_type == 'Boat' | 
                                       data_filtered$property_type == 'Cabin' |
                                       data_filtered$property_type == 'Camper/RV' |
                                       data_filtered$property_type == 'Other'), ]

ggplot(data = property_type1, aes(x = bathrooms, y = price)) + 
  geom_point() + facet_grid(property_type ~ .)

ggplot(data = property_type1, aes(x = bedrooms, y = price)) + 
  geom_point() + facet_grid(property_type ~ .)

ggplot(data = property_type1, aes(x = beds, y = price)) + 
  geom_point() + facet_grid(property_type ~ .)

ggplot(data = property_type1, aes(x = accommodates, y = price)) + 
  geom_point() + facet_grid(property_type ~ .)

ggplot(data = property_type1, aes(x = log(accommodates + bathrooms + beds + bedrooms), y = log(price))) + 
  geom_point() + facet_grid(property_type ~ .)



property_type2 = data_filtered[which(data_filtered$property_type == 'Condominium' | 
                                       data_filtered$property_type == 'Guesthouse' |
                                       data_filtered$property_type == 'House' | 
                                       data_filtered$property_type == 'Loft' |
                                       data_filtered$property_type == 'Townhouse' |
                                       data_filtered$property_type == 'Villa'), ]

ggplot(data = property_type2, aes(x = bathrooms, y = price)) + 
  geom_point() + facet_grid(property_type ~ .)

ggplot(data = property_type2, aes(x = bedrooms, y = price)) + 
  geom_point() + facet_grid(property_type ~ .)

ggplot(data = property_type2, aes(x = beds, y = price)) + 
  geom_point() + facet_grid(property_type ~ .)

ggplot(data = property_type2, aes(x = accommodates, y = price)) + 
  geom_point() + facet_grid(property_type ~ .)

ggplot(data = property_type2, aes(x = accommodates + bathrooms + beds + bedrooms, y = log(price))) + 
  geom_point() + facet_grid(property_type ~ .)


plot(data_filtered$bed_type, log(data_filtered$price)) # price vs bed type


plot(cleaning_fee$cleaning_fee, cleaning_fee$price)

b=data_filtered
#b=data_filtered[-anomaly,]

model1 = lm(log(b$price) ~ b$neighbourhood_cleansed + 
            b$property_type + 
            b$room_type +
            as.factor(b$accommodates) + 
            as.factor(b$bedrooms) + 
            b$availability_30 + 
            b$number_of_reviews+
            b$cleaning_fee+
            as.factor(b$bathrooms)+
            as.factor(b$beds) + 
            b$room_type:b$cleaning_fee +
            b$room_type:b$bathrooms + 
            b$room_type:b$accommodates)

summary(model1)

qqPlot(model1$residuals)
hist(model1$residuals, breaks = 100)

res = unname(model1$residuals)
anom_obs = which((res > 0.85 | res < -0.85))

data_filtered2 = b[-anom_obs, ]

model2=lm(log(data_filtered2$price) ~ data_filtered2$neighbourhood_cleansed + 
            data_filtered2$property_type + 
            data_filtered2$room_type +
            as.factor(data_filtered2$accommodates) + 
            as.factor(data_filtered2$bedrooms) + 
            data_filtered2$availability_30 + 
            data_filtered2$number_of_reviews+
            data_filtered2$cleaning_fee+
            as.factor(data_filtered2$bathrooms)+
            as.factor(data_filtered2$beds) + 
            data_filtered2$room_type:data_filtered2$cleaning_fee +
            data_filtered2$room_type:data_filtered2$bathrooms + 
            data_filtered2$room_type:data_filtered2$accommodates)


summary(model2)
qqPlot(model2$residuals)
hist(model2$residuals)
plot(model2$fitted.values,model2$residuals)



