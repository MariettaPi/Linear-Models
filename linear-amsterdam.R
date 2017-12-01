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

# Variables we are going to keep
v = c('id',
      'host_is_superhost',
      'host_listings_count',
      'neighborhood_cleansed',
      'property_type',
      'room_type')

# Some variables might not be very important. I included them anyway to test whether or not
# they could be useful
variables = c('id',
              'host_is_superhost',
              'host_listings_count',
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
              'security_deposit',
              'cleaning_fee',
              'guests_included',
              'extra_people',
              'minimum_nights',
              'availability_30',
              'availability_60',
              'availability_90',
              'availability_365',
              'number_of_reviews',
              'review_scores_rating',
              'instant_bookable',
              'reviews_per_month')


data_filtered = data[ ,variables] # filter the data set based on variables
data_filtered = (na.omit(data_filtered)) # omit NA values

# convert price variable to numeric
data_filtered$price = as.character(data_filtered$price)
data_filtered$price = str_replace(data_filtered$price, ",", "")
data_filtered$price = as.numeric(str_sub(data_filtered$price, 2))

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


cat('Cleaning fee has a linear relationship with price.')
# converting security deposit from factor to numeric
data_filtered$cleaning_fee = as.character(data_filtered$cleaning_fee)
data_filtered$cleaning_fee = str_replace(data_filtered$cleaning_fee, ",", "")
data_filtered$cleaning_fee = as.numeric(str_sub(data_filtered$cleaning_fee, 2))

ind=which(is.na(data_filtered$cleaning_fee))
data_filtered[ind,"cleaning_fee"]=0

# Setting NA's to zero
cleaning_fee = na.omit(data_filtered)
plot(cleaning_fee$cleaning_fee, cleaning_fee$price)
##

model=lm(data = data_filtered, price ~ neighbourhood_cleansed + property_type + 
           availability_30 + number_of_reviews + accommodates + bedrooms)

acc_st=scale(data_filtered$accommodates,center=TRUE, scale=FALSE)
bedr_st=scale(data_filtered$bedrooms,center=TRUE, scale=FALSE)
rev_st=scale(data_filtered$number_of_reviews,center=TRUE, scale=FALSE)
av_st=scale(data_filtered$availability_30,center=TRUE, scale=FALSE)
clean_st=scale(data_filtered$cleaning_fee,center=TRUE, scale=FALSE)


model2=lm(log(data_filtered$price) ~data_filtered$neighbourhood_cleansed + data_filtered$property_type + 
            acc_st + bedr_st + rev_st + av_st + clean_st)

summary(model2)
anova(model2)
cor(acc_st,bedr_st)

qqPlot(model2$residuals)




#####################################################################################
#####################################################################################
#####################################################################################

data = data[ , -c(1:39)]

data = (na.omit(data))
#data = data[data$room_type == "Entire home/apt", ]

data = data[,-c(2,3,6,5,7,8,9,10,11,13,18,21,22,25,26,27,29,30,31,32,33,34)] # room_type, guests_included, extra_people, minimum_nights
data = data[,-c(23:29)]

data$price = as.character(data$price)
data$price = str_replace(data$price, ",", "")
data$price = as.numeric(str_sub(data$price, 2))
y = (data$price^-0.4 - 1)/-0.4

extract_amenities = function(amenities){
    
    amenities = as.character(amenities)
    amenities_cleaned = str_replace_all(amenities, "[{|}|\"]","")
    return(str_split(amenities_cleaned, ","))

}

amenities_list = lapply(data$amenities, extract_amenities)

num_amenities = numeric()
for(i in 1:length(amenities_list)){
    
    n = length(amenities_list[[i]][[1]])
    num_amenities = append(num_amenities, n)
    
}

unique_amenities = unique(unlist(amenities_list))

x1 = ((data$bathrooms + 2)^-0.1 - 1)/-0.1
x2 = ((data$beds + 2)^-0.1 - 1)/-0.1
x3 = ((data$accommodates + 2)^-0.1 - 1)/-0.1
x4 = ((data$bedrooms + 2)^-0.1 - 1)/-0.1


#makethelist=function(a){
    
#  b = paste(unlist(strsplit(a,""))[2:(nchar(a)-1)],collapse="",sep="")
#  p = strsplit(str_replace_all(b,"[[:punct:]]", " "),"  ")
#  psoli = str_replace(p[[1]],"^\\s","")
  
#  return(psoli)
#}

q = lapply(as.character(data$amenities), makethelist)

unique(unlist(q))

# tsekare ta plots twra, fainetai oti einai psilomultimodal to distribution giati stis psiles times 
# paizei pali megalo frequency.... kai to qqnorm thymizei kati se x^3 ..na doume ti transform tha paixei
y = data$price
log_y = data$price

qqnorm(y)
qqnorm(log_y)
qqPlot(log_y)
hist(y)
hist(log_y)

# epishs auth einai mia protoleia morfh toy dataset. exw diwxei polla pantelws axrista variables kai 
# exo afairesei ta osa xreiazontai text mining .. 
# protash mou na ftiaxoume grading klimaka gia ta amenities me function poy tha ginei supply pantou kai tha 
# vgazei continious variable.
# deutero variable to accomodates-bedrooms-bathrooms-beds pou tha kanoyme enan typo kai tha dinei epishs
# to "spaciousness" kai tha apofygoume thn metaxy tous syxsetish pernontas ta ola anexartita kai tha rixoume
# to multicollinearity..
# trito poly shmantiko to location analoga thn geitonia.. edw exw krathsei to zipcode giati boroyme na paroyme 
# zip code tou kentroy kai na ypologisoume eukleidies apostaseis kai na ftiaxoume pio akrives variable
# poso konta sto kentro einai giati auto einai TO PIO SHMANTIKO. 
# kata ta alla ta review columns einai ola poly skewed right giati oloi vazoun kales vathmologies 
# kai mas gamane thn mana. alla ta afisa gia na ta deite.
                                                                ### CEO Mr.Efklidis Katsaros.