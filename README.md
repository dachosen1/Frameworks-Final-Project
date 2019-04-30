# TripAdvisor Restaurants Info for 31 Euro-Cities

Authors: Mayur Bensal, Cass Ernst-Faletto and Anderson Nelson 

TripAdvisor an American company that currently holds the world’s largest travel site. It is a travel and restaurant website that hosts restaurant and hotel reviews as well as accommodation bookings and forums. We think analyzing TripAdvisor data is interesting as there is a high volume of traffic on the site, approximately 490 million unique monthly visitors. There is currently 730 million reviews and opinions on TripAdvisor with 7.5 million accommodations, restaurants, and attractions. TripAdvisor covers 136,000 destinations with 4.9 million restaurants on its website1. 

### Ratings and reviews for restaurants across 31 European cities

#### Context
The dataset we have chosen contains restaurant information for 31 European Cities. It is important to note that this dataset only contains restaurants that are listed in the TripAdvisor database, we do not have a list of restaurants in each city. Each restaurant has information about its ranking in the city, cuisine style, rating, price range, number of reviews and the reviews written by customers. 


#### Content
list of 31 cities in the data: Amsterdam (NL), Athens (GR) , Barcelona (ES) , Berlin (DE), Bratislava (SK), Bruxelles (BE), Budapest (HU), Copenhagen (DK), Dublin (IE), Edinburgh (UK), Geneva (CH), Helsinki (FI), Hamburg (DE), Krakow (PL), Lisbon (PT), Ljubljana (SI), London (UK), Luxembourg (LU), Madrid (ES), Lyon (FR), Milan (IT), Munich (DE), Oporto (PT), Oslo (NO), Paris (FR), Prague (CZ), Rome (IT), Stockholm (SE), Vienna (AT), Warsaw (PL), Zurich (CH).

  - **City:**  Location of the restaurant Cuisine Style: cuisine style(s) of the restaurant, in a Python list object 
- **Ranking:** Rank of the restaurant among the total number of restaurants in the city as a float object 
- **Rating:** Rate of the restaurant on a scale from 1 to 5, as a float object 
- **Price Range:** Price range of the restaurant among 3 categories
- **Number of Reviews:** Number of reviews that customers have let to the restaurant
- **Reviews:** 2 reviews that are displayed on the restaurants scrolling page of the city, as a list of list object where the first list contains the 2 reviews, and the second le dates when these reviews were written (115 673 non-null)
- **URL_TA:** part of the URL of the detailed restaurant page that comes after 'www.tripadvisor.com' as a string object (124 995 non-null)
- **ID_TA:** identification of the restaurant in the TA database constructed a one letter and a number (124 995 non-null)

## Goal of the Project : 

The goal of the project is to evaluate the  data from the restaurant owner standpoint: Trip advisor provides a lot of insights to consumer on how to select places to travel and how to evaluate travel options. For this analysis we wanted to provide data to help restaurants make better decision pricing, location, menu options. With this insight, restaurant owner can pivot their marketing campaign and offerings. We wanted to accomplish this by answering the  research questions. 
1. Understand the factors that impact restaurant ratings and price
2. Evaluate the data to understand user preferences and rating rationale 
3. Evaluate the eating trends of the different cities
4. What impact does healthy options have on customer’s rating of restaurants
5. Create a prediction model that classifies the restaurant ratings and prices range

## Conclusion: 

Restaurant owners must consider a variety of factors when launching  or managing a restaurant. 

1. As it relates to pricing the factors that a restaurant should consider are the type of cuisine, specifically recommending to explore recipes relating to: American,Asian,Bar,Brew Pub, British, Central European, European, Fast Food, French,Gluten Free Options,Halal,Healthy,Korean and Mediterranean

2. Europeans are trending towards healthier restaurant options, and there's an opportunity to build healthy conscious restaurants in Lisbon, Paris, Madrid, Oporto, Bratislava and Lyon

3. Digging deeper into Topic Modeling using Latent Dirichlet Algorithm, we found that topics like “Hidden Gem”, “Little and cosy space” actually make a restaurant more likely to receive a thumbs up from customers whereas topics such as “Tourist trap” and “Rude staff” can impact the reputation of restaurant drastically

4. After using predictive analytics to predict ratings,  we realized that analyzing the current system of collecting reviews hampers the analysis as different people have different ways to express and review the restaurants. Therefore, having an automated review predictor would make more valuable contributions in building a better predictive model as the reviews would capture the true sense of how people feel of that particular restaurant

**Source:** https://www.kaggle.com/damienbeneschi/krakow-ta-restaurans-data-raw 
