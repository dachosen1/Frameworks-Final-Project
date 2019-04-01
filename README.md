# TripAdvisor Restaurants Info for 31 Euro-Cities

### Ratings and reviews for restaurants across 31 European cities

#### Context
This dataset has been obtained by scraping TA (the famous tourism website) for information about restaurants for a given city. The scraper goes through the restaurants listing pages and fulfills a raw dataset. The raw datasets for the main cities in Europe have been then curated for futher analysis purposes, and aggregated to obtain this dataset.

IMPORTANT: the restaurants list contains the restaurants that are registrered in the TA database only. All the restaurants of a city may not be resgistered in this database.

#### Content
The dataset contain restaurants information for 31 cities in Europe: Amsterdam (NL), Athens (GR) , Barcelona (ES) , Berlin (DE), Bratislava (SK), Bruxelles (BE), Budapest (HU), Copenhagen (DK), Dublin (IE), Edinburgh (UK), Geneva (CH), Helsinki (FI), Hamburg (DE), Krakow (PL), Lisbon (PT), Ljubljana (SI), London (UK), Luxembourg (LU), Madrid (ES), Lyon (FR), Milan (IT), Munich (DE), Oporto (PT), Oslo (NO), Paris (FR), Prague (CZ), Rome (IT), Stockholm (SE), Vienna (AT), Warsaw (PL), Zurich (CH).

  - **City:**  Location of the restaurant Cuisine Style: cuisine style(s) of the restaurant, in a Python list object 
- **Ranking:** Rank of the restaurant among the total number of restaurants in the city as a float object 
- **Rating:** Rate of the restaurant on a scale from 1 to 5, as a float object 
- **Price Range:** Price range of the restaurant among 3 categories
- **Number of Reviews:** Number of reviews that customers have let to the restaurant
- **Reviews:** 2 reviews that are displayed on the restaurants scrolling page of the city, as a list of list object where the first list contains the 2 reviews, and the second le dates when these reviews were written (115 673 non-null)
- **URL_TA:** part of the URL of the detailed restaurant page that comes after 'www.tripadvisor.com' as a string object (124 995 non-null)
- **ID_TA:** identification of the restaurant in the TA database constructed a one letter and a number (124 995 non-null)

Missing information for restaurants (for example unrated or unreviewed restaurants) are in the dataset as NaN (numpy.nan)
**Source:** https://www.kaggle.com/damienbeneschi/krakow-ta-restaurans-data-raw 

