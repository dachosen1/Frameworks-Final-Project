# TripAdvisor Restaurants Info for 31 Euro-Cities

## Ratings and reviews for restaurants across 31 European cities

### Context
This dataset has been obtained by scraping TA (the famous tourism website) for information about restaurants for a given city. The scraper goes through the restaurants listing pages and fulfills a raw dataset. The raw datasets for the main cities in Europe have been then curated for futher analysis purposes, and aggregated to obtain this dataset. The scraper is a Python script, available on the GitHub repository here.

It uses principally pandas and BeautifulSoup libraries.

IMPORTANT: the restaurants list contains the restaurants that are registrered in the TA database only. All the restaurants of a city may not be resgistered in this database.

### Content
The dataset contain restaurants information for 31 cities in Europe: Amsterdam (NL), Athens (GR) , Barcelona (ES) , Berlin (DE), Bratislava (SK), Bruxelles (BE), Budapest (HU), Copenhagen (DK), Dublin (IE), Edinburgh (UK), Geneva (CH), Helsinki (FI), Hamburg (DE), Krakow (PL), Lisbon (PT), Ljubljana (SI), London (UK), Luxembourg (LU), Madrid (ES), Lyon (FR), Milan (IT), Munich (DE), Oporto (PT), Oslo (NO), Paris (FR), Prague (CZ), Rome (IT), Stockholm (SE), Vienna (AT), Warsaw (PL), Zurich (CH).
The data is a .csv file comma-separated that contains 125 433 entries (restaurants). It is structured as follow: - Name: name of the restaurant

- **City:**  Location of the restaurant Cuisine Style: cuisine style(s) of the restaurant, in a Python list object (94 046 non-null)
- **Ranking:** Rank of the restaurant among the total number of restaurants in the city as a float object (115 645 non-null)
- **Rating:** Rate of the restaurant on a scale from 1 to 5, as a float object (115 658 non-null)
- **Price Range:** Price range of the restaurant among 3 categories , as a categorical type (77 555 non-null) Number of Reviews: number of

**Source:** https://www.kaggle.com/damienbeneschi/krakow-ta-restaurans-data-raw 

