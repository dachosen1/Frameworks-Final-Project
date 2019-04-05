Frameworks Final Project: Data Cleaning and Exloration
================

``` r
library(knitr); library(Hmisc); library(DT); library(ggplot2); library(dplyr); 
library(reshape2); library(ggthemes); library(stringr); library(data.table); library(tidytext); library(recommenderlab); library(qdapTools); library(purrr); library(devtools)
```

``` r
name = "Name"
city.name = "City"
cuisine.name = "Cuisine Style"
ranking.name = "Ranking"
rating.name = "Rating"
price.name = "Price Range"
number.reviews.name = "Number of Reviews"
review.name = "Reviews"
```

``` r
dat <- fread(input ='Data/TA_restaurants_curated.csv', verbose = FALSE)
```

``` r
# specialized function to calculate the numberic value of '$,$$,$$$,$$$$` ratings 
replace.value = function (data, colname, pattern = '-' ){

number = vector() 
for ( i in seq_along(data)) {
 if (is.na(str_match(string = data[i, get(colname)], pattern = '-'))) {
     number[i] = nchar(data[i, get(colname)])
 } else { number[i] = ((nchar(data[i, get(colname)]) - 3) / 2)
  }
}

print(number)

}
```

## Data Exploration

    ##           City count percentage
    ##  1:     London 18212      14.51
    ##  2:      Paris 14874      11.85
    ##  3:     Madrid  9543       7.60
    ##  4:  Barcelona  8425       6.71
    ##  5:     Berlin  7078       5.64
    ##  6:      Milan  6687       5.33
    ##  7:       Rome  5949       4.74
    ##  8:     Prague  4859       3.87
    ##  9:     Lisbon  3986       3.18
    ## 10:     Vienna  3724       2.97
    ## 11:  Amsterdam  3434       2.74
    ## 12:   Brussels  3204       2.55
    ## 13:    Hamburg  3131       2.49
    ## 14:     Munich  2995       2.39
    ## 15:       Lyon  2930       2.33
    ## 16:  Stockholm  2705       2.15
    ## 17:   Budapest  2606       2.08
    ## 18:     Warsaw  2352       1.87
    ## 19: Copenhagen  2109       1.68
    ## 20:     Dublin  2082       1.66
    ## 21:     Athens  1938       1.54
    ## 22:  Edinburgh  1865       1.49
    ## 23:     Zurich  1667       1.33
    ## 24:     Oporto  1580       1.26
    ## 25:     Geneva  1572       1.25
    ## 26:     Krakow  1354       1.08
    ## 27:   Helsinki  1228       0.98
    ## 28:       Oslo  1213       0.97
    ## 29: Bratislava  1067       0.85
    ## 30: Luxembourg   657       0.52
    ## 31:  Ljubljana   501       0.40
    ##           City count percentage

## Data Cleaning

``` r
#Split the cuisine style

the.pattern = "'"
the.pattern.start.end= "\\[|\\]"

pattern.inside=","
dat[, eval(cuisine.name) := gsub(pattern = the.pattern, replacement = "", x = get(cuisine.name))]
dat[, eval(cuisine.name) := gsub(pattern = the.pattern.start.end, replacement = "", x = get(cuisine.name))]


newdat= mtabulate(strsplit(as.character(dat[[cuisine.name]]), ","))

finaldat= (cbind(dat, newdat))
finaldat #with binary coding of cuisine styles
```

    ##           V1                         Name      City
    ##      1:    0   Martine of Martine's Table Amsterdam
    ##      2:    1          De Silveren Spiegel Amsterdam
    ##      3:    2                      La Rive Amsterdam
    ##      4:    3                     Vinkeles Amsterdam
    ##      5:    4    Librije's Zusje Amsterdam Amsterdam
    ##     ---                                            
    ## 125523: 1662 Konrad Kaffee- & Cocktailbar    Zurich
    ## 125524: 1663    Blueberry American Bakery    Zurich
    ## 125525: 1664           Restaurant Bahnhof    Zurich
    ## 125526: 1665                   Yoyo Pizza    Zurich
    ## 125527: 1666                        dieci    Zurich
    ##                                                                                                  Cuisine Style
    ##      1:                                                                                French, Dutch, European
    ##      2:                                              Dutch, European, Vegetarian Friendly, Gluten Free Options
    ##      3:                     Mediterranean, French, International, European, Vegetarian Friendly, Vegan Options
    ##      4: French, European, International, Contemporary, Vegetarian Friendly, Vegan Options, Gluten Free Options
    ##      5:                Dutch, European, International, Vegetarian Friendly, Vegan Options, Gluten Free Options
    ##     ---                                                                                                       
    ## 125523:                                                                                                       
    ## 125524:                                                                                                   Cafe
    ## 125525:                                                                                                       
    ## 125526:                                                                                              Fast Food
    ## 125527:                                                                   Italian, Pizza, Mediterranean, Diner
    ##         Ranking Rating Price Range Number of Reviews
    ##      1:       1    5.0    $$ - $$$               136
    ##      2:       2    4.5        $$$$               812
    ##      3:       3    4.5        $$$$               567
    ##      4:       4    5.0        $$$$               564
    ##      5:       5    4.5        $$$$               316
    ##     ---                                             
    ## 125523:      NA     NA                            NA
    ## 125524:      NA     NA                            NA
    ## 125525:      NA     NA                            NA
    ## 125526:      NA     NA                            NA
    ## 125527:      NA     NA    $$ - $$$                NA
    ##                                                                                                            Reviews
    ##      1:                   [['Just like home', 'A Warm Welcome to Wintry Amsterdam'], ['01/03/2018', '01/01/2018']]
    ##      2:                                   [['Great food and staff', 'just perfect'], ['01/06/2018', '01/04/2018']]
    ##      3:                        [['Satisfaction', 'Delicious old school restaurant'], ['01/04/2018', '01/04/2018']]
    ##      4: [['True five star dinner', 'A superb evening of fine dining, hospitali...'], ['12/20/2017', '12/17/2017']]
    ##      5:                            [['Best meal.... EVER', 'super food experience'], ['01/06/2018', '01/04/2018']]
    ##     ---                                                                                                           
    ## 125523:                                                                                                           
    ## 125524:                                                                                                           
    ## 125525:                                                                                                           
    ## 125526:                                                                                                           
    ## 125527:                                                                                                           
    ##                                                                                                                URL_TA
    ##      1: /Restaurant_Review-g188590-d11752080-Reviews-Martine_of_Martine_s_Table-Amsterdam_North_Holland_Province.html
    ##      2:          /Restaurant_Review-g188590-d693419-Reviews-De_Silveren_Spiegel-Amsterdam_North_Holland_Province.html
    ##      3:                      /Restaurant_Review-g188590-d696959-Reviews-La_Rive-Amsterdam_North_Holland_Province.html
    ##      4:                    /Restaurant_Review-g188590-d1239229-Reviews-Vinkeles-Amsterdam_North_Holland_Province.html
    ##      5:   /Restaurant_Review-g188590-d6864170-Reviews-Librije_s_Zusje_Amsterdam-Amsterdam_North_Holland_Province.html
    ##     ---                                                                                                              
    ## 125523:                            /Restaurant_Review-g188113-d13273526-Reviews-Konrad_Kaffee_Cocktailbar-Zurich.html
    ## 125524:                            /Restaurant_Review-g188113-d13292844-Reviews-Blueberry_American_Bakery-Zurich.html
    ## 125525:                                   /Restaurant_Review-g188113-d13296092-Reviews-Restaurant_Bahnhof-Zurich.html
    ## 125526:                                           /Restaurant_Review-g188113-d13323362-Reviews-Yoyo_Pizza-Zurich.html
    ## 125527:                                                /Restaurant_Review-g188113-d13349842-Reviews-Dieci-Zurich.html
    ##             ID_TA  Afghani  African  Albanian  American  Arabic
    ##      1: d11752080        0        0         0         0       0
    ##      2:   d693419        0        0         0         0       0
    ##      3:   d696959        0        0         0         0       0
    ##      4:  d1239229        0        0         0         0       0
    ##      5:  d6864170        0        0         0         0       0
    ##     ---                                                        
    ## 125523: d13273526        0        0         0         0       0
    ## 125524: d13292844        0        0         0         0       0
    ## 125525: d13296092        0        0         0         0       0
    ## 125526: d13323362        0        0         0         0       0
    ## 125527: d13349842        0        0         0         0       0
    ##          Argentinean  Armenian  Asian  Australian  Austrian  Azerbaijani
    ##      1:            0         0      0           0         0            0
    ##      2:            0         0      0           0         0            0
    ##      3:            0         0      0           0         0            0
    ##      4:            0         0      0           0         0            0
    ##      5:            0         0      0           0         0            0
    ##     ---                                                                 
    ## 125523:            0         0      0           0         0            0
    ## 125524:            0         0      0           0         0            0
    ## 125525:            0         0      0           0         0            0
    ## 125526:            0         0      0           0         0            0
    ## 125527:            0         0      0           0         0            0
    ##          Balti  Bangladeshi  Bar  Barbecue  Belgian  Brazilian  Brew Pub
    ##      1:      0            0    0         0        0          0         0
    ##      2:      0            0    0         0        0          0         0
    ##      3:      0            0    0         0        0          0         0
    ##      4:      0            0    0         0        0          0         0
    ##      5:      0            0    0         0        0          0         0
    ##     ---                                                                 
    ## 125523:      0            0    0         0        0          0         0
    ## 125524:      0            0    0         0        0          0         0
    ## 125525:      0            0    0         0        0          0         0
    ## 125526:      0            0    0         0        0          0         0
    ## 125527:      0            0    0         0        0          0         0
    ##          British  Burmese  Cafe  Cajun & Creole  Cambodian  Canadian
    ##      1:        0        0     0               0          0         0
    ##      2:        0        0     0               0          0         0
    ##      3:        0        0     0               0          0         0
    ##      4:        0        0     0               0          0         0
    ##      5:        0        0     0               0          0         0
    ##     ---                                                             
    ## 125523:        0        0     0               0          0         0
    ## 125524:        0        0     0               0          0         0
    ## 125525:        0        0     0               0          0         0
    ## 125526:        0        0     0               0          0         0
    ## 125527:        0        0     0               0          0         0
    ##          Caribbean  Caucasian  Central American  Central Asian
    ##      1:          0          0                 0              0
    ##      2:          0          0                 0              0
    ##      3:          0          0                 0              0
    ##      4:          0          0                 0              0
    ##      5:          0          0                 0              0
    ##     ---                                                       
    ## 125523:          0          0                 0              0
    ## 125524:          0          0                 0              0
    ## 125525:          0          0                 0              0
    ## 125526:          0          0                 0              0
    ## 125527:          0          0                 0              0
    ##          Central European  Chilean  Chinese  Colombian  Contemporary
    ##      1:                 0        0        0          0             0
    ##      2:                 0        0        0          0             0
    ##      3:                 0        0        0          0             0
    ##      4:                 0        0        0          0             1
    ##      5:                 0        0        0          0             0
    ##     ---                                                             
    ## 125523:                 0        0        0          0             0
    ## 125524:                 0        0        0          0             0
    ## 125525:                 0        0        0          0             0
    ## 125526:                 0        0        0          0             0
    ## 125527:                 0        0        0          0             0
    ##          Croatian  Cuban  Czech  Danish  Delicatessen  Diner  Dutch
    ##      1:         0      0      0       0             0      0      1
    ##      2:         0      0      0       0             0      0      0
    ##      3:         0      0      0       0             0      0      0
    ##      4:         0      0      0       0             0      0      0
    ##      5:         0      0      0       0             0      0      0
    ##     ---                                                            
    ## 125523:         0      0      0       0             0      0      0
    ## 125524:         0      0      0       0             0      0      0
    ## 125525:         0      0      0       0             0      0      0
    ## 125526:         0      0      0       0             0      0      0
    ## 125527:         0      0      0       0             0      1      0
    ##          Eastern European  Ecuadorean  Egyptian  Ethiopian  European
    ##      1:                 0           0         0          0         1
    ##      2:                 0           0         0          0         1
    ##      3:                 0           0         0          0         1
    ##      4:                 0           0         0          0         1
    ##      5:                 0           0         0          0         1
    ##     ---                                                             
    ## 125523:                 0           0         0          0         0
    ## 125524:                 0           0         0          0         0
    ## 125525:                 0           0         0          0         0
    ## 125526:                 0           0         0          0         0
    ## 125527:                 0           0         0          0         0
    ##          Fast Food  Filipino  French  Fujian  Fusion  Gastropub  Georgian
    ##      1:          0         0       0       0       0          0         0
    ##      2:          0         0       0       0       0          0         0
    ##      3:          0         0       1       0       0          0         0
    ##      4:          0         0       0       0       0          0         0
    ##      5:          0         0       0       0       0          0         0
    ##     ---                                                                  
    ## 125523:          0         0       0       0       0          0         0
    ## 125524:          0         0       0       0       0          0         0
    ## 125525:          0         0       0       0       0          0         0
    ## 125526:          0         0       0       0       0          0         0
    ## 125527:          0         0       0       0       0          0         0
    ##          German  Gluten Free Options  Greek  Grill  Guatemalan  Halal
    ##      1:       0                    0      0      0           0      0
    ##      2:       0                    1      0      0           0      0
    ##      3:       0                    0      0      0           0      0
    ##      4:       0                    1      0      0           0      0
    ##      5:       0                    1      0      0           0      0
    ##     ---                                                              
    ## 125523:       0                    0      0      0           0      0
    ## 125524:       0                    0      0      0           0      0
    ## 125525:       0                    0      0      0           0      0
    ## 125526:       0                    0      0      0           0      0
    ## 125527:       0                    0      0      0           0      0
    ##          Hawaiian  Healthy  Hungarian  Indian  Indonesian  International
    ##      1:         0        0          0       0           0              0
    ##      2:         0        0          0       0           0              0
    ##      3:         0        0          0       0           0              1
    ##      4:         0        0          0       0           0              1
    ##      5:         0        0          0       0           0              1
    ##     ---                                                                 
    ## 125523:         0        0          0       0           0              0
    ## 125524:         0        0          0       0           0              0
    ## 125525:         0        0          0       0           0              0
    ## 125526:         0        0          0       0           0              0
    ## 125527:         0        0          0       0           0              0
    ##          Irish  Israeli  Italian  Jamaican  Japanese  Korean  Kosher
    ##      1:      0        0        0         0         0       0       0
    ##      2:      0        0        0         0         0       0       0
    ##      3:      0        0        0         0         0       0       0
    ##      4:      0        0        0         0         0       0       0
    ##      5:      0        0        0         0         0       0       0
    ##     ---                                                             
    ## 125523:      0        0        0         0         0       0       0
    ## 125524:      0        0        0         0         0       0       0
    ## 125525:      0        0        0         0         0       0       0
    ## 125526:      0        0        0         0         0       0       0
    ## 125527:      0        0        0         0         0       0       0
    ##          Latin  Latvian  Lebanese  Malaysian  Mediterranean  Mexican
    ##      1:      0        0         0          0              0        0
    ##      2:      0        0         0          0              0        0
    ##      3:      0        0         0          0              0        0
    ##      4:      0        0         0          0              0        0
    ##      5:      0        0         0          0              0        0
    ##     ---                                                             
    ## 125523:      0        0         0          0              0        0
    ## 125524:      0        0         0          0              0        0
    ## 125525:      0        0         0          0              0        0
    ## 125526:      0        0         0          0              0        0
    ## 125527:      0        0         0          0              1        0
    ##          Middle Eastern  Minority Chinese  Mongolian  Moroccan
    ##      1:               0                 0          0         0
    ##      2:               0                 0          0         0
    ##      3:               0                 0          0         0
    ##      4:               0                 0          0         0
    ##      5:               0                 0          0         0
    ##     ---                                                       
    ## 125523:               0                 0          0         0
    ## 125524:               0                 0          0         0
    ## 125525:               0                 0          0         0
    ## 125526:               0                 0          0         0
    ## 125527:               0                 0          0         0
    ##          Native American  Nepali  New Zealand  Norwegian  Pakistani
    ##      1:                0       0            0          0          0
    ##      2:                0       0            0          0          0
    ##      3:                0       0            0          0          0
    ##      4:                0       0            0          0          0
    ##      5:                0       0            0          0          0
    ##     ---                                                            
    ## 125523:                0       0            0          0          0
    ## 125524:                0       0            0          0          0
    ## 125525:                0       0            0          0          0
    ## 125526:                0       0            0          0          0
    ## 125527:                0       0            0          0          0
    ##          Persian  Peruvian  Pizza  Polish  Polynesian  Portuguese  Pub
    ##      1:        0         0      0       0           0           0    0
    ##      2:        0         0      0       0           0           0    0
    ##      3:        0         0      0       0           0           0    0
    ##      4:        0         0      0       0           0           0    0
    ##      5:        0         0      0       0           0           0    0
    ##     ---                                                               
    ## 125523:        0         0      0       0           0           0    0
    ## 125524:        0         0      0       0           0           0    0
    ## 125525:        0         0      0       0           0           0    0
    ## 125526:        0         0      0       0           0           0    0
    ## 125527:        0         0      1       0           0           0    0
    ##          Puerto Rican  Romanian  Russian  Salvadoran  Scandinavian
    ##      1:             0         0        0           0             0
    ##      2:             0         0        0           0             0
    ##      3:             0         0        0           0             0
    ##      4:             0         0        0           0             0
    ##      5:             0         0        0           0             0
    ##     ---                                                           
    ## 125523:             0         0        0           0             0
    ## 125524:             0         0        0           0             0
    ## 125525:             0         0        0           0             0
    ## 125526:             0         0        0           0             0
    ## 125527:             0         0        0           0             0
    ##          Scottish  Seafood  Singaporean  Slovenian  Soups  South American
    ##      1:         0        0            0          0      0               0
    ##      2:         0        0            0          0      0               0
    ##      3:         0        0            0          0      0               0
    ##      4:         0        0            0          0      0               0
    ##      5:         0        0            0          0      0               0
    ##     ---                                                                  
    ## 125523:         0        0            0          0      0               0
    ## 125524:         0        0            0          0      0               0
    ## 125525:         0        0            0          0      0               0
    ## 125526:         0        0            0          0      0               0
    ## 125527:         0        0            0          0      0               0
    ##          Southwestern  Spanish  Sri Lankan  Steakhouse  Street Food  Sushi
    ##      1:             0        0           0           0            0      0
    ##      2:             0        0           0           0            0      0
    ##      3:             0        0           0           0            0      0
    ##      4:             0        0           0           0            0      0
    ##      5:             0        0           0           0            0      0
    ##     ---                                                                   
    ## 125523:             0        0           0           0            0      0
    ## 125524:             0        0           0           0            0      0
    ## 125525:             0        0           0           0            0      0
    ## 125526:             0        0           0           0            0      0
    ## 125527:             0        0           0           0            0      0
    ##          Swedish  Swiss  Taiwanese  Thai  Tibetan  Tunisian  Turkish
    ##      1:        0      0          0     0        0         0        0
    ##      2:        0      0          0     0        0         0        0
    ##      3:        0      0          0     0        0         0        0
    ##      4:        0      0          0     0        0         0        0
    ##      5:        0      0          0     0        0         0        0
    ##     ---                                                             
    ## 125523:        0      0          0     0        0         0        0
    ## 125524:        0      0          0     0        0         0        0
    ## 125525:        0      0          0     0        0         0        0
    ## 125526:        0      0          0     0        0         0        0
    ## 125527:        0      0          0     0        0         0        0
    ##          Ukrainian  Uzbek  Vegan Options  Vegetarian Friendly  Venezuelan
    ##      1:          0      0              0                    0           0
    ##      2:          0      0              0                    1           0
    ##      3:          0      0              1                    1           0
    ##      4:          0      0              1                    1           0
    ##      5:          0      0              1                    1           0
    ##     ---                                                                  
    ## 125523:          0      0              0                    0           0
    ## 125524:          0      0              0                    0           0
    ## 125525:          0      0              0                    0           0
    ## 125526:          0      0              0                    0           0
    ## 125527:          0      0              0                    0           0
    ##          Vietnamese  Welsh  Wine Bar  Xinjiang  Yunnan Afghani African
    ##      1:           0      0         0         0       0       0       0
    ##      2:           0      0         0         0       0       0       0
    ##      3:           0      0         0         0       0       0       0
    ##      4:           0      0         0         0       0       0       0
    ##      5:           0      0         0         0       0       0       0
    ##     ---                                                               
    ## 125523:           0      0         0         0       0       0       0
    ## 125524:           0      0         0         0       0       0       0
    ## 125525:           0      0         0         0       0       0       0
    ## 125526:           0      0         0         0       0       0       0
    ## 125527:           0      0         0         0       0       0       0
    ##         American Arabic Argentinean Armenian Asian Australian Austrian
    ##      1:        0      0           0        0     0          0        0
    ##      2:        0      0           0        0     0          0        0
    ##      3:        0      0           0        0     0          0        0
    ##      4:        0      0           0        0     0          0        0
    ##      5:        0      0           0        0     0          0        0
    ##     ---                                                               
    ## 125523:        0      0           0        0     0          0        0
    ## 125524:        0      0           0        0     0          0        0
    ## 125525:        0      0           0        0     0          0        0
    ## 125526:        0      0           0        0     0          0        0
    ## 125527:        0      0           0        0     0          0        0
    ##         Balti Bangladeshi Bar Barbecue Belgian Brazilian Brew Pub British
    ##      1:     0           0   0        0       0         0        0       0
    ##      2:     0           0   0        0       0         0        0       0
    ##      3:     0           0   0        0       0         0        0       0
    ##      4:     0           0   0        0       0         0        0       0
    ##      5:     0           0   0        0       0         0        0       0
    ##     ---                                                                  
    ## 125523:     0           0   0        0       0         0        0       0
    ## 125524:     0           0   0        0       0         0        0       0
    ## 125525:     0           0   0        0       0         0        0       0
    ## 125526:     0           0   0        0       0         0        0       0
    ## 125527:     0           0   0        0       0         0        0       0
    ##         Cafe Cajun & Creole Cambodian Canadian Caribbean Caucasian
    ##      1:    0              0         0        0         0         0
    ##      2:    0              0         0        0         0         0
    ##      3:    0              0         0        0         0         0
    ##      4:    0              0         0        0         0         0
    ##      5:    0              0         0        0         0         0
    ##     ---                                                           
    ## 125523:    0              0         0        0         0         0
    ## 125524:    1              0         0        0         0         0
    ## 125525:    0              0         0        0         0         0
    ## 125526:    0              0         0        0         0         0
    ## 125527:    0              0         0        0         0         0
    ##         Central American Central Asian Central European Chilean Chinese
    ##      1:                0             0                0       0       0
    ##      2:                0             0                0       0       0
    ##      3:                0             0                0       0       0
    ##      4:                0             0                0       0       0
    ##      5:                0             0                0       0       0
    ##     ---                                                                
    ## 125523:                0             0                0       0       0
    ## 125524:                0             0                0       0       0
    ## 125525:                0             0                0       0       0
    ## 125526:                0             0                0       0       0
    ## 125527:                0             0                0       0       0
    ##         Colombian Contemporary Croatian Cuban Czech Danish Delicatessen
    ##      1:         0            0        0     0     0      0            0
    ##      2:         0            0        0     0     0      0            0
    ##      3:         0            0        0     0     0      0            0
    ##      4:         0            0        0     0     0      0            0
    ##      5:         0            0        0     0     0      0            0
    ##     ---                                                                
    ## 125523:         0            0        0     0     0      0            0
    ## 125524:         0            0        0     0     0      0            0
    ## 125525:         0            0        0     0     0      0            0
    ## 125526:         0            0        0     0     0      0            0
    ## 125527:         0            0        0     0     0      0            0
    ##         Diner Dutch Eastern European Ecuadorean Ethiopian European
    ##      1:     0     0                0          0         0        0
    ##      2:     0     1                0          0         0        0
    ##      3:     0     0                0          0         0        0
    ##      4:     0     0                0          0         0        0
    ##      5:     0     1                0          0         0        0
    ##     ---                                                           
    ## 125523:     0     0                0          0         0        0
    ## 125524:     0     0                0          0         0        0
    ## 125525:     0     0                0          0         0        0
    ## 125526:     0     0                0          0         0        0
    ## 125527:     0     0                0          0         0        0
    ##         Fast Food Filipino French Fusion Gastropub Georgian German
    ##      1:         0        0      1      0         0        0      0
    ##      2:         0        0      0      0         0        0      0
    ##      3:         0        0      0      0         0        0      0
    ##      4:         0        0      1      0         0        0      0
    ##      5:         0        0      0      0         0        0      0
    ##     ---                                                           
    ## 125523:         0        0      0      0         0        0      0
    ## 125524:         0        0      0      0         0        0      0
    ## 125525:         0        0      0      0         0        0      0
    ## 125526:         1        0      0      0         0        0      0
    ## 125527:         0        0      0      0         0        0      0
    ##         Gluten Free Options Greek Grill Halal Hawaiian Healthy Hungarian
    ##      1:                   0     0     0     0        0       0         0
    ##      2:                   0     0     0     0        0       0         0
    ##      3:                   0     0     0     0        0       0         0
    ##      4:                   0     0     0     0        0       0         0
    ##      5:                   0     0     0     0        0       0         0
    ##     ---                                                                 
    ## 125523:                   0     0     0     0        0       0         0
    ## 125524:                   0     0     0     0        0       0         0
    ## 125525:                   0     0     0     0        0       0         0
    ## 125526:                   0     0     0     0        0       0         0
    ## 125527:                   0     0     0     0        0       0         0
    ##         Indian Indonesian International Irish Israeli Italian Jamaican
    ##      1:      0          0             0     0       0       0        0
    ##      2:      0          0             0     0       0       0        0
    ##      3:      0          0             0     0       0       0        0
    ##      4:      0          0             0     0       0       0        0
    ##      5:      0          0             0     0       0       0        0
    ##     ---                                                               
    ## 125523:      0          0             0     0       0       0        0
    ## 125524:      0          0             0     0       0       0        0
    ## 125525:      0          0             0     0       0       0        0
    ## 125526:      0          0             0     0       0       0        0
    ## 125527:      0          0             0     0       0       1        0
    ##         Japanese Korean Kosher Latin Lebanese Malaysian Mediterranean
    ##      1:        0      0      0     0        0         0             0
    ##      2:        0      0      0     0        0         0             0
    ##      3:        0      0      0     0        0         0             1
    ##      4:        0      0      0     0        0         0             0
    ##      5:        0      0      0     0        0         0             0
    ##     ---                                                              
    ## 125523:        0      0      0     0        0         0             0
    ## 125524:        0      0      0     0        0         0             0
    ## 125525:        0      0      0     0        0         0             0
    ## 125526:        0      0      0     0        0         0             0
    ## 125527:        0      0      0     0        0         0             0
    ##         Mexican Middle Eastern Mongolian Moroccan Nepali New Zealand
    ##      1:       0              0         0        0      0           0
    ##      2:       0              0         0        0      0           0
    ##      3:       0              0         0        0      0           0
    ##      4:       0              0         0        0      0           0
    ##      5:       0              0         0        0      0           0
    ##     ---                                                             
    ## 125523:       0              0         0        0      0           0
    ## 125524:       0              0         0        0      0           0
    ## 125525:       0              0         0        0      0           0
    ## 125526:       0              0         0        0      0           0
    ## 125527:       0              0         0        0      0           0
    ##         Norwegian Pakistani Persian Peruvian Pizza Polish Polynesian
    ##      1:         0         0       0        0     0      0          0
    ##      2:         0         0       0        0     0      0          0
    ##      3:         0         0       0        0     0      0          0
    ##      4:         0         0       0        0     0      0          0
    ##      5:         0         0       0        0     0      0          0
    ##     ---                                                             
    ## 125523:         0         0       0        0     0      0          0
    ## 125524:         0         0       0        0     0      0          0
    ## 125525:         0         0       0        0     0      0          0
    ## 125526:         0         0       0        0     0      0          0
    ## 125527:         0         0       0        0     0      0          0
    ##         Portuguese Pub Romanian Russian Salvadoran Scandinavian Scottish
    ##      1:          0   0        0       0          0            0        0
    ##      2:          0   0        0       0          0            0        0
    ##      3:          0   0        0       0          0            0        0
    ##      4:          0   0        0       0          0            0        0
    ##      5:          0   0        0       0          0            0        0
    ##     ---                                                                 
    ## 125523:          0   0        0       0          0            0        0
    ## 125524:          0   0        0       0          0            0        0
    ## 125525:          0   0        0       0          0            0        0
    ## 125526:          0   0        0       0          0            0        0
    ## 125527:          0   0        0       0          0            0        0
    ##         Seafood Singaporean Slovenian Soups South American Southwestern
    ##      1:       0           0         0     0              0            0
    ##      2:       0           0         0     0              0            0
    ##      3:       0           0         0     0              0            0
    ##      4:       0           0         0     0              0            0
    ##      5:       0           0         0     0              0            0
    ##     ---                                                                
    ## 125523:       0           0         0     0              0            0
    ## 125524:       0           0         0     0              0            0
    ## 125525:       0           0         0     0              0            0
    ## 125526:       0           0         0     0              0            0
    ## 125527:       0           0         0     0              0            0
    ##         Spanish Sri Lankan Steakhouse Street Food Sushi Swedish Swiss
    ##      1:       0          0          0           0     0       0     0
    ##      2:       0          0          0           0     0       0     0
    ##      3:       0          0          0           0     0       0     0
    ##      4:       0          0          0           0     0       0     0
    ##      5:       0          0          0           0     0       0     0
    ##     ---                                                              
    ## 125523:       0          0          0           0     0       0     0
    ## 125524:       0          0          0           0     0       0     0
    ## 125525:       0          0          0           0     0       0     0
    ## 125526:       0          0          0           0     0       0     0
    ## 125527:       0          0          0           0     0       0     0
    ##         Taiwanese Thai Tibetan Tunisian Turkish Ukrainian Uzbek
    ##      1:         0    0       0        0       0         0     0
    ##      2:         0    0       0        0       0         0     0
    ##      3:         0    0       0        0       0         0     0
    ##      4:         0    0       0        0       0         0     0
    ##      5:         0    0       0        0       0         0     0
    ##     ---                                                        
    ## 125523:         0    0       0        0       0         0     0
    ## 125524:         0    0       0        0       0         0     0
    ## 125525:         0    0       0        0       0         0     0
    ## 125526:         0    0       0        0       0         0     0
    ## 125527:         0    0       0        0       0         0     0
    ##         Vegan Options Vegetarian Friendly Venezuelan Vietnamese Wine Bar
    ##      1:             0                   0          0          0        0
    ##      2:             0                   0          0          0        0
    ##      3:             0                   0          0          0        0
    ##      4:             0                   0          0          0        0
    ##      5:             0                   0          0          0        0
    ##     ---                                                                 
    ## 125523:             0                   0          0          0        0
    ## 125524:             0                   0          0          0        0
    ## 125525:             0                   0          0          0        0
    ## 125526:             0                   0          0          0        0
    ## 125527:             0                   0          0          0        0

``` r
new.price.col <- replace.value(data = finaldat, colname = price.name)
```

    ##   [1] 2.5 4.0 4.0 4.0 4.0 4.0 2.5 4.0 4.0 1.0 2.5 2.5 4.0 2.5 4.0 2.5 1.0
    ##  [18] 4.0 2.5 2.5 4.0 4.0 2.5 0.0 2.5 2.5 4.0 1.0 2.5 2.5 2.5 2.5 2.5 4.0
    ##  [35] 2.5 1.0 1.0 1.0 2.5 2.5 4.0 2.5 2.5 2.5 1.0 1.0 1.0 1.0 2.5 1.0 2.5
    ##  [52] 2.5 2.5 4.0 1.0 2.5 4.0 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 4.0 2.5 1.0
    ##  [69] 2.5 2.5 2.5 1.0 1.0 2.5 2.5 1.0 2.5 2.5 1.0 1.0 2.5 2.5 4.0 2.5 2.5
    ##  [86] 1.0 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 1.0 2.5 2.5 2.5 2.5 2.5
    ## [103] 4.0 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 4.0 4.0
    ## [120] 2.5 2.5 2.5 2.5 2.5 1.0 4.0 2.5 2.5 2.5 1.0 2.5 1.0 2.5 2.5 2.5 2.5
    ## [137] 2.5 2.5 2.5 1.0 1.0 2.5 2.5 4.0 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5
    ## [154] 2.5 2.5 2.5 2.5 4.0 4.0 2.5 2.5 2.5 2.5 1.0 4.0 2.5 1.0 2.5 1.0 2.5
    ## [171] 2.5 4.0 2.5 2.5 1.0 0.0 2.5 2.5 1.0 2.5 1.0 2.5 2.5 4.0 4.0 2.5 2.5
    ## [188] 2.5 4.0 4.0 2.5 2.5 2.5 2.5 2.5 1.0 2.5 2.5 2.5 2.5 2.5 2.5 1.0 2.5
    ## [205] 2.5 2.5 1.0 2.5 2.5 2.5 2.5 2.5 2.5 2.5 1.0 2.5 2.5 1.0 4.0 2.5 1.0
    ## [222] 2.5 2.5 2.5 2.5 4.0 2.5 2.5 2.5 2.5 2.5 2.5 2.5 1.0 2.5 4.0 2.5 2.5
    ## [239] 4.0 1.0 2.5 2.5 2.5 2.5 2.5 2.5 1.0 2.5 2.5 2.5 2.5 2.5

``` r
finaldat$new_price <- new.price.col

names(finaldat)
```

    ##   [1] "V1"                   "Name"                 "City"                
    ##   [4] "Cuisine Style"        "Ranking"              "Rating"              
    ##   [7] "Price Range"          "Number of Reviews"    "Reviews"             
    ##  [10] "URL_TA"               "ID_TA"                " Afghani"            
    ##  [13] " African"             " Albanian"            " American"           
    ##  [16] " Arabic"              " Argentinean"         " Armenian"           
    ##  [19] " Asian"               " Australian"          " Austrian"           
    ##  [22] " Azerbaijani"         " Balti"               " Bangladeshi"        
    ##  [25] " Bar"                 " Barbecue"            " Belgian"            
    ##  [28] " Brazilian"           " Brew Pub"            " British"            
    ##  [31] " Burmese"             " Cafe"                " Cajun & Creole"     
    ##  [34] " Cambodian"           " Canadian"            " Caribbean"          
    ##  [37] " Caucasian"           " Central American"    " Central Asian"      
    ##  [40] " Central European"    " Chilean"             " Chinese"            
    ##  [43] " Colombian"           " Contemporary"        " Croatian"           
    ##  [46] " Cuban"               " Czech"               " Danish"             
    ##  [49] " Delicatessen"        " Diner"               " Dutch"              
    ##  [52] " Eastern European"    " Ecuadorean"          " Egyptian"           
    ##  [55] " Ethiopian"           " European"            " Fast Food"          
    ##  [58] " Filipino"            " French"              " Fujian"             
    ##  [61] " Fusion"              " Gastropub"           " Georgian"           
    ##  [64] " German"              " Gluten Free Options" " Greek"              
    ##  [67] " Grill"               " Guatemalan"          " Halal"              
    ##  [70] " Hawaiian"            " Healthy"             " Hungarian"          
    ##  [73] " Indian"              " Indonesian"          " International"      
    ##  [76] " Irish"               " Israeli"             " Italian"            
    ##  [79] " Jamaican"            " Japanese"            " Korean"             
    ##  [82] " Kosher"              " Latin"               " Latvian"            
    ##  [85] " Lebanese"            " Malaysian"           " Mediterranean"      
    ##  [88] " Mexican"             " Middle Eastern"      " Minority Chinese"   
    ##  [91] " Mongolian"           " Moroccan"            " Native American"    
    ##  [94] " Nepali"              " New Zealand"         " Norwegian"          
    ##  [97] " Pakistani"           " Persian"             " Peruvian"           
    ## [100] " Pizza"               " Polish"              " Polynesian"         
    ## [103] " Portuguese"          " Pub"                 " Puerto Rican"       
    ## [106] " Romanian"            " Russian"             " Salvadoran"         
    ## [109] " Scandinavian"        " Scottish"            " Seafood"            
    ## [112] " Singaporean"         " Slovenian"           " Soups"              
    ## [115] " South American"      " Southwestern"        " Spanish"            
    ## [118] " Sri Lankan"          " Steakhouse"          " Street Food"        
    ## [121] " Sushi"               " Swedish"             " Swiss"              
    ## [124] " Taiwanese"           " Thai"                " Tibetan"            
    ## [127] " Tunisian"            " Turkish"             " Ukrainian"          
    ## [130] " Uzbek"               " Vegan Options"       " Vegetarian Friendly"
    ## [133] " Venezuelan"          " Vietnamese"          " Welsh"              
    ## [136] " Wine Bar"            " Xinjiang"            " Yunnan"             
    ## [139] "Afghani"              "African"              "American"            
    ## [142] "Arabic"               "Argentinean"          "Armenian"            
    ## [145] "Asian"                "Australian"           "Austrian"            
    ## [148] "Balti"                "Bangladeshi"          "Bar"                 
    ## [151] "Barbecue"             "Belgian"              "Brazilian"           
    ## [154] "Brew Pub"             "British"              "Cafe"                
    ## [157] "Cajun & Creole"       "Cambodian"            "Canadian"            
    ## [160] "Caribbean"            "Caucasian"            "Central American"    
    ## [163] "Central Asian"        "Central European"     "Chilean"             
    ## [166] "Chinese"              "Colombian"            "Contemporary"        
    ## [169] "Croatian"             "Cuban"                "Czech"               
    ## [172] "Danish"               "Delicatessen"         "Diner"               
    ## [175] "Dutch"                "Eastern European"     "Ecuadorean"          
    ## [178] "Ethiopian"            "European"             "Fast Food"           
    ## [181] "Filipino"             "French"               "Fusion"              
    ## [184] "Gastropub"            "Georgian"             "German"              
    ## [187] "Gluten Free Options"  "Greek"                "Grill"               
    ## [190] "Halal"                "Hawaiian"             "Healthy"             
    ## [193] "Hungarian"            "Indian"               "Indonesian"          
    ## [196] "International"        "Irish"                "Israeli"             
    ## [199] "Italian"              "Jamaican"             "Japanese"            
    ## [202] "Korean"               "Kosher"               "Latin"               
    ## [205] "Lebanese"             "Malaysian"            "Mediterranean"       
    ## [208] "Mexican"              "Middle Eastern"       "Mongolian"           
    ## [211] "Moroccan"             "Nepali"               "New Zealand"         
    ## [214] "Norwegian"            "Pakistani"            "Persian"             
    ## [217] "Peruvian"             "Pizza"                "Polish"              
    ## [220] "Polynesian"           "Portuguese"           "Pub"                 
    ## [223] "Romanian"             "Russian"              "Salvadoran"          
    ## [226] "Scandinavian"         "Scottish"             "Seafood"             
    ## [229] "Singaporean"          "Slovenian"            "Soups"               
    ## [232] "South American"       "Southwestern"         "Spanish"             
    ## [235] "Sri Lankan"           "Steakhouse"           "Street Food"         
    ## [238] "Sushi"                "Swedish"              "Swiss"               
    ## [241] "Taiwanese"            "Thai"                 "Tibetan"             
    ## [244] "Tunisian"             "Turkish"              "Ukrainian"           
    ## [247] "Uzbek"                "Vegan Options"        "Vegetarian Friendly" 
    ## [250] "Venezuelan"           "Vietnamese"           "Wine Bar"            
    ## [253] "new_price"

``` r
calculations_rating= dat[,.( `Mean Rating`=mean(get(rating.name), na.rm=TRUE), `Standard Deviation`=sd(get(rating.name), na.rm=TRUE))]
calculations_rating
```

    ##    Mean Rating Standard Deviation
    ## 1:    3.987441          0.6788136

``` r
#Mean by city name
calculations_rating_city= dat[,.( `Mean Rating`=mean(get(rating.name), na.rm=TRUE), `Standard Deviation`=sd(get(rating.name), na.rm=TRUE)), by=city.name]
calculations_rating_city
```

    ##           City Mean Rating Standard Deviation
    ##  1:  Amsterdam    4.118381          0.6150341
    ##  2:     Athens    4.207774          0.6112002
    ##  3:  Barcelona    3.966829          0.7167667
    ##  4:     Berlin    4.127020          0.6372915
    ##  5: Bratislava    3.989314          0.7906752
    ##  6:   Brussels    3.890106          0.6722238
    ##  7:   Budapest    4.095854          0.6835912
    ##  8: Copenhagen    3.994670          0.7021313
    ##  9:     Dublin    4.051151          0.6073943
    ## 10:  Edinburgh    4.056818          0.6700862
    ## 11:     Geneva    3.969482          0.6435805
    ## 12:    Hamburg    4.030508          0.6892296
    ## 13:   Helsinki    3.908813          0.6523193
    ## 14:     Krakow    4.128812          0.6781594
    ## 15:     Lisbon    4.052128          0.6447481
    ## 16:  Ljubljana    4.128205          0.5776117
    ## 17:     London    3.942896          0.7043545
    ## 18: Luxembourg    3.909310          0.6180901
    ## 19:       Lyon    3.920382          0.7375481
    ## 20:     Madrid    3.796698          0.7469557
    ## 21:      Milan    3.808955          0.6654761
    ## 22:     Munich    4.027525          0.6170954
    ## 23:     Oporto    4.152145          0.6131048
    ## 24:       Oslo    3.899385          0.6337958
    ## 25:      Paris    3.948714          0.6639771
    ## 26:     Prague    4.013423          0.7397098
    ## 27:       Rome    4.232140          0.4428341
    ## 28:  Stockholm    3.873528          0.6590475
    ## 29:     Vienna    4.067984          0.6415099
    ## 30:     Warsaw    4.067102          0.6671923
    ## 31:     Zurich    4.018495          0.6151600
    ##           City Mean Rating Standard Deviation

``` r
#Median by city name
calculations_rating_city= dat[,.( `Median Rating`=median(get(rating.name), na.rm=TRUE)), by=city.name]
calculations_rating_city
```

    ##           City Median Rating
    ##  1:  Amsterdam           4.0
    ##  2:     Athens           4.5
    ##  3:  Barcelona           4.0
    ##  4:     Berlin           4.0
    ##  5: Bratislava           4.0
    ##  6:   Brussels           4.0
    ##  7:   Budapest           4.0
    ##  8: Copenhagen           4.0
    ##  9:     Dublin           4.0
    ## 10:  Edinburgh           4.0
    ## 11:     Geneva           4.0
    ## 12:    Hamburg           4.0
    ## 13:   Helsinki           4.0
    ## 14:     Krakow           4.0
    ## 15:     Lisbon           4.0
    ## 16:  Ljubljana           4.0
    ## 17:     London           4.0
    ## 18: Luxembourg           4.0
    ## 19:       Lyon           4.0
    ## 20:     Madrid           4.0
    ## 21:      Milan           4.0
    ## 22:     Munich           4.0
    ## 23:     Oporto           4.0
    ## 24:       Oslo           4.0
    ## 25:      Paris           4.0
    ## 26:     Prague           4.0
    ## 27:       Rome           4.0
    ## 28:  Stockholm           4.0
    ## 29:     Vienna           4.0
    ## 30:     Warsaw           4.0
    ## 31:     Zurich           4.0
    ##           City Median Rating

``` r
ggplot(data=dat,aes(x=Rating))+
  geom_histogram(fill='blue')+
  theme_economist()
```

![](Frameworks_II_Project_files/figure-gfm/text%20analysis%20looking%20at%20data-1.png)<!-- -->

``` r
#Correlation between review rating and longer reviews
cor_char=cor(nchar(dat$Reviews),dat$Rating,use="complete.obs")
cor_char
```

    ## [1] 0.03120432

``` r
cor.test(nchar(dat$Reviews),dat$Rating)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  nchar(dat$Reviews) and dat$Rating
    ## t = 10.628, df = 115900, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.02545168 0.03695491
    ## sample estimates:
    ##        cor 
    ## 0.03120432

``` r
#Correlation with review length in words
cor_words= cor(str_count(string = dat$Reviews,pattern = '\\S+'),dat$Rating,use="complete.obs")
cor_words
```

    ## [1] 0.01431083

``` r
cor.test(str_count(string = dat$Reviews,pattern = '\\S+'),dat$Rating)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  str_count(string = dat$Reviews, pattern = "\\S+") and dat$Rating
    ## t = 4.8724, df = 115900, p-value = 1.104e-06
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.008554316 0.020066398
    ## sample estimates:
    ##        cor 
    ## 0.01431083

``` r
#Correlation with review sentence
cor_sentence= cor(str_count(string = dat$Reviews,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]"),dat$Rating,use="complete.obs")
cor_sentence
```

    ## [1] 0.03700835

``` r
cor.test(str_count(string = dat$Reviews,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]"),dat$Rating,use="complete.obs")
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  str_count(string = dat$Reviews, pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]") and dat$Rating
    ## t = 12.608, df = 115900, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.03125779 0.04275646
    ## sample estimates:
    ##        cor 
    ## 0.03700835

``` r
#Using lexicon bing
subdat= dat[,c("V1", "Rating", "Reviews")]
subdat%>%
  group_by(V1)%>%
  unnest_tokens(output = word, input = Reviews)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)
```

    ## # A tibble: 174,324 x 4
    ## # Groups:   sentiment [2]
    ##       V1 Rating word      sentiment
    ##    <int>  <dbl> <chr>     <chr>    
    ##  1     0    5   like      positive 
    ##  2     0    5   warm      positive 
    ##  3     0    5   welcome   positive 
    ##  4     1    4.5 great     positive 
    ##  5     1    4.5 perfect   positive 
    ##  6     2    4.5 delicious positive 
    ##  7     3    5   superb    positive 
    ##  8     3    5   fine      positive 
    ##  9     4    4.5 best      positive 
    ## 10     4    4.5 super     positive 
    ## # ... with 174,314 more rows

``` r
#Positive and Negative Words in Reviews
subdat%>%
  group_by(V1)%>%
  unnest_tokens(output = word, input = Reviews)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=sentiment,y=n,fill=sentiment))+geom_col()+theme_economist()+guides(fill=F)
```

![](Frameworks_II_Project_files/figure-gfm/text%20analysis%20sentiment%20bing%20lexicon-1.png)<!-- -->

``` r
#Proportion of Positive words in Reviews
subdat %>%
  select(V1,Reviews)%>%
  group_by(V1)%>%
  unnest_tokens(output=word,input=Reviews)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))
```

    ## # A tibble: 2 x 3
    ##   sentiment      n proportion
    ##   <chr>      <int>      <dbl>
    ## 1 negative   21093      0.121
    ## 2 positive  153231      0.879

``` r
#Are positive reviews helpful?
subdat %>%
  select(V1,Reviews,Rating)%>%
  group_by(V1)%>%
  unnest_tokens(output=word,input=Reviews)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(Rating,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))%>%
  ggplot(aes(x=Rating,y=proportion,fill=sentiment))+geom_col()+theme_economist()
```

![](Frameworks_II_Project_files/figure-gfm/text%20analysis%20sentiment%20bing%20lexicon-2.png)<!-- -->

``` r
#Emotions in ratings
subdat%>%
  group_by(V1)%>%
  unnest_tokens(output = word, input = Reviews)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=reorder(sentiment,X = n),y=n,fill=sentiment))+geom_col()+guides(fill=F)+coord_flip()+theme_wsj()
```

![](Frameworks_II_Project_files/figure-gfm/text%20analysis%20sentiment%20nrc%20lexicon-1.png)<!-- -->

``` r
#Rating and emotions
subdat%>%
  group_by(V1)%>%
  unnest_tokens(output = word, input = Reviews)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(V1,sentiment,Rating)%>%
  count()
```

    ## # A tibble: 175,678 x 4
    ## # Groups:   V1, sentiment, Rating [175,678]
    ##       V1 sentiment    Rating     n
    ##    <int> <chr>         <dbl> <int>
    ##  1     0 anger           4.5     1
    ##  2     0 anticipation    4.5     4
    ##  3     0 anticipation    5       8
    ##  4     0 disgust         4.5     1
    ##  5     0 fear            4.5     1
    ##  6     0 fear            5       3
    ##  7     0 joy             4.5     7
    ##  8     0 joy             5      22
    ##  9     0 negative        4.5     1
    ## 10     0 negative        5       1
    ## # ... with 175,668 more rows

``` r
#Correlation between emotion and rating
subdat%>%
  group_by(V1)%>%
  unnest_tokens(output = word, input = Reviews)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(V1,sentiment,Rating)%>%
  count()%>%
  ungroup()%>%
  group_by(sentiment)%>%
  summarize(correlation = cor(n,Rating))
```

    ## # A tibble: 10 x 2
    ##    sentiment    correlation
    ##    <chr>              <dbl>
    ##  1 anger             0.0191
    ##  2 anticipation      0.165 
    ##  3 disgust          -0.0260
    ##  4 fear             NA     
    ##  5 joy              NA     
    ##  6 negative          0.0752
    ##  7 positive         NA     
    ##  8 sadness           0.112 
    ##  9 surprise         NA     
    ## 10 trust            NA

``` r
subdat %>%
  select(V1,Reviews)%>%
  group_by(V1)%>%
  unnest_tokens(output=word,input=Reviews)%>%
  inner_join(get_sentiments('afinn'))%>%
  summarize(reviewSentiment = mean(score))%>%
  ungroup()%>%
  summarize(min=min(reviewSentiment),max=max(reviewSentiment),median=median(reviewSentiment),mean=mean(reviewSentiment))
```

    ## # A tibble: 1 x 4
    ##     min   max median  mean
    ##   <dbl> <dbl>  <dbl> <dbl>
    ## 1    -3     5    2.5  2.06

``` r
#Distribution of afinn lexicon scores
subdat %>%
  select(V1,Reviews)%>%
  group_by(V1)%>%
  unnest_tokens(output=word,input=Reviews)%>%
  inner_join(get_sentiments('afinn'))%>%
  summarize(reviewSentiment = mean(score))%>%
  ungroup()%>%
  ggplot(aes(x=reviewSentiment,fill=reviewSentiment>0))+
  geom_histogram(binwidth = 0.1)+
  scale_x_continuous(breaks=seq(-5,5,1))+scale_fill_manual(values=c('tomato','seagreen'))+
  guides(fill=F)+
  theme_wsj()
```

![](Frameworks_II_Project_files/figure-gfm/text%20analysis%20sentiment%20affin%20lexicon-1.png)<!-- -->

``` r
subdat= dat[,c("City", "Rating", "Reviews")]

#Positive words and negative words in different cities
subdat%>%
  group_by(City)%>%
  unnest_tokens(output = word, input = Reviews)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(City, sentiment)%>%
  count()%>%
  ggplot(aes(x=City,y=n,fill=sentiment))+geom_col()+theme_economist()+guides(fill=F)+ coord_flip()
```

![](Frameworks_II_Project_files/figure-gfm/text%20analysis%20by%20city-1.png)<!-- -->

``` r
#Similar distribution of emotions
subdat%>%
  group_by(City)%>%
  unnest_tokens(output = word, input = Reviews)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment, City)%>%
  count()%>%
  ggplot(aes(x=City,y=n,fill=sentiment))+geom_bar(position = "fill", stat='identity')+guides(fill=F)+coord_flip()+theme_wsj()
```

![](Frameworks_II_Project_files/figure-gfm/text%20analysis%20by%20city-2.png)<!-- -->

``` r
#Sentiment mean, median, max and min 
subdat %>%
  select(City,Reviews)%>%
  group_by(City)%>%
  unnest_tokens(output=word,input=Reviews)%>%
  inner_join(get_sentiments('afinn'))%>%
  summarize(reviewMeanSentiment = mean(score), reviewMedianSentiment = median(score), reviewMaxSentiment = max(score),reviewMinSentiment = min(score))
```

    ## # A tibble: 31 x 5
    ##    City  reviewMeanSenti~ reviewMedianSen~ reviewMaxSentim~
    ##    <chr>            <dbl>            <dbl>            <dbl>
    ##  1 Amst~             2.52                3                5
    ##  2 Athe~             2.56                3                5
    ##  3 Barc~             2.39                3                5
    ##  4 Berl~             2.46                3                5
    ##  5 Brat~             2.26                3                5
    ##  6 Brus~             2.27                3                5
    ##  7 Buda~             2.34                3                5
    ##  8 Cope~             2.47                3                5
    ##  9 Dubl~             2.39                3                5
    ## 10 Edin~             2.39                3                5
    ## # ... with 21 more rows, and 1 more variable: reviewMinSentiment <dbl>

``` r
#Look at correlation between rating and ranking
cor_rank=cor(dat$Ranking,dat$Rating,use="complete.obs")
cor_rank
```

    ## [1] -0.3634968

``` r
finaldat
```

    ##           V1                         Name      City
    ##      1:    0   Martine of Martine's Table Amsterdam
    ##      2:    1          De Silveren Spiegel Amsterdam
    ##      3:    2                      La Rive Amsterdam
    ##      4:    3                     Vinkeles Amsterdam
    ##      5:    4    Librije's Zusje Amsterdam Amsterdam
    ##     ---                                            
    ## 125523: 1662 Konrad Kaffee- & Cocktailbar    Zurich
    ## 125524: 1663    Blueberry American Bakery    Zurich
    ## 125525: 1664           Restaurant Bahnhof    Zurich
    ## 125526: 1665                   Yoyo Pizza    Zurich
    ## 125527: 1666                        dieci    Zurich
    ##                                                                                                  Cuisine Style
    ##      1:                                                                                French, Dutch, European
    ##      2:                                              Dutch, European, Vegetarian Friendly, Gluten Free Options
    ##      3:                     Mediterranean, French, International, European, Vegetarian Friendly, Vegan Options
    ##      4: French, European, International, Contemporary, Vegetarian Friendly, Vegan Options, Gluten Free Options
    ##      5:                Dutch, European, International, Vegetarian Friendly, Vegan Options, Gluten Free Options
    ##     ---                                                                                                       
    ## 125523:                                                                                                       
    ## 125524:                                                                                                   Cafe
    ## 125525:                                                                                                       
    ## 125526:                                                                                              Fast Food
    ## 125527:                                                                   Italian, Pizza, Mediterranean, Diner
    ##         Ranking Rating Price Range Number of Reviews
    ##      1:       1    5.0    $$ - $$$               136
    ##      2:       2    4.5        $$$$               812
    ##      3:       3    4.5        $$$$               567
    ##      4:       4    5.0        $$$$               564
    ##      5:       5    4.5        $$$$               316
    ##     ---                                             
    ## 125523:      NA     NA                            NA
    ## 125524:      NA     NA                            NA
    ## 125525:      NA     NA                            NA
    ## 125526:      NA     NA                            NA
    ## 125527:      NA     NA    $$ - $$$                NA
    ##                                                                                                            Reviews
    ##      1:                   [['Just like home', 'A Warm Welcome to Wintry Amsterdam'], ['01/03/2018', '01/01/2018']]
    ##      2:                                   [['Great food and staff', 'just perfect'], ['01/06/2018', '01/04/2018']]
    ##      3:                        [['Satisfaction', 'Delicious old school restaurant'], ['01/04/2018', '01/04/2018']]
    ##      4: [['True five star dinner', 'A superb evening of fine dining, hospitali...'], ['12/20/2017', '12/17/2017']]
    ##      5:                            [['Best meal.... EVER', 'super food experience'], ['01/06/2018', '01/04/2018']]
    ##     ---                                                                                                           
    ## 125523:                                                                                                           
    ## 125524:                                                                                                           
    ## 125525:                                                                                                           
    ## 125526:                                                                                                           
    ## 125527:                                                                                                           
    ##                                                                                                                URL_TA
    ##      1: /Restaurant_Review-g188590-d11752080-Reviews-Martine_of_Martine_s_Table-Amsterdam_North_Holland_Province.html
    ##      2:          /Restaurant_Review-g188590-d693419-Reviews-De_Silveren_Spiegel-Amsterdam_North_Holland_Province.html
    ##      3:                      /Restaurant_Review-g188590-d696959-Reviews-La_Rive-Amsterdam_North_Holland_Province.html
    ##      4:                    /Restaurant_Review-g188590-d1239229-Reviews-Vinkeles-Amsterdam_North_Holland_Province.html
    ##      5:   /Restaurant_Review-g188590-d6864170-Reviews-Librije_s_Zusje_Amsterdam-Amsterdam_North_Holland_Province.html
    ##     ---                                                                                                              
    ## 125523:                            /Restaurant_Review-g188113-d13273526-Reviews-Konrad_Kaffee_Cocktailbar-Zurich.html
    ## 125524:                            /Restaurant_Review-g188113-d13292844-Reviews-Blueberry_American_Bakery-Zurich.html
    ## 125525:                                   /Restaurant_Review-g188113-d13296092-Reviews-Restaurant_Bahnhof-Zurich.html
    ## 125526:                                           /Restaurant_Review-g188113-d13323362-Reviews-Yoyo_Pizza-Zurich.html
    ## 125527:                                                /Restaurant_Review-g188113-d13349842-Reviews-Dieci-Zurich.html
    ##             ID_TA  Afghani  African  Albanian  American  Arabic
    ##      1: d11752080        0        0         0         0       0
    ##      2:   d693419        0        0         0         0       0
    ##      3:   d696959        0        0         0         0       0
    ##      4:  d1239229        0        0         0         0       0
    ##      5:  d6864170        0        0         0         0       0
    ##     ---                                                        
    ## 125523: d13273526        0        0         0         0       0
    ## 125524: d13292844        0        0         0         0       0
    ## 125525: d13296092        0        0         0         0       0
    ## 125526: d13323362        0        0         0         0       0
    ## 125527: d13349842        0        0         0         0       0
    ##          Argentinean  Armenian  Asian  Australian  Austrian  Azerbaijani
    ##      1:            0         0      0           0         0            0
    ##      2:            0         0      0           0         0            0
    ##      3:            0         0      0           0         0            0
    ##      4:            0         0      0           0         0            0
    ##      5:            0         0      0           0         0            0
    ##     ---                                                                 
    ## 125523:            0         0      0           0         0            0
    ## 125524:            0         0      0           0         0            0
    ## 125525:            0         0      0           0         0            0
    ## 125526:            0         0      0           0         0            0
    ## 125527:            0         0      0           0         0            0
    ##          Balti  Bangladeshi  Bar  Barbecue  Belgian  Brazilian  Brew Pub
    ##      1:      0            0    0         0        0          0         0
    ##      2:      0            0    0         0        0          0         0
    ##      3:      0            0    0         0        0          0         0
    ##      4:      0            0    0         0        0          0         0
    ##      5:      0            0    0         0        0          0         0
    ##     ---                                                                 
    ## 125523:      0            0    0         0        0          0         0
    ## 125524:      0            0    0         0        0          0         0
    ## 125525:      0            0    0         0        0          0         0
    ## 125526:      0            0    0         0        0          0         0
    ## 125527:      0            0    0         0        0          0         0
    ##          British  Burmese  Cafe  Cajun & Creole  Cambodian  Canadian
    ##      1:        0        0     0               0          0         0
    ##      2:        0        0     0               0          0         0
    ##      3:        0        0     0               0          0         0
    ##      4:        0        0     0               0          0         0
    ##      5:        0        0     0               0          0         0
    ##     ---                                                             
    ## 125523:        0        0     0               0          0         0
    ## 125524:        0        0     0               0          0         0
    ## 125525:        0        0     0               0          0         0
    ## 125526:        0        0     0               0          0         0
    ## 125527:        0        0     0               0          0         0
    ##          Caribbean  Caucasian  Central American  Central Asian
    ##      1:          0          0                 0              0
    ##      2:          0          0                 0              0
    ##      3:          0          0                 0              0
    ##      4:          0          0                 0              0
    ##      5:          0          0                 0              0
    ##     ---                                                       
    ## 125523:          0          0                 0              0
    ## 125524:          0          0                 0              0
    ## 125525:          0          0                 0              0
    ## 125526:          0          0                 0              0
    ## 125527:          0          0                 0              0
    ##          Central European  Chilean  Chinese  Colombian  Contemporary
    ##      1:                 0        0        0          0             0
    ##      2:                 0        0        0          0             0
    ##      3:                 0        0        0          0             0
    ##      4:                 0        0        0          0             1
    ##      5:                 0        0        0          0             0
    ##     ---                                                             
    ## 125523:                 0        0        0          0             0
    ## 125524:                 0        0        0          0             0
    ## 125525:                 0        0        0          0             0
    ## 125526:                 0        0        0          0             0
    ## 125527:                 0        0        0          0             0
    ##          Croatian  Cuban  Czech  Danish  Delicatessen  Diner  Dutch
    ##      1:         0      0      0       0             0      0      1
    ##      2:         0      0      0       0             0      0      0
    ##      3:         0      0      0       0             0      0      0
    ##      4:         0      0      0       0             0      0      0
    ##      5:         0      0      0       0             0      0      0
    ##     ---                                                            
    ## 125523:         0      0      0       0             0      0      0
    ## 125524:         0      0      0       0             0      0      0
    ## 125525:         0      0      0       0             0      0      0
    ## 125526:         0      0      0       0             0      0      0
    ## 125527:         0      0      0       0             0      1      0
    ##          Eastern European  Ecuadorean  Egyptian  Ethiopian  European
    ##      1:                 0           0         0          0         1
    ##      2:                 0           0         0          0         1
    ##      3:                 0           0         0          0         1
    ##      4:                 0           0         0          0         1
    ##      5:                 0           0         0          0         1
    ##     ---                                                             
    ## 125523:                 0           0         0          0         0
    ## 125524:                 0           0         0          0         0
    ## 125525:                 0           0         0          0         0
    ## 125526:                 0           0         0          0         0
    ## 125527:                 0           0         0          0         0
    ##          Fast Food  Filipino  French  Fujian  Fusion  Gastropub  Georgian
    ##      1:          0         0       0       0       0          0         0
    ##      2:          0         0       0       0       0          0         0
    ##      3:          0         0       1       0       0          0         0
    ##      4:          0         0       0       0       0          0         0
    ##      5:          0         0       0       0       0          0         0
    ##     ---                                                                  
    ## 125523:          0         0       0       0       0          0         0
    ## 125524:          0         0       0       0       0          0         0
    ## 125525:          0         0       0       0       0          0         0
    ## 125526:          0         0       0       0       0          0         0
    ## 125527:          0         0       0       0       0          0         0
    ##          German  Gluten Free Options  Greek  Grill  Guatemalan  Halal
    ##      1:       0                    0      0      0           0      0
    ##      2:       0                    1      0      0           0      0
    ##      3:       0                    0      0      0           0      0
    ##      4:       0                    1      0      0           0      0
    ##      5:       0                    1      0      0           0      0
    ##     ---                                                              
    ## 125523:       0                    0      0      0           0      0
    ## 125524:       0                    0      0      0           0      0
    ## 125525:       0                    0      0      0           0      0
    ## 125526:       0                    0      0      0           0      0
    ## 125527:       0                    0      0      0           0      0
    ##          Hawaiian  Healthy  Hungarian  Indian  Indonesian  International
    ##      1:         0        0          0       0           0              0
    ##      2:         0        0          0       0           0              0
    ##      3:         0        0          0       0           0              1
    ##      4:         0        0          0       0           0              1
    ##      5:         0        0          0       0           0              1
    ##     ---                                                                 
    ## 125523:         0        0          0       0           0              0
    ## 125524:         0        0          0       0           0              0
    ## 125525:         0        0          0       0           0              0
    ## 125526:         0        0          0       0           0              0
    ## 125527:         0        0          0       0           0              0
    ##          Irish  Israeli  Italian  Jamaican  Japanese  Korean  Kosher
    ##      1:      0        0        0         0         0       0       0
    ##      2:      0        0        0         0         0       0       0
    ##      3:      0        0        0         0         0       0       0
    ##      4:      0        0        0         0         0       0       0
    ##      5:      0        0        0         0         0       0       0
    ##     ---                                                             
    ## 125523:      0        0        0         0         0       0       0
    ## 125524:      0        0        0         0         0       0       0
    ## 125525:      0        0        0         0         0       0       0
    ## 125526:      0        0        0         0         0       0       0
    ## 125527:      0        0        0         0         0       0       0
    ##          Latin  Latvian  Lebanese  Malaysian  Mediterranean  Mexican
    ##      1:      0        0         0          0              0        0
    ##      2:      0        0         0          0              0        0
    ##      3:      0        0         0          0              0        0
    ##      4:      0        0         0          0              0        0
    ##      5:      0        0         0          0              0        0
    ##     ---                                                             
    ## 125523:      0        0         0          0              0        0
    ## 125524:      0        0         0          0              0        0
    ## 125525:      0        0         0          0              0        0
    ## 125526:      0        0         0          0              0        0
    ## 125527:      0        0         0          0              1        0
    ##          Middle Eastern  Minority Chinese  Mongolian  Moroccan
    ##      1:               0                 0          0         0
    ##      2:               0                 0          0         0
    ##      3:               0                 0          0         0
    ##      4:               0                 0          0         0
    ##      5:               0                 0          0         0
    ##     ---                                                       
    ## 125523:               0                 0          0         0
    ## 125524:               0                 0          0         0
    ## 125525:               0                 0          0         0
    ## 125526:               0                 0          0         0
    ## 125527:               0                 0          0         0
    ##          Native American  Nepali  New Zealand  Norwegian  Pakistani
    ##      1:                0       0            0          0          0
    ##      2:                0       0            0          0          0
    ##      3:                0       0            0          0          0
    ##      4:                0       0            0          0          0
    ##      5:                0       0            0          0          0
    ##     ---                                                            
    ## 125523:                0       0            0          0          0
    ## 125524:                0       0            0          0          0
    ## 125525:                0       0            0          0          0
    ## 125526:                0       0            0          0          0
    ## 125527:                0       0            0          0          0
    ##          Persian  Peruvian  Pizza  Polish  Polynesian  Portuguese  Pub
    ##      1:        0         0      0       0           0           0    0
    ##      2:        0         0      0       0           0           0    0
    ##      3:        0         0      0       0           0           0    0
    ##      4:        0         0      0       0           0           0    0
    ##      5:        0         0      0       0           0           0    0
    ##     ---                                                               
    ## 125523:        0         0      0       0           0           0    0
    ## 125524:        0         0      0       0           0           0    0
    ## 125525:        0         0      0       0           0           0    0
    ## 125526:        0         0      0       0           0           0    0
    ## 125527:        0         0      1       0           0           0    0
    ##          Puerto Rican  Romanian  Russian  Salvadoran  Scandinavian
    ##      1:             0         0        0           0             0
    ##      2:             0         0        0           0             0
    ##      3:             0         0        0           0             0
    ##      4:             0         0        0           0             0
    ##      5:             0         0        0           0             0
    ##     ---                                                           
    ## 125523:             0         0        0           0             0
    ## 125524:             0         0        0           0             0
    ## 125525:             0         0        0           0             0
    ## 125526:             0         0        0           0             0
    ## 125527:             0         0        0           0             0
    ##          Scottish  Seafood  Singaporean  Slovenian  Soups  South American
    ##      1:         0        0            0          0      0               0
    ##      2:         0        0            0          0      0               0
    ##      3:         0        0            0          0      0               0
    ##      4:         0        0            0          0      0               0
    ##      5:         0        0            0          0      0               0
    ##     ---                                                                  
    ## 125523:         0        0            0          0      0               0
    ## 125524:         0        0            0          0      0               0
    ## 125525:         0        0            0          0      0               0
    ## 125526:         0        0            0          0      0               0
    ## 125527:         0        0            0          0      0               0
    ##          Southwestern  Spanish  Sri Lankan  Steakhouse  Street Food  Sushi
    ##      1:             0        0           0           0            0      0
    ##      2:             0        0           0           0            0      0
    ##      3:             0        0           0           0            0      0
    ##      4:             0        0           0           0            0      0
    ##      5:             0        0           0           0            0      0
    ##     ---                                                                   
    ## 125523:             0        0           0           0            0      0
    ## 125524:             0        0           0           0            0      0
    ## 125525:             0        0           0           0            0      0
    ## 125526:             0        0           0           0            0      0
    ## 125527:             0        0           0           0            0      0
    ##          Swedish  Swiss  Taiwanese  Thai  Tibetan  Tunisian  Turkish
    ##      1:        0      0          0     0        0         0        0
    ##      2:        0      0          0     0        0         0        0
    ##      3:        0      0          0     0        0         0        0
    ##      4:        0      0          0     0        0         0        0
    ##      5:        0      0          0     0        0         0        0
    ##     ---                                                             
    ## 125523:        0      0          0     0        0         0        0
    ## 125524:        0      0          0     0        0         0        0
    ## 125525:        0      0          0     0        0         0        0
    ## 125526:        0      0          0     0        0         0        0
    ## 125527:        0      0          0     0        0         0        0
    ##          Ukrainian  Uzbek  Vegan Options  Vegetarian Friendly  Venezuelan
    ##      1:          0      0              0                    0           0
    ##      2:          0      0              0                    1           0
    ##      3:          0      0              1                    1           0
    ##      4:          0      0              1                    1           0
    ##      5:          0      0              1                    1           0
    ##     ---                                                                  
    ## 125523:          0      0              0                    0           0
    ## 125524:          0      0              0                    0           0
    ## 125525:          0      0              0                    0           0
    ## 125526:          0      0              0                    0           0
    ## 125527:          0      0              0                    0           0
    ##          Vietnamese  Welsh  Wine Bar  Xinjiang  Yunnan Afghani African
    ##      1:           0      0         0         0       0       0       0
    ##      2:           0      0         0         0       0       0       0
    ##      3:           0      0         0         0       0       0       0
    ##      4:           0      0         0         0       0       0       0
    ##      5:           0      0         0         0       0       0       0
    ##     ---                                                               
    ## 125523:           0      0         0         0       0       0       0
    ## 125524:           0      0         0         0       0       0       0
    ## 125525:           0      0         0         0       0       0       0
    ## 125526:           0      0         0         0       0       0       0
    ## 125527:           0      0         0         0       0       0       0
    ##         American Arabic Argentinean Armenian Asian Australian Austrian
    ##      1:        0      0           0        0     0          0        0
    ##      2:        0      0           0        0     0          0        0
    ##      3:        0      0           0        0     0          0        0
    ##      4:        0      0           0        0     0          0        0
    ##      5:        0      0           0        0     0          0        0
    ##     ---                                                               
    ## 125523:        0      0           0        0     0          0        0
    ## 125524:        0      0           0        0     0          0        0
    ## 125525:        0      0           0        0     0          0        0
    ## 125526:        0      0           0        0     0          0        0
    ## 125527:        0      0           0        0     0          0        0
    ##         Balti Bangladeshi Bar Barbecue Belgian Brazilian Brew Pub British
    ##      1:     0           0   0        0       0         0        0       0
    ##      2:     0           0   0        0       0         0        0       0
    ##      3:     0           0   0        0       0         0        0       0
    ##      4:     0           0   0        0       0         0        0       0
    ##      5:     0           0   0        0       0         0        0       0
    ##     ---                                                                  
    ## 125523:     0           0   0        0       0         0        0       0
    ## 125524:     0           0   0        0       0         0        0       0
    ## 125525:     0           0   0        0       0         0        0       0
    ## 125526:     0           0   0        0       0         0        0       0
    ## 125527:     0           0   0        0       0         0        0       0
    ##         Cafe Cajun & Creole Cambodian Canadian Caribbean Caucasian
    ##      1:    0              0         0        0         0         0
    ##      2:    0              0         0        0         0         0
    ##      3:    0              0         0        0         0         0
    ##      4:    0              0         0        0         0         0
    ##      5:    0              0         0        0         0         0
    ##     ---                                                           
    ## 125523:    0              0         0        0         0         0
    ## 125524:    1              0         0        0         0         0
    ## 125525:    0              0         0        0         0         0
    ## 125526:    0              0         0        0         0         0
    ## 125527:    0              0         0        0         0         0
    ##         Central American Central Asian Central European Chilean Chinese
    ##      1:                0             0                0       0       0
    ##      2:                0             0                0       0       0
    ##      3:                0             0                0       0       0
    ##      4:                0             0                0       0       0
    ##      5:                0             0                0       0       0
    ##     ---                                                                
    ## 125523:                0             0                0       0       0
    ## 125524:                0             0                0       0       0
    ## 125525:                0             0                0       0       0
    ## 125526:                0             0                0       0       0
    ## 125527:                0             0                0       0       0
    ##         Colombian Contemporary Croatian Cuban Czech Danish Delicatessen
    ##      1:         0            0        0     0     0      0            0
    ##      2:         0            0        0     0     0      0            0
    ##      3:         0            0        0     0     0      0            0
    ##      4:         0            0        0     0     0      0            0
    ##      5:         0            0        0     0     0      0            0
    ##     ---                                                                
    ## 125523:         0            0        0     0     0      0            0
    ## 125524:         0            0        0     0     0      0            0
    ## 125525:         0            0        0     0     0      0            0
    ## 125526:         0            0        0     0     0      0            0
    ## 125527:         0            0        0     0     0      0            0
    ##         Diner Dutch Eastern European Ecuadorean Ethiopian European
    ##      1:     0     0                0          0         0        0
    ##      2:     0     1                0          0         0        0
    ##      3:     0     0                0          0         0        0
    ##      4:     0     0                0          0         0        0
    ##      5:     0     1                0          0         0        0
    ##     ---                                                           
    ## 125523:     0     0                0          0         0        0
    ## 125524:     0     0                0          0         0        0
    ## 125525:     0     0                0          0         0        0
    ## 125526:     0     0                0          0         0        0
    ## 125527:     0     0                0          0         0        0
    ##         Fast Food Filipino French Fusion Gastropub Georgian German
    ##      1:         0        0      1      0         0        0      0
    ##      2:         0        0      0      0         0        0      0
    ##      3:         0        0      0      0         0        0      0
    ##      4:         0        0      1      0         0        0      0
    ##      5:         0        0      0      0         0        0      0
    ##     ---                                                           
    ## 125523:         0        0      0      0         0        0      0
    ## 125524:         0        0      0      0         0        0      0
    ## 125525:         0        0      0      0         0        0      0
    ## 125526:         1        0      0      0         0        0      0
    ## 125527:         0        0      0      0         0        0      0
    ##         Gluten Free Options Greek Grill Halal Hawaiian Healthy Hungarian
    ##      1:                   0     0     0     0        0       0         0
    ##      2:                   0     0     0     0        0       0         0
    ##      3:                   0     0     0     0        0       0         0
    ##      4:                   0     0     0     0        0       0         0
    ##      5:                   0     0     0     0        0       0         0
    ##     ---                                                                 
    ## 125523:                   0     0     0     0        0       0         0
    ## 125524:                   0     0     0     0        0       0         0
    ## 125525:                   0     0     0     0        0       0         0
    ## 125526:                   0     0     0     0        0       0         0
    ## 125527:                   0     0     0     0        0       0         0
    ##         Indian Indonesian International Irish Israeli Italian Jamaican
    ##      1:      0          0             0     0       0       0        0
    ##      2:      0          0             0     0       0       0        0
    ##      3:      0          0             0     0       0       0        0
    ##      4:      0          0             0     0       0       0        0
    ##      5:      0          0             0     0       0       0        0
    ##     ---                                                               
    ## 125523:      0          0             0     0       0       0        0
    ## 125524:      0          0             0     0       0       0        0
    ## 125525:      0          0             0     0       0       0        0
    ## 125526:      0          0             0     0       0       0        0
    ## 125527:      0          0             0     0       0       1        0
    ##         Japanese Korean Kosher Latin Lebanese Malaysian Mediterranean
    ##      1:        0      0      0     0        0         0             0
    ##      2:        0      0      0     0        0         0             0
    ##      3:        0      0      0     0        0         0             1
    ##      4:        0      0      0     0        0         0             0
    ##      5:        0      0      0     0        0         0             0
    ##     ---                                                              
    ## 125523:        0      0      0     0        0         0             0
    ## 125524:        0      0      0     0        0         0             0
    ## 125525:        0      0      0     0        0         0             0
    ## 125526:        0      0      0     0        0         0             0
    ## 125527:        0      0      0     0        0         0             0
    ##         Mexican Middle Eastern Mongolian Moroccan Nepali New Zealand
    ##      1:       0              0         0        0      0           0
    ##      2:       0              0         0        0      0           0
    ##      3:       0              0         0        0      0           0
    ##      4:       0              0         0        0      0           0
    ##      5:       0              0         0        0      0           0
    ##     ---                                                             
    ## 125523:       0              0         0        0      0           0
    ## 125524:       0              0         0        0      0           0
    ## 125525:       0              0         0        0      0           0
    ## 125526:       0              0         0        0      0           0
    ## 125527:       0              0         0        0      0           0
    ##         Norwegian Pakistani Persian Peruvian Pizza Polish Polynesian
    ##      1:         0         0       0        0     0      0          0
    ##      2:         0         0       0        0     0      0          0
    ##      3:         0         0       0        0     0      0          0
    ##      4:         0         0       0        0     0      0          0
    ##      5:         0         0       0        0     0      0          0
    ##     ---                                                             
    ## 125523:         0         0       0        0     0      0          0
    ## 125524:         0         0       0        0     0      0          0
    ## 125525:         0         0       0        0     0      0          0
    ## 125526:         0         0       0        0     0      0          0
    ## 125527:         0         0       0        0     0      0          0
    ##         Portuguese Pub Romanian Russian Salvadoran Scandinavian Scottish
    ##      1:          0   0        0       0          0            0        0
    ##      2:          0   0        0       0          0            0        0
    ##      3:          0   0        0       0          0            0        0
    ##      4:          0   0        0       0          0            0        0
    ##      5:          0   0        0       0          0            0        0
    ##     ---                                                                 
    ## 125523:          0   0        0       0          0            0        0
    ## 125524:          0   0        0       0          0            0        0
    ## 125525:          0   0        0       0          0            0        0
    ## 125526:          0   0        0       0          0            0        0
    ## 125527:          0   0        0       0          0            0        0
    ##         Seafood Singaporean Slovenian Soups South American Southwestern
    ##      1:       0           0         0     0              0            0
    ##      2:       0           0         0     0              0            0
    ##      3:       0           0         0     0              0            0
    ##      4:       0           0         0     0              0            0
    ##      5:       0           0         0     0              0            0
    ##     ---                                                                
    ## 125523:       0           0         0     0              0            0
    ## 125524:       0           0         0     0              0            0
    ## 125525:       0           0         0     0              0            0
    ## 125526:       0           0         0     0              0            0
    ## 125527:       0           0         0     0              0            0
    ##         Spanish Sri Lankan Steakhouse Street Food Sushi Swedish Swiss
    ##      1:       0          0          0           0     0       0     0
    ##      2:       0          0          0           0     0       0     0
    ##      3:       0          0          0           0     0       0     0
    ##      4:       0          0          0           0     0       0     0
    ##      5:       0          0          0           0     0       0     0
    ##     ---                                                              
    ## 125523:       0          0          0           0     0       0     0
    ## 125524:       0          0          0           0     0       0     0
    ## 125525:       0          0          0           0     0       0     0
    ## 125526:       0          0          0           0     0       0     0
    ## 125527:       0          0          0           0     0       0     0
    ##         Taiwanese Thai Tibetan Tunisian Turkish Ukrainian Uzbek
    ##      1:         0    0       0        0       0         0     0
    ##      2:         0    0       0        0       0         0     0
    ##      3:         0    0       0        0       0         0     0
    ##      4:         0    0       0        0       0         0     0
    ##      5:         0    0       0        0       0         0     0
    ##     ---                                                        
    ## 125523:         0    0       0        0       0         0     0
    ## 125524:         0    0       0        0       0         0     0
    ## 125525:         0    0       0        0       0         0     0
    ## 125526:         0    0       0        0       0         0     0
    ## 125527:         0    0       0        0       0         0     0
    ##         Vegan Options Vegetarian Friendly Venezuelan Vietnamese Wine Bar
    ##      1:             0                   0          0          0        0
    ##      2:             0                   0          0          0        0
    ##      3:             0                   0          0          0        0
    ##      4:             0                   0          0          0        0
    ##      5:             0                   0          0          0        0
    ##     ---                                                                 
    ## 125523:             0                   0          0          0        0
    ## 125524:             0                   0          0          0        0
    ## 125525:             0                   0          0          0        0
    ## 125526:             0                   0          0          0        0
    ## 125527:             0                   0          0          0        0
    ##         new_price
    ##      1:       2.5
    ##      2:       4.0
    ##      3:       4.0
    ##      4:       4.0
    ##      5:       4.0
    ##     ---          
    ## 125523:       4.0
    ## 125524:       1.0
    ## 125525:       2.5
    ## 125526:       2.5
    ## 125527:       2.5

``` r
names(finaldat)
```

    ##   [1] "V1"                   "Name"                 "City"                
    ##   [4] "Cuisine Style"        "Ranking"              "Rating"              
    ##   [7] "Price Range"          "Number of Reviews"    "Reviews"             
    ##  [10] "URL_TA"               "ID_TA"                " Afghani"            
    ##  [13] " African"             " Albanian"            " American"           
    ##  [16] " Arabic"              " Argentinean"         " Armenian"           
    ##  [19] " Asian"               " Australian"          " Austrian"           
    ##  [22] " Azerbaijani"         " Balti"               " Bangladeshi"        
    ##  [25] " Bar"                 " Barbecue"            " Belgian"            
    ##  [28] " Brazilian"           " Brew Pub"            " British"            
    ##  [31] " Burmese"             " Cafe"                " Cajun & Creole"     
    ##  [34] " Cambodian"           " Canadian"            " Caribbean"          
    ##  [37] " Caucasian"           " Central American"    " Central Asian"      
    ##  [40] " Central European"    " Chilean"             " Chinese"            
    ##  [43] " Colombian"           " Contemporary"        " Croatian"           
    ##  [46] " Cuban"               " Czech"               " Danish"             
    ##  [49] " Delicatessen"        " Diner"               " Dutch"              
    ##  [52] " Eastern European"    " Ecuadorean"          " Egyptian"           
    ##  [55] " Ethiopian"           " European"            " Fast Food"          
    ##  [58] " Filipino"            " French"              " Fujian"             
    ##  [61] " Fusion"              " Gastropub"           " Georgian"           
    ##  [64] " German"              " Gluten Free Options" " Greek"              
    ##  [67] " Grill"               " Guatemalan"          " Halal"              
    ##  [70] " Hawaiian"            " Healthy"             " Hungarian"          
    ##  [73] " Indian"              " Indonesian"          " International"      
    ##  [76] " Irish"               " Israeli"             " Italian"            
    ##  [79] " Jamaican"            " Japanese"            " Korean"             
    ##  [82] " Kosher"              " Latin"               " Latvian"            
    ##  [85] " Lebanese"            " Malaysian"           " Mediterranean"      
    ##  [88] " Mexican"             " Middle Eastern"      " Minority Chinese"   
    ##  [91] " Mongolian"           " Moroccan"            " Native American"    
    ##  [94] " Nepali"              " New Zealand"         " Norwegian"          
    ##  [97] " Pakistani"           " Persian"             " Peruvian"           
    ## [100] " Pizza"               " Polish"              " Polynesian"         
    ## [103] " Portuguese"          " Pub"                 " Puerto Rican"       
    ## [106] " Romanian"            " Russian"             " Salvadoran"         
    ## [109] " Scandinavian"        " Scottish"            " Seafood"            
    ## [112] " Singaporean"         " Slovenian"           " Soups"              
    ## [115] " South American"      " Southwestern"        " Spanish"            
    ## [118] " Sri Lankan"          " Steakhouse"          " Street Food"        
    ## [121] " Sushi"               " Swedish"             " Swiss"              
    ## [124] " Taiwanese"           " Thai"                " Tibetan"            
    ## [127] " Tunisian"            " Turkish"             " Ukrainian"          
    ## [130] " Uzbek"               " Vegan Options"       " Vegetarian Friendly"
    ## [133] " Venezuelan"          " Vietnamese"          " Welsh"              
    ## [136] " Wine Bar"            " Xinjiang"            " Yunnan"             
    ## [139] "Afghani"              "African"              "American"            
    ## [142] "Arabic"               "Argentinean"          "Armenian"            
    ## [145] "Asian"                "Australian"           "Austrian"            
    ## [148] "Balti"                "Bangladeshi"          "Bar"                 
    ## [151] "Barbecue"             "Belgian"              "Brazilian"           
    ## [154] "Brew Pub"             "British"              "Cafe"                
    ## [157] "Cajun & Creole"       "Cambodian"            "Canadian"            
    ## [160] "Caribbean"            "Caucasian"            "Central American"    
    ## [163] "Central Asian"        "Central European"     "Chilean"             
    ## [166] "Chinese"              "Colombian"            "Contemporary"        
    ## [169] "Croatian"             "Cuban"                "Czech"               
    ## [172] "Danish"               "Delicatessen"         "Diner"               
    ## [175] "Dutch"                "Eastern European"     "Ecuadorean"          
    ## [178] "Ethiopian"            "European"             "Fast Food"           
    ## [181] "Filipino"             "French"               "Fusion"              
    ## [184] "Gastropub"            "Georgian"             "German"              
    ## [187] "Gluten Free Options"  "Greek"                "Grill"               
    ## [190] "Halal"                "Hawaiian"             "Healthy"             
    ## [193] "Hungarian"            "Indian"               "Indonesian"          
    ## [196] "International"        "Irish"                "Israeli"             
    ## [199] "Italian"              "Jamaican"             "Japanese"            
    ## [202] "Korean"               "Kosher"               "Latin"               
    ## [205] "Lebanese"             "Malaysian"            "Mediterranean"       
    ## [208] "Mexican"              "Middle Eastern"       "Mongolian"           
    ## [211] "Moroccan"             "Nepali"               "New Zealand"         
    ## [214] "Norwegian"            "Pakistani"            "Persian"             
    ## [217] "Peruvian"             "Pizza"                "Polish"              
    ## [220] "Polynesian"           "Portuguese"           "Pub"                 
    ## [223] "Romanian"             "Russian"              "Salvadoran"          
    ## [226] "Scandinavian"         "Scottish"             "Seafood"             
    ## [229] "Singaporean"          "Slovenian"            "Soups"               
    ## [232] "South American"       "Southwestern"         "Spanish"             
    ## [235] "Sri Lankan"           "Steakhouse"           "Street Food"         
    ## [238] "Sushi"                "Swedish"              "Swiss"               
    ## [241] "Taiwanese"            "Thai"                 "Tibetan"             
    ## [244] "Tunisian"             "Turkish"              "Ukrainian"           
    ## [247] "Uzbek"                "Vegan Options"        "Vegetarian Friendly" 
    ## [250] "Venezuelan"           "Vietnamese"           "Wine Bar"            
    ## [253] "new_price"

``` r
#Trends- number of vegan options and gluten-free options?
finaldat[get("Gluten Free Options") == 1, .N, by=city.name]
```

    ##          City N
    ##  1: Barcelona 2
    ##  2:  Budapest 1
    ##  3:    London 2
    ##  4:      Lyon 2
    ##  5:    Madrid 4
    ##  6:     Milan 3
    ##  7:    Oporto 1
    ##  8:     Paris 1
    ##  9:    Prague 1
    ## 10: Stockholm 1

``` r
finaldat[get("Vegan Options") == 1, .N, by=city.name]
```

    ##           City N
    ##  1:  Amsterdam 1
    ##  2:     Athens 2
    ##  3:  Barcelona 1
    ##  4:     Berlin 6
    ##  5: Copenhagen 2
    ##  6:     Geneva 1
    ##  7:    Hamburg 2
    ##  8:  Ljubljana 2
    ##  9:     London 1
    ## 10:     Madrid 5
    ## 11:      Milan 9
    ## 12:     Munich 2
    ## 13:      Paris 2
    ## 14:     Prague 1
    ## 15:       Rome 3
    ## 16:     Vienna 3
    ## 17:     Warsaw 1

``` r
finaldat[get( "Healthy") == 1, .N, by=city.name]
```

    ##           City  N
    ##  1:  Amsterdam 20
    ##  2:     Athens  5
    ##  3:  Barcelona 39
    ##  4:     Berlin 12
    ##  5: Bratislava  9
    ##  6:   Brussels 10
    ##  7:   Budapest 21
    ##  8: Copenhagen  9
    ##  9:     Dublin  2
    ## 10:  Edinburgh  4
    ## 11:     Geneva  4
    ## 12:    Hamburg  4
    ## 13:   Helsinki  3
    ## 14:     Krakow  7
    ## 15:     Lisbon 24
    ## 16:     London 59
    ## 17: Luxembourg  3
    ## 18:       Lyon  5
    ## 19:     Madrid 36
    ## 20:      Milan 16
    ## 21:     Munich 12
    ## 22:     Oporto  7
    ## 23:       Oslo  2
    ## 24:      Paris 25
    ## 25:     Prague  5
    ## 26:       Rome  9
    ## 27:  Stockholm  7
    ## 28:     Vienna  8
    ## 29:     Warsaw  9
    ## 30:     Zurich  8
    ##           City  N

``` r
head(finaldat)
```

    ##    V1                       Name      City
    ## 1:  0 Martine of Martine's Table Amsterdam
    ## 2:  1        De Silveren Spiegel Amsterdam
    ## 3:  2                    La Rive Amsterdam
    ## 4:  3                   Vinkeles Amsterdam
    ## 5:  4  Librije's Zusje Amsterdam Amsterdam
    ## 6:  5       Ciel Bleu Restaurant Amsterdam
    ##                                                                                             Cuisine Style
    ## 1:                                                                                French, Dutch, European
    ## 2:                                              Dutch, European, Vegetarian Friendly, Gluten Free Options
    ## 3:                     Mediterranean, French, International, European, Vegetarian Friendly, Vegan Options
    ## 4: French, European, International, Contemporary, Vegetarian Friendly, Vegan Options, Gluten Free Options
    ## 5:                Dutch, European, International, Vegetarian Friendly, Vegan Options, Gluten Free Options
    ## 6:                   Contemporary, International, Vegetarian Friendly, Vegan Options, Gluten Free Options
    ##    Ranking Rating Price Range Number of Reviews
    ## 1:       1    5.0    $$ - $$$               136
    ## 2:       2    4.5        $$$$               812
    ## 3:       3    4.5        $$$$               567
    ## 4:       4    5.0        $$$$               564
    ## 5:       5    4.5        $$$$               316
    ## 6:       6    4.5        $$$$               745
    ##                                                                                                       Reviews
    ## 1:                   [['Just like home', 'A Warm Welcome to Wintry Amsterdam'], ['01/03/2018', '01/01/2018']]
    ## 2:                                   [['Great food and staff', 'just perfect'], ['01/06/2018', '01/04/2018']]
    ## 3:                        [['Satisfaction', 'Delicious old school restaurant'], ['01/04/2018', '01/04/2018']]
    ## 4: [['True five star dinner', 'A superb evening of fine dining, hospitali...'], ['12/20/2017', '12/17/2017']]
    ## 5:                            [['Best meal.... EVER', 'super food experience'], ['01/06/2018', '01/04/2018']]
    ## 6:                                               [['A treat!', 'Wow just Wow'], ['01/01/2018', '12/26/2017']]
    ##                                                                                                           URL_TA
    ## 1: /Restaurant_Review-g188590-d11752080-Reviews-Martine_of_Martine_s_Table-Amsterdam_North_Holland_Province.html
    ## 2:          /Restaurant_Review-g188590-d693419-Reviews-De_Silveren_Spiegel-Amsterdam_North_Holland_Province.html
    ## 3:                      /Restaurant_Review-g188590-d696959-Reviews-La_Rive-Amsterdam_North_Holland_Province.html
    ## 4:                    /Restaurant_Review-g188590-d1239229-Reviews-Vinkeles-Amsterdam_North_Holland_Province.html
    ## 5:   /Restaurant_Review-g188590-d6864170-Reviews-Librije_s_Zusje_Amsterdam-Amsterdam_North_Holland_Province.html
    ## 6:         /Restaurant_Review-g188590-d696902-Reviews-Ciel_Bleu_Restaurant-Amsterdam_North_Holland_Province.html
    ##        ID_TA  Afghani  African  Albanian  American  Arabic  Argentinean
    ## 1: d11752080        0        0         0         0       0            0
    ## 2:   d693419        0        0         0         0       0            0
    ## 3:   d696959        0        0         0         0       0            0
    ## 4:  d1239229        0        0         0         0       0            0
    ## 5:  d6864170        0        0         0         0       0            0
    ## 6:   d696902        0        0         0         0       0            0
    ##     Armenian  Asian  Australian  Austrian  Azerbaijani  Balti  Bangladeshi
    ## 1:         0      0           0         0            0      0            0
    ## 2:         0      0           0         0            0      0            0
    ## 3:         0      0           0         0            0      0            0
    ## 4:         0      0           0         0            0      0            0
    ## 5:         0      0           0         0            0      0            0
    ## 6:         0      0           0         0            0      0            0
    ##     Bar  Barbecue  Belgian  Brazilian  Brew Pub  British  Burmese  Cafe
    ## 1:    0         0        0          0         0        0        0     0
    ## 2:    0         0        0          0         0        0        0     0
    ## 3:    0         0        0          0         0        0        0     0
    ## 4:    0         0        0          0         0        0        0     0
    ## 5:    0         0        0          0         0        0        0     0
    ## 6:    0         0        0          0         0        0        0     0
    ##     Cajun & Creole  Cambodian  Canadian  Caribbean  Caucasian
    ## 1:               0          0         0          0          0
    ## 2:               0          0         0          0          0
    ## 3:               0          0         0          0          0
    ## 4:               0          0         0          0          0
    ## 5:               0          0         0          0          0
    ## 6:               0          0         0          0          0
    ##     Central American  Central Asian  Central European  Chilean  Chinese
    ## 1:                 0              0                 0        0        0
    ## 2:                 0              0                 0        0        0
    ## 3:                 0              0                 0        0        0
    ## 4:                 0              0                 0        0        0
    ## 5:                 0              0                 0        0        0
    ## 6:                 0              0                 0        0        0
    ##     Colombian  Contemporary  Croatian  Cuban  Czech  Danish  Delicatessen
    ## 1:          0             0         0      0      0       0             0
    ## 2:          0             0         0      0      0       0             0
    ## 3:          0             0         0      0      0       0             0
    ## 4:          0             1         0      0      0       0             0
    ## 5:          0             0         0      0      0       0             0
    ## 6:          0             0         0      0      0       0             0
    ##     Diner  Dutch  Eastern European  Ecuadorean  Egyptian  Ethiopian
    ## 1:      0      1                 0           0         0          0
    ## 2:      0      0                 0           0         0          0
    ## 3:      0      0                 0           0         0          0
    ## 4:      0      0                 0           0         0          0
    ## 5:      0      0                 0           0         0          0
    ## 6:      0      0                 0           0         0          0
    ##     European  Fast Food  Filipino  French  Fujian  Fusion  Gastropub
    ## 1:         1          0         0       0       0       0          0
    ## 2:         1          0         0       0       0       0          0
    ## 3:         1          0         0       1       0       0          0
    ## 4:         1          0         0       0       0       0          0
    ## 5:         1          0         0       0       0       0          0
    ## 6:         0          0         0       0       0       0          0
    ##     Georgian  German  Gluten Free Options  Greek  Grill  Guatemalan  Halal
    ## 1:         0       0                    0      0      0           0      0
    ## 2:         0       0                    1      0      0           0      0
    ## 3:         0       0                    0      0      0           0      0
    ## 4:         0       0                    1      0      0           0      0
    ## 5:         0       0                    1      0      0           0      0
    ## 6:         0       0                    1      0      0           0      0
    ##     Hawaiian  Healthy  Hungarian  Indian  Indonesian  International  Irish
    ## 1:         0        0          0       0           0              0      0
    ## 2:         0        0          0       0           0              0      0
    ## 3:         0        0          0       0           0              1      0
    ## 4:         0        0          0       0           0              1      0
    ## 5:         0        0          0       0           0              1      0
    ## 6:         0        0          0       0           0              1      0
    ##     Israeli  Italian  Jamaican  Japanese  Korean  Kosher  Latin  Latvian
    ## 1:        0        0         0         0       0       0      0        0
    ## 2:        0        0         0         0       0       0      0        0
    ## 3:        0        0         0         0       0       0      0        0
    ## 4:        0        0         0         0       0       0      0        0
    ## 5:        0        0         0         0       0       0      0        0
    ## 6:        0        0         0         0       0       0      0        0
    ##     Lebanese  Malaysian  Mediterranean  Mexican  Middle Eastern
    ## 1:         0          0              0        0               0
    ## 2:         0          0              0        0               0
    ## 3:         0          0              0        0               0
    ## 4:         0          0              0        0               0
    ## 5:         0          0              0        0               0
    ## 6:         0          0              0        0               0
    ##     Minority Chinese  Mongolian  Moroccan  Native American  Nepali
    ## 1:                 0          0         0                0       0
    ## 2:                 0          0         0                0       0
    ## 3:                 0          0         0                0       0
    ## 4:                 0          0         0                0       0
    ## 5:                 0          0         0                0       0
    ## 6:                 0          0         0                0       0
    ##     New Zealand  Norwegian  Pakistani  Persian  Peruvian  Pizza  Polish
    ## 1:            0          0          0        0         0      0       0
    ## 2:            0          0          0        0         0      0       0
    ## 3:            0          0          0        0         0      0       0
    ## 4:            0          0          0        0         0      0       0
    ## 5:            0          0          0        0         0      0       0
    ## 6:            0          0          0        0         0      0       0
    ##     Polynesian  Portuguese  Pub  Puerto Rican  Romanian  Russian
    ## 1:           0           0    0             0         0        0
    ## 2:           0           0    0             0         0        0
    ## 3:           0           0    0             0         0        0
    ## 4:           0           0    0             0         0        0
    ## 5:           0           0    0             0         0        0
    ## 6:           0           0    0             0         0        0
    ##     Salvadoran  Scandinavian  Scottish  Seafood  Singaporean  Slovenian
    ## 1:           0             0         0        0            0          0
    ## 2:           0             0         0        0            0          0
    ## 3:           0             0         0        0            0          0
    ## 4:           0             0         0        0            0          0
    ## 5:           0             0         0        0            0          0
    ## 6:           0             0         0        0            0          0
    ##     Soups  South American  Southwestern  Spanish  Sri Lankan  Steakhouse
    ## 1:      0               0             0        0           0           0
    ## 2:      0               0             0        0           0           0
    ## 3:      0               0             0        0           0           0
    ## 4:      0               0             0        0           0           0
    ## 5:      0               0             0        0           0           0
    ## 6:      0               0             0        0           0           0
    ##     Street Food  Sushi  Swedish  Swiss  Taiwanese  Thai  Tibetan  Tunisian
    ## 1:            0      0        0      0          0     0        0         0
    ## 2:            0      0        0      0          0     0        0         0
    ## 3:            0      0        0      0          0     0        0         0
    ## 4:            0      0        0      0          0     0        0         0
    ## 5:            0      0        0      0          0     0        0         0
    ## 6:            0      0        0      0          0     0        0         0
    ##     Turkish  Ukrainian  Uzbek  Vegan Options  Vegetarian Friendly
    ## 1:        0          0      0              0                    0
    ## 2:        0          0      0              0                    1
    ## 3:        0          0      0              1                    1
    ## 4:        0          0      0              1                    1
    ## 5:        0          0      0              1                    1
    ## 6:        0          0      0              1                    1
    ##     Venezuelan  Vietnamese  Welsh  Wine Bar  Xinjiang  Yunnan Afghani
    ## 1:           0           0      0         0         0       0       0
    ## 2:           0           0      0         0         0       0       0
    ## 3:           0           0      0         0         0       0       0
    ## 4:           0           0      0         0         0       0       0
    ## 5:           0           0      0         0         0       0       0
    ## 6:           0           0      0         0         0       0       0
    ##    African American Arabic Argentinean Armenian Asian Australian Austrian
    ## 1:       0        0      0           0        0     0          0        0
    ## 2:       0        0      0           0        0     0          0        0
    ## 3:       0        0      0           0        0     0          0        0
    ## 4:       0        0      0           0        0     0          0        0
    ## 5:       0        0      0           0        0     0          0        0
    ## 6:       0        0      0           0        0     0          0        0
    ##    Balti Bangladeshi Bar Barbecue Belgian Brazilian Brew Pub British Cafe
    ## 1:     0           0   0        0       0         0        0       0    0
    ## 2:     0           0   0        0       0         0        0       0    0
    ## 3:     0           0   0        0       0         0        0       0    0
    ## 4:     0           0   0        0       0         0        0       0    0
    ## 5:     0           0   0        0       0         0        0       0    0
    ## 6:     0           0   0        0       0         0        0       0    0
    ##    Cajun & Creole Cambodian Canadian Caribbean Caucasian Central American
    ## 1:              0         0        0         0         0                0
    ## 2:              0         0        0         0         0                0
    ## 3:              0         0        0         0         0                0
    ## 4:              0         0        0         0         0                0
    ## 5:              0         0        0         0         0                0
    ## 6:              0         0        0         0         0                0
    ##    Central Asian Central European Chilean Chinese Colombian Contemporary
    ## 1:             0                0       0       0         0            0
    ## 2:             0                0       0       0         0            0
    ## 3:             0                0       0       0         0            0
    ## 4:             0                0       0       0         0            0
    ## 5:             0                0       0       0         0            0
    ## 6:             0                0       0       0         0            1
    ##    Croatian Cuban Czech Danish Delicatessen Diner Dutch Eastern European
    ## 1:        0     0     0      0            0     0     0                0
    ## 2:        0     0     0      0            0     0     1                0
    ## 3:        0     0     0      0            0     0     0                0
    ## 4:        0     0     0      0            0     0     0                0
    ## 5:        0     0     0      0            0     0     1                0
    ## 6:        0     0     0      0            0     0     0                0
    ##    Ecuadorean Ethiopian European Fast Food Filipino French Fusion
    ## 1:          0         0        0         0        0      1      0
    ## 2:          0         0        0         0        0      0      0
    ## 3:          0         0        0         0        0      0      0
    ## 4:          0         0        0         0        0      1      0
    ## 5:          0         0        0         0        0      0      0
    ## 6:          0         0        0         0        0      0      0
    ##    Gastropub Georgian German Gluten Free Options Greek Grill Halal
    ## 1:         0        0      0                   0     0     0     0
    ## 2:         0        0      0                   0     0     0     0
    ## 3:         0        0      0                   0     0     0     0
    ## 4:         0        0      0                   0     0     0     0
    ## 5:         0        0      0                   0     0     0     0
    ## 6:         0        0      0                   0     0     0     0
    ##    Hawaiian Healthy Hungarian Indian Indonesian International Irish
    ## 1:        0       0         0      0          0             0     0
    ## 2:        0       0         0      0          0             0     0
    ## 3:        0       0         0      0          0             0     0
    ## 4:        0       0         0      0          0             0     0
    ## 5:        0       0         0      0          0             0     0
    ## 6:        0       0         0      0          0             0     0
    ##    Israeli Italian Jamaican Japanese Korean Kosher Latin Lebanese
    ## 1:       0       0        0        0      0      0     0        0
    ## 2:       0       0        0        0      0      0     0        0
    ## 3:       0       0        0        0      0      0     0        0
    ## 4:       0       0        0        0      0      0     0        0
    ## 5:       0       0        0        0      0      0     0        0
    ## 6:       0       0        0        0      0      0     0        0
    ##    Malaysian Mediterranean Mexican Middle Eastern Mongolian Moroccan
    ## 1:         0             0       0              0         0        0
    ## 2:         0             0       0              0         0        0
    ## 3:         0             1       0              0         0        0
    ## 4:         0             0       0              0         0        0
    ## 5:         0             0       0              0         0        0
    ## 6:         0             0       0              0         0        0
    ##    Nepali New Zealand Norwegian Pakistani Persian Peruvian Pizza Polish
    ## 1:      0           0         0         0       0        0     0      0
    ## 2:      0           0         0         0       0        0     0      0
    ## 3:      0           0         0         0       0        0     0      0
    ## 4:      0           0         0         0       0        0     0      0
    ## 5:      0           0         0         0       0        0     0      0
    ## 6:      0           0         0         0       0        0     0      0
    ##    Polynesian Portuguese Pub Romanian Russian Salvadoran Scandinavian
    ## 1:          0          0   0        0       0          0            0
    ## 2:          0          0   0        0       0          0            0
    ## 3:          0          0   0        0       0          0            0
    ## 4:          0          0   0        0       0          0            0
    ## 5:          0          0   0        0       0          0            0
    ## 6:          0          0   0        0       0          0            0
    ##    Scottish Seafood Singaporean Slovenian Soups South American
    ## 1:        0       0           0         0     0              0
    ## 2:        0       0           0         0     0              0
    ## 3:        0       0           0         0     0              0
    ## 4:        0       0           0         0     0              0
    ## 5:        0       0           0         0     0              0
    ## 6:        0       0           0         0     0              0
    ##    Southwestern Spanish Sri Lankan Steakhouse Street Food Sushi Swedish
    ## 1:            0       0          0          0           0     0       0
    ## 2:            0       0          0          0           0     0       0
    ## 3:            0       0          0          0           0     0       0
    ## 4:            0       0          0          0           0     0       0
    ## 5:            0       0          0          0           0     0       0
    ## 6:            0       0          0          0           0     0       0
    ##    Swiss Taiwanese Thai Tibetan Tunisian Turkish Ukrainian Uzbek
    ## 1:     0         0    0       0        0       0         0     0
    ## 2:     0         0    0       0        0       0         0     0
    ## 3:     0         0    0       0        0       0         0     0
    ## 4:     0         0    0       0        0       0         0     0
    ## 5:     0         0    0       0        0       0         0     0
    ## 6:     0         0    0       0        0       0         0     0
    ##    Vegan Options Vegetarian Friendly Venezuelan Vietnamese Wine Bar
    ## 1:             0                   0          0          0        0
    ## 2:             0                   0          0          0        0
    ## 3:             0                   0          0          0        0
    ## 4:             0                   0          0          0        0
    ## 5:             0                   0          0          0        0
    ## 6:             0                   0          0          0        0
    ##    new_price
    ## 1:       2.5
    ## 2:       4.0
    ## 3:       4.0
    ## 4:       4.0
    ## 5:       4.0
    ## 6:       4.0
