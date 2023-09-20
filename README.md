# count_cooking_emission
Codes for dealing with restaurant cuisine classification for Map POI Data



Purpose:
This file is designed to determine the cuisine category of restaurants on the map, facilitating the assignment of appropriate emission factors for each establishment.

How the code works:

Preliminary Classification using Map POI Data: Restaurants are initially categorized using the third-level labels from map Point Of Interest (POI) data. This allows for some restaurants to be directly assigned to specific cuisines, like Sichuan-Hunan cuisine. However, over 60% of restaurants are merely labeled as "Chinese restaurants", necessitating further detailed classification.

Keyword Frequency Analysis Post-Preliminary Classification: After the initial categorization, most cuisines will have some associated restaurants. We employ a word frequency analysis on the names of the restaurants in each cuisine category using the jiebaR package. This yields high-frequency words for restaurant names linked to a particular cuisine. We then manually select words that are specifically indicative of that cuisine.

Cuisine Determination Based on Restaurant Name: For restaurants that couldn't be precisely classified through labels, their cuisines are determined by matching the keywords derived from their names to the specific cuisines identified in the previous step.

Default Assignment: Lastly, any remaining restaurants that couldn't be classified due to the absence of specific keywords are categorized as "home-style cuisine".

We hope this method simplifies the process of categorizing restaurants on maps and aids in more accurate data representation.

reference:https://essd.copernicus.org/preprints/essd-2023-278/
