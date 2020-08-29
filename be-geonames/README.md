# Analysis of names of Belgian villages

Inspired by [this blogpost about names of villages in Germany](https://rud.is/b/2016/01/03/zellingenach-a-visual-exploration-of-the-spatial-patterns-in-the-endings-of-german-town-and-village-names-in-r/) and [some](http://truth-and-beauty.net/experiments/ach-ingen-zell/) [others](https://github.com/maelle/kervillebourg), I wanted to have a look at the names of Belgian villages. I decided to use different approaches and look for patterns in prefix and suffix, or for cities with distinctive names pointing at rivers (i.e. Petegem-**aan-de-Leie**) or other landscape factors (i.e. Heist-**op-den-Berg**).

This excercise was never ment to be 100% accurate, so some of my analysis and charts may have overlaps, missing data or are simple incomplete. Some code can be more compact and elegant so I welcome all suggestions and adaptations.

# Data
I got a list with all the Belgian cities and their geolocation from [GeoNames](http://download.geonames.org/), as always this file can be found in the data folder of this repository.

# Credits
Special thanks for the inspiration and guidlines to
- Moritz Stefaner
- Bob Rudis
- Maëlle Salmon
