# 🌿 NDVI Analysis Project Worksheet

This worksheet will guide you through the process of designing and carrying out an NDVI-based analysis in R. Fill in the prompts for your chosen research question, location, time period, data sources, and methods.

------------------------------------------------------------------------

## 1. Framing the Question

**What environmental or social issue will you explore?**\
Forest Health and Rapid Invasive Grass Spread around Kealakekua Mountain Reserve

------------------------------------------------------------------------

**Why is NDVI an appropriate tool for this question?**\
Help to identify areas of growth and depletion over time in this area.

------------------------------------------------------------------------

**Who might find your results meaningful or useful?**\
Hiki Ola/KMR crews, DLNR, local schools, community partners, and fire/fuel managers.

------------------------------------------------------------------------

## 2. Choosing a Place and Time

**What geographic area will you focus on?**\
Kealakekua Mountain Reserva Uka of Kona

------------------------------------------------------------------------

**What time frame makes sense for your question?**\
(e.g., single date, multiple years, seasonal patterns)\
Wet vs Dry Season, 2019-2025

------------------------------------------------------------------------

**How will you define the scope of your analysis?**\
Compared fenced, restoration plots to nearby un-fenced areas at the same elevation.

------------------------------------------------------------------------

## 3. Finding Data

**Where could you get satellite imagery or NDVI data?**\
Copernicus Browser Sentinel-2 L1C

------------------------------------------------------------------------

**What resolution and frequency are appropriate?**\
Low cloud coverage, two images a year from 2019-2025 wet and dry seasons

------------------------------------------------------------------------

**Will you download data manually or use an R package? Which one?**\
Download manually off of Copernicus Browser

------------------------------------------------------------------------

## 4. Bringing Data into R

**What R packages can help you work with spatial data?**\
terra, mapview, leaflet, ggspatial, ggplot, sf

------------------------------------------------------------------------

**How will you handle projections, boundaries, or missing data?**\
Take out areas with too many clouds, set the boundary to the reserve and nearby, and use a coordinate reference system (EPSG:32605).

------------------------------------------------------------------------

**What file formats will you be working with?**\
.tif .geojson .shp

------------------------------------------------------------------------

## 5. Calculating NDVI

**What is the NDVI formula?**\
NDVI = (NIR - Red) / (NIR + Red)

------------------------------------------------------------------------

**Which spectral bands are needed?**\
B4 B8

------------------------------------------------------------------------

**How will you apply this formula in R?**\
Reference ndvi_mapping.qmd using the libraries from above.

------------------------------------------------------------------------

## 6. Exploring and Visualizing Data

**How will you summarize NDVI values?**\
(maps, plots, tables)\
Use leaflet library to display maps, and ggplot to display plots and tables.

------------------------------------------------------------------------

**Will you compare locations, before and after events, look at seasonal patterns, or study long-term trends?**\
Wet vs Dry Season, 2019-2025

------------------------------------------------------------------------

**How will you make your visualizations clear and interpretable?**\
Using more basic plots and tables, highlighting significant changes, focusing on a small area, color-coding and labelling.

------------------------------------------------------------------------

## 7. Interpreting Results

**What patterns or relationships do you expect to see?**\
A higher NDVI in the preserved areas and wet season compared to the un-preserved areas and the dry season.

------------------------------------------------------------------------

**How do they relate to your research question?**\
Help track growth and restoration over time while comparing nearby un-preserved areas.

------------------------------------------------------------------------

**What uncertainties or data limitations should you acknowledge?**\
Cloud coverage, and limited data (one day per season per year)

------------------------------------------------------------------------

## 8. Reflecting on Impact

**Could your results inform decisions, policies, or further research?**\
Potentially, could prove the need for more reservations/programs similar to Kealakekua Mountain Reserve.

------------------------------------------------------------------------

**What new questions emerge from your findings?**\
How can reservation/reservation programs be implemented in different locations in Hawaiʻi? How can ongoing programs be improved?
