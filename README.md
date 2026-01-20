# Ride the Data: NYC Taxi Insights

The purpose of this project is to create an interactive app with visualizations using Shiny to let users interact with the data set of my choice.

As such, I will create a narrative visualization to inform users and also let them interact with the data to learn new things by actions such as

1) picking,
2) clicking,
3) sliding,
4) hovering,
5) drilling down,
6) zooming in, etc.

------------------------------------------

For this project, I will be using The New York City Limousine and Taxi Commission (NYC TLC) trip records.

Link: https://www.nyc.gov/site/tlc/about/tlc-trip-record-data.page

The preprocessing was done on my local machine due to the size of the file. The respective notebook will be provided.

Cleaning and quick analysis: Taxi-Data Cleaning

Testing: Testing-Grounds

Clean data file: Taxi_data_8310.csv (170MB)

The due to size of the data, there is a small delay (few seconds) only when opening up the app.

Limitations:
1) Lack of widgets.
2) Nature of data: The distribution of trips across boroughs is wonky and contains millions of noisy trips that make metrics seem unstable or weird.
3) The dataset contains trips only from September. Including additional months would result in a heavier dataset, which in turn would affect the app's performance.

------------------------------------------

The link/work can be only be accessed by users associated with Mizzou since the app is hosted on the Mizzou Shiny Server. As such, kindly refer to the "Pictures" directory to see how the app turned out. 

------------------------------------------

In the future, I plan to host the project on my local server so that everyone can access the app. 
