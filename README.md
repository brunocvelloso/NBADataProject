# NBADataProject
Description: Amasses a large NBA database from public, web-scraped data. Uses a variety of machine learning tools to predict team and player performance. Please see Jupyter Notebook examples for some of the many things I do with the data.

# Main Notes
This is meant mainly to show-case some of the many things I have done with NBA data. The main thing is to look at the Jupyter Notebooks. If you clone the repo and feed in the file location of that repo on your local computer, you should be able to run the notebooks on your local machine, but they are mainly there to show some key results/explanations.

# Organization
- As mentioned above, main outputs are in ".ipynb" files.
- "py" folder contains all python code (mainly for the NBA Roster Tool, which is described in NBARosterTool.ipynb)
- "R" folder contains all R scripts (used for R_ExamplesResults.ipynb and NueralNetwordsExample_NBAData.ipynb)
     
     --> this folder also shows how I scraped and collected the dataset. If you want to follow along, look at the "Master-NBA.R"
     data for a detailed guide of how I update my master dataset (it is quite detailed/complex. I do a lot of things)
     
     --> I do not feel comfortable sharing the master dataset, even though it is all collected/scraped from public datasets. But if you want
     to replicate what I did in some manner, the code in "Master-NBA" should serve as a useful guide. The way it is structured is to take the (sometimes
     quite hefty) raw master data files and update it for any new games over time (I have a cron job that automatically updates it overnight). But you 
     can essentially just add a few lines of code to create this from scratch. All you need as input the game links data "NBA_ESPNgamelinks.RData"
     which is included in the repo


# OTHER NOTES
- The .ipynb notebooks contain pretty detailed descriptions of what I am conceptually trying to do. You can look at whatever scripts I call in more detail if you want
to have a better idea of what I actually coded. Any questions just ask!
- Any typos or mistakes, feel free to send a pull request.
- any big datasets I call are called directly from a publicly available dropbox folder, so you will need an internet connection to run on your local machine.
- Again, I did A LOT with this data. This is merely a showcase of some things I do. In particular, the NBA Roster Tool, which projects player performance
by explicitly
- Marcelo was here!
