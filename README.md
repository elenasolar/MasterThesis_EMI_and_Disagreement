# Bridging Political Divides: Quantifying the Influence of Evidence- and Intuition-Based Language on Disagreement

This repository contains the code to replicate the findings of my Master Thesis in Social and Economic Data Science at the University of Konstanz.


## **Data**

* The raw data files (`.zst`-format) including the redidt submissions and comments from the desired subreddits need to be added in a folder `code/data/raw_data`. Data can be obtained via [Academic Torrtents](https://academictorrents.com/details/56aa49f9653ba545f48df2e33679f014d2829c10)
*  

## **Data Preprocessing**

* needed preprocesing functions are defined in and imported from `code/data_preprocessing/preprocessing_functions.py`
* `code/data_preprocessing/subreddit_xx/` contains the notebooks to import and preprocess the raw reddit data per subreddit. Also, creates and expots the data frames needed in `Empty_non_Empty_Plots.ipynb`.
* final preprocessed data frames are saved as `.csv` to `code/data/new_preprocessing/...`
