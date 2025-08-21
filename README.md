# Bridging Political Divides: Quantifying the Influence of Evidence- and Intuition-Based Language on Disagreement

(Note: reporitory is still work in progress)


This repository contains the code to replicate the findings of my Master Thesis in Social and Economic Data Science at the University of Konstanz.


## **Data**

* The raw data files (`.zst`-format) including the redidt submissions and comments from the desired subreddits need to be added in a folder `code/data/raw_data`. Data can be obtained via [Academic Torrtents](https://academictorrents.com/details/56aa49f9653ba545f48df2e33679f014d2829c10)


## **Data Preprocessing**

* needed preprocesing functions are defined in and imported from `code/data_preprocessing/preprocessing_functions.py`
* `code/data_preprocessing/subreddit_xx/` contains the notebooks to import and preprocess the raw reddit data per subreddit. Also, creates and expots the data frames needed in `code/data_preprocessing/Empty_non_Empty_Plots.ipynb` (comparison of empty vs. non-empty submissions).
* final preprocessed data frames are saved as `.csv` to `code/data/new_preprocessing/...`

## **Classification Model**

* `code/llama/Causal` contains the initial tests of two vs. three labels of the Causal LM, as well as the prompt style comparisons
* `code/llama/Classification/two_labels` contains three version of fine-tuning architectures $\to$ `attention_FFN_LoRA`-folder contains notebooks for training and applying the final model to the six subreddits. Classified labels are saved to `code/data/classified_labels/incl_true_probability/...`
* `code/llama/Validation` contains notebooks to sample validation sample of 10 interactions per subreddit. These were manually classified by three annotators.
The three annotators performance is compared in `code/llama/Validation/PCE_Investigation.ipynb`
* three notebooks investigating the OOD performance of the three Llama_3.1_8B architectures

## **User Ideology**
* `code/user_ideology/compute_ideology_scores.ipynb` computes the ideology score per user in our sample. Final positionings for our 800.000 users are saved to `code/user_ideology/user_positioning.csv`. Subsequently, `code/user_ideology/Add_Ideology_Scores.ipynb` adds all user ideology scores to the df of all interactions for analysis.
* `code/user_ideology/user_subreddit_activity.jsonl`: file containing the post frequencies of all users in our sample across all subreddits. Data was kindly provided by Dr. João Pinheiro Neto.
* `code/user_ideology/scores.csv` contains the subreddits ideology scores, computed by [Waller & Anderson](https://github.com/CSSLab/social-dimensions)
* 
