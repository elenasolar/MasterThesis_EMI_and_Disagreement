# Bridging Political Divides: Quantifying the Influence of Evidence- and Intuition-Based Language on Disagreement

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

## **EMI**
* `code/EMI/reddit-data_preprocess.ipynb` applies the embedding model preprocessing to the six subreddits data and computes plot to investigate text length (see Supplementary Materials, Chapter 4)
* `code/EMI/reddit_data/00_embedding_models/embedding_files` is supposed to contain the base GloVe embeddings which are publicly available [here](https://nlp.stanford.edu/projects/glove/). The fine-tuned and self-build embedding vectors will be saved to this folder as well.
* `code/EMI/reddit_data/00_embedding_models` contains notebooks to fine-tune the GloVe embedding model (`advanced_reddit_fine-tune-embeddings.ipynb`) and transform it into a S-BERT model (`advanced_reddit_embeddings-to-sbert.ipynb`).
The resulting fine-tuned and self-build embedding model are saved to the two respective folders. 
* `code/EMI/reddit_data/01_after_pool` contains notebooks for the analysis of dictionary embeddings (see Supplementary Materials, Section  6.1), word level embeddings (Supplementary Materials, Section 6.3) as well as document level embedding scores (Supplementary Materials, Section 6.4).
Furthermore, Bootstrapping analysis notebooks are included (Supplemenraty Materials, Section 6.2)
* `code/EMI/reddit_data/01_after_pool/reddit_EMI_Computation.ipynb` is the notebook to compute EMI scores for each text in two compute orders and with clipping, as well as no clipping.
The notebook also contains the investigations of different bin counts (see Supplementary Materials, Section 6.4.1)
* `code/EMI/reddit_data/01_after_pool/Order_and_Clipping.ipynb` compares the compute order of EMI scores (first compute EMI, then z-transform vs. z-transforing similarity scores, them computing EMI) as well as the comparison of clipped vs. non-clipped scores.
* `code/EMI/reddit_data/01_after_pool/Make_analysis_data.ipynb` combined the final classification labels, with the interaction data (containing EMI and user ideology scores) in two variations: clipped and non-clipped. Further analysis only uses the non-clipped scores.

## **Analyis**
* required analysis datasets are provided as zip files in `code/data/analysis_data/` and need to be unzipped
* `code/analysis/Subreddits_Mod_Human.ipynb` contains the analysis of subreddit's contributions to interactions and submissions as well as the comparison of human versus moderator started discussions (see Supplementary Materialy, Section 8.1 and 8.2)
* `code/analysis/Correlation_analysis_non_clipped.ipynb` pairwise correlation analysis for EMI and Stance, EMI and Ideology and Ideology and Stance (see Supplementary Materials, Section 8.3 - 8.5)
* `code/analysis/Active Discussions.ipynb` is the analysis focused on the most active discussions
* `code/analysis/EMI_Stance_Ideology.ipynb` analyzes the three variables of interest together (see Supplementary Materials, Section 8.6)
* `code/analysis/keyness_non_clipped.ipynb` is the notebook to compute the three pairwise keyness scatter plots 
* `code/analysis/R-model_fitting` contains four files in order to fit the linear and logistic mixed effect models for our research hypothesis.
* `code/analysis/Interprete_logit_Coefficients.ipynb` transforms the logit coefficients for easier interpretability
