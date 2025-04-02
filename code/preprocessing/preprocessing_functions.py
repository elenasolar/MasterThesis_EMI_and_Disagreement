import pandas as pd
import collections
import zstandard
import io
import json
from datetime import datetime
import pyarrow as pa
import pyarrow.parquet as pq
import matplotlib.pyplot as plt
import numpy as np

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
def import_subreddit_to_parquet(filepath, parquet_filepath, chunk_size=100_000, data_type = 'comments'):
    """
    Reads a large .zst JSON file and saves it in Parquet format in chunks.

    Parameters:
    -----------
        - filepath: str, path to the .zst compressed file.
        - parquet_filepath: str, path to save the Parquet file.
        - chunk_size: int, number of rows per chunk before writing to disk.

    Returns:
    --------
        - None, saves data as a Parquet file.
    """



    if data_type == "comments":
        RELEVANT_COLUMNS = ["author", "created_utc", "parent_id", "subreddit_id", "body", "link_id", "name", "id", "ups", "score", "downs",
              "subreddit",  "likes", "replies", "stickied",  "send_replies", "comment_type"]
        
        COLUMN_DTYPES = {
            "author": "string",
            "created_utc": "string",
            "parent_id": "string",
            "subreddit_id": "string",
            "body": "string",
            "link_id": "string",  
            "name" : "string",
            "id": "string", 
            "ups" : "string", 
            "score" : "string",
            "downs" : "string", 
            "subreddit" : "string", 
            "likes": "string", 
            "replies": "string", 
            "stickied": "string",  
            "send_replies": "string",
            "comment_type": "string",           
        }

    elif data_type == "submissions":
        RELEVANT_COLUMNS = ["author", "created_utc", "id", "num_comments", "score", "send_replies", "stickied", 
            "subreddit", "subreddit_id", "title", "name", "likes", "ups", "category", "selftext"]
    
        COLUMN_DTYPES = {
            "author": "string",
            "created_utc": "string",
            "id": "string",
            "num_comments": "string",
            "score": "string",
            "send_replies": "string",  
            "stickied": "string",
            "subreddit": "string",  
            "subreddit_id" : "string", 
            "title" : "string", 
            "name" : "string", 
            "likes" : "string", 
            "ups" : "string", 
            "category" : "string", 
            "selftext" : "string",
        }


    data = []
    rows = 0
    first_chunk = True

    with open(filepath, 'rb') as compressed_file:
        dctx = zstandard.ZstdDecompressor()
        with dctx.stream_reader(compressed_file) as reader:
            text_stream = io.TextIOWrapper(reader, encoding='utf-8')

            for line in text_stream:
                line = line.strip()
                if line:
                    try:
                        json_object = json.loads(line)
                        data.append(json_object)

                        # Write to parquet in chunks
                        if len(data) >= chunk_size:
                            df = pd.DataFrame(data)
                            
                            df = df.reindex(columns=RELEVANT_COLUMNS, fill_value=None)

                            
                            # Adjusting data types
                            # if "edited" in df.columns:
                            #     df["edited"] = df["edited"].astype(str)  

                            # if "parent_id" in df.columns:
                            #     df["parent_id"] = df["parent_id"].astype(str)

                            # if "created_utc" in df.columns:
                            #     df["created_utc"] = pd.to_numeric(df["created_utc"], errors="coerce").astype("Int64")
                            for col, dtype in COLUMN_DTYPES.items():
                                if col in df.columns:
                                    df[col] = df[col].astype(dtype, errors="ignore")


                            
                            # Append data to Parquet file
                            #pq.write_to_dataset(table, root_path=parquet_filepath, partition_cols=None)
                            df.to_parquet(parquet_filepath, engine="fastparquet", compression="snappy", index=False, append = not first_chunk)

                            
                            rows += len(data)
                            print(f"Written {rows} rows to {parquet_filepath}")

                            data.clear()  # Clear memory
                            first_chunk = False

                    except json.JSONDecodeError as e:
                        print(f"Malformed line: {line}, Error: {e}")

    # Save remaining data
    if data:
        df = pd.DataFrame(data)
        df = df.reindex(columns=RELEVANT_COLUMNS, fill_value=None)
        

        # if "edited" in df.columns:
        #     df["edited"] = df["edited"].astype(str)

        # if "parent_id" in df.columns:
        #     df["parent_id"] = df["parent_id"].astype(str)

        # if "created_utc" in df.columns:
        #     df["created_utc"] = pd.to_numeric(df["created_utc"], errors="coerce").astype("Int64")

        for col, dtype in COLUMN_DTYPES.items():
            if col in df.columns:
                df[col] = df[col].astype(dtype, errors="ignore")

        df.to_parquet(parquet_filepath, engine="fastparquet", compression="snappy", index=False, append = not first_chunk)


        rows += len(data)
        print(f"Final write: Total {rows} rows saved to {parquet_filepath}")

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
def import_subreddit(filepath):

    """
    Function to import reddit data from a z-compressed zip file
     
    Parameters:
    -----------
        - filepath: str, path to storage location

    Return:
    -------
        - data: list of dictionaries containing the data. Needs to be transformed to pandas Data Frame
    """

    data = []

    with open(filepath, 'rb') as compressed_file:
        dctx = zstandard.ZstdDecompressor()
        with dctx.stream_reader(compressed_file) as reader:
            # Wrap the byte stream in a text wrapper to read lines
            text_stream = io.TextIOWrapper(reader, encoding='utf-8')
            for line in text_stream:
                line = line.strip()  # Remove any extra whitespace
                if line:  # Ignore empty lines
                    try:
                        json_object = json.loads(line)  # Parse JSON
                        data.append(json_object)
                    except json.JSONDecodeError as e:
                        print(f"Malformed line: {line}, Error: {e}")

    return data


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
def get_submissions(df):

    """
    Function to get relevant column from submissions df.
    Additionally, add 'type' column with submission and create 'date' as Y-m-d

    Parameter:
    --------- 
        - df: pandas Data Frame containing the submissions, obtained from import_subreddit()

    Return:
    ------
        - data_frame: pandas Data Frame of relevant columns and with type and date 
    """
    
    data_frame = df[["author", "created_utc", "id", "num_comments", "score", "send_replies", "stickied", 
            "subreddit", "subreddit_id", "title", "name", "likes", "ups", "category", "selftext"]]
    
    data_frame['type'] = 'submission'
    data_frame['created_utc'] = pd.to_numeric(data_frame['created_utc'])
    data_frame['date'] = data_frame['created_utc'].apply(lambda x: datetime.fromtimestamp(x).strftime('%Y-%m-%d'))

    # sort by creation time
    data_frame = data_frame.sort_values(by = "created_utc")

    print("Nr. of pulled submissions", len(data_frame))

    
    return data_frame

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
def get_comments_and_replies(df):

    """
    Function to get relevant column from comments and replies df.
    Create 'date' as Y-m-d.
    Strip columns with parent, link, subreddit and name

    Parameter:
    --------- 
        - df: pandas Data Frame containing the comments and replies, obtained from import_subreddit()

    Return:
    ------
        - data_frame: pandas Data Frame of relevant columns, stripped columns and date 
    """

    data_frame = df[["author", "created_utc", "parent_id", "subreddit_id", "body", "link_id", "name", "id", "ups", "score", "downs",
              "subreddit", "likes", "replies", "stickied",  "send_replies", "comment_type"]]

    data_frame['created_utc'] = pd.to_numeric(data_frame['created_utc'])
    data_frame['date'] = data_frame['created_utc'].apply(lambda x: datetime.fromtimestamp(x).strftime('%Y-%m-%d'))

    # strip columns
    data_frame['parent_id'] = data_frame['parent_id'].str[3:]
    data_frame['link_id'] = data_frame['link_id'].str[3:]
    data_frame['subreddit_id'] = data_frame['subreddit_id'].str[3:]
    data_frame['name'] = data_frame['name'].str[3:] # same as comment/reply id

    # sort by creation time
    data_frame = data_frame.sort_values(by = "created_utc")

    print("Nr. of pulled comments & replies", len(data_frame))

    return data_frame
    

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
def assign_types(df_both, df_submissions):

    """
    Function to assign types to comments and replies.
    Start by taking all rows, where parent_id's is in the submissions --> these are comments.
    Then, take all rows, where parent_id is in the comments --> these are first level replies.

    Parameters:
    -----------
        - df_both: pandas Data Frame containing comments and replies. Return of get_comments_and_replies()
        - df_submissions: the matchind pandas Data Frame, contraining the subreddits submissions (from get_submission())

    Return:
    -------
        - comments and replies1: two data frames, subsamples of the big df  

    """

    df = df_both.copy()
    df['type'] = None

    # assign comment type ------------------------------------------------
    df.loc[df['parent_id'].isin(df_submissions['id']), 'type'] = 'comment'
    # extract comments
    comments = df[df['type'] == 'comment'].reset_index(drop = True)

    # assign reply_1 type ------------------------------------------
    df.loc[df['parent_id'].isin(comments['id']), 'type'] = 'reply_1'
    # extract replies
    replies1 = df[df['type'] == 'reply_1'].reset_index(drop = True)
    
    # OPTIONALLY --------------------------------------------------
    # # assign reply_2 type
    # df.loc[df['parent_id'].isin(replies1['id']), 'type'] = 'reply_2'
    # replies2 = df[df['type'] == 'reply_2'].reset_index(drop = True)

    # # assign reply_3 type
    # df.loc[df['parent_id'].isin(replies2['id']), 'type'] = 'reply_3'
    # replies3 = df[df['type'] == 'reply_3'].reset_index(drop = True)

    print("Nr. of comments", len(comments))
    print("Nr. of replies", len(replies1))

    return comments, replies1



#-------------------
def plot_submissions_comments(df_comments, df_submissions, submission_id_col, comment_id_col, log_x = True, log_y = True):

    relevant_submissions = pd.DataFrame(df_comments.groupby(submission_id_col)[comment_id_col].count()).reset_index().rename(columns={submission_id_col : 'submission_id', comment_id_col : 'count'})
    print("Mean nr. of comments per submission", relevant_submissions['count'].mean())
    print("Nr. of submission, that I have comments for", len(relevant_submissions))

    #relevant_submissions

    plt_data = pd.DataFrame(relevant_submissions['count'].value_counts()).rename(columns={'count' : 'class_count'}).reset_index().rename(columns={'count' : 'comment_count'})
    plt_data

    plt_data['log_comm_count'] = plt_data['comment_count'].apply(lambda x: np.log(x))
    plt_data['log_class_count'] = plt_data['class_count'].apply(lambda x: np.log(x))
    plt_data

    if log_x:
        x = plt_data['log_comm_count']
        x_label = "log(comment count)"
    else:
        x = plt_data['comment_count']
        x_label = "comment count"

    if log_y:
        y = plt_data['log_class_count']
        y_label = "log(#submissions)"
    else:
        y = plt_data['class_count']
        y_label = "#submissions"



    
    plt.plot(x, y)
    plt.xlabel(x_label)
    plt.ylabel(y_label)
    plt.title("Nr of Comments per Submission")
    plt.show
    




#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
def find_relevant_submissions(df_comments, df_submissions, submission_id_col, comment_id_col):

    """
    Function to compute the mean of comments per submission in that subreddit, using the comments df.
    Then, filter to only retain submissions, which do not have empty texts.
    Finally, filter comments, to only be comments with parent_id in the relevant submissions.

    Parameter:
    --------- 
        - df_comments: pandas Data Frame of comments, used for grouping and counting
        - df_submissions: pandas Data Frame of submissions, filtered dowm
        - submission_id_col: column of submission id's (in comments_df) used to group
        - comment_id_col: column of comment id's (in comments_df) used to calculate mean

    Return:
    ------
        - df_submissions_filtered: pandas Data Frame of submissions with above average comments
        - df_comments_filtered: pandas Data Frame of comments with parent in relevant_submissions
    """
    
    relevant_submissions = pd.DataFrame(df_comments.groupby(submission_id_col)[comment_id_col].count()).reset_index().rename(columns={submission_id_col : 'submission_id', comment_id_col : 'count'})
    print("Mean nr. of comments per submission", relevant_submissions['count'].mean())
    print("Nr. of submission, that I have comments for", len(relevant_submissions))

    #relevant_submissions = relevant_submissions[relevant_submissions['count'] >= relevant_submissions['count'].mean()]
    

    # filter relevant submissions with > avg nr of comments
    df_submissions_filtered = df_submissions[df_submissions['id'].isin(relevant_submissions['submission_id'])].reset_index(drop=True)
    
    if(len(df_submissions_filtered) == len(relevant_submissions)):
        print("All submissions found")
    else:
        print("Some submissions could not be found")

    # filter submissions with non empty or removed self-text
    df_submissions_filtered = df_submissions_filtered[df_submissions_filtered['selftext'] != "[removed]"].reset_index(drop=True)
    df_submissions_filtered = df_submissions_filtered[df_submissions_filtered['selftext'] != "[deleted]"].reset_index(drop=True)
    df_submissions_filtered = df_submissions_filtered[df_submissions_filtered['selftext'] != "removed"].reset_index(drop=True)
    df_submissions_filtered = df_submissions_filtered[df_submissions_filtered['selftext'] != "deleted"].reset_index(drop=True)
    df_submissions_filtered = df_submissions_filtered[df_submissions_filtered['selftext'] != ""].reset_index(drop=True)
    df_submissions_filtered = df_submissions_filtered[df_submissions_filtered['selftext'].notna()].reset_index(drop=True)
    
    print("Removing empty submissions deleted", len(relevant_submissions) - len(df_submissions_filtered), "submissions")
    print("Kept", len(df_submissions_filtered), "submissions")
    
    # filter comments, with parent in relevant_submissions_id
    df_comments_filtered = df_comments[df_comments[submission_id_col].isin(df_submissions_filtered['id'])].reset_index(drop=True)
    
    print("Nr. of comments, whose submission is retaied", len(df_comments_filtered))
    print("Share", len(df_comments_filtered) / len(df_comments))


    return df_submissions_filtered, df_comments_filtered
    

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
def match_reply_comment_submission(replies, comments, submissions, merge_type, subreddit):

    """
    Function to match first level replies to the still-relevant comments, and these to the still-relevant submissions.
    Then, filter for subreddit and adjust date

    Parameters:
    -----------
    	- replies: all first level replies
        - comments: all relevant comments 
        - submissions: all relevant submissions
        - merge_type: how-argument in pd.merge() function
        - subreddit: subreddit 

    Returns:
    --------
        - df_result: the final data frame of submissions, comments and replies
    """

    replies_temp = replies[replies['type'] == "reply_1"][['parent_id', 'id', 'body', 'subreddit', 'created_utc', 'author', 'ups', 'score', 'downs']].reset_index(drop= True)
    replies_temp.rename(columns = {'parent_id' : 'msg_id_parent', 
                           'id' : 'msg_id_child',
                           'body' : 'body_child', 
                           'created_utc' : 'exact_time_child',
                           'author' : 'author_child',
                           'ups' : 'ups_child',
                           'score' : 'score_child',
                           'downs': 'downs_child'}, inplace=True)
    

    comments_temp = comments[comments['id'].isin(replies_temp['msg_id_parent'])]
           

    comments_temp = comments_temp[['parent_id', 'id', 'body', 'subreddit', 'created_utc', 'author', 'ups', 'score', 'downs']].reset_index(drop= True)
    comments_temp.rename(columns = {'parent_id' : 'submission_id', 
                            'id' : 'msg_id_parent',
                            'body' : 'body_parent', 
                            'created_utc' : 'exact_time_parent',
                            'author' : 'author_parent',
                            'ups' : 'ups_parent',
                            'score' : 'score_parent',
                            'downs': 'downs_parent'}, inplace=True)
    
    submissions_temp = submissions[submissions['id'].isin(comments_temp['submission_id'])]
            

    submissions_temp = submissions_temp[['id', 'selftext', 'subreddit', 'created_utc', 'author', 'ups', 'score']].reset_index(drop= True)
    submissions_temp.rename(columns = {'id' : 'submission_id',
                            'selftext' : 'submission_text', 
                            'created_utc' : 'exact_time_submission',
                            'author' : 'author_submission',
                            'ups' : 'ups_submission',
                            'score' : 'score_submission'}, inplace=True)



    df_result = replies_temp.merge(comments_temp[["submission_id", "msg_id_parent", 'body_parent', "author_parent", 
                        "ups_parent", "score_parent", "downs_parent", 'exact_time_parent']], on = "msg_id_parent", how = merge_type).merge(
                            submissions_temp[["submission_id", "submission_text", "author_submission", 
                        "ups_submission", "score_submission", 'exact_time_submission']], on = 'submission_id', how = merge_type)

    df_result = df_result.drop_duplicates(subset = ["body_child", "body_parent", "author_parent", "author_child"],
                                          keep = 'last').reset_index(drop = True)
    
    print("Nr. of unique replies", len(set(df_result['msg_id_child'])))
    print("Nr. of unique comments", len(set(df_result['msg_id_parent'])))
    print("Nr. of unique submissions", len(set(df_result['submission_id'])))
        
    data_subreddit = df_result[df_result['subreddit'] == subreddit]

    # reset index
    data_subreddit.reset_index(inplace=True, drop = True)

    # recode columns
    data_subreddit['exact_time_parent'] = pd.to_datetime(data_subreddit['exact_time_parent'], unit = 's')
    data_subreddit['exact_time_child'] = pd.to_datetime(data_subreddit['exact_time_child'], unit = 's')
    data_subreddit['exact_time_submission'] = pd.to_datetime(data_subreddit['exact_time_submission'], unit = 's')

    if 'label' in data_subreddit.columns:
        data_subreddit['label'] = data_subreddit['label'].replace({0 : 'neg', 1 : 'neu', 2: 'pos'})


    return df_result 




#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

def preprocess_texts(data, length):

    """
    Function to preprocess the texts of the interactions-df.
    * First, remove line breaks, multiple spaces and replace "&amp" and "&gt"
    * Then, remove user-mentions and URL-links
    * Then, remove all symbols, that are not alphanumeric, i.e. smileys (?)

    Parameters:
    -----------


    Return:
    -------
        - 
    
    """
    
    OG_len = len(data)
    # filter deleted texts 
    data = data[data['body_child'] != "[deleted]"]
    data = data[data['body_child'] != "[removed]"]
    data = data[data['body_child'] != "deleted"]
    data = data[data['body_child'] != "removed"]
    data = data[data['body_child'] != ""]
    data = data[data['body_child'].notna()]

    data = data[data['body_parent'] != "[deleted]"]
    data = data[data['body_parent'] != "[removed]"]
    data = data[data['body_parent'] != "deleted"]
    data = data[data['body_parent'] != "removed"]
    data = data[data['body_parent'] != ""]
    data = data[data['body_parent'].notna()]

    data = data[data['submission_text'] != "[deleted]"]
    data = data[data['submission_text'] != "[removed]"]
    data = data[data['submission_text'] != "deleted"]
    data = data[data['submission_text'] != "removed"]
    data = data[data['submission_text'] != ""]
    data = data[data['submission_text'].notna()]
    
    print("Deleting all interactions with empty texts removed", OG_len - len(data), "interactions. Share: ", (OG_len - len(data))/OG_len)
    print("Kept", len(data), "interactions.")

    # filter deleted authors
    OG_len = len(data)
    data = data[data['author_parent'] != "[deleted]"]
    data = data[data['author_child'] != "[deleted]"]
    data = data[data['author_submission'] != "[deleted]"]

    print("Deleting empty authors deleted", OG_len - len(data), "interactions. Share: ", (OG_len - len(data))/OG_len)
    print("Kept", len(data), "interactions.")

    # filter texts that have been written by a bot = appear too often
    # should already be removes, as body_parent and authors will be deleted/removed
    # do not go over threshold, because a comment, who received just a loot of replies, could be dropped
    
    #text_counts_child = data['body_child'].value_counts()
    #texts_child_remove = text_counts_child[text_counts_child > threshold_child].index
    #text_counts_parents = data['body_parent'].value_counts()
    #texts_parents_remove = text_counts_parents[text_counts_parents > threshold_parent].index
    #data = data[~data['body_child'].isin(texts_child_remove)]
    #data = data[~data['body_parent'].isin(texts_parents_remove)]

    
    data = data.reset_index(drop = True)

    # cleaning
    data['body_child'] = data['body_child'].str.replace(r'\n', " ", regex = True) # remove \n
    data['body_child'] = data['body_child'].str.replace(r' +', " ", regex = True) # remove multiple spaces
    data['body_child'] = data['body_child'].str.replace(r'u/\w+', "", regex = True) # remove user mentions
    data['body_child'] = data['body_child'].str.replace(r'r/\w+', "", regex = True) # remove subreddit mentions
    data['body_child'] = data['body_child'].str.replace(r'/r/\w+', "", regex = True) # remove subreddit mentions
    data['body_child'] = data['body_child'].str.replace(r'https?://\S+', "", regex = True) # remove URLS
    data['body_child'] = data['body_child'].str.replace(r'&amp', "and", regex = True) # replace &amp
    data['body_child'] = data['body_child'].str.replace(r'&gt', ">", regex = True) # replace &gt
    #data['body_child'] = data['body_child'].str.replace(r'([@])', r' \1 ', regex = True) # separate @ from user mentions
    data['body_child'] = data['body_child'].str.replace(r"[^a-zA-Z0-9\s.,!?'-]", '', regex = True) # replace special characters


    data['body_parent'] = data['body_parent'].str.replace(r'\n', " ", regex = True) # remove \n
    data['body_parent'] = data['body_parent'].str.replace(r' +', " ", regex = True) # remove multiple spaces
    data['body_parent'] = data['body_parent'].str.replace(r'u/\w+', "", regex = True) # remove user mentions
    data['body_parent'] = data['body_parent'].str.replace(r'r/\w+', "", regex = True) # remove subreddit mentions
    data['body_parent'] = data['body_parent'].str.replace(r'/r/\w+', "", regex = True) # remove subreddit mentions
    data['body_parent'] = data['body_parent'].str.replace(r'https?://\S+', "", regex = True) # remove URLS
    data['body_parent'] = data['body_parent'].str.replace('&amp', "and", regex = True) # replace &amp
    data['body_parent'] = data['body_parent'].str.replace('&gt', ">", regex = True) # replace &gt
    #data['body_parent'] = data['body_parent'].str.replace(r'([@])', r' \1 ', regex = True) # separate @ from user mentions
    data['body_parent'] = data['body_parent'].str.replace(r"[^a-zA-Z0-9\s.,!?'-]", '', regex = True) # replace special characters

    data['submission_text'] = data['submission_text'].str.replace(r'\n', " ", regex = True) # remove \n
    data['submission_text'] = data['submission_text'].str.replace(r' +', " ", regex = True) # remove multiple spaces
    data['submission_text'] = data['submission_text'].str.replace(r'u/\w+', "", regex = True) # remove user mentions
    data['submission_text'] = data['submission_text'].str.replace(r'r/\w+', "", regex = True) # remove subreddit mentions
    data['submission_text'] = data['submission_text'].str.replace(r'/r/\w+', "", regex = True) # remove subreddit mentions
    data['submission_text'] = data['submission_text'].str.replace(r'https?://\S+', "", regex = True) # remove URLS
    data['submission_text'] = data['submission_text'].str.replace('&amp', "and", regex = True) # replace &amp
    data['submission_text'] = data['submission_text'].str.replace('&gt', ">", regex = True) # replace &gt
    #data['submission_text'] = data['submission_text'].str.replace(r'([@])', r' \1 ', regex = True) # separate @ from user mentions
    data['submission_text'] = data['submission_text'].str.replace(r"[^a-zA-Z0-9\s.,!?'-]", '', regex = True) # replace special characters

    # filter too short texts
    replies_to_remove = []
    comments_to_remove = []

    for idx, row in data.iterrows():
        reply_words = row['body_child'].split()
        comment_words = row['body_parent'].split()
        if len(reply_words) >= length:
            pass
        else:
            replies_to_remove.append(idx)
        if len(comment_words) >= length:
            pass
        else:
            comments_to_remove.append(idx)

    OG_len = len(data)
    data = data.loc[~data.index.isin(replies_to_remove)]
    data = data.loc[~data.index.isin(comments_to_remove)]
    data = data.reset_index(drop = True)

    print(f"Length {length} removes {OG_len - len(data)} interactions") 
    
    print(f"Kept {len(data)} interactions") 
    

    OG_len = len(data)
    data = data.drop_duplicates(subset = ["body_child", "body_parent", "author_parent", "author_child"],
                                          keep = 'last').reset_index(drop = True)
    print("Drop duplicates (in parent & child text and author) removed", OG_len - len(data), "interactions. Share: ", (OG_len - len(data))/OG_len)
    print("Kept", len(data), "interactions.")

    OG_len = len(data)
    # need to check again, after cleaning
    data = data[data['body_child'] != ""].reset_index(drop = True)
    data = data[data['body_parent'] != ""].reset_index(drop = True)
    data = data[data['submission_text'] != ""].reset_index(drop = True)
    data = data[data['body_child'] != " "].reset_index(drop = True)
    data = data[data['body_parent'] != " "].reset_index(drop = True)
    data = data[data['submission_text'] != " "].reset_index(drop = True)
    
    print("After text cleaning", OG_len - len(data), "interactions had empty texts and got removed. Share: ", (OG_len - len(data))/OG_len)
    print("Kept", len(data), "interactions.")


    return data



















#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
def import_merge_predicted_labels(label_file_path, df, scale = False):

    """
    Function to import labels created with i.e. Llama.
    File needs to contain msg_id_parent, msg_id_child and predictions column
    Imports and merges to the df

    Parameters:
    -----------
        - label_file_path: str
            path to csv-file with the predicted labels.
            csv needs to contain "msg_id_parent", "msg_id_child" and "prediction"
        - df: DataFrame
            pandas data frame containing the comment and reply pairs

    Returns:
    --------
        - result: the df, now with predicted labels (pos, neg, neu)
    """

    labels = pd.read_csv(label_file_path)

    result = df.merge(labels, left_on = ["msg_id_parent", "msg_id_child"], right_on = ["msg_id_parent", "msg_id_child"])
    
    if scale:
        result.rename(columns={'predictions' : 'probability'}, inplace = True)
        # transform to numeric
        result['probability'] = pd.to_numeric(result['probability'], errors='coerce')
        # non-invalid
        print("All rows: ", len(result))
        print("Invalid classifications: ", len(result[result['probability'].isna()]))
        print("Share of lost rows: ", len(result[result['probability'].isna()]) / len(result))
        result = result[~result['probability'].isna()].reset_index(drop = True)
    else:
        result.rename(columns={'predictions' :'label'}, inplace = True)
        # recode to fit with predefined functions
        result['label'] = result['label'].replace({'no_disagreement' : 'pos', 'disagree' : 'neg'})

    

    return result

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
def extract_labels_from_probability(df, border_disagree):
    labels_predicted = []
    
    for idx, row in df.iterrows():
    
        if row['probability'] >= border_disagree:
            labels_predicted.append('disagree')
        else:
            labels_predicted.append('no_disagreement')

    df['label'] = labels_predicted
    # recode to fit with predefined functions
    df['label'] = df['label'].replace({'no_disagreement' : 'pos', 'disagree' : 'neg'})

    
    return df

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
def aggregate_interactions_directed(df, two_labels = True, pos_neu = True):
    """
    Function that aggregates a df containing individual interactions (each single comment-reply interaction)
    into aggregated relations (which preserve the oder of whom answered whom).

    Parameters:
    ----------
        - df : the Data Frame containing at least 'author_parent', 'author_child' and 'label'
        - two_labels: boolean, indicating whether two or three labels have been predicted. Default is two labels
        - pos_neu : boolean, indicating whether positive and neutral relations should be aggregted into positives

    Returns:
    --------
        - df_agg: the Data Frame containing the aggregated counts of i.e. 3 x positive and 2 x neutral 
            interaction between user_1 and user_2 (directed)

    """

    
    df_agg = df.groupby(['author_parent', 'author_child'])['label'].value_counts().unstack(fill_value=0).reset_index().rename_axis(None, axis=1)

    
    if two_labels:
        # add column of zeros, all further functions written for neu
        df_agg['neu'] = 0
        #print(df_agg)
     

    #else: # three labels
    if pos_neu == True:
        # extract aggregated edges
        df_agg.loc[df_agg['pos'] + df_agg['neu'] == df_agg['neg'], 'edge'] = 0 # ambiguous
        df_agg.loc[df_agg['pos'] + df_agg['neu'] > df_agg['neg'], 'edge'] = 1 # more positive
        df_agg.loc[df_agg['neg'] > df_agg['pos'] + df_agg['neu'], 'edge'] = -1 # more negative
    
    else:
        # extract aggregated edges
        df_agg.loc[df_agg['pos'] == df_agg['neg'], 'edge'] = 0 # ambiguous
        df_agg.loc[df_agg['pos'] > df_agg['neg'], 'edge'] = 1 # more positive
        df_agg.loc[df_agg['neg'] > df_agg['pos'], 'edge'] = -1 # more negative


    df_agg['edge'] = df_agg['edge'].astype(int)

    # get number of interaction
    if two_labels:
        df_agg['#interact'] = df_agg[['pos', 'neg']].sum(axis=1)
    else:
        df_agg['#interact'] = df_agg[['pos', 'neg', 'neu']].sum(axis=1)

    # only keep interactions where parent and child are different --> removes 33 edges
    df_agg = df_agg[df_agg['author_child'] != df_agg['author_parent']]

    return df_agg

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

def get_counts_and_shares(df, column):

    return df[column].value_counts().to_frame('Count').join(df[column].value_counts(normalize=True).to_frame('%')).reset_index()

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

def find_max(row):

    """ 
    Small helper function to determine the maximal count of either neg, neu or pos. Finds the max()
    and returns the reffering column-name.
    If two interaction types are the max(), 'neutral' is returned.
    
    """
    
    max_count = max(row['neg'], row['neu'], row['pos'])
    max_cols = [col for col in ['neg', 'neu', 'pos'] if row[col] == max_count]

    if len(max_cols) == 1:
        return max_cols[0]
    else:
        return 'amb'
    

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




def build_undirected(df, p_init = 1, n_init = 1, pos_th = 0.6, neg_th = 0.4, pos_neu = True):

    """ 
    Function to aggregate directed relations into undirected.

    First, bi-directional relationships are found, (self-replies are removed) and unique pairs are kept.
    Then, the counts of interaction-type are added per pair (independently of the direction). 
    Depending on pos_neu-value, positives and neutrals are aggregated
    
    The highest count is taken for the final edge (in case of ambigouty, neutral).

    For uni-directional relations, the maximum of interaction type is determined (optionally after pos + neu) and stored as the final edge 
    (again, ambiguous cases result in neutral)

    Add 'direction' column and join both df's

    Parameters:
    -----------
        - df: the Data Frame, containing the aggregated, but still idrected edges.
        - p_init: prior for the positive edges, default = 1
        - n_init: prior for the negative edges, default = 1

    Returns:
    --------
        - df_result: the concatinated df of bi- and uni-directional, aggregated relations 
        (including all three types of interactions)
    
    """

    # copy and self merge
    df_copy = df.copy()
    df_merge = df.merge(df_copy, left_on=['author_child','author_parent'], right_on=['author_parent','author_child'], suffixes = ['1','2'], how = 'left')

    # Bi-directional relations -------------------------------------------------------------------------------------------------
    # extract bi-directional connections only
    df_bi = df_merge.dropna()
    #print(len(df_bi))

    # drop rows, where people replied to their own comments --> should not be included anymore, just for sainity
    df_bi = df_bi[df_bi['author_child1'] != df_bi['author_parent1']]
   
    # change to integer
    df_bi.loc[:, ['neg2', 'neu2', 'pos2', 'edge2', '#interact2']] = df_bi[['neg2', 'neu2', 'pos2', 'edge2', '#interact2']].astype(int)

    # create pair column, to indicate the sorted pair of users, based on that, drop duplicates
    df_bi['pair'] = df_bi.apply(lambda row: tuple(sorted([row['author_child1'], row['author_parent1']])), axis=1)

    # sainity check
    #pair_counts = df_bi['pair'].value_counts()
    #print("Pairs with unexpected counts:", pair_counts[pair_counts != 2])

    df_bi = df_bi.drop_duplicates(subset='pair').drop(columns='pair')

    # some print statements
    print('Nr. of bi-directional relations: ' + str(len(df_bi)))
    print('Nr. of agreeing bi-directional relations: ' + str(len(df_bi[df_bi['edge1'] == df_bi['edge2']])))
    print('Nr. of disagreeing bi-directional relations: ' + str(len(df_bi[df_bi['edge1'] != df_bi['edge2']])))

    # sum counts of edge-type and overall interactions
    if pos_neu == True:
        df_bi.loc[:, 'neg'] = df_bi[['neg1', 'neg2']].sum(axis = 1, skipna = True)
        df_bi.loc[:, 'pos'] = df_bi[['neu1', 'neu2', 'pos1', 'pos2']].sum(axis = 1, skipna = True)
        df_bi.loc[:, 'neu'] = 0
        df_bi.loc[:, 'interact'] = df_bi[['#interact1', '#interact2']].sum(axis = 1, skipna = True)
    
    else:
        df_bi.loc[:, 'neg'] = df_bi[['neg1', 'neg2']].sum(axis = 1, skipna = True)
        df_bi.loc[:, 'neu'] = df_bi[['neu1', 'neu2']].sum(axis = 1, skipna = True)
        df_bi.loc[:, 'pos'] = df_bi[['pos1', 'pos2']].sum(axis = 1, skipna = True)
        df_bi.loc[:, 'interact'] = df_bi[['#interact1', '#interact2']].sum(axis = 1, skipna = True)
        

    # select relevant columns 
    df_bi = df_bi[['author_parent1', 'author_child1', 'neg', 'neu', 'pos', 'interact']]

    # rename
    df_bi.rename(columns = {'author_parent1' : 'user_1', 'author_child1' : 'user_2'}, inplace = True)

    df_bi['final_edge'] = df_bi.apply(find_max, axis = 1)

    df_bi['final_edge_num'] = df_bi['final_edge'].replace({'neg' : -1, 'amb' : 0, 'pos' : 1})



    # Uni-directional relations -------------------------------------------------------------------------------------------------
    df_uni = df_merge[df_merge['author_parent2'].isna()]

    df_uni = df_uni[['author_parent1', 'author_child1', 'neg1', 'neu1', 'pos1', '#interact1']]
    df_uni.rename(columns = {'author_parent1' : 'user_1', 'author_child1' : 'user_2', 'neg1' : 'neg', 'neu1' : 'neu', 'pos1' : 'pos', '#interact1' : 'interact'}, inplace = True)

    print('Nr. of uni-directional relations: ' + str(len(df_uni)))

    if pos_neu == True:
        df_uni.loc[:, 'pos'] = df_uni[['neu','pos']].sum(axis = 1, skipna = True) 
        df_uni.loc[:, 'neu'] = 0
    else:
        None

   
    df_uni['final_edge'] = df_uni.apply(find_max, axis = 1)

    df_uni['final_edge_num'] = df_uni['final_edge'].replace({'neg' : -1, 'amb' : 0, 'pos' : 1})

    # concat with type
    df_bi['direction'] = 'bi'
    df_uni['direction'] = 'uni'
    df_result = pd.concat([df_bi, df_uni])

    # ----------------------------------------------------
    # compute variance and adapt beta prior

    # add priors
    df_result['neg_adj'] = df_result['neg'] + n_init
    df_result['pos_adj'] = df_result['pos'] + p_init
    df_result['interact_adj'] = df_result['pos_adj'] + df_result['neg_adj']

    # share of pos edges --> mean probability, how certain are we?
    df_result['mean_pb'] = df_result['pos_adj'] / df_result['interact_adj']

    # variance with Bayesian prior
    # (share pos * share neg) / number of interact
    df_result['variance'] = (df_result['mean_pb'] * (1 - df_result['mean_pb'])) / df_result['interact_adj']

    # (share pos * share neg) / (pos + neg)
    #epsilon = 0 #1e-10
    #df_result['old_var_Epsilon'] = (df_result['pos'] * df_result['neg']) / (((df_result['pos'] + df_result['neg'])**2 + epsilon) * (df_result['pos'] + df_result['neg'] + 1 + epsilon))
    
    # Emma
    df_result['old_var'] = (df_result['pos']*df_result['neg'])/(((df_result['pos']+df_result['neg'])**2)*(df_result['pos']+df_result['neg']+1))
    
    
    df_result.loc[df_result['mean_pb'] >= pos_th, 'edge_adj'] = +1
    df_result.loc[df_result['mean_pb'] <= neg_th, 'edge_adj'] = -1
    
    return df_result

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


def measures(type_of_edge):
    
    counter = collections.Counter(type_of_edge)
    total = counter[3]+ counter[1]+ counter[-3]+ counter[-1]
    frustration_negative = counter[1] # negative within
    frustration_positive =  counter[-1] # positive between 
    total_negative = counter[1] + counter[-3] # negative within + negative between
    internal = counter[3]+ counter[1] # positive within + negative within
    coh = (counter[3]/(counter[1]+counter[3])) # share of pos within / all within
    div = counter[-3]/(counter[-1]+counter[-3]) # share of neg between / all between
    fff = counter[1]+counter[-1] # negative within, positive between 
    balance = (1- (fff/(total/2))) # 1 - (frustrated/(all/2))
    
    return(balance, total, frustration_negative, frustration_positive, total_negative, internal, coh, div)