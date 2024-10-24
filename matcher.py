import pandas as pd

import numpy as np

 

 

# Load the master sheet

master_sheet = pd.read_csv('/Users/hsm/Desktop/mouse_behavior/github/data/MasterSheet_Experiments_101424.csv')

 

# Load the behavior sheet

behavior_sheet = pd.read_csv('/Users/hsm/Desktop/mouse_behavior/github/data/MWM_behavior_101424_hsm_2.csv', index_col='AnimalID')

 

master_sheet = master_sheet.apply(lambda x: x.str.strip().str.replace('-', '_') if x.dtype == "object" else x)

behavior_sheet = behavior_sheet.apply(lambda x: x.str.strip().str.replace('-', '_') if x.dtype == "object" else x)

 

 

 

 

# Create a new variable for the updated sheet based on the behavior sheet

updated_behavior_sheet = behavior_sheet.copy()

 

# Create a column 'Lifestyle' in the updated behavior sheet and set it to NaN initially

updated_behavior_sheet['Final Weight'] = None

 

# Iterate over each row in the updated behavior sheet

for idx, row in updated_behavior_sheet.iterrows():

    # Find the corresponding row in the master sheet where 'Handling' matches 'AnimalID' from the updated behavior sheet

    matching_row = master_sheet[master_sheet['BadeaID'] == idx]

   

    # If a match is found, copy the 'Lifestyle' value to the updated behavior sheet

    if not matching_row.empty:

        updated_behavior_sheet.at[idx, 'Final Weight'] = matching_row.iloc[0]['Final Weight']

 

# Reset the index to bring 'AnimalID' back as a column

updated_behavior_sheet.reset_index(inplace=True)

 

 

updated_behavior_sheet.to_csv('/Users/hsm/Desktop/mouse_behavior/github/data/MWM_behavior_102424_hsm.csv', index=False)

 