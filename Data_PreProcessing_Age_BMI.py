def separate_proteins(filename):
    training_dataset = pd.read_csv(filename)
    print("Original Data for " + filename)
    # Drop unnecessary columns
    training_dataset = training_dataset.drop(
        ['Unnamed: 0', 'Threshold', 'Desc', 'OlinkID', 'UniProt', 'Panel', 'Desc_Column', 'term', 'df',
         'sumsq', 'meansq', 'statistic'], axis=1)
    # training_dataset = training_dataset.drop(['Unnamed: 0','Threshold','OlinkID','UniProt','Panel','term','df','sumsq','meansq','statistic'], axis=1)

    training_dataframe = pd.DataFrame()
    # validation_dataframe = pd.DataFrame()

    # if(filename.find('train')>0):
    #    print('This is training file')
    # if training_dataframe.empty:
    #    training_dataframe = training_dataset
    #    print('creating empty dataframe and placing training_dataset into it')

    # else:
    #    print('This is validation file')

    return (list(training_dataset['Assay'].values), list(training_dataset['Tissue_Initials'].values))

def build_ML_data_prep(filename, df, age):
    print("Processing file " + filename)
    training_dataset1 = pd.read_csv(filename)
    for protein in training_dataset1['Assay']:
        # print(protein)
        if protein in df.columns:
            df.loc[df['AgeCat'] == age, [protein]] = 1
        else:
            df.loc[df['AgeCat'] == age, [protein]] = 0
    return (df)

def get_key_by_value(dictionary, target_value):
    for key, value in dictionary.items():
        if value == target_value:
            return key
    # If the value is not found, you can return a default value or raise an exception.
    # For example, you can return None if the value is not found.
    return None

import pandas as pd
import numpy as np
import re
import sys

Suffix = "AGE_BMI"

#Read MGH Olink ANOVA dataset for the referred category
category = ['age','BMI']
category_range_age = range(1,6)
category_range_BMI = range(0,5)

for i in category_range_age:
    print("Age category:", category_range_age[i-1])
print("\r")
for i in category_range_BMI:
    print("BMI category:", category_range_BMI[i])

import itertools

Pre_conditions_list = [1,2,3,4,5]
Pre_conditions_yes_no = [0, 1]
Age_list = [1, 2, 3, 4, 5]
BMI_list = [0, 1, 2, 3, 4]

# Generate combinations of three items
combinations = list(itertools.product(Pre_conditions_list,Pre_conditions_yes_no, Age_list, BMI_list))

# Print the combinations
#for combo in combinations:
#    print(combo)

combinations = [list(ele) for ele in combinations]
#print(combinations)

import glob, os
files_to_prepare = []
files_to_prepare_train = []

my_dict = { 1:'HTN',
           2:'HEART',
           3:'LUNG',
           4:'KIDNEY',
           5:'DIABETES'
          }

for i in my_dict:
    print(my_dict[i])
    files = '/Users/marcelo_palermo/Doutorado/R/Olink/Qualification/AGE_BMI_Significatives_Conditions/MGH_ANOVA_categories_' + my_dict[i] + '_*_Age_*_BMI_*.csv'
    listing = glob.glob(files)

    for filename in listing:
        if "cut" in filename:
            print("NOT INCLUDED - Found cut in ", filename)
        else:
            print(filename)
            files_to_prepare.append(filename)

file_test = "/Users/marcelo_palermo/Doutorado/R/Olink/Qualification/AGE_BMI_Significatives_Conditions/MGH_ANOVA_categories_DIABETES_1_Age_5_BMI_3.csv"
features_1 = re.findall(r'\d+', file_test)
print(features_1)
features_1_int = [int(numeric_string) for numeric_string in features_1]
print(features_1_int)

for key, substring in my_dict.items():
    print(substring)
    if substring in file_test:
        print("Found " + substring)
        print("Key is " + str(key))
    else:
        print("Not found")

array_protein_names = []
array_each_file = []
X = []
X_each = []
y_each = []
y = []

array_organ_names = []
array_each_organ = []
X_organ = []
X_organ_each = []
y_organ = []
y_organ_each = []

for file in (files_to_prepare):

    for key, substring in my_dict.items():
        if substring in file:
            condition_name = key

    array_each_file, array_each_organ = separate_proteins(file)
    print("len(array_each_file) = ", len(array_each_file))
    print("len(array_each_organ) = ", len(array_each_organ))

    ####### SEPARATE_PROTEINS ---> SEPARATE_TISSUES (HERE) for Data Reduction

    #  print("Number of proteins in y", len(array_each_file))
    # print(file)
    # print(array_each_file)
    if len(array_each_file) != 0:
        print(file)
        feature = re.findall(r'\d+', file)
        feature = [int(numeric_string) for numeric_string in feature]
        feature.insert(0, condition_name)

        for i in range(0, len(array_each_file)):
            X_each.append(feature)
            y_each.append(array_each_file[i])

        X.append(feature)
        y.append(array_each_file)

    #     print("X,y,y_organ =", X, y, y_organ)
    array_protein_names = array_protein_names + array_each_file

    if len(array_each_organ) != 0:
        print(file)
        feature = re.findall(r'\d+', file)
        feature = [int(numeric_string) for numeric_string in feature]
        feature.insert(0, condition_name)

        for i in range(0, len(array_each_organ)):
            X_organ_each.append(feature)
            y_organ_each.append(array_each_organ[i])

        X_organ.append(feature)
        y_organ.append(array_each_organ)

    #     print("X,y,y_organ =", X, y, y_organ)
    array_organ_names = array_organ_names + array_each_organ

#  print("\n\n\n")
#  print("X = ", X, len(X))
#  print("y = ", y, len(y))
#  print("y_organ = ",y_organ, len(y_organ))
#  print("\n\n\n")
#  print("X_each = ", X_each, len(X_each))
#  print("y_each = ", y_each, len(y_each))
#  print("y_organ_each = ", y_organ_each, len(y_organ_each))


print(X, y, y_organ)

# Find the combinations with no proteins and fill the gaps in _X,y_
combinations_set = set(map(tuple, combinations))
X_set = set(map(tuple, X))
differences_set = combinations_set.symmetric_difference(X_set)

combinations_set_organ = set(map(tuple, combinations))
X_set_organ = set(map(tuple, X_organ))
differences_set_organ = combinations_set.symmetric_difference(X_set_organ)

differences_list = [list(ele) for ele in differences_set]
differences_list = sorted(differences_list)

differences_list_organ = [list(ele) for ele in differences_set_organ]
differences_list_organ = sorted(differences_list_organ)

for item in differences_list:
    X.append(item)
    y.append('')

for item in differences_list_organ:
    X_organ.append(item)
    y_organ.append('')


for i in range(0, len(y_organ)-1):
    print("---- ",i,X[i],y_organ[i])

for i in range(0, len(y_organ)-1):
    print("---- BEFORE: ",i,X[i],y_organ[i], len(y_organ[i]))
    y_organ[i] = list(set(y_organ[i]))
    print("---- AFTER: ",i,X[i],y_organ[i], len(y_organ[i]))

from sklearn.preprocessing import LabelEncoder
from sklearn.preprocessing import MultiLabelBinarizer

label_encoder = LabelEncoder()
encoded_y_each = label_encoder.fit_transform(y_each)
print(encoded_y_each)
mlb = MultiLabelBinarizer()
binary_y_each = mlb.fit_transform([[label] for label in encoded_y_each])

# Print the binary labels
print(binary_y_each)


## from sklearn.preprocessing import MultiLabelBinarizer, LabelEncoder
mlb = MultiLabelBinarizer()
y_transformed_initial = mlb.fit_transform(y)
#y_organ_transformed_initial = mlb.fit_transform(y_organ)
df_y_transformed = pd.DataFrame(y_transformed_initial,columns=mlb.classes_)
#df_y_organ_transformed = pd.DataFrame(y_organ_transformed_initial,columns=mlb.classes_)

print(df_y_transformed.shape)

encoded_y_organ_each = label_encoder.fit_transform(y_organ_each)
print(encoded_y_organ_each)
mlb = MultiLabelBinarizer()
binary_y_organ_each = mlb.fit_transform([[label] for label in encoded_y_organ_each])

# Print the binary labels
print(binary_y_organ_each)


## from sklearn.preprocessing import MultiLabelBinarizer, LabelEncoder
mlb = MultiLabelBinarizer()
y_organ_transformed_initial = mlb.fit_transform(y_organ)
#y_organ_transformed_initial = mlb.fit_transform(y_organ)
df_y_organ_transformed = pd.DataFrame(y_organ_transformed_initial,columns=mlb.classes_)
#df_y_organ_transformed = pd.DataFrame(y_organ_transformed_initial,columns=mlb.classes_)

print(df_y_organ_transformed.shape)

df_y_transformed.to_csv('./DataCleansingResults/df_y_transformed_tissue.csv')

print(df_y_organ_transformed.head())

df_y_organ_transformed.to_csv('./DataCleansingResults/df_y_organ_transformed_tissue.csv')

df_X = pd.DataFrame(X,columns=['Condition','Condition_Yes_No','Age','BMI'])
print(df_X.head(100))
df_X_organ = pd.DataFrame(X_organ,columns=['Condition','Condition_Yes_No','Age','BMI'])
print(df_X_organ.head(100))

print(X_organ)

df_train = pd.concat([df_X,df_y_transformed], axis=1)

print(df_train)
df_train.to_csv('./DataCleansingResults/df_train_tissue.csv')

df_train_organ = pd.concat([df_X_organ,df_y_organ_transformed], axis=1)
print(df_train_organ)

df_train_organ.to_csv('./DataCleansingResults/df_train_organ_tissue.csv')

df_train_each = df_train.head(0)
print(df_train_each)

df_train_organ_each = df_train_organ.head(0)
print(df_train_organ_each)

print(y_each)
print(y_organ_each)

label_encoder = LabelEncoder()
encoded_y_each = label_encoder.fit_transform(y_each)
print(encoded_y_each)
mlb = MultiLabelBinarizer()
binary_y_each = mlb.fit_transform([[label] for label in encoded_y_each])

# Print the binary labels
print(binary_y_each)

encoded_y_organ_each = label_encoder.fit_transform(y_organ_each)
print(encoded_y_organ_each)
binary_y_organ_each = mlb.fit_transform([[label] for label in encoded_y_organ_each])

# Print the binary labels
print(binary_y_organ_each)

print(type(binary_y_each))
print(type(binary_y_organ_each))

train_array = np.hstack((X_each,binary_y_each))
train_organ_array = np.hstack((X_organ_each,binary_y_organ_each))
print(train_array)
print(train_organ_array)

df_train_each.iloc[:,:] = train_array
df_train_each = df_train_each.astype(int)
print(df_train_each.head(9000))

df_train_organ_each.iloc[:,:] = train_organ_array
df_train_organ_each = df_train_organ_each.astype(int)
print(df_train_organ_each.head(9000))

df_train_each.to_csv('./DataCleansingResults/df_train_each_tissue.csv')
df_train_organ_each.to_csv('./DataCleansingResults/df_train_organ_each_tissue.csv')

combinations_set = set(map(tuple, combinations))
X_set_each = set(map(tuple, X_each))
differences_set = combinations_set.symmetric_difference(X_set_each)

combinations_set_organ = set(map(tuple, combinations))
X_set_organ_each = set(map(tuple, X_organ_each))
differences_set_organ = combinations_set.symmetric_difference(X_set_organ_each)

differences_list = [list(ele) for ele in differences_set]
differences_list = sorted(differences_list)

differences_list_organ = [list(ele) for ele in differences_set_organ]
differences_list_organ = sorted(differences_list_organ)

print(differences_list)
print(df_train_each.head())

df_train_each_list = df_train_each.values.tolist()
print(df_train_each_list)

df_train_organ_each_list = df_train_organ_each.values.tolist()
print(df_train_organ_each_list)

desired_length = df_train_each.shape[1]

for i in range(0, len(differences_list) - 1):
    differences_list[i].extend([0] * (desired_length - len(differences_list[i])))
    df_train_each_list.append(differences_list[i])

desired_length_organ = df_train_organ_each.shape[1]
for i in range(0, len(differences_list_organ) - 1):
    differences_list_organ[i].extend([0] * (desired_length_organ - len(differences_list_organ[i])))
    df_train_organ_each_list.append(differences_list_organ[i])

print(len(df_train_each_list))

df_train_each = df_train_each.head(0)
df_train_each.iloc[:,:] = df_train_each_list
df_train_each = df_train_each.astype(int)

df_train_organ_each = df_train_organ_each.head(0)
df_train_organ_each.iloc[:,:] = df_train_organ_each_list
df_train_organ_each = df_train_organ_each.astype(int)

df_train_each.to_csv('./DataCleansingResults/df_train_each_with_zeroes_tissue.csv')
df_train_organ_each.to_csv('./DataCleansingResults/df_train_organ_each_with_zeroes_tissue.csv')





