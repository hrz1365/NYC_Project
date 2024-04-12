# Header

# Putpose: This script is for generating projections of built-up areas over time
# Author : Hamidreza Zoraghein
# Date   : 6/6/2023



# Pacakages 
import sys
import os
import tensorflow as tf
import geopandas as gpd
sys.path.append(os.path.join('.', 'scripts', 'python'))
import model_definition 


# Paths and Inputs
inputs_dir   = os.path.join('.', 'inputs', 'dem_decomposition')
outputs_dir  = os.path.join('.', 'outputs', 'decomposition')
dem_ts_file  = os.path.join(inputs_dir, 'char_ts.gpkg')

scenario    = 'SSP1'
dem_char    = 'age'
dem_ts_file = dem_ts_file.replace('char', dem_char)

if dem_char == 'race':
    pred_years   = [str(year) for year in range(1990, 2030, 10)]
    target_year  = int(pred_years[-1]) + 10
    chars = ['ASO', 'HIS', 'NHB', 'NHW']
    prediction_features  = [i + char + year for year in pred_years for char in chars for i in ['P', 'HH', 'LL']]
    target_features      = ['P' + char + str(target_year) for char in chars]


elif dem_char == 'age':
    pred_years  = [str(year) for year in range(1980, 2030, 10)]
    target_year = int(pred_years[-1]) + 10
    chars = ['0', '20', '40', '60', '80']
    prediction_features  = [i + char + '_' + year for year in pred_years for char in chars for i in ['pop', 'HH', 'LL']]
    target_features      = ['pop' + char + '_' + str(target_year) for char in chars]


# Main Code
# Extract the feature space and features for each characteristic
features_spatial = gpd.read_file(dem_ts_file)
features_df      = features_spatial.drop(columns = 'geometry').loc[:, ['ID'] + prediction_features]
features_df.fillna(0, inplace = True)


model_outputs_path  = os.path.join(outputs_dir, 'model_outputs')
cur_model = tf.keras.models.load_model(os.path.join(model_outputs_path, dem_char))

# Extract the number of time steps and features per time step
num_time_steps = len(pred_years)
num_features   = int(len(prediction_features) / len(pred_years))
num_target_variables = len(target_features)


for target_year in range(2030, 2110, 10):
    
    if target_year == 2030:
        model_obj   = model_definition.PredictionModel(features_df, num_time_steps, num_features, num_target_variables)
    else:
        features_df = model_definition.generate_feature_space(features_df, target_year, chars, predictions)
        model_obj   = model_definition.PredictionModel(features_df, num_time_steps, num_features, num_target_variables)
    

    predictions = model_obj.model_predict(cur_model)
    predictions[predictions < 0] = 0
    predictions[predictions > 1] = 1


    features_spatial[['P' + char + str(target_year) for char in chars]] = predictions

    print('target year {} is done!'.format(target_year))


features_spatial.to_file(os.path.join(outputs_dir, dem_char + '_' + scenario + '.gpkg'),
                         driver='GPKG')

