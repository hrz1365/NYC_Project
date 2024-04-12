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
pred_years             = [str(year) for year in range(1990, 2030, 10)]
target_year            = int(pred_years[-1]) + 10
current_characteristic = 'ASO'
inputs_dir             = os.path.join('.', 'inputs', 'dem_decomposition')
race_ts_file           = os.path.join(inputs_dir, 'race_ts.gpkg')
outputs_dir            = os.path.join('.', 'outputs', 'decomposition')


# Main Code
# Extract the feature space and features for each characteristic
prediction_features  = [i + current_characteristic + year for year in pred_years for i in ['P', 'HH', 'LL']]
target_feature   = 'P' + current_characteristic + str(target_year)
features_spatial = gpd.read_file(race_ts_file)
features_df      = features_spatial.drop(columns = 'geometry').loc[:, ['ID'] + prediction_features]
features_df.fillna(0, inplace = True)


model_outputs_path  = os.path.join(outputs_dir, 'model_outputs')
cur_model = tf.keras.models.load_model(os.path.join(model_outputs_path, current_characteristic))

# Extract the number of time steps and features per time step
num_time_steps, num_features = len(pred_years), int(len(prediction_features) / len(pred_years))



for target_year in range(2030, 2110, 10):
    features_df = model_definition.generate_feature_space(features_df, current_characteristic, target_year)
    model_obj   = model_definition.PredictionModel(features_df, num_time_steps, num_features)

    predictions = model_obj.model_predict(cur_model)
    predictions[predictions < 0] = 0
    predictions[predictions > 1] = 1
    features_df['P' + current_characteristic + str(target_year)] = predictions

    features_spatial['P' + current_characteristic + str(target_year)] = predictions

    print('target year {} is done!'.format(target_year))


features_spatial.to_file(os.path.join(outputs_dir, current_characteristic + '.gpkg'),
                         driver='GPKG')


