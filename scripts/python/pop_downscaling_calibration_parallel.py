import os
import sys
import numpy as np
import pyreadr
import pandas as pd
import scipy.optimize
import multiprocessing
from pathos.multiprocessing import ProcessingPool as Pool


sys.path.append(os.path.join('.', 'scripts', 'python'))
import pop_downscaling_module_parallel as pdm



# Calibration inputs -- derived from user inputs
borough = 'Queens'
calibration_inputs_path = os.path.join('.', 'inputs', 'main_downscaling_inputs', 'calibration')
pop_fst_year  = os.path.join(calibration_inputs_path, borough, 'pop_grid_2010.tif') 
pop_snd_year  = os.path.join(calibration_inputs_path, borough, 'pop_grid_2020.tif') 
mask_raster   = os.path.join(calibration_inputs_path, borough, 'mask_raster.tif') 
point_indices = os.path.join(calibration_inputs_path, borough, 'within_indices.rds')  
point_coors   = os.path.join(calibration_inputs_path, borough, 'coors.csv') 


# Projection inputs
scenario               = 'SSP1'
neighborhood_dis       = 25000
projection_inputs_path = os.path.join('.', 'inputs', 'main_downscaling_inputs', 'projection')
aggregate_projections  = os.path.join(projection_inputs_path,
                                      'pop_projections_bor_agg_' + scenario  + '.csv')
first_pop_grid         = os.path.join(projection_inputs_path, borough, 'pop_grid_2020.tif') 
params_file            = os.path.join(projection_inputs_path, borough, 'parameters_' + scenario  + '.csv')


# Calibration outputs
try:
    calibration_outputs = os.path.join('.', 'outputs', 'population_downscaling', 'calibration', borough)
    os.makedirs(calibration_outputs)
except:
    print('The folder already exists!')


# Projection outputs
try:
    projection_outputs = os.path.join('.', 'outputs', 'population_downscaling', 'projection', scenario,
                                      borough)
    os.makedirs(projection_outputs)
except:
    print("The folder already exists!")




#=========================================================================================
def calibration(pop_fst_year, pop_snd_year, mask_raster, point_indices, point_coors, neighborhood_dis):

    # Define local variables    
    pop_files       = [] # List containing population grids
    population_1st  = {} # Dictionary containing population of each point in year 1
    population_2nd  = {} # Dictionary containing population of each point in year 2
    parameters_dict = {} # Dictionary storing calibration parameters
    

    ## baseline population grids
    pop_files.append(pop_fst_year)
    pop_files.append(pop_snd_year)

   
    #Populate the array containing mask values 
    points_mask = pdm.raster_to_array(mask_raster)
    

    # Read historical population grids into arrrays
    population_1st = pdm.raster_to_array(pop_files[0])        
    population_2nd = pdm.raster_to_array(pop_files[1])
    
    
    # All indices
    all_indices = pdm.all_index_retriever(pop_fst_year, ['row', 'column'])
    

    # Read indices of points that fall within the state boundary
    within_indices = pyreadr.read_r(point_indices)
    within_indices = pd.DataFrame(list(within_indices.values())[0])
    within_indices = list(within_indices.loc[:, 'index'].astype(int).values)


    # Calculate a distance matrix that serves as a template
    cut_off_meters = neighborhood_dis
    dist_matrix    = pdm.dist_matrix_calculator(within_indices[0], cut_off_meters, all_indices, point_coors)

         
    # Initial alpha values
    a_lower = -1.0
    a_upper = 1.0 
    
    # Initial beta values
    b_lower = 0
    b_upper = 1
            

    # Parameters to be used in optimization        
    params = (population_1st, population_2nd, points_mask, dist_matrix, within_indices)
    
    
    # Initialize the dataframe that will hold values of the brute force
    a_list = np.linspace(a_lower, a_upper, 10)
    b_list = np.linspace(b_lower, b_upper, 5)
    fst_results = pd.DataFrame(data={'a'        : np.repeat(a_list, len(b_list)).astype(np.float32),
                                     'b'        : np.tile(b_list, len(a_list)).astype(np.float32),
                                     'estimate' : np.full(len(a_list)*len(b_list), np.nan, dtype=np.float64)
                    }
                )
    
    # Run brute force to calculate optimization per grid point
    i = 0
    for a in a_list:
        for b in b_list:
            fst_results.loc[(fst_results['a'] == a) & (fst_results['b'] == b),
                            'estimate'] = pdm.pop_min_function((a, b), *params)
            print('{}th iteratin done'.format(i))
            i+=1
    

    # Save the current optimization file
    brute_force_csv = os.path.join(calibration_outputs, 'initial_values.csv')
    fst_results.to_csv(brute_force_csv)
        

    # Use the point with the minimum value as an initial guess for the second optimizer
    (a0, b0) = fst_results.loc[fst_results['estimate'].idxmin(), ['a', 'b']] 
    
    
    # Final optimization
    parameters = scipy.optimize.minimize(pdm.pop_min_function, x0 = (a0, b0), 
                                            args = params, method = 'SLSQP',
                                            options = {'disp' : True, 'eps': 0.01, 'ftol': 0.01},
                                            bounds = ((-2.0, 2.0), (-0.5, 2.0)))
    

    # Write the parameters to the designated csv file
    parameters_dict        = pd.DataFrame({'alpha':[parameters['x'][0]] * 9, 'beta':[parameters['x'][1]] * 9,
                                            'scenario':['SSP2'] * 9, 'year': list(range(2020, 2110, 10))})
    output_parameters_file = os.path.join(calibration_outputs, 'parameters_SSP2.csv')
    parameters_dict.to_csv(output_parameters_file)




#=========================================================================================       
def pop_projection(pop_start_year, mask_raster, aggregate_projections, point_indices,
                   params_file, point_coors, neighborhood_dis, scenario):

    
    # Define local variables
    cur_year  = [int(s) for s in pop_start_year.split("/")[-1][:-4].split("_") if s.isdigit()][0]
    proj_year = cur_year + 10
    borough_name = pop_start_year.split("/")[-2]
    
    # Population array in the first year
    population_1st_array = pdm.raster_to_array(pop_start_year) 

    # Mask array
    points_mask = pdm.raster_to_array(mask_raster)
    
    # All indices
    all_indices = pdm.all_index_retriever(pop_start_year, ["row", "column"])
    

    # Read indices of points that fall within the state boundary
    within_indices = pyreadr.read_r(point_indices)
    within_indices = pd.DataFrame(list(within_indices.values())[0])
    within_indices = list(within_indices.loc[:, 'index'].astype(int).values)
        

    # Calculate a distance matrix that serves as a template
    cut_off_meters = neighborhood_dis
    dist_matrix    = pdm.dist_matrix_calculator(within_indices[0], cut_off_meters, all_indices, point_coors)
    

    # Number of columns of population grid required to derive linear indices  
    ind_diffs = dist_matrix["ind_diff"].values
    
    # Distances between current point and its close points
    ini_dist = dist_matrix["dis"].values/1000.0


    # Derive aggregate population at time 1
    pop_first_year = pdm.raster_array_modifier(population_1st_array, within_indices)
    pop_t1         = pop_first_year.sum()


    # Extract aggregate population at time 2
    pop_t2_df = pd.read_csv(aggregate_projections)
    pop_t2    = pop_t2_df.loc[(pop_t2_df.Year == proj_year) & (pop_t2_df.Region == borough_name),
                               "Population"].iloc[0]

    
    # Population change between years 1 and 2
    pop_change = pop_t2 - pop_t1
    
    if pop_change < 0:
        negative_mod = 1
    else:
        negative_mod = 0
    
    # Extract the alpha and beta values from the calibration files
    calib_params = pd.read_csv(params_file)
    
    a = calib_params.loc[(calib_params.year == cur_year) & (calib_params.scenario == scenario), 'alpha'].iloc[0]
    b = calib_params.loc[(calib_params.year == cur_year) & (calib_params.scenario == scenario), 'beta'].iloc[0]


    # Distance elements 
    dist                 = -b * ini_dist
    exp_xx_inv_beta_dist = np.exp(dist)
    

    # Initialize the parallelization
    pool = Pool(processes = multiprocessing.cpu_count() - 1)
    
    # Provide the inputs for the parallelized function
    parallel_elements = [(i, ind_diffs, population_1st_array, a, exp_xx_inv_beta_dist) for i in within_indices]
    
    # Derive suitability estimates
    suitability_estimates = pool.map(pdm.suitability_estimator, parallel_elements)
    

    # Change suitability estimates to a numpy array
    suitability_estimates = np.array(suitability_estimates)
    

    # Exract only the necessary mask values that fall within the state boundary
    points_mask = pdm.raster_array_modifier(points_mask, within_indices)  
    points_mask[np.isnan(points_mask)] = 0


    # In case of population decline, suitability estimates are reciprocated 
    if negative_mod:
        
        # find those whose mask is 0 but have population, they should decline anyway 
        mask_zero    = np.where(points_mask == 0)[0]
        pop_non_zero = np.where(pop_first_year != 0)[0]
        
        # Those cells with mask value of zero and population are the intersection of the two above arrays
        pop_mask = np.intersect1d(mask_zero, pop_non_zero, assume_unique=True)
        
        # Change the mask value of the above cells to 1 so that they also lose population
        points_mask[pop_mask] = points_mask.mean()
        
        # Adjust suitability values by applying mask values
        suitability_estimates = points_mask * suitability_estimates
        
        # Inverse current mask values for a better reflection of population decline
        suitability_estimates[suitability_estimates != 0] = 1.0/suitability_estimates[suitability_estimates != 0]
    
    else:
        # Adjust suitability values by applying its mask values
        suitability_estimates = points_mask * suitability_estimates

    # Total suitability for the whole area, which is the summation of all individual suitability values
    tot_suitability = suitability_estimates.sum()

    # Final population estimates if nagative mode is off
    pop_estimates = suitability_estimates/tot_suitability * pop_change + pop_first_year
    

    # Adjust the projection so that no cell can have less than 0 individuals.    
    if negative_mod:
        # To make sure that there is no negative population
        while any(pop < 0 for pop in pop_estimates):
            new_tot_suitability = 0 # Total suitability calculated over points with positive population
            extra_pop_mod = 0 # For adjusting negative population values
            
            # Treating negative population values
            extra_pop_mod = abs(pop_estimates[pop_estimates < 0].sum())
            pop_estimates[pop_estimates < 0] = 0

            # Calculate the new total suitability value based on points with positive projected population                
            new_tot_suitability = suitability_estimates[pop_estimates > 0].sum()       
            
            # Adjust non-negative population values to maintain the total aggregated population 
            pop_estimates[pop_estimates > 0] = pop_estimates[pop_estimates > 0] - \
            (suitability_estimates[pop_estimates > 0]/new_tot_suitability) * extra_pop_mod
        
    
    
    # Write the final array to the output 
    output_raster = os.path.join(projection_outputs,
                                  'pop_grid_' + scenario + '_' + str(proj_year) + '.tif')
    print("output downscaled data: " + output_raster)
    pdm.array_to_raster(mask_raster, pop_estimates, within_indices, output_raster)




#=========================================================================================

# The calibration component
calibration(pop_fst_year, pop_snd_year, mask_raster, point_indices, point_coors, neighborhood_dis)

 
# The projection component  
for year in range(2020, 2100, 10):
    if year == 2020:
        pop_start_year = first_pop_grid
    else:
        pop_start_year = os.path.join(projection_outputs,
                                'pop_grid_' + scenario + '_' + str(year) + '.tif')

    pop_projection(pop_start_year, mask_raster, aggregate_projections, point_indices,
                    params_file, point_coors, neighborhood_dis, scenario)


   
   

    
