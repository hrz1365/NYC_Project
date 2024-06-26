
"""
A package that contains necessary functions for population downscaling.

@author: Hamidreza Zoraghein
"""
#======================================================================================
import numpy as np
import pandas as pd
import rasterio
from scipy.spatial import cKDTree
import multiprocessing
from pathos.multiprocessing import ProcessingPool as Pool



def raster_to_array(raster):
    
    # Read the input raster before conveting it to an array
    with rasterio.open(raster) as src_raster:
        band        = src_raster.read(1)
        final_array = band.flatten()
    
    return(final_array)
    


def array_to_raster(input_raster, input_array, within_indices, output_raster):
    
    with rasterio.open(input_raster) as src_raster:
        band        = src_raster.read(1)
        src_profile = src_raster.profile
        row_count   = band.shape[0]
        col_count   = band.shape[1]
        flat_array  = band.flatten()
    
    # Replace initial array values with those from the input array 
    flat_array[within_indices] = input_array
    
    
    array = flat_array.reshape(row_count, col_count)
    
    with rasterio.open(output_raster, "w", **src_profile) as dst:
        dst.write_band(1, array)
    


def all_index_retriever(raster, columns):
    
    # Read the input raster before conveting it to an array
    with rasterio.open(raster) as src_raster:
        array = src_raster.read(1)
    
    # Put the row, column and linear indices of all elements in a dataframe
    shape = array.shape
    index = pd.MultiIndex.from_product([range(s)for s in shape], names=columns)
    df = pd.DataFrame({'all_index': array.flatten()}, index=index).reset_index()
    df["all_index"] = df.index
    
    df = df.astype({"row": np.int32, "column": np.int32, "all_index": np.int32})
    return df



def suitability_estimator(pop_dist_params):

    import numpy as np
    np.seterr(divide='ignore', invalid='ignore')

    
    # The id of the current focal point
    element = pop_dist_params[0]
    
    # Construct nearby indices
    neigh_indices = element + pop_dist_params[1]
            
    # Population of close points
    pop = pop_dist_params[2][neigh_indices]
           
    # Calculation of the other elements of the suitability equation
    pop_xx_alpha = np.where(pop > 0, np.power(pop, pop_dist_params[3]), pop)

    pop_dist_factor = pop_xx_alpha * pop_dist_params[4]
    
    estimate = np.nanmean(pop_dist_factor)
    
    return(estimate)
    


def dist_matrix_calculator(first_index, cut_off_meters, all_indices, coors_csv_file):
    
    # Read all points with their coordinates
    points = np.genfromtxt(coors_csv_file, delimiter = ',', skip_header = 1, usecols = (0, 1, 2),
                           dtype = float)
    
    # Calculate distances between the first point and all other points within the specified neighborhood
    cut_off_metres = cut_off_meters + 1
    tree_1 = cKDTree(points[first_index:first_index + 1, [1, 2]])
    tree_2 = cKDTree(points[:, [1, 2]])         
    tree_dist = cKDTree.sparse_distance_matrix(tree_1, tree_2, cut_off_metres, output_type  = 'dict',
                                               p = 2)
    
    # Put distances and indices of neighboring in a dataframe 
    dist_df = pd.DataFrame(columns = ["near_id", "dis"])                 
    dist_df["near_id"]  = [near_id[1] for near_id in list(tree_dist.keys())]
    dist_df["dis"]      = tree_dist.values()  
    dist_df = dist_df.loc[dist_df.dis != 0, :] # Remove the distance to itself
    
    # Bring row and column indices of neighboring points by a join
    dist_df = dist_df.join(all_indices, on = "near_id")
    
    # Add to columns holding the relative difference in rows and colums beween focal point and its neighbors
    foc_indices = all_indices.loc[first_index, ["row", "column"]].values  
    dist_df["ind_diff"] = dist_df["near_id"] - first_index
    dist_df["row_diff"] = dist_df["row"] - foc_indices[0]
    dist_df["col_diff"] = dist_df["column"] - foc_indices[1]
    
    # Drop unwanted columns
    dist_df = dist_df.drop(["row", "column", "near_id", "all_index"], axis = 1)
        
    dist_df = dist_df.astype({"ind_diff": np.int32, "row_diff": np.int32, "col_diff": np.int32})
        
    return dist_df



def raster_array_modifier(raster_array, within_indices):
    
    mod_raster_array = raster_array[within_indices]
    mod_raster_array[mod_raster_array < 0] = 0
    mod_raster_array[np.isnan(mod_raster_array)] = 0 

    return(mod_raster_array)



def pop_min_function(z, *params):
    
    #Initialize the parameters
    a,b = z

    # Inputs to the optimization
    population_1st  = params[0] # Population of points in the first year (urban/rural)
    population_2nd  = params[1] # Population of points in the second year (urban/rural)
    points_mask     = params[2] # Mask values of points 
    dist_matrix     = params[3] # Template distance matrix
    within_indices  = params[4] # Indices of points within the state boundary (subset of the above)
    
    # Outputs of the optimization at each step
    pop_estimates  = np.zeros(len(within_indices)) #Population estimates in the second year
    
    
    # Calculate aggregate population at times 1 and 2 
    pop_t1_array = raster_array_modifier(population_1st, within_indices) 
    pop_t2_array = raster_array_modifier(population_2nd, within_indices) 

    pop_t1 = pop_t1_array.sum()
    pop_t2 = pop_t2_array.sum()
    
    
    #Population change between the two reference years    
    pop_change = pop_t2 - pop_t1    
    if pop_change < 0:
        negative_mod = 1
    else:
        negative_mod = 0
    
    # Differences in index between focal and nearby points as a template  
    ind_diffs = dist_matrix["ind_diff"].values
    
    # Distances between current point and its close points
    ini_dist = dist_matrix["dis"].values/1000.0
    dist     = -b * ini_dist
    
    exp_xx_inv_beta_dist = np.exp(dist)
    
    #Initialize the parallelization
    pool = Pool(processes = multiprocessing.cpu_count() - 1)
    
    # Provide the inputs for the parallelized function
    parallel_elements = [(i, ind_diffs, population_1st, a, exp_xx_inv_beta_dist) for i in within_indices]

    # Derive suitability estimates
    suitability_estimates = pool.map(suitability_estimator, parallel_elements)

    #Change suitability estimates to a numpy array
    suitability_estimates = np.array(suitability_estimates)
    
    # Exract only the necessary mask values that fall within the state boundary
    points_mask = raster_array_modifier(points_mask, within_indices)  


    # In case of population decline, suitability estimates are reciprocated for non-zero values       
    if negative_mod:
        
        # find those whose mask is 0 but have population, they should decline anyway 
        mask_zero    = np.where(points_mask == 0)[0]
        pop_non_zero = np.where(pop_t1_array != 0)[0]
        
        # Those cells with mask value of zero and population are the intersection of the two above arrays
        pop_mask = np.intersect1d(mask_zero, pop_non_zero, assume_unique=True)
        
        # Change the mask value of the above cells to the mean so that they also lose population
        points_mask[pop_mask] = points_mask.mean()
        
        #Adjust suitability values by applying mask values
        suitability_estimates = points_mask * suitability_estimates
        
        # Inverse current mask values for a better reflection of population decline
        suitability_estimates[suitability_estimates != 0] = 1.0/suitability_estimates[suitability_estimates != 0]
    
    else:
        # Adjust suitability values by applying mask values
        suitability_estimates = points_mask * suitability_estimates
    
    # Total suitability for the whole area, which is the summation of all individual suitability values
    tot_suitability = suitability_estimates.sum()
    
    # Final population estimate for each point if nagative mode is off
    pop_estimates = suitability_estimates/tot_suitability * pop_change + pop_t1_array
    
    if negative_mod:
        while any(pop < 0 for pop in pop_estimates): #To ensure there is no negative population
            new_tot_suitability = 0 #Total suitability calculated over points with positive population
            extra_pop_mod       = 0 #For adjusting negative population values
            
            #Treating negative population values
            extra_pop_mod = abs(pop_estimates[pop_estimates < 0].sum())
            pop_estimates[pop_estimates < 0] = 0

            #Calculate the new total suitability value based on those points whose projected population is positive                
            new_tot_suitability = suitability_estimates[pop_estimates > 0].sum()       
            
            #Adjust non-negative population values to maintain the total aggregated population 
            pop_estimates[pop_estimates > 0] = pop_estimates[pop_estimates > 0] - (suitability_estimates[pop_estimates > 0]/new_tot_suitability) * extra_pop_mod


    # Produce the total error compared to observed values 
    # population_2nd = raster_array_modifier(population_2nd, within_indices)   
    tot_error = abs(pop_t2_array - pop_estimates).sum()
    
    print("the first run is done")
    
    return tot_error
        


