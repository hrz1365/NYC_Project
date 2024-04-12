""" This scripy calls dasymetry.py. User can modify or call only the
    methods they want to use. Please see the namelist file for info
    on the parameters.
"""

import sys
from pathlib import Path

# enter your dasymetry path
dasypath    = './scripts/python/'
output_path = Path('./outputs/parcel_downscaling/')
workdir     = Path(dasypath)

sys.path.append(dasypath)

import dasymetry as dasy


dasy = dasy.Dasymetry(workdir)
dasy.load_namelist(dasy.rundir)
dasy.load_source_files(dasy.configdict)


# Pre-process blocks and lots for disaggregation
dasy.getOverpopParcels(dasy.parcel_df, dasy.block_df)
dasy.assignParcels(dasy.parcel_df, dasy.block_df)

a = dasy.parcel_df.copy()
b = dasy.block_df.copy()

b.loc[:, 'totpop_e'].sum()
a.loc[:, 'totpop_e'].sum()

# parcels = dasy.parcel_df.copy()
# blocks  = dasy.block_df.copy()


# blocks.loc[:, 'totpop_e'].sum()
# parcels.loc[:, 'totpop_e'].sum()


a[a.index == 3009020001]       

# dasy.block_df.loc[:, 'totpop_e'].sum()
# dasy.parcel_df.loc[:, 'totpop_e'].sum()

# disaggregate
dasy.blocksToOverpop(dasy.parcel_df, dasy.block_df)    # Determines parcels intersecting more than 1 block 
dasy.disaggregate(dasy.parcel_df, dasy.block_df)


dasy.disaggregate_leftover(dasy.parcel_df, dasy.block_df)


# Write output
output_name = 'parcels_pop_2020.shp'
dasy.writeOutput(output_name, dasy.parcel_df)
