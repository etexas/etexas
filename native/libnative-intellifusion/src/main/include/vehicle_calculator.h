/**
 * Defines the function that should be implemented by algorithms that
 * calculate the presence of vehicles without DSRC capabilities using detector
 * information.
 *
 * @author emyers
 */

#ifndef ETEXAS_INTELLIFUSION_VEHICLES_FROM_DETECTORS
#define ETEXAS_INTELLIFUSION_VEHICLES_FROM_DETECTORS

// library files
#include <detector.h>  // detector list

// project files
#include "intellifusion_data_model.h"  // model data structures

/**
 * Returns a pointer to a list of vehicles calculated from detector
 * information and current DSRC vehicles.
 *
 * @param vehicles a pointer to the list of current DSRC vehicles
 * @param detectors a pointer to the list of detectors
 * @param lane_manager a pointer to the lane manager
 * @return a pointer to a list of vehicles calculated from detector
 * information and current DSRC vehicles
 */
VehicleList *CalculateVehiclesFromDetectors(
    VehicleList *vehicles, DetectorList *detectors,
    LaneManager *lane_manager);

#endif
