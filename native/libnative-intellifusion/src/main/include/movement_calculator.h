/**
 * Defines the functions that should be implemented by algorithms that update
 * vehicle movements based on light state information for a given time point.
 *
 * @author emyers
 */

#ifndef ETEXAS_INTELLIFUSION_MOVEMENT
#define ETEXAS_INTELLIFUSION_MOVEMENT

// project files
#include "intellifusion_data_model.h"  // model data structures

/**
 * Returns a pointer to a list of vehicles updated to reflect the movements of
 * the given standard vehicles based on the states of the given DSRC vehicles
 * and light state information for the given time point.
 *
 * @param time the current time (s)
 * @param std_vehs a pointer to the list of standard vehicles to move
 * @param dsrc_vehs a pointer to the list of present DSRC vehicles
 * @param lane_manager a pointer to the lane manager
 * @param states a pointer to a list of light states affecting movement
 * @return a pointer to the list of vehicles with updated positions
 */
VehicleList *CalculateMovements(
    double time, VehicleList *std_vehs, VehicleList *dsrc_vehs,
    LaneManager *lane_manager, LightStateList *states);

/**
 * Returns whether the given DSRC vehicle should be dropped this iteration.
 *
 * @param vehicle a pointer to the DSRC vehicle to test
 * @param lane_manager a pointer to the lane manager
 * @return a value of 1 (true) if the vehicle should be dropped, otherwise a
 * value of 0 (false) is returned
 */
int DropDsrcVehicle(Vehicle *vehicle, LaneManager *lane_manager);

#endif
