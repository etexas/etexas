/**
 * Defines the functions that should be implemented by algorithms that parse
 * vehicles, lane managers, and signal indications from BSM, MAP, and SPaT
 * messages, respectively.
 *
 * @author emyers
 */

#ifndef ETEXAS_INTELLIFUSION_MESSAGES
#define ETEXAS_INTELLIFUSION_MESSAGES

// project files
#include "intellifusion_data_model.h"  // model data structures

/**
 * Returns a pointer to a vehicle parsed from the given BSM.
 *
 * @param bytes a pointer to the BSM
 * @param size the length (number of bytes) of the message
 * @param lane_manager a pointer to the lane manager
 * @param known_vehicles a list of vehicles which are previously known to exist in a particular lane
 * @return a pointer to the parsed vehicle
 */
Vehicle *ParseBsm(char *bytes, int size, LaneManager *lane_manager, VehicleList *known_vehicles);

/**
 * Returns a pointer to a list of lane managers (one for each intersection)
 * parsed from the given map data message.
 *
 * @param bytes a pointer to the map data message
 * @param size the length (number of bytes) of the map data message
 * @return a pointer to the parsed list of lane managers
 */
LaneManagerList *ParseMapData(char *bytes, int size);

/**
 * Returns a pointer to a list of signal indications parsed from the given
 * SPaT message.
 *
 * @param bytes a pointer to the SPaT message
 * @param size the length (number of bytes) of the SPaT message
 * @return a pointer to the parsed list of signal indications
 */
SignalIndicationList *ParseSpat(char *bytes, int size);

#endif
