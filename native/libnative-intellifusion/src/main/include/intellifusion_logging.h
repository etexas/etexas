/**
 * Defines the structures and functions required for logging in the native
 * IntelliFusion application.
 *
 * @author emyers
 */

#ifndef ETEXAS_INTELLIFUSION_LOGGING
#define ETEXAS_INTELLIFUSION_LOGGING

// project files
#include "intellifusion_data_model.h"  // model data structures

/**
 * Logs information for the given Detector.
 *
 * @param detector a pointer to the detoctor to log
 * @param indentations the number of indentations to use
 */
void LogDetector(Detector *detector, int indentations);

/**
 * Logs information for each detector in the given list.
 *
 * @param list a pointer to the list of detectors to log
 * @param indentations the number of indentations to use
 */
void LogDetectorList(DetectorList *list, int indentations);

/**
 * Logs information for the given information model.
 *
 * @param model a pointer to the information model to log
 * @param indentations the number of indentations to use
 */
void LogInfoModel(InfoModel *model, int indentations);

/**
 * Logs information for the given lane.
 *
 * @param lane a pointer to the lane to log
 * @param indentations the number of indentations to use
 */
void LogLane(Lane *lane, int indentations);

/**
 * Logs lane information for the given lane manager.
 *
 * @param lane_manager a pointer to the lane manager containing the lanes to
 * log
 * @param indentations the number of indentations to use
 */
void LogLanes(LaneManager *lane_manager, int indentations);

/**
 * Logs information for the given lane manager.
 *
 * @param lane_manager a pointer to the lane manager to log
 * @param indentations the number of indentations to use
 */
void LogLaneManager(LaneManager *lane_manager, int indentations);

/**
 * Logs information for the given lane movement.
 *
 * @param movement a pointer to the lane movement to log
 * @param indentations the number of indentations to use
 */
void LogLaneMovement(LaneMovementType *movement, int indentations);

/**
 * Logs movement information for the given lane.
 *
 * @param lane a pointer to the lane containing the movements to log
 * @param indentations the number of indentations to use
 */
void LogLaneMovements(Lane *lane, int indentations);

/**
 * Logs information for the given lane node.
 *
 * @param node a pointer to the lane node to log
 * @param indentations the number of indentations to use
 */
void LogLaneNode(LaneNode *node, int indentations);

/**
 * Logs node information for the given lane.
 *
 * @param lane a pointer to the lane containing the nodes to log
 * @param indentations the number of indentations to use
 */
void LogLaneNodes(Lane *lane, int indentations);

/**
 * Logs information for the given light change event.
 *
 * @param light_change a pointer to the light change event to log
 * @param indentations the number of indentations to use
 */
void LogLightChange(LightChange *light_change, int indentations);

/**
 * Logs information for the given list of light change events.
 *
 * @param list a pointer to the list of light change events to log
 * @param indentations the number of indentations to use
 */
void LogLightChangeList(LightChangeList *list, int indentations);

/**
 * Logs information for the given signal indication.
 *
 * @param signal a pointer to the signal indication to log
 * @param indentations the number of indentations to use
 */
void LogSignalIndication(SignalIndication *signal, int indentations);

/**
 * Logs information for the given signal indication list.
 *
 * @param list a pointer to the signal indication list to log
 * @param indentations the number of indentations to use
 */
void LogSignalIndicationList(SignalIndicationList *list, int indentations);

/**
 * Logs information for the given vehicle.
 *
 * @param vehicle a pointer to the vehicle to log
 * @param indentations the number of indentations to use
 */
void LogVehicle(Vehicle *vehicle, int indentations);

/**
 *
 * Logs information for each vehicle in the given list.
 *
 * @param list a pointer to the list of vehicles to log
 * @param indentations the number of indentations to use
 */
void LogVehicleList(VehicleList *list, int indentations);

#endif
