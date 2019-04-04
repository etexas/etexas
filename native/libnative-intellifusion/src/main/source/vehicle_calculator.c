/**
 * Defines structures and functions to implement an algorithm that calculates
 * the presence of vehicles without DSRC capabilities using detector vehicle
 * count information.
 *
 * @author emyers
 */

// system files
#include <limits.h>  // INT_MIN
#include <math.h>    // atan, M_PI

// library files
#include <detector.h>  // detectors

// project files
#include "intellifusion_config.h"      // injection variables
#include "intellifusion_data_model.h"  // model data structures
#include "vehicle_calculator.h"        // function implementation

// forward type declarations
typedef struct LaneStatistics LaneStatistics;
typedef struct LaneStatisticsList LaneStatisticsList;

/**
 * The vehicle count information for a lane.
 */
struct LaneStatistics {

  /**
   * The lane ID.
   */
  int lane_id;

  /**
   * The current number of known vehicles counted.
   */
  int known_count;

  /**
   * The previous number of known vehicles counted.
   */
  int prev_known_count;

  /**
   * The current number of detected vehicles counted.
   */
  int det_count;

  /**
   * The previous number of detected vehicles counted.
   */
  int prev_det_count;

  /**
   * Flags whether the known vehicle count has looped.
   */
  int8_t known_looped;

  /**
   * Flags whether the detected vehicle count has looped.
   */
  int8_t det_looped;

};

/**
 * The list of lane statistics.
 */
struct LaneStatisticsList {

  /**
   * The current lane statistics node.
   */
  LaneStatistics stats;

  /**
   * The next lane statistics node.
   */
  LaneStatisticsList *next;
};

/**
 * The number used to assign IDs to vehicles created from detector vehicle
 * count information.
 */
static int vehicle_id_ = INT_MIN;

/**
 * The list of IDs for tracked DSRC vehicles.
 */
static IntegerList *tracked_vehicles_ = NULL;

/**
 * The list of lane statistics.
 */
static LaneStatisticsList *stats_list_ = NULL;

// forward function declarations
static double CalculateAngle(double x1, double y1, double x2, double y2);
static double CalculateVehicleHeading(
    Vehicle *vehicle, LaneManager *lane_manager);
static Vehicle *CreateDetectedVehicle(
    Detector *detector, LaneManager *lane_manager);
static LaneStatistics *GetLaneStatistics(int lane);
static void InitializeLaneStatistics(DetectorList *detectors);
static int IsTracked(Vehicle *vehicle);
static void ProcessDsrcVehicles(VehicleList *vehicles);
static void UpdateTrackedVehicles(VehicleList *vehicles);

/**
 * Returns the direction (degrees) of a vector based on the given source and
 * target coordinates. The initial calculation assumes that East is 0 degrees
 * and that degrees increase in the counter-clockwise direction (i.e. North is
 * 90 degrees). The SAE J2735 specification requires North to be 0 degrees and
 * for degrees to increase in the clockwise direction (i.e. East is 90
 * degrees). As such, the direction is reversed and rotated by 90 degrees
 * after the direction is calculated.
 *
 * @param x1 the x coordinate of the source point
 * @param y1 the y coordinate of the source point
 * @param x2 the x coordinate of the target point
 * @param y2 the y coordinate of the target point
 * @return the direction (degrees), d, of a vector such that 0 <= d <=
 * 359.9875
 */
static double CalculateAngle(double x1, double y1, double x2, double y2) {

  double dx = x2 - x1;
  double dy = y2 - y1;
  double angle = atan2(dy, dx) * (180 / M_PI);

  // convert from (-180, 180) to (0, 360)
  if (angle < 0.0) {
    angle += 360;
  }
  //reverse the direction
  angle = 360 - angle;

  // rotate 90 degrees from east to north
  if (angle < 270) {

    angle = angle + 90;

  } else if (angle >= 270 && angle <= 360) {

    angle = angle - 270;
  }

  // J2735 spec requirement.
  if (359.9875 < angle) {
    angle = 359.9875;
  }

  return angle;
}

/**
 * Returns the heading (degrees) of the given vehicle. The degrees adhere to
 * the J2735 specification, meaning that they are specified relative to a
 * northern heading and increase in the clockwise direction (North is 0
 * degrees, East is 90 degrees, South is 180 degrees, etc.).
 *
 * @param vehicle a pointer to the vehicle whose heading is calculated
 * @param lane_manager a pointer to the lane manager
 * @return the heading (degrees) of the given vehicle
 */
static double CalculateVehicleHeading(
    Vehicle *vehicle, LaneManager *lane_manager) {

  double heading = 0.0;

  // use the direction of the lane if there is no previous vehicle data
  Lane *lane = GetLaneById(vehicle->lane_id, lane_manager);
  if (lane != NULL) {

    // TODO: janway - use the pair of nodes the vehicle is between
    LaneNode *first = &(lane->nodes[0]);
    LaneNode *second = &(lane->nodes[1]);

    // switch first and second (vehicle and lane directions are opposites)
    heading = CalculateAngle(second->x, second->y, first->x, first->y);
  }

  return heading;
}

/**
 * Returns a pointer to a list of vehicles calculated from detector vehicle
 * count information and current DSRC vehicles.
 *
 * @param vehicles a pointer to the list of current DSRC vehicles
 * @param detectors a pointer to the list of detectors
 * @param lane_manager a pointer to the lane manager
 * @return a pointer to a list of vehicles calculated from detector vehicle
 * count information and current DSRC vehicles
 */
VehicleList *CalculateVehiclesFromDetectors(
    VehicleList *vehicles, DetectorList *detectors,
    LaneManager *lane_manager) {

  // initialize the lane statistics
  if (stats_list_ == NULL) {
    InitializeLaneStatistics(detectors);
  }

  // update the known vehicle counts
  ProcessDsrcVehicles(vehicles);

  VehicleList *detected = NULL;  // detected vehicles

  // for each detector
  DetectorList *det_iter;
  for (det_iter = detectors; det_iter != NULL; det_iter = det_iter->next) {

    // get the statistics for the detector's lane
    LaneStatistics *stats = GetLaneStatistics(det_iter->detector.lane_id);

    // update whether the detected count has looped
    stats->det_count = det_iter->detector.count;
    if (stats->det_count < stats->prev_det_count) {
      stats->det_looped = 1;
    }

    // if only the detected vehicle count has looped
    if (stats->det_looped && !stats->known_looped) {

      // update the detected and known vehicle counts
      stats->det_count += INT_MAX - stats->prev_det_count + 1;
      stats->known_count -= stats->prev_known_count;
    }

    // calculate the number of detected vehicles to created
    int det_vehs = 0;
    if (!stats->known_looped || stats->det_looped) {
      det_vehs = stats->det_count - stats->known_count;
    }

    // create the detected vehicles
    int i;
    for (i = 0; i < det_vehs; i++) {
      detected = AddVehicleToList(detected, CreateDetectedVehicle(
          &(det_iter->detector), lane_manager));
    }

    // reset the difference in counts (if necessary)
    if (det_vehs > 0) {
      stats->known_count = stats->det_count;
    }

    // reset the loop status flags (if necessary)
    if (stats->det_looped) {
      stats->det_looped = 0;
      stats->known_looped = 0;
    }

    // update the previous counts
    stats->prev_known_count = stats->known_count;
    stats->prev_det_count = stats->det_count;
  }

  // update the list of tracked vehicle IDs
  UpdateTrackedVehicles(vehicles);

  return detected;  // return the detected vehicles
}

/**
 * Returns a pointer to a default vehicle created for injection.
 *
 * @param lane the ID of the lane where the vehicle resides
 * @param lane_manager a pointer to the lane manager
 * @return a pointer to a default vehicle created for injection
 */
static Vehicle *CreateDetectedVehicle(
    Detector *detector, LaneManager *lane_manager) {

  Vehicle *vehicle = malloc(sizeof(Vehicle));
  vehicle->id = vehicle_id_++;
  vehicle->x = detector->x;
  vehicle->y = detector->y;
  vehicle->lane_id = detector->lane_id;
  vehicle->speed = GetInjectedVehicleSpeed();
  vehicle->length = GetInjectedVehicleLength();
  vehicle->heading = CalculateVehicleHeading(vehicle, lane_manager);
  vehicle->type = VehicleType_car;

  // reset the vehicle ID to prevent conflicts with real IDs
  if (vehicle_id_ == 0) {
    vehicle_id_ = INT_MIN;
  }

  return vehicle;
}

/**
 * Returns a pointer to the lane statistics for the given lane.
 *
 * @param lane the lane ID
 * @return a pointer to the lane statistics for the given lane (or NULL if
 * none exists)
 */
static LaneStatistics *GetLaneStatistics(int lane) {

  LaneStatisticsList *stats_iterator = stats_list_;
  while (stats_iterator != NULL) {
    if (stats_iterator->stats.lane_id == lane) {
      return &(stats_iterator->stats);
    }

    stats_iterator = stats_iterator->next;
  }

  return NULL;
}

/**
 * Initializes the lane statistics for each detector in the given list.
 *
 * @param detectors a pointer to the list of detectors
 */
static void InitializeLaneStatistics(DetectorList *detectors) {

  DetectorList *detector_iterator = detectors;
  while (detector_iterator != NULL) {
    LaneStatisticsList *temp = malloc(sizeof(LaneStatisticsList));
    temp->stats.lane_id = detector_iterator->detector.lane_id;
    temp->stats.known_count = detector_iterator->detector.count;
    temp->stats.prev_known_count = detector_iterator->detector.count;
    temp->stats.det_count = detector_iterator->detector.count;
    temp->stats.prev_det_count = detector_iterator->detector.count;
    temp->stats.known_looped = 0;
    temp->stats.det_looped = 0;
    temp->next = stats_list_;
    stats_list_ = temp;
    detector_iterator = detector_iterator->next;
  }
}

/**
 * Returns whether the given vehicle is a tracked DSRC vehicle.
 *
 * @param vehicle a pointer to the vehicle to test
 * @return the status regarding whether the vehicle is tracked (1) or not(0)
 */
static int IsTracked(Vehicle *vehicle) {

  IntegerList *integer_iterator = tracked_vehicles_;
  while (integer_iterator != NULL) {
    if (integer_iterator->integer == vehicle->id)
      return 1;

    integer_iterator = integer_iterator->next;
  }

  return 0;
}

/**
 * Updates the known vehicle count using the given DSRC vehicles.
 *
 * @param vehicles pointer to the list of current DSRC vehicles
 */
static void ProcessDsrcVehicles(VehicleList *vehicles) {

  // for each DSRC vehicle
  VehicleList *veh_iter;
  for (veh_iter = vehicles; veh_iter != NULL; veh_iter = veh_iter->next) {

    // if the vehicle is not already tracked
    if (!IsTracked(veh_iter->vehicle)) {

      // get the statistics for the vehicle's lane
      LaneStatistics *stats = GetLaneStatistics(veh_iter->vehicle->lane_id);
      if (stats != NULL) {

        // if the known vehicle count is about to loop
        if (stats->known_count == INT_MAX) {

          // reset the known vehicle count
          stats->known_count = 0;
          stats->known_looped = 1;

          // if the known vehicle count is not about to loop
        } else {

          // increment the known vehicle count
          stats->known_count++;
        }

        // track the new DSRC vehicle
        IntegerList *temp = malloc(sizeof(IntegerList));
        temp->integer = veh_iter->vehicle->id;
        temp->next = tracked_vehicles_;
        tracked_vehicles_ = temp;
      }
    }
  }
}

/**
 * Removes DSRC vehicles that no longer need to be tracked.
 *
 * @param vehicles a pointer to the list of current DSRC vehicles
 */
static void UpdateTrackedVehicles(VehicleList *vehicles) {

  int found;                      // vehicle found status
  int vehicle_id;                 // tracked vehicle ID
  VehicleList *vehicle_iterator;  // vehicle iterator

  // reset the tracked vehicles reference
  IntegerList *curr_node = tracked_vehicles_;
  IntegerList *prev_node = NULL;

  // while tracked vehicles exist
  while (curr_node != NULL) {

    // get the current tracked vehicle ID
    vehicle_id = curr_node->integer;

    found = 0;
    vehicle_iterator = vehicles;

    // if the tracked vehicle broadcast a message (i.e. is in range)
    while (vehicle_iterator != NULL && !found) {
      if (vehicle_id == vehicle_iterator->vehicle->id) {

        // flag that the vehicle was found and keep ID for future tracking
        found = 1;
      }

      // move to the next vehicle
      vehicle_iterator = vehicle_iterator->next;
    }

    // if vehicle is not found then take it out of the tracked list
    if (!found) {
      IntegerList *to_delete = curr_node;
      if (prev_node != NULL) {
        prev_node->next = curr_node->next;
      }
      free(curr_node);

      curr_node = prev_node;
    }

    // update the list nodes for iterating
    prev_node = curr_node;
    curr_node = curr_node->next;
  }
}
