/**
 * Defines functions to implement an algorithm that updates vehicle movements
 * based on light state information for a given time point.
 *
 * @author emyers
 */

// system files
#include <limits.h>  // INT_MAX
#include <math.h>    // sqrt

// project files
#include "intellifusion_config.h"      // following speed and distances
#include "intellifusion_data_model.h"  // model data structures
#include "movement_calculator.h"       // function implementation

/**
 * The list of vehicles from the last movement update.
 */
static VehicleList *previous_vehicles_ = NULL;

/**
 * The simulation time for the last movement update.
 */
static double previous_time_ = 0.0;

/**
 * The vehicle ID to use for generated vehicles.
 */
static int gen_veh_id_ = INT_MAX;

// forward function declarations
static int DropVeh(Vehicle *vehicle, Lane *lane);
static double Eq3(Vehicle *leading, Vehicle *following);
static LaneNode *GetBefore(Vehicle *vehicle, Lane *lane);
static Vehicle *GetInFront(Vehicle *vehicle, VehicleList *vehicles);
static LaneNode *GetMoveToward(Vehicle *vehicle, Lane *lane, double distance);
static Vehicle *GetVehForLight(Lane *lane);
static double *MoveAlongAxis(
    double x, double y, double x2, double y2, double distance);
static void MoveVehicle(Vehicle *vehicle, Lane *lane, double duration);
static int Over50sAway(
    Vehicle *vehicle1, Vehicle *vehicle2, double speed_limit);
static void PerformMove(Vehicle *vehicle, LaneNode *target, double distance);
static VehicleList *PlaceVehForLight(
    VehicleList *vehicles, Lane *lane, LightStateList *light_states);

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
    LaneManager *lane_manager, LightStateList *states) {

  if (!std_vehs) {

    previous_time_ = time;
    return NULL;
  }

  VehicleList *vehicles = std_vehs;
  VehicleList *last_std_veh = vehicles;
  while (last_std_veh->next) {
    last_std_veh = last_std_veh->next;
  }

  last_std_veh->next = dsrc_vehs;

  VehicleList *dropped_vehicles = NULL;
  VehicleList *updated_vehicles = NULL;

  int i;
  for (i = 0; i < lane_manager->lane_count; i++) {

    Lane *lane = &(lane_manager->lanes[i]);

    if (lane->type == INBOUND) {

      VehicleList *lane_vehicles = SortByDist(PlaceVehForLight(
          VehiclesInLane(vehicles, lane->id), lane, states),
          lane->nodes[0].x, lane->nodes[0].y);

      int is_dsrc;
      int is_generated;
      Vehicle *vehicle;
      VehicleList *vehicle_iterator = lane_vehicles;

      while (vehicle_iterator) {

        vehicle = vehicle_iterator->vehicle;
        is_dsrc = VehicleInList(vehicle->id, dsrc_vehs);
        is_generated = (vehicle->id == gen_veh_id_);

        if (!(is_generated || is_dsrc)) {

          MoveVehicle(vehicle, lane, time - previous_time_);
        }

        if (!(is_generated || is_dsrc || DropVeh(vehicle, lane))) {

          Vehicle *obstruction = GetInFront(vehicle, lane_vehicles);

          if (!obstruction || Over50sAway(
              vehicle, obstruction, GetFollowingSpeed())) {

            vehicle->speed = GetFollowingSpeed();

          } else {

            vehicle->speed = Eq3(obstruction, vehicle);
            vehicle->speed = (vehicle->speed < 0.0) ? 0.0 : vehicle->speed;
          }

          VehicleList *temp = (VehicleList *) malloc(sizeof(VehicleList));
          temp->vehicle = vehicle;
          temp->next = updated_vehicles;
          updated_vehicles = temp;

        } else if (!is_dsrc) {

          VehicleList *temp = (VehicleList *) malloc(sizeof(VehicleList));
          temp->vehicle = vehicle;
          temp->next = dropped_vehicles;
          dropped_vehicles = temp;
        }

        vehicle_iterator = vehicle_iterator->next;
      }

      CleanupVehicleList(lane_vehicles);
    }
  }

  previous_time_ = time;
  last_std_veh->next = NULL;
  DestroyVehicleList(dropped_vehicles);

  return updated_vehicles;
}

/**
 * Returns whether the given DSRC vehicle should be dropped this iteration.
 *
 * @param vehicle a pointer to the DSRC vehicle to test
 * @param lane_manager a pointer to the lane manager
 * @return a value of 1 (true) if the vehicle should be dropped, otherwise a
 * value of 0 (false) is returned
 */
int DropDsrcVehicle(Vehicle *vehicle, LaneManager *lane_manager) {

  Lane *lane = GetLaneById(vehicle->lane_id, lane_manager);

  return (!lane || DropVeh(vehicle, lane));
}

/**
 * Returns whether the given vehicle has left the given lane. The calculation
 * is based on whether the distance between the vehicle and the last lane node
 * is greater than the distance between the first and last lane nodes.
 *
 * @param vehicle a pointer to the vehicle to test
 * @param lane a pointer to the lane to test
 * @return a value of 1 (true) if the vehicle has left the given lane,
 * otherwise a value of 0 (false)
 */
static int DropVeh(Vehicle *vehicle, Lane *lane) {

  double d1 = Dist(
      vehicle->x, vehicle->y, lane->nodes[lane->node_count - 1].x,
      lane->nodes[lane->node_count - 1].y);

  double d2 = Dist(
      lane->nodes[0].x, lane->nodes[0].y, lane->nodes[lane->node_count - 1].x,
      lane->nodes[lane->node_count - 1].y);

  return d1 > d2;
}

/**
 * Returns the velocity (m/s) of the given following vehicle based on the
 * position of the given leading vehicle and the parameters of the
 * intersection.
 *
 * @param leading a pointer to the leading vehicle
 * @param following a pointer to the following vehicle
 * @return the calculated velocity (m/s) for the following vehicle
 */
static double Eq3(Vehicle *leading, Vehicle *following) {

  double sj = GetJamSpacing() * 0.01;       // spacing at jam density (m)
  double sc = GetCapacitySpacing() * 0.01;  // spacing at capacity (m)
  double vd = GetFollowingSpeed();          // velocity desired (m/s)
  double vc = GetFollowingSpeed() - 3.0;    // velocity at capacity (m/s)

  // calculate the Van Aerde steady-state model parameters
  double subc = (sj * vd) / (vc * vc);
  double c1 = subc * ((2 * vc) - vd);
  double c2 = subc * ((vd - vc) * (vd - vc));
  double c3 = (sc / vc) - subc;

  // calculate the distance (m) between the leading and following vehicles
  double dv = Dist(following->x, following->y, leading->x, leading->y) * 0.01;

  // calculate the velocity of the following vehicle
  double sub1 = (c3 * vd) + dv - c1;
  double sub2 = c1 - (c3 * vd) - dv;
  double sub3 = (dv * vd) - (c1 * vd) - c2;

  return (sub1 - sqrt((sub2 * sub2) - (4 * c3 * sub3))) / (2 * c3);
}

/**
 * Returns a pointer to the lane node closest to the given vehicle. The first
 * lane node is returned if no closer lane node is found.
 *
 * @param vehicle a pointer to the vehicle
 * @param lane a pointer to the vehicle lane to search
 * @return a pointer to the lane node closest to the given vehicle (or the
 * first lane node if none is closer)
 */
static LaneNode *GetBefore(Vehicle *vehicle, Lane *lane) {

  LaneNode *test;
  LaneNode *node = &(lane->nodes[0]);

  int is_horizontal = (vehicle->heading >= 45 && vehicle->heading < 135)
      || (vehicle->heading >= 225 && vehicle->heading < 315);

  int i;
  for (i = 1; i < lane->node_count; i++) {

    test = &(lane->nodes[i]);

    if (is_horizontal) {

      if (fabs(test->x) > fabs(vehicle->x)) {

        break;
      }

    } else if (fabs(test->y) > fabs(vehicle->y)) {

      break;
    }

    node = test;
  }

  return node;
}

/**
 * Returns a pointer to the vehicle in front of the given vehicle.
 *
 * @param vehicle a pointer to the source vehicle
 * @param vehicles a pointer to a list of vehicles to search
 * @return a pointer to vehicle in front of the given vehicle (or the last
 * vehicle in the searched list if it does not include the given vehicle)
 */
static Vehicle *GetInFront(Vehicle *vehicle, VehicleList *vehicles) {

  Vehicle *found_vehicle = NULL;
  VehicleList *vehicle_iterator = vehicles;

  while (vehicle_iterator != NULL) {

    if (vehicle->id == vehicle_iterator->vehicle->id) {

      break;

    } else {

      found_vehicle = vehicle_iterator->vehicle;
    }

    vehicle_iterator = vehicle_iterator->next;
  }

  return found_vehicle;
}

/**
 * Returns a pointer to the target lane node when moving the given vehicle.
 *
 * @param vehicle a pointer to the vehicle to move
 * @param lane a pointer to the vehicle lane
 * @param distance the movement distance (cm)
 * @return a pointer to the target lane node
 */
static LaneNode *GetMoveToward(
    Vehicle *vehicle, Lane *lane, double distance) {

  LaneNode *node = GetBefore(vehicle, lane);
  double d = Dist(vehicle->x, vehicle->y, node->x, node->y);

  if (d < distance) {

    Vehicle temp;
    temp.x = node->x;
    temp.y = node->y;
    node = GetBefore(&temp, lane);
  }

  return node;
}

/**
 * Returns a pointer to a vehicle in the given lane that will induce red light
 * behavior.
 *
 * @param lane a pointer to the relevant lane
 * @return a pointer to a vehicle to induce red light behavior
 */
static Vehicle *GetVehForLight(Lane *lane) {

  Vehicle *vehicle = (Vehicle *) malloc(sizeof(Vehicle));

  vehicle->id = gen_veh_id_;
  vehicle->length = 0.0;
  vehicle->x = lane->nodes[0].x;
  vehicle->y = lane->nodes[0].y;
  vehicle->speed = 0.0;

  return vehicle;
}

/**
 * Returns a pointer to an array of two double values for the x and y
 * coordinates of the given source point that haven been updated to reflect
 * the given movement distance toward the given target point.
 *
 * @param x the x coordinate of the source point
 * @param y the y coordinate of the source point
 * @param x2 the x coordinate of the target point
 * @param y2 the y coordinate of the target point
 * @param distance the distance (cm) to move the point
 * @return a pointer to an array of two double values for the updated x and y
 * coordinates
 */
static double *MoveAlongAxis(
    double x, double y, double x2, double y2, double distance) {

  double deltax = x2 - x;
  double deltay = y2 - y;
  double ratio = distance / sqrt(deltax * deltax + deltay * deltay);

  double *coordinates = (double *) malloc(sizeof(double) * 2);
  coordinates[0] = x + deltax * ratio;
  coordinates[1] = y + deltay * ratio;

  return coordinates;
}

/**
 * Updates the coordinates of the given vehicle to reflect forward movement
 * for the given amount of time.
 *
 * @param vehicle a pointer to vehicle to move
 * @param lane a pointer to the vehicle lane
 * @param duration the duration (s) of the movement
 */
static void MoveVehicle(Vehicle *vehicle, Lane *lane, double duration) {

  // * 100 to convert meters to centimeters
  double distance = vehicle->speed * duration * 100.0;
  PerformMove(vehicle, GetMoveToward(vehicle, lane, distance), distance);
}

/**
 * Returns whether the there is more than 50 seconds of movement time between
 * the first and second vehicles based on the given speed limit. The 50 second
 * threshold is mentioned in the document as the point at which the equation
 * breaks down.
 *
 * @param vehicle1 a pointer to the vehicle to test
 * @param vehicle2 a pointer to the vehicle in front of the tested vehicle
 * @param speed_limit the speed limit (m/s)
 * @return a value of 1 (true) if the tested vehicle has more than 50 seconds
 * of movement time before reaching the vehicle in front of it, otherwise a
 * value of 0 (false) is returned
 */
static int Over50sAway(
    Vehicle *vehicle1, Vehicle *vehicle2, double speed_limit) {

  // * 100.0 to convert meters to centimeters
  return Dist(vehicle1->x, vehicle1->y, vehicle2->x, vehicle2->y)
      / (speed_limit * 100.0) > 50.0;
}

/**
 * Updates the coordinates of the given vehicle to reflect a movement toward
 * the given lane node by the given movement distance.
 *
 * @param vehicle a pointer to the vehicle to move
 * @param target a pointer to the target lane node
 * @param distance the distance (cm) to move the vehicle
 */
static void PerformMove(Vehicle *vehicle, LaneNode *target, double distance) {

  double *d = MoveAlongAxis(
      vehicle->x, vehicle->y, target->x, target->y, distance);

  vehicle->x = d[0];
  vehicle->y = d[1];
  free(d);
}

/**
 * Returns a pointer to a list of vehicles that may include an additional
 * vehicle to force red light behavior. The additional vehicle is only
 * generated if the given lane signal is red.
 *
 * @param vehicles a pointer to the list of vehicles currently in the lane
 * @param lane a pointer to the lane containing the given vehicles
 * @param light_states a pointer to the list of light states
 * @return a pointer to the list vehicles that may include an additional
 * vehicle to force red light behavior
 */
static VehicleList *PlaceVehForLight(
    VehicleList *vehicles, Lane *lane, LightStateList *light_states) {

  VehicleList *updated_vehicles = vehicles;
  LightState *state = GetLightStateByLaneId(light_states, lane->id);

  if (state != NULL) {

    if (state->color == RED) {

      VehicleList *temp = (VehicleList*) malloc(sizeof(VehicleList));
      temp->vehicle = GetVehForLight(lane);
      temp->next = updated_vehicles;
      updated_vehicles = temp;
    }
  }

  return updated_vehicles;
}
