/**
 * Defines the types that represent intersection traffic entities in the
 * native IntelliFusion application and the utility functions that support
 * basic type operations.
 *
 * @author emyers
 */

#ifndef ETEXAS_INTELLIFUSION_DATAMODEL
#define ETEXAS_INTELLIFUSION_DATAMODEL

// library files
#include <detector.h>     // detector list in information model
#include <VehicleType.h>  // vehicle types

// forward type declarations
typedef enum LaneMovementType LaneMovementType;
typedef enum LaneType LaneType;
typedef enum SignalColor SignalColor;
typedef enum SignalState SignalState;
typedef enum SignalType SignalType;
typedef struct InfoModel InfoModel;
typedef struct IntegerList IntegerList;
typedef struct Lane Lane;
typedef struct LaneManager LaneManager;
typedef struct LaneManagerList LaneManagerList;
typedef struct LaneNode LaneNode;
typedef struct LightChange LightChange;
typedef struct LightChangeList LightChangeList;
typedef struct LightState LightState;
typedef struct LightStateList LightStateList;
typedef struct SignalIndication SignalIndication;
typedef struct SignalIndicationList SignalIndicationList;
typedef struct Vehicle Vehicle;
typedef struct VehicleList VehicleList;

/**
 * The lane movement types.
 */
enum LaneMovementType {
  LEFT_TURN,
  RIGHT_TURN,
  STRAIGHT,
  NO_TURN_ON_RED
};

/**
 * The lane direction types.
 */
enum LaneType {
  INBOUND,
  OUTBOUND
};

/**
 * The signal color types.
 */
enum SignalColor {
  GREEN,
  YELLOW,
  RED,
  NONE
};

/**
 * The signal state types.
 */
enum SignalState {
  STEADY = 1,
  FLASHING,
  SOFT
};

/**
 * The signal types.
 */
enum SignalType {
  BALL,
  LEFT_ARROW,
  RIGHT_ARROW,
  STRAIGHT_ARROW,
  UTURN_ARROW,
  STOP_SIGN,
  YIELD_SIGN,
  UNCONTROLLED,
  UNKNOWN
};

/**
 * An intersection information model.
 */
struct InfoModel {

  /**
   * The list of vehicles for the intersection.
   */
  VehicleList *vehicles;

  /**
   * The lane manager for the intersection.
   */
  LaneManager *lane_manager;

  /**
   * The list of signal indications for the intersection.
   */
  SignalIndicationList *signals;
};

/**
 * A list of integer values.
 */
struct IntegerList {

  /**
   * The current integer node in the list.
   */
  int integer;

  /**
   * The next integer node in the list.
   */
  IntegerList *next;
};

/**
 * An intersection lane.
 */
struct Lane {

  /**
   * The type of lane.
   */
  LaneType type;

  /**
   * The ID of the lane.
   */
  int id;

  /**
   * The lane nodes.
   */
  LaneNode *nodes;

  /**
   * The number of lane nodes.
   */
  int node_count;

  /**
   * The lane movements.
   */
  LaneMovementType *movements;

  /**
   * The number of lane movements.
   */
  int movement_count;
};

/**
 * An intersection lane manager.
 */
struct LaneManager {

  /**
   * The latitude (decimal degrees) of the lane manager.
   */
  double latitude;

  /**
   * The longitude (decimal degrees) of the lane manager.
   */
  double longitude;

  /**
   * The elevation (m) of the lane manager.
   */
  double elevation;

  /**
   * The array of lanes.
   */
  Lane *lanes;

  /**
   * The number of lanes.
   */
  int lane_count;

  /**
   * The ID of the intersection.
   */
  int intersection_id;
};

/**
 * A list of intersection lane managers.
 */
struct LaneManagerList {

  /**
   * The current lane manager node in the list.
   */
  LaneManager *lane_manager;

  /**
   * The next lane manager node in the list.
   */
  struct LaneManagerList *next;
};

/**
 * An intersection lane node.
 */
struct LaneNode {

  /**
   * The x coordinate (cm) of the lane node.
   */
  double x;

  /**
   * The y coordinate (cm) of the lane node.
   */
  double y;

  /**
   * The z coordinate (cm) of the lane node.
   */
  double z;

  /**
   * The width (cm) of the lane node.
   */
  double width;
};

/**
 * A light change event.
 */
struct LightChange {

  /**
   * The previous color of the light.
   */
  SignalColor old_color;

  /**
   * The new color of the light.
   */
  SignalColor new_color;

  /**
   * The IDs of the lanes whose lights changed as specified.
   */
  IntegerList *lane_ids;
};

/**
 * A list of light change events.
 */
struct LightChangeList {

  /**
   * The current light change node in the list.
   */
  LightChange *light_change;

  /**
   * The next light change node in the list.
   */
  LightChangeList *next;
};

/**
 * A light state.
 */
struct LightState {

  /**
   * The ID of the lane managed by the light.
   */
  int lane_id;

  /**
   * The current color of the light.
   */
  SignalColor color;

  /**
   * The time (s) for the light to change colors.
   */
  double time_to_change;
};

/**
 * A list of light states.
 */
struct LightStateList {

  /**
   * The current light state node in the list.
   */
  LightState *light_state;

  /**
   * The next light state node in the list.
   */
  LightStateList *next;
};

/**
 * A signal indication.
 */
struct SignalIndication {

  /**
   * The ID of the lane controlled by the signal.
   */
  int lane_id;

  /**
   * The ID of the intersection where the signal is located.
   */
  int intersection_id;

  /**
   * The color of the signal.
   */
  SignalColor color;

  /**
   * The type of signal.
   */
  SignalType type;

  /**
   * The state of the signal.
   */
  SignalState state;

  /**
   * The time (s) for the signal to change.
   */
  double time_to_change;
};

/**
 * A list of signal indications.
 */
struct SignalIndicationList {

  /**
   * The current signal indication node in the list.
   */
  SignalIndication *signal;

  /**
   * The next indication node in the list.
   */
  SignalIndicationList *next;
};

/**
 * A vehicle.
 */
struct Vehicle {

  /**
   * The ID of the vehicle.
   */
  int id;

  /**
   * The x coordinate (cm) of the vehicle.
   */
  double x;

  /**
   * The y coordinate (cm) of the vehicle.
   */
  double y;

  /**
   * The z coordinate (cm) of the vehicle.
   */
  double z;

  /**
   * The speed (m/s) of the vehicle.
   */
  double speed;

  /**
   * The length (cm) of the vehicle.
   */
  double length;

  /**
   * The width (cm) of the vehicle.
   */
  double width;

  /**
   * The ID of the lane where the vehicle is located.
   */
  int lane_id;

  /**
   * The heading (degrees) of the vehicle.
   */
  double heading;

  /**
   * The height (cm) of the vehicle.
   */
  double height;

  /**
   * The type of vehicle.
   */
  e_VehicleType type;
};

/**
 * A list of vehicles.
 */
struct VehicleList {

  /**
   * The current vehicle node in the list.
   */
  Vehicle *vehicle;

  /**
   * The next vehicle node in the list.
   */
  VehicleList *next;
};

/**
 * Returns a pointer to a list of integers that includes the given integer and
 * those already present in the given list.
 *
 * @param list a pointer to a list of integers
 * @param integer the integer to include
 * @return a pointer to a list containing all of the given integers
 */
IntegerList *AddIntegerToList(IntegerList *list, int integer);

/**
 * Returns a pointer to a list of light change events that includes the given
 * light change event and those already present in the given list.
 *
 * @param list a pointer to a list of light change events
 * @param light_change a pointer to the light change event to include
 * @return a pointer to a list containing all of the given light change events
 */
LightChangeList *AddLightChangeToList(
    LightChangeList *list, LightChange *light_change);

/**
 * Returns a pointer to a list of light states that includes the given light
 * state and those already present in the given list.
 *
 * @param list a pointer to a list of light states
 * @param light_state a pointer to the light state to include
 * @return a pointer to a list containing all of the given light states
 */
LightStateList *AddLightStateToList(
    LightStateList *list, LightState *light_state);

/**
 * Returns a pointer to a list of signal indications that includes the given
 * signal indication and those already present in the given list.
 *
 * @param list a pointer to a list of signal indications
 * @param signal the signal indication to include
 * @return a pointer to a list containing all of the given signal indications
 */
SignalIndicationList *AddSignalIndicationToList(
    SignalIndicationList *list, SignalIndication *signal);

/**
 * Returns a pointer to a list of vehicles that includes the given vehicle and
 * those already present in the given list.
 *
 * @param list a pointer to a list of vehicles
 * @param vehicle the vehicle to include
 * @return a pointer to a list containing all of the given vehicles
 */
VehicleList *AddVehicleToList(VehicleList *list, Vehicle *vehicle);

/**
 * Frees the memory occupied by the given list (Note: does not free vehicles themselves).
 *
 * @param list a pointer to the list to free from memory
 */
void CleanupVehicleList(VehicleList *list);

/**
 * Returns a pointer to a copy of the given vehicle.
 *
 * @param vehicle a pointer to the vehicle to copy
 * @return a pointer to a copy of the given vehicle
 */
Vehicle *CopyVehicle(Vehicle *vehicle);

/**
 * Frees the memory occupied by the given info model.
 *
 * @param info_model a pointer to the info model to free from memory
 */
void DestroyInfoModel(InfoModel *info_model);

/**
 * Frees the memory occupied by the given list of integers.
 *
 * @param list a pointer to the list of integers to free from memory
 */
void DestroyIntegerList(IntegerList *list);

/**
 * Frees the memory occupied by the given lane.
 *
 * @param lane a pointer to the lane to free from memory.
 */
void DestroyLane(Lane *lane);

/**
 * Frees the memory occupied by the given lane manager.
 *
 * @param lane_manager a pointer to the lane manager to free from memory
 */
void DestroyLaneManager(LaneManager *lane_manager);

/**
 * Frees the memory occupied by the given list of lane managers.
 *
 * @param list a pointer to the list of lane managers to free from memory
 */
void DestroyLaneManagerList(LaneManagerList *list);

/**
 * Frees the memory occupied by the given lane manager list node.
 *
 * @param node a pointer to the lane manager list node to free from memory
 * @return a pointer to the next lane manager list node
 */
LaneManagerList *DestroyLaneManagerListNode(LaneManagerList *node);

/**
 * Frees the memory occupied by the given list of light change events.
 *
 * @param list a pointer to the list of light change events to free from
 * memory
 */
void DestroyLightChangeList(LightChangeList *list);

/**
 * Frees the memory occupied by the given list of light states.
 *
 * @param list a pointer to the list of light states to free from memory
 */
void DestroyLightStateList(LightStateList *list);

/**
 * Frees the memory occupied by the given list of signal indications.
 *
 * @param list a pointer to the list of signal indications to free from memory
 */
void DestroySignalIndicationList(SignalIndicationList *list);

/**
 * Frees the memory occupied by the given vehicle.
 *
 * @param vehicle a pointer to the vehicle to free from memory.
 */
void DestroyVehicle(Vehicle *vehicle);

/**
 * Frees the memory occupied by the given list of vehicles.
 *
 * @param list a pointer to the list of vehicles to free from memory
 */
void DestroyVehicleList(VehicleList *list);

/**
 * Returns the distance between two given points.
 *
 * @param x1 the x coordinate of the first point
 * @param y1 the y coordinate of the first point
 * @param x2 the x coordinate of the second point
 * @param y2 the y coordinate of the second point
 * @return the distance between the two given points
 */
double Dist(double x1, double y1, double x2, double y2);

/**
 * Returns a pointer to a list of signal indications extracted from the given
 * list of light states.
 *
 * @param light_states a pointer to a list of light states
 * @return a pointer to the list of extracted signal indications
 */
SignalIndicationList *GetCurrentSignalStates(LightStateList *light_states);

/**
 * Returns a lane in the given lane manager with the given lane ID.
 *
 * @param lane_id the lane ID to match
 * @param lane_manager the lane manager to search
 * @return a pointer to a lane in the given lane manager with the specified
 * lane ID
 */
Lane *GetLaneById(int lane_id, LaneManager *lane_manager);

/**
 * Returns a pointer to a light state from the given list that has the
 * specified lane ID.
 *
 * @param states a pointer to the list of light states to search
 * @param lane_id the lane ID to match
 * @return a pointer to a light state from the given list that has the
 * specified lane ID
 */
LightState *GetLightStateByLaneId(LightStateList *light_states, int lane_id);

/**
 * Returns a pointer to a list of signal indications from the given list that
 * have the specified lane ID.
 *
 * @param signals a pointer to the list of signal indications to search
 * @param lane_id the lane ID to match
 * @return a pointer to a list of signal indications from the given list that
 * have the specified lane ID
 */
SignalIndicationList *GetSignalsByLaneId(
    SignalIndicationList *signals, int lane_id);

/**
 * Returns a pointer to a vehicle from the given list that
 * has the specified vehicle ID.
 *
 * @param vehicles a pointer to the list of vehicles to search
 * @param vehicle_id the vehicle ID to match
 * @return a pointer to a vehicle from the given list that
 * has the specified vehicle ID
 */
Vehicle *GetVehicleById(
    VehicleList *vehicles, int vehicle_id);

/**
 * Returns a pointer to a vehicle list that contains the given vehicles sorted
 * by their distance (closest to farthest) from the given reference
 * coordinates.
 *
 * @param list a pointer to the list of vehicles to sort
 * @param x the x coordinate of the reference point
 * @param y the y coordinate of the reference point
 * @return a pointer to the sorted list of vehicles
 */
VehicleList *SortByDist(VehicleList *list, double x, double y);

/**
 * Returns whether the given vehicle is in the logout zone of the
 * intersection.
 *
 * @param lane_manager the intersection lane manager
 * @param vehicle a pointer to the vehicle to test
 * @param time the current time (in seconds)
 * @return a value of 1 (true) if the vehicle is in the logout zone, otherwise
 * a value of 0 (false) is returned
 */
int VehicleInLogoutZone(
    LaneManager *lane_manager, Vehicle *vehicle, double time);

/**
 * Returns whether a given vehicle ID is in the given list of vehicles.
 *
 * @param id the vehicle ID to match
 * @param list a pointer to the list of vehicles to search
 * @return a value of 1 (true) if the ID is in the list of vehicles, otherwise
 * a value of 0 (false) is returned
 */
int VehicleInList(int id, VehicleList *list);

/**
 * Returns a pointer to a list of vehicles from the given list that occupy the
 * lane with the given lane ID.
 *
 * @param vehicles a pointer to the list of vehicles to search
 * @param lane_id the lane ID to match
 * @return a pointer to a list of vehicles from the given list that occupy the
 * lane with the given lane ID
 */
VehicleList *VehiclesInLane(VehicleList *vehicles, int lane_id);

#endif
