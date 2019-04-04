/**
 * Implements utility functions that support basic type operations on traffic
 * entities from the IntelliFusion data model.
 *
 * @author emyers
 */

// system files
#include <math.h>    // sqrt
#include <stdlib.h>  // malloc

// project files
#include "intellifusion_data_model.h"  // model data structures, functions
#include "intellifusion_config.h"      // obtaining intersection id

/**
 * Returns a pointer to a list of integers that includes the given integer and
 * those already present in the given list. For this implementation, the given
 * integer is appended to the end of the given list.
 *
 * @param list a pointer to a list of integers
 * @param integer the integer to include
 * @return a pointer to a list containing all of the given integers
 */
IntegerList *AddIntegerToList(IntegerList *list, int integer) {

  IntegerList *temp = list;

  // if the given list is null
  if (list == NULL) {

    // create a new list that contains the given element
    temp = malloc(sizeof(IntegerList));

    if (temp != NULL) {

      temp->next = NULL;
      temp->integer = integer;
      return temp;

    } else {

      return list;
    }
  }

  // find the last element of the list
  while (temp->next != NULL) {
    temp = temp->next;
  }

  // add the given element to the end of the existing list
  temp->next = malloc(sizeof(IntegerList));

  if (temp->next != NULL) {

    temp = temp->next;
    temp->next = NULL;
    temp->integer = integer;

    return list;
  }

  return list;
}

/**
 * Returns a pointer to a list of light change events that includes the given
 * light change event and those already present in the given list.
 *
 * @param list a pointer to a list of light change events
 * @param light_change a pointer to the light change event to include
 * @return a pointer to a list containing all of the given light change events
 */
LightChangeList *AddLightChangeToList(
    LightChangeList *list, LightChange *light_change) {

  LightChangeList *temp = list;

  // if the given list is null
  if (list == NULL) {

    // create a new list that contains the given element
    temp = malloc(sizeof(LightChangeList));

    if (temp != NULL) {

      temp->next = NULL;
      temp->light_change = light_change;
      return temp;

    } else {

      return list;
    }
  }

  // find the last element of the list
  while (temp->next != NULL) {
    temp = temp->next;
  }

  // add the given element to the end of the existing list
  temp->next = malloc(sizeof(LightChangeList));

  if (temp->next != NULL) {

    temp = temp->next;
    temp->next = NULL;
    temp->light_change = light_change;

    return list;
  }

  return list;
}

/**
 * Returns a pointer to a list of light states that includes the given light
 * state and those already present in the given list.
 *
 * @param list a pointer to a list of light states
 * @param light_state a pointer to the light state to include
 * @return a pointer to a list containing all of the given light states
 */
LightStateList *AddLightStateToList(
    LightStateList *list, LightState *light_state) {

  LightStateList *temp = list;

  // if the given list is null
  if (list == NULL) {

    // create a new list that contains the given element
    temp = malloc(sizeof(LightStateList));

    if (temp != NULL) {

      temp->next = NULL;
      temp->light_state = light_state;
      return temp;

    } else {

      return list;
    }
  }

  // find the last element of the list
  while (temp->next != NULL) {
    temp = temp->next;
  }

  // add the given element to the end of the existing list
  temp->next = malloc(sizeof(LightStateList));

  if (temp->next != NULL) {

    temp = temp->next;
    temp->next = NULL;
    temp->light_state = light_state;

    return list;
  }

  return list;
}

/**
 * Returns a pointer to a list of signal indications that includes the given
 * signal indication and those already present in the given list. For this
 * implementation, the given signal indication is appended to the end of the
 * given list.
 *
 * @param list a pointer to a list of signal indications
 * @param signal the signal indication to include
 * @return a pointer to a list containing all of the given signal indications
 */
SignalIndicationList *AddSignalIndicationToList(
    SignalIndicationList *list, SignalIndication *signal) {

  SignalIndicationList *temp = list;

  // if the given list is null
  if (list == NULL) {

    // create a new list that contains the given element
    temp = malloc(sizeof(SignalIndicationList));

    if (temp != NULL) {

      temp->next = NULL;
      temp->signal = signal;
      return temp;

    } else {

      return list;
    }
  }

  // find the last element of the list
  while (temp->next != NULL) {
    temp = temp->next;
  }

  // add the given element to end of the existing list
  temp->next = malloc(sizeof(SignalIndicationList));

  if (temp->next != NULL) {

    temp = temp->next;
    temp->next = NULL;
    temp->signal = signal;

    return list;
  }

  return list;
}

/**
 * Returns a pointer to a list of vehicles that includes the given vehicle and
 * those already present in the given list. For this implementation, the given
 * vehicle is appended to the end of the given list.
 *
 * @param list a pointer to a list of vehicles
 * @param vehicle the vehicle to include
 * @return a pointer to a list containing all of the given vehicles
 */
VehicleList *AddVehicleToList(VehicleList *list, Vehicle *vehicle) {

  VehicleList *temp = list;

  // if the given list is null
  if (list == NULL) {

    // create a new list that contains the given element
    temp = malloc(sizeof(VehicleList));

    if (temp != NULL) {

      temp->next = NULL;
      temp->vehicle = vehicle;
      return temp;

    } else {

      return list;
    }
  }

  // find the last element of the list
  while (temp->next != NULL) {
    temp = temp->next;
  }

  // add the given element to end of the existing list
  temp->next = malloc(sizeof(VehicleList));

  if (temp->next != NULL) {

    temp = temp->next;
    temp->next = NULL;
    temp->vehicle = vehicle;

    return list;
  }

  return list;
}

/**
 * Frees the memory occupied by the given list (Note: does not free vehicles themselves).
 *
 * @param list a pointer to the list to free from memory
 */
void CleanupVehicleList(VehicleList *list) {

  VehicleList *list_iterator = list;

  while (list_iterator != NULL) {

    VehicleList *temp = list_iterator->next;
    free(list_iterator);
    list_iterator = temp;
  }
}

/**
 * Returns a pointer to a copy of the given vehicle.
 *
 * @param vehicle a pointer to the vehicle to copy
 * @return a pointer to a copy of the given vehicle
 */
Vehicle *CopyVehicle(Vehicle *vehicle) {

  // create a new vehicle
  Vehicle *copy = (Vehicle *) malloc(sizeof(Vehicle));

  // copy the attributes from the given vehicle
  copy->id = vehicle->id;
  copy->x = vehicle->x;
  copy->y = vehicle->y;
  copy->z = vehicle->z;
  copy->speed = vehicle->speed;
  copy->length = vehicle->length;
  copy->width = vehicle->width;
  copy->lane_id = vehicle->lane_id;
  copy->heading = vehicle->heading;
  copy->height = vehicle->height;
  copy->type = vehicle->type;

  return copy;  // return the pointer to the created vehicle
}

/**
 * Frees the memory occupied by the given info model.
 *
 * @param info_model a pointer to the info model to free from memory
 */
void DestroyInfoModel(InfoModel *info_model) {
  if (info_model) {
    // TODO: bbadillo - this will need to cleanup LaneManager when
    // it is copied rather than kept. See note in ParseModel function.
    DestroySignalIndicationList(info_model->signals);
    DestroyVehicleList(info_model->vehicles);
    free(info_model);
  }
}

/**
 * Frees the memory occupied by the given list of integers.
 *
 * @param list a pointer to the list of integers to free from memory
 */
void DestroyIntegerList(IntegerList *list) {

  IntegerList *list_iterator = list;
  IntegerList *temp = NULL;

  while (list_iterator != NULL) {

    temp = list_iterator->next;
    free(list_iterator);
    list_iterator = temp;
  }
}

/**
 * Frees the memory occupied by the given lane.
 *
 * @param lane a pointer to the lane to free from memory.
 */
void DestroyLane(Lane *lane) {

  free(lane->nodes);
  free(lane->movements);
}

/**
 * Frees the memory occupied by the given lane manager.
 *
 * @param lane_manager a pointer to the lane manager to free from memory
 */
void DestroyLaneManager(LaneManager *lane_manager) {

  int i;
  for (i = 0; i < lane_manager->lane_count; i++) {
    DestroyLane(&(lane_manager->lanes[i]));
  }
  free(lane_manager->lanes);
  free(lane_manager);
}

/**
 * Frees the memory occupied by the given list of lane managers.
 *
 * @param list a pointer to the list of lane managers to free from memory
 */
void DestroyLaneManagerList(LaneManagerList *list) {

  while (list != NULL) {
    list = (LaneManagerList *) DestroyLaneManagerListNode(list);
  }
}

/**
 * Frees the memory occupied by the given lane manager list node.
 *
 * @param node a pointer to the lane manager list node to free from memory
 * @return a pointer to the next lane manager list node
 */
LaneManagerList *DestroyLaneManagerListNode(LaneManagerList *node) {

  LaneManagerList* next = node->next;
  DestroyLaneManager(node->lane_manager);
  free(node);

  return next;
}

/**
 * Frees the memory occupied by the given list of light change events.
 *
 * @param list a pointer to the list of light change events to free from
 * memory
 */
void DestroyLightChangeList(LightChangeList *list) {

  LightChangeList *list_iterator = list;
  LightChangeList *temp = NULL;

  while (list_iterator != NULL) {

    temp = list_iterator->next;
    DestroyIntegerList(list_iterator->light_change->lane_ids);
    free(list_iterator->light_change);
    free(list_iterator);
    list_iterator = temp;
  }
}

/**
 * Frees the memory occupied by the given list of light states.
 *
 * @param list a pointer to the list of light states to free from memory
 */
void DestroyLightStateList(LightStateList *list) {

  LightStateList *list_iterator = list;
  LightStateList *temp = NULL;

  while (list_iterator != NULL) {

    temp = list_iterator->next;
    free(list_iterator->light_state);
    free(list_iterator);
    list_iterator = temp;
  }
}

/**
 * Frees the memory occupied by the given list of signal indications.
 *
 * @param list a pointer to the list of signal indications to free from memory
 */
void DestroySignalIndicationList(SignalIndicationList *list) {

  SignalIndicationList* list_iterator = list;
  SignalIndicationList* temp;

  while (list_iterator != NULL) {

    free(list_iterator->signal);
    temp = list_iterator;
    list_iterator = list_iterator->next;
    free(temp);
  }
}

/**
 * Frees the memory occupied by the given vehicle.
 *
 * @param vehicle a pointer to the vehicle to free from memory.
 */
void DestroyVehicle(Vehicle *vehicle) {

  free(vehicle);
}

/**
 * Frees the memory occupied by the given list of vehicles.
 *
 * @param list a pointer to the list of vehicles to free from memory
 */
void DestroyVehicleList(VehicleList *list) {

  VehicleList *list_iterator = list;
  VehicleList *temp = NULL;

  while (list_iterator != NULL) {

    temp = list_iterator->next;
    free(list_iterator->vehicle);
    free(list_iterator);
    list_iterator = temp;
  }
}

/**
 * Returns the distance between two given points.
 *
 * @param x1 the x coordinate of the first point
 * @param y1 the y coordinate of the first point
 * @param x2 the x coordinate of the second point
 * @param y2 the y coordinate of the second point
 * @return the distance between the two given points
 */
double Dist(double x1, double y1, double x2, double y2) {

  double dx, dy;
  dx = x2 - x1;
  dy = y2 - y1;

  return sqrt((dx * dx) + (dy * dy));
}

/**
 * Returns a pointer to a list of signal indications extracted from the given
 * list of light states.
 *
 * @param light_states a pointer to a list of light states
 * @return a pointer to the list of extracted signal indications
 */
SignalIndicationList *GetCurrentSignalStates(LightStateList *light_states) {

  SignalIndicationList *signals = NULL;
  LightStateList *state_iterator = light_states;

  while (state_iterator != NULL) {

    SignalIndicationList *tmp = (SignalIndicationList *) malloc(
        sizeof(SignalIndicationList));

    tmp->signal = (SignalIndication *) malloc(sizeof(SignalIndication));
    tmp->signal->lane_id = state_iterator->light_state->lane_id;
    tmp->signal->intersection_id = GetIntersectionId();
    tmp->signal->color = state_iterator->light_state->color;
    tmp->signal->type = BALL;
    tmp->signal->state = STEADY;
    tmp->signal->time_to_change = state_iterator->light_state->time_to_change;
    tmp->next = signals;
    signals = tmp;
    state_iterator = state_iterator->next;
  }

  return signals;
}

/**
 * Returns a pointer to a lane in the given lane manager with the given ID.
 *
 * @param lane_id the lane ID to match
 * @param lane_manager the lane manager to search
 * @return a pointer to a lane in the given lane manager with the given ID
 */
Lane *GetLaneById(int lane_id, LaneManager *lane_manager) {

  int i;
  for (i = 0; i < lane_manager->lane_count; i++) {
    if (lane_manager->lanes[i].id == lane_id) {
      return &(lane_manager->lanes[i]);
    }
  }

  return NULL;
}

/**
 * Returns a pointer to a light state from the given list that has the
 * specified lane ID.
 *
 * @param states a pointer to the list of light states to search
 * @param lane_id the lane ID to match
 * @return a pointer to a light state from the given list that has the
 * specified lane ID (or NULL if none exists)
 */
LightState *GetLightStateByLaneId(LightStateList *light_states, int lane_id) {

  LightStateList *state_iterator = light_states;

  while (state_iterator != NULL) {

    if (state_iterator->light_state->lane_id == lane_id) {
      return state_iterator->light_state;
    }

    state_iterator = state_iterator->next;
  }

  return NULL;
}

/**
 * Returns a pointer to a vehicle from the given list that
 * has the specified vehicle ID.
 *
 * @param vehicles a pointer to the list of vehicles to search
 * @param vehicle_id the vehicle ID to match
 * @return a pointer to a vehicle from the given list that
 * has the specified vehicle ID or NULL if not found
 */
Vehicle *GetVehicleById(
    VehicleList *vehicles, int vehicle_id) {

  VehicleList *vehicle_iterator;

  for (vehicle_iterator = vehicles; vehicle_iterator != NULL;
      vehicle_iterator = vehicle_iterator->next) {
    if (vehicle_iterator->vehicle->id == vehicle_id) {
      return vehicle_iterator->vehicle;
    }
  }

  return NULL;
}

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
    SignalIndicationList *signals, int lane_id) {

  SignalIndicationList *list = NULL;
  SignalIndicationList *list_iterator = signals;

  while (list_iterator != NULL) {
    if (list_iterator->signal->lane_id == lane_id) {
      list = AddSignalIndicationToList(list, list_iterator->signal);
    }

    list_iterator = list_iterator->next;
  }

  return list;
}

/**
 * Returns a pointer to a vehicle list that contains the given vehicles sorted
 * by their distance (closest to farthest) from the given reference
 * coordinates. Note: this function preserves list nodes so that references
 * to them before the call remain intact.
 *
 * @param list a pointer to the list of vehicles to sort
 * @param x the x coordinate of the reference point
 * @param y the y coordinate of the reference point
 * @return a pointer to the sorted list of vehicles
 */
VehicleList *SortByDist(VehicleList *list, double x, double y) {

  VehicleList *sorted_vehicles = NULL;
  VehicleList *vehicle_iterator = list;
  VehicleList *sorted_iterator;

  Vehicle *vehicle, *sorted_vehicle;

  while (vehicle_iterator != NULL) {

    VehicleList *insertion_point = NULL;

    // Detach the current node and point iterator to next vehicle
    VehicleList *node_to_sort = vehicle_iterator;
    vehicle = node_to_sort->vehicle;
    vehicle_iterator = node_to_sort->next;
    node_to_sort->next = NULL;

    // find where in the sorted list the current element should go
    sorted_iterator = sorted_vehicles;
    while (sorted_iterator != NULL) {

      sorted_vehicle = sorted_iterator->vehicle;

      if (Dist(x, y, vehicle->x, vehicle->y)
          <= Dist(x, y, sorted_vehicle->x, sorted_vehicle->y)) {

        // insertion_point points to the node before this one
        break;
      }

      insertion_point = sorted_iterator;
      sorted_iterator = sorted_iterator->next;
    }

    if (insertion_point == NULL) {

      // add the vehicle to the front of the list
      node_to_sort->next = sorted_vehicles;
      sorted_vehicles = node_to_sort;

    } else {

      // insert the vehicle at the appropriate position
      VehicleList *rest_of_list = insertion_point->next;
      insertion_point->next = node_to_sort;
      insertion_point->next->next = rest_of_list;
    }

  }

  return sorted_vehicles;
}

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
    LaneManager *lane_manager, Vehicle *vehicle, double time) {

  Lane *lane = GetLaneById(vehicle->lane_id, lane_manager);
  LaneNode* last_node = &(lane->nodes[lane->node_count - 1]);
  if (lane->type == OUTBOUND) {
    if (vehicle->speed * time * 100 <=
        Dist(vehicle->x, vehicle->y, last_node->x, last_node->y)) {

      return 1;
    }
  }

  return 0;
}

/**
 * Returns whether a given vehicle ID is in the given list of vehicles.
 *
 * @param id the vehicle ID to match
 * @param list a pointer to the list of vehicles to search
 * @return a value of 1 (true) if the ID is in the list of vehicles, otherwise
 * a value of 0 (false) is returned
 */
int VehicleInList(int id, VehicleList *list) {

  VehicleList *vehicle_iterator = list;

  while (vehicle_iterator != NULL) {

    if (id == vehicle_iterator->vehicle->id) {
      return 1;
    }

    vehicle_iterator = vehicle_iterator->next;
  }

  return 0;
}

/**
 * Returns a pointer to a list of vehicles from the given list that occupy the
 * lane with the given lane ID.
 *
 * @param vehicles a pointer to the list of vehicles to search
 * @param lane_id the lane ID to match
 * @return a pointer to a list of vehicles from the given list that occupy the
 * lane with the given lane ID
 */
VehicleList *VehiclesInLane(VehicleList *vehicles, int lane_id) {

  VehicleList *list = NULL;
  VehicleList *list_iterator = vehicles;

  while (list_iterator != NULL) {
    if (list_iterator->vehicle->lane_id == lane_id) {
      VehicleList *temp = (VehicleList *) malloc(sizeof(VehicleList));
      temp->vehicle = list_iterator->vehicle;
      temp->next = list;
      list = temp;
    }

    list_iterator = list_iterator->next;
  }

  return list;
}
