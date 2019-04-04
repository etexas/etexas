/**
 * Defines the functions to implement an algorithm that calculates queue
 * lengths from a given list of vehicles and a corresponding lane manager.
 *
 * @author emyers
 */

// library files
#include <intellifusion_data_model.h>  // model data structures
#include <intellifusion_config.h>      // jam spacing

// project files
#include "queue_length_calculator.h"  // function implementations
#include "moe_data_model.h"           // model data structures
#include "moe_config.h"               // queue speed

// forward function declarations
static int ContainsVehicleWithId(VehicleList *list, int id);
static Queue *CreateQueueForLane(
    VehicleList *current_vehicles, VehicleList *previous_vehicles,
    Lane *lane);
static QueueList *FilterByLane(VehicleList *vehicles);
static Vehicle *GetVehicleWithId(VehicleList *list, int id);

/**
 * Returns a pointer to a list of queue lengths for the given queues.
 *
 * @param queues a pointer to the queues for length calculation
 * @param lane_manager a pointer to the lane manager
 * @return a pointer to a list of calculated queue lengths
 */
QueueLengthList *CalculateQueueLengths(
    QueueList *queues, LaneManager *lane_manager) {

  QueueLengthList *queue_lengths = NULL;
  QueueList *queue_iterator;
  VehicleList *vehicle_iterator;
  QueueLength *queue_length;

  int i;

  queue_iterator = queues;
  while (queue_iterator != NULL) {

    // find the lane for the queue
    for (i = 0; i < lane_manager->lane_count; i++) {
      if (lane_manager->lanes[i].id == queue_iterator->queue->lane_id) {
        break;
      }
    }

    // find the last vehicle in the queue
    vehicle_iterator = queue_iterator->queue->vehicles;
    if (vehicle_iterator != NULL) {
      while (vehicle_iterator->next != NULL) {
        vehicle_iterator = vehicle_iterator->next;
      }
    }

    queue_length = malloc(sizeof(QueueLength));
    queue_length->lane_id = queue_iterator->queue->lane_id;

    if (vehicle_iterator == NULL) {

      queue_length->length = 0.0;

    } else {

      queue_length->length = Dist(
          lane_manager->lanes[i].nodes[0].x,
          lane_manager->lanes[i].nodes[0].y, vehicle_iterator->vehicle->x,
          vehicle_iterator->vehicle->y) + vehicle_iterator->vehicle->length;
    }

    queue_lengths = AddQueueLengthToList(queue_lengths, queue_length);
    queue_iterator = queue_iterator->next;
  }

  return queue_lengths;
}

/**
 * Returns whether the given list of vehicles contains a vehicle with the
 * specified ID.
 *
 * @param list a pointer to the list of vehicles to search
 * @param id the vehicle ID to match
 * @return a value of 1 (true) if a vehicle with the given ID is found,
 * otherwise a value of 0 (false) is returned
 */
static int ContainsVehicleWithId(VehicleList *list, int id) {

  VehicleList *vehicle_iterator = list;

  while (vehicle_iterator) {

    if (vehicle_iterator->vehicle->id == id) {
      return 1;
    }

    vehicle_iterator = vehicle_iterator->next;
  }

  return 0;
}

/**
 * Returns a pointer to a queue of vehicles in the given lane for the current
 * time step. The queue is calculated based on vehicles that are in the
 * current time step that were also in the last time step (i.e. still queued)
 * and vehicles that are moving below a certain speed and within a certain
 * distance of the preceding vehicle (i.e. stopped traffic).
 *
 * @param current_vehicles a pointer to a list of vehicles in the given lane
 * @param previous_vehicles a pointer to a list of vehicles previously in the
 * given lane
 * @param lane a pointer to the lane containing the given vehicles
 * @return a pointer to the queue of vehicles currently in the lane
 */
static Queue *CreateQueueForLane(
    VehicleList *current_vehicles, VehicleList *previous_vehicles,
    Lane *lane) {

  double curx = lane->nodes[0].x;
  double cury = lane->nodes[0].y;
  VehicleList *queued_vehicles = NULL;

  Vehicle *vehicle;
  VehicleList *sorted_vehicles = SortByDist(current_vehicles, curx, cury);
  VehicleList *vehicle_iterator = sorted_vehicles;

  while (vehicle_iterator) {

    vehicle = vehicle_iterator->vehicle;

    if (ContainsVehicleWithId(previous_vehicles, vehicle->id)) {

      // add the vehicle to the queued vehicles
      curx = vehicle->x;
      cury = vehicle->y;
      queued_vehicles = AddVehicleToList(
          queued_vehicles, CopyVehicle(vehicle));

    } else if (vehicle->speed < GetQueueSpeed()
        && Dist(curx, cury, vehicle->x, vehicle->y) < GetJamSpacing()) {

      // add the vehicle to the queued vehicles
      curx = vehicle->x;
      cury = vehicle->y;
      queued_vehicles = AddVehicleToList(
          queued_vehicles, CopyVehicle(vehicle));

    } else {

      break;
    }

    vehicle_iterator = vehicle_iterator->next;
  }

  // for each vehicle previously in the lane
  vehicle_iterator = previous_vehicles;
  while (vehicle_iterator) {

    vehicle = vehicle_iterator->vehicle;

    // if the vehicle is not already queued and is still in the lane
    if (!ContainsVehicleWithId(queued_vehicles, vehicle->id)
        && ContainsVehicleWithId(sorted_vehicles, vehicle->id)) {

      // add the vehicle to the queued vehicles
      queued_vehicles = AddVehicleToList(
          queued_vehicles, CopyVehicle(vehicle));
    }

    vehicle_iterator = vehicle_iterator->next;
  }

  Queue *queue = (Queue *) malloc(sizeof(Queue));
  queue->lane_id = lane->id;
  queue->vehicles = queued_vehicles;

  return queue;
}

/**
 * Returns a pointer to a new queue length calculator.
 *
 * @return a pointer to a new queue length calculator
 */
QueueLengthCalculator *CreateQueueLengthCalculator() {

  QueueLengthCalculator *queue_length_calculator =
      malloc(sizeof(QueueLengthCalculator));

  queue_length_calculator->previous_queues = NULL;

  return queue_length_calculator;
}

/**
 * Frees the memory occupied by the given queue length calculator.
 *
 * @param queue_length_calculator a pointer to the queue length calculator to
 * free from memory
 */
void DestroyQueueLengthCalculator(
    QueueLengthCalculator *queue_length_calculator) {

  DestroyQueueList(queue_length_calculator->previous_queues);
  free(queue_length_calculator);
}

/**
 * Returns a pointer to a list of queues that contain the given list of
 * vehicles sorted into the appropriate lanes.
 *
 * @param vehicles a pointer to the list of vehicles
 * @return a pointer to the list of queues containing the sorted vehicles
 */
static QueueList *FilterByLane(VehicleList *vehicles) {

  Queue *queue;
  QueueList *queues = NULL;
  VehicleList *vehicle_iterator = vehicles;

  while (vehicle_iterator) {

    // get the queue for this lane, adding a new one if it doesn't exist
    queue = GetQueueByLaneId(queues, vehicle_iterator->vehicle->lane_id);

    if (queue == NULL) {

      queue = (Queue *) malloc(sizeof(Queue));
      queue->lane_id = vehicle_iterator->vehicle->lane_id;
      queue->vehicles = NULL;
      queues = AddQueueToList(queues, queue);
    }

    // add the current vehicle to the queue
    queue->vehicles = AddVehicleToList(
        queue->vehicles, CopyVehicle(vehicle_iterator->vehicle));

    vehicle_iterator = vehicle_iterator->next;
  }

  return queues;
}

/**
 * Returns a pointer to the first occurrence of a vehicle with the given ID in
 * the given list of vehicles.
 *
 * @param list a pointer to the list of vehicles to search
 * @param id the vehicle ID to match
 * @return a pointer to the found vehicle (or NULL if none was found)
 */
static Vehicle *GetVehicleWithId(VehicleList *list, int id) {

  VehicleList *vehicle_iterator = list;
  while (vehicle_iterator != NULL) {

    if (vehicle_iterator->vehicle->id == id) {
      return vehicle_iterator->vehicle;
    }

    vehicle_iterator = vehicle_iterator->next;
  }

  return NULL;
}

/**
 * Returns a pointer to a list of queues that reflect the current state of
 * traffic based on the given vehicles and lane manager.
 *
 * @param queue_length_calculator a pointer to the queue length calculator
 * @param vehicles a pointer to a list of current vehicles
 * @param lane_manager a pointer to the lane manager
 * @return a pointer to the list of updated queues
 */
QueueList *UpdateQueues(
    QueueLengthCalculator *queue_length_calculator, VehicleList *vehicles,
    LaneManager *lane_manager) {

  QueueList *queues = NULL;
  Queue *new_queue, *old_queue;
  VehicleList *new_vehicles, *old_vehicles;
  QueueList *filtered = FilterByLane(vehicles);

  int i;
  for (i = 0; i < lane_manager->lane_count; i++) {

    if (lane_manager->lanes[i].type == INBOUND) {

      new_queue = GetQueueByLaneId(filtered, lane_manager->lanes[i].id);

      if (new_queue == NULL) {

        new_vehicles = NULL;

      } else {

        new_vehicles = new_queue->vehicles;
      }

      old_queue = GetQueueByLaneId(
          queue_length_calculator->previous_queues,
          lane_manager->lanes[i].id);

      if (old_queue == NULL) {

        old_vehicles = NULL;

      } else {

        old_vehicles = old_queue->vehicles;
      }

      queues = AddQueueToList(queues, CreateQueueForLane(
          new_vehicles, old_vehicles, &(lane_manager->lanes[i])));
    }
  }

  // free memory
  DestroyQueueList(filtered);
  DestroyQueueList(queue_length_calculator->previous_queues);
  queue_length_calculator->previous_queues = CopyQueueList(queues);

  return queues;
}
