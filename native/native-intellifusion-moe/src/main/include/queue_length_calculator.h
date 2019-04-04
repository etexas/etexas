/**
 * Defines the supporting structure and functions that should be implemented
 * by algorithms that calculate queue lengths from a given list of vehicles
 * and a corresponding lane manager.
 *
 * @author emyers
 */

#ifndef ETEXAS_INTELLIFUSION_QUEUE_LENGTHS
#define ETEXAS_INTELLIFUSION_QUEUE_LENGTHS

// library files
#include <intellifusion_data_model.h>  // model data structures

// project files
#include "moe_data_model.h"  // model data structures

// forward type declarations
typedef struct QueueLengthCalculator QueueLengthCalculator;

/**
 * A calculator to store previous queues for queue length calculations.
 */
struct QueueLengthCalculator {

  /**
   * A list of previous queues.
   */
  QueueList *previous_queues;
};

/**
 * Returns a pointer to a list of queue lengths for the given queues.
 *
 * @param queues a pointer to the queues for length calculation
 * @param lane_manager a pointer to the lane manager
 * @return a pointer to a list of calculated queue lengths
 */
QueueLengthList *CalculateQueueLengths(
    QueueList *queues, LaneManager *lane_manager);

/**
 * Returns a pointer to a new queue length calculator.
 *
 * @return a pointer to a new queue length calculator
 */
QueueLengthCalculator *CreateQueueLengthCalculator();

/**
 * Frees the memory occupied by the given queue length calculator.
 *
 * @param queue_length_calculator a pointer to the queue length calculator to
 * free from memory
 */
void DestroyQueueLengthCalculator(
    QueueLengthCalculator *queue_length_calculator);

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
    LaneManager *lane_manager);

#endif
