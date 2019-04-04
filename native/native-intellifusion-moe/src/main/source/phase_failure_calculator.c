/**
 * Defines the functions to implement an algorithm that calculates phase
 * failures from a given list of vehicle queues and a given list of light
 * changes.
 *
 * @author emyers
 */

// library files
#include <intellifusion_data_model.h>  // model data structures

// project files
#include "moe_data_model.h"            // model data structures
#include "phase_failure_calculator.h"  // function implementations

// forward function declarations
static LightChange *GetLightChangeByColors(
    LightChangeList *light_changes, SignalColor old_color,
    SignalColor new_color);
static int IsOverlap(
    VehicleList *current_vehicles, VehicleList *previous_vehicles);
static void OnGreen(
    PhaseFailureCalculator *phase_failure_calculator, QueueList *queues,
    IntegerList *lanes);
static int OnRed(
    PhaseFailureCalculator *phase_failure_calculator, QueueList *queues,
    IntegerList *lanes);

/**
 * Returns the number of phase failures (or number of lanes where a phase
 * failure occurred) as calculated from the given values for the current
 * queues and light changes.
 *
 * @param phase_failure_calculator a pointer to the phase failure calculator
 * @param queues a pointer to the current queues
 * @param light_changes a pointer to the current light changes
 * @return the calculated number of phase failures
 */
int CalculatePhaseFails(
    PhaseFailureCalculator *phase_failure_calculator, QueueList *queues,
    LightChangeList *light_changes) {

  int fails = 0;
  LightChange *light_change = NULL;

  light_change = GetLightChangeByColors(light_changes, GREEN, RED);
  if (light_change != NULL) {
    fails += OnRed(phase_failure_calculator, queues, light_change->lane_ids);
  }

  light_change = GetLightChangeByColors(light_changes, YELLOW, RED);
  if (light_change != NULL) {
    fails += OnRed(phase_failure_calculator, queues, light_change->lane_ids);
  }

  light_change = GetLightChangeByColors(light_changes, YELLOW, GREEN);
  if (light_change != NULL) {
    OnGreen(phase_failure_calculator, queues, light_change->lane_ids);
  }

  light_change = GetLightChangeByColors(light_changes, RED, GREEN);
  if (light_change != NULL) {
    OnGreen(phase_failure_calculator, queues, light_change->lane_ids);
  }

  return fails;
}

/**
 * Returns a pointer to a new phase failure calculator.
 *
 * @return a pointer to a new phase failure calculator
 */
PhaseFailureCalculator *CreatePhaseFailureCalculator() {

  PhaseFailureCalculator *phase_failure_calculator;

  phase_failure_calculator = malloc(sizeof(PhaseFailureCalculator));
  phase_failure_calculator->previous_queues = NULL;
  phase_failure_calculator->failures = 0;

  return phase_failure_calculator;
}

/**
 * Frees the memory occupied by the given phase failure calculator.
 *
 * @param phase_failure_calculator a pointer to the phase failure calculator
 * to free from memory.
 */
void DestroyPhaseFailureCalculator(
    PhaseFailureCalculator *phase_failure_calculator) {

  DestroyQueueList(phase_failure_calculator->previous_queues);
  free(phase_failure_calculator);
}

/**
 * Returns a pointer to the first light change in the given list of light
 * changes that matches the change indicated by the given old and new light
 * colors.
 *
 * @param light_changes a pointer to the list of light changes
 * @param old_color the old light color
 * @param new_color the new light color
 * @return a pointer to the first matching light change
 */
static LightChange *GetLightChangeByColors(
    LightChangeList *light_changes, SignalColor old_color,
    SignalColor new_color) {

  LightChangeList *list_iterator = light_changes;

  while (list_iterator != NULL) {

    if (list_iterator->light_change->old_color == old_color
        && list_iterator->light_change->new_color == new_color) {

      return list_iterator->light_change;
    }

    list_iterator = list_iterator->next;
  }

  return NULL;
}

/**
 * Returns the total number of phase failures after updating for the phase
 * failures that have occurred since the last report.
 *
 * @param phase_failure_calculator a pointer to the phase failure calculator
 * @param failures the number of failures since the last report
 */
int GetTotalFails(
    PhaseFailureCalculator *phase_failure_calculator, int failures) {

  phase_failure_calculator->failures += failures;

  return phase_failure_calculator->failures;
}

/**
 * Returns whether the given lists of vehicles share at least one common
 * vehicle.
 *
 * @param current_vehicles a pointer to the first list of vehicles
 * @param previous_vehicles a pointer to the second list of vehicles
 * @return a value of 1 (true) if the lists contain a common vehicle,
 * otherwise a value of 0 (false) is returned
 */
static int IsOverlap(
    VehicleList *current_vehicles, VehicleList *previous_vehicles) {

  VehicleList *current_iterator = current_vehicles;
  VehicleList *previous_iterator = previous_vehicles;

  while (current_iterator != NULL) {

    previous_iterator = previous_vehicles;
    while (previous_iterator != NULL) {
      if (current_iterator->vehicle->id == previous_iterator->vehicle->id)
        return 1;

      previous_iterator = previous_iterator->next;
    }

    current_iterator = current_iterator->next;
  }

  return 0;
}

/**
 * Updates the given queues for each lane whose light changed to green.
 *
 * @param phase_failure_calculator a pointer to the phase failure calculator
 * @param queues a pointer to the queues to update
 * @param lanes a pointer to the IDs of the lanes whose light changed to green
 */
static void OnGreen(
    PhaseFailureCalculator *phase_failure_calculator, QueueList *queues,
    IntegerList *lanes) {

  IntegerList *lane_iterator = lanes;
  QueueList *queue_iterator = NULL;

  while (lane_iterator != NULL) {

    // find existing data for this queue
    queue_iterator = phase_failure_calculator->previous_queues;
    while (queue_iterator != NULL) {

      if (queue_iterator->queue->lane_id == lane_iterator->integer)
        break;

      queue_iterator = queue_iterator->next;
    }

    if (queue_iterator != NULL) {

      DestroyQueue(queue_iterator->queue);
      queue_iterator->queue = CopyQueue(
          GetQueueByLaneId(queues, lane_iterator->integer));

    } else {

      phase_failure_calculator->previous_queues =
          AddQueueToList(phase_failure_calculator->previous_queues,
              CopyQueue(GetQueueByLaneId(
              queues, lane_iterator->integer)));
    }

    lane_iterator = lane_iterator->next;
  }
}

/**
 * Returns the number of lanes that experienced a phase failure when their
 * light changed to red.
 *
 * @param phase_failure_calculator a pointer to the phase failure calculator
 * @param queues a pointer to the current queues
 * @param lanes a pointer to the IDs of the lanes whose light changed to red
 * @return the number of lanes that experienced a phase failure
 */
static int OnRed(
    PhaseFailureCalculator *phase_failure_calculator, QueueList *queues,
    IntegerList *lanes) {

  int failures = 0;
  Queue *previous_queue = NULL;
  Queue *current_queue = NULL;
  IntegerList *lane_iterator = lanes;

  while (lane_iterator != NULL) {

    previous_queue = GetQueueByLaneId(
        phase_failure_calculator->previous_queues, lane_iterator->integer);

    current_queue = GetQueueByLaneId(queues, lane_iterator->integer);

    if (!(previous_queue == NULL || current_queue == NULL))
      failures += IsOverlap(
          current_queue->vehicles, previous_queue->vehicles);

    lane_iterator = lane_iterator->next;
  }

  return failures;
}
