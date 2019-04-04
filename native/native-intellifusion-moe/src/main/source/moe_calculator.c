/**
 * Defines functions to implement an algorithm that calculates phase failure
 * and queue length MOEs from a given list of vehicles and a given list of
 * signal indications.
 *
 * @author emyers
 */

// library files
#include <intellifusion_data_model.h>  // model data structures
#include <light_change_calculator.h>   // light change calculator
#include <logging.h>                   // log statements

// project files
#include "moe_calculator.h"            // function implementations
#include "phase_failure_calculator.h"  // phase failure calculator
#include "queue_length_calculator.h"   // queue length calculator

/**
 * Updates the MOEs for intersection evaluation based on the given vehicles,
 * signal indications, and lane manager.
 *
 * @param moe_calculator a pointer to the MOE calculator
 * @param vehicles a pointer to the list of vehicles
 * @param signals a pointer to the list of signals
 * @param lane_manager a pointer to the lane manager
 * @param time the current time (s)
 */
void CalculateMoes(
    MoeCalculator *moe_calculator, VehicleList *vehicles,
    SignalIndicationList *signals, LaneManager *lane_manager, double time) {

  QueueList *queues = UpdateQueues(
      moe_calculator->queue_length_calculator, vehicles, lane_manager);

  Log("*******************************\n");
  Log("Created the following queues...\n");
  Log("*******************************\n");
  LogQueueList(queues, 0);

  QueueLengthList *queue_lengths =
      CalculateQueueLengths(queues, lane_manager);

  Log("*****************************************\n");
  Log("Calculated the following queue lengths...\n");
  Log("*****************************************\n");
  LogQueueLengthList(queue_lengths, 0);

  /*
  if (moe_calculator->light_change_calculator == NULL) {

    IntegerList *lanes;

    int i;
    for (i = 0; i < lane_manager->lane_count; i++) {
      lanes = AddIntegerToList(lanes, lane_manager->lanes[i].id);
    }

    moe_calculator->light_change_calculator =
        CreateLightChangeCalculator(lanes);
  }

  LightChangeList *light_changes = CalculateLightChanges(
      moe_calculator->light_change_calculator, signals, time);

  int current_fails = CalculatePhaseFails(
      moe_calculator->phase_failure_calculator, queues, light_changes);

  int total_fails = GetTotalFails(
      moe_calculator->phase_failure_calculator, current_fails);

  Log("******************************************\n");
  Log("Calculated the following phase failures...\n");
  Log("******************************************\n");
  Log("<phase_failures current=%d total=%d/>\n", current_fails, total_fails);
*/
  // free memory
  DestroyQueueLengthList(queue_lengths);
  DestroyQueueList(queues);
}

/**
 * Returns a pointer to a new MOE calculator.
 *
 * @return a pointer to a new MOE calculator
 */
MoeCalculator *CreateMoeCalculator() {

  MoeCalculator *moe_calculator = malloc(sizeof(MoeCalculator));
  moe_calculator->phase_failure_calculator = CreatePhaseFailureCalculator();
  moe_calculator->queue_length_calculator = CreateQueueLengthCalculator();
  moe_calculator->light_change_calculator = NULL;

  return moe_calculator;
}

/**
 * Frees the given MOE calculator from memory.
 *
 * @param moe_calculator a pointer to the MOE calculator to free from memory
 */
void DestroyMoeCalculator(MoeCalculator *moe_calculator) {

  DestroyLightChangeCalculator(moe_calculator->light_change_calculator);
  DestroyQueueLengthCalculator(moe_calculator->queue_length_calculator);
  DestroyPhaseFailureCalculator(moe_calculator->phase_failure_calculator);
  free(moe_calculator);
}
