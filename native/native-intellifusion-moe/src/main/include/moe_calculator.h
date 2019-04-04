/**
 * Defines the supporting structure and functions that should be implemented
 * by algorithms that calculate phase failure and queue length MOEs from
 * a given list of vehicles and a given list of signal indications.
 *
 * @author emyers
 */

#ifndef ETEXAS_INTELLIFUSION_MOES
#define ETEXAS_INTELLIFUSION_MOES

// library files
#include <intellifusion_data_model.h>  // model data structures
#include <light_change_calculator.h>   // light change calculator

// project files
#include "phase_failure_calculator.h"  // phase failure calculator
#include "queue_length_calculator.h"   // queue length calculator

// forward type declarations
typedef struct MoeCalculator MoeCalculator;

/**
 * A master MOE calculator to store other MOE calculators.
 */
struct MoeCalculator {

  /**
   * A calculator for phase failures.
   */
  PhaseFailureCalculator *phase_failure_calculator;

  /**
   * A calculator for queue lengths.
   */
  QueueLengthCalculator *queue_length_calculator;

  /**
   * A calculator for light changes.
   */
  LightChangeCalculator *light_change_calculator;
};

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
    SignalIndicationList *signals, LaneManager *lane_manager, double time);

/**
 * Returns a pointer to a new MOE calculator.
 *
 * @return a pointer to a new MOE calculator
 */
MoeCalculator *CreateMoeCalculator();

/**
 * Frees the given MOE calculator from memory.
 *
 * @param moe_calculator a pointer to the MOE calculator to free from memory
 */
void DestroyMoeCalculator(MoeCalculator *moe_calculator);

#endif
