/**
 * Defines the supporting structure and functions that should be implemented
 * by algorithms that calculate phase failures from a given list of vehicle
 * queues and a given list of light changes.
 *
 * @author emyers
 */

#ifndef ETEXAS_INTELLIFUSION_PHASE_FAILURES
#define ETEXAS_INTELLIFUSION_PHASE_FAILURES

// library files
#include <intellifusion_data_model.h>  // model data structures

// project files
#include "moe_data_model.h"  // model data structures

// forward type declarations
typedef struct PhaseFailureCalculator PhaseFailureCalculator;

/**
 * A calculator to store previous queues and phase failures for phase failure
 * calculations.
 */
struct PhaseFailureCalculator {

  /**
   * A list of previous queues.
   */
  QueueList *previous_queues;

  /**
   * The cumulative number of phase failures.
   */
  int failures;
};

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
    LightChangeList *light_changes);

/**
 * Returns a pointer to a new phase failure calculator.
 *
 * @return a pointer to a new phase failure calculator
 */
PhaseFailureCalculator *CreatePhaseFailureCalculator();

/**
 * Frees the memory occupied by the given phase failure calculator.
 *
 * @param phase_failure_calculator a pointer to the phase failure calculator
 * to free from memory.
 */
void DestroyPhaseFailureCalculator(
    PhaseFailureCalculator *phase_failure_calculator);

/**
 * Returns the total number of phase failures after updating for the phase
 * failures that have occurred since the last report.
 *
 * @param phase_failure_calculator a pointer to the phase failure calculator
 * @param failures the number of failures since the last report
 */
int GetTotalFails(
    PhaseFailureCalculator *phase_failure_calculator, int failures);


#endif
