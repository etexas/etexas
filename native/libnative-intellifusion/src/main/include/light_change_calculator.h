/**
 * Defines the supporting structure and functions that should be implemented
 * by algorithms that calculate light change events from the signal
 * indications that exist for a given point in time.
 *
 * @author emyers
 */

#ifndef ETEXAS_INTELLIFUSION_LIGHT_CHANGES
#define ETEXAS_INTELLIFUSION_LIGHT_CHANGES

// project files
#include "intellifusion_data_model.h"  // model data structures

// forward type declarations
typedef struct LightChangeCalculator LightChangeCalculator;

/**
 * The light state information for a given point in time.
 */
struct LightChangeCalculator {

  /**
   * The previous list of light states.
   */
  LightStateList *previous_states;

  /**
   * The current list of light states.
   */
  LightStateList *current_states;

  /**
   * The current time (s).
   */
  double time;

  /**
   * The lane IDs where light change events are calculated.
   */
  IntegerList *lane_ids;
};

/**
 * Returns a pointer to a list of light change events calculated from the
 * given list of signal indications and the current time.
 *
 * @param light_change_calculator a pointer to the light change calculator
 * @param signals a pointer to a list of signal indications
 * @param time the current time (s)
 * @return a pointer to a list of light change events calculated from the
 * given list of signal indications and the current time
 */
LightChangeList *CalculateLightChanges(
    LightChangeCalculator *light_change_calculator,
    SignalIndicationList *signals, double time);

/**
 * Returns a pointer to a new light change calculator.
 *
 * @param lane_ids a pointer to a list of lane ID numbers
 * @return a pointer to the new light change calculator
 */
LightChangeCalculator *CreateLightChangeCalculator(IntegerList *lane_ids);

/**
 * Frees the memory occupied by the given light change calculator.
 *
 * @param light_change_calculator a pointer to the light change calculator to
 * free from memory
 */
void DestroyLightChangeCalculator(
    LightChangeCalculator *light_change_calculator);

#endif
