/**
 * Defines the functions to implement an algorithm that calculates light
 * change events from the signal indications that exist for a given point in
 * time.
 *
 * @author emyers
 */

// system files
#include <float.h>  // DBL_MIN

// project files
#include "intellifusion_data_model.h"  // model data structures
#include "light_change_calculator.h"   // function implementations

// forward function declarations
static void AdvanceLightState(LightState *light_state);
static LightStateList *CloneLightStateList(LightStateList *list);
static int CompareColor(SignalColor color, LightState *light_state);
static LightStateList *FindLightStateById(LightStateList *light_state_iterator,
    int lane_id);
static LightState *FoldColorIntoLightState(
    SignalColor color, double time_to_change, LightState *light_state);
static LightState *FoldLightState(SignalIndicationList *signals);
static LightState *FoldSignalIntoLightState(
    SignalIndication *signal, LightState *light_state);

/**
 * Advances the given light state to the next color and resets the time for
 * the light state to change.
 *
 * @param light_state a pointer to the light state to advance
 */
static void AdvanceLightState(LightState *light_state) {

  light_state->time_to_change = -DBL_MIN;

  if (light_state->color == GREEN) {

    light_state->color = YELLOW;

  } else if (light_state->color == YELLOW) {

    light_state->color = RED;

  } else {

    light_state->color = GREEN;
  }
}

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
    SignalIndicationList *signals, const double time) {

  LightChangeList *light_changes = NULL;
  double time_elapsed = time - light_change_calculator->time;
  IntegerList *lane_iterator = light_change_calculator->lane_ids;
  LightState *folded_light_state = NULL;
  LightStateList *existing_light_state_node = NULL;
  LightChange *light_change = NULL;
  LightChangeList *light_change_iterator = NULL;

  // Update light change calculator with current info
  DestroyLightStateList(light_change_calculator->previous_states);
  light_change_calculator->previous_states =
      CloneLightStateList(light_change_calculator->current_states);
  light_change_calculator->time = time;

  while (lane_iterator != NULL) {

    folded_light_state = FoldLightState(
        GetSignalsByLaneId(signals, lane_iterator->integer));

    if (folded_light_state != NULL) {

      // find the existing data for this lane
      LightStateList *existing_light_state_node = FindLightStateById(light_change_calculator->current_states, lane_iterator->integer);
      if (existing_light_state_node == NULL) {

        light_change_calculator->current_states = AddLightStateToList(
            light_change_calculator->current_states, folded_light_state);

      } else {

        existing_light_state_node->light_state = folded_light_state;
      }

    } else if (light_change_calculator->current_states != NULL) {

      folded_light_state = GetLightStateByLaneId(
          light_change_calculator->current_states, lane_iterator->integer);

      if(folded_light_state != NULL) {

        if (folded_light_state->time_to_change != -DBL_MIN) {

          folded_light_state->time_to_change -= time_elapsed;

          if (folded_light_state->time_to_change <= 0.0) {
            AdvanceLightState(folded_light_state);
          }
        }
      }
    }

    if(folded_light_state != NULL) {
      // find the previous state of this lane, if known
      LightStateList *existing_light_state_node = FindLightStateById(light_change_calculator->previous_states, lane_iterator->integer);

      // if the previous state was known
      if (existing_light_state_node != NULL) {

        // find the light change we're already returning for this change pattern
        light_change_iterator = light_changes;
        while (light_change_iterator != NULL) {

          if (light_change_iterator->light_change->old_color
              == existing_light_state_node->light_state->color
              && light_change_iterator->light_change->new_color
                  == folded_light_state->color) {

            break;
          }

          light_change_iterator = light_change_iterator->next;
        }

        if (light_change_iterator != NULL) {

          light_change_iterator->light_change->lane_ids =
              AddIntegerToList(
                  light_change_iterator->light_change->lane_ids,
                  lane_iterator->integer);

        } else {

          light_change = malloc(sizeof(LightChange));
          light_change->lane_ids = NULL;
          light_change->old_color =
              existing_light_state_node->light_state->color;
          light_change->new_color = folded_light_state->color;
          light_change->lane_ids = AddIntegerToList(
              light_change->lane_ids, lane_iterator->integer);

          light_changes = AddLightChangeToList(light_changes, light_change);
        }
      }
    }

    lane_iterator = lane_iterator->next;
  }

  return light_changes;
}

/**
 * Returns a pointer to a list of light states cloned from the given list.
 *
 * @param list a pointer to the list of light states to clone
 * @return a pointer to the list of light states cloned from the given list
 * (or NULL if no light states were cloned)
 */
static LightStateList *CloneLightStateList(LightStateList *list) {

  LightStateList *clones = NULL;
  LightStateList *light_state_iterator = list;
  LightState *temp = NULL;

  while (light_state_iterator != NULL) {

    temp = malloc(sizeof(LightState));
    temp->color = light_state_iterator->light_state->color;
    temp->lane_id = light_state_iterator->light_state->lane_id;
    temp->time_to_change = light_state_iterator->light_state->time_to_change;

    clones = AddLightStateToList(clones, temp);
    light_state_iterator = light_state_iterator->next;
  }

  return clones;
}

/**
 * Returns the result of a comparison between the given color and the color
 * represented by the given light state. The order of colors is assumed to be
 * green -> yellow -> red for the purpose of making a comparison.
 *
 * @param color the signal color
 * @param light_state a pointer to the light state whose color is compared
 * @return a value of -1 if the given color occurs prior to the given light
 * state color, a value of 0 if the given color occurs at the same time as the
 * given light state color (i.e. they are equal), or a value of 1 if the given
 * color occurs after the given light state color
 */
static int CompareColor(SignalColor color, LightState *light_state) {

  if (color == light_state->color) {

    return 0;

  } else if (color == RED) {

    return 1;

  } else if ((color == YELLOW || color == GREEN)
      && light_state->color == RED) {

    return -1;

  } else if ((color == GREEN)
      && (light_state->color == YELLOW || light_state->color == RED)) {

    return -1;

  } else {

    return 1;
  }
}

/**
 * Returns a pointer to a new light change calculator.
 *
 * @param lane_ids a pointer to a list of lane ID numbers
 * @return a pointer to the new light change calculator
 */
LightChangeCalculator *CreateLightChangeCalculator(IntegerList *lane_ids) {

  LightChangeCalculator *light_change_calculator;
  light_change_calculator = malloc(sizeof(LightChangeCalculator));
  light_change_calculator->lane_ids = lane_ids;
  light_change_calculator->previous_states = NULL;
  light_change_calculator->current_states = NULL;
  light_change_calculator->time = 0.0;

  return light_change_calculator;
}

/**
 * Frees the memory occupied by the given light change calculator.
 *
 * @param light_change_calculator a pointer to the light change calculator to
 * free from memory
 */
void DestroyLightChangeCalculator(
    LightChangeCalculator *light_change_calculator) {

  DestroyLightStateList(light_change_calculator->previous_states);
  DestroyLightStateList(light_change_calculator->current_states);
  DestroyIntegerList(light_change_calculator->lane_ids);
  free(light_change_calculator);
}

/**
 * Search through a light state list to find the position with information about a particular lane.
 *
 * @param light_state_iterator a list of light states to search
 * @param lane_id the lane for which to search
 * @return the node in the light state list that contains information about the lane in question
 */
static LightStateList *FindLightStateById(LightStateList *light_state_iterator,
    int lane_id) {
  while (light_state_iterator != NULL) {

    if (light_state_iterator->light_state->lane_id
        == lane_id) {

      break;
    }

    light_state_iterator = light_state_iterator->next;
  }

  return light_state_iterator;
}

/**
 * Returns a pointer to a light state that is updated from the given light
 * state and signal color.
 *
 * @param color the signal color
 * @param time_to_change the time (s) for the updated light state to change
 * @param light_state the base light state to update
 * @return a pointer to the updated light state
 */
static LightState *FoldColorIntoLightState(
    SignalColor color, double time_to_change, LightState *light_state) {

  LightState *updated_state = light_state;
  int color_cmp = CompareColor(color, updated_state);

  if (color_cmp == 1) {

    return updated_state;

  } else if (color_cmp == -1) {

    updated_state->color = color;
    updated_state->time_to_change = time_to_change;
    return updated_state;

  } else {

    if (updated_state->time_to_change == DBL_MIN
        || updated_state->time_to_change < time_to_change) {

      updated_state->time_to_change = time_to_change;
      updated_state->color = color;
      return updated_state;

    } else {

      return updated_state;
    }
  }
}

/**
 * Returns a pointer to a light state that is derived from the given list of
 * signal indications.
 *
 * @param signals a pointer to the list of signal indications
 * @return a pointer to a light state derived from the given list of signals
 */
static LightState *FoldLightState(SignalIndicationList *signals) {

  LightState *light_state = NULL;
  SignalIndicationList *signal_iterator = signals;

  if (signal_iterator == NULL) {
    return NULL;
  }

  light_state = malloc(sizeof(LightState));
  light_state->lane_id = signal_iterator->signal->lane_id;
  light_state->color = RED;
  light_state->time_to_change = DBL_MIN;

  while (signal_iterator != NULL) {

    light_state = FoldSignalIntoLightState(
        signal_iterator->signal, light_state);

    signal_iterator = signal_iterator->next;
  }

  return light_state;
}

/**
 * Returns a pointer to a light state that is updated from the given light
 * state and the given signal indication.
 *
 * @param signal a pointer to the signal indication
 * @param light_state a pointer to the base light state to update
 * @return a pointer to the updated light state
 */
static LightState *FoldSignalIntoLightState(
    SignalIndication *signal, LightState *light_state) {

  LightState *updated_state = light_state;
  SignalColor color = signal->color;

  if (color == GREEN) {

    updated_state = FoldColorIntoLightState(
        GREEN, signal->time_to_change, updated_state);

  } else if (color == YELLOW) {

    updated_state = FoldColorIntoLightState(
        YELLOW, signal->time_to_change, updated_state);

  } else if (color == RED) {

    updated_state = FoldColorIntoLightState(
        RED, signal->time_to_change, updated_state);
  }

  return updated_state;
}

