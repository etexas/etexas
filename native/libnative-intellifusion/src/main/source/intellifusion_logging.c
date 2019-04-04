/**
 * Implements the functions required for logging in the native IntelliFusion
 * application.
 *
 * @author emyers
 */

// library files
#include <logging.h>      // log statements
#include <VehicleType.h>  // vehicle types

// project files
#include "intellifusion_data_model.h"  // model data structures
#include "intellifusion_logging.h"     // function implementations

// forward function declarations
static void LogLightChangeLanes(IntegerList *lanes, int indentations);

/**
 * Logs information for the given Detector.
 *
 * @param detector a pointer to the detoctor to log
 * @param indentations the number of indentations to use
 */
void LogDetector(Detector *detector, int indentations) {

  char *indent = GetIndentationString(indentations);

  Log("%s<detector intersection=%d id=%d lane=%d count=%d x=%d y=%d "
      "presence=%d/>\n", indent, detector->intersection_id, detector->id,
      detector->lane_id, detector->count, detector->x, detector->y,
      detector->presence);
}

/**
 * Logs information for each detector in the given list.
 *
 * @param list a pointer to the list of detectors to log
 * @param indentations the number of indentations to use
 */
void LogDetectorList(DetectorList *list, int indentations) {

  char *indent = GetIndentationString(indentations);
  Log("%s<detectors>\n", indent);
  DetectorList* detectors = list;

  while (detectors) {
    LogDetector(&(detectors->detector), indentations + 1);
    detectors = detectors->next;
  }

  Log("%s</detectors>\n", indent);
}

/**
 * Logs information for the given information model.
 *
 * @param model a pointer to the information model to log
 * @param indentations the number of indentations to use
 */
void LogInfoModel(InfoModel *model, int indentations) {

  char *indent = GetIndentationString(indentations);
  Log("%s<info_model>\n", indent);
  LogLaneManager(model->lane_manager, indentations + 1);
  LogSignalIndicationList(model->signals, indentations + 1);
  LogVehicleList(model->vehicles, indentations + 1);
  Log("%s</info_model>\n", indent);
}

/**
 * Logs information for the given lane.
 *
 * @param lane a pointer to the lane to log
 * @param indentations the number of indentations to use
 */
void LogLane(Lane *lane, int indentations) {

  char *type_string;

  if (lane->type == INBOUND) {

    type_string = "inbound";

  } else if (lane->type == OUTBOUND) {

    type_string = "outbound";

  } else {

    type_string = "unknown";
  }

  char *indent = GetIndentationString(indentations);

  Log("%s<lane id=%d type=%s>\n", indent, lane->id, type_string);
  LogLaneNodes(lane, indentations + 1);
  LogLaneMovements(lane, indentations + 1);
  Log("%s</lane>\n", indent);
}

/**
 * Logs lane information for the given lane manager.
 *
 * @param lane_manager a pointer to the lane manager containing the lanes to
 * log
 * @param indentations the number of indentations to use
 */
void LogLanes(LaneManager *lane_manager, int indentations) {

  char *indent = GetIndentationString(indentations);
  Log("%s<lanes>\n", indent);

  int i;
  for (i = 0; i < lane_manager->lane_count; i++) {
    LogLane(&(lane_manager->lanes[i]), indentations + 1);
  }

  Log("%s</lanes>\n", indent);
}

/**
 * Logs information for the given lane manager.
 *
 * @param lane_manager a pointer to the lane manager to log
 * @param indentations the number of indentations to use
 */
void LogLaneManager(LaneManager *lane_manager, int indentations) {

  char *indent = GetIndentationString(indentations);

  Log("%s<lane_manager intersection=%d latitude=%.2f longitude=%.2f "
      "elevation=%.2f>\n", indent, lane_manager->intersection_id,
      lane_manager->latitude, lane_manager->longitude,
      lane_manager->elevation);

  LogLanes(lane_manager, indentations + 1);
  Log("%s</lane_manager>\n", indent);
}

/**
 * Logs information for the given lane movement.
 *
 * @param movement a pointer to the lane movement to log
 * @param indentations the number of indentations to use
 */
void LogLaneMovement(LaneMovementType *movement, int indentations) {

  char *movement_string;
  char *indent = GetIndentationString(indentations);

  if (*movement == LEFT_TURN) {

    movement_string = "left";

  } else if (*movement == RIGHT_TURN) {

    movement_string = "right";

  } else if (*movement == STRAIGHT) {

    movement_string = "straight";

  } else if (*movement == NO_TURN_ON_RED) {

    movement_string = "no_turn_on_red";

  } else {

    movement_string = "unknown";
  }

  Log("%s<movement type=%s/>\n", indent, movement_string);
}

/**
 * Logs movement information for the given lane.
 *
 * @param lane a pointer to the lane containing the movements to log
 * @param indentations the number of indentations to use
 */
void LogLaneMovements(Lane *lane, int indentations) {

  char *indent = GetIndentationString(indentations);

  Log("%s<movements>\n", indent);

  int i;
  for (i = 0; i < lane->movement_count; i++) {
    LogLaneMovement(&(lane->movements[i]), indentations + 1);
  }

  Log("%s</movements>\n", indent);
}

/**
 * Logs information for the given lane node.
 *
 * @param node a pointer to the lane node to log
 * @param indentations the number of indentations to use
 */
void LogLaneNode(LaneNode *node, int indentations) {

  char *indent = GetIndentationString(indentations);
  Log("%s<node x=%.2f y=%.2f z=%.2f width=%.2f/>\n", indent, node->x, node->y,
      node->z, node->width);
}

/**
 * Logs node information for the given lane.
 *
 * @param lane a pointer to the lane containing the nodes to log
 * @param indentations the number of indentations to use
 */
void LogLaneNodes(Lane *lane, int indentations) {

  char *indent = GetIndentationString(indentations);
  Log("%s<nodes>\n", indent);

  int i;
  for (i = 0; i < lane->node_count; i++) {
    LogLaneNode(&(lane->nodes[i]), indentations + 1);
  }

  Log("%s</nodes>\n", indent);
}

/**
 * Logs information for the given light change event.
 *
 * @param light_change a pointer to the light change event to log
 * @param indentations the number of indentations to use
 */
void LogLightChange(LightChange *light_change, int indentations) {

  char *old_color_string;

  if (light_change->old_color == GREEN) {

    old_color_string = "green";

  } else if (light_change->old_color == YELLOW) {

    old_color_string = "yellow";

  } else if (light_change->old_color == RED) {

    old_color_string = "red";

  } else {

    old_color_string = "n/a";
  }

  char *new_color_string;

  if (light_change->new_color == GREEN) {

    new_color_string = "green";

  } else if (light_change->new_color == YELLOW) {

    new_color_string = "yellow";

  } else if (light_change->new_color == RED) {

    new_color_string = "red";

  } else {

    new_color_string = "n/a";
  }

  char *indent = GetIndentationString(indentations);
  Log("%s<light_change old_color=%s new_color=%s>\n", indent,
      old_color_string, new_color_string);

  LogLightChangeLanes(light_change->lane_ids, indentations + 1);
  Log("%s</light_change>\n", indent);
}

/**
 * Logs information for the given list of light change lanes.
 *
 * @param lanes a pointer to the list of light change lanes to log
 * @param indentations the number of indentations to use
 */
static void LogLightChangeLanes(IntegerList *lanes, int indentations) {

  char *indent = GetIndentationString(indentations);
  IntegerList *list_iterator = lanes;

  while (list_iterator != NULL) {

    Log("%s<lane id=%d/>\n", indent, list_iterator->integer);
    list_iterator = list_iterator->next;
  }
}

/**
 * Logs information for the given list of light change events.
 *
 * @param list a pointer to the list of light change events to log
 * @param indentations the number of indentations to use
 */
void LogLightChangeList(LightChangeList *list, int indentations) {

  char *indent = GetIndentationString(indentations);
  Log("%s<light_changes>\n", indent);

  LightChangeList *list_iterator = list;

  while (list_iterator != NULL) {

    LogLightChange(list_iterator->light_change, indentations + 1);
    list_iterator = list_iterator->next;
  }

  Log("%s</light_changes>\n", indent);
}

/**
 * Logs information for the given signal indication.
 *
 * @param signal a pointer to the signal indication to log
 * @param indentations the number of indentations to use
 */
void LogSignalIndication(SignalIndication *signal, int indentations) {

  char *color_string;

  if (signal->color == GREEN) {

    color_string = "green";

  } else if (signal->color == RED) {

    color_string = "red";

  } else if (signal->color == YELLOW) {

    color_string = "yellow";

  } else if (signal->color == NONE) {

    color_string = "n/a";

  } else {

    color_string = "unknown";
  }

  char *type_string;

  if (signal->type == BALL) {

    type_string = "ball";

  } else if (signal->type == LEFT_ARROW) {

    type_string = "left";

  } else if (signal->type == RIGHT_ARROW) {

    type_string = "right";

  } else if (signal->type == STRAIGHT_ARROW) {

    type_string = "straight";

  } else if (signal->type == UTURN_ARROW) {

    type_string = "uturn";

  } else if (signal->type == STOP_SIGN) {

    type_string = "stop";

  } else if (signal->type == YIELD_SIGN) {

    type_string = "yield";

  } else if (signal->type == UNCONTROLLED) {

    type_string = "uncontrolled";

  } else {

    type_string = "unknown";
  }

  char *state_string;

  if (signal->state == STEADY) {

    state_string = "steady";

  } else if (signal->state == FLASHING) {

    state_string = "flashing";

  } else if (signal->state == SOFT) {

    state_string = "soft";

  } else {

    state_string = "unknown";
  }

  char *indent = GetIndentationString(indentations);
  Log("%s<signal intersection=%d lane=%d color=%s type=%s state=%s "
      "change=%.2f/>\n", indent, signal->intersection_id, signal->lane_id,
      color_string, type_string, state_string, signal->time_to_change);
}

/**
 * Logs information for the given signal indication list.
 *
 * @param list a pointer to the signal indication list to log
 * @param indentations the number of indentations to use
 */
void LogSignalIndicationList(SignalIndicationList *list, int indentations) {

  char *indent = GetIndentationString(indentations);
  Log("%s<signals>\n", indent);

  SignalIndicationList *list_iterator = list;
  while (list_iterator != NULL) {

    LogSignalIndication(list_iterator->signal, indentations + 1);
    list_iterator = list_iterator->next;
  }

  Log("%s</signals>\n", indent);
}

/**
 * Logs information for the given vehicle.
 *
 * @param vehicle a pointer to the vehicle to log
 * @param indentations the number of indentations to use
 */
void LogVehicle(Vehicle *vehicle, int indentations) {

  char *type_string;

  if (vehicle->type == VehicleType_car) {

    type_string = "car";

  } else if (vehicle->type == VehicleType_bus) {

    type_string = "bus";

  } else if (vehicle->type == VehicleType_axleCnt5Trailer) {

    type_string = "tractor_trailer";

  } else {

    type_string = "unknown";
  }

  char *indent = GetIndentationString(indentations);
  Log("%s<vehicle id=%d lane=%d length=%.2f width=%.2f height=%.2f "
      "type=%s>\n", indent, vehicle->id, vehicle->lane_id, vehicle->length,
      vehicle->width, vehicle->height, type_string);

  char *indent2 = GetIndentationString(indentations + 1);
  Log("%s<coordinates x=%.2f y=%.2f z=%.2f/>\n", indent2, vehicle->x,
      vehicle->y, vehicle->z);
  Log("%s<trajectory speed=%.2f heading=%.2f/>\n", indent2, vehicle->speed,
      vehicle->heading);
  Log("%s</vehicle>\n", indent);
}

/**
 *
 * Logs information for each vehicle in the given list.
 *
 * @param list a pointer to the list of vehicles to log
 * @param indentations the number of indentations to use
 */
void LogVehicleList(VehicleList *list, int indentations) {

  char *indent = GetIndentationString(indentations);
  Log("%s<vehicles>\n", indent);

  VehicleList *list_iterator = list;
  while (list_iterator != NULL) {

    LogVehicle(list_iterator->vehicle, indentations + 1);
    list_iterator = list_iterator->next;
  }

  Log("%s</vehicles>\n", indent);
}
