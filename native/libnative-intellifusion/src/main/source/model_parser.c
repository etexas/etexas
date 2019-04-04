/**
 * Defines the function to implement an algorithm that parses an information
 * model of an intersection from the available DSRC messages and detector
 * information for a given point in time.
 *
 * @author emyers
 */

// library files
#include <detector.h>  // detectors
#include <stdbool.h>   // standard boolean type
#include <logging.h>   // log statements

// project files
#include "intellifusion_config.h"      // get intersection ID
#include "intellifusion_data_model.h"  // model data structures
#include "light_change_calculator.h"   // light change calculator
#include "message_parser.h"            // message parsing
#include "model_parser.h"              // function implementation
#include "movement_calculator.h"       // car following
#include "vehicle_calculator.h"        // vehicles from detectors

/**
 * The light change calculator to determine light states.
 */
static LightChangeCalculator *light_change_calculator_ = NULL;

/**
 * The lane manager for the intersection.
 */
static LaneManager *lane_manager_ = NULL;

/**
 * The list of DSRC vehicles from the previous time step.
 */
static VehicleList *previous_dsrc_vehicles_ = NULL;

/**
 * The list of vehicles without DSRC capabilities.
 */
static VehicleList *std_vehs_ = NULL;

/**
 * Returns a pointer to an information model of an intersection derived from
 * the given DSRC messages and detector information for a given point in time.
 *
 * @param time the current time (s)
 * @param message_count the number of DSRC messages received
 * @param messages a pointer to an array of DSRC messages
 * @param message_lengths a pointer to an array of message lengths (bytes)
 * @param detectors a pointer to a list of detectors
 * @return a pointer to the parsed information model
 */
InfoModel *ParseModel(
    double time, int message_count, char **messages, int *message_lengths,
    DetectorList *detectors) {

  InfoModel *model = (InfoModel *) malloc(sizeof(InfoModel));

  int i;  // loop iterator

  if (lane_manager_ == NULL) {

    for (i = 0; i < message_count; i++) {

      LaneManagerList *lane_managers =
          (LaneManagerList *) ParseMapData(messages[i], message_lengths[i]);

      while (lane_managers != NULL) {

        if (lane_managers->lane_manager->intersection_id
            == GetIntersectionId()) {

          lane_manager_ = lane_managers->lane_manager;
          lane_managers = NULL;

        } else {

          lane_managers = DestroyLaneManagerListNode(lane_managers);
        }
      }

      if (lane_manager_ != NULL) {

        Log("***************************************************\n");
        Log("Information model has the following lane manager...\n");
        Log("***************************************************\n");
        LogLaneManager(lane_manager_, 0);

        break;
      }
    }
  }

  if (lane_manager_ == NULL) {

    return NULL;
  }

  SignalIndicationList *signals = NULL;

  bool spat_not_found = true;
  i = 0;
  while (spat_not_found && i < message_count) {

    SignalIndicationList *sigs =
        (SignalIndicationList *) ParseSpat(messages[i], message_lengths[i]);

    while (sigs != NULL) {

      SignalIndicationList *current_signal_node = sigs;
      sigs = current_signal_node->next;

      // if the current signal is for the intersection we need then
      // transfer the node to the main signal list, otherwise delete it
      if (current_signal_node->signal->intersection_id == GetIntersectionId()) {

        current_signal_node->next = signals;
        signals = current_signal_node;
        spat_not_found = false;

      } else {

        free(current_signal_node->signal);
        free(current_signal_node);
      }
    }

    i++;
  }

  if (light_change_calculator_ == NULL) {

    IntegerList *lane_ids = NULL;

    for (i = 0; i < lane_manager_->lane_count; i++) {

      if (INBOUND == lane_manager_->lanes[i].type) {

        lane_ids = AddIntegerToList(lane_ids, lane_manager_->lanes[i].id);
      }
    }

    light_change_calculator_ = CreateLightChangeCalculator(lane_ids);
  }

  LightChangeList *light_changes =
      CalculateLightChanges(light_change_calculator_, signals, time);

  DestroySignalIndicationList(signals);

  model->signals = GetCurrentSignalStates(
      light_change_calculator_->current_states);

  Log("*********************************************************\n");
  Log("Information model has the following signal indications...\n");
  Log("*********************************************************\n");
  LogSignalIndicationList(model->signals, 0);

  Vehicle *v = NULL;                         // parsed vehicle
  VehicleList *dsrc_vehs = NULL;             // DSRC vehicle list
  VehicleList *dropped = NULL;               // dropped DSRC vehicles
  VehicleList *current_dsrc_vehicles = NULL; // current DSRC vehicle list

  // for each received message
  for (i = 0; i < message_count; i++) {

    // if a vehicle could be parsed from the message (i.e. BSM)
    v = ParseBsm(messages[i], message_lengths[i], lane_manager_,
        previous_dsrc_vehicles_);

    if (v) {

      /*
       * NOTE: this works because messages are placed at the front of the
       * message buffer as they are received. This means that messages that
       * are found early in the list are more recent than those found later in
       * the list. As such, we can simply ignore future BSMs broadcast by a
       * vehicle that has already been parsed.
       */

      // if the vehicle should be dropped
      if (DropDsrcVehicle(v, lane_manager_)) {

        // add it to the list of dropped DSRC vehicles
        VehicleList *tmp = (VehicleList *) malloc(sizeof(VehicleList));
        tmp->vehicle = v;
        tmp->next = dropped;
        dropped = tmp;

      } else if (!(VehicleInList(v->id, dsrc_vehs)
          || VehicleInList(v->id, dropped))) {

        // add the vehicle to the list of parsed vehicles
        VehicleList *tmp = (VehicleList *) malloc(sizeof(VehicleList));
        tmp->vehicle = v;
        tmp->next = dsrc_vehs;
        dsrc_vehs = tmp;

        // add the vehicle to the list of previous dsrc vehicles
        VehicleList *curr_tmp = (VehicleList *) malloc(sizeof(VehicleList));
        curr_tmp->vehicle = CopyVehicle(v);
        curr_tmp->next = current_dsrc_vehicles;
        current_dsrc_vehicles = curr_tmp;
      }
    }
  }

  DestroyVehicleList(dropped);
  DestroyVehicleList(previous_dsrc_vehicles_);
  previous_dsrc_vehicles_ = current_dsrc_vehicles;

  // calculate the list of detected vehicles
  VehicleList *det_vehs = NULL;
  if (detectors) {
    det_vehs = (VehicleList *) CalculateVehiclesFromDetectors(
        dsrc_vehs, detectors, lane_manager_);
  }

  // add detected vehicles to the standard vehicles
  if (det_vehs) {

    if (std_vehs_) {

      VehicleList *v_iterator = std_vehs_;
      while (v_iterator->next) {
        v_iterator = v_iterator->next;
      }

      v_iterator->next = det_vehs;

    } else {

      std_vehs_ = det_vehs;
    }
  }

  std_vehs_ = (VehicleList *) CalculateMovements(
      time, std_vehs_, dsrc_vehs, lane_manager_,
      light_change_calculator_->current_states);

  Log("********************************************************\n");
  Log("Information model has the following standard vehicles...\n");
  Log("********************************************************\n");
  LogVehicleList(std_vehs_, 0);

  Log("****************************************************\n");
  Log("Information model has the following DSRC vehicles...\n");
  Log("****************************************************\n");
  LogVehicleList(dsrc_vehs, 0);
  /*
   * NOTE: a copy of the parsed vehicle list is used in the returned
   * information model to prevent side effects if the list is freed from
   * memory elsewhere in the program.
   */

  // copy the standard vehicles
  VehicleList *vehicles_copy = dsrc_vehs;
  VehicleList *v_iterator = std_vehs_;
  while (v_iterator) {

    VehicleList *temp = (VehicleList *) malloc(sizeof(VehicleList));
    temp->vehicle = CopyVehicle(v_iterator->vehicle);
    temp->next = vehicles_copy;
    vehicles_copy = temp;

    v_iterator = v_iterator->next;
  }

  model->vehicles = vehicles_copy;

  // TODO: bbadillo - Change this to make a copy of the lane manager to
  // protect the model parser from external changes. Then external programs
  // will be responsible to free it.
  model->lane_manager = lane_manager_;

  return model;
}
