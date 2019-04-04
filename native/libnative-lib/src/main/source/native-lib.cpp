/**
 * *****************************************************************************
 * Copyright (c) 2014 Harmonia Holdings Group LLC and others. All rights
 * reserved. This program and the accompanying materials are made available
 * under the terms of the Harmonia Partner License v1.0 which accompanies this
 * distribution, and is available at http://www.harmonia.com/legal/hpl-v10.html
 *
 * Contributors: Harmonia Holdings Group LLC - initial API and implementation
 ******************************************************************************
 */

/**
 * File:   native-lib.cpp
 * Author: bbadillo
 *
 * Created on September 18, 2014, 1:51 PM
 */

/**
 * MACRO to use for debug output.
 */
#if ENABLE_DEBUG
#define DEBUG_LOG printf
#else
#define DEBUG_LOG(...)
#endif

// NOTE: This define is used to make sure that this compilation unit is built
// as a C++ file even if the compiler does not use this non-standard symbol.
// The detector.h file depends on this.
#define __CPLUSPLUS

// system headers
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>

// c++ headers
#include <iostream>
#include <map>          // mapping detector counts to ids

// library headers
#include <nanomsg/nn.h>           // general purpose nanomsg header
#include <nanomsg/pubsub.h>       // communicating from native-agent to native-lib
#include <nanomsg/pipeline.h>     // communicating from native-lib to native-agent

// project headers
#include "native-lib.h"  // function declarations
#include "fifoqueue.h"  // queues for messages
#include "detector.h"   // detector API

using namespace std;
using namespace etexas;
using namespace google::protobuf;

/**
 * Forward Declarations
 */
void PrintMessages();
void PrintSignalData();
void PrintLaneData();
void PrintDetectorData();
void Start();
void Shutdown();
void PrintBundleWrapperData(BundleWrapper);
void Error(const char *);
void Update();
void SendMessage(WSMIndication *wsm);
char *CheckAndResizeSendBuffer(char *buffer, int &buffer_size, int size_needed);

/**
 * Global Variables
 */
const char ETEXAS_LIB_EXECID[] = "ETEXAS_LIB_EXECID";
const char ETEXAS_LIB_APPID[] = "ETEXAS_LIB_APPID";
const char ETEXAS_LIB_DEVID[] = "ETEXAS_LIB_DEVID";

static Queue *wsmQueue = QueueCreate();

/**
 * File descriptor for subscription of native agent.
 */
int sub_fd;

/**
 * File descriptor for pushing data to pipeline to native agent.
 */
int push_fd;

/**
 * Global variable to hold the execution id.
 */
long exec_id;

/**
 * Global variable to hold the app id.
 */
char* app_id;

/**
 * Global variable to hold the device id.
 */
long dev_id;

/**
 * Pre-allocated object for incoming app data.
 */
BundleWrapper bundle_wrapper;

/**
 * Pre-allocated object for incoming app data (the specific input data).
 */
InputBundle input_bundle;

/**
 * Map of detector counts keyed by detector id.
 */
map<int32, int32> detector_count_map;

/**
 * Pre-allocated object to use for app output.
 */
BundleWrapper app_bundle;
/**
 * Allocated size of array to hold output payload.
 */
int output_array_size = 0;
/**
 * Array to hold output payload.
 */
char *output_array = new char[output_array_size];

/**
 * Library load routine.
 */
__attribute__((constructor)) void init(void) {
  // Hook to the interrupt signal and gracefully shutdown agent.
  signal(SIGINT, exit);

  Start();
}

/**
 * Library unload routine.
 */
__attribute__((destructor)) void fini(void) {
  Shutdown();
}

/**
 * Start contacting the Native Agent.
 */
void Start() {
  GOOGLE_PROTOBUF_VERIFY_VERSION;

  DEBUG_LOG("Native Lib starting up...\n");

  // Create SUBSCRIBE socket to get data from Native Agent.
  sub_fd = nn_socket(AF_SP, NN_SUB);
  if (sub_fd < 0) {
    Error("ERROR on SUBCRIBE socket creation");
  }

  // Create PUSH socket to send data to Native Agent.
  push_fd = nn_socket(AF_SP, NN_PUSH);
  if (push_fd < 0) {
    Error("ERROR on PUSH socket creation");
  }

  // Get the exec id, app id, and dev id to which this process should be tied.
  char* exec_id_str = getenv(ETEXAS_LIB_EXECID);
  if (exec_id_str == NULL) {
    Error(
        "ETEXAS_LIB_EXECID environment variable not found. Please set this and run again. ");
  }
  DEBUG_LOG("Exec Id set to %s\n", exec_id_str);
  exec_id = atol(exec_id_str);

  app_id = getenv(ETEXAS_LIB_APPID);
  if (app_id == NULL) {
    Error(
        "ETEXAS_LIB_APPID environment variable not found. Please set this and run again. ");
  }
  DEBUG_LOG("App Id set to %s\n", app_id);

  char* dev_id_str = getenv(ETEXAS_LIB_DEVID);
  if (dev_id_str == NULL) {
    Error(
        "ETEXAS_LIB_DEVID environment variable not found. Please set this and run again. ");
  }
  DEBUG_LOG("Dev Id set to %s\n", dev_id_str);
  dev_id = atol(dev_id_str);

  // Set the SUBSCRIBE socket to receive only messages for a particular execution id.
  int ret_int = nn_setsockopt(sub_fd, NN_SUB, NN_SUB_SUBSCRIBE, exec_id_str,
      strlen(exec_id_str));
  if (ret_int < 0) {
    Error("ERROR on setting subscribe socket option");
  }

  // Connect to the SUBSCRIBE socket. This will only work if the Native Agent is running.
  ret_int = nn_connect(sub_fd, "ipc:///tmp/pubsub.ipc");
  if (ret_int < 0) {
    Error("ERROR on connecting subscribe socket");
  }

  // Connect to the PUSH socket. This will only work if the Native Agent is running.
  ret_int = nn_connect(push_fd, "ipc:///tmp/pipeline.ipc");
  if (ret_int < 0) {
    Error("ERROR on connecting push socket");
  }

  DEBUG_LOG("Native Lib startup successful.\n");
}

/**
 * Update the internal model with the latest information.
 */
void Update() {
  DEBUG_LOG("Enter Update function.\n");
  char *buffer = NULL;

  // Loop through all queued messages until there are no more available.
  int bytes;
  while ((bytes = nn_recv(sub_fd, &buffer, NN_MSG, NN_DONTWAIT)) >= 0) {

    // The exec id is encoded as a null-terminated string at the front of the message.
    // First, get the length of the string
    // Then, add 1 byte to the size for the null-terminated string length.
    size_t msg_index = strlen(buffer) + 1;

    // Now parse the rest of the message into a protocol buffer and free the original buffer.
    bool parsed = bundle_wrapper.ParseFromArray(&buffer[msg_index],
        bytes - msg_index);
    nn_freemsg(buffer);
    if (!parsed) {
      Error("Message failed to parse.");
    }

    // Print the protocol buffer for test purposes.
    // PrintBundleWrapperData(bundle_wrapper); // Uncomment this to print received data.

    // Switch on the type of message received.
    if (bundle_wrapper.has_inputbundle()) {
      DEBUG_LOG("Bundle wrapper has input bundle\n");
      // If the type of message is correct then update the global data.
      input_bundle = bundle_wrapper.inputbundle();

      // WSMIndication messages = input_bundle.messages();
      for (int i = 0; i < input_bundle.messages_size(); i++) {
        WSMIndication *wsm = new WSMIndication(input_bundle.messages(i).mess());
        QueuePut(wsmQueue, wsm);
      }

      // Gets the DetectorManager to update vehicle counts on detectors.
      if (input_bundle.has_rsedata() && input_bundle.rsedata().detectormap_size() > 0) {
        DetectorManagerData detector_manager_data =
            input_bundle.rsedata().detectormap(0).value();
        for (int i = 0; i < detector_manager_data.detectors_size(); i++) {
          etexas::DetectorManagerData_DetectorData det =
              detector_manager_data.detectors(i);
          if (det.has_detevent() && det.detevent().pulse() > 0) {
            // Increase detector counter, and if none exists yet use initialized value of 0.
            detector_count_map[det.detectorid()] += det.detevent().pulse();
          }
        }
      }

    } else if (bundle_wrapper.has_shutdownbundle()) {
      // If the type of message is a shutdown, then exit the program.
      exit(0);
    }
  }
  DEBUG_LOG("Exit Update function.\n");
}

/**
 * Send a message to the associated agent.
 *
 * @param wsm The message to send.
 */
void SendMessage(WSMIndication *wsm) {
  DEBUG_LOG("Enter SendMessage function.\n");

  // Clear anything in the output model object.
  app_bundle.Clear();

  // Set the execution id and add output.
  app_bundle.set_execid(exec_id);
  OutputBundle* app_output = app_bundle.add_outputbundle();

  // Fill up the output model object with data from the app.
  app_output->set_appid(app_id);
  app_output->set_devid(dev_id);
  app_output->set_x(0); // Dummy value (required)
  app_output->set_y(0); // Dummy value (required)
  app_output->set_z(0); // Dummy value (required)
  WSMIndication* new_wsm = app_output->add_messages();

  // Using deep-copy assignment by dereferencing both pointers.
  *new_wsm = *wsm;

  // Make sure the array buffer is large enough to hold the data.
  int output_size = app_bundle.ByteSize();
  output_array = CheckAndResizeSendBuffer(output_array, output_array_size,
      output_size);

  // Write the protocol buffer data into the array buffer.
  DEBUG_LOG("Message: %s", app_bundle.DebugString().c_str());
  bool success = app_bundle.SerializeToArray(output_array, output_size);
  if (!success) {
    Error("ERROR on serializing input data");
  }

  // Send the data to the associated agent.
  int ret_int = nn_send(push_fd, output_array, output_size, 0);
  if (ret_int < 0) {
    Error("ERROR on sending input data");
  }
  DEBUG_LOG("Exit SendMessage function.\n");
}

/**
 * Shutdown all open connections and cleanup memory usage.
 */
void Shutdown() {
  DEBUG_LOG("Native Lib shutting down...\n");

  // Clean up resources.
  nn_shutdown(sub_fd, 0);
  nn_shutdown(push_fd, 0);

  google::protobuf::ShutdownProtobufLibrary();

  DEBUG_LOG("Native Lib shutdown successful.\n");
}

/**
 * Get the time in the simulation.
 *
 * @return The simulation time in seconds.
 */
double GetSimTime() {
  if (input_bundle.IsInitialized()) {
    return input_bundle.simtime();
  }
  return 0.0;
}

/**
 * Convenience function to print messages.
 */
void PrintMessages() {
  if (input_bundle.IsInitialized()) {
    for (int i = 0; i < input_bundle.messages_size(); i++) {
      WSMIndication wsm = input_bundle.messages(i).mess();
      cout << wsm.DebugString() << endl;
    }
  }
}

/**
 * Convenience function to print signal data.
 */
void PrintSignalData() {
  if (input_bundle.IsInitialized()) {
    cout << input_bundle.rsedata().signalmap(0).DebugString() << endl;
  }
}

/**
 * Convenience function to print lane data.
 */
void PrintLaneData() {
  if (input_bundle.IsInitialized()) {
    cout << input_bundle.rsedata().lanemap(0).DebugString() << endl;
  }
}

/**
 * Convenience function to print detector data.
 */
void PrintDetectorData() {
  if (input_bundle.IsInitialized()) {
    cout << input_bundle.rsedata().detectormap(0).DebugString() << endl;
  }
}

/**
 * Print the protocol buffer message to standard out.
 *
 * @param bundle_wrapper Protocol buffer message.
 */
void PrintBundleWrapperData(BundleWrapper bundle_wrapper) {
  cout << bundle_wrapper.DebugString() << endl;
}

/**
 * Get the current list of received WSM messages
 *
 * @return the running list of received WSM messages
 */
Queue *GetWsmQueue() {
  return wsmQueue;
}

/*
 * This method creates a native detector list with the detector information from the agent
 */
DetectorList *GetCurrentDetectorList() {

  if (input_bundle.IsInitialized()) {

    //Gets the DetectorManager and the intersectionId to fill the native detector list
    if (input_bundle.has_rsedata() && input_bundle.rsedata().detectormap_size() > 0 && input_bundle.rsedata().lanemap_size() > 0) {

      DetectorManagerData detector_manager_data = input_bundle.rsedata().detectormap(0).value();

      int intersectionId = input_bundle.rsedata().lanemap(0).value().intersectionid();

      DetectorList *detector_list = NULL;

      // loops through every detector and adds it to the list
      for (int i = 0; i < detector_manager_data.detectors_size(); i++) {
        Detector *new_det = (Detector *) malloc(sizeof(Detector));
        etexas::DetectorManagerData_DetectorData det =
            detector_manager_data.detectors(i);
        new_det->intersection_id = intersectionId;

        // Gets the average point between all of the given points (center)
        int xtotal = 0;
        int ytotal = 0;
        for (int j = 0; j < det.area().xpoints_size(); j++) {
          xtotal += det.area().xpoints(j);
        }
        for (int j = 0; j < det.area().ypoints_size(); j++) {
          ytotal += det.area().ypoints(j);
        }
        new_det->x = xtotal / det.area().xpoints_size();
        new_det->y = ytotal / det.area().ypoints_size();

        new_det->id = det.detectorid();

        // currently should only get one lane
        new_det->lane_id = det.laneids(0);

        // get detector pulse count and divide by 2 to get vehicle count (pulse is vehicle on and off detector)
        // also, round down to get counts only when vehicles completely pass detectors
        new_det->count = detector_count_map[det.detectorid()] >> 1;

        if(det.has_detevent()) {
          new_det->presence = det.detevent().presence();
        } else {
          new_det->presence = 0;
        }

        // Makes a new detector for each lane
        for (int j = 0; j < det.laneids_size(); j++) {
          new_det->lane_id = det.laneids(j);

          // Adds the detector to the list
          detector_list = AddDetectorToList(detector_list, new_det);
        }
      }
      return detector_list;
    }
  }
  return NULL;
}

/**
 * Make sure that the byte buffer for sending messages is large enough.
 * If not allocate a buffer of the size needed.
 *
 * @param buffer Pointer to the buffer to check and resize.
 * @param buffer_size Current size of the buffer. Will be set to the new size if buffer is resized.
 * @param size_needed The size that is needed for the buffer.
 * @return The checked (and possibly resized) buffer.
 */
char *CheckAndResizeSendBuffer(char *buffer, int &buffer_size,
    int size_needed) {
  if (size_needed > buffer_size) {
    delete[] buffer;
    buffer_size = size_needed;
    return new char[size_needed];
  } else {
    return buffer;
  }
}

/**
 * This function gets the latitude and longitude of the obu/rse
 *
 * @param lat the variable that will hold the latitude of the obu/rse if found
 * @param lon the variable that will hold the longtitude of the obu/rse if found
 * @return true if a latitude and longitude pair are found
 */
bool GetLatitudeAndLongitude(double &lat, double &lon) {
  if (input_bundle.IsInitialized()) {
    // loops through the rse devices until the correct one is found or it has no more rse devices to check
    for (int i = 0; i < input_bundle.rses_size(); i++) {
      if (dev_id == input_bundle.rses(i).devid()) {
        lat = input_bundle.rses(i).latitude();
        lon = input_bundle.rses(i).longitude();
        return true;
      }
    }
    // same as the loop above except for obu devices instead
    for (int i = 0; i < input_bundle.obus_size(); i++) {
      if (dev_id == input_bundle.obus(i).devid()) {
        lat = input_bundle.obus(i).vehicle().latitude();
        lon = input_bundle.obus(i).vehicle().longitude();
        return true;
      }
    }
  }
  return false;
}

/**
 * Stop the program and print the error message.
 *
 * @param msg A message to print to standard error.
 */
void Error(const char *msg) {
  perror(msg);
  exit(1);
}
