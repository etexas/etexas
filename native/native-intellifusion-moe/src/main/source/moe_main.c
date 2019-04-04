/**
 * Implements structures and functions to drive execution of the IntelliFusion
 * MOE program. The program uses SPaT, map, and BSM messages along with
 * detector data to track the state of traffic over time for a given
 * intersection. MOEs such as queue lengths and signal phase failures are
 * calculated for analysis regarding the flow of traffic through the
 * intersection.
 *
 * @author emyers
 * @author ttevendale
 */

// system files
#include <limits.h>  // INT_MAX
#include <signal.h>  // SIG_INT, SIG_TERM
#include <stdlib.h>  // exiting on error
#include <time.h>    // system time request

// library files
#include <detector.h>                  // detectors and detector functions
#include <intellifusion_config.h>      // configuration properties
#include <intellifusion_data_model.h>  // model data structures
#include <intellifusion_logging.h>     // data structure log statements
#include <logging.h>                   // log statements
#include <model_parser.h>              // parsing information model
#include <wave.h>                      // WAVE device interface / DSRC

// project files
#include "moe_calculator.h"  // MOE calculations
#include "moe_config.h"      // MOE configuration properties

// forward type declarations
typedef struct MessageBuffer MessageBuffer;

/**
 * A buffer to create a linked list of messages.
 */
struct MessageBuffer {

  /**
   * The current message node in the list.
   */
  char *message;

  /**
   * The current message length.
   */
  int length;

  /**
   * The next message node in the list.
   */
  struct MessageBuffer *next;

};

static int message_count_;		               // number of buffered messages
static int pid_;				                     // process ID
static WMEApplicationRequest map_request_;   // MapData registration request
static WMEApplicationRequest bsm_request_;   // BSM registration request
static WMEApplicationRequest spat_request_;  // SPaT registration request

// forward function declarations
static MessageBuffer* AddMessageToBuffer(
    WSMIndication *rxpkt, MessageBuffer *buffer);
static void ClearMessageBuffer(MessageBuffer *buffer);
static double GetTimeLocal(GPSData* data);
static void LogConfiguration();
static void PrintUsage();
static void ProcessArguments(int argc, char *argv[]);
static void ReceiveMessages();
static void RegisterUser(WMEApplicationRequest *request);
static void Terminate();

/**
 * Adds the message from the given packet to the given message buffer. The
 * message will be added to the front of the linked list of messages that the
 * message buffer represents.
 *
 * @param rxpkt a pointer to the received WSM packet
 * @param buffer a pointer to the message buffer
 */
static MessageBuffer* AddMessageToBuffer(
    WSMIndication *rxpkt, MessageBuffer *buffer) {

  MessageBuffer *temp = (MessageBuffer *) malloc(sizeof(MessageBuffer));
  temp->message = (char *) malloc(sizeof(char) * rxpkt->data.length);
  temp->length = rxpkt->data.length;

  temp->message = (char *) memcpy(temp->message, rxpkt->data.contents,
      rxpkt->data.length);

  temp->next = buffer;
  return temp;
}

/**
 * Frees the memory occupied by the given message buffer.
 *
 * @param buffer a pointer to the message buffer
 */
static void ClearMessageBuffer(MessageBuffer *buffer) {
  while (buffer != NULL) {
    MessageBuffer *tmp = buffer->next;
    free(buffer->message);
    free(buffer);
    buffer = tmp;
  }
}

/**
 * Returns the current epoch time (seconds since 1970).
 *
 * @param data a pointer to store the GPS data
 * @return the current epoch time (seconds since 1970)
 */
static double GetTimeLocal(GPSData* data) {

  return time(NULL);
}

/**
 * Logs the values set from the command line arguments.
 */
static void LogConfiguration() {

  Log("**********************************************\n");
  Log("Native IntelliFusion configuration settings...\n");
  Log("**********************************************\n");
  Log("MapData Message PSID: %d\n", GetMapPsid());
  Log("BSM PSID: %d\n", GetBsmPsid());
  Log("SPaT Message PSID: %d\n", GetSpatPsid());
  Log("Intersection ID: %d\n", GetIntersectionId());
  Log("Squared BSM Radius: %.2f cm\n", GetSquaredBsmRadius());
  Log("Spacing at Capacity: %.2f cm\n", GetCapacitySpacing());
  Log("Spacing at Jam Density: %.2f cm\n", GetJamSpacing());
  Log("Following Speed: %.2f m/s\n", GetFollowingSpeed());
  Log("Injection Speed: %.2f m/s\n", GetInjectedVehicleSpeed());
  Log("Queue Speed: %.2f m/s\n", GetQueueSpeed());
  Log("Vehicle Length: %.2f cm\n", GetInjectedVehicleLength());
  Log("Lane Assignment: %d\n", GetLaneAssignmentId());
}

/**
 * Logs the program usage information. The program accepts 16 mandatory
 * arguments: the request type, the immediate SCH access flag, the intervals
 * for extended SCH access, the channel, the PSID for MapData messages, the
 * PSID for BSMs, the PSID for SPaT messages, the intersection ID, the radius
 * for BSM filtering, the spacing at capacity, the spacing at jam density, the
 * car following speed, the queue speed, the default vehicle length, the
 * logging type, and the default lane assignment for DSRC vehicles. The first
 * seven arguments specify the conditions for registration with the WAVE
 * device, while the remaining nine are for IntelliFusion configuration.
 */
static void LogUsage() {

  Log("usage: IntelliFusion request_type imm_access ext_access channel "
      "map_psid bsm_psid spat_psid intersection bsm_radius cap_spacing "
      "jam_spacing follow_speed queue_speed length logging_type "
      "lane_assignment\n");
  Log("    request_type: <1-auto> <2-unconditional> <3-none>\n");
  Log("    imm_access: <0-wait for access> <1-immediate access>\n");
  Log("    ext_access: <0-alternate> <1-continuous>\n");
  Log("    channel: [172-184]\n");
  Log("    map_psid: <0-default 49136> (0-%d]\n", INT_MAX);
  Log("    bsm_psid: <0-default 32> (0-%d]\n", INT_MAX);
  Log("    spat_psid: <0-default 49120> (0-%d]\n", INT_MAX);
  Log("    intersection: [0-%d]\n", INT_MAX);
  Log("    bsm_radius: <0-default 16,764> (0-%d] cm\n", INT_MAX);
  Log("    cap_spacing: <0-default 1,652> (0-%d] cm\n", INT_MAX);
  Log("    jam_spacing: <0-default 826> (0-%d] cm\n", INT_MAX);
  Log("    follow_speed: <0-default 13.83> (0-%d] m/s\n", INT_MAX);
  Log("    queue_speed: <0-default 2.24> (0-%d] m/s\n", INT_MAX);
  Log("    length: <0-default 497.84> [250-%d] cm\n", INT_MAX);
  Log("    logging_type: <0-console> <1-file>\n");
  Log("    lane_assignment: <0-default 0> (0-%d]\n", INT_MAX);
}

/**
 * Drives execution of the IntelliFusion MOE program. The method connects to
 * the local WAVE device, registers as a user application, and begins looping
 * to receive incoming messages. Received messages are parsed once every
 * second to update the state of the intersection, which is used to update the
 * calculated MOEs.
 *
 * @param arg the number of command line arguments
 * @param argv a pointer to the array of command line arguments
 * @return the program exit status (0 if no unexpected errors occurred)
 */
int main(int argc, char *argv[]) {

  // handle interrupt and terminate signals
  signal(SIGINT, (void *) Terminate);
  signal(SIGTERM, (void *) Terminate);

  // reset the registration requests
  memset(&map_request_, 0, sizeof(WMEApplicationRequest));
  memset(&bsm_request_, 0, sizeof(WMEApplicationRequest));
  memset(&spat_request_, 0, sizeof(WMEApplicationRequest));

  // process the command line arguments
  ProcessArguments(argc, argv);

  // initialize logging
  int logging_type = atoi(argv[15]);
  SetLogPath("/tmp/usb/intellifusion.log");
  SetLoggingType(logging_type);
  if (logging_type == FILE_LOGGING) {
    ResetLogging();
  }

  // log the configuration
  LogConfiguration();

  // invoke the wave device
  if (invokeWAVEDevice(WAVEDEVICE_LOCAL, 0) < 0) {
    LogError("invoking wave device");
    exit(1);
  }

  // register users for each data message
  pid_ = getpid();
  RegisterUser(&map_request_);
  RegisterUser(&bsm_request_);
  RegisterUser(&spat_request_);

  // receive DSRC messages
  ReceiveMessages();
}

/**
 * Validates the command line arguments and sets the corresponding values.
 *
 * @param argc the number of command line arguments
 * @param argv a pointer to the array of command line arguments
 */
static void ProcessArguments(int argc, char *argv[]) {

  // validate the number of command line arguments
  if (argc != 17) {
    Log("command line error: received %d arguments, expected 16 arguments\n",
        argc - 1);
    LogUsage();
    exit(0);
  }

  // process the application request type
  int int_value = atoi(argv[1]);
  map_request_.userreqtype = int_value;
  bsm_request_.userreqtype = int_value;
  spat_request_.userreqtype = int_value;
  if (int_value < 1 || int_value > 3) {
    Log("command line error (argument 1): received %d, expected [1-3]\n",
        int_value);
    LogUsage();
    exit(0);
  }

  // process the immediate SCH access flag
  map_request_.schaccess = int_value;
  bsm_request_.schaccess = int_value;
  spat_request_.schaccess = int_value;
  int_value = atoi(argv[2]);
  if (int_value < 0 || int_value > 1) {
    Log("command line error (argument 2): received %d, expected [0-1]\n",
        int_value);
    LogUsage();
    exit(0);
  }

  // process the extended SCH access intervals
  int_value = atoi(argv[3]);
  map_request_.schextaccess = int_value;
  bsm_request_.schextaccess = int_value;
  spat_request_.schextaccess = int_value;
  if (int_value < 0 || int_value > 1) {
    Log("command line error (argument 3): received %d, expected [0-1]\n",
        int_value);
    LogUsage();
    exit(0);
  }

  // process the channel
  int_value = atoi(argv[4]);
  map_request_.channel = int_value;
  bsm_request_.channel = int_value;
  spat_request_.channel = int_value;
  if (int_value < 172 || int_value > 184) {
    Log("command line error (argument 4): received %d, expected [172-184]\n",
        int_value);
    LogUsage();
    exit(0);
  }

  // process the MapData PSID
  int_value = atoi(argv[5]);
  int_value = int_value ? int_value : GetMapPsid();
  SetMapPsid(int_value);
  map_request_.psid = GetMapPsid();
  if (int_value < 0 || int_value > INT_MAX) {
    Log("command line error (argument 5): received %d, expected (0-%d]\n",
        int_value, INT_MAX);
    LogUsage();
    exit(0);
  }

  // process the BSM PSID
  int_value = atoi(argv[6]);
  int_value = int_value ? int_value : GetBsmPsid();
  SetBsmPsid(int_value);
  bsm_request_.psid = GetBsmPsid();
  if (int_value < 0 || int_value > INT_MAX) {
    Log("command line error (argument 6): received %d, expected (0-%d]\n",
        int_value, INT_MAX);
    LogUsage();
    exit(0);
  }

  // process the SPaT PSID
  int_value = atoi(argv[7]);
  int_value = int_value ? int_value : GetSpatPsid();
  SetSpatPsid(int_value);
  spat_request_.psid = GetSpatPsid();
  if (int_value < 0 || int_value > INT_MAX) {
    Log("command line error (argument 7): received %d, expected (0-%d]\n",
        int_value, INT_MAX);
    LogUsage();
    exit(0);
  }

  // process the intersection ID
  int_value = atoi(argv[8]);
  SetIntersectionId(int_value);
  if (int_value < 0) {
    Log("command line error (argument 8): received %d, expected [0-%d]\n",
        int_value, INT_MAX);
    LogUsage();
    exit(0);
  }

  // process the BSM radius
  double dec_value = atof(argv[9]);
  dec_value = dec_value ? dec_value * dec_value : GetSquaredBsmRadius();
  SetSquaredBsmRadius(dec_value);
  if (dec_value < 0 || dec_value > INT_MAX) {
    Log("command line error (argument 9): received %f, expected (0-%d]\n",
        dec_value, INT_MAX);
    LogUsage();
    exit(0);
  }

  // process the spacing at capacity
  dec_value = atof(argv[10]);
  dec_value = dec_value ? dec_value : GetCapacitySpacing();
  SetCapacitySpacing(dec_value);
  if (dec_value < 0 || dec_value > INT_MAX) {
    Log("command line error (argument 10): received %f, expected (0-%d]\n",
        dec_value, INT_MAX);
    LogUsage();
    exit(0);
  }

  // process the spacing at jam density
  dec_value = atof(argv[11]);
  dec_value = dec_value ? dec_value : GetJamSpacing();
  SetJamSpacing(dec_value);
  if (dec_value < 0 || dec_value > INT_MAX) {
    Log("command line error (argument 11): received %f, expected (0-%d]\n",
        dec_value, INT_MAX);
    LogUsage();
    exit(0);
  }

  // process the car following speed
  dec_value = atof(argv[12]);
  dec_value = dec_value ? dec_value : GetFollowingSpeed();
  SetFollowingSpeed(dec_value);
  SetInjectedVehicleSpeed(dec_value);
  if (dec_value < 0 || dec_value > INT_MAX) {
    Log("command line error (argument 12): received %f, expected (0-%d]\n",
        dec_value, INT_MAX);
    LogUsage();
    exit(0);
  }

  // process the queue speed
  dec_value = atof(argv[13]);
  dec_value = dec_value ? dec_value : GetQueueSpeed();
  SetQueueSpeed(dec_value);
  if (dec_value < 0 || dec_value > INT_MAX) {
    Log("command line error (argument 13): received %f, expected (0-%d]\n",
        dec_value, INT_MAX);
    LogUsage();
    exit(0);
  }

  // process the vehicle length
  dec_value = atof(argv[14]);
  dec_value = dec_value ? dec_value : GetInjectedVehicleLength();
  SetInjectedVehicleLength(dec_value);
  if (dec_value < 250 || dec_value > INT_MAX) {
    Log("command line error (argument 14): received %f, expected [250-%d]\n",
        dec_value, INT_MAX);
    LogUsage();
    exit(0);
  }

  // process the logging type
  int_value = atoi(argv[15]);
  SetLoggingType(int_value);
  if (int_value < 0 || int_value > 1) {
    Log("command line error (argument 15): received %d, expected [0-1]\n",
        int_value);
    LogUsage();
    exit(0);
  }

  // process the lane assignment parameter
  int_value = atoi(argv[16]);
  int_value = int_value ? int_value : GetLaneAssignmentId();
  SetLaneAssignmentId(int_value);
  if (int_value < 0 || int_value > INT_MAX) {
    Log("command line error (argument 16): received %d, expected (0-%d]\n",
        int_value, INT_MAX);
    LogUsage();
    exit(0);
  }
}

/**
 * Receives WSMs from broadcasting devices. Messages are received in a
 * continual loop, updating the intersection information model and microscopic
 * model driver every second.
 */
static void ReceiveMessages() {

  GPSData data;
  double elapsed_time;
  double current_time;
  double start_time = GetTimeLocal(&data);
  double previous_time = start_time;

  InfoModel *model = NULL;  // intersection information model

  // microscopic model driver
  MoeCalculator *moe_calculator = CreateMoeCalculator();

  WSMIndication rxpkt;		          // received packet
  MessageBuffer *buffer = NULL;     // message buffer
  DetectorList *detectors = NULL;   // list of detectors
  DetectorList *detector_iterator;  // detector iterator
  Detector *detector;               // current detector

  message_count_ = 0;  // initialize the message count

  while (1) {

    // drain received messages to the buffer
    while (rxWSMPacket(pid_, &rxpkt) > 0) {
      buffer = AddMessageToBuffer(&rxpkt, buffer);
      message_count_++;
    }

    current_time = GetTimeLocal(&data);
    elapsed_time = current_time - previous_time;

    // if at least one second has passed
    if (elapsed_time >= 1.0) {

      // get the detector information from the controller
      detectors = GetDetectorList();
      Log("************************************************\n");
      Log("Information model has the following detectors...\n");
      Log("************************************************\n");
      LogDetectorList(detectors, 0);

      // create arrays for the messages and message lengths
      char **messages = (char **) malloc(sizeof(char *) * message_count_);
      int *lengths = (int *) malloc(sizeof(int) * message_count_);

      // build the arrays from the message buffer
      int index = 0;
      MessageBuffer *tmp = buffer;
      while (tmp != NULL) {
        messages[index] = tmp->message;
        lengths[index] = tmp->length;
        tmp = tmp->next;
        index++;
      }

      // update the microscopic model
      model = ParseModel(current_time - start_time, message_count_, messages,
          lengths, detectors);

      if (model) {

        CalculateMoes(
            moe_calculator, model->vehicles, model->signals,
            model->lane_manager, current_time - start_time);

        DestroyInfoModel(model);

      } else {

        Log("***************************************************\n");
        Log("No information model could be parsed this iteration\n");
        Log("***************************************************\n");
      }

      // free the messages from memory
      free(lengths);
      free(messages);
      ClearMessageBuffer(buffer);

      // free the detectors from memory
      DestroyDetectorList(detectors);

      // reset for the next iteration
      message_count_ = 0;
      buffer = NULL;
      previous_time = current_time;
    }
  }
}

/**
 * Registers a user with the given application request.
 *
 * @param request a pointer to the application request
 */
static void RegisterUser(WMEApplicationRequest *request) {

  // attempt to register the user request
  if (registerUser(pid_, request) < 0) {

    // remove the user and try again if registration failed
    removeUser(pid_, request);
    if (registerUser(pid_, request) < 0) {
      LogError("registering application");
      exit(1);
    }
  }
}

/**
 * Terminates the program.
 */
static void Terminate() {

  // clear the user registrations
  removeUser(pid_, &map_request_);
  removeUser(pid_, &bsm_request_);
  removeUser(pid_, &spat_request_);

  // restore the default signal handling
  signal(SIGINT, SIG_DFL);
  signal(SIGTERM, SIG_DFL);

  // exit the program
  exit(0);
}
