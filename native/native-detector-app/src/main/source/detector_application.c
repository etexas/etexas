/**
 * The detector application program is designed as a proof of concept
 * implementation for native programs that request detector information using
 * the arada detector API. The program accepts one command line argument that
 * specifies the logging type.
 *
 * @author emyers
 */

// system files
#include <stdlib.h>  // exiting on program error

// library files
#include <detector.h>  // detector structures and functions
#include <logging.h>   // log statements

/**
 * The received detectors.
 */
static DetectorList *detector_list_;

// forward function declarations
static void LogUsage();

/**
 * Logs the program usage information.
 */
static void LogUsage() {

  Log("usage: detector_application logging_type\n");
  Log("    logging_type: <0-console> <1-file>\n");
}

/**
 * Returns the exit status of the detector application program.
 *
 * @param argc the number of command line arguments
 * @param argv a pointer to the array of command line arguments
 * @return the program exit status (0 if no errors have occurred)
 */
int main(int argc, char *argv[]) {

  // validate the number of command line arguments
  if (argc != 2) {
    Log("command line error: received %d arguments, expected 1\n", argc - 1);
    LogUsage();
    exit(0);
  }

  // validate the logging type
  int input = atoi(argv[1]);
  if (input < 0 || input > 1) {
    Log("command line error (argument 1): received %d, expected 0-1\n", input);
    LogUsage();
    exit(0);
  }

  // initialize logging
  SetLogPath("/var/detector_application.log");
  SetLoggingType(input);
  if (input == FILE_LOGGING) {
    ResetLogging();
  }

  Detector *detector;
  DetectorList *detector_iterator;

  while (1) {

    // receive the detector information from the controller
    detector_list_ = GetDetectorList();
    detector_iterator = detector_list_;

    while (detector_iterator != NULL) {

      // log the detector information
      detector = &(detector_iterator->detector);
      Log("Detector %d in lane %d (%d, %d) has counted %d vehicles and %s "
          "detecting a vehicle\n", detector->id, detector->lane_id,
          detector->x, detector->y, detector->count,
          detector->presence ? "is" : "is not");

      detector_iterator = detector_iterator->next;
    }

    sleep(1);  // pause before the next information request
  }
}
