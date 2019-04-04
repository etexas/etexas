/**
 * The detector controller program is designed to emulate the electronics
 * control unit of an inductive-loop detector. The program receives vehicle
 * presence updates from a connected sensor, and published the information (in
 * a text file) for interested programs. The program accepts seven command
 * line arguments: one to specify the logging type, one to specify the path
 * where the detector information should be written, four to specify the
 * details of the controlled detector (ID, lane, x coordinate, y coordinate),
 * and one to specify the port for communication with the detector sensor.
 *
 * @author emyers
 */

// system files
#include <netdb.h>   // socket structures
#include <signal.h>  // SIGINT, SIGTERM
#include <stdio.h>   // file print statements
#include <stdlib.h>  // atoi function, exit on program error
#include <unistd.h>  // socket file closure

// library files
#include <detector.h>  // detector structures and functions
#include <logging.h>   // log statements
#include <socknet.h>   // socket communication

/**
 * The detector information file path.
 */
static char *detector_path_;

/**
 * The detector to report.
 */
static Detector *detector_;

/**
 * The detector information file.
 */
static FILE *detector_file_;

/**
 * The socket for sensor communication.
 */
static int communication_socket_;

/**
 * The socket for the sensor connection.
 */
static int connection_socket_;

// forward function declarations
static void InitializeDetector(char *argv[]);
static void InitializeListening(char *port);
static void LogUsage();
static void Terminate();
static void ThrowCommandLineError();
static void WriteDetectorInformation();

/**
 * Initializes the reported detector.
 *
 * @param argv a pointer to the array of command line arguments
 */
static void InitializeDetector(char *argv[]) {

  // validate the specified ID
  int input = atoi(argv[3]);
  if (input < 0) {
    ThrowCommandLineError(3, input, "a positive integer");
  }

  // set the validated ID
  detector_ = malloc(sizeof(Detector));
  detector_->id = input;

  // validate the specified lane
  input = atoi(argv[4]);
  if (input < 0) {
    ThrowCommandLineError(4, input, "a positive integer");
  }

  // set the validated lane
  detector_->lane_id = input;

  // set the x and y coordinates
  detector_->x = atoi(argv[5]);
  detector_->y = atoi(argv[6]);
}

/**
 * Initializes listening for sensor status updates.
 *
 * @param port a pointer to the port for sensor connectivity
 */
static void InitializeListening(char *port) {

  // bind to the host device
  if ((connection_socket_ = BindToHost(port)) == -1) {
    LogError("binding to the host device");
    exit(1);
  }

  // listen for the sensor to connect
  if (listen(connection_socket_, 0) == -1) {
    LogError("listening for the sensor connection");
    close(connection_socket_);
    exit(1);
  }

  Log("listening for sensor status updates on port %s...\n", port);

  int8_t presence;
  socklen_t sin_size;
  struct sockaddr_storage sensor_address;

  while (1) {

    // accept connections from the sensor
    sin_size = sizeof(sensor_address);
    if ((communication_socket_ = accept(
        connection_socket_, (struct sockaddr *) &sensor_address,
        &sin_size)) == -1) {
      LogError("accepting the sensor connection");
      continue;
    }

    // receive the detector presence status
    if (ReceiveByte(communication_socket_, &presence) == -1) {
      LogError("receiving sensor presence status");
      close(communication_socket_);
      continue;
    }

    // write any change to the detector presence status
    if (detector_->presence != presence) {

      if (!presence) {
        detector_->count++;
      }

      detector_->presence = presence;
      WriteDetectorInformation();
    }
  }
}

/**
 * Logs the program usage information.
 */
static void LogUsage() {

  Log("usage: detector_controller logging_type detector_path detector_id "
      "detector_lane detector_x detector_y detector_port\n");
  Log("    logging_type: <0-console> <1-file>\n");
  Log("    detector_path: the file path to write detector information\n");
  Log("    detector_id: the ID of the detector\n");
  Log("    detector_lane: the lane ID where the detector is deployed\n");
  Log("    detector_x: the x coordinate of the detector\n");
  Log("    detector_y: the y coordinate of the detector\n");
  Log("    detector_port: the port to receive vehicle presence updates\n");
}

/**
 * Returns the exit status of the detector controller program.
 *
 * @param argc the number of command line arguments
 * @param argv a pointer to the array of command line arguments
 * @return the program exit status (0 if no errors have occurred)
 */
int main(int argc, char *argv[]) {

  // handle interrupt and terminate signals
  signal(SIGINT, Terminate);
  signal(SIGTERM, Terminate);

  // validate the number of command line arguments
  if (argc != 8) {
    Log("command line error: received %d arguments, expected 7\n", argc - 1);
    LogUsage();
    exit(0);
  }

  // validate the specified logging type
  int input = atoi(argv[1]);
  if (input < 0 || input > 1) {
    ThrowCommandLineError(1, input, "0-1");
  }

  // initialize logging
  SetLogPath("/var/detector_controller.log");
  SetLoggingType(input);
  if (input == FILE_LOGGING) {
    ResetLogging();
  }

  // initialize the detector to report
  detector_path_ = argv[2];
  InitializeDetector(argv);

  // validate the specified port for detector updates
  input = atoi(argv[7]);
  if (input < 1 || input > 65535) {
    ThrowCommandLineError(7, input, "1-65535");
  }

  // write the starting detector information
  WriteDetectorInformation();

  // listen for sensor status updates
  InitializeListening(argv[7]);
}

/**
 * Terminates the program.
 */
static void Terminate() {

  // close any potentially open sockets
  close(communication_socket_);
  close(connection_socket_);

  // restore the default signal handling
  signal(SIGINT, SIG_DFL);
  signal(SIGTERM, SIG_DFL);

  // exit the program
  exit(0);
}

/**
 * Logs a command line error and exits the program.
 *
 * @param argument the argument number where the error was found
 * @param value the value provided for the argument
 * @param expected the expected value for the argument
 */
static void ThrowCommandLineError(int argument, int value, char *expected) {

  Log("command line error (argument %d): received %d, expected %s\n",
      argument, value, expected);
  LogUsage();
  exit(0);
}

/**
 * Writes the updated detector information.
 */
static void WriteDetectorInformation() {

  if ((detector_file_ = fopen(detector_path_, "w")) == NULL) {
    LogError("opening detector information file");

  } else if (fprintf(detector_file_, "id=%d\n", detector_->id) < 0) {
    LogError("writing detector id information");

  } else if (fprintf(detector_file_, "lane=%d\n", detector_->lane_id) < 0) {
    LogError("writing detector lane information");

  } else if (fprintf(detector_file_, "x=%d\n", detector_->x) < 0) {
    LogError("writing detector x coordinate information");

  } else if (fprintf(detector_file_, "y=%d\n", detector_->y) < 0) {
    LogError("writing detector y coordinate information");

  } else if (fprintf(detector_file_, "presence=%d\n", detector_->presence)
      < 0) {
    LogError("writing detector presence information");

  } else if (fprintf(detector_file_, "count=%d\n", detector_->count) < 0) {
    LogError("writing detector count information");
  }

  fclose(detector_file_);
}
