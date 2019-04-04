/**
 * The detector sensor program is designed to emulate the information reported
 * by the sensor (i.e. wire loop) of an inductive-loop detector. The
 * information is reported to the controller program, which emulates the
 * electronics control unit that monitors the emulated sensor. The program
 * accepts two command line arguments: one to specify the IP address of the
 * device hosting the controller program, and one to specify the port for
 * controller communication.
 *
 * @author emyers
 */

// system files
#include <signal.h>   // SIGINT, SIGTERM
#include <stdlib.h>   // exiting on program error
#include <unistd.h>   // socket closure

// library files
#include <logging.h>  // log statements
#include <socknet.h>  // socket communication

/**
 * The IP address of the controller device.
 */
static char *ip_;

/**
 * The port for controller communication.
 */
static char *port_;

/**
 * The socket for controller communication.
 */
static int communication_socket_;

/**
 * The vehicle presence status.
 */
static int8_t presence_;

// forward function declarations
static void InitializeReporting();
static void PrintUsage();
static void SendStatusUpdate();
static void Terminate();

/**
 * Initializes reporting vehicle presence updates to the controller.
 */
static void InitializeReporting() {

  presence_ = 0;  // no vehicle presence is sensed

  int input;  // raw input

  while (1) {

    // prompt the user to change the presence status
    if (presence_) {

      Log("Press enter to report an absence: ");

    } else {

      Log("Press enter to report a presence: ");
    }

    do {

      // consume input until the end of the line
      input = getchar();

    } while (input != '\n');

    // change the presence status
    presence_ = !presence_;

    // notify the controller of the status change
    SendStatusUpdate();
  }
}

/**
 * Logs the program usage information.
 */
static void LogUsage() {

  // display the usage information
  Log("usage: detector_sensor ip port\n");
  Log("    ip: address of the controller device\n");
  Log("    port: port for controller communication\n");
}

/**
 * Returns the exit status of the detector sensor program.
 *
 * @param argc the number of command line arguments
 * @param argv a pointer to the array of command line arguments
 * @return the program exit status (0 if no errors have occurred)
 */
int main(int argc, char *argv[]) {

  // handle interrupt and terminate signals
  signal(SIGINT, Terminate);
  signal(SIGTERM, Terminate);

  // initialize logging
  SetLoggingType(CONSOLE_LOGGING);

  // validate the number of command line arguments
  if (argc != 3) {
    Log("command line error: received %d arguments, expected 2\n",
        argc - 1);
    LogUsage();
    exit(0);
  }

  // set the controller device IP address
  ip_ = argv[1];

  // validate the specified port for controller communication
  int input = atoi(argv[2]);
  if (input < 1 || input > 65535) {
    Log("command line error (argument 2): received %d, expected 1-65535\n",
        input);
    LogUsage();
    exit(0);
  }

  // set the validated port
  port_ = argv[2];

  // report vehicle presence updates
  InitializeReporting();
}

/**
 * Sends the current vehicle presence status to the controller.
 */
static void SendStatusUpdate() {

  // connect to the controller device
  if ((communication_socket_ = ConnectToDevice(ip_, port_)) == -1) {
    LogError("connecting to controller device");
    return;
  }

  // send the presence status to the controller
  if (SendByte(communication_socket_, presence_) == -1) {
    LogError("sending vehicle presence status");
  }

  // close the socket for controller communication
  close(communication_socket_);
}

/**
 * Terminates the program.
 */
static void Terminate() {

  // close the potentially open socket
  close(communication_socket_);

  // restore the default signal handling
  signal(SIGINT, SIG_DFL);
  signal(SIGTERM, SIG_DFL);

  // exit the program
  exit(0);
}
