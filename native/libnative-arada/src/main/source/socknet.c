/**
 * Implements functions to facilitate network communication using TCP socket
 * streams among Arada applications.
 *
 * @author emyers
 */

// system files
#include <netdb.h>   // socket address structures
#include <string.h>  // memset
#include <unistd.h>  // socket closure

// library files
#include <logging.h>  // log statements

// project files
#include "socknet.h"  // function implementations

/**
 * Returns the socket file descriptor after binding to the host device.
 * Binding is a prerequisite for all applications that accept incoming
 * connections from other applications (i.e. servers).
 *
 * @param port a pointer to the port number for communication
 * @return the socket file descriptor (or -1 if an error occurred)
 */
int BindToHost(char *port) {

  // set the host address information parameters
  struct addrinfo params;
  memset(&params, 0, sizeof(params));
  params.ai_family = AF_UNSPEC;
  params.ai_socktype = SOCK_STREAM;
  params.ai_flags = AI_PASSIVE;

  // get the host address information
  int result;
  struct addrinfo *host_info;
  if ((result = getaddrinfo(NULL, port, &params, &host_info)) != 0) {
    Log("requesting host address information: %s\n", gai_strerror(result));
    return -1;
  }

  int confirm = 1;
  int socket_file = -1;
  struct addrinfo *temp = host_info;

  // bind to the port at the first available host address
  while (socket_file == -1 && temp != NULL) {

    socket_file = socket(
        temp->ai_family, temp->ai_socktype, temp->ai_protocol);

    if (socket_file == -1) {

      temp = temp->ai_next;

    } else if (setsockopt(
        socket_file, SOL_SOCKET, SO_REUSEADDR, &confirm, sizeof(int)) == -1) {

      LogError("setting socket options");
      return -1;

    } else if (bind(socket_file, temp->ai_addr, temp->ai_addrlen) == -1) {

      close(socket_file);
      socket_file = -1;
      temp = temp->ai_next;
    }
  }

  // verify the binding
  freeaddrinfo(host_info);
  if (temp == NULL) {
    LogError("binding to host");
    return -1;
  }

  return socket_file;  // return the socket file descriptor
}

/**
 * Returns the socket file descriptor after connecting to the device at the
 * given IP address. Connecting is a prerequisite for all applications that
 * initiate communication (i.e. clients).
 *
 * @param ip a pointer to the device IP address
 * @param port a pointer to the port number for communication
 * @return the socket file descriptor (or -1 if an error occurred)
 */
int ConnectToDevice(char *ip, char *port) {

  // set the device address information parameters
  struct addrinfo params;
  memset(&params, 0, sizeof(params));
  params.ai_family = AF_UNSPEC;
  params.ai_socktype = SOCK_STREAM;
  params.ai_flags = AI_PASSIVE;

  // get the device address information
  int result;
  struct addrinfo *device_info;
  if ((result = getaddrinfo(ip, port, &params, &device_info)) != 0) {
    Log("requesting device address information: %s\n", gai_strerror(result));
    return -1;
  }

  int socket_file = -1;
  struct addrinfo *temp = device_info;

  // connect to the port at the first available device address
  while (socket_file == -1 && temp != NULL) {

    socket_file = socket(
        temp->ai_family, temp->ai_socktype, temp->ai_protocol);

    if (socket_file == -1) {

      temp = temp->ai_next;

    } else if (connect(socket_file, temp->ai_addr, temp->ai_addrlen) == -1) {

      close(socket_file);
      socket_file = -1;
      temp = temp->ai_next;
    }
  }

  // verify the connection
  freeaddrinfo(device_info);
  if (temp == NULL) {
    LogError("connecting to device");
    return -1;
  }

  return socket_file;  // return the socket file descriptor
}

/**
 * Returns the socket file descriptor after connecting to the host device. The
 * function is a convenience function for calls to
 *
 * <code>ConnectToDevice(char *ip, char *port)</code>
 *
 * with <code>localhost</code> as the given IP address.
 *
 * @param port a pointer to the port number for communication
 * @return the socket file descriptor (or -1 if an error occurred)
 */
int ConnectToHost(char *port) {

  return ConnectToDevice("localhost", port);
}

/**
 * Returns the number of bytes received after attempting to receive one byte
 * of data using the given socket file descriptor.
 *
 * @param socket_file the socket file descriptor
 * @param value a pointer to the storage location for the received byte
 * @return the actual number of bytes received
 */
int ReceiveByte(int socket_file, int8_t *value) {

  return recv(socket_file, value, sizeof(int8_t), 0);
}

/**
 * Returns the number of bytes received after attempting to receive four bytes
 * of data using the given socket file descriptor.
 *
 * @param socket_file the socket file descriptor
 * @param value a pointer to the storage location for the received bytes
 * @return the actual number of bytes received
 */
int ReceiveLong(int socket_file, int32_t *value) {

  int32_t message;
  int bytes_received = recv(socket_file, &message, sizeof(int32_t), 0);
  *value = ntohl(message);
  return bytes_received;
}

/**
 * Returns the number of bytes received after attempting to receive two bytes
 * of data using the given socket file descriptor.
 *
 * @param socket_file the socket file descriptor
 * @param value a pointer to the storage location for the received bytes
 * @param the actual number of bytes received
 */
int ReceiveShort(int socket_file, int16_t *value) {

  int16_t message;
  int bytes_received = recv(socket_file, &message, sizeof(int16_t), 0);
  *value = ntohs(message);
  return bytes_received;
}

/**
 * Returns the number of bytes sent after attempting to send one byte of data
 * using the given socket file descriptor.
 *
 * @param socket_file the socket file descriptor
 * @param value the value to send
 * @return the actual number of bytes sent
 */
int SendByte(int socket_file, int8_t value) {

  return send(socket_file, &value, sizeof(int8_t), 0);
}

/**
 * Returns the number of bytes sent after attempting to send four bytes of
 * data using the given socket file descriptor.
 *
 * @param socket_file the socket file descriptor
 * @param value the value to send
 * @return the actual number of bytes sent
 */
int SendLong(int socket_file, int32_t value) {

  int32_t message = htonl(value);
  return send(socket_file, &message, sizeof(int32_t), 0);
}

/**
 * Returns the number of bytes sent after attempting to send two bytes of data
 * using the given socket file descriptor.
 *
 * @param socket_file the socket file descriptor
 * @param value the value to send
 * @return the actual number of bytes sent
 */
int SendShort(int socket_file, int16_t value) {

  int16_t message = htons(value);
  return send(socket_file, &message, sizeof(int16_t), 0);
}
