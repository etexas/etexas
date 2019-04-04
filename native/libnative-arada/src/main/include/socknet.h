/**
 * Defines the functions to facilitate network communication using TCP socket
 * streams among Arada applications.
 *
 * @author emyers
 */

/**
 * Returns the socket file descriptor after binding to the host device.
 * Binding is a prerequisite for all applications that accept incoming
 * connections from other applications (i.e. servers).
 *
 * @param port a pointer to the port number for communication
 * @return the socket file descriptor (or -1 if an error occurred)
 */
int BindToHost(char *port);

/**
 * Returns the socket file descriptor after connecting to the device at the
 * given IP address. Connecting is a prerequisite for all applications that
 * initiate communication (i.e. clients).
 *
 * @param ip a pointer to the device IP address
 * @param port a pointer to the port number for communication
 * @return the socket file descriptor (or -1 if an error occurred)
 */
int ConnectToDevice(char *ip, char *port);

/**
 * Returns the socket file descriptor after connecting to the host device.
 *
 * @param port a pointer to the port number for communication
 * @return the socket file descriptor (or -1 if an error occurred)
 */
int ConnectToHost(char *port);

/**
 * Returns the number of bytes received after attempting to receive one byte
 * of data using the given socket file descriptor.
 *
 * @param socket_file the socket file descriptor
 * @param value a pointer to the storage location for the received byte
 * @return the actual number of bytes received
 */
int ReceiveByte(int socket_file, int8_t *value);

/**
 * Returns the number of bytes received after attempting to receive four bytes
 * of data using the given socket file descriptor.
 *
 * @param socket_file the socket file descriptor
 * @param value a pointer to the storage location for the received bytes
 * @return the actual number of bytes received
 */
int ReceiveLong(int socket_file, int32_t *value);

/**
 * Returns the number of bytes received after attempting to receive two bytes
 * of data using the given socket file descriptor.
 *
 * @param socket_file the socket file descriptor
 * @param value a pointer to the storage location for the received bytes
 * @param the actual number of bytes received
 */
int ReceiveShort(int socket_file, int16_t *value);

/**
 * Returns the number of bytes sent after attempting to send one byte of data
 * using the given socket file descriptor.
 *
 * @param socket_file the socket file descriptor
 * @param value the value to send
 * @return the actual number of bytes sent
 */
int SendByte(int socket_file, int8_t value);

/**
 * Returns the number of bytes sent after attempting to send four bytes of
 * data using the given socket file descriptor.
 *
 * @param socket_file the socket file descriptor
 * @param value the value to send
 * @return the actual number of bytes sent
 */
int SendLong(int socket_file, int32_t value);

/**
 * Returns the number of bytes sent after attempting to send two bytes of data
 * using the given socket file descriptor.
 *
 * @param socket_file the socket file descriptor
 * @param value the value to send
 * @return the actual number of bytes sent
 */
int SendShort(int socket_file, int16_t value);
