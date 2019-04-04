#include "wave.h"
#include "native-functions.h"

////////////////////////////////////////////////////////////////////////////////
// Global Variables
////////////////////////////////////////////////////////////////////////////////
static int wave_device_type = -1;
static int wave_device_blockflag = -1;
static int gpscsockfd = -1;

/**
 * Sets the mode of the WAVE Device.
 *
 * @param type Either WAVEDEVICE_REMOTE or WAVEDEVICE_LOCAL.
 * @param blockflag If local type, then whether or not to block on read/write
 * operations.
 * @return A return code (non-zero is bad).
 */
int invokeWAVEDevice(int type, int blockflag) {
  if (type != WAVEDEVICE_REMOTE && type != WAVEDEVICE_LOCAL) {
    return 1;
  }

  wave_device_type = type;
  wave_device_blockflag = blockflag;

  return 0;
}

int setWMEApplRegNotifParams(WMEApplicationRequest *req) {
  return 0;
}

int registerProvider(int pid, WMEApplicationRequest *appreq) {
  return 0;
}

int Get_Available_Serviceinfo(int pid, struct availserviceInfo *get_astinfo,
    uint32_t integer) {
  return 0;
}

int removeProvider(int pid, WMEApplicationRequest *appreq) {
  return 0;
}

int removeAll(void) {
  return 0;
}

int transmitTA(WMETARequest *tareq) {
  return 0;
}

int wsa_config(WMEWSAConfig *wsaconfig) {
  return 0;
}

int registerUser(int pid, WMEApplicationRequest *appreq) {
  return 0;
}

int removeUser(int pid, WMEApplicationRequest *appreq) {
  return 0;
}

int startWBSS(int pid, WMEApplicationRequest *req) {
  return 0;
}

int stopWBSS(int pid, WMEApplicationRequest *req) {
  return 0;
}

int makeUnavailableWBSS(int pid, WMEApplicationRequest *req) {
  return 0;
}

int getWRSSReport(int pid, WMEWRSSRequest *req) {
  return 0;
}

u_int64_t getTsfTimer(int pid) {
  return 0;
}

u_int64_t setTsfTimer(uint64_t tsf) {
  return 0;
}

/**
 * Transmits a WSMP packet.
 *
 * @param pid The identifier for the calling process.
 * @param req The packet.
 * @return A return code (-1 is bad).
 */
int txWSMPacket(int pid, WSMRequest *req) {
  return lib_txWSMPacket(pid, req, wave_device_type, wave_device_blockflag);
}

/**
 * Receives a WSMP packet.
 *
 * @param pid The identifier for the calling process.
 * @param ind The packet.
 * @return A return code (0 is bad).
 */
int rxWSMPacket(int pid, WSMIndication *ind) {
  return lib_rxWSMPacket(pid, ind, wave_device_type, wave_device_blockflag);
}

int __txWSMPacket(int pid, void *txbuf, int size) {
  return 0;
}

int __rxWSMPacket(int pid, void *rxbuf) {
  return 0;
}

int cancelTX(int pid, WMECancelTxRequest *req) {
  return 0;
}

void getUSTIpv6Addr(struct in6_addr *ustAddr, char *ifName) {
}

/**
 * Get the current GPS data.
 *
 * @param gpsdat The GPS data structure in which to put current data.
 * @param gpsadd IP address of the server providing GPS information.
 */
void get_gps_status(GPSData *gpsdat, char *gpsadd) {

  lib_get_gps_status(gpsdat, gpsadd);
}

/**
 * Creates a socket to GPS data and returns the file descriptor of the socket. The
 * file descriptor is also stored in the global variable named gpscsockfd.
 *
 * @param ip IP address of the server providing GPS information.
 * @return The file descriptor of the newly created socket to GPS data.
 */
int gpsc_connect(char *ip) {
  if (gpscsockfd >= 0)
  {
    return -1;
  }
  return (gpscsockfd = 1);
}

/**
 * Terminates the connection to a GPS socket according to a file descriptor stored
 * in the global variable named gpscsockfd.
 *
 * @return -1 if a connection was never opened.
 */
int gpsc_close_sock() {
  int ret_val = gpscsockfd;
  gpscsockfd = -1;
  return ret_val;
}
