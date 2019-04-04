/**
 * Implements the functions to set and retrieve MOE properties that are
 * required for accurate calculations in a variety of contexts.
 *
 * @author emyers
 */

// project files
#include "moe_config.h"  // function implementation

/**
 * The speed (m/s) threshold for queued vehicles.
 */
static double queue_speed_ = 2.24;

/**
 * The PSID to listen for BSMs (0x0020).
 */
static int bsm_psid_ = 32;

/**
 * The PSID to listen for MapData messages (0xBFF0).
 */
static int map_psid_ = 49136;

/**
 * The PSID to listen for SPaT messages (0xBFE0).
 */
static int spat_psid_ = 49120;

/**
 * Returns the PSID to listen for BSMs.
 *
 * @return the PSID to listen for BSMs
 */
int GetBsmPsid() {

  return bsm_psid_;
}

/**
 * Sets the PSID to listen for BSMs.
 *
 * @param bsm_psid the PSID to listen for BSMs
 */
void SetBsmPsid(int bsm_psid) {

  bsm_psid_ = bsm_psid;
}

/**
 * Returns the PSID to listen for MapData messages.
 *
 * @return the PSID to listen for MapData messages
 */
int GetMapPsid() {

  return map_psid_;
}

/**
 * Sets the PSID to listen for MapData messages.
 *
 * @param map_psid the PSID to listen for MapData messages
 */
void SetMapPsid(int map_psid) {

  map_psid_ = map_psid;
}

/**
 * Returns the speed (m/s) threshold for queued vehicles.
 *
 * @return the speed (m/s) threshold for queue vehicles
 */
double GetQueueSpeed() {

  return queue_speed_;
}

/**
 * Sets the speed (m/s) threshold for queued vehicles.
 *
 * @param queue_speed the speed (m/s) threshold for queued vehicles
 */
void SetQueueSpeed(double queue_speed) {

  queue_speed_ = queue_speed;
}

/**
 * Returns the PSID to listen for SPaT messages.
 *
 * @return the PSID to listen for SPaT messages
 */
int GetSpatPsid() {

  return spat_psid_;
}

/**
 * Sets the PSID to listen for SPaT messages.
 *
 * @param spat_psid the PSID to listen for SPaT messages
 */
void SetSpatPsid(int spat_psid) {

  spat_psid_ = spat_psid;
}
