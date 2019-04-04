/**
 * Defines the functions to set and retrieve MOE properties that are required
 * for accurate calculations in a variety of contexts.
 *
 * @author emyers
 */

#ifndef ETEXAS_INTELLIFUSION_MOE_CONFIG
#define ETEXAS_INTELLIFUSION_MOE_CONFIG

/**
 * Returns the PSID to listen for BSMs.
 *
 * @return the PSID to listen for BSMs
 */
int GetBsmPsid();

/**
 * Sets the PSID to listen for BSMs.
 *
 * @param bsm_psid the PSID to listen for BSMs
 */
void SetBsmPsid(int bsm_psid);

/**
 * Returns the PSID to listen for MapData messages.
 *
 * @return the PSID to listen for MapData messages
 */
int GetMapPsid();

/**
 * Sets the PSID to listen for MapData messages.
 *
 * @param map_psid the PSID to listen for MapData messages
 */
void SetMapPsid(int map_psid);

/**
 * Returns the speed (m/s) threshold for queued vehicles.
 *
 * @return the speed (m/s) threshold for queue vehicles
 */
double GetQueueSpeed();

/**
 * Sets the speed (m/s) threshold for queued vehicles.
 *
 * @param queue_speed the speed (m/s) threshold for queued vehicles
 */
void SetQueueSpeed(double queue_speed);

/**
 * Returns the PSID to listen for SPaT messages.
 *
 * @return the PSID to listen for SPaT messages
 */
int GetSpatPsid();

/**
 * Sets the PSID to listen for SPaT messages.
 *
 * @param spat_psid the PSID to listen for SPaT messages
 */
void SetSpatPsid(int spat_psid);

#endif
