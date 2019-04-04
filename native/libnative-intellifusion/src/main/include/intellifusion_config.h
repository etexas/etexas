/**
 * Defines the functions to set and retrieve intersection properties that are
 * required for accurate calculations in a variety of contexts.
 *
 * @author emyers
 */

#ifndef ETEXAS_INTELLIFUSION_CONFIG
#define ETEXAS_INTELLIFUSION_CONFIG

/**
 * Returns the spacing (cm) between vehicles at capacity.
 *
 * @return the spacing (cm) between vehicles at capacity
 */
double GetCapacitySpacing();

/**
 * Sets the spacing (cm) between vehicles at capacity.
 *
 * @param capacity_spacing the spacing (cm) between vehicles at capacity
 */
void SetCapacitySpacing(double capacity_spacing);

/**
 * Returns the default speed (m/s) for vehicle following.
 *
 * @return the default speed (m/s) for vehicle following
 */
double GetFollowingSpeed();

/**
 * Sets the default speed (m/s) for vehicle following.
 *
 * @param following_speed the default speed (m/s) for vehicle following
 */
void SetFollowingSpeed(double following_speed);

/**
 * Returns the length (cm) assigned to injected vehicles.
 *
 * @return the length (cm) assigned to injected vehicles
 */
double GetInjectedVehicleLength();

/**
 * Sets the length (cm) assigned to injected vehicles.
 *
 * @param injection_length the length (cm) assigned to injected vehicles
 */
void SetInjectedVehicleLength(double injection_length);

/**
 * Returns the speed (m/s) assigned to injected vehicles.
 *
 * @return the speed (m/s) assigned to injected vehicles
 */
double GetInjectedVehicleSpeed();

/**
 * Sets the speed (m/s) assigned to injected vehicles.
 *
 * @param injection_speed the speed (m/s) assigned to injected vehicles
 */
void SetInjectedVehicleSpeed(double injection_speed);

/**
 * Returns the intersection ID
 *
 * @return the intersection ID
 */
int GetIntersectionId();

/**
 * Sets the intersection ID
 *
 * @param intersection_id the intersection ID
 */
void SetIntersectionId(int intersection_id);

/**
 * Returns the spacing (cm) between vehicles at jam density.
 *
 * @return the spacing (cm) between vehicles at jam density
 */
double GetJamSpacing();

/**
 * Sets the spacing (cm) between vehicles at jam density.
 *
 * @param jam_spacing the spacing (cm) between vehicles at jam density
 */
void SetJamSpacing(double jam_spacing);

/**
 * Gets the lane to which all vehicles will be assigned.
 *
 * @return the id of the lane to which all vehicles will be assigned
 */
int GetLaneAssignmentId();

/**
 * Sets the lane to which all vehicles will be assigned.
 *
 * @param lane_id the id of the lane to which all vehicles will be assigned
 */
void SetLaneAssignmentId(int lane_id);

/**
 * Returns the squared radius (cm) used to filter distant BSMs.
 *
 * @return the squared radius (cm) used to filter distant BSMs
 */
double GetSquaredBsmRadius();

/**
 * Sets the squared radius (cm) used to filter distant BSMs.
 *
 * @param squared_bsm_radius the squared radius (cm) used to filter distant
 * BSMs
 */
void SetSquaredBsmRadius(double squared_bsm_radius);

#endif
