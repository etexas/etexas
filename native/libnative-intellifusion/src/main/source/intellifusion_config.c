/**
 * Implements the functions to set and retrieve intersection properties that
 * are required for accurate calculations in a variety of contexts.
 *
 * @author emyers
 */

// project files
#include "intellifusion_config.h"  // function implementations

/**
 * The squared radius (cm) used to filter distant BSMs. The default value for
 * field testing is based on a radius of 550 feet. Use 671,224,464 (radius of
 * 850 feet) when testing in eTEXAS.
 */
static double squared_bsm_radius_ = 281031696;

/**
 * The spacing (cm) between vehicles at capacity.
 */
static double capacity_spacing_ = 826 * 2;

/**
 * The default following speed (m/s) for vehicle movement calculations.
 */
static double following_speed_ = 13.83;

/**
 * The length (cm) assigned to injected vehicles.
 */
static double injection_length_ = 497.84;

/**
 * The speed (m/s) assigned to injected vehicles.
 */
static double injection_speed_ = 13.83;

/**
 * The intersection ID.
 */
static int intersection_id_ = 0;

/**
 * The spacing (cm) between vehicle at jam density.
 */
static double jam_spacing_ = 826;

/**
 * The id of the lane to which all vehicles will be assigned.
 */
static int lane_assignment_id_ = 0;

/**
 * Returns the spacing (cm) between vehicles at capacity.
 *
 * @return the spacing (cm) between vehicles at capacity
 */
double GetCapacitySpacing() {

  return capacity_spacing_;
}

/**
 * Sets the spacing (cm) between vehicles at capacity.
 *
 * @param capacity_spacing the spacing (cm) between vehicles at capacity
 */
void SetCapacitySpacing(double capacity_spacing) {

  capacity_spacing_ = capacity_spacing;
}

/**
 * Returns the default speed (m/s) for vehicle following.
 *
 * @return the default speed (m/s) for vehicle following
 */
double GetFollowingSpeed() {

  return following_speed_;
}

/**
 * Sets the default speed (m/s) for vehicle following.
 *
 * @param following_speed the default speed (m/s) for vehicle following
 */
void SetFollowingSpeed(double following_speed) {

  following_speed_ = following_speed;
}

/**
 * Returns the length (cm) assigned to injected vehicles.
 *
 * @return the length (cm) assigned to injected vehicles
 */
double GetInjectedVehicleLength() {

  return injection_length_;
}

/**
 * Sets the length (cm) assigned to injected vehicles.
 *
 * @param the length (cm) assigned to injected vehicles
 */
void SetInjectedVehicleLength(double injection_length) {

  injection_length_ = injection_length;
}

/**
 * Returns the speed (m/s) assigned to injected vehicles.
 *
 * @return the speed (m/s) assigned to injected vehicles
 */
double GetInjectedVehicleSpeed() {

  return injection_speed_;
}

/**
 * Sets the speed (m/s) assigned to injected vehicles.
 *
 * @param injection_speed the speed (m/s) assigned to injected vehicles
 */
void SetInjectedVehicleSpeed(double injection_speed) {

  injection_speed_ = injection_speed;
}

/**
 * Returns the intersection ID.
 *
 * @return The intersection ID.
 */
int GetIntersectionId() {

  return intersection_id_;
}

/**
 * Sets the intersection ID.
 *
 * @param id the intersection ID
 */
void SetIntersectionId(int intersection_id) {

  intersection_id_ = intersection_id;
}

/**
 * Returns the spacing (cm) between vehicles at jam density.
 *
 * @return the spacing (cm) between vehicles at jam density
 */
double GetJamSpacing() {

  return jam_spacing_;
}

/**
 * Sets the spacing (cm) between vehicles at jam density.
 *
 * @param jam_spacing the spacing (cm) between vehicles at jam density
 */
void SetJamSpacing(double jam_spacing) {

  jam_spacing_ = jam_spacing;
}

/**
 * Gets the lane to which all vehicles will be assigned.
 *
 * @return the id of the lane to which all vehicles will be assigned
 */
int GetLaneAssignmentId() {
  return lane_assignment_id_;
}

/**
 * Sets the lane to which all vehicles will be assigned.
 *
 * @param lane_id the id of the lane to which all vehicles will be assigned
 */
void SetLaneAssignmentId(int lane_id) {
  lane_assignment_id_ = lane_id;
}

/**
 * Returns the squared radius (cm) used to filter distant BSMs.
 *
 * @return the squared radius (cm) used to filter distant BSMs
 */
double GetSquaredBsmRadius() {

  return squared_bsm_radius_;
}

/**
 * Sets the squared radius (cm) used to filter distant BSMs.
 *
 * @param squared_bsm_radius the squared radius (cm) used to filter distant
 * BSMs
 */
void SetSquaredBsmRadius(double squared_bsm_radius) {

  squared_bsm_radius_ = squared_bsm_radius;
}
