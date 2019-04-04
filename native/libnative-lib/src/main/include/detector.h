/**
 * Defines the structures and functions used to work with detectors in native
 * applications.
 *
 * @author emyers
 * @author ttevendale
 */

#ifndef DETECTOR_H
#define DETECTOR_H

#ifdef __CPLUSPLUS
extern "C" {
#endif

// forward type declarations
typedef struct Detector Detector;
typedef struct DetectorList DetectorList;

/**
 * A vehicle detector.
 */
struct Detector {

  /**
   * The intersection ID.
   */
  int intersection_id;

  /**
   * The detector ID.
   */
  int id;

  /**
   * The lane ID.
   */
  int lane_id;

  /**
   * The vehicle count.
   */
  int count;

  /**
   * The x coordinate.
   */
  int x;

  /**
   * The y coordinate.
   */
  int y;

  /**
   * The vehicle presence status.
   */
  int presence;

};

/**
 * A list of vehicle detectors.
 */
struct DetectorList {

  /**
   * The current detector node in the list.
   */
  Detector detector;

  /**
   * The next detector node in the list.
   */
  struct DetectorList *next;

};

/**
 * Returns a pointer to a list of detectors that includes the given detector
 * and those already present in the given list. NOTE: this function is a
 * utility function. It does not modify the list of deployed detectors. It
 * only affects the given list.
 *
 * @param list a pointer to a list of detectors
 * @param detector a pointer to the detector to include
 * @return a pointer to a list containing all of the given detectors
 */
DetectorList *AddDetectorToList(DetectorList *list, Detector *detector);

/**
 * Frees the memory occupied by the given list of detectors.
 *
 * @param list a pointer to the list of detectors to free from memory.
 */
void DestroyDetectorList(DetectorList *list);

/**
 * Returns a pointer to the list of deployed detectors.
 *
 * @return a pointer to the list of deployed detectors
 */
DetectorList *GetDetectorList();

#ifdef __CPLUSPLUS
}
#endif

#endif
