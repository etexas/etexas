/**
 * Implements the functions used to work with detectors in native applications.
 *
 * @author emyers
 * @author ttevendale
 * @author bbadillo
 */

// NOTE: This define is used to make sure that this compilation unit is built
// as a C++ file even if the compiler does not use this non-standard symbol.
// The detector.h file depends on this.
#define __CPLUSPLUS

// system files
#include <stdlib.h>  // malloc

// project files
#include "detector.h"   // function implementations
#include "native-lib.h" // interface functions to native agent

/**
 * Returns a pointer to a list of detectors that includes the given detector
 * and those already present in the given list.
 *
 * @param list a pointer to a list of detectors
 * @param detector a pointer to the detector to include
 * @return a pointer to a list containing all of the given detectors
 */
DetectorList *AddDetectorToList(DetectorList *list, Detector *detector) {

  DetectorList *temp = (DetectorList *) malloc(sizeof(DetectorList));
  temp->detector = *detector;
  temp->next = list;
  list = temp;

  return list;
}

/**
 * Frees the memory occupied by the given list of detectors.
 *
 * @param list a pointer to the list of detectors to free from memory.
 */
void DestroyDetectorList(DetectorList *list) {

  DetectorList *current = list;
  DetectorList *temp;

  while (current != NULL) {
    temp = current;
    current = current->next;
    free(temp);
  }

  list = NULL;
}

/**
 * Returns a pointer to the list of detectors.
 *
 * @return a pointer to the list of detectors
 */
DetectorList *GetDetectorList() {
  Update();
  return GetCurrentDetectorList();
}
