/**
 * Implements the functions used to work with detectors in Arada applications.
 *
 * @author emyers
 */

// system files
#include <stdio.h>   // file reading
#include <stdlib.h>  // atoi, malloc
#include <string.h>  // strcmp, strlen, strtok

// library files
#include <logging.h>  // log statements

// project files
#include "detector.h"  // function implementations

// forward type declarations
typedef struct Property Property;

/**
 * A detector property.
 */
struct Property {

  /**
   * The property name.
   */
  char *name;

  /**
   * The property value.
   */
  int value;
};

/**
 * The environment variable for the detector information file path.
 */
static const char *DETECTOR_INFORMATION_PATH_ = "DETECTOR_INFORMATION_PATH";

/**
 * The detector information file path.
 */
static char *detector_path_;

/**
 * The detector information file.
 */
static FILE *detector_file_;

// forward function declarations
static DetectorList *ReadDetectors();
static Property *ReadProperty();

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
 * Returns a pointer to the list of deployed detectors.
 *
 * @return a pointer to the list of deployed detectors
 */
DetectorList *GetDetectorList() {

  // get the detector information file path
  if (detector_path_ == NULL) {

    detector_path_ = getenv(DETECTOR_INFORMATION_PATH_);

    if (detector_path_ == NULL) {

      detector_path_ = "/var/detector.info";
    }
  }

  // return the list of detectors
  return ReadDetectors();
}

/**
 * Returns a list of detectors read from the detector information file.
 *
 * @return a pointer to the list of detectors read from the information file
 */
static DetectorList *ReadDetectors() {

  // open the detector information file
  if ((detector_file_ = fopen(detector_path_, "r")) == NULL) {
    LogError("opening detector file");
    return NULL;
  }

  DetectorList *detectors = NULL;

  // while more detectors exist
  Property *property;
  while ((property = ReadProperty()) != NULL) {

    // create the detector list item
    Detector *detector = malloc(sizeof(Detector));

    // get the detector ID
    detector->id = property->value;
    if (strcmp(property->name, "id") != 0) {
      Log("expected detector ID, found %s\n", property->name);
      fclose(detector_file_);
      return detectors;
    }

    // get the detector lane
    property = ReadProperty();
    detector->lane_id = property->value;
    if (strcmp(property->name, "lane") != 0) {
      Log("expected detector lane, found %s\n", property->name);
      fclose(detector_file_);
      return detectors;
    }

    // get the detector x coordinate
    property = ReadProperty();
    detector->x = property->value;
    if (strcmp(property->name, "x") != 0) {
      Log("expected detector x coordinate, found %s\n", property->name);
      fclose(detector_file_);
      return detectors;
    }

    // get the detector y coordinate
    property = ReadProperty();
    detector->y = property->value;
    if (strcmp(property->name, "y") != 0) {
      Log("expected detector y coordinate, found %s\n", property->name);
      fclose(detector_file_);
      return detectors;
    }

    // get the detector presence information
    property = ReadProperty();
    detector->presence = property->value;
    if (strcmp(property->name, "presence") != 0) {
      Log("expected detector presence status, found %s\n", property->name);
      fclose(detector_file_);
      return detectors;
    }

    // get the detector vehicle count information
    property = ReadProperty();
    detector->count = property->value;
    if (strcmp(property->name, "count") != 0) {
      Log("expected detector vehicle count, found %s\n", property->name);
      fclose(detector_file_);
      return detectors;
    }

    // add the created detector to the list
    detectors = AddDetectorToList(detectors, detector);
  }

  // close the detector information file
  fclose(detector_file_);

  return detectors;  // return the created detectors
}

/**
 * Returns the next detector property read from the information file.
 *
 * @return a pointer to the next detector property
 */
static Property *ReadProperty() {

  // read the next line from the detector information file
  char *line = malloc(BUFSIZ);
  if (fgets(line, BUFSIZ, detector_file_) == NULL) {
    return NULL;
  }

  // break the line into name=value tokens
  line[strlen(line) - 1] = '\0';
  char *token = strtok(line, "=");

  // create the property read from the file
  Property *property = malloc(sizeof(Property));
  property->name = token;
  property->value = atoi(strtok(NULL, "="));

  return property;  // return the created property
}
