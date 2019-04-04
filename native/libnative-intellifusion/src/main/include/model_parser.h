/**
 * Defines the function that should be implemented by algorithms that parse an
 * information model of an intersection from the available DSRC messages and
 * detector information for a given point in time.
 *
 * @author emyers
 */

#ifndef ETEXAS_INTELLIFUSION_MODEL_PARSING
#define ETEXAS_INTELLIFUSION_MODEL_PARSING

// library files
#include <detector.h>  // detector list

/**
 * Returns a pointer to an information model of an intersection derived from
 * the given DSRC messages and detector information for a given point in time.
 *
 * @param time the current time (s)
 * @param message_count the number of DSRC messages received
 * @param messages a pointer to an array of DSRC messages
 * @param message_lengths a pointer to an array of message lengths (bytes)
 * @param detectors a pointer to a list of detectors
 * @return a pointer to the parsed information model
 */
InfoModel *ParseModel(
    double time, int message_count, char **messages, int *message_lengths,
    DetectorList *detectors);

#endif
