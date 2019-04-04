/**
 * Implements the functions required for logging in the native IntelliFusion
 * MOE application.
 *
 * @author emyers
 */

// library files
#include <intellifusion_logging.h>  // data structure log statements
#include <logging.h>                // log statements

// project files
#include "moe_data_model.h"  // model data structures
#include "moe_logging.h"     // function implementation

/**
 * Logs information for the given queue.
 *
 * @param queue a pointer to the queue to log
 * @param indentations the number of indentations to use
 */
void LogQueue(Queue *queue, int indentations) {

  char *indent = GetIndentationString(indentations);
  Log("%s<queue lane=%d>\n", indent, queue->lane_id);

  VehicleList *vehicles = queue->vehicles;
  while (vehicles) {

    LogVehicle(vehicles->vehicle, indentations + 1);
    vehicles = vehicles->next;
  }

  Log("%s</queue>\n", indent);
}

/**
 * Logs information for the given queue length.
 *
 * @param queue_length a pointer to the queue length to log
 * @param indentations the number of indentations to use
 */
void LogQueueLength(QueueLength *queue_length, int indentations) {

  char *indent = GetIndentationString(indentations);
  Log("%s<queue_length lane=%d length=%.2f/>\n", indent,
      queue_length->lane_id, queue_length->length);
}

/**
 * Logs information for the given list of queue lengths.
 *
 * @param list a pointer to the list of queue lengths to log
 * @param indentations the number of indentations to use
 */
void LogQueueLengthList(QueueLengthList *list, int indentations) {

  char *indent = GetIndentationString(indentations);
  Log("%s<queue_lengths>\n", indent);

  QueueLengthList *queue_lengths = list;
  while (queue_lengths) {

    LogQueueLength(queue_lengths->queue_length, indentations + 1);
    queue_lengths = queue_lengths->next;
  }

  Log("%s</queue_lengths>\n", indent);
}

/**
 * Logs information for each queue in the given list of queues.
 *
 * @param list a pointer to the list of queues to log
 * @param indentations the number of indentations to use
 */
void LogQueueList(QueueList *list, int indentations) {

  char *indent = GetIndentationString(indentations);
  Log("%s<queues>\n", indent);

  QueueList *queues = list;
  while (queues) {

    LogQueue(queues->queue, indentations + 1);
    queues = queues->next;
  }

  Log("%s</queues>\n", indent);
}
