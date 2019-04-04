/**
 * Implements utility functions that support basic type operations on traffic
 * entities from the MOE data model.
 *
 * @author emyers
 */

// library files
#include <intellifusion_data_model.h>  // model data structures and functions

// project files
#include "moe_data_model.h"  // model data structures, functions

/**
 * Returns a pointer to a list of queue lengths that includes the given queue
 * length and those already present in the given list.
 *
 * @param list a pointer to a list of queue length values
 * @param queue_length a pointer to the queue length value to include
 * @return a pointer to a list containing all of the given queue length values
 */
QueueLengthList *AddQueueLengthToList(
    QueueLengthList *list, QueueLength *queue_length) {

  QueueLengthList *temp = list;

  // if the given list is null
  if (list == NULL) {

    // create a new list that contains the given element
    temp = malloc(sizeof(QueueLengthList));

    if (temp != NULL) {

      temp->next = NULL;
      temp->queue_length = queue_length;
      return temp;

    } else {

      return list;
    }
  }

  // find the last element of the list
  while (temp->next != NULL) {
    temp = temp->next;
  }

  // add the given element to the end of the existing list
  temp->next = malloc(sizeof(QueueLengthList));

  if (temp->next != NULL) {

    temp = temp->next;
    temp->next = NULL;
    temp->queue_length = queue_length;

    return list;
  }

  return list;
}

/**
 * Returns a pointer to a list of queues that includes the given queue and
 * those already present in the given list.
 *
 * @param list a pointer to a list of queues
 * @param queue a pointer to the queue to include
 * @return a pointer to a list containing all of the given queues
 */
QueueList *AddQueueToList(QueueList *list, Queue *queue) {

  QueueList *temp = list;

  // if the given list is null
  if (list == NULL) {

    // create a new list that contains the given element
    temp = malloc(sizeof(QueueList));

    if (temp != NULL) {

      temp->next = NULL;
      temp->queue = queue;
      return temp;

    } else {

      return list;
    }
  }

  // find the last element of the list
  while (temp->next != NULL) {
    temp = temp->next;
  }

  // add the given element to the end of the existing list
  temp->next = malloc(sizeof(QueueList));

  if (temp->next != NULL) {

    temp = temp->next;
    temp->next = NULL;
    temp->queue = queue;

    return list;
  }

  return list;
}

/**
 * Returns a pointer to a copy of the given queue.
 *
 * @param queue a pointer to the queue to copy
 * @return a pointer to a copy of the given queue
 */
Queue *CopyQueue(Queue *queue) {

  Queue *copy = malloc(sizeof(Queue));
  VehicleList *vehicles = NULL;
  copy->vehicles = NULL;

  if (queue == NULL) {

    free(copy);
    return NULL;
  }

  copy->lane_id = queue->lane_id;

  while (vehicles != NULL) {

    copy->vehicles = (VehicleList *) AddVehicleToList(
        copy->vehicles, CopyVehicle(vehicles->vehicle));

    vehicles = vehicles->next;
  }

  return copy;
}

/**
 * Returns a pointer to a copy of the given queue list.
 *
 * @param list a pointer to the queue list to copy
 * @return a pointer to a copy of the given queue list
 */
QueueList *CopyQueueList(QueueList *list) {

  QueueList *copy = NULL;
  QueueList *queues = NULL;

  for (queues = list; queues != NULL; queues = queues->next) {
    copy = AddQueueToList(copy, CopyQueue(queues->queue));
  }

  return copy;
}

/**
 * Frees the memory occupied by the given queue.
 *
 * @param queue a pointer to the queue to free from memory
 */
void DestroyQueue(Queue *queue) {

  DestroyVehicleList(queue->vehicles);
  free(queue);
}

/**
 * Frees the memory occupied by the given list of queue lengths.
 *
 * @param list a pointer to the list of queue lengths to free from memory
 */
void DestroyQueueLengthList(QueueLengthList *list) {

  QueueLengthList *queue_lengths = list;
  QueueLengthList *temp = NULL;

  while (queue_lengths != NULL) {

    temp = queue_lengths->next;
    free(queue_lengths->queue_length);
    free(queue_lengths);
    queue_lengths = temp;
  }
}

/**
 * Frees the memory occupied by the given list of queues.
 *
 * @param list a pointer to the list of queues to free from memory
 */
void DestroyQueueList(QueueList *list) {

  QueueList *queues = list;
  QueueList *temp = NULL;

  while (queues != NULL) {

    temp = queues->next;
    DestroyVehicleList(queues->queue->vehicles);
    free(queues->queue);
    free(queues);
    queues = temp;
  }
}

/**
 * Returns a pointer to a queue from the given list that has the specified
 * lane ID.
 *
 * @param queues a pointer to the list of queues to search
 * @param lane_id the lane ID to match.
 * @return a pointer to a queue from the given list that has the specified
 * lane ID
 */
Queue *GetQueueByLaneId(QueueList* queues, int lane_id) {

  QueueList *temp = queues;

  while (temp) {

    if (temp->queue->lane_id == lane_id) {

      return temp->queue;
    }

    temp = temp->next;
  }

  return NULL;
}
