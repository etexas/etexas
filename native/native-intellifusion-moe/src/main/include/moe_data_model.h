/**
 * Defines the types that represent intersection traffic entities in the MOE
 * application and the utility functions that support basic type operations.
 *
 * @author emyers
 */

#ifndef ETEXAS_INTELLIFUSION_MOE_DATAMODEL
#define ETEXAS_INTELLIFUSION_MOE_DATAMODEL

// forward type declarations
typedef struct Queue Queue;
typedef struct QueueLength QueueLength;
typedef struct QueueLengthList QueueLengthList;
typedef struct QueueList QueueList;

/**
 * A queue of vehicles. The queue should be a subset of the vehicles in the
 * lane, where each vehicle is moving slowly enough to be considered as
 * stopped, and each vehicle is close enough to the preceding vehicle to be
 * considered as part of a queue.
 */
struct Queue {

  /**
   * The lane ID where the queue exists.
   */
  int lane_id;

  /**
   * The list of vehicles in the queue.
   */
  VehicleList *vehicles;
};

/**
 * A length metric for queued vehicles.
 */
struct QueueLength {

  /**
   * The lane ID where the queue exists.
   */
  int lane_id;

  /**
   * The length (cm) of the queue
   */
  double length;
};

/**
 * A list of queue lengths.
 */
struct QueueLengthList {

  /**
   * The current queue length node in the list.
   */
  QueueLength *queue_length;

  /**
   * The next queue length node in the list.
   */
  QueueLengthList *next;
};

/**
 * A list of vehicle queues.
 */
struct QueueList {

  /**
   * The current queue node in the list.
   */
  Queue *queue;

  /**
   * The next queue node in the list.
   */
  QueueList *next;
};

/**
 * Returns a pointer to a list of queue lengths that includes the given queue
 * length and those already present in the given list.
 *
 * @param list a pointer to a list of queue length values
 * @param queue_length a pointer to the queue length value to include
 * @return a pointer to a list containing all of the given queue length values
 */
QueueLengthList *AddQueueLengthToList(
    QueueLengthList *list, QueueLength *queue_length);

/**
 * Returns a pointer to a list of queues that includes the given queue and
 * those already present in the given list.
 *
 * @param list a pointer to a list of queues
 * @param queue a pointer to the queue to include
 * @return a pointer to a list containing all of the given queues
 */
QueueList *AddQueueToList(QueueList *list, Queue *queue);

/**
 * Returns a pointer to a copy of the given queue.
 *
 * @param queue a pointer to the queue to copy
 * @return a pointer to a copy of the given queue
 */
Queue *CopyQueue(Queue *queue);

/**
 * Returns a pointer to a copy of the given queue list.
 *
 * @param list a pointer to the queue list to copy
 * @return a pointer to a copy of the given queue list
 */
QueueList *CopyQueueList(QueueList *list);

/**
 * Frees the memory occupied by the given queue.
 *
 * @param queue a pointer to the queue to free from memory
 */
void DestroyQueue(Queue *queue);

/**
 * Frees the memory occupied by the given list of queue lengths.
 *
 * @param list a pointer to the list of queue lengths to free from memory
 */
void DestroyQueueLengthList(QueueLengthList *list);

/**
 * Frees the memory occupied by the given list of queues.
 *
 * @param list a pointer to the list of queues to free from memory
 */
void DestroyQueueList(QueueList *list);

/**
 * Returns a pointer to a queue from the given list that has the specified
 * lane ID.
 *
 * @param queues a pointer to the list of queues to search
 * @param lane_id the lane ID to match.
 * @return a pointer to a queue from the given list that has the specified
 * lane ID
 */
Queue *GetQueueByLaneId(QueueList* queues, int lane_id);

#endif
