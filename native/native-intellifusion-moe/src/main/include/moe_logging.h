/**
 * Defines the functions required for logging in the native IntelliFusion MOE
 * application.
 *
 * @author emyers
 */

#ifndef ETEXAS_INTELLIFUSION_MOE_LOGGING
#define ETEXAS_INTELLIFUSION_MOE_LOGGING

/**
 * Logs information for the given queue.
 *
 * @param queue a pointer to the queue to log
 * @param indentations the number of indentations to use
 */
void LogQueue(Queue *queue, int indentations);

/**
 * Logs information for the given queue length.
 *
 * @param queue_length a pointer to the queue length to log
 * @param indentations the number of indentations to use
 */
void LogQueueLength(QueueLength *queue_length, int indentations);

/**
 * Logs information for the given list of queue lengths.
 *
 * @param list a pointer to the list of queue lengths to log
 * @param indentations the number of indentations to use
 */
void LogQueueLengthList(QueueLengthList *list, int indentations);

/**
 * Logs information for each queue in the given list of queues.
 *
 * @param list a pointer to the list of queues to log
 * @param indentations the number of indentations to use
 */
void LogQueueList(QueueList *list, int indentations);

#endif
