/**
 * Implements the functions used to work with Queues.
 *
 * @author bbadillo
 */

// system files
#include <stdlib.h>     // memory operations

// project files
#include "fifoqueue.h"  // function declarations

/**
 * Create a first-in-first-out queue data structure.
 */
Queue *QueueCreate() {
  Queue *queue = (Queue *) malloc(sizeof(Queue));
  queue->headNode = NULL;
  queue->tailNode = NULL;
  return queue;
}

/**
 * Put a value at the tail of a first-in-first-out queue data structure.
 *
 * @param queue the queue in which to add the data
 * @param data the data to add to the queue
 */
void QueuePut(Queue *queue, void *data) {
  QueueNode *node = (QueueNode *) malloc(sizeof(QueueNode));
  node->data = data;
  node->next = NULL;
  QueueNode *tailNode = queue->tailNode;
  if (tailNode == NULL) {
    queue->headNode = queue->tailNode = node;
  } else {
    tailNode->next = queue->tailNode = node;
  }
}

/**
 * Put a value at the head of a first-in-first-out queue data structure.
 *
 * @param queue the queue from which to remove the data
 */
void *QueueTake(Queue *queue) {
  QueueNode *headNode = queue->headNode;
  if (headNode != NULL) {
    void *data = headNode->data;
    queue->headNode = headNode->next;
    free(headNode);
    if (queue->headNode == NULL) {
      queue->tailNode = NULL;
    }
    return data;
  }
  return NULL;
}
