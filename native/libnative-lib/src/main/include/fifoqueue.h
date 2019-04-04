#ifndef FIFO_QUEUE_H
#define FIFO_QUEUE_H

/* A QueueNode used in the Queue. */
typedef struct QueueNode QueueNode;
struct QueueNode {
  void *data;
  QueueNode *next;
};

/* A Queue data structure. */
typedef struct Queue Queue;
struct Queue {
  QueueNode *headNode;
  QueueNode *tailNode;
};

Queue *QueueCreate();

void QueuePut(Queue *, void *);

void *QueueTake(Queue *);

#endif
