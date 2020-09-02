#ifndef STACK_H
#define STACK_H

#include <mxml.h>    // for mxml_node_t
#include <stdbool.h> // for bool
#include <stddef.h>  // for ptrdiff_t

// Type of element pushed into stack

typedef mxml_node_t *stack_item_t;

// Implement stack using preallocated array

typedef struct
{
    stack_item_t *p_after;  // Pointer to one past top element
    stack_item_t *p_array;  // Pointer to stack's array
    ptrdiff_t     capacity; // Size of stack's array
} stack_t;

// Initialize stack with preallocated array

extern void stack_init(stack_t *p_stack, stack_item_t *p_array,
                       ptrdiff_t capacity);

// Check whether stack is empty

extern bool stack_is_empty(stack_t *p_stack);

// Check whether stack is full

extern bool stack_is_full(stack_t *p_stack);

// Pop element from stack

extern stack_item_t stack_pop(stack_t *p_stack);

// Push element into stack

extern void stack_push(stack_t *p_stack, stack_item_t item);

// Get stack's top element

extern stack_item_t stack_top(stack_t *p_stack);

#endif // STACK_H
