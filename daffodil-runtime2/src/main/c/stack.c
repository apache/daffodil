#include "stack.h"
#include <error.h>   // for error
#include <stdbool.h> // for bool
#include <stddef.h>  // for ptrdiff_t
#include <stdlib.h>  // for EXIT_FAILURE

// Initialize stack with preallocated array

void
stack_init(stack_t *p_stack, stack_item_t *p_array, ptrdiff_t capacity)
{
    p_stack->p_after = p_array;
    p_stack->p_array = p_array;
    p_stack->capacity = capacity;
}

// Check whether stack is empty

bool
stack_is_empty(stack_t *p_stack)
{
    return p_stack->p_after == p_stack->p_array;
}

// Check whether stack is full

bool
stack_is_full(stack_t *p_stack)
{
    ptrdiff_t count = p_stack->p_after - p_stack->p_array;
    return count >= p_stack->capacity;
}

// Pop element from stack

stack_item_t
stack_pop(stack_t *p_stack)
{
    if (stack_is_empty(p_stack))
    {
        error(EXIT_FAILURE, 0, "Stack underflow - program terminated");
    }
    return *(--p_stack->p_after);
}

// Push element into stack

void
stack_push(stack_t *p_stack, stack_item_t item)
{
    if (stack_is_full(p_stack))
    {
        error(EXIT_FAILURE, 0, "Stack overflow - program terminated");
    }
    *(p_stack->p_after++) = item;
}

// Get stack's top element

stack_item_t
stack_top(stack_t *p_stack)
{
    if (stack_is_empty(p_stack))
    {
        error(EXIT_FAILURE, 0, "Stack empty - program terminated");
    }
    return *(p_stack->p_after - 1);
}
