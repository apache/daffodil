/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// clang-format off
#include "stack.h"
#include <stdbool.h>     // for bool
#include <stddef.h>      // for ptrdiff_t
#include "cli_errors.h"  // for CLI_STACK_EMPTY, CLI_STACK_OVERFLOW, CLI_STACK_UNDERFLOW
#include "errors.h"      // for continue_or_exit, Error
// clang-format on

// Initialize stack with preallocated array

void
stack_init(c_stack_t *p_stack, stack_item_t *p_array, ptrdiff_t capacity)
{
    p_stack->p_after = p_array;
    p_stack->p_array = p_array;
    p_stack->capacity = capacity;
}

// Check whether stack is empty

bool
stack_is_empty(c_stack_t *p_stack)
{
    return p_stack->p_after == p_stack->p_array;
}

// Check whether stack is full

bool
stack_is_full(c_stack_t *p_stack)
{
    ptrdiff_t count = p_stack->p_after - p_stack->p_array;
    return count >= p_stack->capacity;
}

// Pop element from stack

stack_item_t
stack_pop(c_stack_t *p_stack)
{
    if (stack_is_empty(p_stack))
    {
        const Error error = {CLI_STACK_UNDERFLOW, {0}};
        continue_or_exit(&error);
    }
    return *(--p_stack->p_after);
}

// Push element into stack

void
stack_push(c_stack_t *p_stack, stack_item_t item)
{
    if (stack_is_full(p_stack))
    {
        const Error error = {CLI_STACK_OVERFLOW, {0}};
        continue_or_exit(&error);
    }
    *(p_stack->p_after++) = item;
}

// Get stack's top element

stack_item_t
stack_top(c_stack_t *p_stack)
{
    if (stack_is_empty(p_stack))
    {
        const Error error = {CLI_STACK_EMPTY, {0}};
        continue_or_exit(&error);
    }
    return *(p_stack->p_after - 1);
}
