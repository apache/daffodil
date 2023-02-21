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

#ifndef STACK_H
#define STACK_H

// clang-format off
#include <mxml.h>     // for mxml_node_t
#include <stdbool.h>  // for bool
#include <stddef.h>   // for ptrdiff_t
// clang-format on

// Type of element pushed into stack

typedef mxml_node_t *stack_item_t;

// Implement stack using preallocated array

typedef struct
{
    stack_item_t *p_after;  // Pointer to one past top element
    stack_item_t *p_array;  // Pointer to stack's array
    ptrdiff_t     capacity; // Size of stack's array
} c_stack_t;

// Initialize stack with preallocated array

extern void stack_init(c_stack_t *p_stack, stack_item_t *p_array, ptrdiff_t capacity);

// Check whether stack is empty

extern bool stack_is_empty(c_stack_t *p_stack);

// Check whether stack is full

extern bool stack_is_full(c_stack_t *p_stack);

// Pop element from stack

extern stack_item_t stack_pop(c_stack_t *p_stack);

// Push element into stack

extern void stack_push(c_stack_t *p_stack, stack_item_t item);

// Get stack's top element

extern stack_item_t stack_top(c_stack_t *p_stack);

#endif // STACK_H
