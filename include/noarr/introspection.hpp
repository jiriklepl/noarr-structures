/**
 * @file introspection.hpp
 * @brief This header file aggregates and exposes other headers necessary for noarr structures introspection.
 *
 * This file includes the following headers:
 * - contiguous.hpp: Provides functionality for contiguous data structures.
 * - is_static.hpp: Checks whether structures and parameters are known at compile time.
 * - lower_bound_along.hpp: Defines operations for finding lower bounds along dimensions.
 * - offset_along.hpp: Defines operations for calculating offsets along dimensions.
 * - stride_along.hpp: Contains definitions for calculating strides along dimensions.
 * - uniform_along.hpp: Offers utilities for uniform operations along dimensions.
 */
#ifndef NOARR_INTROSPECTION_HPP
#define NOARR_INTROSPECTION_HPP

#include "structures/introspection/contiguous.hpp"
#include "structures/introspection/is_static.hpp"
#include "structures/introspection/lower_bound_along.hpp"
#include "structures/introspection/offset_along.hpp"
#include "structures/introspection/stride_along.hpp"
#include "structures/introspection/uniform_along.hpp"

#endif // NOARR_INTROSPECTION_HPP
