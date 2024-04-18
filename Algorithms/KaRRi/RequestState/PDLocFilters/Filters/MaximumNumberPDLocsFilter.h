#pragma once

#include "Algorithms/KaRRi/BaseObjects/Request.h"

namespace karri {
    /**
     * This filter decreases the number of PD Locs by limiting the amount to the given maximum. PD Locs to be excluded
     * are determined randomly.
     */
     class MaximumNumberPDLocsFilter {
     public:
         MaximumNumberPDLocsFilter(const int maximum) : maximum(maximum), rand(seed) {}

         void filter(std::vector<PDLoc>& pdLocs) {
             if (maximum > 1 && pdLocs.size() > maximum) {
                 // If there are more PD-locs than the maximum number, then we permute the PD-locs randomly and
                 // use only the first maxNumber ones. We make sure that the center is included and stays at the
                 // beginning of the PD-locs.
                 const auto perm = Permutation::getRandomPermutation(pdLocs.size(), rand);
                 perm.applyTo(pdLocs);
                 std::swap(pdLocs[perm[0]], pdLocs[0]);
             }

             const int desiredSize = std::min(static_cast<int>(pdLocs.size()), maximum);
             pdLocs.resize(desiredSize);
         }

     private:
         const int maximum;

         static constexpr int seed = 42;
         std::minstd_rand rand;
     };
}
