#pragma once

#include "Algorithms/KaRRi/BaseObjects/Request.h"

namespace karri {
    /**
     * This filter does not change any PD-Locs (i.e. the optimization problem will be solved for all found locations)
     */
    class FilterStrategyAll {
    public:
        void filter(std::vector<PDLoc>& pdLocs) {
            (void) pdLocs;
        }
    };
}
