#pragma once

#include "Algorithms/KaRRi/BaseObjects/Request.h"
#include "Algorithms/KaRRi/RequestState/PDLocFilters/Parameters/AllPDLocsParams.h"

namespace karri {
    /**
     * This filter does not change any PD-Locs (i.e. the optimization problem will be solved for all found locations)
     */
    class AllPDLocsFilter {
    public:
        void filter(std::vector<PDLoc>& pdLocs, AllPDLocsParams& params) {
            (void) pdLocs;
            (void) params;
        }
    };
}
