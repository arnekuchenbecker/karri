#pragma once

#include "Algorithms/KaRRi/BaseObjects/Request.h"

namespace karri {
    /**
     * This filter returns the top maximum number of PDLocs, sorted first by decreasing importance (according to the
     * contraction order of the given CH) and second by increasing walking distance.
     */
    template<typename VehCHEnv, typename VehicleInputGraph>
    class FilterStrategyCHFix {
    public:
        FilterStrategyCHFix(const VehCHEnv &chEnv, const VehicleInputGraph &graph, const int maximum)
                : ch(chEnv.getCH()),
                  graph(graph),
                  maximum(maximum) {}

        void filter(std::vector<PDLoc> &pdLocs) {
            if (pdLocs.size() <= maximum) {
                // Nothing to do here, we already have less than the required amount of PD Locs
                return;
            }

            else if (maximum > 1) {
                std::sort(pdLocs.begin() + 1, pdLocs.end(), [this](const PDLoc& loc1, const PDLoc loc2) {
                    int order1 = ch.rank(graph.edgeHead(loc1.loc));
                    int order2 = ch.rank(graph.edgeHead(loc2.loc));
                    return (order1 > order2) || ((order1 == order2) && (loc1.walkingDist < loc2.walkingDist));
                });
            }

            pdLocs.resize(maximum);
        }

    private:
        const CH& ch;
        const VehicleInputGraph& graph;
        const int maximum;
    };
}
