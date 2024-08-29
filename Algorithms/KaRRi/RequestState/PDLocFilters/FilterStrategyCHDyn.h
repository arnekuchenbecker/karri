#pragma once

#include "Algorithms/KaRRi/BaseObjects/Request.h"

namespace karri {
    template<typename VehCHEnv, typename VehicleInputGraph>
    class FilterStrategyCHDyn {
    public:
        FilterStrategyCHDyn(VehCHEnv& chEnv, VehicleInputGraph& graph, const double percentage)
            : ch(chEnv.getCH()),
              graph(graph),
              percentage(percentage) {}

        void filter(std::vector<PDLoc>& pdLocs) {
            int maximum = 0;
            for (auto& pdLoc : pdLocs) {
                int rank = ch.rank(graph.edgeHead(pdLoc.loc));
                if (rank > maximum) {
                    maximum = rank;
                }
            }

            int cutoff = maximum - maximum * percentage;

            int swapped = 1;
            for (int i = 1; i < pdLocs.size(); i++) {
                if (ch.rank(graph.edgeHead(pdLocs[i].loc)) >= cutoff) {
                    std::swap(pdLocs[swapped], pdLocs[i]);
                    swapped++;
                }
            }

            pdLocs.resize(swapped);
        }

    private:
        const CH& ch;
        const VehicleInputGraph& graph;
        const double percentage;
    };
}