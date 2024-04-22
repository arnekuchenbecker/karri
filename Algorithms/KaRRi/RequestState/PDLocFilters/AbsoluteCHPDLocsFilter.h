#pragma once

#include "Algorithms/KaRRi/BaseObjects/Request.h"

namespace karri {
    /**
     * This filter removes all PDLocs, whose head vertex is not in the top maximum most important vertices, according to
     * the ordering given by the contraction order of the given CH
     */
    template<typename VehCHEnv, typename VehicleInputGraph>
    class AbsoluteCHPDLocsFilter {
    public:
        AbsoluteCHPDLocsFilter(const VehCHEnv &chEnv, const VehicleInputGraph &graph, const int maximum)
                : ch(chEnv.getCH()),
                  graph(graph),
                  maximum(maximum) {}

        void filter(std::vector<PDLoc> &pdLocs) {
            std::vector<int> ranks;

            for (PDLoc& loc: pdLocs) {
                ranks.emplace_back(ch.rank(graph.edgeHead(loc.loc)));
            }
            std::sort(ranks.begin(), ranks.end(), std::greater());
            ranks.erase( std::unique( ranks.begin(), ranks.end() ), ranks.end() );

            int cutoff = ranks[maximum];

            int swapped = 1;

            for (int i = 1; i < pdLocs.size(); i++) {
                int j = ch.rank(graph.edgeHead(pdLocs[i].loc));
                if (j >= cutoff) {
                    std::swap(pdLocs[swapped], pdLocs[i]);
                    swapped++;
                }
            }

            pdLocs.resize(swapped);
        }

    private:
        const CH &ch;
        const VehicleInputGraph graph;
        const int maximum;
    };
}
