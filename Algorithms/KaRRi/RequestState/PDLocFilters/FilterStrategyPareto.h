#pragma once

namespace karri {

    template<typename VehCHEnv, typename VehicleInputGraph>
    class SimpleParetoFilter {
    public:
        SimpleParetoFilter(VehCHEnv &chEnv, VehicleInputGraph &graph, int paretoMaxima)
                : ch(chEnv.getCH()),
                  graph(graph),
                  paretoMaxima(paretoMaxima - 1) {}

        void filter(std::vector<PDLoc> &pdLocs) {
            std::sort(pdLocs.begin() + 1, pdLocs.end(), [this](const PDLoc &loc1, const PDLoc loc2) {
                int order1 = ch.rank(graph.edgeHead(loc1.loc));
                int order2 = ch.rank(graph.edgeHead(loc2.loc));
                return (order1 > order2) || ((order1 == order2) && (loc1.walkingDist < loc2.walkingDist));
            });

            int swapped = paretoMaxima + 2;

            // Get other points
            for (int i = swapped; i < pdLocs.size(); i++) {
                if (isNotDominatedByWalkingDistance(pdLocs[i], pdLocs, swapped)) {
                    std::swap(pdLocs[i], pdLocs[swapped]);
                    swapped++;
                }
            }

            pdLocs.resize(std::min(swapped, (int) pdLocs.size()));
        }

    private:
        bool isNotDominatedByWalkingDistance(PDLoc &pdLoc, std::vector<PDLoc> &pdLocs, int numTaken) {
            int dominatedBy = 0;
            for (int i = 1; i < numTaken; i++) {
                if (pdLocs[i].walkingDist < pdLoc.walkingDist) {
                    dominatedBy++;
                }
            }
            return dominatedBy <= paretoMaxima;
        }

        const CH &ch;
        const VehicleInputGraph &graph;
        const int paretoMaxima;
    };
}