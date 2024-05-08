#pragma once

namespace karri {

    template<typename VehCHEnv, typename VehicleInputGraph>
    class DirectionalParetoFilter {
    public:
        DirectionalParetoFilter(VehCHEnv &chEnv, VehicleInputGraph &graph, int paretoMaxima)
                : ch(chEnv.getCH()),
                  graph(graph),
                  paretoMaxima(paretoMaxima - 1) {}

        void filter(std::vector<PDLoc> &pdLocs) {
            std::sort(pdLocs.begin() + 1, pdLocs.end(), [this](const PDLoc &loc1, const PDLoc loc2) {
                int order1 = ch.rank(graph.edgeHead(loc1.loc));
                int order2 = ch.rank(graph.edgeHead(loc2.loc));
                return (order1 > order2) || ((order1 == order2) && (loc1.walkingDist < loc2.walkingDist));
            });

            std::vector<int> octants;

            for (auto& pdLoc : pdLocs) {
                octants.emplace_back(getOctant(pdLoc, pdLocs[0]));
            }

            int swapped = paretoMaxima + 2;
            int octantNumbers[8] = {0};

            for (int i = 0; i < swapped && i < pdLocs.size(); i++) {
                if (octants[i] != -1) {
                    octantNumbers[octants[i]]++;
                }
            }

            // Get other points
            for (int i = swapped; i < pdLocs.size(); i++) {
                if (isNotDominatedByWalkingDistance(pdLocs[i], pdLocs, swapped)) {
                    std::swap(pdLocs[i], pdLocs[swapped]);
                    swapped++;
                    if (octants[i] != -1) {
                        octantNumbers[octants[i]]++;
                    }
                } else if (octants[i] != -1 && octantNumbers[octants[i]] < paretoMaxima) {
                    std::swap(pdLocs[i], pdLocs[swapped]);
                    swapped++;
                    octantNumbers[octants[i]]++;
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

        int getOctant(PDLoc &loc, PDLoc &origin) {
            LatLng locLatLng = graph.template get<LatLngAttribute>(graph.edgeHead(loc.loc));
            LatLng originLatLng = graph.template get<LatLngAttribute>(graph.edgeHead(origin.loc));

            Point locPoint = locLatLng.webMercatorProjection();
            Point originPoint = originLatLng.webMercatorProjection();

            int dx = locPoint.x() - originPoint.x();
            int dy = locPoint.y() - originPoint.y();

            if (dx == 0 && dy == 0) {
                return -1;
            }

            double angle = atan2(dx, dy);

            if (angle <= -7 * M_PI / 8 || angle > 7 * M_PI / 8) {
                return 0;
            } else if (angle <= -5 * M_PI / 8) {
                return 1;
            } else if (angle <= -3 * M_PI / 8) {
                return 2;
            } else if (angle <= -M_PI / 8) {
                return 3;
            } else if (angle <= M_PI / 8) {
                return 4;
            } else if (angle <= 3 * M_PI / 8) {
                return 5;
            } else if (angle <= 5 * M_PI / 8) {
                return 6;
            } else if (angle <= 7 * M_PI / 8) {
                return 7;
            }
            return -1;
        }

        const CH &ch;
        const VehicleInputGraph &graph;
        const int paretoMaxima;
    };
}