/// ******************************************************************************
/// MIT License
///
/// Copyright (c) 2023 Moritz Laupichler <moritz.laupichler@kit.edu>
///
/// Permission is hereby granted, free of charge, to any person obtaining a copy
/// of this software and associated documentation files (the "Software"), to deal
/// in the Software without restriction, including without limitation the rights
/// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
/// copies of the Software, and to permit persons to whom the Software is
/// furnished to do so, subject to the following conditions:
///
/// The above copyright notice and this permission notice shall be included in all
/// copies or substantial portions of the Software.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
/// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
/// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
/// SOFTWARE.
/// ******************************************************************************


#pragma once

#include <vector>
#include <cstdint>
#include <Algorithms/KaRRi/BaseObjects//Request.h>
#include <random>
#include "DataStructures/Graph/Attributes/PsgEdgeToCarEdgeAttribute.h"
#include "Algorithms/Dijkstra/Dijkstra.h"

namespace karri {


// This class finds every possible location suited for passenger pickups or dropoffs in a given radius around the
// origin or destination of a request. When queried, a local Dijkstra search bounded by the given radius is executed
// around the center point.
    template<typename PassengerGraphT, typename FilterT, typename WeightT = TravelTimeAttribute>
    class FindPDLocsInRadiusQuery {

    private:
        struct StopWhenRadiusExceeded {
            StopWhenRadiusExceeded(const int radius) : radius(radius) {}

            template<typename DistLabelT, typename DistLabelContainerT>
            bool operator()(const int, DistLabelT &distToV, const DistLabelContainerT & /*distLabels*/) {
                return distToV[0] > radius;
            }

        private:
            const int radius;
        };

        struct RememberSearchSpace {

            RememberSearchSpace(std::vector<int> &searchSpace) : searchSpace(searchSpace) {}

            template<typename DistLabelT, typename DistLabelContT>
            bool operator()(const int v, DistLabelT &, const DistLabelContT &) {
                searchSpace.push_back(v);
                return false;
            }

        private:
            std::vector<int> &searchSpace;

        };

        using PickupSearch = Dijkstra<PassengerGraphT, WeightT, BasicLabelSet<0, ParentInfo::NO_PARENT_INFO>, StopWhenRadiusExceeded, RememberSearchSpace>;
        using DropoffSearch = Dijkstra<PassengerGraphT, WeightT, BasicLabelSet<0, ParentInfo::NO_PARENT_INFO>, StopWhenRadiusExceeded, RememberSearchSpace>;

    public:

        FindPDLocsInRadiusQuery(const PassengerGraphT &forwardPsgGraph,
                                const PassengerGraphT &reversePsgGraph,
                                const InputConfig &inputConfig,
                                FilterT& pdLocFilter,
                                std::vector<PDLoc> &pickups,
                                std::vector<PDLoc> &dropoffs)
                : forwardGraph(forwardPsgGraph),
                  reverseGraph(reversePsgGraph),
                  inputConfig(inputConfig),
                  pdLocFilter(pdLocFilter),
                  pickups(pickups),
                  dropoffs(dropoffs),
                  pickupSearch(forwardPsgGraph, {inputConfig.pickupRadius},
                               {searchSpace}),
                  dropoffSearch(reversePsgGraph, {inputConfig.dropoffRadius},
                                {searchSpace}),
                  searchSpace() {}

        // Pickups will be collected into the given pickups vector and dropoffs will be collected into the given dropoffs vector
        void findPDLocs(const int origin, const int destination) {
            assert(origin < forwardGraph.numEdges() && destination < forwardGraph.numEdges());
            pickups.clear();
            dropoffs.clear();

            searchSpace.clear();
            auto headOfOriginEdge = forwardGraph.edgeHead(origin);
            pickupSearch.run(headOfOriginEdge);
            turnSearchSpaceIntoPickupLocations();

            searchSpace.clear();
            auto tailOfDestEdge = forwardGraph.edgeTail(destination);
            auto destOffset = forwardGraph.travelTime(destination);
            dropoffSearch.runWithOffset(tailOfDestEdge, destOffset);
            turnSearchSpaceIntoDropoffLocations();

            finalizePDLocs(origin, pickups);
            finalizePDLocs(destination, dropoffs);
        }

    private:

        void turnSearchSpaceIntoPickupLocations() {
            for (const auto &v: searchSpace) {
                const auto distToV = pickupSearch.getDistance(v);
                assert(distToV <= inputConfig.pickupRadius);
                FORALL_INCIDENT_EDGES(forwardGraph, v, e) {
                    const int eInVehGraph = forwardGraph.toCarEdge(e);
                    if (eInVehGraph == PsgEdgeToCarEdgeAttribute::defaultValue() ||
                        distToV + forwardGraph.travelTime(e) > inputConfig.pickupRadius)
                        continue;

                    pickups.push_back({INVALID_ID, eInVehGraph, e, distToV + forwardGraph.travelTime(e), INFTY, INFTY});
                }
            }
        }

        void turnSearchSpaceIntoDropoffLocations() {
            for (const auto &v: searchSpace) {
                const auto distToV = dropoffSearch.getDistance(v);
                assert(distToV <= inputConfig.dropoffRadius);
                FORALL_INCIDENT_EDGES(reverseGraph, v, e) {
                    const auto eInForwGraph = reverseGraph.edgeId(e);
                    const int eInVehGraph = forwardGraph.toCarEdge(eInForwGraph);
                    if (eInVehGraph == PsgEdgeToCarEdgeAttribute::defaultValue())
                        continue;
                    dropoffs.push_back({INVALID_ID, eInVehGraph, eInForwGraph, distToV, INFTY, INFTY});
                }
            }
        }

        void finalizePDLocs(const int centerInPsgGraph, std::vector<PDLoc> &pdLocs) {
            // Add center to PD locs
            const int nextSeqId = pdLocs.size();
            const int centerInVehGraph = forwardGraph.toCarEdge(centerInPsgGraph);
            pdLocs.push_back({nextSeqId, centerInVehGraph, centerInPsgGraph, 0, INFTY, INFTY});

            // Remove duplicates
            removeDuplicates(pdLocs);

            // Make sure center is at beginning
            auto centerIt = std::find_if(pdLocs.begin(), pdLocs.end(),
                                         [centerInVehGraph](const auto &h) { return h.loc == centerInVehGraph; });
            assert(centerIt < pdLocs.end());
            const auto idx = centerIt - pdLocs.begin();
            std::swap(pdLocs[0], pdLocs[idx]);

            // Filter the PD Locs according to the provided Filter strategy
            pdLocFilter.filter(pdLocs);

            // Assign sequential ids
            for (int i = 0; i < pdLocs.size(); ++i) {
                pdLocs[i].id = i;
            }

            assert(sanityCheckPDLocs(pdLocs, centerInVehGraph));
        }

        // Remove duplicate PDLocs.
        // Out of each set of PDLocs that have same location, we keep only the one with the shortest walking distance.
        static void removeDuplicates(std::vector<PDLoc> &pdLocs) {
            std::sort(pdLocs.begin(), pdLocs.end(), [](const auto &h1, const auto &h2) {
                return h1.loc < h2.loc || (h1.loc == h2.loc && h1.walkingDist < h2.walkingDist);
            });
            auto last = std::unique(pdLocs.begin(), pdLocs.end(), [](const auto &h1, const auto &h2) {
                return h1.loc == h2.loc;
            });
            pdLocs.erase(last, pdLocs.end());
        }

        static bool sanityCheckPDLocs(const std::vector<PDLoc> &pdLocs, const int centerInVehGraph) {
            if (pdLocs.empty()) return false;
            if (pdLocs[0].loc != centerInVehGraph) return false;
            if (pdLocs[0].walkingDist != 0) return false;
            for (int i = 0; i < pdLocs.size(); ++i) {
                if (pdLocs[i].id != i) return false;
                if (pdLocs[i].vehDistToCenter != INFTY) return false;
                if (pdLocs[i].vehDistFromCenter != INFTY) return false;
            }
            return true;
        }

        const PassengerGraphT &forwardGraph;
        const PassengerGraphT &reverseGraph;
        const InputConfig &inputConfig;
        FilterT& pdLocFilter;
        std::vector<PDLoc> &pickups;
        std::vector<PDLoc> &dropoffs;
        PickupSearch pickupSearch;
        DropoffSearch dropoffSearch;

        std::vector<int> searchSpace;
    };
}