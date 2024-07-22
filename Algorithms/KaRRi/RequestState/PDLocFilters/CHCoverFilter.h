#pragma once

#include "Algorithms/KaRRi/BaseObjects/Request.h"
#include "DataStructures/Graph/Graph.h"

namespace karri {
    /**
    * This filter returns all PDLocs that are not covered by other PDLocs, i.e. that have at least one upward neighbor
    * that is not contained in the pdlocs for this request
    */
    template<typename VehCHEnv, typename VehicleInputGraph>
    class CHCoverFilter {
    public:
        CHCoverFilter(const VehCHEnv &chEnv, const VehicleInputGraph &graph)
        : chEnv(chEnv), graph(graph) {}

        void filter(std::vector<PDLoc> &pdLocs) {
            size_t length = pdLocs.size();
            size_t coveredVertices = 0;
            for(size_t i = 1; i < length; i++) {
                if (isLocationCovered(pdLocs, i - coveredVertices)) {
                    std::swap(pdLocs[i - coveredVertices], pdLocs[length - coveredVertices - 1]);
                    coveredVertices++;
                }
            }
            pdLocs.resize(length - coveredVertices);
        }

    private:
        const VehCHEnv& chEnv;
        const VehicleInputGraph& graph;

        bool isLocationCovered(std::vector<PDLoc> &pdLocs, size_t location) {
            const CH &ch = chEnv.getCH();
            auto &upwardGraph = ch.upwardGraph();
            auto &downwardGraph = ch.downwardGraph();
            int vertex = graph.edgeHead(pdLocs[location].loc);
            int rank = ch.rank(vertex);
            FORALL_INCIDENT_EDGES(upwardGraph, rank, edge) {
                int upwardsVertex = upwardGraph.edgeHead(edge);
                if (!isVertexCovered(upwardsVertex, pdLocs, ch)) {
                    return false;
                }
            }
            FORALL_INCIDENT_EDGES(downwardGraph, rank, edge) {
                int downwardsVertex = downwardGraph.edgeHead(edge);
                if (!isVertexCovered(downwardsVertex, pdLocs, ch)) {
                    return false;
                }
            }
            return true;
        }

        bool isVertexCovered(int vertex, std::vector<PDLoc> &pdLocs, const CH &ch) {
            for (PDLoc& loc: pdLocs) {
                if(ch.rank(graph.edgeHead(loc.loc)) == vertex) {
                    return true;
                }
            }
            return false;
        }
    };
}
