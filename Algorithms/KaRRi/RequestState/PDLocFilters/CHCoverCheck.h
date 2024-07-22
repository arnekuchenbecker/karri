#pragma once

#include "Algorithms/KaRRi/BaseObjects/Request.h"
#include "DataStructures/Graph/Graph.h"


namespace karri {
    template<typename FilterT, typename GraphT, typename CHEnvT, typename LoggerT>
    class CHCoverCheck {
    public:
        CHCoverCheck(FilterT &internal, GraphT &graph, CHEnvT &chEnv)
                : internal(internal),
                  vehicleGraph(graph),
                  chEnv(chEnv),
                  logger(LogManager<LoggerT>::getLogger("vertices_covered.csv", "filter_run,num_vertices_total,num_vertices_covered\n")) {}

        void filter(std::vector<PDLoc> &pdLocs) {
            const CH &ch = chEnv.getCH();
            auto &upwardGraph = ch.upwardGraph();
            auto &downwardGraph = ch.downwardGraph();
            int coveredVertices = 0;
            for (PDLoc &pdLoc: pdLocs) {
                int vertex = vehicleGraph.edgeHead(pdLoc.loc);
                int rank = ch.rank(vertex);
                bool covered = true;
                FORALL_INCIDENT_EDGES(upwardGraph, rank, edge) {
                    int upwardsVertex = upwardGraph.edgeHead(edge);
                    if (!isVertexCovered(upwardsVertex, pdLocs, ch)) {
                        covered = false;
                        break;
                    }
                }
                FORALL_INCIDENT_EDGES(downwardGraph, rank, edge) {
                    int downwardsVertex = downwardGraph.edgeHead(edge);
                    if (!isVertexCovered(downwardsVertex, pdLocs, ch)) {
                        covered = false;
                        break;
                    }
                }
                if (covered) {
                    coveredVertices++;
                }
            }

            logger << processed << "," << pdLocs.size() << "," << coveredVertices << "\n";

            internal.filter(pdLocs);

            processed++;
        }

    private:
        FilterT &internal;
        GraphT& vehicleGraph;
        CHEnvT& chEnv;
        LoggerT &logger;
        int processed = 0;

        bool isVertexCovered(int vertex, std::vector<PDLoc> &pdLocs, const CH &ch) {
            bool found = false;
            for (PDLoc& loc: pdLocs) {
                if(ch.rank(vehicleGraph.edgeHead(loc.loc)) == vertex) {
                    found = true;
                    break;
                }
            }
            return found;
        }
    };
}