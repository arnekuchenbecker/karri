#pragma once

namespace karri {
    template<typename FilterT, typename GraphT, typename CHEnvT, typename LoggerT>
    class LoggedFilter {
    public:
        LoggedFilter(FilterT& internal, GraphT& graph, CHEnvT& chEnv)
            : internalFilter(internal),
              graph(graph),
              ch(chEnv.getCH()),
              logger(LogManager<LoggerT>::getLogger("filter_information.csv", "filter_run,x_coord,y_coord,rank,walking_distance\n")){}

        void filter(std::vector<PDLoc>& pdLocs) {
            LatLng originCoords = graph.template get<LatLngAttribute>(graph.edgeHead(pdLocs[0].loc));
            Point origin = originCoords.webMercatorProjection();

            internalFilter.filter(pdLocs);

            for (auto& pdLoc : pdLocs) {
                LatLng coords = graph.template get<LatLngAttribute>(graph.edgeHead(pdLoc.loc));
                Point flattened = coords.webMercatorProjection(); // using mercator to preserve angles
                logger << processed << "," << flattened.x() - origin.x() << "," << flattened.y() - origin.y() << "," << ch.rank(graph.edgeHead(pdLoc.loc)) << "," << pdLoc.walkingDist << "\n";
            }

            processed++;
        }
    private:
        FilterT& internalFilter;
        const GraphT& graph;
        const CH& ch;
        LoggerT& logger;
        int processed = 0;
    };
}