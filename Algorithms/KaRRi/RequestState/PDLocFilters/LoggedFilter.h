#pragma once

#include "Tools/Timer.h"

namespace karri {
    template<typename FilterT, typename GraphT, typename CHEnvT, typename LoggerT>
    class LoggedFilter {
    public:
        LoggedFilter(FilterT& internal, GraphT& graph, CHEnvT& chEnv)
            : internalFilter(internal),
              graph(graph),
              ch(chEnv.getCH()),
              logger(LogManager<LoggerT>::getLogger("filter_time.csv", "filter_run,time\n")){}

        void filter(std::vector<PDLoc>& pdLocs) {
            Timer timer;
            timer.restart();
            internalFilter.filter(pdLocs);
            auto time = timer.elapsed();

            logger << processed << "," << time << "\n";

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