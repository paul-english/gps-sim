# Survey of Numerical Analysis Term Project

http://www.math.utah.edu/~pa/5600/tp/

## Getting Started

To bootstrap the environment and start up a clojure repl,

    bin/download-files.sh
    lein repl

Once the repl has started,

    user> (go)

This will startup the tests and load anything useful for development.

To build jars for submission

    lein with-profile vehicle:satellite:receiver uberjar
