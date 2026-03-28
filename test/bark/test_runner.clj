(ns bark.test-runner
  "Wrapper around cognitect test-runner that calls System/exit after tests.
  This ensures the JVM terminates even when libraries (e.g. Datalevin)
  leave non-daemon threads running.  Used via `clj -X:test`."
  (:refer-clojure :exclude [test])
  (:require [cognitect.test-runner.api :as api]))

(defn test
  "Run tests and exit. Accepts the same options as cognitect.test-runner.api/test."
  [opts]
  (try
    (api/test opts)
    (shutdown-agents)
    (System/exit 0)
    (catch Throwable t
      (when-not (instance? clojure.lang.ExceptionInfo t)
        (.printStackTrace t))
      (shutdown-agents)
      (System/exit 1))))
