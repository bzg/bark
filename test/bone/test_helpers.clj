(ns bone.test-helpers
  "Shared test utilities for Datalevin-backed tests."
  (:require [clojure.java.io :as io]
            [datalevin.core :as d]
            [bone.commands :as commands]))

(defn teardown!
  "Close the Datalevin connection and delete the temporary database directory."
  [{:keys [conn db-path]}]
  (d/close conn)
  (let [dir (io/file db-path)]
    (when (.exists dir)
      (doseq [f (reverse (file-seq dir))]
        (.delete f)))))

(defn with-temp-failures-file
  "A `clojure.test` fixture (`:once` or `:each`) that rebinds
  `bone.commands/*failures-file*` to a temp path for the duration of
  the test run, so denied commands don't pollute the real
  `data/.failures.edn`.  The file is marked `deleteOnExit` and also
  removed eagerly after the fixture body."
  [f]
  (let [tmp (doto (java.io.File/createTempFile "bone-failures" ".edn")
              (.deleteOnExit))]
    (try
      (binding [commands/*failures-file* (.getAbsolutePath tmp)]
        (f))
      (finally
        (.delete tmp)))))
