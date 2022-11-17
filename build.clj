(ns build
  "Usage:
  clojure -T:build org.corfield.build/clean
  clojure -T:build org.corfield.build/jar
  clojure -T:build deploy
  etc
  "
  (:require
    [clojure.tools.build.api :as b]
    [org.corfield.build :as bb]))

#_:clj-kondo/ignore
(def lib 'org.clojars.hsartoris/malli-forms)

#_:clj-kondo/ignore
(def major 0)

#_:clj-kondo/ignore
(def minor 0)

#_:clj-kondo/ignore
(def patch (b/git-count-revs nil))

#_:clj-kondo/ignore
(def version (format "%s.%s.%s" major minor patch))

(defn jar
  "Wraps org.corfield.build/jar, providing lib and version"
  [opts]
  (bb/jar (assoc opts :lib lib, :version version)))

(defn deploy
  "Deploy JAR to clojars. Requires having CLOJARS_USERNAME and CLOJARS_PASSWORD set in env."
  [opts]
  (bb/deploy (assoc opts :lib lib, :version version)))
