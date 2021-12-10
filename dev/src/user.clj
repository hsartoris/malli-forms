#_:clj-kondo/ignore
(ns user
  (:require
    [clojure.data :refer [diff]]
    [clojure.pprint :as pp :refer [pp pprint]]
    [clojure.set :as set]
    [clojure.string :as str]
    [malli-forms :as mf]
    [malli.core :as m]
    [malli.transform :as mt]
    [malli.util :as mu])
  (:use clojure.repl))

;; s/(pprint (m\/form *1))/(pp)/g
(defmethod pp/simple-dispatch malli.core.Schema
  [schema]
  (pp/simple-dispatch (m/form schema)))
