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

(def ex-schema
  "Schema for https://medium.com/@kirill.ishanov/building-forms-with-re-frame-and-clojure-spec-6cf1df8a114d"
  [:map {:closed true
         :registry {::range [:map {:closed true}
                             [:first  [:int {:min 1, :max 65536}]]
                             [:last   [:int {:min 1, :max 65536}]]]}}
   [:label                          [:string {:min 1, :max 64}]]
   [:preallocated {:default false}  boolean?]
   [:type         {:optional true}  [:enum "multihomed" "stub" "transit" "ixp"]]
   [:tags         {:optional true}  [:set [:string {:max 32}]]]
   [:ranges                         [:set {:min 1} ::range]]])

(comment
  (require '[malli-forms-test :as mft]
           '[malli-forms-util-test :as mfut]
           '[malli-forms.reitit-test :as mfrt]))
