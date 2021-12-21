(ns malli-forms.util
  "Mostly helpers for producing markup or munging HTML"
  (:require
    [clojure.set :as set]
    [clojure.string :as str]
    ;; TODO
    [reitit.impl :refer [url-encode #_url-decode]]))

;; ------ general utilities -------

(defn unqualify
  ":some/kw -> :kw"
  [kw]
  (keyword (name kw)))

(defn default
  "If `k` is not set in `m`, set it to `v`."
  [m k v]
  (if (some? (get m k)) m (assoc m k v)))

(def ^:private sorted-set-by-count
  (sorted-set-by
    (fn [x y]
      (compare [(count x) x] [(count y) y]))))

;; TODO: test
(defn intersect-maps
  "Returns a map that contains only those entries that are present in every
  map provided."
  ([m] m)
  ([m1 m2]
   (into {} (set/intersection (set m1) (set m2))))
  ([m1 m2 & maps]
   ;; TODO: doubtless wildly suboptimal, not that it's all that important
   (let [map-sets (into sorted-set-by-count (map set) (conj maps m1 m2))]
     (->> (reduce set/intersection (first map-sets) (rest map-sets))
          (into {})))))

;; ------ name/label handling ------

(defn- munge-name-part
  "Munge a part of a field name into an HTML-compatible string"
  [s]
  ;; TODO: not very robust
  (cond
    (keyword? s) (recur (subs (str s) 1))
    (not (string? s)) (recur (str s))
    :else (str/replace (url-encode s) "." "_DOT_")))

(defn path->name
  "Takes a path to a field in a nested data structure and produces a suitable
  HTML input name"
  [path]
  (if (seq path)
    (let [[head & tail] (mapv munge-name-part path)]
      (apply str head (when tail
                        (mapv #(format "[%s]" %) tail))))
    "root"))

;; TODO: test
(defn value->label
  "Process a value into a form label"
  [v]
  (some-> v str
          (cond->
            (keyword? v) (subs 1))
          (str/replace #"[\/\._-]" " ")
          (str/replace #"\bid(?:\b|\z)" "ID")
          (#(str (.toUpperCase (subs % 0 1)) (subs % 1)))))

(defn path->label
  "Takes a path to a field in a nested data structure and attempts to produce
  a human-readable label"
  [path]
  (when (seq path)
    (value->label (last path))))

(defn props->attrs
  "Convert field spec from a schema into an attribute map for an input"
  [{:keys [attributes required selected value] :as spec}]
  (cond-> (dissoc spec :attributes :required :selected :value)
    (true? required)    (assoc :required true)
    (some? value)       (assoc :value value)
    (some? attributes)  (conj attributes)
    (true? selected)    (assoc :selected true)))
