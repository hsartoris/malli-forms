(ns malli-forms.util
  "Mostly helpers for producing markup or munging HTML"
  (:require
    [clojure.set :as set]
    [clojure.string :as str]
    [malli.core :as-alias m])
  (:import
    (java.net URLEncoder URLDecoder)))

(set! *warn-on-reflection* true)

;; ------ general utilities -------

(defn url-encode
  "URL-encode a string, including ."
  [^String s]
  (-> s (URLEncoder/encode "UTF-8") (str/replace "." "%2E")))

(defn url-decode
  "URL-decode a string"
  [^String s]
  (URLDecoder/decode s "UTF-8"))

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

(defn update-in*
  "Like clojure.core/update-in, but works with sets as well"
  [m ks f & args]
  (letfn [(up [m ks f args]
            (let [[k & ks] ks
                  newval (if ks
                           (up (get m k) ks f args)
                           (apply f (get m k) args))]
              (if (set? m)
                (conj (disj m k) newval)
                (assoc m k newval))))]
    (up m ks f args)))

;; ------ walking, with inspiration from https://gist.github.com/stuarthalloway/b6d1c8766c747fd81018

(defprotocol PathWalkable
  (pathwalk* [form inner outer path-to-form]
             "Replace subforms of form with (pathwalk inner outer subform path-to-subform),
             where path-to-subform is built from path-to-form"))

(defn pathwalk
  "Walks form with outer and optional inner function.
  Calls `(inner form path), then recursively walks result, then yields `(outer result path)`"
  ([outer form] (pathwalk (fn [form _path] form) outer form))
  ([inner outer form] (pathwalk inner outer form []))
  ([inner outer form path]
   (-> (inner form path)
       (pathwalk* inner outer path)
       (outer path))))

(extend-protocol PathWalkable
  java.util.List
  (pathwalk* [form in out path]
    ;; TODO: should subform have in called with path updated?
    (map-indexed
      (fn [idx subform]
        (pathwalk in out subform (conj path idx)))
      form))
  clojure.lang.PersistentVector
  (pathwalk* [form in out path]
    ;; note: dispatches to pathwalk* intentionally - don't want to call outer on result yet
    (into [] (pathwalk* (seq form) in out path)))

  java.util.Map
  (pathwalk* [form in out path]
    (reduce-kv (fn [ret k v]
                 (assoc ret k (pathwalk in out v (conj path k))))
               form form))
  java.util.Set
  (pathwalk* [form in out path]
    (into #{}
          (map #(pathwalk in out % (conj path %)))
          form))
  ;; bottom out on Object/nil - will be wrapped in call to (outer path subform),
  ;; so just yield form unchanged
  java.lang.Object
  (pathwalk* [form _ _ _] form)
  nil
  (pathwalk* [_ _ _ _] nil))


;; ------ name/label handling ------

(defn- munge-name-part
  "Munge a part of a field name into an HTML-compatible string"
  [s]
  ;; TODO: not very robust
  (cond
    (keyword? s) (recur (subs (str s) 1))
    (not (string? s)) (recur (str s))
    :else (url-encode s)))

(defn path->name
  "Takes a path to a field in a nested data structure and produces a suitable
  HTML input name"
  [path]
  (when (seq path)
    (let [[head & tail] (mapv munge-name-part path)]
      (apply str head (when tail
                        (mapv #(format "[%s]" %) tail))))))

;; TODO: total hack
(defprotocol Labeled
  (label* ^String [this] "Get a String label for an object, or nil"))

(extend-protocol Labeled
  clojure.lang.Keyword
  (label* [kw] (subs (str kw) 1))
  String
  (label* [s] s)
  Object
  (label* [_] nil)
  nil
  (label* [_] nil))

;; TODO: test
(defn value->label
  "Process a value into a form label"
  [v]
  (some-> v label* not-empty
          (str/replace #"[\/\._-]" " ")
          (str/replace #"\bid(?:\b|\z)" "ID")
          (#(str (.toUpperCase (subs % 0 1)) (subs % 1)))))

(defn path->label
  "Takes a path to a field in a nested data structure and attempts to produce
  a human-readable label. Drops first item in path as it will be the data node"
  [path]
  ;; TODO: remove hardcoded hack to do with appending things to path
  (some-> path rest last value->label))

(defn label
  "When appropriate, get a best-guess label for a spec. Assumes :name is set
  correctly."
  [spec]
  (println spec)
  (or (:label spec)
      ;; TODO: never used at the moment
      (some-> spec ::m/name value->label)
      (let [path (:path spec)]
        ;; TODO: maybe just blacklist things
        (when-not (= ::m/in (last path))
          (path->label path)))))

;; TODO: doesn't seem like this belongs here
;; TODO: review for consistency
(def ^:private internal-attrs
  "Keys that may be present on a field spec, and which should be stripped
  before rendering"
  [:render?
   :label?
   :concrete-path?
   :path
   :children
   :order
   :options
   :finalize
   :malli.core/type])

(defn props->attrs
  "Convert field spec from a schema into an attribute map for an input"
  [{:keys [attributes required selected value] :as spec}]
  (cond-> (apply dissoc spec :attributes :required :selected :value internal-attrs)
    (true? required)    (assoc :required true)
    (some? value)       (assoc :value value)
    (some? attributes)  (conj attributes)
    (true? selected)    (assoc :selected true)))
