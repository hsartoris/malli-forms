(ns malli-forms
  (:require
    [clojure.set :as set]
    [clojure.string :as str]
    [malli.core :as m]
    [malli.error :as me]
    [malli.registry :as mr]
    [malli.transform :as mt]
    [malli.util :as mu]
    ;; TODO
    [reitit.impl :refer [url-encode #_url-decode]]))

(def form-ns
  "Namespace of keys that control behavior on field generation for schemas"
  "malli-forms")

;; ------ schema definitions ------

(def field-spec-properties-keys
  "The keys of a field spec, as they will appear when embedded in the
  properties of a schema"
  [::name
   ::label
   ::id
   ::type
   ::attributes
   ;; TODO: will/should this appear?
   ::render?])

(comment
  (def s1 [:map [:a string?] [:b int?]])
  (def s2 [:merge s1 ::field-spec])
  (def s3 [:select-keys s1 [:a]]))

(def ^:private local-registry
  {::type         [:enum
                   :number :text :select
                   ;; TODO
                   :date :email :hidden :checkbox :password :submit :url
                   ;; even more TODO
                   ;; :radio :range
                   ;; :color :datetime-local :file :image :month :reset :search :tel :time :week
                   ::collection
                   ]
   ::name         string?
   ::label        string?
   ::id           string?
   ;; TODO
   ::attributes   [:map-of :keyword :any]
   ::render?      [boolean?
                   {:doc "When true, this field spec should be preserved as it is ready to render"}]
   ::path         [:sequential :any]
   ::field-spec.partial (into [:map {:doc "What a field spec will look like
                                          when one or more fields is defined in
                                          the properties of a schema"}]
                              (map #(vector % {:optional true}))
                              field-spec-properties-keys)
   ::field-spec.input
   [:map {:doc "Field spec for an actual input"}
    [:name        [:ref ::name]]
    [:label       {:optional true} ::label]
    [:id          ::id]
    [:type        {:optional true} ::type]
    [:attributes  {:optional true} ::attributes]
    [:render?     {:optional true} ::render?]]

   ::field-spec.collection
   [:map {:doc "Field spec for a parent of multiple other field specs"}
    [:path ::path]
    [:children [:sequential [:ref ::field-spec]]]]
   ::field-spec.form
   [:map {:doc "Field spec for a root element"}
    [:path ::path]
    [:children [:sequential [:ref ::field-spec]]]]
   ::field-spec.map
   [:map {:doc "Field spec for a map, which is a special case"}
    [:path ::path]
    [:children [:sequential [:ref ::field-spec]]]]
   ::field-spec
   [:multi {:dispatch :type}
    [::collection ::field-spec.collection]
    [::form       ::field-spec.form]
    [::map        ::field-spec.map]
    [::m/default  ::field-spec.input]]})

(def registry
  "malli registry for this project"
  (mr/composite-registry
    (m/default-schemas)
    (mu/schemas)
    local-registry))

(def field-spec-schema
  "Schema for a field spec"
  (m/schema ::field-spec {:registry registry}))

;; ------ utilities -------

(defn- unqualify
  ":some/kw -> :kw"
  [kw]
  (keyword (name kw)))

(defn- default
  "If `k` is not set in `m`, set it to `v`."
  [m k v]
  (if (some? (get m k)) m (assoc m k v)))

(def ^:private sorted-set-by-count
  (sorted-set-by
    (fn [x y]
      (compare [(count x) x] [(count y) y]))))

;; TODO: test
(defn- intersect-maps
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

(defn- add-path-info
  "Add name, id, and label to a spec, based on a path already added to it"
  [spec]
  (let [path (:path spec)
        input-name (or (:name spec) (path->name path))]
    (-> (assoc spec :name input-name)
        (default :label   (path->label path))
        ;; if still unset after above, try again with name
        (default :label   (value->label input-name))
        ;; TODO: probably gensym for ids
        (default :id      (str "mf-" input-name)))))

;; ------ building specs from a schema ------

(def schema-type-by-input-type
  "Map of input type keywords to the schema types that they correspond with"
  '{:text     [string?
               :string
               keyword?
               :keyword
               ident?
               simple-ident?
               qualified-ident?
               simple-keyword?
               qualified-keyword?
               symbol?
               simple-symbol?
               qualified-symbol?
               char?
               bytes?
               :re]
    :checkbox [boolean?
               :boolean
               false?
               true?]
    :url      [url?]
    :date     [inst?]
    :number   [number?
               integer?
               int?
               pos-int?
               neg-int?
               nat-int?
               float?
               double?
               pos?
               neg?
               decimal?
               zero?
               rational?
               ratio?
               :> :>= :< :<=
               :int :double]
    :select   [:enum]
    ;; fake input type that includes schema types that may have (many) children
    ;; intentionally does not include predicate schemas, as they cannot have children
    ::collection [;:map ;; TODO: should?: doesn't include map because it really needs special handling
                  :map-of
                  :sequential :vector :set
                  :tuple ;; TODO
                  ;; really big TODO
                  :+ :*
                  :repeat
                  :cat :catn]
    ::map [:map]})

(def schema-type->input-type
  "Simple mapping from schema type to HTML form input type"
  (reduce-kv
    (fn [out input-type schema-types]
      (into out (zipmap schema-types (repeat input-type))))
    {}
    schema-type-by-input-type))

(defn- extract-field-spec
  "Produce a (partial) field spec from a schema by pulling keys with the
  appropriate namespace out of its properties & unqualifying them, and
  attempting to add the schema type based on [[schema-type->input-type]].
  Defaults to required=true"
  {:malli/schema [:=> [:cat ::m/schema] ::field-spec]}
  [schema]
  (let [input-type ((m/type schema) schema-type->input-type)]
    (into (cond-> {}
            input-type (assoc :type input-type))
          (keep (fn [[k v]]
                  (when (= form-ns (namespace k))
                    [(unqualify k) v])))
          (m/properties schema))))

(defn- schema->spec
  "Get a spec from a schema, except for when it is flagged no-spec, or it is
  not a schema"
  [schema]
  (when (m/schema? schema)
    (let [spec (::spec (m/properties schema))]
      (when-not (:no-spec spec) spec))))

(defn- schemas->specs
  "Get specs for every schema in `schemas` that is a schema and is not flagged
  as no-spec"
  [schemas]
  (keep schema->spec schemas))

(defmulti
  ^{:arglists '([schema naive-field-spec child-specs])}
  complete-field-spec
  "Complete or override a field spec for a particular schema. Basic field spec
  as produced by [[extract-field-spec]] will be provided, as well as specs
  already generated for children. Keyed on schema type.
  Default action is to return the naive spec; only necessary to override when
  child specs inform parent spec in some way, such as with `or`, `and`, etc."
  (fn [schema _naive-field-spec _children]
    (m/type schema)))

(defmethod complete-field-spec :default [_ spec _] spec)
(comment
  ;; among other things, default intentionally covers
  #{ :map 'map?  :map-of
  'list? 'seqable? 'seq? 'sequential? :sequential
  'vector? :vector :set 'set? 'coll?
  'indexed? 'associative?
  ;; TODO: evaluate strategy here
  'empty? :tuple}
  ;; Not needed to cover :merge, :select-keys, :union, as they are derefed out
  )

(defmethod complete-field-spec :or
  [_ spec children]
  (->> children schemas->specs intersect-maps (into spec)))
(defmethod complete-field-spec :and
  [_ spec children]
  (->> children schemas->specs intersect-maps (into spec)))

(defmethod complete-field-spec :maybe
  ;; any properties set explicitly on this schema, under those of child, and
  ;; set to required=false
  [_ spec [child]]
  (-> (conj spec (schema->spec child))
      (assoc :required false)))

(defmethod complete-field-spec :re
  [_ spec [child]]
  ;; TODO: figure out how to actually convert
  (assoc spec :pattern (str child)))

(defmethod complete-field-spec :string
  [schema spec _]
  (let [#_:clj-kondo/ignore {:keys [min max]} (m/properties schema)]
    (cond-> spec
      min (assoc :minlength min)
      max (assoc :maxlength max))))

;; why though
(defmethod complete-field-spec 'nil?
  [_ spec _]
  (assoc spec :type :text, :pattern "^$"))

(defmethod complete-field-spec :fn
  [_ _ _]
  {:no-spec true})

(defn- boolean-radio-when-required
  "Mark a boolean field as type radio buttons when required"
  [{:keys [required] :as spec}]
  (if required
    (assoc spec
           :type :radio
           :options [true false])
    spec))

(defmethod complete-field-spec 'boolean?
  [_ spec _]
  (boolean-radio-when-required spec))
(defmethod complete-field-spec :boolean
  [_ spec _]
  (boolean-radio-when-required spec))

(defmethod complete-field-spec :enum
  [_ spec children]
  (assoc spec :options children))

;; TODO
(defmethod complete-field-spec :orn [_ _ _])
(defmethod complete-field-spec :andn [_ _ _])

;;????????
(comment
  :+
  :*
  :repeat
  :cat
  :catn)

(defn add-field-specs
  "Walk schema structure, building field specs with entry and exit transforms.

  Control flow:
  - on enter:
    1. check if schema is flagged as deferring to a child. If so, walk the child
      TODO: change this behvavior
    2. check if the schema is a ref schema that should be derefed. If so, call
      m/-walk on the result of derefing it - this preserves path.
    3. otherwise, use type of schema to add context to `options` before calling
      m/-walk on children, including render flag if schema is of a collection,
      and setting required if at root, or schema is a map and optional=false.
    4. if context is marked as render, and schema is a collection, pass flag on.
      if schema is not a collection, unset flag for walking children (but
      somehow preserve flag for exit?)
  - on exit:
    - if schema "
  ([schema]
   ;; TODO: remove when not doing active dev
   (add-field-specs schema {:registry registry}))
  ([schema options]
   (let [base-path (or (and (m/-ref-schema? schema)
                            (some-> (m/-ref schema) vector))
                       [])]
     (letfn [(inner [walker schema path options]
               (let [stype (m/type schema)
                     children (m/children schema)
                     first-child (first children)]
                 (or ;; when indicated, directly replace with target child - enter:1
                     ;; TODO: ditch this in favor of pattern matching etc
                     (when-some [child-idx (::use-child (m/properties schema))]
                       (m/-inner walker (nth children child-idx) path options))
                     ;; schema is a ref schema but not of type :ref specifically
                     (when (and (m/-ref-schema? schema) (not= :ref stype))
                       (let [ref-name (case stype ;; extraction rules for getting ref name
                                        ::m/schema  (m/-ref schema)
                                        :schema     (m/form first-child)
                                        nil)]
                         (m/-inner walker
                                   ;; add ref name to properties when available
                                   (cond-> (m/deref-all schema options)
                                     ref-name (mu/update-properties assoc-in [::spec ::m/ref] ref-name))
                                   path options)))
                     ;; schema is an unwalked :ref schema - walk and mark
                     (when (and (= :ref stype) (not (contains? (::m/walked-refs options) first-child)))
                       (m/-inner walker
                                 (-> (m/deref-all schema options)
                                     ;; can always add ref name for :ref
                                     (mu/update-properties assoc-in [::spec ::m/ref] (m/-ref schema)))
                                 path
                                 (update options ::m/walked-refs (fnil conj #{}) first-child)))
                     ;; cases that hinge only on schema type
                     (case stype
                       :map ;; basically reproduce the malli source here
                       ;; TODO: might be able to replace this with walking vals
                       (m/-outer walker schema path
                                 (m/-vmap
                                   (fn [[k s]]
                                     (let [props (m/-properties s)]
                                       [k props
                                        (m/-inner walker s
                                                  (conj path k)
                                                  (assoc options
                                                         ::render? true
                                                         ::required (not (:optional props))))]))
                                   (m/-entries schema))
                                 options)
                       ;; TODO: good?
                       :maybe
                       (m/-walk first-child walker path (assoc options ::required false))
                       ;; otherwise, recurse with options sometimes updated
                       ;; TODO: terminating render is killing it for both
                       ;; parent and children - only want to hit children
                       (m/-walk schema walker path
                                (update options ::render?
                                        #(or (= (schema-type->input-type stype) ::collection)
                                             (and % (not (#{:and :andn :or :orn} stype))))))))))
             (outer [schema path children options]
               (let [naive-spec (extract-field-spec schema)
                     root? (= path base-path)
                     render? (and (or (::render? options) root?)
                                  (not (#{::map ::collection} (:type naive-spec))))
                     reqd? (and render? (or root? (::required options)))
                     naive-spec' (cond-> naive-spec
                                   root?    (assoc :root? true)
                                   render?  (assoc :render? true)
                                   reqd?    (assoc :required true))
                     spec (-> (complete-field-spec schema naive-spec' children)
                              ;; add path after complete-field-spec in case of
                              ;; accidental override from child specs
                              (assoc :path path))
                     ;; TODO: maybe set this as an options flag
                     concrete-path? (not (some #(= % ::m/in) path))
                     spec (if (and render? concrete-path?)
                            (add-path-info spec) spec)]
                 ;; TODO: remove children marked no-spec and send back to inner if any
                 (-> schema
                     (mu/update-properties assoc ::spec spec)
                     (m/-set-children children))))]
       (m/-inner
         (reify m/Walker
           (-accept [_ schema _ _] schema)
           (-inner [this schema path options]
             (inner this schema path options))
           (-outer [_ schema path children options]
             (outer schema path children options)))
         (m/schema schema options)
         base-path
         options)))))

;; TODO: use some kind of identifiable value like ::placeholder for placeholders
(def add-placeholders
  "Transformer that adds placeholder values by schema type"
  {:name :add-placeholders
   :encoders {:map {:compile (fn [schema _]
                               (let [child-keys (map #(nth % 0) (m/children schema))]
                                 {;; on enter, ensure placeholder values are present, as
                                  ;; otherwise transform doesn't recurse to  them
                                  :enter (fn [value]
                                           (if (or (nil? value) (map? value))
                                             (reduce #(update %1 %2 identity) value child-keys)
                                             value))}))}
              :set {:enter (fn [value]
                             (if (nil? value) #{nil} value))}
              :map-of {:enter (fn [value]
                                (if (nil? value) {nil nil} value))}}})

(def collect-specs
  "Transformer that collects field specs based on input value."
  {:name            :collect-specs
   :default-encoder {:compile (fn [schema _]
                                ;; maybe store val props in special key on child?
                                (let [props (m/properties schema)
                                      spec (::spec props)]
                                  (cond
                                    (:render? spec)
                                    {:leave (fn [value]
                                              (assoc spec :value value))}

                                    ;; TODO: actually perform splicing here?
                                    (= ::collection (:type spec))
                                    {:leave (fn [child-specs]
                                              (->> child-specs
                                                   (map-indexed
                                                     (fn [idx child-spec]
                                                       (update child-spec :idxs #(cons idx %))))
                                                   (assoc spec :children)))})))}
   :encoders  {:map {:compile (fn [schema _]
                                (let [child-keys (map #(nth % 0) (m/children schema))
                                      spec (::spec (m/properties schema))]
                                  {;; collect child specs, which are now in the vals
                                   :leave (fn [value]
                                            (if (map? value)
                                              ;; preserve order
                                              ;; TODO: custom order via attributes
                                              (assoc spec :children (map value child-keys))
                                              ;; TODO: then what?
                                              value))}))}}})


(defn collect-field-specs
  "Given a schema, a value, and options, prepare the schema via add-field-specs,
  then encode it with collect-specs into a renderable AST"
  ([schema] (collect-field-specs schema nil))
  ([schema source] (collect-field-specs schema source {}))
  ([schema source {::keys [auto-placeholder] :or {auto-placeholder true} :as options}]
   (-> (add-field-specs schema options)
       (m/encode source options (mt/transformer
                                  (when auto-placeholder
                                    add-placeholders)
                                  mt/default-value-transformer
                                  collect-specs)))))


(defn- props->attrs
  "Convert field spec from a schema into an attribute map for an input"
  [{:keys [attributes required selected value] :as spec}]
  (cond-> (dissoc spec :attributes :required :selected :value)
    (true? required)    (assoc :required true)
    (some? value)       (assoc :value value)
    (some? attributes)  (conj attributes)
    (true? selected)    (assoc :selected true)))

(defn- labeled-input
  [{:keys [id label] :as field-spec}]
  [:div.form-row
   (when label
     [:label {:for id} label])
   [:input (props->attrs field-spec)]])

(defn- splice-real-indexes
  "Given a path that contains one or more ::m/in and a sequence of actual
  indexes, replace the first ::m/in with the first index, etc."
  [path idxs]
  (loop [out                    []
         [head & tail :as path] path
         [idx :as idxs]         idxs]
    (cond
      ;; out of replacements - dump the rest of the path into out
      (nil? idx) (into out
                       (map (fn [item]
                              (assert (not= item ::m/in) "Mismatched indexes; extra ::m/in!")
                              item))
                       path)
      ;; out of path - return out, but check no more real indexes
      (nil? head) (do (assert (nil? idx) "Extra indexes; out of ::m/in!") out)
      ;; replace ::m/in with next idx
      (= ::m/in head) (recur (conj out idx) tail (next idxs))
      ;; don't replace, just put on stack
      :else (recur (conj out head) tail idxs))))

(defn- coll-legend
  "Get a legend value for a collection based on its spec"
  [spec]
  (or (some-> spec ::m/name value->label)
      (let [path-end (last (:path spec))]
        (when (and (some? path-end)
                   (not= path-end ::m/in))
          (value->label path-end)))))


(defmulti default-renderer
  "Renderer used when no theme is specified"
  :type)

(defmethod default-renderer :default
  [spec]
  (labeled-input spec))

;(defmethod default-renderer :checkbox
;  [spec]
;  ;; TODO
;  (labeled-input (assoc spec :required false)))

(defmethod default-renderer :radio
  [{:keys [options label value path] :as spec}]
  ;(prn path options)
  [:fieldset
   (when label
     [:legend label])
   (for [option options
         :let [label (value->label option)
               id (path->name (conj path option))
               sel? (= option value)]]
     (list
       [:label {:for id} label]
       [:input (props->attrs (assoc spec
                                    :selected sel?
                                    :id       id
                                    :value    option))]))])

(defmethod default-renderer :select
  [{:keys [options label #_:clj-kondo/ignore name value id] :as spec}]
  (list
    (when label
      [:label {:for id} label])
    [:select (dissoc spec :label :options :value)
     (list
       (when-not (some #(= value %) options) ;; nothing selected
         [:option {:selected true :value "" :disabled true} "Select an option"])
       (for [option options
             :let [label  (value->label option)
                   sel?   (= option value)]]
         [:option 
          (cond-> {:value option}
            sel? (assoc :selected true))
          label]))]))

(defmethod default-renderer ::collection
  [{:keys [children] :as spec}]
  [:fieldset
   (when-some [l (coll-legend spec)]
     [:legend l])
   (interpose [:br] (seq children))])

;; TODO: basically the same as above, but otherwise impossible to distinguish
;; in collect-field-specs
(defmethod default-renderer ::map
  [{:keys [children] :as spec}]
  [:fieldset
   (when-some [l (coll-legend spec)]
     [:legend l])
   (interpose [:br] (seq children))])

(defmethod default-renderer ::form
  [{:keys [child] :as spec}]
  [:div
   ;; TODO: better system
   [:style
    "form { display: table; }
    label, input { display: table-cell; margin-bottom: 10px; }
    div.form-row { display: table-row; }
    label { padding-right: 10px; }"]
   [:form
    (default (props->attrs spec) :method "POST")
    child
    ;; TODO: better solution
    [:input {:type "submit" :name "submit" :value "Submit"}]]])

(defn render-specs
  "Given a value as produced by collect-field-specs and options, renders fields
  defined by AST into markup"
  ([source] (render-specs source {}))
  ([source {:keys [render] :or {render default-renderer} :as options}]
   (m/encode (m/deref field-spec-schema) source options
             (mt/transformer
               {:name :render-specs ;; due to transformer ordering this runs after splice-idxs:leave
                :encoders {:map {:leave render}}}
               {:name :wrap-root
                :encoders {:map {:leave (fn [spec]
                                          (if (:root? spec)
                                            (-> (dissoc spec :children)
                                                (assoc :type  ::form
                                                       :child (render spec)))
                                            spec))}}}
               {:name :splice-idxs
                :encoders {:map {:enter
                                 (fn [{:keys [idxs] :as spec}]
                                   (if (seq idxs)
                                     (-> spec
                                         (update :path splice-real-indexes idxs)
                                         ;; pass idxs on to children if they have none
                                         (update :children
                                                 (fn [children]
                                                   (for [child children]
                                                     (update child :idxs #(or % idxs))))))
                                     spec))
                                 :leave
                                 (fn [spec]
                                   ;; recalculate path info for specs with idxs on them 
                                   (if (seq (:idxs spec))
                                     (add-path-info spec)
                                     ;; path info added at build time for concrete paths
                                     spec))}}}))))

(defn render-form
  "Full pipeline"
  ([schema] (render-form schema nil))
  ([schema source] (render-form schema source {}))
  ([schema source options]
   (-> (collect-field-specs schema source options)
       (render-specs options))))

(defn- unnest-seq
  "See unnest-seq-transformer"
  [value]
  (prn value)
  (if (map? value) (vals value) value))

(def unnest-seq-transformer
  "Transformer that handles the way data in sequential forms (including sets)
  will be returned by ring's nested-params middleware, namely:
  ```
  x[0][a]=1&x[0][b]=2
  ;=>
  {:x {:0 {:a \"1\", :b \"2\"}}}
  ```
  In a nutshell: when this transformer encounters a map where a flat collection
  is expected, it returns the values of the map. Further coercion should be
  performed by other transformers, like malli.transformer/collection-transformer"
  (let [coders (into {}
                     (for [stype (::collection schema-type-by-input-type)
                           :when (not= stype :map-of)]
                       {stype unnest-seq}))]
    {:name      :unnest-seq
     :encoders  coders
     :decoders  coders}))

(def ^:private decoder
  "Memoized m/decoder"
  (memoize m/decoder))

(def parse-stack
  "Transformer stack for parsing input data"
  [;; TODO: can do better than this. Will need to cover map-of as well
   (mt/key-transformer {:decode keyword})
   mt/default-value-transformer ;; TODO: options for this
   unnest-seq-transformer
   (mt/string-transformer) ;; includes mt/json-transformer, basically
   (mt/strip-extra-keys-transformer)])

(def ^:private parse-transformer
  (apply mt/transformer parse-stack))

(defrecord ParseFailure [schema data options decoded explanation humanized])

(defn parse
  "Simple parse and validate using schema against data. Throws on failure."
  ([schema data] (parse schema data {}))
  ([schema data {::keys [validate] :or {validate true} :as options}]
   (let [decode (decoder schema options parse-transformer)
         ;valid? (if validate (m/validator schema options) (constantly true))
         explain (if validate (m/explainer schema options) (constantly nil))
         decoded (try
                   (decode data)
                   (catch Exception e
                     (throw (ex-info "Unexpected exception parsing data"
                                     {:data     data
                                      :schema   schema
                                      :options  options}
                                     e))))]
     (if-some [error (explain decoded)]
       (map->ParseFailure
         {:schema       schema
          :data         data
          :options      options
          :decoded      decoded
          :explanation  error
          :humanized    (me/humanize error)})
       decoded))))

;; remaining:
;; - clean up field-spec schema to match reality
;; - format errors into rendered form
;; - attempt parse macro
;; - attempt parse=>re-render form macro built on previous
;; - optional reitit coercion module
