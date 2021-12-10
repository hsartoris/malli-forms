(ns malli-forms
  (:require
    [clojure.set :as set]
    [clojure.string :as str]
    [malli.core :as m]
    [malli.registry :as mr]
    [malli.transform :as mt]
    [malli.util :as mu]))

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

(def ^:private static-registry
  {::type         [:enum :number :text
                   ;; TODO
                   :date :email :hidden :checkbox :password :submit :url
                   ;; even more TODO
                   ;; :radio :range
                   ;; :color :datetime-local :file :image :month :reset :search :tel :time :week
                   ]
   ::name         string?
   ::label        string?
   ::id           string?
   ;; TODO
   ::attributes   [:map-of :keyword :any]
   ::render?      [boolean?
                   {:doc "When true, this field spec should be preserved as it is ready to render"}]
   ::field-spec.partial (into [:map {:doc "What a field spec will look like
                                          when one or more fields is defined in
                                          the properties of a schema"}]
                              (map #(vector % {:optional true}))
                              field-spec-properties-keys)
   ::field-spec   [:map 
                   [:name     [:ref ::name]]
                   [:label    {:optional true} ::label]
                   [:id       ::id]
                   [:type     {:optional true} ::type]
                   [:attributes ::attributes]
                   [:render?  {:optional true} ::render?]]})


(def ^:private local-registry
  "Temporary mutable malli registry for development; no need for it to stay."
  (atom {}))

;(defn- def!
;  "Add a schema definition to the local registry"
;  [schema-key schema]
;  (swap! local-registry assoc schema-key schema))

(mr/set-default-registry!
  (mr/composite-registry
    (m/default-schemas)
    (mu/schemas)
    (mr/simple-registry static-registry)
    (mr/mutable-registry local-registry)))

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

(defn deref-subschemas
  "Walk schema derefing subchemas; i.e., replace schemas that are references
  to others with those others. Must happen before walking, as otherwise paths
  are difficult to recover. Not the same as m/deref-all.
  Note that this is not fully recursive in that, though it fully descends and
  derefs subschemas in the original, it will not chase emitted refs any further
  than m/walk does."
  ([schema] (deref-subschemas schema {}))
  ([schema options]
   (m/walk schema
           (fn [schema _ children _]
             (let [;; can't call immediately as select-keys will break
                   simple #(m/-set-children schema children)]
               (case (m/type schema)
                 (::m/val ::m/schema :schema :ref) (first children)
                 (:merge :union) (m/deref (simple))
                 ;; needs special handling as long as upstream is broken
                 :select-keys (-> schema
                                  (m/-set-children [(first children) (last (m/children schema))])
                                  m/deref)
                 (simple))))
           (assoc options
                  ::m/walk-entry-vals   false
                  ::m/walk-schema-refs  true
                  ::m/walk-refs         true))))

;; ------ name/label handling ------

(defn- re-quote
  [s]
  (str/replace s #"([-\[\]{}()\*\+\?\.\,\\\^\$\|\#])" #(str \\ (peek %))))

(def ^:private name-substring-replacements
  "Replacement map for characters that will not work in HTML names"
  {"/" "_FSLASH_"
   "<" "_LT_"
   ">" "_GT_"
   "." "_DOT_"})

(def ^:private munge-re
  "Regex matching all entries in [[name-substring-replacements]]"
  ;; TODO: regex quote probably
  (->> (keys name-substring-replacements)
       (mapv re-quote)
       (str/join \|)
       re-pattern))

;(def ^:private demunge-re
;  "Regex matching replacements in [[name-substring-replacements]]"
;  (->> (vals name-substring-replacements)
;       (mapv re-quote)
;       (str/join \|)
;       re-pattern))

(defn munge-name-part
  "Munge a part of a field name into an HTML-compatible string"
  [s]
  (if (ident? s)
    (munge-name-part (subs (str s) 1))
    (str/replace s munge-re name-substring-replacements)))

(defn path->name
  "Takes a path to a field in a nested data structure and produces a suitable
  HTML input name"
  [path]
  (if (seq path)
    (let [[head & tail] (mapv munge-name-part path)]
      (apply str head (when tail
                        (mapv #(format "[%s]" %) tail))))
    "root"))

(defn path->label
  "Takes a path to a field in a nested data structure and attempts to produce
  a human-readable label"
  [path]
  (when (seq path)
    (-> (str/join \space (mapv #(if (keyword? %)
                                  (subs (str %) 1)
                                  (str %))
                               path))
        (str/replace #"[\/\._-]" " ")
        (str/replace #"\bid(?:\b|\z)" "ID")
        (#(str (.toUpperCase (subs % 0 1)) (subs % 1))))))

(defn value->label
  "Render a value via path->label"
  [value]
  (path->label [value]))

(defn- add-path-info
  "Add name, id, and label to a spec, based on a path already added to it"
  [spec]
  (let [path (:path spec)
        input-name (or (:name spec) (path->name path))]
    (-> (assoc spec :name input-name)
        (default :label   (path->label path))
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
               :int :double]})

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
  attempting to add the schema type based on [[schema-type->input-type]]."
  {:malli/schema [:=> [:cat ::m/schema] ::field-spec]}
  [schema]
  (let [input-type ((m/type schema) schema-type->input-type)]
    (into (if input-type {:type input-type} {})
          (keep (fn [[k v]]
                  (when (= form-ns (namespace k))
                    [(unqualify k) v])))
          (m/properties schema))))

(defmulti
  ^{:arglists '([schema naive-field-spec child-specs])}
  complete-field-spec
  "Complete or override a field spec for a particular schema. Basic field spec
  as produced by [[extract-field-spec]] will be provided, as well as specs
  already generated for children. Keyed on schema type.
  Default action is to return the naive spec; only necessary to override when
  child specs inform parent spec in some way, such as with `or`, `and`, etc."
  (fn [schema _naive-field-spec _child-specs]
    (m/type schema)))

(defmethod complete-field-spec :default [_ spec _] spec)
(comment
  ;; among other things, default intentionally covers
  :map 'map?  :map-of
  'list? 'seqable? 'seq? 'sequential? :sequential
  'vector? :vector :set 'set? 'coll?
  'indexed? 'associative?
  ;; TODO: evaluate strategy here
  'empty? :tuple
  ;; Not needed to cover :merge, :select-keys, :union, as they are derefed out
  )

(defmethod complete-field-spec :or
  [_ spec child-specs]
  (into spec (intersect-maps child-specs)))
(defmethod complete-field-spec :and
  [_ spec child-specs]
  (into spec (intersect-maps child-specs)))

(defmethod complete-field-spec :maybe
  ;; any properties set explicitly on this schema, under those of child, and
  ;; set to required=false
  [_ spec [child-spec]]
  (-> (conj spec child-spec)
      (default :required false)))

(defmethod complete-field-spec :re
  [_ spec [child]]
  ;; TODO: figure out how to actually convert
  (assoc spec :pattern (str child)))

;; why though
(defmethod complete-field-spec 'nil?
  [_ spec _]
  (assoc spec :type :text, :pattern "^$"))

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

;; TODO: some of these imply multivalued; others specific, separable fields
(def children-render
  "Children of schemas of these types should render as distinct inputs"
  '#{;; needs special handling
     ;:map
     :map-of ;TODO
     coll?
     :vector vector?
     :set set?
     :sequential sequential? seqable? seq? list?
     ;empty?  ; TODO
     associative?
     :tuple})

(defn- mark-render
  "Mark a schema as rendering an input field"
  [schema]
  (mu/update-properties schema update ::spec #(add-path-info (assoc % :render? true))))

(defmulti set-children
  "Called by the walker in add-field-specs to update the child schemas on a
  parent.
  Default is to replace existing children with provided (i.e., postwalked),
  and marked as rendering where parent is in children-render.
  Override in situations where parent schema needs to modify child properties,
  such as :map, and simply marking the direct child as rendering isn't enough."
  (fn [schema _children]
    (m/type schema)))

(defmethod set-children :default
  [schema children]
  (m/-set-children schema children))

(defmethod set-children :map
  [schema children]
  (->> (for [[k props cschema] children]
         [k props
          (mu/update-properties cschema update ::spec
                                #(-> (add-path-info %)
                                     (assoc :required (not (:optional props)))
                                     (default :render? true)))])
       (m/-set-children schema)))

(defn add-field-specs
  "Postwalk schema, calling `build-field-spec` with the schema, its properties,
  and the properties of its children, and adding the output to the schema
  properties."
  [schema]
  ;; TODO: use schema to define base path, where possible
  ;; i.e., if it's a ::m/schema or a keyword, use the type as the root
  ;; this would allow for better names/labels/legends etc
  ;(let [base-path (
  (m/walk
    (deref-subschemas schema)
    (fn [schema path children _]
      ;(prn schema path children)
      (let [children' (if (children-render (m/type schema))
                        (map mark-render children)
                        children)
            spec (-> schema
                     (complete-field-spec (extract-field-spec schema)
                                          (mapv #(when (m/schema? %)
                                                   (::spec (m/properties %)))
                                                children))
                     ;; add path after complete-field-spec in case of
                     ;; accidental override from child specs
                     (assoc :path path))]
        (-> schema
            (mu/update-properties assoc ::spec spec)
            (set-children children'))))))

(defn- props->attrs
  "Convert field spec from a schema into an attribute map for an input"
  [{:keys [attributes] :as spec} value]
  (cond-> (dissoc spec :attributes)
    (some? value)       (assoc :value value)
    (some? attributes)  (conj attributes)))

(defn- add-label
  "When `label` is non-nil, adds a label field in front of `markup`"
  [markup label id]
  (if label
    (list [:label {:for id} label] markup)
    markup))

(defn- labeled-input
  [{:keys [id label] :as field-spec} value]
  (add-label
    [:input (props->attrs field-spec value)]
    label id))

(defmulti render-field
  "Renders a field, keyed on `(:type field-spec)`"
  (fn [schema _value]
    (-> schema m/properties ::spec :type)))

(defmethod render-field :default
  [schema value]
  ;(prn schema value)
  (labeled-input (::spec (m/properties schema)) value))
  
(defn- render-enum
  "Render an enum schema, with possible value"
  [schema value]
  (let [{:keys [id label] :as props} (::spec (m/properties schema))]
    (add-label
      [:select (props->attrs props nil)
       (for [child (m/children schema)
             :let [cval (if (keyword? child)
                          (subs (str child) 1)
                          (str child))
                   sel? (= cval value)]]
         [:option
          (cond-> {:value cval}
            sel? (assoc :selected true))
          (value->label cval)])]
      label id)))

(def render-fields
  "Transformer that renders field specs into hiccup markup"
  ;(let [render-
  {:name :render-fields
   ;:default-encoder identity
   ;; can actually override by settings {:encode/render-field {:compile (fn [schema _] ....
   ;; on schema
   :default-encoder {:compile (fn [schema _]
                                (let [spec (::spec (m/properties schema))]
                                  (cond
                                    (:render? spec)     (fn [v] (render-field schema v))
                                    (:yield-child spec) (fn [v] v))))}
   :encoders {:map  {:compile (fn [schema _]
                                (let [child-keys (map #(nth % 0) (m/children schema))]
                                  {:enter (fn [value]
                                            ;; if child keys aren't present, transform doesn't recurse to them
                                            (if (or (nil? value) (map? value))
                                              (reduce #(update %1 %2 identity) value child-keys)
                                              value))
                                   :leave (fn [value]
                                            ;; preserve order
                                            (into [:fieldset]
                                                  (comp (map value)
                                                        (interpose [:br]))
                                                  child-keys))}))}
                                            ;[:fieldset
                                             
                                            ;(map value child-keys))}))}
              :map-of {:compile (fn [schema _]
                                  (prn schema)
                                  (let [render?  (-> schema m/properties ::spec :render?)
                                        ;; TODO
                                        path (-> schema m/properties ::spec :path)]
                                    {:enter #(or % {nil nil})
                                     :leave (fn [value]
                                              (if render?
                                                [:fieldset
                                                 [:legend (path->label path)]
                                                 (map #(cons :fieldset %) value)]
                                                value))}))}
              ;; ok tuple is borked good
              ;:tuple {:compile (fn [schema _]
              ;                   (let [len (count (m/children schema))
              ;                         render (-> schema m/properties ::spec :render?)]
              ;                     {:enter (fn [value]
              ;                               (prn value)
              ;                               (or value (repeat len nil)))
              ;                      :leave (fn [value]
              ;                               (prn value)
              ;                               (if render
              ;                                 [:fieldset value]
              ;                                 value))}))}

              :enum {:compile (fn [schema _]
                                (partial render-enum schema))}}})


(defn encode-fields
  "Encode the fields of a form from a schema, with an optional object"
  [schema source]
  (-> (add-field-specs schema)
      (m/encode source (mt/transformer render-fields))))

(def form-props-schema
  "Attributes map that may be defined in the top level field of a schema"
  [:map
   [::attributes
    [:map
     [:method [:enum {:default :POST} :GET :POST]]]]])

(defn encode-form
  "Encode a form from a schema, optionally with a (partial) object"
  ([schema] (encode-form schema nil))
  ([schema source]
   ;[:form (update (::attributes (m/properties schema))
   ;               :method #(or 
   (encode-fields schema source)))
