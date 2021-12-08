(ns malli-forms
  (:require
    [clojure.set :as set]
    [clojure.string :as str]
    [malli.core :as m]
    [malli.registry :as mr]
    [malli.transform :as mt]
    [malli.util :as mu]))

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

(def ^:private static-registry
  {::type         [:enum :number :text
                   ;; TODO
                   :date :email :hidden :checkbox :password :submit :url]
                   ;; even more TODO
                   ;; :radio :range
                   ;; :color :datetime-local :file :image :month :reset :search :tel :time :week
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
                   [:name     ::name]
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
               bytes?]
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

(defn schema->input-type
  "Looks up (m/type schema) in schema-type->input-type"
  [schema]
  ((m/type schema) schema-type->input-type))



(defn- naive-field-spec
  "Extract a (partial) field spec from the properties of a schema by retrieving
  the namespaced keys into non-namespaced ones"
  {:malli/schema [:=> [:cat [:maybe :map]] ::field-spec]}
  [props]
  (reduce
    (fn [out k]
      (if-some [kv (find props k)]
        (assoc out (unqualify (key kv)) (val kv))
        out))
    {} field-spec-properties-keys))

(defn- default
  "If `k` is not set in `m`, set it to `v`."
  [m k v]
  (if (some? (get m k)) m (assoc m k v)))

(defmulti build-field-spec
  "Build the components of a field spec that can be known for a particular
  schema, including based on the field specs of its children"
  (fn [schema _naive-field-spec _children _path]
    (m/type schema)))

(defmethod build-field-spec :default
  [schema spec _ _]
  ;; best effort when no specific handling is to guess the type
  (default spec :type (schema->input-type schema)))

(defmethod build-field-spec :or
  [_ spec child-specs _]
  (into spec (intersect-maps child-specs)))
(defmethod build-field-spec :and
  [_ spec child-specs _]
  (into spec (intersect-maps child-specs)))

(defmethod build-field-spec :maybe
  ;; any properties set explicitly on this schema, under those of child, and
  ;; set to required=false
  [_ spec [child-spec] _]
  (-> (conj spec child-spec)
      (default :required false)))

(defn- add-path-info
  "Add name, id, and label to a spec, based on a path already added to it"
  [spec]
  (let [path (:path spec)
        input-name (or (:name spec) (path->name path))]
    (-> (assoc spec :name input-name)
        (default :label   (path->label path))
        (default :id      (str "mf-" input-name)))))

(defmethod build-field-spec ::m/val
  [schema spec [child-spec] _]
  ;; any properties set explicitly on this schema, under those of child, and
  ;; set to required=(not optional)
  (-> (conj spec child-spec)
      (default :required  (not (:optional (m/properties schema))))))

(defmethod build-field-spec ::m/schema
  [_ _ [child-spec] _]
  ;; contains a ref to another schema - take unconditionally
  child-spec)

(defmethod build-field-spec :map
  [_ spec _ _]
  ;; Map children will render as fields, so they don't get integrated
  spec)

(defmethod build-field-spec 'map?
  [_ spec _ _]
  ;; Map children will render as fields, so they don't get integrated
  spec)

;; TODO: to implement
(defmethod build-field-spec 'seqable? [_ _ _ _])
(defmethod build-field-spec 'indexed? [_ _ _ _])
(defmethod build-field-spec 'vector? [_ _ _ _])
(defmethod build-field-spec :vector [_ _ _ _])
(defmethod build-field-spec 'list? [_ _ _ _])
(defmethod build-field-spec 'seq? [_ _ _ _])
(defmethod build-field-spec :set [_ _ _ _])
(defmethod build-field-spec 'set? [_ _ _ _])
(defmethod build-field-spec 'coll? [_ _ _ _])
(defmethod build-field-spec 'empty? [_ _ _ _])
(defmethod build-field-spec 'associative? [_ _ _ _])
(defmethod build-field-spec 'sequential? [_ _ _ _])
(defmethod build-field-spec :sequential [_ _ _ _])
(defmethod build-field-spec :tuple [_ _ _ _])

(defmulti mark-render-children
  "Mark a schema's children as rendering. Default is not to do anything. Keyed
  on schema type"
  (fn [schema _children]
    (m/type schema)))

(defmethod mark-render-children :default
  [_ children]
  children)

(defn add-field-specs
  "Postwalk schema, calling `build-field-spec` with the schema, its properties,
  and the properties of its children, and adding the output to the schema
  properties."
  [schema]
  (m/walk schema
          (fn [schema path children _]
            ;(prn schema path children)
            (prn (m/type schema) children)
            (let [children' (mark-render-children schema children)
                  child-specs (mapv #(when (m/schema? %)
                                       (::spec (m/properties %)))
                                    children')
                  schema' (m/-set-children schema children')
                  props   (m/properties schema)
                  ;; TODO: remove path I suppose
                  spec    (build-field-spec schema (naive-field-spec props) child-specs path)]
              ;(if (some :render? child-specs)
              ;  (do (println "Not building spec for parent with rendering child:"
              ;               schema child-specs)
              ;      schema')
              (mu/update-properties
                schema'
                #(assoc % ::spec (assoc spec :path path)))))
          #::m{:walk-entry-vals   true
               :walk-schema-refs  true
               :walk-refs         true}))

;(defn complete-field-specs
;  [schema]
;  (m/walk schema
;          (fn [schema path children _]
;            (prn schema path children)
;            (m/-set-properties
;              (m/-set-children schema children)
;              (complete-field-spec
;                (m/type schema)
;                (field-spec schema path)
;                children)))
;          {::m/walk-entry-vals true}))

;(defn complete-field-specs
;  [schema]
;  (m/walk schema
;          (fn [schema path children _]
;            (let [input-type ((m/type schema) schema-type->input-type)]
;              (m/-set-properties
;                (m/-set-children schema children)
;                (-> (field-spec schema path)
;                    (update ::type #(or % (input-type schema children)))))))))

;(defn complete-field-specs
;  "Walk `schema`, adding values to complete field specs on every subschema"
;  [schema]
;  (m/walk schema
;          (fn [schema path children _]
;            (let [children' (if (= :map (m/type schema))
;                              (for [[k cprops cschema] children]
;                                [k cprops (mu/update-properties cschema merge cprops)])
;                              children)]
;            (m/-set-properties
;              (m/-set-children schema children')
;              (-> (field-spec schema path)
;                  (update ::type #(or % (input-type schema children')))))))))

(defn add-paths
  "Walk `schema`, adding path to each subschema to its properties"
  [schema]
  (m/walk schema
          (fn [schema path children _]
            (-> (m/-set-children schema children) ;; children already postwalked
                (mu/update-properties assoc ::path path)))))

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
  [{::keys [id label] :as field-spec} value]
  (add-label
    [:input (props->attrs field-spec value)]
    label id))

(defmulti render-field
  "Renders a field, keyed on `(::type field-spec)`"
  (fn [schema _value]
    (::type (m/properties schema))))

(defmethod render-field :default
  [schema value]
  (prn schema value)
  (labeled-input (m/properties schema) value))
  
(defn- render-enum
  "Render an enum schema, with possible value"
  [schema value]
  (let [{::keys [id label] :as props} (m/properties schema)]
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

(def collect-fields
  "Transformer that collects field specs"
  {:name :collect-fields
   :default-encoder {:compile (fn [schema _]
                                ;(let [field-spec (m/properties schema)]
                                ;  {:enter (fn [value]
                                ;            (
                                (fn [value]
                                  ;(if-some
                                  (assoc (m/properties schema) ::value value)))}
                                ;{:enter (constantly (m/properties schema))})}
   :encoders {:map    {:compile (fn [schema _]
                                  (let [child-keys (map #(nth % 0) (m/children schema))]
                                    {:enter (fn [value]
                                              ;; if child keys aren't present, transform doesn't recurse to them
                                              (if (or (nil? value) (map? value))
                                                (reduce #(update %1 %2 identity) value child-keys)
                                                value))
                                     :leave (fn [value]
                                              ;; preserve order
                                              (map value child-keys))}))}}})

  "Transformer that replaces schema leaves with form specs for that leaf by
  default, with some higher-level schema types rendering based on the form
  specs of their children."

(def render-field-transformer
  "Transformer that renders field specs into hiccup markup"
  ;(let [render-
  {:name :render-field
   ;:default-encoder identity
   ;; can actually override by settings {:encode/render-field {:compile (fn [schema _] ....
   ;; on schema
   :default-encoder {:compile (fn [schema _]
                                {:leave (fn [value]
                                          ;(prn schema value)
                                          (render-field schema value))})}
   :encoders {:map  {:compile (fn [schema _]
                                (let [child-keys (map #(nth % 0) (m/children schema))]
                                  {:enter (fn [value]
                                            ;; if child keys aren't present, transform doesn't recurse to them
                                            (if (or (nil? value) (map? value))
                                              (reduce #(update %1 %2 identity) value child-keys)
                                              value))
                                   :leave (fn [value]
                                            ;; preserve order
                                            (map value child-keys))}))}
                                 ;:leave (fn [value]
                                 ;         (map (fn [[field _ child-schema :as child]]
                                 ;                (render-field child-schema (get value field)))
                                 ;              (m/children schema)))})}
              :enum {:compile (fn [schema _]
                                #(render-enum schema %))}}})


(defn encode-fields
  "Encode the fields of a form from a schema, with an optional object"
  [schema source]
  ;; TODO: point to new version
  (-> (add-field-specs schema)
      (m/encode source (mt/transformer render-field-transformer))))

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
