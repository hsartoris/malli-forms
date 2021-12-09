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

(defn schema->input-type
  "Looks up (m/type schema) in schema-type->input-type"
  [schema]
  ((m/type schema) schema-type->input-type))



(defn- copy-field-spec
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
  (fn [schema _naive-field-spec _children]
    (m/type schema)))

(defmethod build-field-spec :default
  [_schema spec _]
  spec)
  ;; best effort when no specific handling is to guess the type
  ;(default spec :type (schema->input-type schema)))

(defmethod build-field-spec :or
  [_ spec child-specs]
  (into spec (intersect-maps child-specs)))
(defmethod build-field-spec :and
  [_ spec child-specs]
  (into spec (intersect-maps child-specs)))

(defmethod build-field-spec :maybe
  ;; any properties set explicitly on this schema, under those of child, and
  ;; set to required=false
  [_ spec [child-spec]]
  (-> (conj spec child-spec)
      (default :required false)))
(defmethod build-field-spec ::m/val
  [_ spec [child-spec]]
  ;; any properties set explicitly on this schema, under those of child, and
  ;; set to required=(not optional)
  (conj spec child-spec))
  ;(-> (conj spec child-spec)
      ;(default :required  (not (:optional (m/properties schema))))))

(defmethod build-field-spec ::m/schema
  [_ _ [child-spec]]
  ;; contains a ref to another schema - take unconditionally
  child-spec)

(defmethod build-field-spec :map
  [_ spec _]
  ;; Map children will render as fields, so they don't get integrated
  spec)

(defmethod build-field-spec 'map?
  [_ spec _]
  ;; Map children will render as fields, so they don't get integrated
  spec)

(defmethod build-field-spec :re
  [_ spec [child]]
  ;; TODO: figure out how to actually convert
  (assoc spec :pattern (str child)))

;; why though
(defmethod build-field-spec 'nil?
  [_ spec _]
  (assoc spec :type :text, :pattern "^$"))


;; TODO
(defmethod build-field-spec :orn [_ _ _])
(defmethod build-field-spec :andn [_ _ _])

;; TODO: to implement
(defmethod build-field-spec 'seqable? [_ _ _])
(defmethod build-field-spec 'indexed? [_ _ _])
(defmethod build-field-spec 'vector? [_ _ _])
(defmethod build-field-spec :vector [_ _ _])
(defmethod build-field-spec 'list? [_ _ _])
(defmethod build-field-spec 'seq? [_ _ _])
(defmethod build-field-spec :set [_ _ _])
(defmethod build-field-spec 'set? [_ _ _])
(defmethod build-field-spec 'coll? [_ _ _])
(defmethod build-field-spec 'empty? [_ _ _])
(defmethod build-field-spec 'associative? [_ _ _])
(defmethod build-field-spec 'sequential? [_ _ _])
(defmethod build-field-spec :sequential [_ _ _])
(defmethod build-field-spec :tuple [_ _ _])

;;????????
(comment
  :+
  :*
  :repeat
  :cat
  :catn
  :merge
  :union
  :select-keys)

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

(defn- add-path-info
  "Add name, id, and label to a spec, based on a path already added to it"
  [spec]
  (let [path (:path spec)
        input-name (or (:name spec) (path->name path))]
    (-> (assoc spec :name input-name)
        (default :label   (path->label path))
        (default :id      (str "mf-" input-name)))))

(defn- mark-render
  "Mark a schema as rendering an input field"
  [schema]
  (mu/update-properties schema update ::spec #(add-path-info (assoc % :render? true))))

(defmulti set-children
  "Called by the walker in add-field-specs to update the child schemas on a
  parent.
  Default is to replace existing children with provided (i.e., postwalked).
  Override in situations where parent schema needs to modify child properties,
  such as a map."
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

;(defmethod set-children :map-of
;  [schema children]
;  ;; is this a terrible idea?
;  (m/schema
;    ;; technically less restrictive, but w/e
;    [:set {::original schema
;           ::spec     (assoc (::spec (m/properties schema))
;                             :render? false
;                             :yield-child true)}
;     (into [:tuple {::spec {:render? true}}] (map mark-render) children)]))

(defn- recursive-deref
  "Recursively actually deref schema. Must happen before walking, as otherwise
  paths are difficult to recover. m/deref-all doesn't work."
  [schema]
  (m/walk schema
          (m/schema-walker
            (fn [subschema]
              ;; TODO: others include :merge, :union, :select-keys
              (if (#{::m/val ::m/schema :schema} (m/type subschema))
                (first (m/children subschema))
                subschema)))
          #::m{:walk-schema-refs  true
               :walk-refs         true}))

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
    (recursive-deref schema)
    (fn [schema path children _]
      ;(prn schema path children)
      (let [schema-type (m/type schema)
            input-type (schema-type->input-type schema-type)
            children' (if (children-render schema-type)
                        (map mark-render children)
                        children)
            spec (build-field-spec
                   schema
                   (cond-> (copy-field-spec (m/properties schema))
                     input-type (assoc :type input-type))
                   (mapv #(when (m/schema? %)
                            (::spec (m/properties %)))
                         children'))]
        (-> schema
            (mu/update-properties assoc ::spec (assoc spec :path path))
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
