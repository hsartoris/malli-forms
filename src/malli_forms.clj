(ns malli-forms
  (:require
    [clojure.set :as set]
    [clojure.string :as str]
    [malli.core :as m]
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

(def ^:private local-registry
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
   ::path         [:sequential :any]
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

(def registry
  "malli registry for this project"
  (merge (m/default-schemas)
         (mu/schemas)
         local-registry))

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
             ;(prn schema children)
             (let [;; can't call immediately as select-keys will break
                   simple #(m/-set-children schema children)]
               ;; TODO: keep track of the ref on the child schema for labeling
               (case (m/type schema)
                 :schema (first children)

                 (::m/val ::m/schema :ref)
                 (let [child (first children)]
                   (cond-> child ;; some will have keywords etc as first child
                     (m/schema? child)
                     (mu/update-properties assoc-in [::spec ::m/name] (m/-ref schema))))
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
  ;; TODO: set - special behavior?
  ;(= ::m/in s) ""
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
               :int :double]
    ;; fake input type that includes schema types that may have (many) children
    ;; intentionally does not include predicate schemas, as they cannot have children
    ::collection [:map :map-of
                  :sequential :vector :set
                  :tuple ;; TODO
                  ;; really big TODO
                  :+ :*
                  :repeat
                  :cat :catn]})

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
    (into (cond-> {:required true}
            input-type (assoc :type input-type))
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
  #{ :map 'map?  :map-of
  'list? 'seqable? 'seq? 'sequential? :sequential
  'vector? :vector :set 'set? 'coll?
  'indexed? 'associative?
  ;; TODO: evaluate strategy here
  'empty? :tuple}
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
      (assoc :required false)))

(defmethod complete-field-spec :re
  [_ spec [child]]
  ;; TODO: figure out how to actually convert
  (assoc spec :pattern (str child)))

;; why though
(defmethod complete-field-spec 'nil?
  [_ spec _]
  (assoc spec :type :text, :pattern "^$"))

(defmethod complete-field-spec :fn
  [_ _ _]
  {:no-spec true})

(defmethod complete-field-spec :enum
  [_ spec _]
  (assoc spec :type :select))

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
  (set (::collection schema-type-by-input-type)))
  ;'#{;; needs special handling
  ;   :map
  ;   :map-of ;TODO
  ;   coll?
  ;   :vector vector?
  ;   :set set?
  ;   :sequential sequential? seqable? seq? list?
  ;   ;empty?  ; TODO
  ;   associative?
  ;   :tuple})

(defn- mark-render
  "Mark a schema as rendering an input field"
  [schema]
  (if (m/schema? schema)
    (mu/update-properties schema update ::spec
                          #(-> % ;; unless renders children, mark as render
                               (assoc :render? (not (children-render (m/type schema))))
                               add-path-info))
    schema))

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
                                     (assoc :required (not (:optional props))
                                            ;; TODO
                                            :default (:default props)
                                            :render? (not (children-render (m/type cschema))))))])
       (m/-set-children schema)))

(defn add-field-specs
  "Postwalk schema, calling `build-field-spec` with the schema, its properties,
  and the properties of its children, and adding the output to the schema
  properties."
  ([schema]
   ;; TODO: remove when not doing active dev
   (add-field-specs schema {:registry registry}))
  ([schema options]
   (let [schema (m/schema schema options)
         base-path (if (m/-ref-schema? schema) [(m/-ref schema)] [])]
     (m/walk
       (deref-subschemas schema options)
       (fn [schema path children _]
         ;(prn schema path children)
         (let [naive-spec (extract-field-spec schema)
               ;; TODO: add schema type under ::m/type field here?
               spec (-> (complete-field-spec
                          schema naive-spec
                          (keep #(when (m/schema? %)
                                   (let [spec (::spec (m/properties %))]
                                     (when-not (:no-spec spec) spec)))
                                children))
                        ;; add path after complete-field-spec in case of
                        ;; accidental override from child specs
                        (assoc :path (into base-path path)))
               children-render? (= ::collection (:type spec))
               ;; root node without rendering children renders
               spec (cond-> spec
                      (and (empty? path) (not children-render?))
                      (-> (assoc :render? true) add-path-info))]
           (-> schema
               (mu/update-properties assoc ::spec spec)
               (set-children
                 (if children-render?
                   (map mark-render children)
                   children)))))
       options))))

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
  ([spec] (labeled-input spec (:value spec))) ;; TODO
  ([{:keys [id label] :as field-spec} value]
   (add-label
     [:input (props->attrs field-spec value)]
     label id)))

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
             ;; TODO: leave stringification up to generator?
             :let [str-val (if (keyword? child)
                             (subs (str child) 1)
                             (str child))
                   sel? (= child value)]]
         [:option
          (cond-> {:value str-val}
            sel? (assoc :selected true))
          (value->label str-val)])]
      label id)))

;; TODO: use some kind of identifiable value like ::placeholder for placeholders
(defn- collection-schema-collector
  ([empty-val] (collection-schema-collector empty-val nil))
  ([empty-val placeholder]
   {:compile (fn [schema _]
               (let [spec (::spec (m/properties schema))]
                 {:enter (fn [value]
                           (let [value (if (nil? value) empty-val value)]
                             (cond
                               (fn? placeholder) (placeholder value)
                               (some? placeholder) (conj value placeholder)
                               :else value)))
                  :leave (fn [value]
                           (prn (m/type schema) spec value)
                           (assoc spec :children (seq value)))}))}))

(def collect-specs
  "Transformer that collects field specs based on input value."
  {:name            :collect-specs
   :default-encoder {:compile (fn [schema _]
                                ;; TODO: parameterize probably
                                ;; maybe store val props in special key on child?
                                (let [props (m/properties schema)
                                      spec (::spec props)
                                      default-kv (find props :default)]
                                  (cond-> nil
                                    default-kv
                                    (assoc :enter (fn [value]
                                                    (if (nil? value) (val default-kv) value)))
                                    (:render? spec)
                                    (assoc :leave
                                           (fn [value] 
                                             (assoc spec :value value))))))}
                                             ;(render-field schema value))))))}
   :encoders  {:map {:compile (fn [schema _]
                                (let [child-keys (map #(nth % 0) (m/children schema))
                                      spec (::spec (m/properties schema))]
                                  {;; on enter, ensure placeholder values are present, as
                                   ;; otherwise transform doesn't recurse to  them
                                   :enter (fn [value]
                                            (if (or (nil? value) (map? value))
                                              (reduce #(update %1 %2 identity) value child-keys)
                                              value))
                                   ;; collect child specs, which are now in the vals
                                   :leave (fn [value]
                                            (if (map? value)
                                              ;; preserve order
                                              ;; TODO: custom order via attributes
                                              (assoc spec :children (map value child-keys))
                                              ;; TODO: then what?
                                              value))}))}
               :map-of  (collection-schema-collector {} #(assoc % nil nil))
               :set     (collection-schema-collector #{} #(conj % nil))}})

(defn collect-field-specs
  "Given a schema, a value, and options, prepare the schema via add-field-specs,
  then encode it with collect-specs into a renderable AST"
  ([schema] (collect-field-specs schema nil))
  ([schema source] (collect-field-specs schema source {}))
  ([schema source options]
   (-> (add-field-specs schema options)
       (m/encode source options (mt/transformer collect-specs)))))


;(def render-fields
;  "It's very important that the functions here are on :leave, not :enter."
;  {:name :render-fields
;   :default-encoder {:compile (fn [schema _]
;                                {:leave 
;                                 (let [spec (::spec (m/properties schema))]
;                                   (when (and (:render? spec)
;                                              (not (children-render (m/type schema))))
;                                     (partial labeled-input spec)))})}
;   :encoders {:map {:leave (fn [child-specs]
;                             (into [:fieldset] (interpose [:br]) child-specs))}
;              :map-of {:compile (fn [schema _]
;                                  (let [path (-> schema m/properties ::spec :path)]
;                                    {:leave (fn [pair-specs]
;                                              [:fieldset
;                                               [:legend (path->label path)]
;                                               (map #(vector :fieldset %) pair-specs)])}))}}})


(defn- collection-schema-transformer
  [empty-val add-nil?]
  {:compile (fn [schema _]
              (let [path (-> schema m/properties ::spec :path)]
                {:enter (fn [value]
                          (if (nil? value)
                            empty-val
                            (if add-nil?
                              (conj value nil)
                              value)))
                 :leave (fn [value]
                          [:fieldset
                           [:legend (path->label path)]
                           (seq value)])}))})

(def render-fields
  "Transformer that renders field specs into hiccup markup"
  ;(let [render-
  {:name :render-fields
   ;:default-encoder identity
   ;; can actually override by settings {:encode/render-field {:compile (fn [schema _] ....
   ;; on schema
   :default-encoder {:compile (fn [schema _]
                                ;; TODO: parameterize probably
                                ;; maybe store val props in special key on child?
                                (let [props (m/properties schema)
                                      default-kv (find props :default)]
                                  (cond-> nil
                                    default-kv
                                    (assoc :enter (fn [value]
                                                    (if (nil? value) (val default-kv) value)))
                                    (:render? (::spec props))
                                    (assoc :leave (fn [value] (render-field schema value))))))}
   :encoders {:set  (collection-schema-transformer #{nil} true)
              :map  {:compile (fn [schema _]
                                (let [child-keys (map #(nth % 0) (m/children schema))]
                                  {:enter (fn [value]
                                            ;; if child keys aren't present, transform doesn't recurse to them
                                            (if (or (nil? value) (map? value))
                                              (reduce #(update %1 %2 identity) value child-keys)
                                              value))
                                   :leave (fn [value]
                                            ;; preserve order
                                            ;; things like (interpost [:br]) need to happen during actual render
                                            (into [:fieldset] (map value) child-keys))}))}
              :map-of {:compile (fn [schema _]
                                  (prn schema)
                                  (let [spec (::spec (m/properties schema))]
                                    {:enter #(or % {nil nil})
                                     :leave (when (:render? spec)
                                              (fn [value]
                                                [:fieldset
                                                 [:legend (path->label (:path spec))]
                                                 (map #(cons :fieldset %) value)]))}))}
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
  ([schema] (encode-fields schema nil))
  ([schema source] (encode-fields schema source {}))
  ([schema source options]
   (-> (add-field-specs schema options)
       (m/encode source options (mt/transformer render-fields)))))

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
