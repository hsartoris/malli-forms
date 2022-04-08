(ns malli-forms
  (:require
    [malli-forms.render.table :as table]
    [malli-forms.util :as util :refer [default
                                       path->name
                                       unqualify]]
    [malli.core :as m]
    [malli.error :as me]
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

(def ^:private base-data-key
  "Key under which actual schema data will be placed in form"
  "data")

(def ^:private base-options-key
  "Key under which options for the handler will be stored in form"
  "mf-options")

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
    [:order {:optional true
             :doc "Used when collection is of type map"} sequential?]
    [:children [:sequential [:ref ::field-spec]]]]

   ::field-spec.form
   [:map {:doc "Field spec for a root element"}
    [:path ::path]
    [:children [:sequential [:ref ::field-spec]]]]

   ::field-spec
   [:multi {:dispatch :type}
    [::collection ::field-spec.collection]
    [::form       ::field-spec.form]
    [::m/default  ::field-spec.input]]

   ::options
   [:map {:doc "Options available for various functions in this library"}
    [::placeholder-target
     {:doc      "When set, should match name of an input that can have a placeholder value added"
      :optional true}
     string?]
    [::auto-placeholder
     {:doc      "When true, all schema items that are able will have placeholder values added"
      :default  true}
     boolean?]
    [::add-placeholder-inputs
     {:doc      "Should input buttons be added that will trigger ::placeholder-target"
      :default  true}
     boolean?]
    [::render
     {:doc      "Function to be used to render field specs."
      :default  table/render}
     [:=> [:cat [:ref ::field-spec]] :any]]
    [::validate
     {:doc      "Should the parser also attempt to validate the provided data"
      :default  true}
     boolean?]
    [::ignore-form-options
     {:doc      "Should handle-submit skip merging options values from the form"
      :default  false}
     boolean?]]

   ::form
   [:map {:doc "Expected shape of data returned from form"}
    [base-data-key {:doc "Where the provided schema will be placed"} :any]
    [base-options-key
     {:doc "Map of options to forms handler"
      :optional true}
     [:ref ::options]]]})

(def registry
  "malli registry for this project"
  (mr/composite-registry
    (m/default-schemas)
    (mu/schemas)
    local-registry))

(def ^:private ring-anti-forgery
  "When ring.middleware.anti-forgery is available, will be bound to
  #'ring.middleware.anti-forgery/*anti-forgery-token"
  (try
    (requiring-resolve 'ring.middleware.anti-forgery/*anti-forgery-token*)
    (catch Exception _)))

(defn- wrap-schema
  "Wrap provided schema into a map that matches ::form schema"
  [schema]
  [:map {::type   ::form
         ::method "POST"}
   ;; TODO: test
   ["__anti-forgery-token" {:optional true}
    [:string {::type :hidden
              ::finalize (fn [spec]
                           (when ring-anti-forgery
                             (assoc spec :value (deref ring-anti-forgery))))}]]
   [base-data-key schema]
   ["submit" {:optional true}
    [:string {::type :submit
              :default "Submit"}]]])

(def field-spec-schema
  "Schema for a field spec"
  (m/deref (m/schema ::field-spec {:registry registry})))

(def ^:private key-transformer
  "mt/key-transformer, equipped with a url-decoding step before keyword"
  ;; TODO: can do better than this. Will need to cover map-of as well
  (mt/key-transformer {:decode #(-> % util/url-decode keyword)}))

(def ^:private default-options
  "Decoder function that will apply default values to an ::options map"
  (m/decoder (m/schema ::options {:registry registry})
             (mt/transformer
               mt/default-value-transformer
               (mt/strip-extra-keys-transformer))))

(def ^:private decode-options
  "Decoder function that invokes a transformer stack suitable for parsing
  options from form input; does not apply defaults"
  (m/decoder (m/schema ::options {:registry registry})
             (mt/transformer
               key-transformer
               (mt/string-transformer) ;; includes mt/json-transformer, basically
               (mt/strip-extra-keys-transformer))))


(defn- add-path-info
  "Add name, id, and label to a spec, based on a path already added to it."
  ;Also updates path to prepend base-data-key - do not run multiple times!"
  [spec]
  (let [;{:keys [path] :as spec} (update spec :path #(cons base-data-key %))
        path (:path spec)
        input-name (or (:name spec) (path->name path))
        spec (assoc spec :name input-name)]
    (-> spec
        ;; TODO: probably gensym for ids
        (default :id    (str "mf-" input-name))
        (default :label (util/label spec)))))

(def ^:private collection?
  "Is a type a collection type? That is, either ::collection or ::form"
  #{::form ::collection})

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
               ;; TODO: false? is sort of nonsensical
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
  (let [mtype (m/type schema)
        input-type (schema-type->input-type mtype)]
    (into (cond-> {::m/type mtype}
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
  (->> children schemas->specs util/intersect-maps (into spec)))
(defmethod complete-field-spec :and
  [_ spec children]
  (->> children schemas->specs util/intersect-maps (into spec)))

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

;; ---- string schema handling
(defn- add-min-max
  "Add minlength and maxlength when min/max provided on string schema"
  [schema spec]
  (let [#_:clj-kondo/ignore {:keys [min max]} (m/properties schema)]
    (cond-> spec
      min (assoc :minlength min)
      max (assoc :maxlength max))))

(defmethod complete-field-spec :string
  [schema spec _]
  (add-min-max schema spec))
(defmethod complete-field-spec 'string?
  [schema spec _]
  (add-min-max schema spec))

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

;; TODO TODO:
;; would be nice to be able to apply attrs on map vals, not just on the
;; child schemas.
;; failing this, there at least needs to be a BIG WARNING

;; TODO: do I even need to keep track of real path here? just gets overwritten anyway...

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
     ;; TODO TODO
     ;; TODO: use mu/path->in to remove need for all this shenanigans
     ;; - use-child marks parent as no render, child as render
     (letfn [(inner* [walker schema path {::keys [parent] :as options}]
               (let [;; TODO: elsewhere
                     no-render-children #{:and :andn :or :orn}
                     no-add-path (conj no-render-children :maybe ::m/schema :schema :ref ::m/val)
                     add-to-path (not (or (no-add-path parent)
                                          (contains? options ::use-child)))
                     child-idx (::use-child (m/properties schema))
                     stype (m/type schema)
                     naive-spec (extract-field-spec schema)
                     root? (= path base-path)
                     ptype (schema-type->input-type parent)
                     ;; TODO: simplify all these crazy conditionals
                     render? (and (not (collection? (:type naive-spec)))
                                  ;; if render? is flagged in spec, respect it
                                  (or (not (contains? naive-spec :render?))
                                      (:render? naive-spec))
                                  (or root?
                                      (collection? ptype)
                                      ;(= ptype ::collection)
                                      ;; TODO: can remove this right?
                                      ;(= parent :map)
                                      (and (::render? options)
                                           (not (no-render-children parent)))))
                     ;; should a label be eventually generated for this item?
                     label?  (and (or ;(not= ::collection ptype)
                                      (not (collection? ptype))
                                      ;; TODO: might actually be more convenient to use ::m/val here
                                      (= :map parent))
                                  ;; TODO: kinda sketch
                                  (not= ::m/in (peek (::path options))))
                     reqd? (and render?
                                (not= stype :maybe) ;; TODO: good approach?
                                (if (= parent :map)
                                  ;; val schema - use optional if set
                                  (not (:optional (m/properties schema)))
                                  ;; otherwise just default to root is required
                                  (or root? (::required options))))]
                 (->> (-> (dissoc options ::use-child)
                          (assoc ::parent stype
                                 ::naive-spec (cond-> naive-spec
                                                render? (assoc :render? true)
                                                reqd?   (assoc :required true)
                                                label?  (assoc :label? true))
                                 ;; TODO: replace with just referencing naive parent spec
                                 ::render?  render?
                                 ::required reqd?)
                          (cond->
                            child-idx
                            (assoc ::use-child child-idx)

                            (and add-to-path (seq path))
                            (update ::path conj (peek path))))
                      (m/-walk schema walker path))))
                                
             (outer* [schema _abs-path children
                      {::keys [use-child path] :as options}]
               (cond
                 use-child (nth children use-child)

                 (#{:maybe :schema ::m/schema :ref ::m/val} (m/type schema))
                 (first children)

                 :else
                 ;; otherwise...
                 (let [naive-spec (::naive-spec options)
                       finalize (:finalize naive-spec identity)
                       spec (-> (complete-field-spec schema naive-spec children)
                                ;; add path after complete-field-spec in case of
                                ;; accidental override from child specs
                                (assoc :path path))
                       spec (cond-> spec
                              ;; TODO: better ordering control
                              (= :map (::m/type spec))
                              (assoc :order (map #(nth % 0) children)))]
                   ;; TODO: remove children marked no-spec and send back to inner if any
                   (-> schema
                       (mu/update-properties assoc ::spec (finalize spec))
                       (m/-set-children children)))))]
       (m/-inner
         (reify m/Walker
           (-accept [_ schema _ _] schema)
           (-inner [this schema path options]
             (inner* this schema path options))
           (-outer [_ schema path children options]
             (outer* schema path children options)))
         (m/schema schema options)
         base-path
         (assoc options
                ::path base-path
                ::m/walk-refs true
                ::m/walk-entry-vals true
                ::m/walk-schema-refs true))))))

(defn- placeholder-target?
  "Pass a schema and an options map; checks if the path specified by the
  schema's spec matches the placeholder-target in the options map"
  [schema {::keys [placeholder-target]}]
  (= placeholder-target (-> schema m/properties ::spec :path path->name)))

(def ensure-required-keys
  "Transformer that ensures that required keys are present in maps by adding
  nil values when missing."
  {:name :ensure-required-keys
   :encoders {:map {:compile (fn [schema _]
                               (let [child-keys (map #(nth % 0) (m/children schema))]
                                 {;; on enter, ensure placeholder values are present, as
                                  ;; otherwise transform doesn't recurse to  them
                                  :enter (fn [value]
                                           (if (or (nil? value) (map? value))
                                             (reduce #(update %1 %2 identity) value child-keys)
                                             value))}))}}})

;; TODO: use some kind of identifiable value like ::placeholder for placeholders

;; TODO TODO: I think this is the only place that actually uses the :path
;; set by add-field-specs. If it could be straightforwardly restricted to this
;; use case, add-field-specs could potentially be much simpler
(def add-placeholders
  "Transformer that adds placeholder values by schema type"
  {:name :add-placeholders
   :encoders {:set {:compile (fn [schema options]
                               (cond
                                 (placeholder-target? schema options)
                                 #((fnil conj #{}) % nil)

                                 (::auto-placeholder options)
                                 #(or % #{nil})))}
              :map-of {:compile (fn [schema options]
                                  (cond
                                    (placeholder-target? schema options)
                                    #(assoc % nil nil)
                                    (::auto-placeholder options)
                                    #(or % {nil nil})))}}})

(def ^:private may-placeholder
  "Malli types that support adding a placeholder value"
  (set (keys (:encoders add-placeholders))))

;; ------ schema to AST based on real values ------

(def ^:private prep-schema
  "Memoized version of add-field-specs"
  (memoize add-field-specs))

(def ^:private encoder
  "Memoized version of m/encoder"
  (memoize m/encoder))

(defn- index-specs
  "Produce an index into the given schema, consisting of nested maps whose keys
  are elements in a schema :in path, or the special key ::spec, whose value
  will be the spec corresponding with the schema at that path"
  [schema]
  (reduce
    (fn [index {:keys [in schema]}]
      (assoc-in index (conj in ::spec) (::spec (m/properties schema))))
    {}
    (mu/subschemas schema)))

(defn- index-errors
  "Provide a seq of errors as produced by m/explain, yielding a map of path to
  errors for that path, where each error will have a :message field from
  me/error-message. Adds provided base-level key to path."
  [errors]
  (reduce (fn [index {:keys [in] :as error}]
            (update index in (fnil conj []) (assoc error :message (me/error-message error))))
          {}
          errors))

(defn collect-field-specs
  "Given a schema, a value, optional errors, and options, encode the value
  according to the schema, then value the transformed value, turning it into a
  ::field-spec AST"
  ([schema]
   (collect-field-specs schema nil))
  ([schema source]
   (collect-field-specs schema source {}))
  ([schema source options]
   (collect-field-specs schema source nil options))
  ([schema source errors {::keys [add-placeholder-inputs] :as options}]
   (let [encode (m/encoder schema
                           options
                           (mt/transformer
                             ensure-required-keys
                             add-placeholders
                             mt/default-value-transformer))
         indexed-specs (index-specs schema)
         ;; TODO: figure out why this is necessary again
         ;; specifically, the some [head ::m/in] that prevents this just being
         ;; a regular get-in
         path->spec (fn [path]
                      (loop [cursor indexed-specs
                             [head & tail :as path] path]
                        (if (empty? path)
                          (get cursor ::spec)
                          (some-> (some cursor [head ::m/in])
                                  (recur tail)))))
         path->errors (index-errors errors)]
     ;; TODO: problem: error paths are in terms of the decoded data, before
     ;; any placeholder application (though not default values, as they're
     ;; applied in the parse stack as well). Really, this means that errors
     ;; should be retrieved *before* placeholder application, but it is also
     ;; necessary to apply placeholders
     (util/pathwalk
       (fn [item path]
         (let [errors (path->errors path)
               spec (cond-> (path->spec path)
                      (some? errors) (assoc :errors errors))
               mtype (::m/type spec)
               stype (:type spec)
               children (cond
                          (= :map mtype)          (map item (:order spec))
                          (collection? stype)     (seq item))
               children (cond-> children
                          (and add-placeholder-inputs (may-placeholder mtype))
                          ;; TODO: awkward
                          (some-> vec
                                  (conj
                                    {:type    :submit
                                     :name    (path->name [base-options-key
                                                           ::placeholder-target])
                                     :onclick "this.closest('form').noValidate=true;"
                                     :value   (path->name path)
                                     :label   "+"})))]
           (cond
             (collection? stype) ;; also catches mtype :map
             (assoc spec :children children)

             (:render? spec)
             (-> spec (assoc :value item, :path path) add-path-info))))
       (encode source)))))


;; ------ rendering AST to markup ------

(defn render-specs
  "Given a value as produced by collect-field-specs and options, renders fields
  defined by AST into markup"
  ([source] (render-specs source {}))
  ([source {::keys [render] :as options}]
   (m/encode field-spec-schema source options
             (mt/transformer
               {:name :render-specs
                :encoders {:map {:leave #(some-> % render)}}}))))

(defn render-form
  "Renders a form based on a schema. Accepts various optional arguments:
    - source: initial data to seed form with. default: nil
    - options: malli options map. see options-schema for custom keys. default: {}
    - errors: seq of errors produced by m/explain

  Rendering a form consists of the following steps:
    1. Provided schema is wrapped in a map, under the key base-data-key. This
      allows for consistent handling of schemas that aren't maps, as well as
      allowing for storing other data in the form, such as an anti-forgery
      token, or signals to this library to help with adding placeholder values.
    2. Wrapped schema is run through add-field-specs, which walks the entire
      schema, adding path information and calculating various attributes, such
      as whether or not the given item should render as a field, input type,
      etc., based on the schema and sometimes its children.
    3. Source data is wrapped in a map under base-data-key (TODO TODO), and
      transformed according to the prepared schema, ensuring that required map
      keys are present, adding default values, and adding placeholders based on
      the provided options; i.e., either in all possible cases, or to a
      particular field.
    4. The transformed data is walked, using the path to each location in the
      data to locate a relevant field spec, adding any errors, and outputting
      nil, the field spec, or a collection field spec with the original value
      under the key :children; at this point, the output is an AST of the type
      defined in the local registry as ::field-spec.
    5. The AST is transformed by calling the function under ::render in the
      options (default: malli-forms.render.table/render) with each node. A good
      option for this function is a multimethod dispatching on :type; it should
      return something representing the HTML-encoded form, be it hiccup, or a
      String directly; depends on what your rendering pipeline looks like."
  ([schema] (render-form schema nil))
  ([schema source] (render-form schema source {}))
  ([schema source options] (render-form schema source nil options))
  ([schema source errors options]
   (let [options (default-options options)]
     (-> (wrap-schema schema) ;; 1
         (prep-schema options) ;; 2
         (collect-field-specs {base-data-key source}
                              (map (fn [error]
                                     (update error :in #(cons base-data-key %)))
                                   errors)
                              options)
         (render-specs options)))))

;; ------ parsing ------

(defn- unnest-seq
  "See unnest-seq-transformer"
  [value]
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
                     (for [stype (remove #{:map :map-of}
                                         (::collection schema-type-by-input-type))]
                       {stype unnest-seq}))]
    {:name      :unnest-seq
     :encoders  coders
     :decoders  coders}))

(def ^:private decoder
  "Memoized m/decoder"
  (memoize m/decoder))

(def parse-stack
  "Transformer stack for parsing input data"
  [key-transformer
   mt/default-value-transformer ;; TODO: options for this
   ensure-required-keys
   unnest-seq-transformer
   (mt/string-transformer) ;; includes mt/json-transformer, basically
   (mt/strip-extra-keys-transformer)])

(def ^:private parse-transformer
  (apply mt/transformer parse-stack))

(defrecord ParseFailure
  [schema value errors  ;; these three from m/explain
   options source       ;; these from provided input
   form]) ;; form is a delay that yields the form with errors

(defn parse-failed?
  "Is x an instance of ParseFailure"
  [x]
  (instance? ParseFailure x))

(defn parse
  "Simple parse using schema against data. Throws on failure.
  data should be as returned by ring's nested-params middleware, but should NOT
  be wrapped in the containing map - that is, the data should match the schema."
  ([schema data] (parse schema data {}))
  ([schema data {::keys [validate] :as options}]
   (let [decode (m/decoder schema options parse-transformer)
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
       (-> error
           (assoc :source data
                  :options options)
           map->ParseFailure)
       {:schema schema
        :value  decoded}))))

(defn handle-submit
  "Provide original schema and data as returned by ring-nested-params directly;
  that is, a map containing the desired value to match against `schema` under
  base-data-key.
  Updates options based on form data, unless ::ignore-form-options is set.
  Returns a map containing :value, the desired parsed value, :schema, the
  provided schema, and :form, a delay that, when realized, re-renders the form
  with the new value and any parse errors encountered. In the scenario that
  parse errors are encountered, they will be included under :errors."
  ([schema raw-data] (handle-submit schema raw-data {}))
  ([schema raw-data options]
   (println "Parsing raw data" raw-data)
   (let [options (default-options options)
         ;; TODO: handle potential failure to decode options
         options (cond-> options
                   (not (::ignore-form-options options))
                   (conj (decode-options (get raw-data base-options-key))))
         _ (println "Final options:" options)
         data (get raw-data base-data-key)
         parsed (parse schema data options)]
     (assoc parsed :form (delay (render-form schema
                                             (:value parsed)
                                             (:errors parsed)
                                             options))))))

;; remaining:
;; - [ ] Field specs:
;;    - [ ] clean up schema to match reality
;;    - [X] strip internal keys before rendering
;;      - see util/internal-attrs
;;    - [X] store malli type for utility
;; [X] format errors into rendered form
;; [X] attempt parse macro
;;  - just function
;; [X] attempt parse=>re-render form macro built on previous
;;  - part of above
;; [ ] simple, configurable middleware to parse
;; [ ] optional reitit coercion module
;; [ ] ring-anti-forgery
