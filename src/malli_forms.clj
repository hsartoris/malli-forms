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
   ::prefix
   ::suffix
   ;; TODO: will/should this appear?
   ::render?
   ::placeholder
   ::placeholder-fn])

(comment
  (def s1 [:map [:a string?] [:b int?]])
  (def s2 [:merge s1 ::field-spec])
  (def s3 [:select-keys s1 [:a]]))

(def ^:private base-data-key
  "Key under which actual schema data will be placed in form"
  "data")

(def ^:private base-flags-key
  "Key under which flags for the handler will be stored in form"
  "mf-flags")

(def ^:private simple-conformer
  "Transformer that applies default values and conforms collections"
  (mt/transformer (mt/collection-transformer) mt/default-value-transformer))

(def parseable
  "Schema types that are subject to m/parse and m/unparse"
  #{:orn :catn :altn :multi})

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
   ::prefix       [string? {:doc "Value a given input field should be prefixed with when rendered"}]
   ::suffix       [string? {:doc "Value a given input field should be suffixed with when rendered"}]
   ;; TODO
   ::attributes   [:map-of :keyword :any]
   ::render?      [boolean?
                   {:doc "When true, this field spec should be preserved as it is ready to render"}]
   ::path         [:sequential :any]
   ::placeholder-fn [fn?
                     {:doc "1-arity function that, when called with a collection,
                           adds a placeholder value to that collection. Be prepared
                           for the inbound value to be nil."}]
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
    [:type        {:optional true
                   :doc "HTML form element type, with some exceptions"} ::type]
    [::m/type     {:doc "Type of malli schema that this input is representing"} any?]
    [:value       {:optional true} any?]
    [:attributes  {:optional true} ::attributes]
    [:placeholder {:optional true} string?]
    [:prefix      {:optional true} ::prefix]
    [:suffix      {:optional true} ::suffix]
    [:required    {:optional true} boolean?]
    [:render?     {:optional true} ::render?]]

   ::field-spec.group
   [:map {:doc "Field spec for a group of closely-related specs.
               May be rendered differently from a collection."}
    [:path ::path]
    [:children [:sequential [:ref ::field-spec]]]]

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
   [:multi {:dispatch :type
            :encode {::render-spec {:compile (fn [_schema {::keys [render]}]
                                               {:leave #(some-> % render)})}}}
    [::group      ::field-spec.group]
    [::collection ::field-spec.collection]
    [::form       ::field-spec.form]
    [:select {:encode
              {::finalize-spec
               {:compile (fn [schema _]
                           (let [conform (m/decoder schema simple-conformer)]
                             {:leave (fn [spec]
                                       (let [{:keys [value placeholder options] :as spec} (conform spec)
                                             selected (first (keep (fn [[idx option]]
                                                                     (when (= value (:value option))
                                                                       idx))
                                                                   (map-indexed vector options)))
                                             opt1 {:label placeholder
                                                   :value ""
                                                   :disabled (:required spec)
                                                   :selected (nil? selected)}]
                                         (assoc spec :options (into [opt1]
                                                                    (cond-> options
                                                                      selected (update selected assoc :selected true))))))}))}}}
     [:merge
      [:ref ::field-spec.input]
      [:map
       ;; not optional here, as opposed to typical definition
       [:placeholder [:string {:default "Select an option"}]]
       [:options [:vector [:map
                           [:value any?]
                           [:label {:optional true} string?]]]]]]]
    [:checkbox {:encode
                {::finalize-spec
                 ;; checkboxes don't use 'value' in the same way other inputs do
                 ;; here, we set 'value' to what should be sent *if the box is checked*
                 ;; and set 'checked' to reflect what was provided in value
                 ;; this logic differs between :boolean, boolean, true?, vs false?
                 {:leave (fn [spec]
                           (case (::m/type spec)
                             (:boolean boolean? true?)
                             (assoc spec
                                    :value "true" ;; what will be sent to backend if checked
                                    :checked (boolean (:value spec))) ;; whether or not checked
                             ;; use cases?
                             ;; "false" will be sent when the box is checked; we check when value is false
                             false?
                             (do (println "You'd better have a good label explaining how this false? schema works")
                                 (assoc spec :value "false" :checked (not (:value spec))))))}}}

     ::field-spec.input]
    [::m/default  ::field-spec.input]]

   ::options
   [:map {:doc "Options available for various functions in this library"}
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

   ::form-flags
   [:map {:doc "Flags that can be sent during form submission, changing the behavior of parsing"}
    [::placeholder-target
     {:doc      "When set, should match name of an input that can have a placeholder value added"
      :optional true}
     string?]
    [::removal-target
     {:doc      "When set, should match name of an input that should be removed from its parent collection"
      :optional true}
     string?]
    [::selected-leaves
     {:doc      "Notes which leaves have been selected in such schemas as need them"
      :optional true}
     [:map-of 
      string?
      ;; TODO: figure out why this is necessary
      ;[:string {:decode/string util/url-decode}]
      string?]]]

   ::options+flags [:merge [:ref ::options] [:ref ::form-flags]]

   ::form
   [:map {:doc "Expected shape of data returned from form"}
    [base-data-key {:doc "Where the provided schema will be placed"} :any]
    [base-flags-key
     {:doc "Map of flags embedded in form to control handler"
      :optional true}
     [:ref ::form-flags]]]})

(derive ::group ::collection) ;; default handling for groups

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
  ;; TODO: copy any form parameters from schema
  [:map {::type   ::form
         ::method "POST"}
   ;; TODO: test
   ["__anti-forgery-token" {:optional true}
    [:string {::type :hidden
              :encode {::anti-forgery (when ring-anti-forgery
                                        (fn [_] (deref ring-anti-forgery)))}}]]
   [base-data-key schema]
   ["submit" {:optional true}
    [:string {::type :submit
              :default "Submit"}]]])

(def field-spec-schema
  "Schema for a field spec"
  (m/deref (m/schema ::field-spec {:registry registry})))

(def ^:private collection?
  "Is a type a collection type? That is, either ::collection or ::form"
  #{::form ::collection ::group})

(def ^:private no-label-children
  "Malli types whose children should not be labeled"
  ;; TODO: exhaustive?
  #{:sequential :vector :set :tuple})

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
               :re
               ;; TODO: unsure
               :=]
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
    ;; TODO: replace this with fieldset, probably
    ;; though it would be nice to take advantage of derivation for multimethods - ::fieldset ?
    ::collection [:map :map-of
                  :sequential :vector :set
                  ;; really big TODO
                  :+ :*
                  :repeat
                  :cat :catn]
    ::group     [:tuple]})

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

(defmethod complete-field-spec :map
  [_ spec children]
  (assoc spec :order (map #(nth % 0) children)))

(defmethod complete-field-spec :or
  [_ spec children]
  (->> children schemas->specs util/intersect-maps (into spec)))
(defmethod complete-field-spec :and
  [_ spec children]
  (->> children schemas->specs util/intersect-maps (into spec)))

;; TODO
(defmethod complete-field-spec :orn
  [_ spec children]
  (let [child-keys (map #(nth % 0) children)
        child-specs (schemas->specs (map #(nth % 2) children))]
    (if (apply = (map :type child-specs))
      (-> (assoc spec ::m/type :or) ;; TODO: good idea?
          (into (util/intersect-maps child-specs)))
      ;; TODO: steal code from enum to apply labels to options
      (assoc spec
             :key-decoder (util/generous-decoder child-keys)
             :keys child-keys))))

(defmethod complete-field-spec :=
  [_ spec [target-value]]
  ;; TODO: good idea?
  (assoc spec :value target-value))

;; TODO: not sure if this should share semantics with orn
(defmethod complete-field-spec :andn [_ _ _])

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
  [_ spec _]
  ;; leave room for the possibility that user supplies a spec on a fn schema
  (if (seq spec) spec {:no-spec true}))

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
  [_ {:keys [labels] :as spec} children]
  ;; children of an enum are literal values
  ;; if vector of labels, treat as one-to-one with children
  (when (sequential? labels)
    (assert (= (count labels) (count children))
            "Sequence of enum labels must match count of children"))
  (assoc spec :options
         (into []
               (map (fn [[child-val label]]
                      {:value child-val
                       :label (or label (util/value->label child-val))}))
               (if (vector? labels)
                 (zipmap children labels)
                 (for [child-val children]
                   [child-val (get labels child-val)])))))

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
  Does not meaningfully transform schema except in wrapping the values of
  optional keys in :maybe

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
                     ;no-render-children #{:and :andn :or :orn}
                     ;; orn children do render, but selectively
                     no-render-children #{:and :andn :or}
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
                                      (and (::render? options)
                                           (not (no-render-children parent)))))
                     reqd? (and render?
                                (not= stype :maybe) ;; TODO: good approach?
                                (if (= parent :map)
                                  ;; val schema - use optional if set
                                  (not (:optional (m/properties schema)))
                                  ;; otherwise just default to root is required
                                  (or root? (::required options))))]
                 (->> (-> (dissoc options ::use-child)
                          (assoc ::parent stype ;; pass to children
                                 ::naive-spec (cond-> (assoc naive-spec :parent parent)
                                                render? (assoc :render? true)
                                                reqd?   (assoc :required true))
                                 ;; TODO: replace with just referencing naive parent spec
                                 ::render?  render?
                                 ::required reqd?)
                          (cond->
                            child-idx
                            (assoc ::use-child child-idx)))
                      (m/-walk schema walker path))))
                                
             (outer* [schema _abs-path children {::keys [use-child] :as options}]
               (cond
                 use-child (nth children use-child)

                 (#{:maybe :schema ::m/schema :ref ::m/val} (m/type schema))
                 (first children)

                 :else
                 ;; otherwise...
                 (let [naive-spec (::naive-spec options)
                       spec (complete-field-spec schema naive-spec children)]
                   ;; TODO: remove children marked no-spec and send back to inner if any
                   (-> schema
                       (mu/update-properties assoc ::spec spec)
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

;; ------ Transformers ------

(def ^:private xf-anti-forgery
  "Transformer that adds value for __anti-forgery-token. Should NOT be run when
  parsing a user-provided value, as it may erroneously add a valid token."
  (mt/transformer {:name ::anti-forgery}))

(def ^:private key-transformer
  "Like mt/key-transformer, but way, way better"
  (let [coders {:map {:compile (fn [schema _]
                                 (let [ks (map #(nth % 0) (m/children schema))
                                       decoder (util/generous-decoder ks)]
                                   (fn [value]
                                     (into {}
                                           (map (fn [[k v]] [(decoder k) v]))
                                           value))))}
                :enum {:compile (fn [schema _]
                                  (let [decoder (util/generous-decoder (m/children schema))]
                                    #(decoder %)))}
                :map-of #(update-keys % util/url-decode)}]
    {:name :key-transformer
     :encoders coders
     :decoders coders}))


(def ensure-map-keys
  "Transformer that ensures that required and optional keys are present in maps
  by adding nil values when missing."
  (let [coders {:map
                {:compile
                 (fn [schema _]
                   (let [child-keys (map #(nth % 0) (m/children schema))]
                     ;; on enter, ensure placeholder values are present, as
                     ;; otherwise transform doesn't recurse to  them
                     (fn [value]
                       (if (or (nil? value) (map? value))
                         (reduce #(update %1 %2 identity) value child-keys)
                         value))))}}]
    {:name :ensure-map-keys
     :encoders coders
     :decoders coders}))

(def remove-optional-nils
  "Transformer that removes keys with nil values from maps when those values are
  optional."
  (let [coders {:map
                {:compile
                 ;; TODO: configurable placeholder removal
                 (fn [schema _]
                   (let [optionals (keep #(let [[k props _schema] %]
                                            (when (:optional props)
                                              k))
                                         (m/children schema))]
                     (fn [value]
                       (reduce (fn [m k]
                                 (if (get m k) m (dissoc m k))) value optionals))))}}]
    {:name ::remove-optional-nils
     :encoders coders
     :decoders coders}))
                 

;; TODO: use some kind of identifiable value like ::placeholder for placeholder

(def ^:private auto-placeholder-fns
  "auto-placeholder functions by malli type. differs from placeholder-fns in
  that these return a collection with a placeholder value when no value exists
  for the collection on the whole"
  {:set     #(or % #{nil})
   :map-of  #(or % {nil nil})})

(def auto-placeholder
  "Transformer that adds placeholder values by schema type"
  (let [coders (-> auto-placeholder-fns
                   (update-vals (fn [placeholder-fn]
                                  {:compile (fn [_ options]
                                              (when (::auto-placeholder options)
                                                placeholder-fn))}))
                   (assoc := {:compile (fn [schema options]
                                         (when (::auto-placeholder options)
                                           (let [target-value (-> schema m/children first)]
                                             #(or % target-value))))}
                          :tuple {:compile (fn [schema options]
                                             (when (::auto-placeholder options)
                                               (let [placeholder (into [] (repeat (count (m/children schema)) nil))]
                                                 #(or % placeholder))))}))]
    {:name :add-placeholders
     :encoders coders
     :decoders coders}))

(def ^:private xf-placeholders+defaults
  "Transformer that applies placeholders and default values"
  (mt/transformer
    ensure-map-keys
    auto-placeholder
    mt/default-value-transformer))

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

;; TODO: transformer that does what the silly middleware in malli-forms.reitit-test is doing by stripping out empty values
(def parse-stack
  "Transformer stack for parsing input data"
  [
   xf-placeholders+defaults
   remove-optional-nils
   unnest-seq-transformer
   (mt/string-transformer) ;; includes mt/json-transformer, basically
   key-transformer
   (mt/strip-extra-keys-transformer)])

(def ^:private parse-transformer
  (apply mt/transformer parse-stack))


;; ------ End transformers ------

(def ^:private placeholder-fns
  "functions that add a placeholder value to a collection, by malli type.
  differs from auto-placeholder-fns in that these also update an existing
  collection to add a new placeholder value within it"
  {:set     #((fnil conj #{}) % nil)
   :map-of  #(assoc % nil nil)})

(def ^:private may-placeholder
  "set of malli types of collections that support having a placeholder value added"
  (set (keys placeholder-fns)))

(def ^:private default-options
  "Decoder function that will apply default values to an ::options map"
  (m/decoder (m/schema ::options+flags {:registry registry})
             (mt/transformer
               mt/default-value-transformer
               (mt/strip-extra-keys-transformer))))

(def ^:private decode-flags
  "Decoder function that invokes a transformer stack suitable for parsing
  flags from form input; does not apply defaults"
  (m/decoder (m/schema ::form-flags {:registry registry})
             (mt/transformer
               key-transformer
               (mt/string-transformer) ;; includes mt/json-transformer, basically
               (mt/strip-extra-keys-transformer))))

;; ------ schema to AST based on real values ------

(def ^:private prep-schema
  "Memoized version of add-field-specs"
  (memoize add-field-specs))

(def ^:private encoder
  "Memoized version of m/encoder"
  (memoize m/encoder))

(defn schema-index
  "Provide a schema; receive a function that, when called with a value path (as
  in util/pathwalk), returns the subschema associated with that path"
  [schema]
  (let [index (reduce (fn [idx {:keys [in schema]}]
                        (assoc-in idx (conj in ::schema) schema))
                      {} (mu/subschemas schema))]
    (fn [path]
      (loop [cursor index
             [head & tail :as path] path]
        (if (empty? path)
          (get cursor ::schema)
          (some-> (some cursor [head ::m/in]) (recur tail)))))))

(defn- index-errors
  "Provide a seq of errors as produced by m/explain, yielding a map of path to
  errors for that path, where each error will have a :message field from
  me/error-message. Adds provided base-level key to path."
  [errors]
  (reduce (fn [index {:keys [in] :as error}]
            (update index in (fnil conj []) (assoc error :message (me/error-message error))))
          {}
          errors))


;; TODO: evaluate possibility that having path on field-spec is actually unecessary
(defn- add-path-info
  "Add name, id, and label to a spec based on provided path.
  name and id will be the same."
  [spec path]
  (let [input-name (path->name path)]
    (-> (assoc spec :name input-name, :id input-name, :path path)
        (cond->
          (not (no-label-children (:parent spec)))
          (default :label (util/path->label path))))))

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
  ([schema source errors {::keys [add-placeholder-inputs
                                  placeholder-target removal-target
                                  selected-leaves] :as options}]
   (let [path->schema (memoize (schema-index schema))
         path->errors (index-errors errors)]
     (util/pathwalk
       ;; this is called on an item on the way down the tree
       ;; its only function at the moment is to handle when placeholder values have
       ;; been requested for a given target, before further descent into collection
       (fn inner [item path]
         (let [subschema (path->schema path)
               mtype (m/type subschema)
               spec (-> subschema m/properties ::spec)
               add-placeholder (when (= placeholder-target (path->name path))
                                 (or (:placeholder-fn spec)
                                     (placeholder-fns mtype)))
               item (if-not add-placeholder
                      item
                      ;; here, we transform with xf-placeholders+defaults for
                      ;; the benefit of the newly-added item within the collection
                      ;; currently being considered. it would be preferable for
                      ;; the transform to only take place on the newly added
                      ;; item, but this requires knowing the appropriate schema
                      ;; and is actually impossible in the case of maps - must
                      ;; operate on key, and not just value
                      ((encoder subschema options xf-placeholders+defaults)
                       (add-placeholder item)))
               parsed (when (parseable mtype)
                        (if-let [leaf (some->> (path->name path)
                                               (get selected-leaves)
                                               ((:key-decoder spec)))]
                          ;; this means the user has explicitly switched leaf type - reset item
                          ;; also attempt to provide a placeholder value
                          (->> (m/encode (mu/get subschema leaf) nil options xf-placeholders+defaults)
                               (clojure.lang.MapEntry. leaf))
                          ;; otherwise, attempt to parse the leaf
                          (let [parsed (m/parse subschema item options)]
                            (when-not (= ::m/invalid parsed) parsed))))]
           (when (= :tuple mtype)
             (println "tuple:" spec item parsed))
           ;; even if this is a parseable node, we may end up with the original
           ;; item if parsing fails
           (or parsed item)))
       (fn outer [item path]
         (let [errors (path->errors path)
               subschema (path->schema path)
               spec (cond-> (some-> subschema m/properties ::spec)
                      (some? errors) (assoc :errors errors)
                      :always (assoc :path path))
               mtype (::m/type spec)
               stype (:type spec)
               pname (path->name path)]
           (cond
             ;; this path has been flagged for removal
             (some-> removal-target (= pname)) nil

             ;; schema may have branch nodes, represented as map-entry with
             ;; name of branch as key and item as value
             ;; TODO: validate with schemas with leaf nodes other than orn
             (parseable mtype)
             (let [[branch item] (if (map-entry? item) item [nil item])]
               (add-path-info
                 {:type ::collection
                  ::m/type mtype ;; TODO: does this actually generalize beyond orn?
                  :children [{:type :select
                              :name (path->name [base-flags-key ::selected-leaves pname])
                              :value branch
                              :options (for [k (:keys spec)]
                                         {:value k
                                          ;; cannot select current branch
                                          :disabled (= k branch)
                                          :label (util/value->label k)})}
                             ;; already transformed into a field spec, but with path containing branch
                             ;; replace path and regenerate name, don't label separately from parent
                             (some-> (dissoc item :name) (assoc :label nil) (add-path-info path))]}
                 path))


             (collection? stype) ;; also catches mtype :map
             (let [children (if (= :map mtype)
                              (keep item (:order spec))
                              (filter some? item))
                   children (cond->> children ;; if eligible for placeholder inputs, eligible for removals as well
                              (may-placeholder mtype) 
                              (map (fn [child-spec]
                                     (when-not (:name child-spec)
                                       (println "Child spec with no name:" child-spec))
                                     {:type ::group
                                      :id (str (path->name (:path child-spec)) "-group")
                                      :children
                                      [child-spec
                                       {:type :submit
                                        :name (path->name [base-flags-key ::removal-target])
                                        :onclick "this.closest('form').noValidate=true;"
                                        ;; TODO: strongly suggests need at least name/id on everything
                                        :value (path->name (:path child-spec))
                                        ;; TODO: configurable
                                        :label "-"}]})))
                   children (cond-> children
                              ;; TODO: why did I decide not to add placeholders inputs to empty collections
                              (and add-placeholder-inputs (may-placeholder mtype))
                              (-> vec
                                  (conj
                                    ;; maybe use a custom internal type here
                                    {:type    :submit
                                     :name    (path->name [base-flags-key
                                                           ::placeholder-target])
                                     :onclick "this.closest('form').noValidate=true;"
                                     :value   (path->name path)
                                     ;; TODO: customizable
                                     :label   "+"})))]
               (-> spec (assoc :children children) (add-path-info path)))

             ;; nodes that render are a strict subset of all leaf nodes (within
             ;; the actual data structure, maybe also the schema (?)). must be
             ;; the case because only for leaf nodes will `item` not be a field
             ;; spec (true when this is a parseable node) or a collection of
             ;; field specs (true when this is a collection node)
             (:render? spec)
             (-> (cond-> spec
                   (some? item) (assoc :value item))
                 (add-path-info path)))))
       source))))

;; ------ rendering AST to markup ------

(defn render-specs
  "Given a value as produced by collect-field-specs and options, renders fields
  defined by AST into markup"
  ([source] (render-specs source {}))
  ([source options]
   ;; see encode/render-spec definition on ::field-spec schema above
   (m/encode field-spec-schema source options
             (mt/transformer ;; these are done on leave, so order is finalize->render
               {:name ::render-spec}
               {:name ::finalize-spec}))))

;; TODO: separate steps in render-form so final field spec AST can be inspected during dev
;(defn- prerender-form
;  "All processing up to calling render-specs to generate a form. See docstring
;  on render-form for more information on steps involved."
;  [schema source errors options]
;  (let [options (default-options options)
;        wrapped-schema (-> (wrap-schema schema) (prep-schema options))
;        apply-defaults (->> (mt/transformer xf-placeholders+defaults xf-anti-forgery)
;                            (encoder wrapped-schema options))]
;    (collect-field-specs
;      wrapped-schema
;      (apply-defaults {base-data-key source})
;      (map (fn [error]
;             (update error :in #(cons base-data-key %)))
;           errors)
;      options)))

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
   (let [options (default-options options)
         wrapped-schema (-> (wrap-schema schema) (prep-schema options))
         apply-defaults (->> (mt/transformer xf-placeholders+defaults xf-anti-forgery)
                             (encoder wrapped-schema options))]
     (-> (collect-field-specs
           wrapped-schema
           (apply-defaults {base-data-key source})
           (map (fn [error]
                  (update error :in #(cons base-data-key %)))
                errors)
           options)
         ;(#(do (clojure.pprint/pprint %) %))
         (render-specs options)))))

;; ------ parsing ------

(def ^:private decoder
  "Memoized m/decoder"
  (memoize m/decoder))

(def ^:private explainer
  "Memoized m/explainer"
  (memoize m/explainer))

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
   (let [decode (decoder schema options parse-transformer)
         explain (if validate (explainer schema options) (constantly nil))
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
   (let [options (default-options options)
         ;; TODO: handle potential failure to decode options
         options (cond-> options
                   (not (::ignore-form-options options))
                   (conj (decode-flags (get raw-data base-flags-key))))
         data (get raw-data base-data-key)
         parsed (parse schema data options)]
     (assoc parsed
            :options options
            :form (delay (render-form schema
                                      (:value parsed)
                                      (:errors parsed)
                                      options))))))

;; remaining:
;; - [ ] Field specs:
;;    - [ ] clean up schema to match reality
;;    - [X] strip internal keys before rendering
;;      - see util/internal-attrs
;;    - [X] store malli type for utility
;; - [ ] simplify add-field-specs walker
;; - [ ] use new mt default values to populate all keys when available
;; - [ ] integrate functionality currently in middleware in mfrt to remove nil keys
;; [X] format errors into rendered form
;; [X] attempt parse macro
;;  - just function
;; [X] attempt parse=>re-render form macro built on previous
;;  - part of above
;; [ ] simple, configurable middleware to parse
;; [ ] optional reitit coercion module
;; [X] ring-anti-forgery
