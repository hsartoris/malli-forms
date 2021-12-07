(ns malli-forms
  (:require
    [clojure.string :as str]
    [malli.core :as m]
    [malli.transform :as mt]
    [malli.util :as mu])
  (:import java.util.regex.Pattern))

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

(def ^:private demunge-re
  "Regex matching replacements in [[name-substring-replacements]]"
  (->> (vals name-substring-replacements)
       (mapv re-quote)
       (str/join \|)
       re-pattern))

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

(def input-types-schema
  [:enum
   :checkbox
   ;:color
   :date
   ;:datetime-local
   :email
   ;:file
   :hidden
   ;:image
   ;:month
   :number
   :password
   ;; TODO
   ;:radio
   ;; TODO
   ;:range
   ;:reset
   ;:search
   :submit
   ;:tel
   :text
   ;:time
   ;:week
   :url])

(def field-spec-schema
  [:map {:registry {::type input-types-schema}}
   [::name    string?]
   [::label   {:optional true} string?]
   [::id      string?]
   [::type    {:optional true}]
   [::attributes [:map-of :keyword :any]]])

(defn- field-spec
  "Generate a field spec for a given schema and path"
  [schema path]
  (let [props (m/properties schema)
        input-name (or (::name props) (path->name path))]
    (-> props
        (assoc  ::name  input-name, ::path path)
        (update ::label #(or % (path->label path)))
        (update ::id    #(or % (str "mf-" input-name))))))

(def schema-type-by-input-type
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

(defmulti input-type
  "Get an input type for a given schema and children, given that the children
  will already have had ::type set on them, if possible"
  (fn [schema _children]
    (m/type schema)))

(defmethod input-type :default
  [schema _]
  ((m/type schema) schema-type->input-type))

(defmethod input-type :maybe
  [_ children]
  (::type (m/properties (first children))))

(defmethod input-type :and
  [_ children]
  (let [ctypes (into #{} (map #(-> % m/properties ::type)) children)]
    (when (= 1 (count ctypes))
      (first ctypes))))

(defmethod input-type :enum
  [_ _]
  ;; enum will be a select and that doesn't support input type
  nil)

(defn schema->input-type
  "Wraps [[input-type]] to call with schema and (m/children schema)"
  [schema]
  (input-type schema (m/children schema)))

(defn complete-field-specs
  "Walk `schema`, adding values to complete field specs on every subschema"
  [schema]
  (m/walk schema
          (fn [schema path children _]
            (let [children' (if (= :map (m/type schema))
                              (for [[k cprops cschema] children]
                                [k cprops (mu/update-properties cschema merge cprops)])
                              children)]
            (m/-set-properties
              (m/-set-children schema children')
              (-> (field-spec schema path)
                  (update ::type #(or % (input-type schema children')))))))))

(defn add-paths
  "Walk `schema`, adding path to each subschema to its properties"
  [schema]
  (m/walk schema
          (fn [schema path children _]
            (-> (m/-set-children schema children) ;; children already postwalked
                (mu/update-properties assoc ::path path)))))

(defn- props->attrs
  "Convert malli properties from a schema into an attribute map for an input"
  [{::keys [id name attributes type] :keys [optional]} value]
  (cond-> {:id    id
           :name  name}
    (some? value)                 (assoc :value value)
    (not (:optional field-spec))  (assoc :required true)
    (some? type)                  (assoc :type type)
    (some? attributes)            (conj attributes)))

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
  "Renders a field, keyed either on `(::type field-spec)` or `(m/type schema)`"
  (fn [schema _value]
    (or (::type (m/properties schema))
        (m/type schema))))

(defmethod render-field :default
  [schema value]
  ;(prn schema value)
  (labeled-input (m/properties schema) value))

(defmethod render-field :map
  [schema m]
  ;(prn m)
  (map (fn [[field _ child-schema :as child]]
         ;(prn  child)
         (render-field child-schema (get m field)))
       (m/children schema)))
  
(defmethod render-field :enum
  [schema value]
  (let [{::keys [id label name] :as props} (m/properties schema)]
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
                                          (render-field schema value))})}})

(defn encode-fields
  "Encode the fields of a form from a schema, with an optional object"
  [schema source]
  (-> (complete-field-specs schema)
      (m/encode source (mt/transformer render-field-transformer))))

(def form-props-schema
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
