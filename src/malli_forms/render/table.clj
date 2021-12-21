(ns malli-forms.render.table
  "Provides a table-based form rendering function. Used as the default when no
  other render function is provided."
  (:require
    [malli-forms :as-alias mf]
    [malli-forms.util :refer [default
                              path->name
                              props->attrs
                              value->label]]
    [malli.core :as-alias m]))

(defn- labeled-input
  [{:keys [id label] :as field-spec}]
  [:div.form-row
   (when label
     [:label {:for id} label])
   [:input (props->attrs field-spec)]])

(defn- coll-legend
  "Get a legend value for a collection based on its spec"
  [spec]
  (or (some-> spec ::m/name value->label)
      (let [path-end (last (:path spec))]
        (when (and (some? path-end)
                   (not= path-end ::m/in))
          (value->label path-end)))))

(defmulti render
  "Renderer used when no theme is specified"
  :type)

(defmethod render :default
  [spec]
  (labeled-input spec))

;(defmethod render :checkbox
;  [spec]
;  ;; TODO
;  (labeled-input (assoc spec :required false)))

(defmethod render :radio
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

(defmethod render :select
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

(defmethod render ::mf/collection
  [{:keys [children] :as spec}]
  [:fieldset
   (when-some [l (coll-legend spec)]
     [:legend l])
   (interpose [:br] (seq children))])

;; TODO: basically the same as above, but otherwise impossible to distinguish
;; in collect-field-specs
(defmethod render ::mf/map
  [{:keys [children] :as spec}]
  [:fieldset
   (when-some [l (coll-legend spec)]
     [:legend l])
   (interpose [:br] (seq children))])

(defmethod render ::mf/form
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

