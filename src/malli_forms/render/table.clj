(ns malli-forms.render.table
  "Provides a table-based form rendering function. Used as the default when no
  other render function is provided."
  (:require
    [malli-forms :as-alias mf]
    [malli-forms.util :as util :refer [default default-in
                                       path->name
                                       props->attrs
                                       value->label]]))

(defn- error-span
  "Get zero or more spans containings errors for the given field spec"
  [{:keys [errors]}]
  (for [error errors]
    [:span.error (:message error)]))

(defn- labeled-input
  [{:keys [id label] :as field-spec}]
  [:div.form-row
   (when label
     [:label {:for id} label])
   [:input (props->attrs field-spec)]
   (error-span field-spec)])

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
  [:fieldset
   (when label
     [:legend label])
   (for [option options
         :let [label (value->label option)
               id (path->name (conj path option))
               sel? (= option value)]]
     [:div.form-row
      [:input (props->attrs (assoc spec
                                   :checked  sel?
                                   :id       id
                                   :value    (str option)))]
      [:label {:for id} label]])
   (error-span spec)])

(defmethod render :select
  [{:keys [options label #_:clj-kondo/ignore name value id] :as spec}]
  (list
    (when label
      [:label {:for id} label])
    [:select 
     (-> spec props->attrs (dissoc :label :options :value))
     (list
       (when-not (some #(= value %) options) ;; nothing selected
         [:option {:selected true :value "" :disabled true} "Select an option"])
       (for [option options
             :let [label  (value->label option)
                   sel?   (= option value)]]
         [:option 
          (cond-> {:value option}
            sel? (assoc :selected true))
          label]))]
    (error-span spec)))

(defmethod render :submit
  [spec]
  [:button (props->attrs spec) (or (util/label spec) (:value spec))])

(defmethod render ::mf/collection
  [{:keys [children] :as spec}]
  [:fieldset
   (when-some [l (util/label spec)]
     [:legend l])
   (seq children)
   ;(interpose [:br] (seq children))
   (error-span spec)])

(defmethod render ::mf/form
  [{:keys [children] :as spec}]
  [:form
   (-> (props->attrs spec)
       (default :method "POST") ;; TODO: better way of defaulting
       (default-in [:style :display] :table)
       (default-in [:style :border-collapse] :separate)
       (default-in [:style :border-spacing] "0 5px"))
   [:style
    "label, input { display: table-cell; }
    div.form-row { display: table-row; }
    label { padding-right: 10px; }
    span.error { font-size: 80%; color: red; }"]
   (interpose [:br] (seq children))])
