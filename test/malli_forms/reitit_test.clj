(ns malli-forms.reitit-test
  (:require
    [clojure.pprint :refer [pprint]]
    [malli-forms :as mf]
    [malli.core :as m]
    [malli.transform :as mt]
    [org.httpkit.server :refer [run-server]]
    [muuntaja.core :as muuntaja]
    [reitit.coercion :as coercion]
    [reitit.coercion.malli]
    [reitit.core :as r]
    [reitit.dev.pretty :as pretty]
    [reitit.ring :as ring]
    [reitit.ring.coercion :as rrc]
    [reitit.ring.middleware.exception :as exception]
    [reitit.ring.middleware.muuntaja]
    [reitit.ring.middleware.parameters :as parameters]
    [ring.adapter.jetty :refer [run-jetty]]
    [ring.middleware.nested-params :refer [wrap-nested-params]]
    [rum.core :as rum]))

(def asn-pool-schema
  "Schema for https://medium.com/@kirill.ishanov/building-forms-with-re-frame-and-clojure-spec-6cf1df8a114d"
  [:map {:closed true
         :registry {::range-data [:map {:closed true
                                        :doc "range, pure data requirements only"}
                                  [:first [:int {:min 1, :max 65536}]]
                                  [:last  [:int {:min 1, :max 65536}]]]
                    ::range [:and {:doc "range-data with validation for first < last"
                                   ::mf/use-child 0}
                             ::range-data
                             [:fn #(< (:first %) (:last %))]]}}
   [:label
    [:string {:min 1, :max 64}]]
   [:preallocated {:default false}
    boolean?]
   [:type         {:optional true}
    [:enum "multihomed" "stub" "transit" "ixp"]]
   ;; TODO: this is resulting in required tags - not good
   [:tags         {:optional true}
    [:set [:string {:max 32}]]]
   [:ranges
    [:and {::mf/use-child 0} ;; TODO: not good; somehow simplify this situation
     ;[:vector {:min 1} ::range]
     [:set {:min 1} ::range]
     [:fn (fn [ranges]
            (boolean (reduce #(if (< %1 (:first %2)) (:last %2) (reduced false)) 0 ranges)))]]]])

(def ex-input
  "input valid for ex-schema"
  {:label "blah"
   :preallocated false
   :type "stub"
   :ranges #{{:first  1
              :last   5}
             {:first  10
              :last   20}}})

(def router
  "Reitit router for testing above schema"
  (ring/router
    ["/pools/new"
     {:name     ::new-pool
      :get      {:handler (fn [_req]
                            {:status 200
                             :headers {"content-type" "text/html"}
                             :body 
                             (rum/render-static-markup
                               [:html
                                [:body
                                 [:main
                                  (-> asn-pool-schema mf/collect-field-specs mf/render-specs)]]])})}
      :post     {;:compile coercion/compile-request-coercers
                 :parameters {:form asn-pool-schema}
                 :handler   (fn [req]
                              ;(pprint req)
                              {:status 200
                               :body (rum/render-static-markup
                                       [:html
                                        [:body
                                         (let [params (:params req)
                                               pp->str #(with-out-str (pprint %))
                                               ; for whatever reason, this doesn't work when provided to the malli coercion
                                               decoded (m/decode asn-pool-schema params
                                                                 (mt/transformer
                                                                   (mt/key-transformer {:decode keyword})
                                                                   mt/default-value-transformer
                                                                   (mt/json-transformer)
                                                                   mf/unnest-seq-transformer
                                                                   (mt/string-transformer)
                                                                   (mt/strip-extra-keys-transformer)))]
                                           (list
                                             [:pre (pp->str params)]
                                             [:pre (pp->str decoded)]
                                             [:pre (pp->str (m/explain asn-pool-schema decoded))]))]])})}}]
    {;:compile coercion/compile-request-coercers
     :exception pretty/exception
     :data {:muuntaja muuntaja/instance
            :middleware [parameters/parameters-middleware
                         (fn [handler]
                           (fn [req]
                             (handler (update req :params #(into {} ;; TODO: not a general solution probably
                                                                 ;; that or requires a nil->empty string transformer
                                                                 (filter (fn [kv] (not-empty (val kv))))
                                                                 %)))))
                         wrap-nested-params
                         ;;https://clojurians-log.clojureverse.org/reitit/2021-05-07
                         (fn [handler]
                           (fn [{:keys [params] :as req}]
                             ;(prn params)
                             (handler (assoc req :form-params params))))
                         ;reitit.ring.middleware.muuntaja/format-negotiate-middleware
                         reitit.ring.middleware.muuntaja/format-response-middleware
                         (exception/create-exception-middleware
                           {::exception/default (partial exception/wrap-log-to-console exception/default-handler)})
                         ;reitit.ring.middleware.muuntaja/format-request-middleware
                         ;(fn [handler]
                         ;  (fn [req]
                         ;    (let [res (handler req)]
                         ;      (pprint res)
                         ;      res)))
                         rrc/coerce-exceptions-middleware
                         rrc/coerce-request-middleware]}}))

(def app (ring/ring-handler router))

(defonce server (atom nil))

(defn stop []
  (when-some [s @server]
    (.stop s))
  (reset! server nil))

(defn start []
  (stop)
  (reset! server
          (run-jetty
            (fn [& args]
              (prn args)
              (apply app args))
            {:join? false
             :port 8081})))
