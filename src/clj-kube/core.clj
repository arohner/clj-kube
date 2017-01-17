(ns clj-kube.core
  (:require [cemerick.url :as url]
            [cheshire.core :as json]
            [clj-http.client :as http]
            [clj-time.core :as time]
            [clojure.core.strint :refer (<<)]))

(defn api-request [{:keys [url method path body opts return-body?] :as args
                    :or {method :get
                         return-body? true}}]
  (let [args (merge {:url (str url path)
                     :method method
                     :content-type :json
                     :as :json}
                    args
                    (when body
                      {:body (json/generate-string body)}))
        resp (http/request args)]
    (if (and (= 200 (:status resp)) return-body?)
      (:body resp)
      resp)))

(defn make-path [{:keys [api namespace resource name]}]
  (assert api)
  (assert resource)
  (let [path (if namespace
               (<< "~{api}/namespaces/~{namespace}/~{resource}")
               (<< "~{api}/~{resource}"))
        path (if name
               (<< "~{path}/~{name}")
               path)]
    path))

(defmacro def-resource [name {:keys [api namespaced? resource read-only?]}]
  (assert api)
  (assert resource)
  (let [getter (symbol (<< "get-~{name}"))
        get-doc-string (<< "return a ~{resource}")
        creater (symbol (<< "create-~{name}"))
        applyer (symbol (<< "apply-~{name}"))
        deleter (symbol (<< "delete-~{name}"))
        ensurer (symbol (<< "ensure-~{name}"))
        exister (symbol (<< "~{name}-exists?"))
        writeable? (not read-only?)]
    `(do
       (defn ~getter
         ~get-doc-string
         [name# & [{:keys [~'namespace] :or {~'namespace "default"}}]]
         (api-request {:path (make-path (merge {:api ~api
                                                :resource ~resource
                                                :name name#}
                                               (when ~namespaced?
                                                 {:namespace ~'namespace})))
                       :method :get}))
       ~(when writeable?
          `(do
             (defn ~creater [url# data# & [{:keys [~'namespace] :or {~'namespace "default"}}]]
               (let [name# (-> data# :metadata :name)]
)
               (api-request {:url url#
                             :path (make-path (merge {:api ~api
                                                      :resource ~resource}
                                                     (when ~namespaced?
                                                       {:namespace ~'namespace})))
                             :method :post
                             :body data#}))
             (defn ~applyer [url# data# & [{:keys [~'namespace] :or {~'namespace "default"}}]]
               (let [name# (-> data# :metadata :name)]
                 (assert name#)
                 (api-request {:url url#
                               :path (make-path (merge {:api ~api
                                                        :resource ~resource
                                                        :name name#}
                                                       (when ~namespaced?
                                                         {:namespace ~'namespace})))
                               :method :put
                               :body data#})))
             (defn ~exister [url# name# & [{:keys [~'namespace] :or {~'namespace "default"}}]]
               (-> (api-request {:url url#
                                 :path (make-path (merge {:api ~api
                                                          :resource ~resource
                                                          :name name#}
                                                         (when ~namespaced?
                                                           {:namespace ~'namespace})))
                                 :method :get
                                 :return-body? false
                                 :throw-exceptions false})
                   :status
                   (= 200)))

             (defn ~deleter [url# name# & [{:keys [~'namespace] :or {~'namespace "default"}}]]
               (api-request {:url url#
                             :path (make-path (merge {:api ~api
                                                      :resource ~resource
                                                      :name name#}
                                                     (when ~namespaced?
                                                       {:namespace ~'namespace})))
                             :method :delete}))

             (defn ~ensurer
               "Ensure the resource exists, creating it if it does not. `data` is the complete resource. Identity determined using `(-> data :metadata :name)` "
               [url# data# & [{:keys [~'namespace] :or {~'namespace "default"}}]]
               (let [name# (-> data# :metadata :name)]
                 (assert name#)
                 (if (~exister name#)
                   (let [old# (~getter name#)
                         new# (update-in data# [:metadata] (fn [m#]
                                                             (merge (select-keys (:metadata old#) [:resourceVersion]) m#)))
                         new# (if (and (-> old# :spec :clusterIP)
                                       (not (-> new# :spec :clusterIP)))
                                (assoc-in new# [:spec :clusterIP] (-> old# :spec :clusterIP))
                                new#)]

                     (~applyer url# new# {:namespace ~'namespace}))
                   (~creater url# data#)))))))))

(def-resource configmap {:api "/api/v1"
                         :resource "configmaps"
                         :namespaced? true})

(def-resource deployment {:api "/apis/extensions/v1beta1"
                          :resource "deployments"
                          :namespaced? true})

(def-resource node {:api "/api/v1"
                    :resource "nodes"
                    :namespaced? false})

(def-resource petset {:api "/apis/apps/v1alpha1"
                      :resource "petsets"
                      :namespaced? true})

(def-resource pods {:api "/api/v1"
                    :resource "pods"
                    :namespaced? true})

(def-resource secret {:api "/api/v1"
                      :resource "secrets"
                      :namespaced? true})

(def-resource service {:api "/api/v1"
                       :resource "services"
                       :namespaced? true})
