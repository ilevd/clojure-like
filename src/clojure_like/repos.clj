(ns clojure-like.repos
  (:require [clojure-like.api :as api]
            [clojure-like.config :as conf]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def cache-data (atom (try (edn/read-string (slurp conf/repos-cache-path))
                           (catch Exception _ []))))

(defn read-from-cache [url]
  (->> @cache-data
       (some #(when (= url (:html_url %)) %))))

(defn add-to-cache [info]
  (let [new-cache-data (swap! cache-data (fn [data] (->> (conj data info)
                                                         (group-by :html_url)
                                                         (mapv #(-> % second first)))))]
    (spit (io/file conf/repos-cache-path) new-cache-data #_(with-out-str (pprint/pprint new-cache-data)))))


(defn load-info [{:keys [url] :as repo}]
  (merge (or (read-from-cache url)
             (let [info #_(api/make-rest-req repo)
                   (api/get-repo-info repo)]
               (add-to-cache info)
               info))
         repo))


(defn read-repos []
  (distinct (edn/read-string (slurp (io/file conf/repos-path)))))

(defn load-repos [repos]
  (->> repos
       (mapv load-info)
       (sort-by :stargazers_count >)
       ;(sort-by :size)
       ))
