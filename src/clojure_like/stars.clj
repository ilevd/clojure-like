(ns clojure-like.stars
  (:require [clojure-like.config :as conf]
            [clojure-like.utils :as utils]
            [cheshire.core :as json]
            [clojure.java.io :as io]))


(defn read-data []
  (try (-> (slurp conf/stars-path)
           json/parse-string
           (update-keys keyword))
       (catch Exception _ nil)))


(defn write-data! [repos-info]
  (let [{:keys [dates repos]} (read-data)]
    (when (or (nil? (last dates))
              (not= (.toLocalDate (utils/parse-date (last dates)))
                    (.toLocalDate (utils/now-date))))
      (let [new-repos (->> repos-info
                           (mapv (fn [{:keys [url stargazers_count]}]
                                   (let [stars     (get repos url)
                                         new-stars (->> (concat stars
                                                                (when (< (count stars) (count dates))
                                                                  (repeat (- (count dates) (count stars)) nil))
                                                                [stargazers_count])
                                                        (take-last conf/max-stars-record-length))]
                                     [url new-stars])))
                           (into {}))
            new-stat  {:dates (->> (concat dates [(utils/format-iso-instant (utils/now-date))])
                                   (take-last conf/max-stars-record-length))
                       :repos new-repos}]
        (doto (io/file conf/stars-path)
          (io/make-parents)
          (spit (json/generate-string new-stat)))))))