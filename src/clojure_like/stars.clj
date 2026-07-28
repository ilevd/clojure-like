(ns clojure-like.stars
  (:require [clojure-like.config :as conf]
            [clojure-like.utils :as utils]
            [cheshire.core :as json]
            [clojure.java.io :as io])
  (:import (java.time.temporal ChronoUnit)))


(def periods [[2 ChronoUnit/MONTHS "2 months"]
              [1 ChronoUnit/MONTHS "month"]
              [3 ChronoUnit/WEEKS "3 weeks"]
              [2 ChronoUnit/WEEKS "2 weeks"]
              [1 ChronoUnit/WEEKS "week"]
              [6 ChronoUnit/DAYS "6 days"]
              [5 ChronoUnit/DAYS "5 days"]
              [4 ChronoUnit/DAYS "4 days"]
              [3 ChronoUnit/DAYS "3 days"]
              [2 ChronoUnit/DAYS "2 days"]
              [1 ChronoUnit/DAYS "day"]])


(def default-label (-> periods first last))


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


(defn find-period [{:keys [dates]}]
  (some (fn [[amount unit label]]
          (let [date (.minus (utils/now-date) amount unit)]
            (some (fn [[index date-string]]
                    (when (= (.toLocalDate (utils/parse-date date-string))
                             (.toLocalDate date))
                      [index amount unit label]))
                  (map-indexed vector dates))))
        periods))


(defn add-new-stars [repos-info]
  (let [{:keys [dates repos] :as stat} (read-data)]
    (if-let [[index amount unit label] (find-period stat)]
      (let [repos-prev       (update-vals repos #(get % index))
            repos-info-stars (->> repos-info
                                  (map (fn [{:keys [url stargazers_count created_at] :as repo}]
                                         (let [prev-stars-count (repos-prev url)
                                               new-stars        (cond
                                                                  prev-stars-count (- stargazers_count prev-stars-count)

                                                                  (.isBefore (utils/parse-date (dates index))
                                                                             (utils/parse-date created_at)) stargazers_count

                                                                  :else 0)]
                                           (assoc repo :new-stars new-stars)))))]
        {:repos repos-info-stars
         :label label})
      {:repos repos-info})))