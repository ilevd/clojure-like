(ns clojure-like.utils
  (:require [clojure.string :as str])
  (:import (com.google.common.net InternetDomainName)
           (java.net URI)
           (java.time Instant ZoneId ZonedDateTime)
           (java.time.format DateTimeFormatter)
           (java.time.temporal ChronoUnit TemporalUnit)
           (java.util Locale)
           (org.apache.http.client.utils URIBuilder)))


(defn now-date [] (ZonedDateTime/now (ZoneId/of "UTC")))
(defn parse-date [s] (.atZone (Instant/parse s) (ZoneId/of "UTC")))


(defn- less-ago? [amount ^TemporalUnit unit s]
  (let [date     (parse-date s)
        date-ago (.minus (now-date) amount unit)]
    (.isAfter date date-ago)))


(def less-1-month-ago? (partial less-ago? 1 ChronoUnit/MONTHS))
(def less-2-months-ago? (partial less-ago? 2 ChronoUnit/MONTHS))
(def less-6-months-ago? (partial less-ago? 6 ChronoUnit/MONTHS))
(def less-year-ago? (partial less-ago? 1 ChronoUnit/YEARS))


(defn- format-date
  "2026-05-14T11:28:18Z -> May 2026"
  [date pattern]
  (let [date (if (string? date) (parse-date date) date)]
    (.format (-> (DateTimeFormatter/ofPattern pattern)
                 (.withLocale Locale/ENGLISH)
                 (.withZone (ZoneId/of "UTC")))
             date)))

(defn format-date-MMM-yyyy [date] (format-date date "MMM yyyy"))
(defn format-date-dd-MMM-yyyy [date] (format-date date "dd MMM yyyy"))
(defn current-date-dd-MMM-yyyy [] (format-date-dd-MMM-yyyy (now-date)))


(defn round-num [num]
  (if (>= num 1000)
    (str (str/replace (str (/ (Math/round (float (/ num 100))) 10.0)) ".0" "") "k")
    num))

(defn round-size [num]
  (if (>= num 1000)
    (str (str/replace (str (/ (Math/round (float (/ num 100))) 10.0)) ".0" "") " Mb")
    (str num " Kb")))

(defn add-query-param [^String url & kvs]
  (let [b (URIBuilder. url)]
    (run! (fn [[k v]] (.addParameter b k (str v))) (partition 2 kvs))
    (-> b .build .toString)))

(defn host-kw [url]
  (let [idn         (InternetDomainName/from (.getHost (URI. url)))
        parts-count (count (.parts (.publicSuffix idn)))]
    (->> (str/split (str idn) #"\.")
         reverse
         (drop parts-count)
         first
         keyword)))