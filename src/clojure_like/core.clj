(ns clojure-like.core
  (:require [clj-http.client :as http]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [marge.core :as marge])
  (:import (java.net URI)
           (java.text SimpleDateFormat)
           (java.time Instant ZoneId ZonedDateTime)
           (java.time.format DateTimeFormatter)
           (java.time.temporal ChronoUnit)
           (java.util Date TimeZone)
           (java.util.regex Pattern)
           (javax.imageio ImageIO)))


(def ^:const cache-path "cache-data.edn")
(def ^:const repos-path "repos.edn")
(def ^:const readme-path "README.md")
(def ^:const readme-template-path "README-template.md")


(def ^:const sandglass-icon "⏳")
(def ^:const home-icon "\uD83C\uDFE0")
(def ^:const link-icon "\uD83D\uDD17")
(def ^:const eye-icon "\uD83D\uDC41\uFE0F")
(def ^:const star-icon "⭐")
(def ^:const fork-icon "⎇")
(def ^:const green-circle-icon "\uD83D\uDFE2")
(def ^:const yellow-circle-icon "\uD83D\uDFE1")
(def ^:const sprout-icon "\uD83C\uDF31")
(def ^:const net-icon "\uD83D\uDD78")
(def ^:const glasses-icon "\uD83D\uDD76")


(def cache-data (atom (try (edn/read-string (slurp cache-path))
                           (catch Exception _ []))))

(defn read-from-cache [url]
  (->> @cache-data
       (some #(when (= url (:html_url %)) %))))

(defn add-to-cache [info]
  (let [new-cache-data (swap! cache-data (fn [data] (->> (conj data info)
                                                         (group-by :html_url)
                                                         (mapv #(-> % second first)))))]
    (spit (io/file cache-path) new-cache-data #_(with-out-str (pprint/pprint new-cache-data)))))

(defn load-info [{:keys [url] :as repo}]
  (merge (or (read-from-cache url)
             (let [api-url (str/replace url "github.com" "api.github.com/repos")
                   _       (println "Make request to:" api-url)
                   info    (merge (-> (http/get api-url {:as :json}) :body) repo)]
               (add-to-cache info)
               info))
         repo))


(defn read-repos []
  (distinct (edn/read-string (slurp (io/file repos-path)))))

(defn load-repos [repos]
  (->> repos
       (mapv load-info)
       (sort-by :stargazers_count >)
       ;(sort-by :size)
       ))

(defn round-num [num]
  (if (>= num 1000)
    (str (str/replace (str (/ (Math/round (float (/ num 100))) 10.0)) ".0" "") "k")
    num))

(defn round-size [num]
  (if (>= num 1000)
    (str (str/replace (str (/ (Math/round (float (/ num 100))) 10.0)) ".0" "") " Mb")
    (str num " Kb")))

(defn str-date [s]
  (.format (-> (DateTimeFormatter/ofPattern "MMM yyyy")
               (.withZone (ZoneId/of "UTC")))
           (Instant/parse s)))


(defn status [s]
  (let [date        (.atZone (Instant/parse s) (ZoneId/of "UTC"))
        now         (ZonedDateTime/now (ZoneId/of "UTC"))
        month-ago   (.minus now 1 ChronoUnit/MONTHS)
        month-ago-6 (.minus now 6 ChronoUnit/MONTHS)]
    (cond
      (.isAfter date month-ago) green-circle-icon
      (.isAfter date month-ago-6) sandglass-icon
      :else "")))

(defn less-year-ago? [s]
  (let [date         (.atZone (Instant/parse s) (ZoneId/of "UTC"))
        now          (ZonedDateTime/now (ZoneId/of "UTC"))
        month-ago-12 (.minus now 1 ChronoUnit/YEARS)]
    (.isAfter date month-ago-12)))

(defn md-table [headers rows]
  (->> (into
         [(->> headers (str/join " | ") (#(str "|" % "|")))
          (str (str/join (repeat (count headers) "|---")) "|")]
         (->> rows
              (mapv (fn [row]
                      (str "|" (str/join " | " row) "|")))))
       (str/join \newline)))

(defn md-link [title url hover]
  (format "[%s](%s \"%s\")" title url (or hover "")))

(defn md-bold [s]
  (str "**" s "**"))

#_(defn gen-table [data]
    (marge/markdown
      [:table [
               "Name" (mapcat #(do [:link {:text (:name %)
                                           :url  (:html_url %)}]) data)
               "Description" (mapv :description data)
               "Stars" (->> data (mapv (fn [item] (-> item :stargazers_count round-num (str star-icon)))))
               "Language" (mapv :language data)
               "Forks" (->> data (mapv (fn [item] (-> item :forks round-num (str "⤴")))))
               "Watching" (->> data (mapv (fn [item] (-> item :subscribers_count round-num (str glasses-icon)))))
               ]]))

(def ^:const image-size 30)

(defn add-image-size [src]
  (str src "&s=" image-size))

(defn uploaded-image?
  "Determine if avatar is uploaded or default"
  [src]
  (println "Check avatar:" src)
  (let [s (add-image-size src)]
    (= image-size (.getWidth (ImageIO/read (.toURL (URI. s)))))))

(defn image [src]
  ; (format "![Avatar](%s&s=30)" src)
  (format "<img src='%s' height='%s' width='%s'>" src image-size image-size))

(defn gen-table [data]
  (md-table
    [#_"" "Icon" "Name" "Description" "Language" "Stars"  #_#_#_"Forks" "Watching" "Size" #_"Status"]
    (->> data
         (mapv (fn [{:keys [title name homepage description html_url stargazers_count language forks subscribers_count size
                            pushed_at organization icon]}]
                 [#_(-> pushed_at status)
                  (cond
                    (and (:avatar_url organization) (uploaded-image? (:avatar_url organization)))
                    (image (add-image-size (:avatar_url organization)))
                    icon (image icon))
                  (str (cond-> (md-link (or title name)
                                        html_url
                                        (str "Last push: " (str-date pushed_at)))
                               (less-year-ago? pushed_at) md-bold)
                       " "
                       (when-not (str/blank? homepage)
                         (md-link link-icon homepage "Homepage")))
                  description
                  language
                  (-> stargazers_count round-num (str star-icon))
                  #_#_#_(-> forks round-num (str fork-icon))
                          (-> subscribers_count round-num (str eye-icon))
                          (-> size round-size)
                  ;(-> pushed_at status #_(str (str-date pushed_at)))
                  ])))))


(defn gen-date-label []
  (let [formatter (doto (SimpleDateFormat. "dd MMM yyyy")
                    (.setTimeZone (TimeZone/getTimeZone "UTC")))]
    (.format formatter (Date.))))


(defn replace-vars [template data]
  (->> data
       (reduce (fn [s [k v]]
                 (str/replace s (re-pattern (Pattern/quote (str "{{" (name k) "}}"))) (str v)))
               template)))


(defn -main []
  (let [data       (load-repos (read-repos))
        table      (gen-table data)
        table-date (gen-date-label)
        template   (slurp readme-template-path)
        readme     (replace-vars template {:table table
                                           :date  table-date
                                           :count (count data)})]
    (spit (io/file readme-path) readme)))


(comment
  (-main)
  (count (read-repos))
  (count (edn/read-string (slurp cache-path)))
  )
