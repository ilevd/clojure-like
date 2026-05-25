(ns clojure-like.core
  (:require [clojure-like.config :as conf]
            [clojure-like.repos :as repos]
            [clojure-like.utils :as utils]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.util.regex Pattern)))


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
(def ^:const plus-icon "➕")


(defn status [s]
  (cond
    (utils/less-1-month-ago? s) green-circle-icon
    (utils/less-6-months-ago? s) sandglass-icon
    :else ""))

(defn md-table
  ([headers rows]
   (md-table headers [] rows))
  ([headers alignment rows]
   (let [enclose      #(str "|" % "|")
         to-align-str #(get {:left    ":---"
                             :right   "---:"
                             :center  ":---:"
                             :default "---"} % "---")
         n            (count headers)
         align        (take n (concat alignment
                                      (repeat (max 0 (- n (count alignment))) :default)))
         line-str     (->> align
                           (map to-align-str)
                           (str/join "|")
                           enclose)]
     (->> (into
            [(->> headers (str/join " | ") enclose)
             line-str]
            (->> rows
                 (mapv (fn [row] (enclose (str/join " | " row))))))
          (str/join \newline)))))

(defn md-link [title url hover]
  (format "[%s](%s \"%s\")" title url (or hover "")))

(defn md-bold [s]
  (str "**" s "**"))

(defn md-italic [s]
  (str "*" s "*"))

(defn html-image [src height]
  ; (format "![Avatar](%s&s=30)" src)
  (format "<img src='%s' height='%s'>" src height))

(defn details-html [summary body]
  (format "<details><summary>%s</summary>\n\n%s\n</details>" summary body))


(defn icon-field [{:keys [icon-avatar]}]
  (when icon-avatar (html-image icon-avatar conf/icon-size)))

(defn title-field [{:keys [title name homepage html_url pushed_at]}]
  (str (cond-> (md-link (str (or title name)
                             #_(when (= :gitlab (utils/host-kw html_url))
                                 (str " " (html-image conf/gl-icon 14))))
                        html_url
                        (str "Last push: " (utils/format-date-MMM-yyyy pushed_at)))
               (utils/less-year-ago? pushed_at) md-bold)
       " "
       (when-not (str/blank? homepage)
         (md-link link-icon homepage "Homepage"))))


(defn gen-table [data]
  (md-table
    [#_"" "Icon" "Name" "Description" "Language" "Stars" #_"Git" #_#_#_"Forks" "Watching" "Size" #_"Status"]
    [#_:center :center]
    (->> data
         (mapv (fn [{:keys [description stargazers_count language url forks subscribers_count size pushed_at]
                     :as   repo}]
                 [#_(-> pushed_at status)
                  (icon-field repo)
                  (title-field repo)
                  description
                  language
                  (-> stargazers_count utils/round-num (str star-icon))
                  #_(cond
                      (= :gitlab (utils/host-kw url)) (html-image conf/gl-icon 30)
                      (= :github (utils/host-kw url)) (html-image conf/gh-icon 30)
                      )
                  #_#_#_(-> forks utils/round-num (str fork-icon))
                          (-> subscribers_count utils/round-num (str eye-icon))
                          (-> size utils/round-size)
                  ;(-> pushed_at status #_(str (str-date pushed_at)))
                  ])))))

(defn gen-stars-table [data]
  (md-table
    ["Icon" "Name" "Stars added"] [:center]
    (->> data
         (mapv (fn [{:keys [new-stars] :as repo}]
                 [(icon-field repo)
                  (title-field repo)
                  (str plus-icon " " (utils/round-num new-stars) star-icon)])))))

(defn gen-commits-table [data]
  (md-table
    ["Icon" "Name" "New commits"] [:center]
    (->> data
         (mapv (fn [{:keys [new-commits] :as repo}]
                 [(icon-field repo)
                  (title-field repo)
                  (str plus-icon " " (utils/round-num new-commits) " commits")])))))

(defn gen-new-table [data]
  (md-table
    ["Icon" "Name" "Created"] [:center]
    (->> data
         (mapv (fn [{:keys [created_at] :as repo}]
                 [(icon-field repo)
                  (title-field repo)
                  (utils/format-date-dd-MMM-yyyy created_at)])))))

(defn gen-table-with-details [gen-table-fn data split-num]
  (let [[top-data more-data] (->> data (split-at split-num))]
    (if (zero? (count top-data))
      (md-italic "Nothing to show")
      (str (gen-table-fn top-data)
           \newline
           (when (seq more-data)
             (details-html "Show more" (gen-table-fn more-data)))))))


(defn replace-vars [template data]
  (->> data
       (reduce (fn [s [k v]]
                 (str/replace s (re-pattern (Pattern/quote (str "{{" (name k) "}}"))) (str v)))
               template)))


(defn -main []
  (let [data          (repos/load-repos (repos/read-repos))
        stars-data    (->> data (sort-by :new-stars >) (take-while (comp pos? :new-stars)))
        stars-table   (gen-table-with-details gen-stars-table stars-data 10)

        commits-data  (->> data (sort-by :new-commits >) (take-while (comp pos? :new-commits)))
        commits-table (gen-table-with-details gen-commits-table commits-data 10)

        new-data      (->> data (sort-by :created_at) reverse (take-while #(utils/less-year-ago? (:created_at %))))
        new-table     (gen-table-with-details gen-new-table new-data 10)

        main-table    (gen-table data)
        template      (slurp conf/readme-template-path)
        readme        (replace-vars template {:stars-table   stars-table
                                              :commits-table commits-table
                                              :new-table     new-table
                                              :main-table    main-table
                                              :date          (utils/current-date-dd-MMM-yyyy)
                                              :count         (count data)})]
    (spit (io/file conf/readme-path) readme)))


(comment
  (-main)
  (count (read-repos))
  (count (edn/read-string (slurp cache-path)))
  )
