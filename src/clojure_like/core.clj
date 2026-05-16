(ns clojure-like.core
  (:require [clojure-like.repos :as repos]
            [clojure-like.utils :as utils]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.net URI)
           (java.util.regex Pattern)
           (javax.imageio ImageIO)))

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
  (format "<img src='%s' height='%s'>" src image-size))

(defn gen-table [data]
  (md-table
    [#_"" "Icon" "Name" "Description" "Language" "Stars" #_#_#_"Forks" "Watching" "Size" #_"Status"]
    [#_:center :center]
    (->> data
         (mapv (fn [{:keys [title name homepage description html_url stargazers_count language forks subscribers_count size
                            pushed_at icon] {avatar-url :avatar_url} :organization}]
                 [#_(-> pushed_at status)
                  (cond
                    icon (image icon)
                    (and avatar-url (uploaded-image? avatar-url)) (image (add-image-size avatar-url)))
                  (str (cond-> (md-link (or title name)
                                        html_url
                                        (str "Last push: " (utils/format-date-MMM-yyyy pushed_at)))
                               (utils/less-year-ago? pushed_at) md-bold)
                       " "
                       (when-not (str/blank? homepage)
                         (md-link link-icon homepage "Homepage")))
                  description
                  language
                  (-> stargazers_count utils/round-num (str star-icon))
                  #_#_#_(-> forks utils/round-num (str fork-icon))
                          (-> subscribers_count utils/round-num (str eye-icon))
                          (-> size utils/round-size)
                  ;(-> pushed_at status #_(str (str-date pushed_at)))
                  ])))))

(defn gen-stars-table [data]
  (md-table
    ["Icon" "Name" "Stars added"]
    [:center]
    (->> data
         (mapv (fn [{:keys [title name homepage description html_url stargazers_count language new-stars
                            pushed_at icon] {avatar-url :avatar_url} :organization}]
                 [(cond
                    icon (image icon)
                    (and avatar-url (uploaded-image? avatar-url)) (image (add-image-size avatar-url)))
                  (str (cond-> (md-link (or title name)
                                        html_url
                                        (str "Last push: " (utils/format-date-MMM-yyyy pushed_at)))
                               (utils/less-year-ago? pushed_at) md-bold)
                       " "
                       (when-not (str/blank? homepage)
                         (md-link link-icon homepage "Homepage")))
                  (str plus-icon " " new-stars star-icon)
                  ])))))

(defn gen-commits-table [data]
  (md-table
    ["Icon" "Name" "New commits"]
    [:center]
    (->> data
         (mapv (fn [{:keys [title name homepage description html_url stargazers_count language new-commits
                            pushed_at icon] {avatar-url :avatar_url} :organization}]
                 [(cond
                    icon (image icon)
                    (and avatar-url (uploaded-image? avatar-url)) (image (add-image-size avatar-url)))
                  (str (cond-> (md-link (or title name)
                                        html_url
                                        (str "Last push: " (utils/format-date-MMM-yyyy pushed_at)))
                               (utils/less-year-ago? pushed_at) md-bold)
                       " "
                       (when-not (str/blank? homepage)
                         (md-link link-icon homepage "Homepage")))
                  (str plus-icon " " new-commits " commits")
                  ])))))

(defn gen-new-table [data]
  (md-table
    ["Icon" "Name" "Created"]
    [:center]
    (->> data
         (mapv (fn [{:keys [title name homepage html_url created_at
                            pushed_at icon] {avatar-url :avatar_url} :organization}]
                 [(cond
                    icon (image icon)
                    (and avatar-url (uploaded-image? avatar-url)) (image (add-image-size avatar-url)))
                  (str (cond-> (md-link (or title name)
                                        html_url
                                        (str "Last push: " (utils/format-date-MMM-yyyy pushed_at)))
                               (utils/less-year-ago? pushed_at) md-bold)
                       " "
                       (when-not (str/blank? homepage)
                         (md-link link-icon homepage "Homepage")))
                  (utils/format-date-dd-MMM-yyyy created_at)
                  ])))))

(defn replace-vars [template data]
  (->> data
       (reduce (fn [s [k v]]
                 (str/replace s (re-pattern (Pattern/quote (str "{{" (name k) "}}"))) (str v)))
               template)))


(defn -main []
  (let [data          (repos/load-repos (repos/read-repos))
        stars-table   (gen-stars-table (->> data (sort-by :new-stars >) (take 10)))
        commits-table (gen-commits-table (->> data (sort-by :new-commits >) (take 10)))
        new-table     (gen-new-table (->> data (sort-by :created_at) reverse
                                          (filter #(utils/less-year-ago? (:created_at %))) (take 10)))
        main-table    (gen-table data)
        template      (slurp readme-template-path)
        readme        (replace-vars template {:stars-table   stars-table
                                              :commits-table commits-table
                                              :new-table     new-table
                                              :main-table    main-table
                                              :date          (utils/current-date-dd-MMM-yyyy)
                                              :count         (count data)})]
    (spit (io/file readme-path) readme)))


(comment
  (-main)
  (count (read-repos))
  (count (edn/read-string (slurp cache-path)))
  )
