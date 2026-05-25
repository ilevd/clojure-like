(ns clojure-like.gl
  (:require [clojure-like.config :as conf]
            [clojure-like.utils :as utils]
            [cheshire.core :as json]
            [clj-http.client :as http]
            [clojure.string :as str])
  (:import (java.net URLEncoder)
           (java.nio.charset StandardCharsets)))


(def ^:const per-page 100)


(defn rest-to-gh-rest-format [{:keys [web_url star_count forks_count avatar_url] :as data}]
  (-> data
      (select-keys [:name :description :new-commits :new-stars :created_at :pushed_at])
      (assoc
        ;:homepage homepageUrl
        :html_url web_url
        :stargazers_count star_count
        ;:language (:name primaryLanguage)
        :forks forks_count
        ; :size diskUsage
        :organization {:avatar_url avatar_url})))

(defn make-rest-req [owner name]
  (println (format "Make request to GitLab - %s repo " name))
  (let [api-url (str "https://gitlab.com/api/v4/projects/"
                     (URLEncoder/encode (str owner "/" name) StandardCharsets/UTF_8))]
    (-> (http/get api-url {:as :json}) :body)))


(def ^:const query
  "query($fullPath: ID!) {
     project(fullPath: $fullPath) {
       id name description createdAt webUrl starCount forksCount avatarUrl languages { name } } }")

(defn to-gh-rest-format [{:keys [starCount createdAt webUrl forksCount id languages avatarUrl] :as data}]
  (-> data
      (select-keys [:name :description])
      (assoc :id (-> (str/split id #"/") last read-string)
             :html_url webUrl
             :stargazers_count starCount
             :forks forksCount
             :language (-> languages first :name)
             :created_at createdAt
             :organization {:avatar_url avatarUrl})))

(defn make-graphql-req [owner name msg]
  (println "Make GitLab graphql request -" msg)
  (let [res (http/post "https://gitlab.com/api/graphql"
                       {:as           :json
                        :content-type :json
                        :body         (json/generate-string {:query     query
                                                             :variables {:fullPath (format "%s/%s" owner name)}})})]
    (-> res :body :data :project to-gh-rest-format)))


(defn get-commits [project-id page name]
  (println (format "Make GitLab request - %s commits" name))
  (let [api-url (format "https://gitlab.com/api/v4/projects/%s/repository/commits?per_page=%s&page=%s"
                        project-id per-page page)]
    (-> (http/get api-url {:as :json}) :body)))

(defn get-new-commits-count [project-id name]
  (loop [total 0 page 1 last-push-date nil]
    (let [data           (get-commits project-id page name)
          dates-all      (->> data (map :committed_date))
          dates          (->> dates-all (take-while utils/less-2-months-ago?))
          last-push-date (if (nil? last-push-date)
                           (->> data first :created_at)
                           last-push-date)]
      (if (and (seq dates)
               (= (count dates) (count dates-all) per-page))
        (recur (+ total (count dates)) (inc page) last-push-date)
        {:new-commits (+ total (count dates))
         :pushed_at   last-push-date}))))


(defn get-stars [project-id page name]
  (println (format "Make GitLab request - %s stars" name))
  (let [api-url (format "https://gitlab.com/api/v4/projects/%s/starrers?per_page=%s&page=%s"
                        project-id per-page page)]
    (-> (http/get api-url {:as :json}) :body)))

(defn get-new-stars-count [project-id name]
  (loop [all-dates [] page 1]
    (let [data  (get-stars project-id page name)
          dates (->> data (map :starred_since))]
      (if (and (seq dates)
               (= (count dates) per-page))
        (recur (into all-dates dates) (inc page))
        (->> (into all-dates dates)
             reverse
             (take-while utils/less-2-months-ago?)
             count)))))


(defn add-image-size [url] (utils/add-query-param url "width" conf/icon-size))

(defn add-icon-avatar [{icon :icon {avatar-url :avatar_url} :organization :as repo}]
  (assoc repo :icon-avatar (cond icon icon
                                 avatar-url (add-image-size avatar-url))))


(defn get-repo-info [{:keys [url] :as repo}]
  (let [[_ _ _ owner name] (str/split url #"/")
        {:keys [id] :as main-info} (make-graphql-req owner name (format "%s repo" name))
        {:keys [new-commits pushed_at]} (get-new-commits-count id name)
        new-stars (get-new-stars-count id name)]
    (-> (assoc main-info
          :pushed_at pushed_at
          :new-commits new-commits
          :new-stars new-stars)
        (merge repo)
        add-icon-avatar)))