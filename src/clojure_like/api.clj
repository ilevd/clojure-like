(ns clojure-like.api
  (:require [clojure-like.config :as conf]
            [clojure-like.utils :as utils]
            [cheshire.core :as json]
            [clj-http.client :as http]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.net URI)
           (javax.imageio ImageIO)))


(defn parse-graphql [s]
  (->> (str/split s #"(?=fragment |query )")
       (map (fn [s]
              (let [[_ t n] (re-find #"(fragment|query)\s+([A-Za-z0-9_]+)" s)]
                (when (or (nil? t) (nil? n))
                  (throw (str "Can't parse GraphQL part: " s)))
                [(keyword t n) s])))
       (into {})))

(def graphql-data (parse-graphql (slurp (io/file conf/gh-graphql-queries-path))))


(defn make-rest-req [{:keys [url] :as repo}]
  (let [api-url (str/replace url "github.com" "api.github.com/repos")]
    (println "Make request to:" api-url)
    (-> (http/get api-url
                  (cond-> {:as :json}
                          conf/gh-token (assoc :headers {"Authorization" (str "token " conf/gh-token)})))
        :body)))


(defn make-graphql-req [query vars msg]
  (println "Make graphql request -" msg)
  (let [res (http/post "https://api.github.com/graphql"
                       {:as           :json
                        :content-type :json
                        :headers      {"Authorization" (str "token " conf/gh-token)}
                        :body         (json/generate-string {:query     query
                                                             :variables vars})})]
    (:body res)))


(defn to-rest-api-format [{:keys [watchers diskUsage stargazerCount createdAt pushedAt
                                  primaryLanguage homepageUrl url forkCount owner] :as data}]
  (-> data
      (select-keys [:name :description :new-stars :new-commits])
      (assoc :homepage homepageUrl
             :html_url url
             :stargazers_count stargazerCount
             :language (:name primaryLanguage)
             :forks forkCount
             :size diskUsage
             :created_at createdAt
             :pushed_at pushedAt
             :subscribers_count (:totalCount watchers)
             :organization {:avatar_url (:avatarUrl owner)})))


(defn add-image-size [src] (str src "&s=" conf/icon-size))

(defn uploaded-image?
  "Determine if avatar is uploaded or default"
  [src]
  (println "Check avatar:" src)
  (let [s (add-image-size src)]
    (= conf/icon-size (.getWidth (ImageIO/read (.toURL (URI. s)))))))

(defn add-icon-avatar [{icon :icon {avatar-url :avatar_url} :organization :as repo}]
  (assoc repo :icon-avatar
              (cond
                icon icon
                (and avatar-url (uploaded-image? avatar-url)) (add-image-size avatar-url))))


(defn get-repo-info [{:keys [url] :as repo}]
  (let [[_ _ _ owner name] (str/split url #"/")
        main-info (-> (make-graphql-req (str (:fragment/RepoFields graphql-data) \newline (:query/GetRepo graphql-data))
                                        {:owner owner
                                         :name  name}
                                        (format "%s repo" name))
                      :data :repository)
        stars     (loop [total 0 cursor nil]
                    (let [data         (make-graphql-req (str (:fragment/StarsFields graphql-data) \newline (:query/GetStars graphql-data))
                                                         {:owner  owner
                                                          :name   name
                                                          :before cursor}
                                                         (format "%s stars" name))
                          new-cursor   (-> data :data :repository :stargazers :pageInfo :startCursor)
                          has-new-page (-> data :data :repository :stargazers :pageInfo :hasPreviousPage)
                          dates-all    (->> data :data :repository :stargazers :edges reverse (map :starredAt))
                          dates        (->> dates-all (take-while utils/less-2-months-ago?))]
                      (if (and has-new-page
                               (= (count dates) (count dates-all)))
                        (recur (+ total (count dates)) new-cursor)
                        (+ total (count dates)))))
        commits   (loop [total 0 cursor nil]
                    (let [data         (make-graphql-req (str (:fragment/CommitsFields graphql-data) \newline (:query/GetCommits graphql-data))
                                                         {:owner owner
                                                          :name  name
                                                          :after cursor}
                                                         (format "%s commits" name))
                          new-cursor   (-> data :data :repository :defaultBranchRef :target :history :pageInfo :endCursor)
                          has-new-page (-> data :data :repository :defaultBranchRef :target :history :pageInfo :hasNextPage)
                          dates-all    (->> data :data :repository :defaultBranchRef :target :history :edges (map (comp :committedDate :node)))
                          dates        (->> dates-all (take-while utils/less-2-months-ago?))]
                      (if (and has-new-page
                               (= (count dates) (count dates-all)))
                        (recur (+ total (count dates)) new-cursor)
                        (+ total (count dates)))))]
    (-> (assoc main-info
          :new-stars stars
          :new-commits commits)
        to-rest-api-format
        (merge repo)
        add-icon-avatar)))