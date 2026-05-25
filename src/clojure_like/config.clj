(ns clojure-like.config)

(def ^:const gh-token (or (System/getenv "GITHUB_TOKEN")
                          (try (slurp "token.txt") (catch Exception _))
                          (throw (Exception. "GitHub token is not provided"))))

(def ^:const gh-graphql-queries-path "src/graphql/queries.graphql")

(def ^:const repos-path "repos.edn")
(def ^:const repos-cache-path "cache-data.edn")

(def ^:const readme-path "README.md")
(def ^:const readme-template-path "README-template.md")

(def ^:const icon-size 30)
(def ^:const gh-icon "https://logo-teka.com/wp-content/uploads/2026/02/github-icon-logo.svg")
(def ^:const gl-icon "https://logo-teka.com/wp-content/uploads/2026/02/gitlab-icon-logo.svg")