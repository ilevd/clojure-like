(defproject clojure-like "0.1.0-SNAPSHOT"
  :description "List of Clojure-like projects"
  :url "https://github.com/ilevd/clojure-like"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.12.1"]
                 ; http
                 [clj-http "3.12.3"]
                 [cheshire "6.0.0"]
                 ; markdown
                 [marge "0.16.0"]
                 ]
  :main clojure-like.core
  :repl-options {:init-ns clojure-like.core})
