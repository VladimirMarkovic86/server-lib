(defproject org.clojars.vladimirmarkovic86/server-lib "0.3.36"
  :description "Server library"
  :url "http://github.com/VladimirMarkovic86/server-lib"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojars.vladimirmarkovic86/utils-lib "0.4.13"]
                 [org.clojars.vladimirmarkovic86/ajax-lib "0.1.14"]
                 [org.clojars.vladimirmarkovic86/request-server-lib "0.1.9"]
                 ]

  :min-lein-version "2.0.0"

  :source-paths ["src/clj"]
  :test-paths ["test/clj"]
  :java-source-paths ["src/java"]
  
  :jar-exclusions [#"^public/"
                   #"^certificate/"])

