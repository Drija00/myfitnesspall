(defproject myfitnesspall "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/tools.cli "0.4.2"]
                 [org.clojure/data.json "2.4.0"]
                 [http-kit "2.8.0-beta3"]
                 [codax "1.3.1"]
                 [clj-time "0.15.2"]]
  :repl-options {:init-ns myfitnesspall.core})
