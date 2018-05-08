(def package-version
  (let [v (clojure.string/trim-newline (slurp "VERSION"))
        p (System/getenv "PATCH_LEVEL")]
    (if p
      (str v "." p)
      (str v ".0-SNAPSHOT" p))))

(defproject telent/texticlj package-version
  :description "Convert approximately-Textile markup to Hiccup"
  :url "http://github.com/telent/texticlj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories [["snapshots" {:url  "https://clojars.org/repo"
                               :username "telent"
                               :password :env/CLOJARS_PASSWORD}]]
  :dependencies [[org.clojure/clojure "1.6.0"]])
