(ns texticlj.core
  (:require [clojure.string :as str]))

(defn all-matches [re string]
  (let [r (re-matcher re string)]
    (take-while vector?
                (repeatedly #(and (.find r)
                                  (let [s (.start r) e (.end r)]
                                    [s (- e s) (subs string s e)]))))))

(defn match-with-groups [re string]
  (let [r (re-matcher re string)]
    (and (.find r)
         (let [s (.start r) e (.end r)]
           [s e (re-groups r ) ]))))

(declare hiccup-inline)
(def inline-replacements
  [[#"(?s)<(.*?)>" (fn [[whole]] whole)] ;don't markup inside html tags
   [#"(?is)<pre>(.*?)</pre>" (fn [[whole]] whole)] ; or PRE body
   [#"(?s)_(.*?)_" (fn [[_ g]] (into [:i] (hiccup-inline g)))]
   [#"(?s)@(.*?)@" (fn [[_ g]] [:code g])]
   [#"\s-(\w.*?\w)-\s" (fn [[_ g]] (into [:s] (hiccup-inline g)))]
   [#"(?s)\*(.*?)\*" (fn [[_ g]] (into [:b] (hiccup-inline g)))]

   [#"!([<>]?)(.*?)!"
    (fn [[_ align src]]
      (let [klass (case align ">" "right" "<" "left" "flow")]
        [:img {:class klass :src src}]))]

   [#"!F([<>]?)(.*?)!"
    (fn [[_ align id]]
      (let [klass (case align ">" "right" "<" "left" "flow")]
        [:div {:class (str "flickr-photo image " klass)
               :photo_id id}]))]

   [#"(?s)\"([^\"]*?)\":((https?|ftp|gopher|mailto)\S+)"
    (fn [[_ label target]] (into [:a] [{:href target} label]))]

   [#"((https?|ftp|gopher|mailto)\S+)"
    (fn [[whole]] (into [:a] [{:href whole} whole]))]

   [#"(?s)\[(\d+)\]"
    (fn [[_ index]]
      [:sup {:class "footnote" :id (str "fnr" index)}
       [:a {:href (str "#fn" index)} (str index)]])]

   [#"(?s)(\A|\n\n)fn(\d+)\.(\s+.*?)(\n{2}?|\z)"
    (fn [[_ _ index text _]]
      [:p {:class "footnote" :id (str "fn" index)}
       [:a {:href (str "#fnr" index)} [:sup (str index)]]
       (hiccup-inline text)])]
   ])

(defn hiccup-inline [body]
  (let [matches (reduce (fn [r l]
                          (if-let [m (match-with-groups (first l) body)]
                            (assoc r m (second l))
                            r))
                        {}
                        inline-replacements)
        goodness (fn [[s e & rest]] [s (- s e)])
        m (if-let [k (keys matches)] (first (sort-by goodness (keys matches))))
        fun (get matches m)]
    (if-let [[s e groups] m]
      (cons (subs body 0 s)
            (cons (fun groups)
                  (hiccup-inline (subs body  e))))
      (list body))))

(assert (= '("test text")
           (hiccup-inline "test text")))
(assert (= '("foo " (:i "bar") " baz")
           (hiccup-inline "foo _bar_ baz")))
(assert (= '("foo " (:i "hello") " i " (:i "hello 2") "")
           (hiccup-inline "foo _hello_ i _hello 2_")))
(assert (= '("foo "
             (:code "code") "  "
             (:i "hello") " i "
             (:i "hello 2") "")
           (hiccup-inline "foo @code@  _hello_ i _hello 2_")))
(assert (= '("foo " (:b "code") "")
           (hiccup-inline "foo *code*")))
;; and nested markup works too
(assert (= '("" (:b "bold " (:i "italics") "") " rule")
           (hiccup-inline "*bold _italics_* rule")))
;; url markup
(assert (= '("" (:b "bold " (:i "italics") "") " rule "
             (:a {:href "https://www.google.com"} "https://www.google.com") "")
           (hiccup-inline "*bold _italics_* rule https://www.google.com")))

;; no markup of html tag contents

(assert (= "hello <a href='http://foo.com'>foo</a>"
           (str/join (hiccup-inline "hello <a href='http://foo.com'>foo</a>"))))

(assert (= "hello <a\nhref='http://foo.com'>foo</a>"
           (str/join (hiccup-inline "hello <a\nhref='http://foo.com'>foo</a>"))))

;; simple footnotes
(assert (= '("Sentence." (:sup {:class "footnote" :id "fnr13"} (:a {:href "#fn13"} "13")) "" (:sup {:class "footnote" :id "fnr14"} (:a {:href "#fn14"} "14")) "")
  (hiccup-inline "Sentence.[13][14]")))

(assert (= '(""
              (:p {:class "footnote" :id "fn13"}
                (:a {:href "#fnr13"} (:sup "13"))
                (" This is a footnote.\nfn14. This is a "
                 (:i "second line")
                 " that's part of the first footnote.")) ""
              (:p {:class "footnote" :id "fn15"}
                (:a {:href "#fnr15"} (:sup "15"))
                (" This is a new footnote.")) "")
  (hiccup-inline "fn13. This is a footnote.\nfn14. This is a _second line_ that's part of the first footnote.\n\nfn15. This is a new footnote.")))



(defn treeify [level matches strings]
  (when-let [s (seq matches)]
    (let [head [(nth (first s) 2) (get strings (ffirst s))]
          leaves (take-while #(> (second %) level) (rest s))]
      (cons (cons head (treeify (inc level) leaves strings))
            (treeify level (drop (inc (count leaves)) s) strings)))))

(defn list-type [l]
  (let [[[marker text] & leaves] (first l)]
    (case (first marker)
      \* :ul
      \# :ol)))

(declare hiccup-list-item)
(defn hiccup-list-body [tree]
  [(into [(list-type tree)] (map hiccup-list-item tree))])

(defn hiccup-list-item [[[marker text] & leaves]]
  (let [self (into [:li] (hiccup-inline text))]
    (if-let [l (seq leaves)]
      (into self (hiccup-list-body l))
      self)))

(defn hiccup-for-lists [block]
  (let [matches (all-matches #"(?m)^[*#]+" block)]
    (if (seq matches)
      (let [substrings (conj
                        (map (fn [a b]
                               [(first a)
                                (subs block (+ (first a) (second a)) (first b))])
                             matches
                             (rest matches))
                        (let [[s l _] (last matches)] [s (subs block (+ s l))]))
            tree (treeify 1 matches (into {} substrings))]
        (hiccup-list-body tree))
      (hiccup-inline block))))

(def block-elements
  (let [names [:p :h1 :h2 :h3 :h4 :h5 :h6 :pre]]
    (assoc (zipmap (map name names) names)
      "bq" :blockquote)))

(def block-element-re
  (re-pattern
   (str "\\A("
        (str/join "|" (keys block-elements))
        ")\\. *")))

(defn hiccup-for-block [block]
  (let [[match tagname] (re-find block-element-re block)]
    (if match
      (into [(get block-elements tagname)]
            (hiccup-for-lists (str/replace-first block match "")))
      (into [:p] (hiccup-for-lists block)))))

(defn to-hiccup [text]
  (let [blocks (str/split text #"\r?\n\r?\n")]
    (map hiccup-for-block blocks)))


(assert (= '([:h1 "hello"]) (to-hiccup "h1. hello\n\n")))

(assert (= '([:p [:ul [:li " hello \n" [:ul [:li " goodbye \n" [:ul [:li " three\n # a\n"]]]]] [:li " b \n" [:ul [:li " two \n"]]] [:li " again \n" [:ul [:li " two \n"]]] [:li " andf again"]]])
           (to-hiccup "* hello \n** goodbye \n*** three\n # a\n# b \n** two \n* again \n** two \n* andf again")))
