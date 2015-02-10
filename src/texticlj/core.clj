(ns texticlj.core
  (require [clojure.string :as str]))

(def inline-replacements
  [[#"_(.*?)_" (fn x [body matches]
                 (let [s (first match)
                       e (+ (first match) (second match))]
                   (println more)
                   [(subs body 0 s)
                    [:i (subs body (inc s) (dec e))]
                    (if (seq more)
                      (x body more)
                      (subs body e))]))]])


(defn hiccup-inline [body]
  (let [[re fun] (first inline-replacements)]
    (if-let [m (seq (all-matches re body))]
      (fun body m)
      body)))

(defn all-matches [re string]
  (let [r (re-matcher re string)]
    (take-while vector?
                (repeatedly #(and (.find r)
                                  (let [s (.start r) e (.end r)]
                                    [s (- e s) (subs string s e)]))))))

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
  (apply vector (list-type tree) (map hiccup-list-item tree)))

(defn hiccup-list-item [[[marker text] & leaves]]
  (if-let [l (seq leaves)]
    [:li text (hiccup-list-body l)]
    [:li text]))

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
      block)))

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
      [(get block-elements tagname)
       (hiccup-for-lists (str/replace-first block match ""))]
      [:p (hiccup-for-lists block)])))

(defn to-hiccup [text]
  (let [blocks (str/split text #"\n\n")]
    (map hiccup-for-block blocks)))


(assert (= '([:h1 "hello"]) (to-hiccup "h1. hello\n\n")))

(assert (= '([:p [:ul [:li " hello \n" [:ul [:li " goodbye \n" [:ul [:li " three\n # a\n"]]]]] [:li " b \n" [:ul [:li " two \n"]]] [:li " again \n" [:ul [:li " two \n"]]] [:li " andf again"]]])
           (to-hiccup "* hello \n** goodbye \n*** three\n # a\n# b \n** two \n* again \n** two \n* andf again")))
