;; * Cuerdas
;; ** Imports
(import regex
        math
        [numbers [Number]])
(require [hy.extra.anaphoric [*]])

;; ** Helpers
(defmacro defall [#* syms]
  `(setv __all__ ~(lfor sym syms (mangle (str sym)))))

(defall empty? empty-or-none? includes? starts-with? ends-with? lower upper caseless=
  blank? alpha? digits? alphanum? word? letters? numeric? trim rtrim ltrim clean strip rstrip
  repeat replace replace-first strip-newlines split reverse chars lines unlines words join
  surround unsurround quote-str unquote-str stylize capital camel snake phrase human title
  pascal kebab js-selector css-selector slug uslug keyword parse-number parse-float parse-int
  one-of? to-bool pad collapse-whitespace strip-tags unindent)

(setv keyword* hy.models.Keyword)

(defmacro some->
  [expr #* forms]
  "When expr is not nil, threads it into the first form (via ->),
  and when that result is not nil, through the next etc"
  (setv g (gensym)
        steps (list (map (fn [step] `(if (none? ~g) None (-> ~g ~step)))
                         forms)))
  `(do
     (setv ~g ~expr
           ~@(interleave (repeat g) (butlast steps)))
     ~(if (empty? steps)
          g
          (last steps))))

;; ** Lib
(defn empty?
  [s]
  "Checks if a string is empty."
  (when (string? s)
    (zero? (len s))))

(defn empty-or-none?
  [s]
  "Conveinent helper for check emptines or if value is none."
  (or (none? s)
      (empty? s)))

(defn includes?
  [s subs]
  "Determines whether a string contains a substring"
  (when (string? s)
    (if (none? subs)
        False
        (in subs s))))

(defn starts-with?
  [s prefix]
  "Check if the string starts with prefix"
  (when (string? s)
    (cond
      [(none? prefix) False]
      [(empty? prefix) True]
      [:else (.startswith s prefix)])))

(defn ends-with?
  [s suffix]
  "Check if the string ends with suffix."
  (when (string? s)
    (cond
      [(none? suffix) False]
      [(none? suffix) False]
      [(empty? suffix) True]
      [:else (str.endswith s suffix)])))

(defn lower
  [s]
  "Converts string to all lower-case"
  (when (string? s)
    (str.lower s)))

(defn upper
  [s]
  "Converts string to all upper-case"
  (when (string? s)
    (str.upper s)))

(defn caseless=
  [s1 s2]
  "Compare strings in a case-insensitive manner."
  (when (string? s1)
    (= (lower s1) (lower s2))))

(defn blank?
  [s]
  "Checks if a string is empty or contains only whitespace."
  (when (string? s)
    (or (zero? (len s))
        (-> (regex.compile r"[\s\p{Z}]+$")
            (regex.match s)
            bool))))

(defn alpha?
  [s]
  "Checks if a string contains only alpha characters."
  (when (string? s)
    (-> r"^[a-zA-Z]+$"
        (regex.match s)
        bool)))

(defn digits?
  [s]
  "Checks if a string contains only digit characters."
  (when (string? s)
    (-> r"^[0-9]+$"
        (regex.match s)
        bool)))

(defn alphanum?
  [s]
  "Checks if a string contains only alphanumeric characters."
  (when (string? s)
    (-> r"^[a-zA-Z0-9]+$"
        (regex.match s)
        bool)))

(defn word?
  [s]
  "Checks if a string contains only the word characters."
  (when (string? s)
    (-> r"^[\p{N}\p{L}_-]+$"
        regex.compile
        (regex.match s)
        bool)))

(defn letters?
  [s]
  "Checks if a string contains only letters.
  This function will use all the unicode range."
  (when (string? s)
    (-> r"^\p{L}+$"
        regex.compile
        (regex.match s)
        bool)))

(defn numeric?
  [s]
  "Checks if a string contains only numeric values."
  (when (string? s)
    (-> r"^[+-]?([0-9]*\.?[0-9]+|[0-9]+\.?[0-9]*)([eE][+-]?[0-9]+)?$"
        regex.compile
        (regex.match s)
        bool)))

(defn trim
  [s [chs None]]
  "Removes whitespace or specific characters
  from both ends of string."
  (when (string? s)
    (if chs
        (str.strip s chs)
        (str.strip s))))

(defn rtrim
  [s [chs None]]
  "Removes whitespace or specific characters
  from right side of string."
  (when (string? s)
    (if chs
        (str.rstrip s chs)
        (str.rstrip s))))

(defn ltrim
  [s [chs None]]
  "Removes whitespace or specific characters
  from left side of string."
  (when (string? s)
    (if chs
        (str.lstrip s chs)
        (str.lstrip s))))

(defn clean
  [s]
  "Trim and replace multiple spaces with a single space."
  (when (string? s)
    (-> r"[\s\p{Z}]+"
        regex.compile
        (regex.sub " " (trim s)))))

(setv strip trim)
(setv rstrip rtrim)
(setv lstrip ltrim)

(defn repeat
  [s [n 1]]
  "Repeats string n times."
  (when (and (string? s)
             (isinstance n int))
    (* s n)))

(defn replace
  [s match replacement]
  "Replaces all instance of match with replacement in s.

  The replacement is literal (i.e. none of its characters are treated
  specially) for all cases above except pattern / string.

  In match is pattern instance, replacement can contain $1, $2, etc.
  will be substituted with string that matcher the corresponding
  parenthesized group in pattern.

  If you wish your replacement string to be used literary,
  use `(cuerdas.regexp/escape replacement)`.

  Example:
    (replace \"Almost Pig Latin\" #\"\\b(\\w)(\\w+)\\b\" \"$2$1ay\")
    ;; => \"lmostAay igPay atinLay\"
  "
  (when (string? s)
    (regex.sub match replacement s)))

(defn replace-first
  [s match replacement]
  "Replaces first instance of match with replacement in s."
  (when (string? s)
    (regex.sub match replacement s :count 1)))


;; (defn prune
;;   [s num &optional [subs "..."]]
;;   "Truncates a string to a certain length and adds '...'
;;   if necessary."
;;   (if (<= (len s) num)
;;       s
;;       (do (setv tmpl (fn [c] (if (!= (upper c) (lower c)) "A" " "))
;;                 template (-> (cut s 0 (inc num))
;;                              spy
;;                              (replace r".(?=\W*\w*$)" tmpl)
;;                              spy)
;;                 tmp (cut template (- (len template) 1))
;;                 _ (spy tmp)
;;                 template (if (regex.match r"\w\w" tmp)
;;                              (replace-first template r"\s*\S+$" "")
;;                              (rtrim (cut template 0 (dec (len template))))))
;;           (if (> (len (+ template subs)) (len s))
;;               s
;;               (+ (cut s 0 (len template)) subs)))))

(defn strip-newlines
  [s]
  "Takes a string and replaces newlines with a space.
  Multiple lines are replaced with a single space."
  (replace s r"[\n\r|\n]+" " "))

(defn split
  [s [sep r"\s+"] [num None]]
  (if-not num
      (cond
        [(none? s) s]
        [(string? sep) (regex.split (regex.compile sep) s)]
        [:else (raise (ValueError "Invalid arguments."))])
      (cond
        [(none? s) s]
        [(string? sep) (regex.split (regex.compile sep) s num)]
        [:else (rase (ValueError "Invalid arguments."))])))

(defn reverse
  [s]
  "Return string reversed."
  (when (string? s)
    (cut s None None -1)))

(defn chars
  [s]
  "Split a string in a seq of chars."
  (when (string? s)
    (lfor c s c)))

(defn lines
  [s]
  "Return a list of the lines in the string."
  (split s r"\n|\r\n"))

(defn unlines
  [s]
  "Returns a new string joining a list of strings with a newline char (\\n)."
  (when (coll? s)
    (.join "\n" s)))

(defn words
  [s [re r"[\p{N}\p{L}_-]+"]]
  "Returns a vector of the words in the string."
  (if re
      (when (string? s)
        (list (regex.findall re s)))))

(defn join
  [coll [separator None]]
  "Joins strings together with given separator."
  (if separator
      (.join separator coll)
      (.join "" coll)))

(defn surround
  [s wrap]
  "Surround a string with another string or character."
  (when (string? s)
    (.join "" [wrap s wrap])))

(defn unsurround
  [s surrounding]
  "Unsurround a string surrounded by another string or character."
  (when (string? s)
    (setv surrounding (str surrounding)
          length (len surrounding)
          fstr (cut s 0 length)
          slength (len s)
          rightend (- slength length)
          lstr (cut s rightend slength))
    (if (and (= fstr surrounding) (= lstr surrounding))
        (cut s length rightend)
        s)))

(defn quote-str
  [s [qchar "\""]]
  "Quotes a string."
  (surround s qchar))

(defn unquote-str
  [s [qchar "\""]]
  (unsurround s qchar))

(defn -stylize-split
  [s]
  (setv re1 (regex.compile r"(\p{Lu}+[\p{Ll}'\p{Ps}\p{Pe}]*)")
        re2 (regex.compile r"[^\p{L}\p{N}'\p{Ps}\p{Pe}]+"))
  (->> s
      str
      (regex.sub re1 r"-\1")
      (regex.split re2)
      list))

(defn -stylize-join
  [coll join-with first-fn [rest-fn None]]
  (when (iterable? coll)
    (setv (, head #* tail) coll)
    (if rest-fn
        (.join join-with (chain [(first-fn head)] (map rest-fn tail)))
        (.join join-with (map first-fn coll)))))

(defn stylize
  [s first-fn join-with [rest-fn None]]
    (setv remove-empty (fn [seq] (remove empty? seq))
          rest-fn (if-not rest-fn first-fn rest-fn))
   (-> s
       -stylize-split
       remove-empty
       (-stylize-join join-with first-fn rest-fn )))

(defn capital
  [s]
  "Uppercases the first character of a string"
  (if (empty-or-none? s)
      s
      (+ (upper (first s)) (cut s 1))))

(defn camel
  [s]
  "Output will be: lowerUpperUpperNoSpaces
  accepts strings and keywords"
  (stylize s str.lower "" str.capitalize))

(defn snake
  [s]
  "Output will be: lower_cased_and_underscore_separated
  accepts strings and keywords"
  (stylize s str.lower "_"))

(defn phrase
  [s]
  "Output will be: Space separated with the first letter capitalized.
  accepts strings and keywords"
  (stylize s str.capitalize " " str.lower ))

(defn human
  [s]
  "Output will be: lower cased and space separated
  accepts strings and keywords"
  (stylize s str.lower " "))

(defn title
  [s]
  "Output will be: Each Word Capitalized And Separated With Spaces
  accepts strings and keywords"
  (stylize s str.capitalize " "))

(defn pascal
  [s]
  "Output will be: CapitalizedAndTouchingTheNext
  accepts strings and keywords"
  (stylize s str.capitalize ""))

(defn kebab
  [s]
  "Output will be: lower-cased-and-separated-with-dashes
  accepts strings and keywords"
  (stylize s str.lower "-"))

(defn js-selector
  [s]
  "Output will be either:
     (js-selector \"-pascal-case-me\") ;; => PascalCaseMe
     (js-selector \"camel-case-me\") ;; => camelCaseMe
  accepts keywords and strings, with any standard delimiter"
  (-stylize-join (-stylize-split s) "" identity capital))

(defn css-selector
  [s]
  "Output will be either:
     (js-selector \"LeadingDash\") ;; => -leading-dash
     (js-selector \"noLeadingDash\") ;; => no-leading-dash
  accepts keywords and strings, with any standard delimiter"
  (some-> s
      -stylize-split
      (-stylize-join "-" lower)))

(setv -slug-tr-map
      (dict (zip "ąàáäâãåæăćčĉęèéëêĝĥìíïîĵłľńňòóöőôõðøśșšŝťțŭùúüűûñÿýçżźž"
                 "aaaaaaaaaccceeeeeghiiiijllnnoooooooossssttuuuuuunyyczzz")))

(defn slug
  [s]
  "Transform text into a URL slug."
  (some-> (lower s)
          (.translate (str.maketrans -slug-tr-map))
          (replace r"[^\w\s]+" "")
          (replace r"\s+" "-")))

(defn uslug
  [s]
  "Unicode friendly version of `slug` function."
  (some-> (lower s)
          (replace r"[^\p{L}\p{N}+]" " ")
          (replace r"[\p{Z}\s]+" "-")))

(defn keyword
  [k]
  "Safer version of clojure keyword, accepting a
  symbol for the namespace and kebab-casing the key"
  (keyword* (kebab k)))

(defn parse-number
  [s]
  "General purpose function for parse number like
  string to number. It works with both integers
  and floats."
  (if (none? s)
      math.nan
      (if (numeric? s)
          (try (int s) (except [ValueError] (float s)))
          math.nan)))

(defn parse-float
  [s]
  "Return the double value from string."
  (cond
    [(instance? Number s) (float s)]
    [(string? s) (try (float s) (except [ValueError] math.nan))]
    [:else math.nan]))

(defn parse-int
  [s]
  "Return the number value in integer form."
  (cond
    [(instance? Number s) (int s)]
    [(and (string? s)
          (numeric? s))
     (int s)]
    [:else math.nan]))

(defn one-of?
  [coll s]
  "Returns true if s can be found in coll."
  (bool (some (fn [x] (= x s)) coll)))

(defn to-bool
  [s]
  "Returns true for 1/on/true/yes string values (case-insensitive),
  false otherwise."
  (one-of? ["1" "on" "true" "yes"] (lower s)))

(defn pad
  [s [length 0] [padding " "] [type :left]]
  "Pads the str with characters until the total string
  length is equal to the passed length parameter. By
  default, pads on the left with the space char."
  (when (string? s)
    (setv padding (cut padding 0 1)
          padlen (- length (len s))
          padlen (if (< padlen 0) 0 padlen))
    (cond
      [(= type :right ) (.join "" [s #* (repeat padding padlen)])]
      [(= type :both) (.join ""
                             [#* (repeat padding (math.ceil (/ padlen 2)))
                              s
                              #* (repeat padding (math.floor (/ padlen 2)))])]
      [:left (.join "" [#* (repeat padding padlen) s])])))

(defn collapse-whitespace
  [s]
  "Converts all adjacent whitespace characters"
  (some-> s
          (replace r"[\p{Z}\s]+" " ")
          (replace r"^\s+|\s+$" "")))

;; (defn escape-html
;;   [s]
;;   "Converts HTML special characters to their entity equivalents.")

;; (defn unescape-html
;;   [s]
;;   "Converts entity characters to HTML equivalents.")

(defn -strip-tags-impl
  [s tags mappings]
  (setv kwdize (comp keyword lower #%(getattr %1 "name"))
        tags (cond
               [(none? tags) tags]
               [(string? tags) (set (kwdize tags))]
               [(coll? tags) (set (map kwdize tags))])
        rx (regex.compile r"<\/?([^<>]*)>"))
  (replace s rx (if (none? tags)
                    (fn [match] (.get mappings (kwdize (.group match 0)) ""))
                    (fn [match]
                      (setv tag (.group match 0)
                            ktag (kwdize tag))
                      (if (in ktag tags)
                          (.get mappings ktag "")
                          tag)))))

(defn strip-tags
  [s [tags None] [mapping None]]
  "Remove html tags from string."
  (cond
    [(and (none? tags) (none? mapping)) (-strip-tags-impl s None {})]
    [(and tags (none? mapping))
     (if (instance? dict tags)
         (-strip-tags-impl s None tags)
         (-strip-tags-impl s tags {}))]
    [(and tags mapping) (-strip-tags-impl s tags mapping)]))

(defn unindent
  [s [r None]]
  (defn helper
    [s r]
    (->> s lines (map (fn [x] (replace x r ""))) unlines))

  (if r
      (helper s r)
      (do (setv all-indents (->> (rest (lines s))
                                 (remove blank?)
                                 list
                                 (+ [(last (lines s))])
                                 (map (fn [x]
                                        (as-> x $
                                             (regex.finditer r"^( +)" $)
                                             (next $)
                                             (.group $ 0)
                                             (len $)
                                             ))))
                min-indent (min all-indents)
                r (regex.compile (+ "^ {" (str min-indent) "}")))
          (helper s r))))
