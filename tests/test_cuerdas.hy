(import math
        [cuerdas :as cds])

(defn nan?
  [s]
  (math.isnan s))

(defn empty-tests
  [func]
  (not (or (func None) (func "") (func " "))))

(defn test-lower []
  (assert (= None (cds.lower None)))
  (assert (= "" (cds.lower "")))
  (assert (= "foo" (cds.lower "FOO"))))

(defn test-upper []
  (assert (= None (cds.upper None)))
  (assert (= "" (cds.upper "")))
  (assert (= "FOO" (cds.upper "foo"))))

(defn test-collapse-whitespace []
  (assert (= None (cds.collapse-whitespace None)))
  (assert (= "a b" (cds.collapse-whitespace "a\u2003b")))
  (assert (= "a b" (cds.collapse-whitespace "a\u3000b")))
  (assert (= "a b c" (cds.collapse-whitespace "a  b\n c"))))

;; (defn test-locale-lower []
;;   (assert (= None (cds.locale-lower None)))
;;   (assert (= "foobar" (cds.locale-lower "foobar")))
;;   (assert (= "foobar" (cds.locale-lower "FOOBAR")))
;;   #?(:clj (assert (= "foobar" (cds.locale-lower "FOOBAR" Locale/UK)))))

;; (defn test-locale-upper []
;;   (assert (= None (cds.locale-upper None)))
;;   (assert (= "FOOBAR" (cds.locale-upper "foobar")))
;;   (assert (= "FOOBAR" (cds.locale-upper "FOOBAR")))
;;   #?(:clj (assert (= "FOOBAR" (cds.locale-upper "FOOBAR" Locale/UK)))))

(defn test-caseless= []
  (assert (= None (cds.caseless= None "foobar")))
  (assert (= None (cds.caseless= None None)))
  (assert (= True (cds.caseless= "foo" "Foo"))))

;; (defn test-locale-caseless= []
;;   (assert (= None (cds.locale-caseless= None "foobar")))
;;   (assert (= None (cds.locale-caseless= None None)))
;;   (assert (= False (cds.locale-caseless= "foo" None)))
;;   (assert (= True (cds.locale-caseless= "foo" "Foo")))
;;   #?(:clj
;;       (assert (= True (cds.locale-caseless= "foo" "Foo" Locale/UK)))))

(defn test-includes? []
  (assert (= True (cds.includes? "abc" "ab")))
  (assert (= True (cds.includes? "abc" "")))
  (assert (= False (cds.includes? "abc" "cba")))
  (assert (= False (cds.includes? "abc" None)))
  (assert (= None (cds.includes? None None)))
  (assert (= False (cds.includes? "abc" None)))
  (assert (= False (cds.includes? "abc" " ")))
  (assert (= True (cds.includes? "a bc" " "))))

(defn test-starts-with? []
  (assert (= None (cds.starts-with? None "ab")))
  (assert (= None (cds.starts-with? None None)))
  (assert (= True (cds.starts-with? "abc" "ab")))
  (assert (= False (cds.starts-with? "abc" "cab")))
  (assert (= False (cds.starts-with? "abc" None)))
  (assert (= True (cds.starts-with? "abc" "")))
  (assert (= False (cds.starts-with? "abc" "b")))
  (assert (= True (cds.starts-with? "abc" "a"))))

(defn test-ends-with? []
  (assert (= None (cds.ends-with? None None)))
  (assert (= None (cds.ends-with? None "bc")))
  (assert (= False (cds.ends-with? "abc" None)))
  (assert (= False (cds.ends-with? "abc" "bca")))
  (assert (= True (cds.ends-with? "abc" "bc")))
  (assert (= True (cds.ends-with? "abc" "")))
  (assert (= False (cds.ends-with? "abc" "b")))
  (assert (= True (cds.ends-with? "abc" "c"))))

(defn test-trim []
  (assert (= "a" (cds.trim " a ")))
  (assert (= None (cds.trim None)))
  (assert (= "a" (cds.trim "-a-" "-"))))

(defn test-ltrim []
  (assert (= "a " (cds.ltrim " a ")))
  (assert (= None (cds.ltrim None)))
  (assert (= "a-" (cds.ltrim "-a-" "-"))))

(defn test-rtrim []
  (assert (= " a" (cds.rtrim " a ")))
  (assert (= "a" (cds.rtrim "a" "foo")))
  (assert (= None (cds.rtrim None)))
  (assert (= "-a" (cds.rtrim "-a-" "-"))))

(defn test-empty? []
  (assert (cds.empty? ""))
  (assert (not (cds.empty? None)))
  (assert (not (cds.empty? " ")))
  (assert (not (cds.empty? " s "))))

(defn test-empty-or-none? []
    (assert (cds.empty-or-none? ""))
    (assert (cds.empty-or-none? None))
    (assert (not (cds.empty-or-none? " ")))
    (assert (not (cds.empty-or-none? " s "))))

  (defn test-blank? []
    (assert (cds.blank? ""))
    (assert (cds.blank? " "))
    (assert (not (cds.blank? None)))
    (assert (not (cds.blank? " s "))))

  (defn test-alpha? []
    (assert (empty-tests cds.alpha?))
    (assert (not (cds.alpha? "test1")))
    (assert (not (cds.alpha? "test.")))
    (assert (not (cds.alpha? "test\ntest")))
    (assert (cds.alpha? "Test")))

  (defn test-digits? []
    (assert (empty-tests cds.digits?))
    (assert (not (cds.digits? "test1")))
    (assert (not (cds.digits? "1.1")))
    (assert (not (cds.digits? "1\n1")))
    (assert (cds.digits? "0123")))

  (defn test-numeric? []
    (assert (cds.numeric? "1"))
    (assert (cds.numeric? "+1"))
    (assert (cds.numeric? "-1"))
    (assert (cds.numeric? "1.1"))
    (assert (cds.numeric? "+1.1"))
    (assert (cds.numeric? "+1.1e2"))
    (assert (not (cds.numeric? "_1.1")))
    (assert (not (cds.numeric? "test"))))

  (defn test-letters? []
    (assert (cds.letters? "hello"))
    (assert (cds.letters? "Русский"))
    (assert (cds.letters? "日本語"))
    (assert (cds.letters? "العربية")))

  (defn test-word? []
    (assert (cds.word? "world2000"))
    (assert (cds.word? "world⑤"))
    (assert (cds.word? "he_ll-o"))
    (assert (cds.word? "Русс_кий"))
    (assert (cds.word? "日_本-語"))
    (assert (cds.word? "ال-ع_ربية")))

  (defn test-uslug []
    (assert (is (cds.uslug None) None))
    (assert (= "русский-001-foobar" (cds.uslug "Русский 001   foobar"))))

  (defn test-alphanum? []
    (assert (empty-tests cds.alphanum?))
    (assert (cds.alphanum? "test1"))
    (assert (not (cds.alphanum? "test.1")))
    (assert (not (cds.alphanum? "test\n1")))
    (assert (cds.alphanum? "0A1B2C")))

  (defn test-repeat []
    (assert (= "a" (cds.repeat "a")))
    (assert (= "aaa" (cds.repeat "a" 3)))
    (assert (= None (cds.repeat None 3))))

  ;; (defn test-slice []
  ;;   (assert (= "bc" (cds.slice "abc" 1)))
  ;;   (assert (= "bc" (cds.slice "abcd" 1 3)))
  ;;   (assert (= "morning is upon u" (cds.slice "The morning is upon us." 4 -2)))
  ;;   (assert (= None (cds.slice None 1))))

  (defn test-strip-newlines []
    (assert (= None (cds.strip-newlines None)))
    (assert (= "a b" (cds.strip-newlines "a\n\nb"))))

  (defn test-split []
    (assert (= None (cds.split None)))
    (assert (= ["1" "2" "3"] (cds.split "1 2 3")))
    (assert (= ["1" "2" "3"] (cds.split "1 2 3" " ")))
    (assert (= ["1" "2" "3"] (cds.split "1 2 3" r"\s")))
    (assert (= ["1" "2 3"] (cds.split "1 2 3" r"\s" 1)))
    (assert (= ["1" "2" "3"] (cds.split "1 2 3" r" ")))
    (assert (= ["1" "2 3"] (cds.split "1,2 3" r",")))
    (assert (= ["1" "2 3"] (cds.split "1 2 3" r" " 1))))

  (defn test-replace []
    (assert (= None (cds.replace None "aa" "kk")))
    (assert (= "kk bb kk" (cds.replace "aa bb aa" "aa" "kk")))
    (assert (= "kk bb kk" (cds.replace "AA bb aa" "(?i)aa" "kk")))
    ;; (assert (= "aa bb cc" (cds.replace "aa bb cc" "(?:aa|bb)" "kk")))
    (assert (= "kk kk cc" (cds.replace "aa bb cc" "(?:aa|bb)" "kk"))))

  (defn test-replace-first []
    (assert (= None (cds.replace-first None "aa" "kk")))
    (assert (= "kk bb cc" (cds.replace-first "AA bb cc" "(?i)aa" "kk")))
    (assert (= "kk bb cc" (cds.replace-first "aa bb cc" "aa" "kk"))))

  (defn test-surround []
    (assert (= None (cds.surround None "-")))
    (assert (= "-aaa-" (cds.surround "aaa" "-")))
    (assert (= "-aaa-" (cds.surround "aaa" "-")))
    (assert (= "-^-aaa-^-" (cds.surround "aaa" "-^-"))))

  (defn test-unsurround []
    (assert (= None (cds.unsurround None "-")))
    (assert (= "aaa" (cds.unsurround "-aaa-" "-")))
    (assert (= "aaa" (cds.unsurround "-aaa-" "-")))
    (assert (= "aaa" (cds.unsurround "-^-aaa-^-" "-^-"))))

  ;; (defn test-chars []
  ;;   (assert (= None (cds.chars None)))
  ;;   (assert (= ["a" "b"] (cds.chars "ab"))))

  ;; (defn test-reverse []
  ;;   (assert (= None (cds.reverse None)))
  ;;   (assert (= "cba" (cds.reverse "abc")))
  ;;   (assert (= "anañam anañam rab 𝌆 oof"
  ;;            (cds.reverse "foo 𝌆 bar mañana mañana"))))

  ;; (defn test-prune []
  ;;   (assert (= None (cds.prune None 8)))
  ;;   (assert (= "Hello..." (cds.prune "Hello World" 8)))
  ;;   (assert (= "Hello (...)"
  ;;            (cds.prune "Hello World" 10 " (...)")))
  ;;   (assert (= "Hello world"
  ;;            (cds.prune "Hello world" 11 " (...)")))
  ;;   (assert (= "Hello World I'm pruning..."
  ;;            (cds.prune "Hello World I'm pruning strings today!" 25))))

  (defn test-join []
    (assert (= "ab" (cds.join ["a" "b"]))))

 ;; (defn test-quote []
 ;;    (assert (= None (cds.quote None)))
 ;;    (assert (= "\"a\"" (cds.quote "a")))
 ;;    (assert (= "\"\"\"" (cds.quote "\""))))

  ;; (defn test-unquote []
  ;;   (assert (= "\"" (cds.unquote "\"\"\"")))
  ;;   (assert (= "a" (cds.unquote "\"a\""))))

  (defn test-slug []
    (assert (= None (cds.slug None)))
    (assert (= "page-0001" (cds.slug "page 0001")))
    (assert (= "un-elephant-a-loree-du-bois"
             (cds.slug "Un éléphant à l'orée du bois"))))

  (defn test-clean []
    (assert (= None (cds.clean None)))
    (assert (= "a b" (cds.clean " a   b  ")))
    (assert (= "23.12.2014 10:09:19" (cds.clean "23.12.2014    10:09:19"))))

  ;; (defn test-escape-html []
  ;;   (assert (= "&lt;div&gt;Blah blah blah&lt;/div&gt;"
  ;;            (cds.escape-html "<div>Blah blah blah</div>"))))

  ;; (defn test-unescape-html []
  ;;   (assert (= "<div>Blah blah blah</div>"
  ;;            (cds.unescape-html "&lt;div&gt;Blah blah blah&lt;/div&gt;"))))

  ;; (defn test-strip-tags []
  ;;   (assert (= None (cds.strip-tags None)))
  ;;   (assert (= "just\ntext"
  ;;            (cds.strip-tags "just<br>text" {:br "\n"})))
  ;;   (assert (= "just some text"
  ;;            (cds.strip-tags "<p>just <b>some</b> text</p>")))
  ;;   (assert (= "just <b>some</b> text"
  ;;            (cds.strip-tags "<p>just <b>some</b> text</p>" ["p"])))
  ;;   (assert (= "just <b>some</b> text"
  ;;            (cds.strip-tags "<p>just <b>some</b> text</p>" "P"))))

  (defn test-parse-number []
    (assert (nan? (cds.parse-number None)))
    (assert (nan? (cds.parse-number "foobar")))
    (assert (= 1.4 (cds.parse-number "1.4")))
    (assert (= 1 (cds.parse-number "1"))))

  ;; (defn test-parse-double []
  ;;   (assert (nan? (cds.parse-double None)))
  ;;   (assert (nan? (cds.parse-double "foobar")))
  ;;   (assert (= 1.4 (cds.parse-double "1.4")))
  ;;   (assert (= 1.0 (cds.parse-double "1")))
  ;;   (assert (= 1.4 (cds.parse-double 1.4)))
  ;;   (assert (= 1.0 (cds.parse-double 1))))

  ;; (defn test-parse-int []
  ;;   (assert (nan? (cds.parse-int None)))
  ;;   (assert (nan? (cds.parse-int "foobar")))
  ;;   (assert (= 1 (cds.parse-int "1.4")))
  ;;   (assert (= 1 (cds.parse-int 1.4)))
  ;;   (assert (= 1 (cds.parse-int 1))))

  (defn test-to-bool []
    (assert (empty-tests cds.to-bool))
    (assert (cds.to-bool "1"))
    (assert (cds.to-bool "yes"))
    (assert (cds.to-bool "True"))
    (assert (cds.to-bool "on"))
    (assert (cds.to-bool "ON"))
    (assert (not (cds.to-bool "False")))
    (assert (not (cds.to-bool "hello"))))

  ;; (defn test-format []
  ;;   (assert (= None (cds.format None "pepe")))
  ;;   (assert (= "hello pepe" (cds.format "hello %s" "pepe")))
  ;;   (assert (= "hello pepe" (cds.format "hello %(name)s" {:name "pepe"}))))

  (defn test-pad []
    (assert (= None (cds.pad None :length 8)))
    (assert (= "       1" (cds.pad "1" :length 8)))
    (assert (= "00000001" (cds.pad "1" :length 8 :padding "0")))
    (assert (= "10000000" (cds.pad "1" :length 8 :padding "0" :type :right)))
    (assert (= "00001000" (cds.pad "1" :length 8 :padding "0" :type :both)))
    (assert (= "12345" (cds.pad "12345" :padding "0" :length 4))))

  (defn test-capital []
    (assert (= None (cds.capital None)))
    (assert (= "" (cds.capital "")))
    (assert (= "Foo" (cds.capital "foo")))
    (assert (= "FooBar" (cds.capital "fooBar"))))

  ;; (defn test-strip-prefix []
  ;;   (assert (= "ab" (cds.strip-prefix "ab" None)))
  ;;   (assert (= None (cds.strip-prefix None None)))
  ;;   (assert (= "a" (cds.strip-prefix "-=a" "-=")))
  ;;   (assert (= "a" (cds.strip-prefix "-a" \-)))
  ;;   (assert (= "=-a" (cds.strip-prefix "=-a" "-="))))

  ;; (defn test-strip-suffix []
  ;;   (assert (= "ab" (cds.strip-suffix "ab" None)))
  ;;   (assert (= None (cds.strip-suffix None None)))
  ;;   (assert (= "a" (cds.strip-suffix "a=-" "=-")))
  ;;   (assert (= "a" (cds.strip-suffix "a-" \-)))
  ;;   (assert (= "a-=" (cds.strip-suffix "a-=" "=-"))))

  ;; #?(:clj
  ;;    (defn test-strip-suffix []
  ;;      (assert (= None (cds.strip-suffix None "foo")))
  ;;      (assert (= "foobar" (cds.strip-suffix "foobar-" "-")))))

  (defn test-camel []
    (assert (= None (cds.camel None)))
    (assert (= "mozTransform" (cds.camel ':-moz-transform)))
    (assert (= "mozTransform" (cds.camel ':moz-transform)))
    (assert (= "mozTransform" (cds.camel "moz_transform")))
    (assert (= "mozTransform" (cds.camel "moz transform"))))

  (defn test-kebab []
   (assert (= None (cds.kebab None)))
   (assert (= "moz" (cds.kebab "MOZ")))
   (assert (= "dasherized-keyword" (cds.kebab ':dasherized-keyword)))
   (assert (= "moz-transform" (cds.kebab "MozTransform"))))


  (defn test-snake []
    (assert (= None (cds.snake None)))
    (assert (= "user_table" (cds.snake ':user-table)))
    (assert (= "moz_transform" (cds.snake "MozTransform"))))

  (defn test-phrase []
    (assert (= None (cds.phrase None)))
    (assert (= "A short phrase" (cds.phrase ':a-short-phrase)))
    (assert (= "Capitalize dash camel case underscore trim"
             (cds.phrase "  capitalize dash-CamelCase_underscore trim  "))))

  (defn test-human []
    (assert (= None (cds.human None)))
    (assert (= "human friendly" (cds.human ':human-friendly)))
    (assert (= "nice for people to read" (cds.human "NiceForPeopleToRead"))))

  (defn test-title []
    (assert (= None (cds.title None)))
    (assert (= "Mini²" (cds.title "mini²")))
    (assert (= "Mini's" (cds.title "mini's")))
    (assert (= "Mini S" (cds.title "mini-s")))
    (assert (= "Mini(s)" (cds.title "mini(s)")))
    (assert (= "My Name Is Epeli" (cds.title "my name is epeli")))
    (assert (= "Regular Keyword" (cds.title ':regular-keyword))))

  (defn test-pascal []
    (assert (= None (cds.pascal None)))
    (assert (= "SomeKeywordName" (cds.pascal ':*some-keyword-name*)))
    (assert (= "SomeClassName" (cds.pascal "_some_class_name_"))))

  (defn test-js-selector []
    (assert (= None (cds.js-selector None)))
    (assert (= "keywordizeKeys" (cds.js-selector "keywordize-keys")))
    (assert (= "SomeKeywordName" (cds.js-selector ':-some-keyword-name)))
    (assert (= "SomeClassName" (cds.js-selector "_some_class_name"))))

  (defn test-css-selector []
    (assert (= None (cds.css-selector None)))
    (assert (= "some-keyword-name" (cds.css-selector "someKeywordName")))
    (assert (= "-some-keyword-name" (cds.css-selector ':SomeKeywordName)))
    (assert (= "-some-keyword-name" (cds.css-selector "SomeKeywordName"))))

  (defn test-keyword []
    (assert (= None (cds.css-selector None)))
    (assert (= ':keyword-this (cds.keyword " keyword this")))
    ;; (assert (= ':bar-foo/key-this (cds.keyword "bar-foo" " Key_This")))
    ;; (let [n "foo-bar"]
    ;;   (assert (= ':foo-bar/key-that (cds.keyword n "KeyThat"))))
    )

  (defn test-unlines []
    (assert (= None (cds.unlines None)))
    (assert (= "foo\nbar" (cds.unlines ["foo" "bar"])))
    (assert (= "" (cds.unlines []))))

  (defn test-words []
    (assert (= None (cds.words None)))
    (assert (= [] (cds.words "")))
    (assert (= ["test"] (cds.words "test")))
    (assert (= ["one" "two" "3"] (cds.words "  one, two 3.  ")))
    (assert (= ["re" "test."] (cds.words " re,  test." r"[^, ]+"))))
