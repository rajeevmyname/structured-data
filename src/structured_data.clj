(ns structured-data
  (:require clojure.set))

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

(defn spiff [v]
  (let [x (get v 0)
        y (get v 2)]
    (if (and x y)
        (+ x y)
        "Vector is not large enough")))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[x y z v]]
  (if (and x z)
      (+ x z)
      "Vector is not large enough"))


(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- x2 x1))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y2 y1))))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
         [x3 y3] point]
    (and (or (>= x1 x3 x2) (<= x1 x3 x2))
         (or (>= y1 y3 y2) (<= y1 y3 y2)))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer (point x1 y1))
         (contains-point? outer (point x2 y2)))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [{v :authors} book]
    (assoc book :authors (conj v new-author))))

(defn alive? [author]
  (let [{n :death-year} author]
    (if (nil? n)
        true
        false)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [x] (first (rest x)))]
    (map second collection)))

(defn titles [books]
  (map :title books ))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [new-seq (set a-seq)
        original_count (count a-seq)
        new_count (count new-seq)]
    (> original_count new_count)))

(defn old-book->new-book [book]
  (let [{v :authors} book
        authors_set (set v)]
    (assoc book :authors authors_set)))

(defn has-author? [book author]
  (let [{s :authors} book]
    (contains? s author)))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [birth-year (:birth-year author)
        death-year (:death-year author)]
    (str (:name author)
         (if (:birth-year author) 
           (str " (" birth-year " - " death-year ")")
           ""))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (:authors book)]
    (str title
         ","
         " written by "
         (authors->string authors))))

(defn books->string [books]
  (let [book_count (count books)
        books_count_str (cond 
                          (== book_count 0) "No books"
                          (== book_count 1) "1 book. "
                          :else             (str book_count " books. "))
        list_of_books_str (apply str (interpose ", "
                                                (map book->string books)))]
    (str books_count_str list_of_books_str ".")))

(defn books-by-author [author books]
  (let [is-author? (fn [book] (has-author? book author))]
    (filter is-author? books)))

(defn author-by-name [name authors]
  (let [has_name? (fn [author] (= (:name author) name))]
   (first (filter has_name? authors))))

(defn living-authors [authors]
  (let [living? (fn [author] (not (:death-year author)))]
    (filter living? authors)))

(defn has-a-living-author? [book]
  (let [authors (:authors book)
        authors_alive (living-authors authors)]
    (not (empty? authors_alive))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
