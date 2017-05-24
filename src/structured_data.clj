(ns structured-data)

(defn do-a-thing [x]
  (let [thing (+ x x)]
    (Math/pow thing thing)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[first second third]]
  (+ first third))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (if (= (width rectangle) (height rectangle))
    true
    false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))


(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [x y] point]
    (if (and (<= x1 x x2) (<= y1 y y2))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let [[x y] inner]
    (if
      (and (contains-point? outer x) (contains-point? outer y))
      true
      false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (if (:death-year author)
    false
    true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element (fn [x] (get x 1))]
    (map second-element collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq))
    true
    false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (= (count (set a-seq)) (count a-seq))
    false
    true))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [author-names
        (fn [book]
          (set (:authors book)))]
    (apply clojure.set/union (map author-names books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [years
        (fn [author] (if (:birth-year author) (str " (" (:birth-year author) " - " (:death-year author) ")") ""))]
    (str (:name author) (years author))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [book-count->string
        (fn [books]
          (cond
            (= (count books) 0) "No books."
            (= (count books) 1) "1 book."
            (> (count books) 1) (str (count books) " books.")))
        book-strings (map book->string books)]

    (if (>= (count books) 1)
      (str (book-count->string books) (str " " (apply str (interpose ". " book-strings)) "."))
      (book-count->string books))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (let [correct-name?
        (fn [author] (= (:name author) name))]
    (first (filter correct-name? authors))))

(defn living-authors [authors]
  (let [is-alive?
        (fn [author] (alive? author))]
    (filter is-alive? authors)))


(defn has-a-living-author? [book]
  (let [living-authors-for-book
        (fn [book]
          (living-authors (:authors book)))]
       (if (empty? (living-authors-for-book book))
         false
         true)))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
