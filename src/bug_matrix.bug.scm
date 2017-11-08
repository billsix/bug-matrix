;;;Copyright 2017 - William Emerison Six
;;;All rights reserved
;;;Distributed under LGPL 2.1 or Apache 2.0


(##include "config.scm")

{at-both-times
 {define-structure matrix rows}}

{unit-test
 (equal? (matrix-rows (make-matrix '((1 2 3)
                                     (4 5 6)
                                     (7 8 9))))
         '((1 2 3)
           (4 5 6)
           (7 8 9)))}

{define matrix-row
  [|m r| (make-matrix (list
                       (list-ref (matrix-rows m)
                                 (- r 1))))]}

{unit-test
 {let ((m (make-matrix '((1 2 3)
                         (4 5 6)
                         (7 8 9)))))
   (satisfies?
    [|n| (matrix-row m n)]
    `((1 ,(make-matrix '((1 2 3))))
      (2 ,(make-matrix '((4 5 6))))
      (3 ,(make-matrix '((7 8 9))))))
   }
 }

{define matrix-column
  [|m c| (make-matrix
          (map [|r| (list (list-ref r (- c 1)))]
               (matrix-rows m)))]}

{unit-test
 {let ((m (make-matrix '((1 2 3)
                         (4 5 6)
                         (7 8 9)))))
   (satisfies?
    [|n| (matrix-column m n)]
    `((1 ,(make-matrix '((1)
                         (4)
                         (7))))
      (2 ,(make-matrix '((2)
                         (5)
                         (8))))
      (3 ,(make-matrix '((3)
                         (6)
                         (9))))))}
 }


{define matrix-column-vector?
  [|m| (all? (map [|x| (equal? 1 (length x))]
                  (matrix-rows m)))]}

{unit-test
 (matrix-column-vector? (make-matrix '((1)
                                       (2)
                                       (3))))
 (all? (map [|i|
             (matrix-column-vector?
              (matrix-column (make-matrix '((1 2 3)
                                            (4 5 6)
                                            (7 8 9)))
                             i))]
            '(1 2 3)))
 }


{define matrix-row-vector?
  [|m|
   (not (any? (map pair? (list-ref (matrix-rows m)
                                   0))))]}
{unit-test
 {let ((m (make-matrix '((1 2 3)
                         (4 5 6)
                         (7 8 9)))))
   (satisfies?
    [|n| (matrix-row-vector? (matrix-row m n))]
    `((1 #t)
      (2 #t)
      (3 #t)))
   }
 }


{define matrix-element-at
  [|m r c| (list-ref (car
                      (matrix-rows (matrix-row m r)))
                     (- c 1))]}

{unit-test
 {let ((m (make-matrix '((1 2 3)
                         (4 5 6)
                         (7 8 9)))))
   (satisfies?
    [|pair| (matrix-element-at m (car pair) (cadr pair))]
    '(((1 1) 1)
      ((1 2) 2)
      ((1 3) 3)
      ((2 1) 4)
      ((2 2) 5)
      ((2 3) 6)
      ((3 1) 7)
      ((3 2) 8)
      ((3 3) 9)
      ))}
 }

{define matrix-size
  [|m| (list
        (length (matrix-rows m))
        (length (car (matrix-rows (matrix-row m 1)))))]}

{unit-test
 {let ((m (make-matrix '((1 2 3)
                         (4 5 6)
                         (7 8 9)))))
   {destructuring-bind (rows columns)
                       (matrix-size m)
      {and (equal? rows 3)
           (equal? columns 3)}}}
 }

{define matrix-square?
  [|m|  {destructuring-bind (rows columns)
                            (matrix-size m)
           (equal? rows columns)}]}

{unit-test
 (matrix-square? (make-matrix '((1 2 3)
                                (4 5 6)
                                (7 8 9))))
 (matrix-square? (make-matrix '((1 2)
                                (4 5))))
 (not (matrix-square? (make-matrix '((1 2)
                                     (4 5)
                                     (7 8)))))
 }

{define matrix-indices
  [|m|
   (cartesian-product (map [|x| (stream->list
                                 (stream-enumerate-interval 1 x))]
                           (matrix-size m)))]}
{unit-test
 (equal? (matrix-indices (make-matrix '((1 2 3)
                                        (4 5 6))))
         '((1 1)
           (1 2)
           (1 3)
           (2 1)
           (2 2)
           (2 3)
           ))
 }


{define matrix-diagonal?
  [|m|
   {and (matrix-square? m)
        {destructuring-bind (same-index different-index)
                            (partition (matrix-indices m)
                                       [|x| (equal? (car x)
                                                    (cadr x))])
           {and (all? (map [|i| (not
                                 (equal? 0
                                         (apply matrix-element-at
                                                (cons m i))))]
                           same-index))
                (all? (map [|i| (equal? 0
                                        (apply matrix-element-at
                                               (cons m i)))]
                           different-index))}}}]}

{unit-test
 (matrix-diagonal? (make-matrix '((1 0 0)
                                  (0 5 0)
                                  (0 0 9))))
 (not (matrix-diagonal? (make-matrix '((1 2 3)
                                       (4 5 4)
                                       (7 8 9)))))
 (not (matrix-diagonal? (make-matrix '((1 0 0)
                                       (0 5 0)
                                       (0 8 9)))))
 (not (matrix-diagonal? (make-matrix '((1 0 0)
                                       (0 5 0)
                                       (0 0 0)))))
 }

{define matrix-zero?
  [|m|
   (all? (map [|i| (equal? 0 (apply matrix-element-at
                                    (cons m i)))]
              (matrix-indices m)))]}
{unit-test
 (matrix-zero? (make-matrix '((0 0 0)
                              (0 0 0)
                              (0 0 0))))
 (not (matrix-zero? (make-matrix '((1 0 0)
                                   (0 0 0)
                                   (0 0 0)))))
 (not (matrix-zero? (make-matrix '((0 0 0)
                                   (1 0 0)
                                   (0 0 0)))))
 (not (matrix-zero? (make-matrix '((0 0 0)
                                   (0 0 1)
                                   (0 0 0)))))
 (not (matrix-zero? (make-matrix '((0 0 0)
                                   (0 0 0)
                                   (0 1 0)))))
 }


{define matrix-upper-triangular?
  [|m|
   {and (matrix-square? m)
        {destructuring-bind (below-diagonal diagonal-or-above)
                            (partition (matrix-indices m)
                                       [|x| (> (car x)
                                               (cadr x))])
            (all? (map [|i| (equal? 0
                                    (apply matrix-element-at
                                           (cons m i)))]
                       below-diagonal))}}]}
{unit-test
 (matrix-upper-triangular? (make-matrix '((1 2 3)
                                          (0 5 4)
                                          (0 0 1))))
 (matrix-upper-triangular? (make-matrix '((1 0 0)
                                          (0 5 0)
                                          (0 0 0))))
 (matrix-upper-triangular? (make-matrix '((0 0 0)
                                          (0 0 0)
                                          (0 0 0))))
 (not (matrix-upper-triangular? (make-matrix '((1 2 3)
                                               (4 5 4)
                                               (0 0 0)))))
 (not (matrix-upper-triangular? (make-matrix '((1 2 3)
                                               (0 5 4)
                                               (0 2 0)))))
 }

{define matrix-lower-triangular?
  [|m|
   {and (matrix-square? m)
        {destructuring-bind (above-diagonal diagonal-or-below)
                            (partition (matrix-indices m)
                                       [|x| (< (car x)
                                               (cadr x))])
            (all? (map [|i| (equal? 0
                                    (apply matrix-element-at
                                           (cons m i)))]
                       above-diagonal))}}]}
{unit-test
 (matrix-lower-triangular?
  (make-matrix '((1 0 0)
                 (1 5 0)
                 (1 1 1))))
 (matrix-lower-triangular?
  (make-matrix '((1 0 0)
                 (1 5 0)
                 (0 0 0))))
 (matrix-lower-triangular?
  (make-matrix '((0 0 0)
                 (0 0 0)
                 (0 0 0))))
 (not (matrix-lower-triangular?
       (make-matrix '((0 1 0)
                      (0 0 0)
                      (0 0 0)))))
 (not (matrix-lower-triangular?
       (make-matrix '((0 0 1)
                      (0 0 0)
                      (0 0 0)))))
 (not (matrix-lower-triangular?
       (make-matrix '((0 0 0)
                      (0 0 1)
                      (0 0 0)))))
 }

{define matrix-sort-by-diagonal-values
  [|m|
   ;; N.B.  In this procedure, 0-indexing is used instead
   ;; of the matrix-style 1-indexing
   {destructuring-bind (rows columns)
                       (matrix-size m)
     (make-matrix
      {let loop ((data (matrix-rows m))
                 (current-row 0))
        (if (equal? current-row (- rows 1))
            [data]
            [{destructuring-bind
                  (row-of-max-diagonal-value #!rest to-be-sorted)
                  (sort data [|r1 r2| (> (list-ref r1 current-row)
                                       (list-ref r2 current-row))])
               (cons row-of-max-diagonal-value
                     (loop to-be-sorted
                           (+ current-row 1)))}])})}]}

{unit-test
 (satisfies?
  matrix-sort-by-diagonal-values
  `(
    ;; sort by 7, then by 5, then nothing to sort
    (,(make-matrix '((1 2 3)
                     (4 5 6)
                     (7 8 9)))
     ,(make-matrix '((7 8 9)
                     (4 5 6)
                     (1 2 3))))
    ;; sort by 7, then by 5, then nothing to sort
    (,(make-matrix '((1 5 3)
                     (4 2 6)
                     (7 8 9)))
     ,(make-matrix '((7 8 9)
                     (1 5 3)
                     (4 2 6))))
     ))
 }


{define matrix-row-set!
  [|m r newrow|
   ;; N.B.  In this procedure, 0-indexing is used instead
   ;; of the matrix-style 1-indexing
   (make-matrix
    {let ((foo (matrix-rows m)))
      (setf! (list-ref foo (- r 1)) newrow)})]}

{unit-test
 {let ((m (make-matrix '((1 2 3)
                         (4 5 6)
                         (7 8 9)))))
   (matrix-row-set! m 1 '(10 11 12))
   (equal? m (make-matrix '((10 11 12)
                            (4 5 6)
                            (7 8 9))))}
 {let ((m (make-matrix '((1 2 3)
                         (4 5 6)
                         (7 8 9)))))
   ;; N.B setf!
   {setf! (matrix-row m 2) '(10 11 12)}
   (equal? m (make-matrix '((1 2 3)
                            (10 11 12)
                            (7 8 9))))}
 {let ((m (make-matrix '((1 2 3)
                         (4 5 6)
                         (7 8 9)))))
   {setf! (matrix-row m 3) '(10 11 12)}
   (equal? m (make-matrix '((1 2 3)
                            (4 5 6)
                            (10 11 12))))}
 {let ((m (make-matrix '((1 2 3)
                         (4 5 6)
                         (7 8 9)))))
   {setf! (matrix-row m 3) '(10 11 12)}
   (equal? m (make-matrix '((1 2 3)
                            (4 5 6)
                            (10 11 12))))}
 ;; N.B mutate!
 {let ((m (make-matrix '((1 2 3)
                         (4 5 6)
                         (7 8 9)))))
   {mutate! (matrix-row m 3) [|row-vector|
                              (map [|x| (* x x)]
                                   (car
                                    (matrix-rows row-vector)))]}
   (equal? m (make-matrix '((1 2 3)
                            (4 5 6)
                            (49 64 81))))}
 }


;; todo -- does this really need to mutate?
{define rref!
  [|m|
   {destructuring-bind (rows columns)
                       (matrix-size m)
       ;; in the case of an inconsistent matrix, return early
       (call/cc
        [|return|
         {let loop ((m {begin
                         (mutate! m matrix-sort-by-diagonal-values)
                         m})
                    (row-index 1)
                    (column-index 1))
           ;; set the leading term to 1
           {mutate! (matrix-row m row-index)
                    [|r| {let denominator-loop ((denominator
                                                 (matrix-element-at
                                                  m
                                                  row-index
                                                  column-index)))
                           (if (equal? denominator 0)
                               [{set! column-index (+ 1 column-index)}
                                (if (> column-index columns)
                                    ;; inconsistent solution, exit
                                    [(return m)]
                                    ;; keep looking for that non-zero value
                                    [(denominator-loop (matrix-element-at
                                                        m
                                                        row-index
                                                        column-index))])]
                               [(map [|x| (/ x denominator)]
                                     (car (matrix-rows r)))])}]}
           ;; set the subsequent row's leading term to 0
           (for-each [|r|
                      {mutate!
                       (matrix-row m r)
                       [|row-data|
                        {let ((cancel (map [|q| (* q
                                                   (- (list-ref
                                                       (car (matrix-rows row-data))
                                                       (- column-index 1))))]
                                           (car
                                            (matrix-rows
                                             (matrix-row m
                                                         row-index))))))
                          (zip-with +
                                    cancel
                                    (car (matrix-rows row-data)))}]}]
                     (stream->list
                      (stream-filter [|x| (not (equal? x row-index))]
                                     (stream-enumerate-interval 1 rows))))
           ;; repeat for subsequent rows, or return
           (if (< row-index rows)
               [(loop {begin
                        (mutate! m matrix-sort-by-diagonal-values)
                        m}
                      (+ row-index 1)
                      (+ column-index 1))]
               [m])}])}]}

{unit-test
 ;; one solution
 {let ((m (make-matrix '((2 8 4 2)
                         (2 5 1 5)
                         (4 10 -1 1)))))
   (equal? (rref! m)
           (make-matrix '((1 0 0 11)
                          (0 1 0 -4)
                          (0 0 1 3))))}
 ;; infinite solutions
 {let ((m (make-matrix '((1 2 3 5)
                         (4 5 6 6)
                         (7 8 9 7)))))
   (equal? (rref! m)
           (make-matrix '((1 0 -1 -13/3)
                          (0 1 2 14/3)
                          (0 0 0 0))))}
 ;; no solution
 {let ((m (make-matrix '((1  -3  0  -5  -7)
                         (3  -12 -2 -27 -33)
                         (-2 10  2  24  29)
                         (-1  6  1  14  17)
                         ))))
   (equal? (rref! m)
           (make-matrix '((1 0 0 1 0)
                          (0 1 0 2 0)
                          (0 0 1 3 0)
                          (0 0 0 0 1))))}
 }

{define matrix-rank
  [|m|
   ;; todo, if I'm gonna mutate, why do I have to set!
   (set! m (rref! m))
   {destructuring-bind (rows columns)
                       (matrix-size m)
      (length (stream->list
               (stream-take-while
                [|x| (equal? 1 x)]
                (stream-map
                 [|x| (matrix-element-at m x x)]
                 (stream-enumerate-interval 1 rows)))))}]}

{unit-test
 ;; one solution
 {let ((m (make-matrix '((2 8 4 2)
                         (2 5 1 5)
                         (4 10 -1 1)))))
   {and (equal? (rref! m)
                (make-matrix '((1 0 0 11)
                               (0 1 0 -4)
                               (0 0 1 3))))
        (equal? (matrix-rank m) 3)}}
 ;; infinite solutions
 {let ((m (make-matrix '((1 2 3 5)
                         (4 5 6 6)
                         (7 8 9 7)))))
   {and (equal? (rref! m)
                (make-matrix '((1 0 -1 -13/3)
                               (0 1 2 14/3)
                               (0 0 0 0))))
        (equal? (matrix-rank m) 2)}}
 ;; no solution
 {let ((m (make-matrix '((1  -3  0  -5  -7)
                         (3  -12 -2 -27 -33)
                         (-2 10  2  24  29)
                         (-1  6  1  14  17)
                         ))))
   {and (equal? (rref! m)
                (make-matrix '((1 0 0 1 0)
                               (0 1 0 2 0)
                               (0 0 1 3 0)
                               (0 0 0 0 1))))
        (equal? (matrix-rank m) 3)}}
 }

{define matrix-consistent?
  [|m|
   ;; todo, ensure that this is not a square matrix, as
   ;; they can end with 1
   (equal? '(1)
           (map [|row|
                 {let ((stream-row (list->stream row)))
                   (stream->list
                    (stream-drop-while [|x| (equal? 0 x)]
                                       stream-row))}]
                (matrix-rows m)))]}

{define matrix-solution
  [|m|
   {let* ((solved (rref! m))
          (solved-rank (matrix-rank solved)))
     {destructuring-bind (rows columns)
                         (matrix-size m)
       ;; if the rank equals the number of rows, then
       ;; each variable has a solution
       (if (equal? solved-rank rows)
           [(matrix-column m columns)]
           ;; if any rows are 0,0,0....1, then it is inconsistent,
           ;; any no solutions exist.  Otherwise there are
           ;; infinitely many.
           [(if (any?
                 (map [|r| {and (all? (map [|e| (equal? 0 e)]
                                           (but-last r)))
                                (equal? 0 (last r))}]
                      (matrix-rows solved)))
                ['infinite]
                ['inconsistent])])}}]}

{unit-test
 ;; one solution
 {let ((m (make-matrix '((2 8 4 2)
                         (2 5 1 5)
                         (4 10 -1 1)))))
   {and (equal? (rref! m)
                (make-matrix '((1 0 0 11)
                               (0 1 0 -4)
                               (0 0 1 3))))
        (equal? (matrix-solution m)
                (make-matrix '((2)
                               (5)
                               (1))))}}
 ;; infinite solutions
 {let ((m (make-matrix '((1 2 3 5)
                         (4 5 6 6)
                         (7 8 9 7)))))
   {and (equal? (rref! m)
                (make-matrix '((1 0 -1 -13/3)
                               (0 1 2 14/3)
                               (0 0 0 0))))

        (equal? 'infinite
                (matrix-solution m))}}
 ;; ;; no solution
 {let ((m (make-matrix '((1  -3  0  -5  -7)
                         (3  -12 -2 -27 -33)
                         (-2 10  2  24  29)
                         (-1  6  1  14  17)
                         ))))
   {and (equal? (rref! m)
                (make-matrix '((1 0 0 1 0)
                               (0 1 0 2 0)
                               (0 0 1 3 0)
                               (0 0 0 0 1))))
        (equal? 'inconsistent
                (matrix-solution m))}}

 }


{define matrix-transpose
  [|m|
   {destructuring-bind (rows columns)
                       (matrix-size m)
      (make-matrix
       (stream->list
        (stream-map
         [|i| (flatmap identity
                       (matrix-rows (matrix-column m i)))]
         (stream-enumerate-interval 1 columns))))}]}

{unit-test
 (equal? (matrix-transpose (make-matrix '((2 8 4 2)
                                          (2 5 1 5)
                                          (4 10 -1 1))))
         (make-matrix '((2 2 4)
                        (8 5 10)
                        (4 1 -1)
                        (2 5 1))))
 ;; double traspose equals returns the original matrix
 {let ((m (make-matrix '((2 8 4 2)
                         (2 5 1 5)
                         (4 10 -1 1)))))
   (equal? m
           ((compose matrix-transpose
                     matrix-transpose) m))}

 (equal? (matrix-transpose (make-matrix '((1)
                                          (2)
                                          (3))))
         (make-matrix '((1 2 3))))
 }

{define matrix-sum
  [|m1 m2 #!key (onError noop)|
   (if (not (equal? (matrix-size m1) (matrix-size m2)))
       [(onError)]
       [(make-matrix (map [|r1 r2|
                           (map + r1 r2)]
                          (matrix-rows m1)
                          (matrix-rows m2)))])]}

{unit-test
 {let ((m (make-matrix '((1 2)
                         (3 4))))
       (m2 (make-matrix '((4 3)
                          (2 1))))
       (m3 (make-matrix '((4 3 3)
                          (2 1 3)))))
   {and (equal? (make-matrix '((5 5)
                               (5 5)))
                (matrix-sum m m2))
        (equal? 'noop (matrix-sum m m3))}}
}

{define matrix-scale
  [|m1 k|
   (make-matrix (map [|r|
                      (map [|x| (* k x)] r)]
                     (matrix-rows m1)))]}

{unit-test
 {let ((m (make-matrix '((1 2)
                         (3 4)))))
   (equal? (make-matrix '((6 12)
                          (18 24)))
           (matrix-scale m 6))}
}


{define matrix-dot-product
  [|v1 v2|
   {destructuring-bind
    (ignore
     v1-columns
     ignore2
     v2-columns)
    (append (matrix-size v1)
            (matrix-size v2))
    (if (not {and (equal? v1-columns 1)
                  (equal? v2-columns 1)})
        ['too-many-columns-to-be-vector]
        [(sum (map *
                   (car (matrix-rows (matrix-transpose v1)))
                   (car (matrix-rows (matrix-transpose v2)))))
         ])}]}

{unit-test
 {let ((m  (matrix-transpose (make-matrix '((1 2)))))
       (m2 (matrix-transpose (make-matrix '((3 4))))))
   (equal? (matrix-dot-product m m2)
           11)}
}
