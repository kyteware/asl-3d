;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 3dworld) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)

(define FOV (/ pi 3))
(define W/H 75)

;; Angle is (Number, Number)
;; interp. yaw north @ 0, rotates cw in radians
;;         pitch ->horizon @ 0, rotates up in radians
(define-struct dir (yaw pitch))
(define DIR-NORTH-STRAIGHT (make-dir 0 0))
;; Vec3 is (Number, Number, Number)
;; interp x is +e/-w, y is +n/-s, z is +up/-down
(define-struct vec3 (x y z))
(define VEC3-ORIGIN (make-vec3 0 0 0))
;; Triangle is (Vec3 Vec3 Vec3 Color)
(define-struct tri (a b c col))
(define SAMPLE-TRI (make-tri (make-vec3 -1 0 2)
                             (make-vec3 1 0 2)
                             (make-vec3 0 -1 3)
                             "red"))
(define TRI2 (make-tri (make-vec3 -1 0 -1)
                       (make-vec3 1 0 -1)
                       (make-vec3 0 0 3)
                       "blue"))
(define TRIA (make-tri (make-vec3 -1 -1 -1)
                       (make-vec3 -1 1 -1)
                       (make-vec3 -1 -1 1)
                       "black"))
(define TRIB (make-tri (make-vec3 -1 -1 -1)
                       (make-vec3 1 -1 -1)
                       (make-vec3 -1 -1 1)
                       "grey"))
(define TRIC (make-tri (make-vec3 -1 -1 -1)
                       (make-vec3 1 -1 -1)
                       (make-vec3 -1 1 -1)
                       "blue"))

;; WorldState is (Vec3 Angle (listof Triangle))
(define-struct ws (pos facing objs prog))

(define WS-1 (make-ws (make-vec3 0 -3 0)
                      DIR-NORTH-STRAIGHT
                      (list TRIA TRIB TRIC)
                      0))

;; the main program
(define (main w)
  (big-bang w           ; WS
    (on-tick tock)     ; WS -> WS
    (to-draw render)
    (on-key key-handle)))   ; WS -> Image)    ; WS KeyEvent -> WS

;; tick tock goes the clock
(define (tock w)
  (local [(define new-prog (+ (ws-prog w) 0.1))
          (define new-pos (make-vec3 (* 5 (sin new-prog))
                                     (* -5 (cos new-prog))
                                     4))
          (define new-dir (make-dir (- new-prog) (- (/ pi 4))))]
    (make-ws new-pos new-dir (progged-tris new-prog) new-prog)))

;; generate the list of triangles with one animated
(define (progged-tris prog)
  (local [(define offset (* 3 (sin (/ prog 2))))]
    (list TRIA TRIB TRIC
          (make-tri (make-vec3 -2 (+ -2 offset) -2)
                    (make-vec3 2 (+ -2 offset) -2)
                    (make-vec3 0 (+ 1 offset) -1)
                    (make-color (round (* 40 (+ offset 3))) 0 200)))))

;; when up pressed
(define (key-handle w k)
  (cond [(key=? k "up")
         (make-ws (ws-pos w)
                  (make-dir (dir-yaw (ws-facing w))
                            (+ (dir-pitch (ws-facing w)) 0.1))
                  (ws-objs w)
                  0)]
        [(key=? k "down")
         (make-ws (ws-pos w)
                  (make-dir (dir-yaw (ws-facing w))
                            (- (dir-pitch (ws-facing w)) 0.1))
                  (ws-objs w)
                  0)]
        [(key=? k "left")
         (make-ws (ws-pos w)
                  (make-dir (- (dir-yaw (ws-facing w)) 0.1)
                            (dir-pitch (ws-facing w)))
                  (ws-objs w)
                  0)]
        [(key=? k "right")
         (make-ws (ws-pos w)
                  (make-dir (+ (dir-yaw (ws-facing w)) 0.1)
                            (dir-pitch (ws-facing w)))
                  (ws-objs w)
                  0)]
        [else w]))

;; master render
(define (render w)
  (local [;; offset from camera 
          (define (dir-off p)
            (* FOV (- (/ p W/H) 0.5))) ; get offset of an angle of a ray

          ;; gen list of pixels of a row
          (define (row-pixels r)
            (build-list W/H
                        (lambda (c)
                          (square 4
                                  "solid"
                                  (trace-ray w (dir-off c) (dir-off r))))))
          
          ;; draw a row of the image
          (define (draw-row r)
            (foldr beside
                   empty-image
                   (row-pixels r)))
                                              
          (define rows (build-list W/H draw-row))]
    (foldl above empty-image rows)))

;; trace a single ray
(define (trace-ray w yaw-offset pitch-offset)
  (local [(define w-yaw (dir-yaw (ws-facing w)))
          (define w-pitch (dir-pitch (ws-facing w)))
          (define ray-origin (ws-pos w))
          
          (define ray-dir (make-dir (+ w-yaw yaw-offset)
                                    (+ w-pitch pitch-offset)))

          (define (nearest-color tris closest-dist closest-col)
            (cond [(empty? tris) closest-col]
                  [else
                   (local [(define t (first tris))
                           (define dist
                             (intersect ray-origin ray-dir t))]
                     (if (and (not (false? dist))
                              (< dist closest-dist))
                         (nearest-color (rest tris) dist (tri-col t))
                         (nearest-color (rest tris)
                                        closest-dist
                                        closest-col)))]))]
    (nearest-color (ws-objs w) 1000000000000 "white")))

;; distance of intersect for ray and triangle (or false)
(check-expect (intersect (make-vec3 0 -10 0)
                         DIR-NORTH-STRAIGHT
                         SAMPLE-TRI)
              false)
(check-expect (intersect (make-vec3 0 -100 0)
                         DIR-NORTH-STRAIGHT
                         TRI2)
              100)

(define (intersect ray-origin ray-dir t)
  (local [(define lamb (inter-lamb ray-origin ray-dir t))]
    (if (> lamb 0)
        (local [(define inter-point
                  (vec3+ ray-origin
                         (vec3* (dir->vec3 ray-dir)
                                (vec3-splat lamb))))]
          (if (and (same-side (tri-b t) (tri-c t) (tri-a t) inter-point)
                   (same-side (tri-a t) (tri-c t) (tri-b t) inter-point)
                   (same-side (tri-a t) (tri-b t) (tri-c t) inter-point))
              (vec3-dist ray-origin inter-point)
              false))
        false)))

;; checks if x is on the same side as the v1/v2 line as r is
(define (same-side v1 v2 r x)
  (local [(define l (vec3- v1 v2))
          (define a (vec3x l (vec3- x v2)))
          (define b (vec3x l (vec3- r v2)))
          (define c (vec3* a b))]
    (> (vec3-sum c) 0)))

;; distance from ray to triangle plane (doesn't check for intersect)
(check-expect (inter-lamb (make-vec3 0 -10 0)
                          DIR-NORTH-STRAIGHT
                          SAMPLE-TRI)
              12)
(check-expect (inter-lamb (make-vec3 0 -100 0)
                          DIR-NORTH-STRAIGHT
                          TRI2)
              100)

(define (inter-lamb ray-origin ray-dir t)
  (local [(define n (tri-normal t))
          (define k (tri-k t n))
          (define m (dir->vec3 ray-dir))]
    (- (/ (+ (vec3-sum (vec3* n ray-origin)) k)
          (vec3-sum (vec3* n m))))))
    

;; k of triangle plane
(check-expect (tri-k SAMPLE-TRI (tri-normal SAMPLE-TRI))
              4)
(define (tri-k t n)
  (vec3-sum (vec3* (vec3* (vec3-splat -1) n) (tri-a t))))

;; normal of triangle
(check-expect (tri-normal SAMPLE-TRI)
              (make-vec3 0 -2 -2))

(define (tri-normal t)
  (vec3x (vec3- (tri-b t)
                (tri-a t))
         (vec3- (tri-c t)
                (tri-a t))))


;; cross product of 2 vectors
(check-expect (vec3x (make-vec3 2 0 0)
                     (make-vec3 1 -1 1))
              (make-vec3 0 -2 -2))

(define (vec3x a b)
  (make-vec3 (- (* (vec3-y a) (vec3-z b)) (* (vec3-z a) (vec3-y b)))
             (- (* (vec3-z a) (vec3-x b)) (* (vec3-x a) (vec3-z b)))
             (- (* (vec3-x a) (vec3-y b)) (* (vec3-y a) (vec3-x b)))))

;; add 2 vectors
(define (vec3+ a b)
  (make-vec3 (+ (vec3-x a) (vec3-x b))
             (+ (vec3-y a) (vec3-y b))
             (+ (vec3-z a) (vec3-z b))))

;; subtract 2 vectors
(check-expect (vec3- (make-vec3 0 -1 3) (make-vec3 -1 0 2))
              (make-vec3 1 -1 1))
(check-expect (vec3- (make-vec3 1 0 2) (make-vec3 -1 0 2))
              (make-vec3 2 0 0))
 
(define (vec3- a b)
  (make-vec3 (- (vec3-x a) (vec3-x b))
             (- (vec3-y a) (vec3-y b))
             (- (vec3-z a) (vec3-z b))))

;; dot product 2 vectors
(define (vec3* a b)
  (make-vec3 (* (vec3-x a) (vec3-x b))
             (* (vec3-y a) (vec3-y b))
             (* (vec3-z a) (vec3-z b))))

;; make a vec3 with all components = x
(define (vec3-splat x)
  (make-vec3 x x x))

;; sum components of vec3
(define (vec3-sum a)
  (+ (vec3-x a) (vec3-y a) (vec3-z a)))

;; generate vector with length 1 from angle
(define (dir->vec3 d)
  (make-vec3 (* (sin (dir-yaw d)) (cos (dir-pitch d)))
             (* (cos (dir-yaw d)) (cos (dir-pitch d)))
             (sin (dir-pitch d))))

(define (vec3-dist a b)
  (sqrt (+ (sqr (- (vec3-x a) (vec3-x b)))
           (sqr (- (vec3-y a) (vec3-y b)))
           (sqr (- (vec3-z a) (vec3-z b))))))


