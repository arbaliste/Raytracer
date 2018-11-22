;;; Scheme Recursive Art Contest Entry
;;;
;;; Please do not include your name or personal info in this file.
;;;
;;; Title: <Your title here>
;;;
;;; Description:
;;;   <It's your masterpiece.
;;;    Use these three lines to describe
;;;    its inner meaning.>

; Uncomment for running in racket
#lang racket
(require racket/draw)
(define (screen_width) 500)
(define (screen_height) 500)
(define target (make-bitmap (screen_width) (screen_height)))
(define dc (new bitmap-dc% [bitmap target]))
(define (exitonclick) (send target save-file "output.png" 'png))
(send dc set-pen "" 0 'transparent)
(define (pixel x y color)
  (send dc set-brush color 'solid)
  (send dc draw-rectangle x y x y))
(define (rgb r g b)
  (make-object color%
    (exact-round (exact->inexact (* 255 r)))
    (exact-round (exact->inexact (* 255 g)))
    (exact-round (exact->inexact (* 255 b)))))
(define (reduce func l)
  (if (null? (cdr l))
      (car l)
      (func (car l) (reduce func (cdr l)))))

; General utils
(define (clamp oldmin oldmax newmin newmax val)
  (+ (* (/ (- val oldmin) (- oldmax oldmin)) (- newmax newmin)) newmin))
(define (min a b)
  (if (< a b)
      a
      b))
(define (max a b)
  (if (< a b)
      b
      a))
(define (loop-range min-val max-val func)
  ; Basically a for loop
  (func min-val)
  (if (< min-val max-val)
      (loop-range (+ min-val 1) max-val func)
      '()))
(define (zip pairs)
  ; Zips multiple lists together
  ; Returns: list of lists
  (if (null? pairs)
      '(() ())
      (if (null? (car pairs))
          '()
          (cons (map car pairs) (zip (map cdr pairs))))))
(define (list-index l index)
  ; Gets the element of a list at an index
  ; Returns: (index)th element of l
  (if (= 0 index)
      (car l)
      (list-index (cdr l) (- index 1))))
(define (square x) (* x x))

; Vectors
; Vector structure: (x y z)
(define vec-create list)
(define (vec-x vec) (list-index vec 0))
(define (vec-y vec) (list-index vec 1))
(define (vec-z vec) (list-index vec 2))
(define (vec-mul v1 scalar) (map (lambda (x) (* x scalar)) v1))
(define (vec-add v1 v2) (map (lambda (x) (reduce + x)) (zip v1 v2)))
(define (vec-sub v1 v2) (vec-add v1 (vec-mul v2 -1)))
(define (vec-dot v1 v2) (reduce + (map (lambda (x) (reduce * x)) (zip v1 v2))))
(define (vec-cross v1 v2) ; Only 3d
  (vec-create
   (- (* (vec-y v1) (vec-z v2)) (* (vec-z v1) (vec-y v2)))
   (- (* (vec-z v1) (vec-x v2)) (* (vec-x v1) (vec-z v2)))
   (- (* (vec-x v1) (vec-y v2)) (* (vec-y v1) (vec-x v2)))))
(define (vec-distsq v1 v2) (reduce + (map (lambda (x) (square (reduce - x))) (zip v1 v2))))
(define (vec-dist v1 v2) (sqrt (vec-distsq v1 v2)))
(define vec-zero (vec-create 0 0 0))
(define (vec-magnitude v1) (vec-dist v1 vec-zero))
(define (vec-normalize v1) (vec-mul v1 (/ 1 (vec-magnitude v1))))
(define (vec-rgb vec) (apply rgb vec))

; Rays
(define (ray-create orig dir) (list orig (vec-normalize dir)))
(define (ray-orig ray) (list-index ray 0))
(define (ray-dir ray) (list-index ray 1))

; Objects
; Object structure: (intersect-function properties color reflection)
; Intersect function: determines whether an object intersects with a ray
;   Returns: ray of phit and nhit
;            nil if no intersection
; Properties: list of object type specific attributes
; Color: vec3 (color)
; Reflection: from 0 to 1, amount reflected
(define object-create list)
(define (object-intersect obj) (list-index obj 0))
(define (object-properties obj) (list-index obj 1))
(define (object-color obj) (list-index obj 2))
(define (object-reflection obj) (list-index obj 3))
; Spheres
; Sphere properties: (radius position)
(define (sphere-create radius vec color reflection)
  (object-create sphere-intersect (list radius vec) color reflection))
(define (sphere-intersect sphere ray)
  (define radius (sphere-radius sphere))
  (define position (sphere-position sphere))
  (define origin (ray-orig ray))
  (define direction (ray-dir ray))
  (define l (vec-sub position origin))
  (define tca (vec-dot l direction))
  (define d2 (- (vec-dot l l) (square tca)))
  (if (> d2 radius)
      nil
      (begin
         (define thc (sqrt (- (square radius) d2)))
         (define t0 (- tca thc))
         (define t1 (+ tca thc))
         (define t
           (cond
             ((< t0 0) t1)
             ((< t1 0) t0)
             (else (min t0 t1))))
         (if (< t 0)
             nil
             (begin
               (define phit (+ origin (* direction t)))
               (define nhit (vec-sub phit position))
               (ray-create phit nhit))))))
(define (sphere-radius sphere) (list-index (object-properties sphere) 0))
(define (sphere-position sphere) (list-index (object-properties sphere) 1))
; Triangles
; Triangle properties: (p1 p2 p3)
; TODO
(define (triangle-create p1 p2 p3 color)
  (object-create triangle-intersect (list p1 p2 p3) color))
(define (triangle-intersect triangle ray)
  #f)
(define (triangle-p1 triangle) (list-index (object-properties triangle) 0))
(define (triangle-p2 triangle) (list-index (object-properties triangle) 1))
(define (triangle-p3 triangle) (list-index (object-properties triangle) 2))
(define (mesh-create encoded color)
  ; Creates a mesh from an encoded list of triplets of vecs
  ; Returns: list of triangles
  #f)

; Raytracing
(define (ray-closest ray)
  ; Find closest object intersecting with a ray
  ; Returns: (distance^2: number, hit: ray, object: object)
  ; Returns nil if nothing hit
  (reduce
     (lambda (o1 o2)
         (cond
           ((null? o1) o2)
           ((null? o2) o1)
           ((> (list-index o1 0) (list-index o2 0)) o2)
           (else o1)))
     (cons nil (map
      (lambda (object)
        (define intersect ((object-intersect object) object ray))
        (if (null? intersect)
            nil
            (list (vec-distsq (ray-orig intersect) (ray-orig ray)) intersect object)))
      objects))))
(define (get-brightness hit)
  ; Gets brightness as a function of hit position, hit normal, light position, and light intensity
  ; https://www.scratchapixel.com/lessons/3d-basic-rendering/introduction-to-shading/shading-spherical-light
  ; Returns: number from 0 to 1
  (*
    light-intensity
    ;(/ (* 4 pi)                                                                      
    ;(/ (vec-distsq (ray-orig hit) list-pos))                                         ; Inverse square
    (vec-dot (ray-dir hit) (vec-normalize (vec-sub (ray-orig hit) light-pos)))))      ; Angle between nhit and -lightdir
(define (get-reflect lightdir nhit)
  ; Get a reflection direction from a light direction and normal
  ; https://www.scratchapixel.com/lessons/3d-basic-rendering/introduction-to-shading/reflection-refraction-fresnel
  (vec-sub lightdir (vec-mul nhit (* 2 (vec-dot lightdir nhit)))))
(define (ray-trace depth ray)
  ; Traces a ray into the scene
  ; Returns: vec3 (color)
  (if (> depth max-depth)
    (sky-color ray)
    (begin
      (define closest (ray-closest ray))  
      (define hit (list-index closest 1))
      (define phit (ray-orig hit))
      (define nhit (ray-dir hit))
      (cond
        ((null? closest) (sky-color ray))                           ; If no object, use sky color
        ((> (object-reflection closest) 0)                          ; If reflects, recurse with reflection and multiply by reflection amount
          (vec-mul
            (ray-trace (+ depth 1) (ray-create (ray-orig closest) (get-reflect (vec-sub hit light-pos) nhit)))
            (object-reflection closest)))                                               
        (else (begin                                                ; If object hit, cast shadow ray and calculate brightness if not in shadow
          (define shadow-closest
            (ray-closest (ray-create
              (ray-orig hit)
              (vec-sub light-pos hit))))                            ; TODO: Add bias?
          (if (or                                                   ; If no intersecting object with shadow ray or object is beyond light, illuminate
                (null? shadow-closest)
                (> (square (list-index shadow-closest 0)) (vec-distsq (ray-orig hit) light-pos)))
              (vec-mul (object-color closest) (get-brightness hit))
              vec-zero))))))                                         ; Otherwise, black
(define (pixel-trace x y)
  ; Get pixel color at (x, y) by casting rays
  ; Returns: vec3 (color)
  (ray-trace 0 (ray-create
    camera-pos
    #f))) ; Replace #f with actual ray direction

; Setup
(define pi 3.141592653589793)
(define ray-depth 5)
(define camera-pos (vec-create 0 0 2))
(define camera-lookat vec-zero)
(define camera-up (vec-create 0 1 0))
(define camera-fov 45)
(define light-pos (vec-create 0 1 0))
(define light-intensity 1)
(define (sky-color ray)
  vec-zero)
(define encoded-triangles nil)
(define objects
  (append
    (list
      (sphere-create 1 (vec-create 0 0 0) (vec-create 1 0 0)))
    (mesh-create encoded-triangles (vec-create 0 0 1))))

; Main draw function
(define (draw)
  ; Loops over all the pixels in the image and sets each one's color
  (loop-range 0 (screen_width)
    (lambda (x)
      (loop-range 0 (screen_height)
        (lambda (y)
          (pixel x y (vec-rgb (pixel-trace x y)))))
      (display (quotient (* 100 x) (screen_width)))
      (display "%\n")))
  (exitonclick))

; Please leave this last line alone.  You may add additional procedures above
; this line.
(draw)
