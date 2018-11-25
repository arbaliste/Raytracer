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

; Math from:
; https://www.scratchapixel.com/lessons/3d-basic-rendering/ray-tracing-generating-camera-rays
; https://www.scratchapixel.com/lessons/3d-basic-rendering/minimal-ray-tracer-rendering-simple-shapes
; https://www.scratchapixel.com/lessons/3d-basic-rendering/ray-tracing-rendering-a-triangle
; https://www.scratchapixel.com/lessons/3d-basic-rendering/introduction-to-shading

; Uncomment for running in racket
#lang racket
(require racket/draw)
(define (screen_width) 300)
(define (screen_height) 300)
(define target (make-bitmap (screen_width) (screen_height)))
(define dc (new bitmap-dc% [bitmap target]))
(define (exitonclick) (send target save-file "output.png" 'png))
(send dc set-pen "" 0 'transparent)
(define (pixel x y color)
 (send dc set-pixel x (- (screen_height) 1 y) color))
(define (rgb r g b)
 (make-object color%
   (exact-round (exact->inexact (* 255 r)))
   (exact-round (exact->inexact (* 255 g)))
   (exact-round (exact->inexact (* 255 b)))))
(define nil '())

; General utils
(define (random-gen seed n min max)
  ; Generates n "random" numbers from min to max with a seed
  (define a 22695477)
  (define c 1)
  (define mod (expt 2 32))
  (define (iter seed n prev)
    (define new (modulo (+ (* a seed) c) mod))
    (if (= n 0)
        prev
        (iter new (- n 1) (cons (+ min (* (- max min) (/ new mod))) prev))))
  (reverse (iter seed n nil)))
(define (min a b)
  (if (< a b) a b))
(define (max a b)
  (if (< a b) b a))
(define (ntake list n)
    ; Takes n elements from a list and returns (first-n . remaining)
    ; WARNING: Not tail recursive, n shouldn't be large
    (define (iter list n)
      (if (= n 0)
        (cons nil list)
        (let
          ((next (iter (cdr list) (- n 1))))
          (cons
            (cons (car list) (car next))
            (cdr next)))))
    (iter list n))
(define (ngroup list n)
    ; Splits a list into sublists of n elements each
    ; Tail recursive
    (define (iter-reverse prev list)
      (if (null? list)
        prev
        (let
          ((take (ntake list n)))
          (iter-reverse
            (cons (car take) prev)
            (cdr take)))))
    (reverse (iter-reverse nil list)))
(define (loop-range min-val max-val func)
  ; Basically a for loop
  (func min-val)
  (if (< (+ min-val 1) max-val)
      (loop-range (+ min-val 1) max-val func)
      nil))
(define (zip pairs)
  ; Zips multiple lists together
  ; Returns: list of lists
  ; WARNING: Not fail recursive, lists shouldn't be large
  (if (null? pairs)
      '(() ())
      (if (null? (car pairs))
          nil
          (cons (map car pairs) (zip (map cdr pairs))))))
(define (list-index s index)
  ; Gets the element of a list at an index
  ; Returns: (index)th element of l
  (if (= 0 index)
      (car s)
      (list-index (cdr s) (- index 1))))
(define (map procedure s)
  ; Tail-recursive map, from https://cs61a.org/assets/slides/29-Tail_Calls_full.pdf
  (define (map-reverse s m)
    (if (null? s)
      m
      (map-reverse
        (cdr s)
        (cons (procedure (car s)) m))))
  (reverse (map-reverse s nil)))
(define (reverse s)
  ; Tail-recursive reverse, from https://cs61a.org/assets/slides/29-Tail_Calls_full.pdf
  (define (reverse-iter s r)
    (if (null? s)
      r
      (reverse-iter
        (cdr s)
        (cons (car s) r))))
  (reverse-iter s nil))
(define (reduce func s)
  ; Tail-recursive reduce
  (if (null? (cdr s))
    (car s)
    (reduce func
      (cons
        (func (car s) (car (cdr s)))
        (cdr (cdr s))))))
(define (square x) (* x x))
(define (round x)
  (if (>= x 0)
      (floor (+ x 0.5))
      (- (round (- x)))))

; Vectors
; Vector structure: (x y z)
(define vec-create list)
(define (vec-x vec) (list-index vec 0))
(define (vec-y vec) (list-index vec 1))
(define (vec-z vec) (list-index vec 2))
(define (vec-mul v1 scalar) (map (lambda (x) (* x scalar)) v1))
(define (vec-mulvec v1 v2) (map (lambda (x) (reduce * x)) (zip (list v1 v2))))
(define (vec-add v1 v2) (map (lambda (x) (reduce + x)) (zip (list v1 v2))))
(define (vec-sub v1 v2) (vec-add v1 (vec-mul v2 -1)))
(define (vec-dot v1 v2) (reduce + (map (lambda (x) (reduce * x)) (zip (list v1 v2)))))
(define (vec-cross v1 v2) ; Only 3d
  (vec-create
   (- (* (vec-y v1) (vec-z v2)) (* (vec-z v1) (vec-y v2)))
   (- (* (vec-z v1) (vec-x v2)) (* (vec-x v1) (vec-z v2)))
   (- (* (vec-x v1) (vec-y v2)) (* (vec-y v1) (vec-x v2)))))
(define (vec-distsq v1 v2) (reduce + (map (lambda (x) (square (reduce - x))) (zip (list v1 v2)))))
(define (vec-dist v1 v2) (sqrt (vec-distsq v1 v2)))
(define vec-zero (vec-create 0 0 0))
(define (vec-magnitudesq v1) (vec-dot v1 v1))
(define (vec-magnitude v1) (sqrt (vec-magnitudesq v1)))
(define (vec-normalize v1) (vec-mul v1 (/ 1 (vec-magnitude v1))))
(define (vec-colormap vec) (map (lambda (x) (min x 1)) vec))  
(define (vec-rgb vec) (apply rgb vec))

; Rays
(define (ray-create orig dir) (list orig (vec-normalize dir)))
(define (ray-orig ray) (list-index ray 0))
(define (ray-dir ray) (list-index ray 1))

; Objects
; Intersect function: determines whether an object intersects with a ray
;   Returns: ray of phit and nhit
;            nil if no intersection
; Properties: list of object type specific attributes
; Material: material list (below)
(define (object-create intersect properties material)
  (list intersect properties material))
(define (object-intersect obj) (list-index obj 0))
(define (object-properties obj) (list-index obj 1))
(define (object-material obj) (list-index obj 2))
; Materials
; Color: vec3 (color)
; Reflection: vec3 (color), amount reflected
; Transparency: vec3 (color), amount refracted
; Index of refraction: amount to bend light
(define (material-create color-func reflection transparency ior)
  (list color-func reflection transparency ior))
(define (material-color material) (list-index material 0))
(define (material-reflection material) (list-index material 1))
(define (material-refraction material) (list-index material 2))
(define (material-ior material) (list-index material 3))
(define (make-constant-color color)
  (lambda (object point)
    color))
(define (make-checkerboard-color color1 color2 gridsize)
  (lambda (object point)
    (define modvec (map (lambda (p) (modulo (round (/ p gridsize)) 2)) point))
    (define xmod (vec-x modvec))
    (define ymod (vec-y modvec))
    (define zmod (vec-z modvec))
    (if (if (= ymod 0)
          (= xmod zmod)
          (not (= xmod zmod)))
        color1
        color2)))
; Planes
; Plane properties: (p normal)
(define (plane-create p normal material)
  (define realnorm (vec-normalize normal))
  (define invnorm (vec-mul (vec-normalize normal) -1))
  (object-create plane-intersect (list p realnorm invnorm) material))
(define (plane-intersect plane ray)
  (define invnorm (plane-invnorm plane))
  (define point (plane-point plane))
  (define direction (ray-dir ray))
  (define origin (ray-orig ray))
  (define denom (vec-dot invnorm direction))
  (if (<= denom bias)
      nil
      (let
          ((t (/ (vec-dot (vec-sub point origin) invnorm) denom)))
          (if (< t 0)
              nil
              (ray-create (vec-add origin (vec-mul direction t)) (plane-normal plane))))))
(define (plane-point plane)
  (list-index (object-properties plane) 0))
(define (plane-normal plane)
  (list-index (object-properties plane) 1))
(define (plane-invnorm plane)
  (list-index (object-properties plane) 2))
; Disks
; Disk properties: (radius plane)
; Basically a plane with a radius
(define (disk-create p normal radius material)
  (define plane
    (plane-create p normal material))
  (object-create disk-intersect (list radius plane) material))
(define (disk-intersect disk ray)
  (define plane (disk-plane disk))
  (define intersect (plane-intersect plane ray))
  (if (null? intersect)
      nil
      (if (< (vec-distsq (plane-point plane) (ray-orig intersect)) (square (disk-radius disk)))
          intersect
          nil)))
(define (disk-radius disk) (list-index (object-properties disk) 0))
(define (disk-plane disk) (list-index (object-properties disk) 1))
; Spheres
; Sphere properties: (radius position)
(define (sphere-create radius vec material)
  (object-create sphere-intersect (list radius vec) material))
(define (sphere-intersect sphere ray)
  (define radius (sphere-radius sphere))
  (define position (sphere-position sphere))
  (define origin (ray-orig ray))
  (define direction (ray-dir ray))
  (define l (vec-sub position origin))
  (define tca (vec-dot l direction))
  (define d2 (- (vec-magnitudesq l) (square tca)))
  (if (or (< tca 0) (> d2 (square radius)))
      nil
      ((lambda () ; Uses lambda because begin in an expression doesn't allow defines in racket
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
             ((lambda ()
               (define phit (vec-add origin (vec-mul direction t)))
               (define nhit (vec-sub phit position))
               (ray-create phit nhit))))))))
(define (sphere-radius sphere) (list-index (object-properties sphere) 0))
(define (sphere-position sphere) (list-index (object-properties sphere) 1))
; Triangles
; Triangle properties: (p1 p2 p3 plane)
; Basically another constrained plane
(define (triangle-create p1 p2 p3 material)
  (define plane
    (plane-create p1 (vec-cross (vec-sub p2 p1) (vec-sub p3 p1)) material))
  (object-create triangle-intersect (list p1 p2 p3 plane) material)) ; Pre-computes plane for easier collision detection
(define (triangle-intersect triangle ray)
  ;
  ;    C
  ;   ^  
  ;  /    
  ; A ---> B
  ; CCW definition
  (define origin (ray-orig ray))
  (define direction (ray-dir ray))
  (define a (triangle-p1 triangle))
  (define b (triangle-p2 triangle))
  (define c (triangle-p3 triangle))
  (define plane (triangle-plane triangle))
  (define normal (plane-normal plane))
  (define intersect (plane-intersect plane ray))
  (if (null? intersect)
      nil
      (let
          ((phit (ray-orig intersect)))
        (if
         (and
          (> (vec-dot normal (vec-cross (vec-sub b a) (vec-sub phit a))) 0)
          (> (vec-dot normal (vec-cross (vec-sub c b) (vec-sub phit b))) 0)
          (> (vec-dot normal (vec-cross (vec-sub a c) (vec-sub phit c))) 0))
         (ray-create phit (vec-normalize normal))
         nil))))
(define (triangle-p1 triangle) (list-index (object-properties triangle) 0))
(define (triangle-p2 triangle) (list-index (object-properties triangle) 1))
(define (triangle-p3 triangle) (list-index (object-properties triangle) 2))
(define (triangle-plane triangle) (list-index (object-properties triangle) 3))
(define (calculate-bbox points)
  ; Finds the smallest bounding box around a set of points, represented as (min max)
  (list
    (vec-create
      (reduce min (map vec-x points))
      (reduce min (map vec-y points))
      (reduce min (map vec-z points)))
    (vec-create
      (reduce max (map vec-x points))
      (reduce max (map vec-y points))
      (reduce max (map vec-z points)))))
(define (bbox-intersect? bbox ray)
  ; Checks if a bounding box intersects with a ray, returns a BOOLEAN (NOT standard intersect function)
  (define origin (ray-orig ray))
  (define invdir (map / (ray-dir ray)))
  (define signs (map (lambda (x) (if (< x 0) 1 0)) invdir))
  (define (get-minmax axis max?)
    (define index
      (if max?
          (- 1 (axis signs))
          (axis signs)))
    (* (- (axis (list-index bbox index)) (axis origin)) (axis invdir)))
           
  (define txmin (get-minmax vec-x #f))
  (define txmax (get-minmax vec-x #t))
  (define tymin (get-minmax vec-y #f))
  (define tymax (get-minmax vec-y #t))
  (if (or
       (> txmin tymax)
       (> tymin txmax))
      #f
      ((lambda ()
         (define newmin (max tymin txmin))
         (define newmax (min tymax txmax))
         (define tzmin (get-minmax vec-z #f))
         (define tzmax (get-minmax vec-z #t))
         (not
          (or
           (> newmin tzmax)
           (> tzmin newmax)))))))
(define (mesh-create points material)
  ; Creates a mesh from a list of triangle vertex positions
  (define triangles
    (map
      (lambda (vertices)
        (triangle-create (list-index vertices 0) (list-index vertices 1) (list-index vertices 2) material))
      (ngroup points 3)))
  (define bbox (calculate-bbox points))
  (object-create mesh-intersect (list triangles bbox) material))
(define (mesh-intersect mesh ray)
  ; Checks for intersection with bounding box for optimization
  ; If passed, checks for intersection with any of the triangles
  (if (not (bbox-intersect? (mesh-bbox mesh) ray))
      nil
      (let
        ((intersect (ray-closest ray (mesh-triangles mesh))))
        (if (null? intersect)
          nil
          (list-index intersect 1)))))
(define (mesh-triangles mesh) (list-index (object-properties mesh) 0))
(define (mesh-bbox mesh) (list-index (object-properties mesh) 1))

; Num encoding
(define (num-to-list num pow)
  ; Converts num to a list of its segments of 10**pow
  ; WARNING: Requires python scheme builtin quotient to use // instead of / 
  (define pow10 (expt 10 pow))
  (define (iter-reverse num prev)
    (if (= num 0)
      prev
      (iter-reverse (quotient num pow10) (cons (modulo num pow10) prev))))
  (iter-reverse num nil))
(define (num-to-coords num)
  ; Converts an encoded number to a list of vec coords
  ; Groups of 9 represent numbers
  ; 3 groups of numbers represent a vec

  ; 1-2 - Tens and ones
  ; 3-8 - Decimal digits
  ; 9   - Sign: 1 = positive, 2 = negative
  (ngroup
    (map
      (lambda (d)
        (define sign
          (if (= 1 (modulo d 10))
            1
            -1))
        (define num (quotient d 10))
        (* sign num (/ (expt 10 6)))
      )
      (num-to-list num 9))
    3))

; Raytracing
(define (ray-closest ray objects)
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
     (map
      (lambda (object)
        (define intersect ((object-intersect object) object ray))
        (if (null? intersect)
            nil
            (list (vec-distsq (ray-orig intersect) (ray-orig ray)) intersect object)))
      objects)))
(define (get-brightness hit light)
  ; Gets brightness as a function of hit position, hit normal, light position, and light intensity
  ; Returns: number from 0 to 1
  (vec-mul
    (ray-dir light)
    (max 0 (vec-dot (ray-dir hit) (vec-normalize (vec-sub (ray-orig light) (ray-orig hit)))))))      ; Angle between nhit and -lightdir
(define (get-reflect dir nhit)
  ; Get a reflection direction from a direction and normal
  (vec-sub dir (vec-mul nhit (* 2 (vec-dot dir nhit)))))
(define (get-refract dir nhit ior)
  ; Get a refraction direction (or none if total internal reflection) from direction, normal, and ior
  (define cos-diff (vec-dot (vec-normalize dir) (vec-normalize nhit)))
  (define abs-cos-diff (abs cos-diff))
  (define fixed-normal
    (if (> cos-diff 0) (vec-mul nhit -1) nhit))
  (define ior-ratio
    (if (> cos-diff 0) ior (/ ior)))
  (define c2sq (- 1 (* (square ior-ratio) (- 1 (square abs-cos-diff)))))
  (if (< c2sq 0)
      vec-zero
      (vec-add (vec-mul dir ior-ratio) (vec-mul fixed-normal (- (* ior-ratio abs-cos-diff) (sqrt c2sq))))))
(define (get-fresnel dir nhit ior)
  (define cosi (vec-dot (vec-normalize dir) (vec-normalize nhit)))
  (define etai
    (if (> cosi 0) 1 ior))
  (define etat
    (if (> cosi 0) ior 1))
  (define sint (* (/ etai etat) (sqrt (max 0 (- 1 (square cosi))))))
  (if (>= sint 1)
      1
      ((lambda ()
         (define cost (sqrt (max 0 (- 1 (square sint)))))
         (define abs-cosi (abs cosi))
         (define rs (/ (- (* etat abs-cosi) (* etai cost)) (+ (* etat abs-cosi) (* etai cost))))
         (define rp (/ (- (* etai abs-cosi) (* etat cost)) (+ (* etai abs-cosi) (* etat cost))))
         (/ (+ (square rs) (square rp)) 2)))))
(define (ray-trace depth ray)
  ; Traces a ray into the scene
  ; Returns: vec3 (color)
  (define origin (ray-orig ray))
  (define direction (ray-dir ray))
  (define closest (ray-closest ray objects))
  (if (or (null? closest) (> depth max-depth))                           ; If no object, use sky color
      (sky-color ray)
      ((lambda ()
          (define hit (list-index closest 1))
          (define object (list-index closest 2))
          (define phit (ray-orig hit))
          (define nhit (ray-dir hit))
          (define reflection-mag (vec-magnitudesq (material-reflection (object-material object))))
          (define refraction-mag (vec-magnitudesq (material-refraction (object-material object))))
          (define diffuse-component
            (reduce vec-add
                    (map (lambda (light)
                           (define shadow-closest ; If object hit, cast shadow ray and calculate brightness if not in shadow
                             (ray-closest
                              (ray-create
                               (vec-add phit (vec-mul nhit bias))
                               (vec-sub (ray-orig light) phit)) objects))
                           (if (or ; If no intersecting object with shadow ray or object is beyond light, illuminate
                                (null? shadow-closest)
                                (> (square (list-index shadow-closest 0)) (vec-distsq phit (ray-orig light))))
                               (vec-mulvec ((material-color (object-material object)) object phit) (get-brightness hit light))
                               vec-zero))
                         lights)))
          (define reflect-refract-component
            (if (or
                (> reflection-mag 0)
                (> refraction-mag 0))
              ((lambda ()
                 (define inside (<= (vec-dot nhit direction) 0))
                 (define signedbias (if inside (- bias) bias))
                 (define ratio (get-fresnel direction nhit (material-ior (object-material object))))
                 (define reflect-component ; Calculate reflection by tracing a ray
                  (if (> reflection-mag 0)
                    (vec-mulvec
                      (ray-trace
                       (+ depth 1)
                       (ray-create
                        (vec-add phit (vec-mul nhit signedbias))
                        (get-reflect direction nhit)))
                      (material-reflection (object-material object)))
                    vec-zero))
                (define refract-component ; Calculate refraction by tracing a ray
                  (if (and
                       (> refraction-mag 0)
                       (< ratio 1))
                    (vec-mulvec (material-refraction (object-material object))
                     (let
                         ((refract-dir (get-refract direction nhit (material-ior (object-material object)))))
                       (if (> (vec-magnitudesq refract-dir) 0)
                       (ray-trace
                        (+ depth 1)
                        (ray-create (vec-add phit (vec-mul nhit signedbias)) refract-dir))
                       vec-zero)))
                    vec-zero))
                (cond
                  ((= reflection-mag 0) refract-component)
                  ((= refraction-mag 0) reflect-component)
                  (else (vec-add (vec-mul reflect-component ratio) (vec-mul refract-component (- 1 ratio)))))))
              vec-zero))
         (vec-colormap (vec-add diffuse-component reflect-refract-component)))))) ; Add all components together, not entirely accurate ¯\_(ツ)_/¯
(define (pixel-trace x y)
  ; Get pixel color at (x, y) by casting rays
  ; Returns: vec3 (color)
  (define lookdir (vec-normalize (vec-sub camera-lookat camera-pos)))
  (define upvec (vec-normalize (vec-cross (vec-cross lookdir (vec-create 0 1 0)) lookdir)))
  (define rightvec (vec-normalize (vec-cross lookdir upvec)))
  (if (< bias (vec-dot upvec lookdir)) (/ 1 0) 1) ; Break if up vector isn't perpendicular to look direction

  (define screen-height
    (* 2
     (vec-dist camera-pos camera-lookat)
     (tan (* camera-fov pi (/ 360)))))
  (define scale (/ screen-height (screen_height))) ; Units per pixel
  (define yoffset (- y (/ (screen_height) 2) -0.5)) ; Offset in pixels from camera lookat. Y is not flipped because turtle graphics 0 is bottom
  (define xoffset (- x (/ (screen_width) 2) -0.5))
  (define screen-pos
    (vec-add
      (vec-add
       (vec-mul upvec (* scale yoffset))
       (vec-mul rightvec (* scale xoffset)))
      camera-lookat))
  (ray-trace 0 (ray-create
    camera-pos
    (vec-sub screen-pos camera-pos))))

; Setup
; X positive is ~left
; Y positive is ~down
; Z positive is ~farther
(define pi 3.141592653589793)
(define bias 0.00001)
(define max-depth 5)
(define camera-pos (vec-create -50 35 -60)) ; Camera might have to be on negative z for triangle winding to function properly?
(define camera-lookat (vec-create 0 0 0))
(define camera-fov 90)
(define lights ; Lights are represented as rays
  (list
   (ray-create (vec-create -40 60 -50) (vec-create 1 1 1))
   (ray-create (vec-create 0 60 20) (vec-create 1 1 1))))
(define (sky-color ray)
  (vec-create 0 0 0))
(define meshes '( ; Go bEaRs! 💛🐻💙
; bear-leg4
041973142042312051005567432074614772039374801014195442067274712134982511033096242041973142042312051005567432093570182018218851045818382074614772039374801014195442041973142042312051005567432033940112005804711047072542093570182018218851045818382041973142042312051005567432029598152136541121007526762033214752130980111053919141029598152136541121007526762041973142042312051005567432067274712134982511033096242057669132003080081052531351062513452144874381067899301099733102003176731028516901062513452144874381067899301057669132003080081052531351028166372000979081023144411033214752130980111053919141062513452144874381067899301028166372000979081023144411028166372000979081023144411041973142042312051005567432033214752130980111053919141073374882005808861055178962033940112005804711047072542099733102003176731028516901041973142042312051005567432028166372000979081023144411033940112005804711047072542028166372000979081023144411057669132003080081052531351099733102003176731028516901033940112005804711047072542028166372000979081023144411099733102003176731028516901062513452144874381067899301094972462182143671062129391099733102003176731028516901067274712134982511033096242099733102003176731028516901122012502251768741032769552099733102003176731028516901067274712134982511033096242074614772039374801014195442093570182018218851045818382073374882005808861055178962099733102003176731028516901074614772039374801014195442093570182018218851045818382099733102003176731028516901033940112005804711047072542073374882005808861055178962093570182018218851045818382094972462182143671062129391122012502251768741032769552099733102003176731028516901
; bear-head
041710482220055521160887282061052362311527441177289302078980452276306591211204452078980452276306591211204452070995902319063931215498012070461592296969831264648232024672731305165811293760262018018812289948601307128832027499101323922201292382972070461592296969831264648232031985272329121441286177182018018812289948601307128832042308792261885071288354912070461592296969831264648232018018812289948601307128832066381732245311341230031722078980452276306591211204452070461592296969831264648232039695512226918851245616822070461592296969831264648232042308792261885071288354912066381732245311341230031722070461592296969831264648232039695512226918851245616822012375242257553731347482912042308792261885071288354912018018812289948601307128832039695512226918851245616822019472521218429851242534812041710482220055521160887282066381732245311341230031722041710482220055521160887282078980452276306591211204452066381732245311341230031722039695512226918851245616822041710482220055521160887282042308792261885071288354912017090262235261081335721632039695512226918851245616822070260541284966581259710772066189411324988061223225842076734711277533071210924402066189411324988061223225842054709141236158581148370202076734711277533071210924402055980541307874321167957192054709141236158581148370202066189411324988061223225842045126121224981211225480712076734711277533071210924402054709141236158581148370202018018812289948601307128832031985272329121441286177182027499101323922201292382972027499101323922201292382972070260541284966581259710772024672731305165811293760262057997381339814001185878302030736121364061621196205082018230001360099981152870002057997381339814001185878302075878171358799781218461512058833601376171041215887642057997381339814001185878302058833601376171041215887642030736121364061621196205082018230001360099981152870002030736121364061621196205082012680002362169991158900002075878171358799781218461512057997381339814001185878302055980541307874321167957192058833601376171041215887642066189411324988061223225842033878741362143401232628542075878171358799781218461512066189411324988061223225842058833601376171041215887642014916321240433161339344642012375242257553731347482912024672731305165811293760262066189411324988061223225842075878171358799781218461512055980541307874321167957192033878741362143401232628542066189411324988061223225842070260541284966581259710772012375242257553731347482912018018812289948601307128832024672731305165811293760262058833601376171041215887642033878741362143401232628542030736121364061621196205082027499101323922201292382972033878741362143401232628542070260541284966581259710772041710482220055521160887282019472521218429851242534812030057981222443831165581972057174532374464041212812632077814182346522751208134402039863432354895171180891782070260541284966581259710772045126121224981211225480712019472521218429851242534812070260541284966581259710772076734711277533071210924402045126121224981211225480712045126121224981211225480712030057981222443831165581972019472521218429851242534812054709141236158581148370202030057981222443831165581972045126121224981211225480712039863432354895171180891782012680002362169991158900002037692172361959691230787682012680002362169991158900002030736121364061621196205082033878741362143401232628542012680002362169991158900002033878741362143401232628542037692172361959691230787682057174532374464041212812632037692172361959691230787682066800162365107191223110752039863432354895171180891782037692172361959691230787682057174532374464041212812632057174532374464041212812632066800162365107191223110752077814182346522751208134402014916321240433161339344642024672731305165811293760262034610001243020001282470002034610001243020001282470002024672731305165811293760262070260541284966581259710772019472521218429851242534812034610001243020001282470002070260541284966581259710772039695512226918851245616822017090262235261081335721632019472521218429851242534812019472521218429851242534812014916321240433161339344642034610001243020001282470002019472521218429851242534812017090262235261081335721632014916321240433161339344642070995902319063931215498012037692172361959691230787682070461592296969831264648232066800162365107191223110752037692172361959691230787682070995902319063931215498012077814182346522751208134402061052362311527441177289302039863432354895171180891782077814182346522751208134402066800162365107191223110752070995902319063931215498012077814182346522751208134402070995902319063931215498012061052362311527441177289302078980452276306591211204452061052362311527441177289302070995902319063931215498012027499101323922201292382972031985272329121441286177182033878741362143401232628542031985272329121441286177182037692172361959691230787682033878741362143401232628542031985272329121441286177182070461592296969831264648232037692172361959691230787682055980541307874321167957192057997381339814001185878302018230001360099981152870002012375242257553731347482912017090262235261081335721632042308792261885071288354912012375242257553731347482912014916321240433161339344642017090262235261081335721632
; bear-torso
016110361364783941280185621046570002362210011188340001014846812340823901312043021011261922381702311034196842088023382334556351021443441046570002362210011188340001016110361364783941280185621011261922381702311034196842046570002362210011188340001090012251329576871062588412119842191256219061027704012091855531123928431020717962090012251329576871062588412091855531123928431020717962044933891150734051035052802079134281165911371066770241084133341155132791129932771006352382112853721137850731079134281165911371066770241006352382112853721137850731070136061148072201067679421029598152136541121007526762030057981222443831165581972044933891150734051035052802054709141236158581148370202044933891150734051035052802030057981222443831165581972090012251329576871062588412044933891150734051035052802054709141236158581148370202035417411265897061335790821096346061288218381203703081016110361364783941280185621035417411265897061335790821087000791196247621299830381096346061288218381203703081035417411265897061335790821016110361364783941280185621014846812340823901312043021057450001352210011194300001016110361364783941280185621096346061288218381203703081086755982251967241306213491035417411265897061335790821014846812340823901312043021006352382112853721137850731069920562148030251187642141062513452144874381067899301086755982251967241306213491014846812340823901312043021095489642292250441232541201070136061148072201067679421101085611141411481047261771079134281165911371066770241095489642292250441232541201014846812340823901312043021046570002362210011188340001066365082203488581088787302122012502251768741032769552071920002323079991095920002066365082203488581088787302071920002323079991095920002061052362311527441177289302071920002323079991095920002011261922381702311034196842012680002362169991158900002033214752130980111053919141025685581130947261019799441037502031129764211055505691037502031129764211055505691070136061148072201067679421006352382112853721137850731066365082203488581088787302067274712134982511033096242122012502251768741032769552033214752130980111053919141037502031129764211055505691006352382112853721137850731066365082203488581088787302029598152136541121007526762067274712134982511033096242029598152136541121007526762044933891150734051035052802025685581130947261019799441066365082203488581088787302041710482220055521160887282030057981222443831165581972029598152136541121007526762066365082203488581088787302030057981222443831165581972061052362311527441177289302041710482220055521160887282066365082203488581088787302033687391147901941308908291035417411265897061335790821060881962201267971314285371001686642124364311267959161033687391147901941308908291060881962201267971314285371029598152136541121007526762025685581130947261019799441033214752130980111053919141035417411265897061335790821086755982251967241306213491060881962201267971314285371069920562148030251187642141086755982251967241306213491095489642292250441232541201099495611277651711034045071057450001352210011194300001096346061288218381203703081033214752130980111053919141006352382112853721137850731062513452144874381067899301090012251329576871062588412057450001352210011194300001099495611277651711034045071069920562148030251187642141094972462182143671062129391062513452144874381067899301016110361364783941280185621057450001352210011194300001011261922381702311034196842071920002323079991095920002088023382334556351021443441011261922381702311034196842012680002362169991158900002011261922381702311034196842018230001360099981152870002071920002323079991095920002012680002362169991158900002039863432354895171180891782057450001352210011194300001090012251329576871062588412011261922381702311034196842090012251329576871062588412018230001360099981152870002011261922381702311034196842099495611277651711034045071096346061288218381203703081084133341155132791129932771096346061288218381203703081078259461162346951197548181084133341155132791129932771119842191256219061027704012099495611277651711034045071101085611141411481047261771119842191256219061027704012101085611141411481047261771091855531123928431020717962079134281165911371066770241101085611141411481047261771099495611277651711034045071084133341155132791129932771079134281165911371066770241099495611277651711034045071039863432354895171180891782061052362311527441177289302071920002323079991095920002094972462182143671062129391088023382334556351021443441122012502251768741032769552006352382112853721137850731028384262126112111201223911069920562148030251187642141095489642292250441232541201046570002362210011188340001088023382334556351021443441071920002323079991095920002122012502251768741032769552088023382334556351021443441106509112255694271120623161095489642292250441232541201088023382334556351021443441106509112255694271120623161069920562148030251187642141095489642292250441232541201088023382334556351021443441094972462182143671062129391106509112255694271120623161094972462182143671062129391069920562148030251187642141106509112255694271120623161087000791196247621299830381035417411265897061335790821033687391147901941308908291119842191256219061027704012090012251329576871062588412099495611277651711034045071055980541307874321167957192090012251329576871062588412054709141236158581148370202055980541307874321167957192018230001360099981152870002090012251329576871062588412022650231127687911203822151084133341155132791129932771078259461162346951197548181022650231127687911203822151006352382112853721137850731084133341155132791129932771006352382112853721137850731022650231127687911203822151028384262126112111201223911022650231127687911203822151001686642124364311267959161028384262126112111201223911
; bear-leg2
052552172047568021242361681061792872010378781196858881074321202041010421252358681061792872010378781196858881089389382007474451218448031074321202041010421252358681032035682024335941307602481001686642124364311267959161060881962201267971314285371077358602003013351285202471060881962201267971314285371086755982251967241306213491086755982251967241306213491074321202041010421252358681077358602003013351285202471086755982251967241306213491069920562148030251187642141074321202041010421252358681031393722045185181259841711052552172047568021242361681028384262126112111201223911052552172047568021242361681074321202041010421252358681028384262126112111201223911028384262126112111201223911001686642124364311267959161031393722045185181259841711032035682024335941307602481031393722045185181259841711001686642124364311267959161060881962201267971314285371077358602003013351285202471032035682024335941307602481019775392000422352257135141031393722045185181259841711032035682024335941307602481032035682024335941307602481077358602003013351285202471019775392000422352257135141089389382007474451218448031061792872010378781196858881077358602003013351285202471019775392000422352257135141077358602003013351285202471024044962007665031209812601074321202041010421252358681089389382007474451218448031077358602003013351285202471069920562148030251187642141028384262126112111201223911074321202041010421252358681024044962007665031209812601061792872010378781196858881031393722045185181259841711077358602003013351285202471061792872010378781196858881024044962007665031209812601024044962007665031209812601031393722045185181259841711019775392000422352257135141061792872010378781196858881052552172047568021242361681031393722045185181259841711
; bear-leg1
077270601008566801282218971036085161024047091313807111018341581001599381256888051055376371046266161241262741077270601008566801282218971088946691005683521222130181061197871012224521197660031055376371046266161241262741088946691005683521222130181096346061288218381203703081087000791196247621299830381077270601008566801282218971055376371046266161241262741061197871012224521197660031023286321006168141212603091023286321006168141212603091061197871012224521197660031088946691005683521222130181023286321006168141212603091028626551049675221258123631055376371046266161241262741023286321006168141212603091018341581001599381256888051028626551049675221258123631023286321006168141212603091088946691005683521222130181077270601008566801282218971077270601008566801282218971018341581001599381256888051023286321006168141212603091077270601008566801282218971078259461162346951197548181096346061288218381203703081001686642124364311267959161036085161024047091313807111033687391147901941308908291055376371046266161241262741022650231127687911203822151078259461162346951197548181028626551049675221258123631022650231127687911203822151055376371046266161241262741077270601008566801282218971055376371046266161241262741078259461162346951197548181036085161024047091313807111087000791196247621299830381033687391147901941308908291077270601008566801282218971087000791196247621299830381036085161024047091313807111001686642124364311267959161028626551049675221258123631036085161024047091313807111001686642124364311267959161022650231127687911203822151028626551049675221258123631018341581001599381256888051036085161024047091313807111028626551049675221258123631
; bear-leg3
091855531123928431020717962044373111041626691011391682044933891150734051035052802095277781008159001021773471091855531123928431020717962101085611141411481047261771083006331032576431009776772095277781008159001021773471093355781012920571046569192070136061148072201067679421095277781008159001021773471101085611141411481047261771095277781008159001021773471083006331032576431009776772091855531123928431020717962093355781012920571046569192095277781008159001021773471057299961002319541052361231057299961002319541052361231095277781008159001021773471070136061148072201067679421070136061148072201067679421037502031129764211055505691057299961002319541052361231025406531000524131018944261025685581130947261019799441044373111041626691011391682044933891150734051035052802044373111041626691011391682025685581130947261019799441025406531000524131018944261057299961002319541052361231037502031129764211055505691037502031129764211055505691025685581130947261019799441025406531000524131018944261044373111041626691011391682091855531123928431020717962083006331032576431009776772044373111041626691011391682083006331032576431009776772093355781012920571046569192058534461005552711058945732093355781012920571046569192057299961002319541052361231025406531000524131018944261058534461005552711058945732057299961002319541052361231044373111041626691011391682058534461005552711058945732025406531000524131018944261058534461005552711058945732044373111041626691011391682093355781012920571046569192
))
(define ball-radius 90)
(define snow-num 30)
(define objects
  (reduce append (list
    (list ; Normal objects
      ;(sphere-create 15 (vec-create 15 15 -5)
      ;  (material-create (make-constant-color (vec-create 0 0 0)) (vec-create 0.8 0.8 0.8) (vec-create 1 1 1) 1.1))
      ;(sphere-create 10 (vec-create 15 10 0)
      ;  (material-create (make-constant-color (vec-create 0.9922 0.7098 0.0824)) vec-zero vec-zero 1))
      ;(sphere-create 10 (vec-create -40 10 -20)
      ;  (material-create (make-constant-color (vec-create 0 0.196 0.3943)) vec-zero vec-zero 1))
      ;(sphere-create 15 (vec-create -50 25 0)
      ;  (material-create (make-constant-color (vec-create 0.9922 0.7098 0.0824)) vec-zero vec-zero 1))
      (disk-create (vec-create 0 0.0001 0) (vec-create 0 1 0) ball-radius
        (material-create (make-constant-color (vec-create 1 1 1)) vec-zero vec-zero 1))
      (plane-create (vec-create 0 0 0) (vec-create 0 1 0)
        (material-create (make-constant-color (vec-create 0.2 0.2 0.2)) (vec-create 0 0 0) vec-zero 1)))
    ;(map ; Snow!
    ; (lambda (coord)
    ;   (sphere-create 3 coord
    ;    (material-create (make-constant-color (vec-create 1 1 1)) vec-zero vec-zero 1)))
    ; (ngroup (random-gen 1868 (* 3 snow-num) 40 ball-radius) 3))
    (map ; Mesh objects
      (lambda (num)
        (define coords (num-to-coords num))
        (display "Found ")
        (display (/ (length coords) 3))
        (display " faces\n")
        ;(sphere-create 0 vec-zero (material-create (make-constant-color vec-zero) vec-zero vec-zero 0)))
        (mesh-create coords (material-create (make-constant-color (vec-create 0.7686 0.5098 0.0588)) vec-zero vec-zero 1)))
      meshes))))

; Main draw function
(define (draw)
  (display "Starting draw\n")
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
