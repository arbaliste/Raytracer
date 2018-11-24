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
(define (screen_width) 30)
(define (screen_height) 30)
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
  (object-create plane-intersect (list p normal) material))
(define (plane-intersect plane ray)
  (define invnorm (vec-mul (vec-normalize (plane-normal plane)) -1))
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
; Triangle properties: (p1 p2 p3)
(define (triangle-create p1 p2 p3 material)
  (object-create triangle-intersect (list p1 p2 p3) material))
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
  (define invnorm (vec-cross (vec-sub c a) (vec-sub b a)))
  (define normal (vec-mul invnorm -1))
  (define denom (vec-dot invnorm direction))
  (if (< (abs denom) bias) ; (abs normdotray) for backwards triangles
      nil
      (let
          ((t (/ (vec-dot (vec-sub a origin) invnorm) denom)))
        (if (< t 0)
            nil
            (let
                ((phit (vec-add origin (vec-mul direction t))))
              (if
               (and
                (> (vec-dot normal (vec-cross (vec-sub b a) (vec-sub phit a))) 0)
                (> (vec-dot normal (vec-cross (vec-sub c b) (vec-sub phit b))) 0)
                (> (vec-dot normal (vec-cross (vec-sub a c) (vec-sub phit c))) 0))
               (ray-create phit (vec-normalize normal))
               nil))))))
(define (triangle-p1 triangle) (list-index (object-properties triangle) 0))
(define (triangle-p2 triangle) (list-index (object-properties triangle) 1))
(define (triangle-p3 triangle) (list-index (object-properties triangle) 2))
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
  #t)
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
(define (get-brightness hit)
  ; Gets brightness as a function of hit position, hit normal, light position, and light intensity
  ; Returns: number from 0 to 1
  (vec-mul
    light-color
    (max 0 (vec-dot (ray-dir hit) (vec-normalize (vec-sub light-pos (ray-orig hit)))))))      ; Angle between nhit and -lightdir
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
            ((lambda ()
               (define shadow-closest ; If object hit, cast shadow ray and calculate brightness if not in shadow
                 (ray-closest
                  (ray-create
                   (vec-add phit (vec-mul nhit bias))
                   (vec-sub light-pos phit)) objects))
               (if (or ; If no intersecting object with shadow ray or object is beyond light, illuminate
                    (null? shadow-closest)
                    (> (square (list-index shadow-closest 0)) (vec-distsq phit light-pos)))
                   (vec-mulvec ((material-color (object-material object)) object phit) (get-brightness hit))
                   vec-zero))))
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
(define camera-pos (vec-create 40 40 -60)) ; Camera should be on negative z for triangle winding to function properly
(define camera-lookat (vec-create 0 0 0))
(define camera-fov 90)
(define light-pos (vec-create 40 60 -50))
(define light-color (vec-create 1 1 1))
(define (sky-color ray)
  (vec-create 0 0 0))
(define meshes '( ; Go bEaRs! 💛🐻💙
; bear-leg4
067274712134982511033096241074614772039374801014195441041973142042312051005567431074614772039374801014195441093570182018218851045818381041973142042312051005567431093570182018218851045818381033940112005804711047072541041973142042312051005567431033214752130980111053919142029598152136541121007526761041973142042312051005567431067274712134982511033096241041973142042312051005567431029598152136541121007526761099733102003176731028516902062513452144874381067899302057669132003080081052531352028166372000979081023144412057669132003080081052531352062513452144874381067899302028166372000979081023144412062513452144874381067899302033214752130980111053919142033214752130980111053919142041973142042312051005567431028166372000979081023144412099733102003176731028516902033940112005804711047072541073374882005808861055178961033940112005804711047072541028166372000979081023144412041973142042312051005567431099733102003176731028516902057669132003080081052531352028166372000979081023144412099733102003176731028516902028166372000979081023144412033940112005804711047072541099733102003176731028516902094972462182143671062129392062513452144874381067899302122012502251768741032769551099733102003176731028516902067274712134982511033096241074614772039374801014195441067274712134982511033096241099733102003176731028516902099733102003176731028516902073374882005808861055178961093570182018218851045818381099733102003176731028516902093570182018218851045818381074614772039374801014195441093570182018218851045818381073374882005808861055178961033940112005804711047072541099733102003176731028516902122012502251768741032769551094972462182143671062129392
; bear-head
078980452276306591211204451061052362311527441177289301041710482220055521160887281070461592296969831264648231070995902319063931215498011078980452276306591211204451027499101323922201292382971018018812289948601307128831024672731305165811293760261018018812289948601307128831031985272329121441286177181070461592296969831264648231018018812289948601307128831070461592296969831264648231042308792261885071288354911070461592296969831264648231078980452276306591211204451066381732245311341230031721042308792261885071288354911070461592296969831264648231039695512226918851245616821039695512226918851245616821070461592296969831264648231066381732245311341230031721018018812289948601307128831042308792261885071288354911012375242257553731347482911041710482220055521160887281019472521218429851242534811039695512226918851245616821078980452276306591211204451041710482220055521160887281066381732245311341230031721041710482220055521160887281039695512226918851245616821066381732245311341230031721039695512226918851245616821017090262235261081335721631042308792261885071288354911076734711277533071210924401066189411324988061223225841070260541284966581259710771076734711277533071210924401054709141236158581148370201066189411324988061223225841066189411324988061223225841054709141236158581148370201055980541307874321167957191054709141236158581148370201076734711277533071210924401045126121224981211225480711027499101323922201292382971031985272329121441286177181018018812289948601307128831024672731305165811293760261070260541284966581259710771027499101323922201292382971018230001360099981152870001030736121364061621196205081057997381339814001185878301058833601376171041215887641075878171358799781218461511057997381339814001185878301030736121364061621196205081058833601376171041215887641057997381339814001185878301012680002362169991158900001030736121364061621196205081018230001360099981152870001055980541307874321167957191057997381339814001185878301075878171358799781218461511033878741362143401232628541066189411324988061223225841058833601376171041215887641058833601376171041215887641066189411324988061223225841075878171358799781218461511024672731305165811293760261012375242257553731347482911014916321240433161339344641055980541307874321167957191075878171358799781218461511066189411324988061223225841070260541284966581259710771066189411324988061223225841033878741362143401232628541024672731305165811293760261018018812289948601307128831012375242257553731347482911030736121364061621196205081033878741362143401232628541058833601376171041215887641070260541284966581259710771033878741362143401232628541027499101323922201292382971030057981222443831165581971019472521218429851242534811041710482220055521160887281039863432354895171180891781077814182346522751208134401057174532374464041212812631019472521218429851242534811045126121224981211225480711070260541284966581259710771045126121224981211225480711076734711277533071210924401070260541284966581259710771019472521218429851242534811030057981222443831165581971045126121224981211225480711045126121224981211225480711030057981222443831165581971054709141236158581148370201037692172361959691230787681012680002362169991158900001039863432354895171180891781033878741362143401232628541030736121364061621196205081012680002362169991158900001037692172361959691230787681033878741362143401232628541012680002362169991158900001066800162365107191223110751037692172361959691230787681057174532374464041212812631057174532374464041212812631037692172361959691230787681039863432354895171180891781077814182346522751208134401066800162365107191223110751057174532374464041212812631034610001243020001282470001024672731305165811293760261014916321240433161339344641070260541284966581259710771024672731305165811293760261034610001243020001282470001070260541284966581259710771034610001243020001282470001019472521218429851242534811019472521218429851242534811017090262235261081335721631039695512226918851245616821034610001243020001282470001014916321240433161339344641019472521218429851242534811014916321240433161339344641017090262235261081335721631019472521218429851242534811070461592296969831264648231037692172361959691230787681070995902319063931215498011070995902319063931215498011037692172361959691230787681066800162365107191223110751039863432354895171180891781061052362311527441177289301077814182346522751208134401070995902319063931215498011066800162365107191223110751077814182346522751208134401061052362311527441177289301070995902319063931215498011077814182346522751208134401070995902319063931215498011061052362311527441177289301078980452276306591211204451033878741362143401232628541031985272329121441286177181027499101323922201292382971033878741362143401232628541037692172361959691230787681031985272329121441286177181037692172361959691230787681070461592296969831264648231031985272329121441286177181018230001360099981152870001057997381339814001185878301055980541307874321167957191042308792261885071288354911017090262235261081335721631012375242257553731347482911017090262235261081335721631014916321240433161339344641012375242257553731347482911
; bear-torso
014846812340823901312043022046570002362210011188340002016110361364783941280185622046570002362210011188340002088023382334556351021443442011261922381702311034196841046570002362210011188340002011261922381702311034196841016110361364783941280185622091855531123928431020717961119842191256219061027704011090012251329576871062588411044933891150734051035052801091855531123928431020717961090012251329576871062588411006352382112853721137850732084133341155132791129932772079134281165911371066770242070136061148072201067679422006352382112853721137850732079134281165911371066770242044933891150734051035052801030057981222443831165581971029598152136541121007526761030057981222443831165581971044933891150734051035052801054709141236158581148370201054709141236158581148370201044933891150734051035052801090012251329576871062588411016110361364783941280185622096346061288218381203703082035417411265897061335790822096346061288218381203703082087000791196247621299830382035417411265897061335790822014846812340823901312043022016110361364783941280185622035417411265897061335790822096346061288218381203703082016110361364783941280185622057450001352210011194300002014846812340823901312043022035417411265897061335790822086755982251967241306213492062513452144874381067899302069920562148030251187642142006352382112853721137850732095489642292250441232541202014846812340823901312043022086755982251967241306213492079134281165911371066770242101085611141411481047261772070136061148072201067679422046570002362210011188340002014846812340823901312043022095489642292250441232541202071920002323079991095920001122012502251768741032769551066365082203488581088787301061052362311527441177289301071920002323079991095920001066365082203488581088787301012680002362169991158900001011261922381702311034196841071920002323079991095920001037502031129764211055505692025685581130947261019799442033214752130980111053919142006352382112853721137850732070136061148072201067679422037502031129764211055505692122012502251768741032769551067274712134982511033096241066365082203488581088787301006352382112853721137850732037502031129764211055505692033214752130980111053919142067274712134982511033096241029598152136541121007526761066365082203488581088787301025685581130947261019799442044933891150734051035052801029598152136541121007526761030057981222443831165581971041710482220055521160887281066365082203488581088787301030057981222443831165581971066365082203488581088787301029598152136541121007526761066365082203488581088787301041710482220055521160887281061052362311527441177289301060881962201267971314285372035417411265897061335790822033687391147901941308908292060881962201267971314285372033687391147901941308908292001686642124364311267959162033214752130980111053919142025685581130947261019799442029598152136541121007526761060881962201267971314285372086755982251967241306213492035417411265897061335790822095489642292250441232541202086755982251967241306213492069920562148030251187642142096346061288218381203703082057450001352210011194300002099495611277651711034045072062513452144874381067899302006352382112853721137850732033214752130980111053919142099495611277651711034045072057450001352210011194300002090012251329576871062588411062513452144874381067899302094972462182143671062129392069920562148030251187642142011261922381702311034196841057450001352210011194300002016110361364783941280185622011261922381702311034196841088023382334556351021443442071920002323079991095920001018230001360099981152870001011261922381702311034196841012680002362169991158900001039863432354895171180891781012680002362169991158900001071920002323079991095920001011261922381702311034196841090012251329576871062588411057450001352210011194300002011261922381702311034196841018230001360099981152870001090012251329576871062588411084133341155132791129932772096346061288218381203703082099495611277651711034045072084133341155132791129932772078259461162346951197548182096346061288218381203703082101085611141411481047261772099495611277651711034045072119842191256219061027704011091855531123928431020717961101085611141411481047261772119842191256219061027704011099495611277651711034045072101085611141411481047261772079134281165911371066770242099495611277651711034045072079134281165911371066770242084133341155132791129932772071920002323079991095920001061052362311527441177289301039863432354895171180891781122012502251768741032769551088023382334556351021443442094972462182143671062129392069920562148030251187642142028384262126112111201223912006352382112853721137850732088023382334556351021443442046570002362210011188340002095489642292250441232541202088023382334556351021443442122012502251768741032769551071920002323079991095920001088023382334556351021443442095489642292250441232541202106509112255694271120623162095489642292250441232541202069920562148030251187642142106509112255694271120623162106509112255694271120623162094972462182143671062129392088023382334556351021443442106509112255694271120623162069920562148030251187642142094972462182143671062129392033687391147901941308908292035417411265897061335790822087000791196247621299830382099495611277651711034045072090012251329576871062588411119842191256219061027704011054709141236158581148370201090012251329576871062588411055980541307874321167957191090012251329576871062588411018230001360099981152870001055980541307874321167957191078259461162346951197548182084133341155132791129932772022650231127687911203822152084133341155132791129932772006352382112853721137850732022650231127687911203822152028384262126112111201223912022650231127687911203822152006352382112853721137850732028384262126112111201223912001686642124364311267959162022650231127687911203822152
; bear-leg2
074321202041010421252358682061792872010378781196858882052552172047568021242361682074321202041010421252358682089389382007474451218448032061792872010378781196858882060881962201267971314285372001686642124364311267959162032035682024335941307602482086755982251967241306213492060881962201267971314285372077358602003013351285202472077358602003013351285202472074321202041010421252358682086755982251967241306213492074321202041010421252358682069920562148030251187642142086755982251967241306213492028384262126112111201223912052552172047568021242361682031393722045185181259841712028384262126112111201223912074321202041010421252358682052552172047568021242361682031393722045185181259841712001686642124364311267959162028384262126112111201223912001686642124364311267959162031393722045185181259841712032035682024335941307602482032035682024335941307602482077358602003013351285202472060881962201267971314285372032035682024335941307602482031393722045185181259841712019775392000422352257135142019775392000422352257135142077358602003013351285202472032035682024335941307602482077358602003013351285202472061792872010378781196858882089389382007474451218448032024044962007665031209812602077358602003013351285202472019775392000422352257135142077358602003013351285202472089389382007474451218448032074321202041010421252358682074321202041010421252358682028384262126112111201223912069920562148030251187642142031393722045185181259841712061792872010378781196858882024044962007665031209812602024044962007665031209812602061792872010378781196858882077358602003013351285202472019775392000422352257135142031393722045185181259841712024044962007665031209812602031393722045185181259841712052552172047568021242361682061792872010378781196858882
; bear-leg1
018341581001599381256888052036085161024047091313807112077270601008566801282218972088946691005683521222130182077270601008566801282218972055376371046266161241262742088946691005683521222130182055376371046266161241262742061197871012224521197660032077270601008566801282218972087000791196247621299830382096346061288218381203703082023286321006168141212603092061197871012224521197660032055376371046266161241262742088946691005683521222130182061197871012224521197660032023286321006168141212603092055376371046266161241262742028626551049675221258123632023286321006168141212603092028626551049675221258123632018341581001599381256888052023286321006168141212603092077270601008566801282218972088946691005683521222130182023286321006168141212603092023286321006168141212603092018341581001599381256888052077270601008566801282218972096346061288218381203703082078259461162346951197548182077270601008566801282218972033687391147901941308908292036085161024047091313807112001686642124364311267959162078259461162346951197548182022650231127687911203822152055376371046266161241262742055376371046266161241262742022650231127687911203822152028626551049675221258123632078259461162346951197548182055376371046266161241262742077270601008566801282218972033687391147901941308908292087000791196247621299830382036085161024047091313807112036085161024047091313807112087000791196247621299830382077270601008566801282218972036085161024047091313807112028626551049675221258123632001686642124364311267959162028626551049675221258123632022650231127687911203822152001686642124364311267959162028626551049675221258123632036085161024047091313807112018341581001599381256888052
; bear-leg3
044933891150734051035052801044373111041626691011391681091855531123928431020717961101085611141411481047261772091855531123928431020717961095277781008159001021773472093355781012920571046569191095277781008159001021773472083006331032576431009776771101085611141411481047261772095277781008159001021773472070136061148072201067679422091855531123928431020717961083006331032576431009776771095277781008159001021773472057299961002319541052361232095277781008159001021773472093355781012920571046569191070136061148072201067679422095277781008159001021773472057299961002319541052361232057299961002319541052361232037502031129764211055505692070136061148072201067679422044373111041626691011391681025685581130947261019799442025406531000524131018944262025685581130947261019799442044373111041626691011391681044933891150734051035052801037502031129764211055505692057299961002319541052361232025406531000524131018944262025406531000524131018944262025685581130947261019799442037502031129764211055505692083006331032576431009776771091855531123928431020717961044373111041626691011391681093355781012920571046569191083006331032576431009776771044373111041626691011391681057299961002319541052361232093355781012920571046569191058534461005552711058945731057299961002319541052361232058534461005552711058945731025406531000524131018944262025406531000524131018944262058534461005552711058945731044373111041626691011391681093355781012920571046569191044373111041626691011391681058534461005552711058945731
))
(define objects
  (append
    (list ; Normal objects
      (plane-create (vec-create 0 0 0) (vec-create 0 1 0)
        (material-create (make-checkerboard-color (vec-create 0.3 0.3 0.3) (vec-create 0.5 0.5 0.5) 10) (vec-create 0.2 0.2 0.2) vec-zero 0))

      ;(sphere-create 5 (vec-create -5 5 -10)
      ;  (material-create (make-constant-color (vec-create 0.2 0.2 0.2)) (vec-create 0.8 0.8 0.8) vec-zero 0))
      ;(sphere-create 15 (vec-create 15 15 -5)
      ;  (material-create (make-constant-color (vec-create 0 0 0)) (vec-create 0.8 0.8 0.8) (vec-create 1 1 1) 1.1))
      ;(sphere-create 10 (vec-create 15 10 0)
      ;  (material-create (make-constant-color (vec-create 0.9922 0.7098 0.0824)) vec-zero vec-zero 0))
      (sphere-create 5 (vec-create -30 5 -20)
        (material-create (make-constant-color (vec-create 0 0.196 0.3943)) vec-zero vec-zero 0)))
    (map ; Mesh objects
      (lambda (num)
        (define coords (num-to-coords num))
        (display "Found ")
        (display (/ (length coords) 3))
        (display " faces\n")
        ;(sphere-create 0 vec-zero (material-create (make-constant-color vec-zero) vec-zero vec-zero 0)))
        (mesh-create coords (material-create (make-constant-color (vec-create 0.9922 0.7098 0.0824)) vec-zero vec-zero 0)))
      meshes)))

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
