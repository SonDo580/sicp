;;; Project 1, 6.001, Spring 2005

;;; idea is to simulate a baseball robot

;; imagine hitting a ball with an initial velocity of v 
;; at an angle alpha from the horizontal, at a height h
;; we would like to know how far the ball travels.

;; as a first step, we can just model this with simple physics
;; so the equations of motion for the ball have a vertical and a 
;; horizontal component

;; the vertical component is governed by
;; y(t) = v sin alpha t + h - 1/2 g t^2 
;; where g is the gravitational constant of 9.8 m/s^2

;; the horizontal component is governed by
;; x(t) = v cos alpha t
;; assuming it starts at the origin

;; First, we want to know when the ball hits the ground
;; this is governed by the quadratic equation, so we just need to know when 
;; y(t)=0 (i.e. for what t_impact is y(t_impact)= 0).
;; note that there are two solutions, only one makes sense physically

(define square
  (lambda (x) (* x x)))

;; these are constants that will be useful to us
(define gravity 9.8)  ;; in m/s^2
(define pi 3.14159)

;; Problem 1

;; This is just a raw calculation
(define position
  (lambda (a v u t)
    (+ (* 0.5 a (square t)) 
       (* v t)
       u)))


;; Problem 2

;; root1: (-b + sqrt(b^2 - 4ac)) / 2a
(define root1
  (lambda (a b c)
    (/ (+ (- b)
          (sqrt (- (square b) 
                   (* 4 a c)))) 
       (* 2 a))))

;; root2: (-b - sqrt(b^2 - 4ac)) / 2a
(define root2
  (lambda (a b c)
    (/ (- (- b)
          (sqrt (- (square b) 
                   (* 4 a c))))
       (* 2 a))))


;; Problem 3

;; y = h + vy*t - 0.5g*t^2
;; to yield positive t, we use root2
(define time-to-impact
  (lambda (vertical-velocity elevation)
    (root2 (- (* 0.5 gravity))
           vertical-velocity
           elevation)))

;; Note that if we want to know when the ball drops to a particular height r 
;; (for receiver), we have

(define time-to-height
  (lambda (vertical-velocity elevation target-elevation)
    (root2 (- (* 0.5 gravity))
           vertical-velocity
           (- elevation target-elevation))))

;; Problem 4

;; once we can solve for t_impact, we can use it to figure out how far the ball went

;; conversion procedure
(define degree-to-radian
  (lambda (deg)
    (/ (*  deg pi) 180.)))

;; y = h + v*sin(a)*t - 0.5g*t^2
;; x = v*cos(a)*t
(define travel-distance-simple
  (lambda (elevation velocity angle)
    (define radian (degree-to-radian angle))
    (define vx (* velocity (cos radian)))
    (define vy (* velocity (sin radian)))
    (define flight-time (time-to-impact vy elevation))
    (* vx flight-time)))

;; let's try this out for some example values.  Note that we are going to 
;; do everything in metric units, but for quaint reasons it is easier to think
;; about things in English units, so we will need some conversions.

(define meters-to-feet
  (lambda (m)
    (/ (* m 39.6) 12)))

(define feet-to-meters
  (lambda (f)
    (/ (* f 12) 39.6)))

(define hours-to-seconds
  (lambda (h)
    (* h 3600)))

(define seconds-to-hours
  (lambda (s)
    (/ s 3600)))

;; what is time to impact for a ball hit at a height of 1 meter
;; with a velocity of 45 m/s (which is about 100 miles/hour)
;; at an angle of 0 (straight horizontal)
;; at an angle of (/ pi 2) radians or 90 degrees (straight vertical)
;; at an angle of (/ pi 4) radians or 45 degrees

;; what is the distance traveled in each case?
;; record both in meters and in feet

(travel-distance-simple 1 45 0)
(travel-distance-simple 1 45 90)
(travel-distance-simple 1 45 45)

;; Problem 5

;; these sound pretty impressive, but we need to look at it more carefully

;; first, though, suppose we want to find the angle that gives the best
;; distance
;; assume that angle is between 0 and (/ pi 2) radians or between 0 and 90
;; degrees

(define alpha-increment 0.01)

(define alpha-degree-increment 1)

(define find-best-angle
  (lambda (velocity elevation)
    (define (iter degree optimal-degree max-distance)
      (define distance (travel-distance-simple elevation velocity degree))
      (define next-degree (- degree alpha-degree-increment))
      (cond ((< degree 0) optimal-degree)
            ((> distance max-distance) (iter next-degree degree distance))
            (else (iter next-degree optimal-degree max-distance))))
    (iter 90 -1 -1)))

;; find best angle
;; try for other velocities
;; try for other heights

(find-best-angle 15 1)
(find-best-angle 45 2)
(find-best-angle 20 1)

;; Conclusion: the optimal angle is around 45 degrees (pi/4 radians)
;; Note: actually the optimal angle is exactly 45 degrees

;; Problem 6

;; problem is that we are not accounting for drag on the ball (or on spin 
;; or other effects, but let's just stick with drag)
;;
;; Newton's equations basically say that ma = F, and here F is really two 
;; forces.  One is the effect of gravity, which is captured by mg.  The
;; second is due to drag, so we really have
;;
;; a = drag/m + gravity
;;
;; drag is captured by 1/2 C rho A vel^2, where
;; C is the drag coefficient (which is about 0.5 for baseball sized spheres)
;; rho is the density of air (which is about 1.25 kg/m^3 at sea level 
;; with moderate humidity, but is about 1.06 in Denver)
;; A is the surface area of the cross section of object, which is pi D^2/4 
;; where D is the diameter of the ball (which is about 0.074m for a baseball)
;; thus drag varies by the square of the velocity, with a scaling factor 
;; that can be computed

;; We would like to again compute distance , but taking into account 
;; drag.
;; Basically we can rework the equations to get four coupled linear 
;; differential equations
;; let u be the x component of velocity, and v be the y component of velocity
;; let x and y denote the two components of position (we are ignoring the 
;; third dimension and are assuming no spin so that a ball travels in a plane)
;; the equations are
;;
;; dx/dt = u
;; dy/dt = v
;; du/dt = -(drag_x/m + g_x)
;; dv/dt = -(drag_y/m + g_y)
;; we have g_x = - and g_y = - gravity
;; to get the components of the drag force, we need some trig.
;; let speeed = (u^2+v^2)^(1/2), then
;; drag_x = - drag * u /speed
;; drag_y = - drag * v /speed
;; where drag = beta speed^2
;; and beta = 1/2 C rho pi D^2/4
;; note that we are taking direction into account here

;; we need the mass of a baseball -- which is about .15 kg.

;; so now we just need to write a procedure that performs a simple integration
;; of these equations -- there are more sophisticated methods but a simple one 
;; is just to step along by some step size in t and add up the values

;; dx = u dt
;; dy = v dt
;; du = - 1/m speed beta u dt
;; dv = - (1/m speed beta v + g) dt

;; initial conditions
;; u_0 = V cos alpha
;; v_0 = V sin alpha
;; y_0 = h
;; x_0 = 0

;; we want to start with these initial conditions, then take a step of size dt
;; (which could be say 0.1) and compute new values for each of these parameters
;; when y reaches the desired point (<= 0) we stop, and return the distance (x)

(define drag-coeff 0.5)
(define density 1.25)  ; kg/m^3
(define mass .145)  ; kg
(define diameter 0.074)  ; m
(define beta (* .5 drag-coeff density (* pi .25 (square diameter))))

(define integrate
  (lambda (x0 y0 u0 v0 dt g m beta)
    (define (u-change u v)
      (/ (* (- beta) u dt 
            (sqrt (+ (square u) (square v)))) 
         m))
    (define (v-change u v)
      (- (/ (* (- beta) v dt 
               (sqrt (+ (square u) (square v)))) 
            m)
         (* g dt)))
    (define (iter x y u v)
      (define dx (* u dt))
      (define dy (* v dt))
      (define du (u-change u v))
      (define dv (v-change u v))
      (cond ((not (> y 0)) x)
            (else (iter (+ x dx) (+ y dy) (+ u du) (+ v dv)))))
    (iter x0 y0 u0 v0)))

(define travel-distance
  (lambda (elevation velocity degree)
          (define radian (degree-to-radian degree))
          (define u (* velocity (cos radian)))
          (define v (* velocity (sin radian)))
          (define dt 0.01)
          (integrate 0 elevation u v dt gravity mass beta)))


;; RUN SOME TEST CASES
(travel-distance 1 45 45)
(travel-distance 1 40 45)
(travel-distance 1 35 45)

;; what about Denver?
(define density-denver 1.06)  ;  kg/m^3
(define beta-denver (* .5 drag-coeff density-denver (* pi .25 (square diameter))))
;; then calculate similarly


;; Problem 7
 
;; now let's turn this around.  Suppose we want to throw the ball.  The same
;; equations basically hold, except now we would like to know what angle to 
;; use, given a velocity, in order to reach a given height (receiver) at a 
;; given distance


;; a cather trying to throw someone out at second has to get it roughly 36 m
;; (or 120 ft) how quickly does the ball get there, if he throws at 55m/s,
;;  at 45m/s, at 35m/s?

;; try out some times for distances (30, 60, 90 m) or (100, 200, 300 ft) 
;; using 45m/s

(define reach-target?
  (lambda (distance desired-distance)
          (< (abs (- distance desired-distance)) 0.5)))

(define integrate-time
  (lambda (x0 y0 u0 v0 dt g m beta desired-distance)
          (define (u-change u v)
            (/ (* (- beta) u dt
                  (sqrt (+ (square u) (square v))))
               m))
          (define (v-change u v)
            (- (/ (* (- beta) v dt
                     (sqrt (+ (square u) (square v))))
                  m)
               (* g dt)))
          (define (iter x y u v t)
            (define dx (* u dt))
            (define dy (* v dt))
            (define du (u-change u v))
            (define dv (v-change u v))
            (cond ((and (not (> y 0)) (reach-target? x desired-distance)) t)
                  ((and (not (> y 0)) (not (reach-target? x desired-distance))) -1)
                  (else (iter (+ x dx) (+ y dy) (+ u du) (+ v dv) (+ t dt)))))
          (iter x0 y0 u0 v0 0)))

(define travel-time
  (lambda (elevation velocity degree desired-distance)
          (define radian (degree-to-radian degree))
          (define u (* velocity (cos radian)))
          (define v (* velocity (sin radian)))
          (define dt 0.01)
          (integrate-time 0 elevation u v dt gravity mass beta desired-distance)))

(define should-update-time?
  (lambda (t min-time)
          (and (not (< t 0)) 
               (or (< t min-time)
                   (< min-time 0)))))

(define find-shortest-time
  (lambda (elevation velocity desired-distance)
          (define (iter degree min-time)
            (define next-degree (+ degree alpha-degree-increment))
            (define t (travel-time elevation velocity degree desired-distance))
            (cond ((> degree 90) min-time)
                  ((should-update-time? t min-time) (iter next-degree t))
                  (else (iter next-degree min-time))))
          (iter -90 -1)))

;; Some cases
(find-shortest-time 1 55 36)
(find-shortest-time 1 45 36)
(find-shortest-time 1 35 36)

;; Problem 8
;; assume that when a ball bounces, 
;; it leaves the ground at the same angle as it was initially thrown, 
;; but with half the velocity. 

(define travel-distance-with-bounces
  (lambda (elevation velocity degree max-bounces)
          (define (iter distance v bounces)
            (define d (travel-distance elevation v degree))
            (cond ((> bounces max-bounces) distance)
                  (else (iter (+ distance d)
                              (/ v 2)
                              (+ bounces 1)))))
          (iter 0 velocity 0)))

(define travel-distance-with-arbitrary-bounces
  (lambda (elevation velocity degree)
          (define (iter distance v)
            (define d (travel-distance elevation v degree))
            (cond ((< d 0.01) distance)
                  (else (iter (+ distance d)
                              (/ v 2)))))
          (iter 0 velocity)))

;; Some cases
(travel-distance-with-bounces 1 30 45 0)
(travel-distance-with-bounces 1 30 45 1)
(travel-distance-with-bounces 1 30 45 2)
(travel-distance-with-arbitrary-bounces 1 30 45)

;; Problem 9
;; Assume that the collision is elastic
;; u' = u; v' = -v

(define integrate-velocity
  (lambda (x0 y0 u0 v0 dt g m beta return-u)
          (define (u-change u v)
            (/ (* (- beta) u dt
                  (sqrt (+ (square u) (square v))))
               m))
          (define (v-change u v)
            (- (/ (* (- beta) v dt
                     (sqrt (+ (square u) (square v))))
                  m)
               (* g dt)))
          (define (iter x y u v)
            (define dx (* u dt))
            (define dy (* v dt))
            (define du (u-change u v))
            (define dv (v-change u v))
            (cond ((not (> y 0)) (if return-u u v))
                  (else (iter (+ x dx) (+ y dy) (+ u du) (+ v dv)))))
          (iter x0 y0 u0 v0)))

(define next-u
  (lambda (elevation velocity degree)
          (define radian (degree-to-radian degree))
          (define u (* velocity (cos radian)))
          (define v (* velocity (sin radian)))
          (define dt 0.01)
          (integrate-velocity 0 elevation u v dt gravity mass beta true)))

(define next-v
  (lambda (elevation velocity degree)
          (define radian (degree-to-radian degree))
          (define u (* velocity (cos radian)))
          (define v (* velocity (sin radian)))
          (define dt 0.01)
          (integrate-velocity 0 elevation u v dt gravity mass beta false)))

(define next-degree
  (lambda (u v)
          (atan (/ (abs v) u))))

(define travel-distance-with-bounces-v2
  (lambda (elevation initial-velocity initial-degree max-bounces)
          (define (iter distance degree velocity bounces)
            (define d (travel-distance elevation velocity degree))
            (define u (next-u elevation velocity degree))
            (define v (next-v elevation velocity degree))
            (cond ((> bounces max-bounces) distance)
                  (else (iter (+ distance d)
                              (next-degree u v)
                              (sqrt (+ (square u) (square v)))
                              (+ bounces 1)))))
          (iter 0 initial-degree initial-velocity 0)))

;; Some cases
(travel-distance-with-bounces-v2 1 30 45 0)
(travel-distance-with-bounces-v2 1 30 45 1)
(travel-distance-with-bounces-v2 1 30 45 2)