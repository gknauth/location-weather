#lang racket

(require "structs.rkt"
         (file "~/.location-weather-prefs.rkt"))

(provide (all-defined-out))

(define const-hPa 1012.0)

(define (osha-wbgt tempc-web-bulb tempc-globe tempc-air)
  (+ (* 0.7 tempc-web-bulb)
     (* 0.2 tempc-globe)
     (* 0.1 tempc-air)))

(define (mi/h->m/h miles-per-hour)
  (* 1609.34 miles-per-hour))

(define const-sigma 5.67e-8) ; Boltzman constant
(define const-h 0.315)

(define (f2c tempf)
  (* (/ 5.0 9.0) (- tempf 32.0)))

(define (c2f tempc)
  (+ 32 (* tempc (/ 9.0 5.0))))

(define (atmospheric-vapor-pressure dewpointc ambient-tempc hPa)
  (* (exp (/ (* 17.67 (- dewpointc ambient-tempc))
             (+ dewpointc 243.5)))
     (+ 1.0007 (* 3.46e-6 hPa))
     (* 6.112
        (exp (/ (* 17.502 ambient-tempc)
                (+ 240.97 ambient-tempc))))))

(define (thermal-emmissivity dewpointc ambient-tempc hPa)
  (* 0.575 (expt (atmospheric-vapor-pressure dewpointc ambient-tempc hPa) 1/7)))

(define (globe-part-B solar-watts-per-meter-squared
                      direct-beam-radiation-from-sun diffuse-radiation-from-sun zenith-r
                      dewpointc ambient-tempc hPa)
  (+ (* solar-watts-per-meter-squared
        (+ (/ direct-beam-radiation-from-sun
              (* 4 const-sigma (cos zenith-r)))
           (* (/ 1.2 const-sigma) diffuse-radiation-from-sun)))
     (* (thermal-emmissivity dewpointc ambient-tempc hPa)
        (expt ambient-tempc 4))))

(define (globe-part-c wind-m/h)
  (/ (* const-h (expt wind-m/h 0.58)) 5.53865e-8))

(define (globe-C wind-m/h ambient-tempc dewpointc
                 solar-watts-per-meter-squared
                 direct-beam-radiation-from-sun
                 diffuse-radiation-from-sun
                 zenith-r
                 hPa)
  (let ([C (globe-part-c wind-m/h)])
    (/ (+ (globe-part-B solar-watts-per-meter-squared
                        direct-beam-radiation-from-sun diffuse-radiation-from-sun zenith-r
                        dewpointc
                        ambient-tempc
                        hPa)
          (* C ambient-tempc)
          7680000)
       (+ C 256000))))

(define (globe-F wind-m/h ambient-tempf dewpointf
                 solar-watts-per-meter-squared
                 direct-beam-radiation-from-sun
                 diffuse-radiation-from-sun
                 zenith-r
                 hPa)
  (let ([dewpointc (f2c dewpointf)]
        [ambient-tempc (f2c ambient-tempf)])
    (c2f (globe-C wind-m/h ambient-tempc dewpointc
                  solar-watts-per-meter-squared
                  direct-beam-radiation-from-sun
                  diffuse-radiation-from-sun
                  zenith-r
                  hPa))))

(define (wgbt-C direct-sun? wetbulbc
                wind-m/h ambient-tempc dewpointc 
                solar-watts-per-meter-squared
                direct-beam-radiation-from-sun
                diffuse-radiation-from-sun
                zenith-r
                hPa)
  (let* ([globeC (globe-C wind-m/h ambient-tempc dewpointc
                          solar-watts-per-meter-squared
                          direct-beam-radiation-from-sun
                          diffuse-radiation-from-sun
                          zenith-r
                          hPa)])
    (printf "~a ~a ~a\n" wetbulbc globeC ambient-tempc)
    (if direct-sun?
        (+ (* .7 wetbulbc) (* .2 globeC) (* .1 ambient-tempc))
        (+ (* .7 wetbulbc) (* .3 globeC)))))

(define (globe-C-o o)
  (let* ([r (obs-raw o)])
    (globe-C
     (mi/h->m/h (raw-obs-windspeed r))
     (f2c (raw-obs-temp r))
     (f2c (raw-obs-dewpoint r))
     (est-irradiance (obs-cover o) (raw-obs-day? r))
     (radiation-from-sun 'summer 'direct-beam )
     (radiation-from-sun 'summer 'diffuse)
     zenith-radians
     1011.85)))

(define (globe-F-o o)
  (c2f (globe-C-o o)))

(define (wbgt-C-o o)
  (let ([r (obs-raw o)])
    (wgbt-C (obs-day? o)
            (f2c (raw-obs-wetbulb r))
            (raw-obs-windspeed r)
            (f2c (raw-obs-temp r))
            (f2c (raw-obs-dewpoint r))
            (est-irradiance (obs-cover o) (obs-day? o))
            (if (obs-day? o) (radiation-from-sun 'summer 'direct-beam ) 0)
            (if (obs-day? o) (radiation-from-sun 'summer 'diffuse) 0)
            zenith-radians
            1013.2)))

(define (wbgt-F-o o)
  (c2f (wbgt-C-o o)))

; cover is 0..100
(define (sky cover)
  (let ([eighths (/ cover (/ 100 8.))])
    (cond [(<  eighths 1) "SKC"]
          [(<= eighths 2) "FEW"]
          [(<= eighths 4) "SCT"]
          [(<= eighths 7) "BKN"]
          [else "OVC"])))

(define (est-irradiance sky-cover day?)
  (if day?
      (cond [(string=? sky-cover "SKC") 990]
            [(string=? sky-cover "FEW") 980]
            [(string=? sky-cover "SCT") 980]
            [(string=? sky-cover "BKN") 710]
            [(string=? sky-cover "OVC") 250]
            [else 0])
      0))

(define (radiation-from-sun season kind)
  (cond [(eq? kind 'direct-beam) (cond [(eq? season 'summer) 0.75]
                                       [(eq? season 'winter) 0.67]
                                       [else 0.71])]
        [(eq? kind 'diffuse) (cond [(eq? season 'summer) 0.25]
                                   [(eq? season 'winter) 0.33]
                                   [else 0.29])]
        [else 0]))
  
(define (heat-category-class tempf)
  (let ([x (heat-category tempf)])
    (cond [(= x 2) "heatgreen"]
          [(= x 3) "heatyellow"]
          [(= x 4) "heatred"]
          [(= x 5) "heatblack"]
          [else "heatwhite"])))

(define (heat-category wbgt-F)
  (cond [(and (>= wbgt-F 78.0) (< wbgt-F 82.0)) 1]
        [(and (>= wbgt-F 82.0) (< wbgt-F 85.0)) 2]
        [(and (>= wbgt-F 85.0) (< wbgt-F 88.0)) 3]
        [(and (>= wbgt-F 88.0) (< wbgt-F 90.0)) 4]
        [(>= wbgt-F 90.0) 5]
        [else 0]))
  
(define days-of-week (vector "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
  
(define (day-of-week wday)
  (vector-ref days-of-week wday))

(define (test-globe-F wind-mi/h tempf dewpointf cover day? hPa)
  (globe-F (mi/h->m/h wind-mi/h) tempf dewpointf
           (est-irradiance cover day?)
           (radiation-from-sun 'summer 'direct-beam )
           (radiation-from-sun 'summer 'diffuse)
           zenith-radians
           hPa))

(module+ test
  ;; Tests to be run with raco test
  (require rackunit)
  (check-equal? (sky 100) "OVC")
  (check-equal? (sky 88) "OVC")
  (check-equal? (sky 85) "BKN")
  (check-equal? (sky 51) "BKN")
  (check-equal? (sky 40) "SCT")
  (check-equal? (sky 30) "SCT")
  (check-equal? (sky 20) "FEW")
  (check-equal? (sky 10) "SKC")
  (check-equal? (sky 0) "SKC")
  (check-equal? (est-irradiance "SKC" #t) 990)
  (check-equal? (est-irradiance "FEW" #t) 980)
  (check-equal? (est-irradiance "SCT" #t) 980)
  (check-equal? (est-irradiance "BKN" #t) 710)
  (check-equal? (est-irradiance "OVC" #t) 250)
  (check-equal? (est-irradiance "SKC" #f) 0)
  (check-equal? (radiation-from-sun 'summer 'direct-beam) 0.75)
  (check-equal? (radiation-from-sun 'summer 'diffuse) 0.25)
  (check-equal? (radiation-from-sun 'winter 'direct-beam) 0.67)
  (check-equal? (radiation-from-sun 'winter 'diffuse) 0.33)
  (check-equal? (radiation-from-sun 'other 'direct-beam) 0.71)
  (check-equal? (radiation-from-sun 'other 'diffuse) 0.29)
  (check-equal? (radiation-from-sun 'foo 'bar) 0)
  (check-equal? (heat-category-class 75.0) "heatwhite")
  (check-equal? (heat-category-class 80.0) "heatwhite")
  (check-equal? (heat-category-class 84.0) "heatgreen")
  (check-equal? (heat-category-class 87.0) "heatyellow")
  (check-equal? (heat-category-class 89.0) "heatred")
  (check-equal? (heat-category-class 95.0) "heatblack")
  (check-equal? (heat-category 75.0) 0)
  (check-equal? (heat-category 80.0) 1)
  (check-equal? (heat-category 84.0) 2)
  (check-equal? (heat-category 87.0) 3)
  (check-equal? (heat-category 89.0) 4)
  (check-equal? (heat-category 95.0) 5)
  )
