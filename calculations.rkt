#lang racket

(require "structs.rkt"
         (file "~/.location-weather-prefs.rkt"))

(provide (all-defined-out))

(define const-hPa 1012.0)

(define (osha-wbgt tempc-web-bulb tempc-globe tempc-air)
  (+ (* 0.7 tempc-web-bulb)
     (* 0.2 tempc-globe)
     (* 0.1 tempc-air)))

(define (meters-per-hour miles-per-hour)
  (* 1609.34 miles-per-hour))

(define const-sigma 5.67e-8)
(define const-h 0.315)

(define (f2c tempf)
  (* (/ 5.0 9.0) (- tempf 32.0)))

(define (c2f tempc)
  (+ 32 (* tempc (/ 9.0 5.0))))

(define (atmospheric-vapor-pressure dewpointc ambient-tempc hPa)
  (* (exp (/ (* 17.67 (- dewpointc ambient-tempc))
             (+ dewpointc 243.5)))
     (+ 1.0007 (* 0.00000346 hPa))
     (* 6.112
        (exp (/ (* 17.502 ambient-tempc)
                (+ 240.97 ambient-tempc))))))

(define (thermal-emmissivity dewpointc ambient-tempc hPa)
  (* 0.575 (expt (atmospheric-vapor-pressure dewpointc ambient-tempc hPa) 1/7)))

(define (wbgt-part-B solar-watts-per-meter-squared
                     direct-beam-radiation-from-sun diffuse-radiation-from-sun zenith-r
                     dewpointc ambient-tempc hPa)
  (+ (* solar-watts-per-meter-squared
        (+ (/ direct-beam-radiation-from-sun
              (* 4 const-sigma (cos zenith-r)))
           (* (/ 1.2 const-sigma) diffuse-radiation-from-sun)))
     (* (thermal-emmissivity dewpointc ambient-tempc hPa)
        (expt ambient-tempc 4))))

(define (wbgt-part-C wind-meters-per-hour)
  (/ (* const-h (expt wind-meters-per-hour 0.58)) 5.53865e-8))

(define (wbgt-C wind-meters-per-hour ambient-tempc dewpointc solar-watts-per-meter-squared
                direct-beam-radiation-from-sun diffuse-radiation-from-sun zenith-r hPa)
  (let ([C (wbgt-part-C wind-meters-per-hour)])
    (/ (+ (wbgt-part-B solar-watts-per-meter-squared
                       direct-beam-radiation-from-sun diffuse-radiation-from-sun zenith-r
                       dewpointc ambient-tempc hPa)
          (* C ambient-tempc)
          7680000)
       (+ C 256000))))

(define (wbgt-fahrenheit o)
  (c2f (wbgt-celsius o)))

(define (wbgt-celsius o)
  (let* ([r (obs-raw o)])
    (wbgt-C
     (meters-per-hour (raw-obs-windspeed r))
     (f2c (raw-obs-temp r))
     (f2c (raw-obs-dewpoint r))
     (est-irradiance (obs-cover o) (raw-obs-day? r))
     (radiation-from-sun 'direct-beam 'summer)
     (radiation-from-sun 'diffuse 'summer)
     zenith-radians
     1011.85)))

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

(define (radiation-from-sun kind season)
  (cond [(eq? kind 'direct-beam) (cond [(eq? season 'summer) 0.75]
                                       [(eq? season 'winter) 0.67]
                                       [else 0.71])]
        [(eq? kind 'diffuse) (cond [(eq? season 'summer) 0.25]
                                   [(eq? season 'winter) 0.33]
                                   [else 0.29])]
        [else 0.5]))
  
(define (heat-category-class tempf)
  (let ([x (heat-category tempf)])
    (cond [(= x 2) "heatgreen"]
          [(= x 3) "heatyellow"]
          [(= x 4) "heatred"]
          [(= x 5) "heatblack"]
          [else "heatwhite"])))

(define (heat-category wbgt)
  (cond [(and (>= wbgt 78.0) (< wbgt 82.0)) 1]
        [(and (>= wbgt 82.0) (< wbgt 85.0)) 2]
        [(and (>= wbgt 85.0) (< wbgt 88.0)) 3]
        [(and (>= wbgt 88.0) (< wbgt 90.0)) 4]
        [(>= wbgt 90.0) 5]
        [else 0]))
  
(define days-of-week (vector "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
  
(define (day-of-week wday)
  (vector-ref days-of-week wday))

