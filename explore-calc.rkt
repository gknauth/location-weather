#lang racket

(require "structs.rkt"
         (file "~/.location-weather-prefs.rkt")
         "calculations.rkt")


(define (zip . ls) (apply map (cons list ls)))

(define inputs-globe-part-c (list 0 1 5 10 15 20))
(printf "globe-part-c:\n")
(zip inputs-globe-part-c
     (map (λ (x)
            (globe-part-c (mi/h->m/h x))) inputs-globe-part-c))

(struct inb (solar-watts-per-meter-squared
             direct-beam-radiation-from-sun
             diffuse-radiation-from-sun
             zenith-r
             dewpointc
             ambient-tempc
             hPa)
  #:transparent)
(define inputs-globe-part-b
  (list
   (inb (est-irradiance "SKC" #t)
         (radiation-from-sun 'summer 'direct-beam)
         (radiation-from-sun 'summer 'diffuse)
         zenith-radians
         (f2c 72.0)
         (f2c 78.8)
         1013.2)
   (inb (est-irradiance "OVC" #t)
                  (radiation-from-sun 'summer 'direct-beam)
         (radiation-from-sun 'summer 'diffuse)
         zenith-radians
         (f2c 72.0)
         (f2c 78.8)
         1013.2)
   (inb (est-irradiance "SKC" #t)
                  (radiation-from-sun 'summer 'direct-beam)
         (radiation-from-sun 'summer 'diffuse)
         zenith-radians
         (f2c 80.0)
         (f2c 90.0)
         1013.2)
   (inb (est-irradiance "OVC" #t)
                  (radiation-from-sun 'summer 'direct-beam)
         (radiation-from-sun 'summer 'diffuse)
         zenith-radians
         (f2c 80.0)
         (f2c 90.0)
         1013.2)
   (inb (est-irradiance "SKC" #f)
                  (radiation-from-sun 'summer 'direct-beam)
         (radiation-from-sun 'summer 'diffuse)
         zenith-radians
         (f2c 80.0)
         (f2c 90.0)
         1013.2)
   (inb (est-irradiance "OVC" #f)
                  (radiation-from-sun 'summer 'direct-beam)
         (radiation-from-sun 'summer 'diffuse)
         zenith-radians
         (f2c 80.0)
         (f2c 90.0)
         1013.2)))
(printf "globe-part-b:\n")
(zip inputs-globe-part-b
     (map (λ (x)
            (globe-part-B (inb-solar-watts-per-meter-squared x)
                          (inb-direct-beam-radiation-from-sun x)
                          (inb-diffuse-radiation-from-sun x)
                          (inb-zenith-r x)
                          (inb-dewpointc x)
                          (inb-ambient-tempc x)
                          (inb-hPa x)))
          inputs-globe-part-b))

(struct inte (dewpointc ambient-tempc hPa) #:transparent)
(define inputs-thermal-emmissivity
  (list
   (inte (f2c 72.0) (f2c 78.0) 1013.2)
   (inte (f2c 90.0) (f2c 80.0) 1013.2)))
(printf "thermal-emmissivity:\n")
(zip inputs-thermal-emmissivity
     (map (λ (x)
            (thermal-emmissivity (inte-dewpointc x)
                                 (inte-ambient-tempc x)
                                 (inte-hPa x)))
          inputs-thermal-emmissivity))

(struct invp (dewpointc ambient-tempc hPa) #:transparent)
(define inputs-atmospheric-vapor-pressure
  (list
   (invp (f2c 72.0) (f2c 78.0) 1013.2)
   (invp (f2c 90.0) (f2c 80.0) 1013.2)))
(printf "atmospheric-vapor-pressure:\n")
(zip inputs-atmospheric-vapor-pressure
     (map (λ (x)
            (atmospheric-vapor-pressure (invp-dewpointc x)
                                        (invp-ambient-tempc x)
                                        (invp-hPa x)))
          inputs-atmospheric-vapor-pressure))
