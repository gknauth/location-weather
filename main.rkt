#lang racket

(require net/url
         json
         gregor
         format-numbers
         web-server/servlet
         web-server/servlet-env
         (file "~/.location-weather-prefs.rkt"))

(define hours 72)

(define s-hourly-url
  (string-append
   "http://apidev.accuweather.com/forecasts/v1/hourly/240hour/"
   location-key "?apikey=" accuwx-api-key "&details=true"))

(define (get-hourly-forecast)
  (call/input-url (string->url s-hourly-url) get-pure-port read-json))
  
(define hourly-json (get-hourly-forecast))
  
(struct raw-obs (ceil cover dt dewpoint precip? wx day? rain-in prob-rain realfeel rh temp uv uvtext viz wxicon windir wintxt windspeed gust) #:transparent)
  
(struct obs (raw ceil cover dt dewpoint precip? wx day? rain-in prob-rain realfeel rh temp uv uvtext viz wxicon windir wintxt windspeed gust) #:transparent)

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
  (let ([r (obs-raw o)])
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
      0
      (cond [(string=? sky-cover "SKC") 990]
            [(string=? sky-cover "FEW") 980]
            [(string=? sky-cover "SCT") 980]
            [(string=? sky-cover "BKN") 710]
            [(string=? sky-cover "OVC") 250]
            [else 0])))

(define (radiation-from-sun kind season)
  (cond [(eq? kind 'direct-beam) (cond [(eq? season 'summer) 0.75]
                                       [(eq? season 'winter) 0.67]
                                       [else 0.71])]
        [(eq? kind 'diffuse) (cond [(eq? season 'summer) 0.25]
                                   [(eq? season 'winter) 0.33]
                                   [else 0.29])]
        [else 0.5]))
  
(define (fmt-i-03d n)
  (let ((s (format "~a" n)))
    (cond [(= (string-length s) 2) (string-append "0" s)]
          [(= (string-length s) 1) (string-append "00" s)]
          [else s])))
  
(define (feet-hundreds ft)
  (fmt-i-03d (exact-round (/ ft 100.))))

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

(define (js->raw-obs h)
  (raw-obs
   (hash-ref (hash-ref h 'Ceiling) 'Value)
   (hash-ref h 'CloudCover)
   (iso8601->datetime (hash-ref h 'DateTime))
   (hash-ref (hash-ref h 'DewPoint) 'Value)
   (hash-ref h 'HasPrecipitation)
   (hash-ref h 'IconPhrase)
   (hash-ref h 'IsDaylight)
   (hash-ref (hash-ref h 'Rain) 'Value)
   (hash-ref h 'RainProbability)
   (hash-ref (hash-ref h 'RealFeelTemperature) 'Value)
   (hash-ref h 'RelativeHumidity)
   (hash-ref (hash-ref h 'Temperature) 'Value)
   (hash-ref h 'UVIndex)
   (hash-ref h 'UVIndexText)
   (hash-ref (hash-ref h 'Visibility) 'Value)
   (hash-ref h 'WeatherIcon)
   (hash-ref (hash-ref (hash-ref h 'Wind) 'Direction) 'Degrees)
   (hash-ref (hash-ref (hash-ref h 'Wind) 'Direction) 'English)
   (hash-ref (hash-ref (hash-ref h 'Wind) 'Speed) 'Value)
   (hash-ref (hash-ref (hash-ref h 'WindGust) 'Speed) 'Value)))

(define (raw->obs r)
  (obs
   r
   (feet-hundreds (raw-obs-ceil r))
   (sky (raw-obs-cover r))
   (let ([x (raw-obs-dt r)])
     (format "~a ~a/~a ~a00" (day-of-week (->wday x)) (->month x) (->day x) (fmt-i-02d (->hours x))))
   (format "~a°F" (exact-round (raw-obs-dewpoint r)))
   (if (raw-obs-precip? r) "rain" "")
   (raw-obs-wx r)
   (raw-obs-day? r)
   (let ([x (raw-obs-rain-in r)])
     (if (< x 0.001) "" (format "~a in" x)))
   (let ([x (exact-round (raw-obs-prob-rain r))])
     (if (> x 0) (format "~a %" (exact-round (raw-obs-prob-rain r))) ""))
   (format "~a°F"(exact-round (raw-obs-realfeel r)))
   (format "~a %" (exact-round (raw-obs-rh r)))
   (format "~a°F" (exact-round (raw-obs-temp r)))
   (raw-obs-uv r)
   (raw-obs-uvtext r)
   (format "~a mi"(let ([x (raw-obs-viz r)]) (if (< x 3.0) x (exact-round x))))
   (raw-obs-wxicon r)
   (format "~a°" (raw-obs-windir r))
   (raw-obs-wintxt r)
   (let ([x (raw-obs-windspeed r)]) (if (>= x 0.5) (format "~a mph" (exact-round x)) "calm"))
   (let ([x (raw-obs-gust r)]) (if (>= x 0.5) (format "~a mph" (exact-round x)) ""))))

(define (td-class-s class s)
  (list 'td `((class ,class)) s))

(define rxi-showers #rx"(?i:showers)")
(define rxi-thunderstorms #rx"(?i:thunderstorms)")
(define rxi-t-storms #rx"(?i:t-storms)")
(define rx-realfeel #rx"(\\d+).*")

(define align-right "ra")

(define (obs->tr o)
  `(tr
    ,(td-class-s (if (obs-day? o) "day" "night") (obs-dt o))
    (td "")
    ;,(td-class-s (heat-category-class (wbgt-fahrenheit o)) (number->string(wbgt-fahrenheit o)))
    ,(let ([x (obs-wx o)])
       (cond [(regexp-match rxi-showers x)
              (td-class-s "green" x)]
             [(or (regexp-match rxi-thunderstorms x)
                  (regexp-match rxi-t-storms x))
              (td-class-s "red" x)]
             [else (list 'td x)]))
    ,(td-class-s align-right (obs-prob-rain o))
    ,(td-class-s align-right (obs-rain-in o))
    ,(let ([x (obs-precip? o)])
       (if (= 0 (string-length x))
           (list 'td x)
           (td-class-s "green" x)))
    ,(td-class-s align-right (obs-realfeel o))
    ,(td-class-s align-right (obs-temp o))
    ,(td-class-s align-right (obs-dewpoint o))
    ,(td-class-s align-right (obs-rh o))
    ,(td-class-s align-right (obs-windir o))
    (td ,(obs-wintxt o))
    ,(td-class-s align-right (obs-windspeed o))
    ,(td-class-s align-right (obs-gust o))
    ,(td-class-s align-right (obs-viz o))
    ,(td-class-s align-right (obs-cover o))
    ,(td-class-s align-right (obs-ceil o))))

(define jsons-hourly-72 (take hourly-json 72))

(define jsons-grouped-by-day
  (group-by (λ (h) (substring (hash-ref h 'DateTime) 8 10))
            jsons-hourly-72))

(define (detail-table-rows day-jsons)
  (map (λ (json)
         (obs->tr (raw->obs (js->raw-obs json))))
       day-jsons))

(define (gen-hourly-table json)
  (list 'div
        '(p)
        (append `(table (tr (th "DateTime")
                            (th "Heat")
                            (th "Weather")
                            (th "Rain") (th "Amt") (th "Type")
                            (th "Feel") (th "Temp") (th "DP") (th "Hum")
                            (th "Wind") (th "Dir") (th "Speed") (th "Gust")
                            (th "Viz")
                            (th "Sky") (th "Ceil")))
                (detail-table-rows json))))

(define head-title (format "~a ~a hour hourly forecast" location-short-name hours))

(define computer-generated
  (string-append "computer generated as of " (substring (datetime->iso8601 (now)) 0 19)))

(define (gen-page)
  `(html (head (title ,head-title)
               (link ((rel "stylesheet")
                      (href ,css-path)
                      (type "text/css")))
               ,(gen-body))))

(define (gen-body)
  `(body ,(gen-top)
         ,(gen-hourly-tables-divs)))

(define (gen-top)
  `(div ((class "test"))
        (h1 ,(string-append (number->string hours) " hour forecast for " location-long-name))
        (h3 "from AccuWeather API data")
        (p ((class "compgen")) ,computer-generated)))

(define (gen-hourly-tables-divs)
  (let ([hourly-tables
         (append (map (λ (j)
                        (gen-hourly-table j))
                      jsons-grouped-by-day))])
    (append '(div) hourly-tables)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Process Web Request

(define (start req)
  (response/xexpr (gen-page)))

(serve/servlet start
               #:extra-files-paths
               (list
                (build-path site-path)))

