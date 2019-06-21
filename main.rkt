#lang racket

(require net/url
         json
         gregor
         format-numbers
         web-server/servlet
         web-server/servlet-env
         "calculations.rkt"
         "structs.rkt"
         "misc.rkt"
         (file "~/.location-weather-prefs.rkt"))

(define hours 72)

(define s-hourly-url
  (string-append
   "http://apidev.accuweather.com/forecasts/v1/hourly/72hour/"
   location-key "?apikey=" accuwx-api-key "&details=true"))

(define (get-hourly-forecast)
  (call/input-url (string->url s-hourly-url) get-pure-port read-json))
  
(define hourly-json (get-hourly-forecast))

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

