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
  
(struct obs (ceil cover dt dewpoint precip? wx day? rain-in prob-rain realfeel rh temp uv uvtext viz wxicon windir wintxt windspeed gust) #:transparent)
  
(define (sky cover)
  (let ([eighths (/ cover (/ 100 8.))])
  (cond [(<  eighths 1) "SKC"]
        [(<= eighths 2) "FEW"]
        [(<= eighths 4) "SCT"]
        [(<= eighths 7) "BKN"]
        [else "OVC"])))
  
(define (fmt-i-03d n)
  (let ((s (format "~a" n)))
    (cond [(= (string-length s) 2) (string-append "0" s)]
          [(= (string-length s) 1) (string-append "00" s)]
          [else s])))
  
(define (feet-hundreds ft)
  (fmt-i-03d (exact-round (/ ft 100.))))

(define (heat-category wbgt)
  (cond [(and (>= wbgt 78.0) (< wbgt 82.0)) (list 1 #f)]
        [(and (>= wbgt 82.0) (< wbgt 85.0)) (list 2 "green")]
        [(and (>= wbgt 85.0) (< wbgt 88.0)) (list 3 "yellow")]
        [(and (>= wbgt 88.0) (< wbgt 90.0)) (list 4 "red")]
        [(>= wbgt 90.0) (list 5 "black")]
        [else (list 0 #f)]))
  
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
   (feet-hundreds (raw-obs-ceil r))
   (sky (raw-obs-cover r))
   (let ([x (raw-obs-dt r)])
     (format "~a ~a/~a ~a00" (day-of-week (->wday x)) (->month x) (->day x) (fmt-i-02d (->hours x))))
   (format "~a°F" (exact-round (raw-obs-dewpoint r)))
   (if (raw-obs-precip? r) "rain" "")
   (raw-obs-wx r)
   (if (raw-obs-day? r) "day" "night")
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

(define (obs->row o)
  `(tr
    (td ,(obs-dt o))
    (td "")
    ,(let ([x (obs-wx o)])
       (cond [(regexp-match rxi-showers x)
              (td-class-s "green" x)]
             [(or (regexp-match rxi-thunderstorms x)
                  (regexp-match rxi-t-storms x))
              (td-class-s "red" x)]
             [else (list 'td x)]))
    ,(td-class-s "label" (obs-prob-rain o))
    ,(td-class-s "label" (obs-rain-in o))
    ,(let ([x (obs-precip? o)])
      (if (= 0 (string-length x))
          (list 'td x)
          (td-class-s "green" x)))
    ,(td-class-s "label" (obs-realfeel o))
    ,(td-class-s "label" (obs-temp o))
    ,(td-class-s "label" (obs-dewpoint o))
    ,(td-class-s "label" (obs-rh o))
    (td ,(obs-day? o))
    (td ,(obs-windir o))
    (td ,(obs-wintxt o))
    ,(td-class-s "label" (obs-windspeed o))
    ,(td-class-s "label" (obs-gust o))
    ,(td-class-s "label" (obs-viz o))
    (td ,(obs-cover o)) (td ,(obs-ceil o))
    )
  )

(define hourly-json-72 (take hourly-json 72))
(define jsons (group-by (λ (h)
            (let ([dt (hash-ref h 'DateTime)])
              (substring dt 8 10)
              ))
          hourly-json-72))

(define (detail-table-rows day-json)
  (map (λ (x)
         (obs->row (raw->obs (js->raw-obs x))))
       day-json))

(define (gen-hourly-table json)
  (append `(table (tr (th "DateTime")
                      (th "Heat")
                      (th "Weather")
                      (th "Rain") (th "Amt") (th "Type")
                      (th "Feel") (th "Temp") (th "DP") (th "Hum")
                      (th "Light")
                      (th "Wind") (th "Dir") (th "Speed") (th "Gust")
                      (th "Viz")
                      (th "Sky") (th "Ceil")))
          (detail-table-rows json)))

(define head-title (format "GAP ~a hour hourly forecast" hours))

(define computer-generated
  (string-append "computer generated as of " (substring (datetime->iso8601 (now)) 0 19)))

(define (gen-page)
  `(html (head (title ,head-title)
                (link ((rel "stylesheet")
                       (href ,css-path)
                       (type "text/css")))
                ,(gen-body))))

(define (gen-body)
  `(body ,(gen-div)))

(define (gen-div)
  (let ([hourly-tables (append (map (λ (j) (gen-hourly-table j)) jsons))])
     (append
      (list 'div)
      (list '((class "test")))
      (list (list 'h1 `(string-append ,(number->string hours) " hour forecast for Fort Indiantown Gap")))
      (list (list 'h3 "from AccuWeather API data"))
      (list (list 'p '((class "compgen")) computer-generated))
      hourly-tables)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Process Web Request

(define (start req)
  (response/xexpr (gen-page)))

(serve/servlet start
               #:extra-files-paths
               (list
                (build-path site-path)))
