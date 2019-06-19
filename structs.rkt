#lang racket

(provide (all-defined-out))

(struct raw-obs (ceil cover dt dewpoint precip? wx day? rain-in prob-rain realfeel rh temp uv uvtext viz wxicon windir wintxt windspeed gust) #:transparent)
  
(struct obs (raw ceil cover dt dewpoint precip? wx day? rain-in prob-rain realfeel rh temp uv uvtext viz wxicon windir wintxt windspeed gust) #:transparent)

