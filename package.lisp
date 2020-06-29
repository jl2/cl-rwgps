;; package.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(defpackage :cl-rwgps
  (:use #:cl #:j-utils #:alexandria #:st-json)
  (:nicknames #:rwgps)
  (:export

   ;; Login and authentication
   #:api-key
   #:user-name
   #:login

   ;; Debug logging stream
   #:*debug-stream*

   ;; Function to query user's password
   #:*password-function*

   ;; Direct API access
   #:get-api
   #:post-api

   ;; Export JSON query function
   #:getjso

   ;; User queries
   #:user
   #:user-current
   #:user-detail
   #:user-search

   ;; Route queries
   #:user-routes
   #:route-details
   #:route-search

   ;; Ride queries
   #:user-rides
   #:trip-details

   ;; Club queries
   #:user-clubs
   #:club-detail
   #:club-routes

   ;; Queued task queries
   #:queued-task-status

   ;; Ride creation commands
   #:new-ride-points
   #:new-ride-file
   #:upload-ride

   ))