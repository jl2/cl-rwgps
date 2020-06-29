;; cl-rwgps.lisp
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

(in-package :cl-rwgps)

(defparameter *api-key-file* (asdf:system-relative-pathname :cl-rwgps ".api-key")
  "Path to a JSON file containing the RideWithGPS API key.")

(defvar *debug-stream* nil
  "Set to non-nil to log to a stream.")

(defvar *user* nil
  "Cached copy of the authenticated user.  Used as default user and stores the auth_token.")

(defvar *user-name* nil
  "The RideWithGPS user to login with.")

(defvar *api-key* nil
  "The RideWithGPS API key.")

(defparameter *rwgps-url* "https://ridewithgps.com"
  "Default RideWithGPS URL, can be changed for testing.")

(defvar *cookies* (cl-cookie:make-cookie-jar)
  "Holds cookies exchanged with server.")


(defvar *password-function*
  (lambda ()
    #+swank(swank:eval-in-emacs '(password-read "RideWithGPS Password? " :rwgps))
    #-swank(with-input-from-file (ins *api-key-file*)
      (setf *api-key*
            (getjso "password" (read-json ins)))))
  "Function to fetch user password.")

;; Inline functions that just call a single other function
(declaim (inline to-bool get-headers api-key user-name auth-token api-url
                 to-id user-id user user-current
                 user-detail user-routes user-rides user-clubs user-search
                 trip-details route-details route-search club-detail club-routes queued-task-status
                 new-ride-points new-ride-file))

(defun to-bool (value)
  "Return string 'true' if value is non-nil, string 'false' otherwise."
  (if value "true" "false"))


(defun api-key ()
  "Return the RideWithGPS API key, reading from the config file if necessary."
  (when (null *api-key*)
   (with-input-from-file (ins *api-key-file*)
      (setf *api-key*
            (getjso "api-key" (read-json ins)))))
  *api-key*)

(defun user-name ()
  "Return the RideWithGPS user name, reading from the config file if necessary."
  (when (null *user-name*)
    (with-input-from-file (ins *api-key-file*)
      (setf *user-name*
            (getjso "user-name" (read-json ins)))))
  *user-name*)

(defun auth-token ()
  "Return the RideWithGPS authentication token, logs in if necessary."
  (when (null *user*)
    (login))
  (getjso* "user.auth_token" *user*))

(defun api-url (api &rest parameters &key &allow-other-keys)
  "Returns the full URL to an API call.  Adds 'https://ridewithgps.com' prefix, '.json' suffix,
   and appends URL encoded parameters."
  (concatenate
   'string
   *rwgps-url* "/" api ".json"
   (when parameters
     (format nil "?~{~a~^&~}"
             (loop for (name  value) on parameters by #'cddr
                when value
                collect (format nil "~a=~a"
                                (quri:url-encode (string-downcase name))
                                (quri:url-encode (format nil "~a" value))))))))

(defun get-headers ()
  "Returns headers used to make API requests."
  '(("content-type" . "application/json")))

(defun get-api (api &rest parameters &key &allow-other-keys)
  "Make a GET request to RideWithGPS and parses the JSON result.
   Calls (login) when authentication fails (401 error)."

  (multiple-value-bind (body status response-headers uri)
      (handler-bind (
                     ;; Try reconnecting on 401 errors
                     (dex:http-request-unauthorized
                      (lambda (&rest params)
                        (declare (ignorable params))
                        (when *debug-stream*
                          (format *debug-stream* "~a - reconnecting...~%" params))
                        (login)
                        (dex:retry-request 1 :interval 0)))

                     ;; Ignore 404 errors because they indicate empty result sets
                     (dex:http-request-not-found
                      #'dex:ignore-and-continue))

        ;; Include auth_token if it's available
        (if *user*
            (dex:get (apply #'api-url api
                            :version 2
                            :apikey (api-key)
                            :auth_token (auth-token)
                            parameters)
                     :headers (get-headers))
            (dex:get (apply #'api-url api
                            :version 2
                            :apikey (api-key)
                            parameters)
                     :headers (get-headers))))
    (values (read-json-from-string body) status response-headers uri)))

(defun post-api (api &rest parameters &key (content) &allow-other-keys)
  "Make a POST request to RideWithGPS and parses the JSON result.
   Calls (login) when authentication fails (401 error).
   Parameters can be passed as a JSON object using :content keyword, or as
   keyword arguments."

  (let* (
         ;; Remove :content from parameters
         (filtered-parameters (remove content (remove :content parameters)))
         ;; If :content was passed into function, use it, otherwise
         ;; create a default JSON object with version and apikey.
         (real-content (cond (content content)
                            (t (jso "version" 2 "apikey" (api-key))))))

    ;; Set version and apikey
    (setf (getjso "version" real-content) 2)
    (setf (getjso "apikey" real-content) (api-key))

    ;; Add auth_token
    (when *user*
      (setf (getjso "auth_token" real-content) (auth-token)))

    ;; Convert keyword parameters into JSON content parameters
    (loop for (name  value) on filtered-parameters by #'cddr
       when value
       do (setf (getjso (quri:url-encode (format nil "~a" name)) real-content)
                (quri:url-encode (format nil "~a" value))))

    (multiple-value-bind (body status response-headers uri)
        (handler-bind (
                       ;; Try reconnecting on 401 errors
                       (dex:http-request-unauthorized
                        (lambda (&rest params)
                          (declare (ignorable params))
                          (when *debug-stream*
                            (format *debug-stream* "~a - reconnecting...~%" params))
                          (login)
                          (dex:retry-request 1 :interval 0)))

                       ;; Ignore 404 errors because they indicate empty result sets
                       (dex:http-request-not-found
                        #'dex:ignore-and-continue))

          (dex:post (api-url api)
                    :content (format nil "~a" real-content)
                    :headers (get-headers)
                    :cookie-jar *cookies*))
      (values (read-json-from-string body) status response-headers uri))))

(defun login ()
  "Authenticate with RideWithGPS.
   Reads user name from *api-key-file* and calls *password-function* to get password.
   Sets *user* special variable used in other functions."
  (multiple-value-bind (json status response-headers uri)
      (get-api "users/current"
               :email (user-name)
               :password (funcall *password-function*))
    (setf *user* json)
    (values *user* status response-headers uri)))

(defun user ()
  "Returns details about the currently authenticated user."
  (user-current))

(defun user-current ()
  "Returns details about the currently authenticated user."
  (when (null *user*)
    (login))
  *user*)


(defun to-id (json-or-number field)
  "Convenience function to convert a JSON object or number into a numeric ID.
   If json-or-number is a number it is returned as-is.
   If json-or-number[field] exists then return json-or-number[field]['id']
   If json-or-number[field] doesn't exist then return json-or-number['id']."

  (etypecase json-or-number
    (jso (if-let ((js (getjso field json-or-number)))
           (getjso "id" js)
           (getjso "id" json-or-number)))
    (integer json-or-number)))

(defun user-id (user)
  "Return the ID of user.
   If user is null, call (login) and then return ID of user."
  (if (null user)
      (to-id (login) "user")
      (to-id user "user")))

(defun user-detail (&key (user *user*))
  "Return details about any user."
  (get-api (format nil "users/~a" (user-id user))))

(defun user-routes (&key (user *user*) (offset 0) (limit 20))
  "Get a list of a user's routes.
   :offset is the zero based index of the first route to return.
   :limit is the maximum number of route to return."

  (get-api (format nil "users/~a/routes" (user-id user))
           :offset offset
           :limit limit))

(defun user-rides (&key (user *user*) (offset 0) (limit 20))
  "Get a list of a user's rides.
   :offset is the zero based index of the first ride to return.
   :limit is the maximum number of rides to return."
  (get-api (format nil "users/~a/trips" (user-id user))
           :offset offset
           :limit limit))


(defun user-clubs (&optional (user *user*))
  "Get details about all clubs a user is in."
  (get-api (format nil "users/~a/clubs" (user-id user))))

(defun user-search (name &key (location) (distance) (offset 0) (limit 20))
  "Search all users. Users are indexed on first, last and display name."
  (post-api "users/search"
            :query name
            :location location
            :distance distance
            :offset offset
            :limit limit))

;; Rides
(defun trip-details (trip)
  "Get details about a trip."
  (get-api (format nil "trips/~a" (to-id trip "ride"))))

(defun route-details (route)
  "Get details about a route."
  (get-api (format nil "routes/~a" (to-id route "route"))))

(defun route-search (keywords &key
                                start-location
                                start-distance
                                elevation-min
                                elevation-max
                                length-min
                                length-max
                                (sort-by "length asc")
                                (offset 0)
                                (limit 20))

  "Search for routes, rides, and segments."
  (post-api "find/search"
            :keywords keywords
            :start_location start-location
            :start_distance start-distance
            :elevation_min elevation-min
            :elevation_max elevation-max
            :length_min length-min
            :length_max length-max
            :sort_by sort-by
            :offset offset
            :limit limit))

(defun club-detail (club)
  "Get details about a particular club."
  (get-api (format nil "clubs/~a" (to-id club "club"))))

(defun club-routes (club &key (offset 0) (limit 20))
  "Get details about all of a particular club's routes.
   :offset is the zero based index of the first clube to return.
   :limit is the maximum number of clubs to return."
  (get-api (format nil "clubs/~a/routes" (to-id club "club"))
           :offset offset
           :limit limit))

(defun queued-task-status (jobs &key (include-objects nil))
  "After uploading a file (only file, not raw points) and getting a queued task id back,
   use this end point to poll for the status of the queued job.
   Status is -1 for failed, 0 for pending, 1 for success.
   If include-objects is non-nil then also return the route or ride."
  (get-api "queued_tasks/status"
           :ids (format nil "~{~a~^,~}" (mapcar (lambda (job) (etypecase job
                                                                (integer job)
                                                                (jso (getjso "task_id" job))))
                                                (ensure-list jobs)))
           :include_objects (to-bool include-objects)))


(defclass point ()
  ((longitude :initarg :longitude)
   (latitude :initarg :latitude)
   (elevation :initarg :elevation)
   (time :initarg :time)
   (cadence :initarg :cadence)
   (heartrate :initarg :heartrate)
   (power :initarg :power)
   (speed :initarg :speed-m/s)
   (distance :initarg :distance)
   (temp-celsius :initarg :temp-celsius))
  (:documentation "A point used by new-ride-points"))

(defmethod print-object ((pt point) stream)
  "Print a point object in a format usable by the new-rid-points API."
  (with-slots (longitude latitude elevation) pt
    (format stream "{x: ~a,y:~a,e:~a" longitude latitude elevation))
  (loop for (string slot) on '(("t" . 'time)
                               ("c" . 'cadence)
                               ("h" . 'heartrate)
                               ("p" . 'power)
                               ("s" . 'speed)
                               ("d" . 'distance)
                               ("T" . 'temp-celsius))
     do
       (when (slot-boundp pt slot)
         (format stream ",~a:~a" string (slot-value pt slot))))
  (format stream "}"))

(defun create-point (longitude latitude &optional (elevation 0.0) )
  "Create a new point for use with new-ride-points."
  (make-instance 'point
                 :longitude longitude
                 :latitude latitude
                 :elevation elevation))

(defun new-ride-points (name track-points &key (description) (bad-elevations))
  "Create a new ride from raw point objects."
  (post-api "trips"
            :name name
            :track_points track-points
            :bad_elevations (to-bool bad-elevations)
            :description (if description description "")))

(defun file-post (api content)
  "Used to post a file to the trips.json API end-point."
  (let ((real-content (copy-list content)))
    (push (cons "version" 2) real-content)
    (push (cons "apikey" (api-key)) real-content)
    (push (cons "auth_token" (auth-token)) real-content)
    (multiple-value-bind (body status response-headers uri)
        (handler-bind (
                       ;; Try reconnecting on 401 errors
                       (dex:http-request-unauthorized
                        (lambda (&rest params)
                          (declare (ignorable params))
                          (when *debug-stream*
                            (format *debug-stream* "~a - reconnecting...~%" params))
                          (login)
                          (dex:retry-request 1 :interval 0)))

                       ;; Ignore 404 errors because they indicate empty result sets
                       (dex:http-request-not-found
                        #'dex:ignore-and-continue))

          (dex:post (api-url api)
                    :content real-content
                    :cookie-jar *cookies*))
      (values (read-json-from-string body) status response-headers uri))))

(defun new-ride-file (name file-name &key (description) (bad-elevations))
  "Low level function to create a new ride from an uploaded TCX, GPX, FIT or KML file. Due to the potential time it takes to parse a file, this end point is non-blocking and queues up a background job to process the file. As a result, you will need to poll a second endpoint in order to get the actual trip information. Usually jobs are processed within ten seconds, though occasionally they can take a couple minutes depending on server load. As a result, please implement some sort of exponential back-off to reduce the number of requests.
  The convenience function (upload-ride) is better in most situations, as it implements exponential back-off."
  (file-post "trips"
             (list (cons "name" name)
                   (cons "file" (pathname file-name))

                   (cons "bad_elevations" (to-bool bad-elevations))
                   (cons "description" (if description description "")))))

(defun upload-ride (name file-name &key (description) (bad-elevations))
  "Upload a ride and wait for the resulting object to be usable.
   Uses (new-ride-file) to upload and queued-task-status with exponential back-off to wait for results."
  (let ((background-job (new-ride-file name file-name
                                       :description description
                                       :bad-elevations bad-elevations)))
    (when *debug-stream* (format *debug-stream* "background-job: ~a~%" background-job))
    (flet ((check-task ()
             (when *debug-stream* (format *debug-stream* "Checking on ~a~%" background-job))
       (let* ((all-tasks (getjso "queued_tasks" (rwgps:queued-task-status background-job :include-objects t))))
         (when *debug-stream* (format *debug-stream* "Tasks: ~a~%" all-tasks))
         (loop for task in all-tasks
            do (when *debug-stream* (format *debug-stream* "Checking on ~a~%" task))
            (when (and (= (getjso "id" task) (getjso "task_id" background-job))
                       (= (getjso "status" task) 1))
              (return-from check-task task))))))
      (j-utils:timed-loop 600 2 #'check-task))))
