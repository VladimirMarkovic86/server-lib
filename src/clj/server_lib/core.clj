(ns server-lib.core
 (:require [clojure.string :as cstring]
           [utils-lib.core :as utils]
           [ajax-lib.http.status-code :refer [status-code]]
           [ajax-lib.http.entity-header :as eh]
           [ajax-lib.http.response-header :as rsh])
 (:import [java.net ServerSocket]
          [java.util Scanner]))

(def main-thread (atom nil))

(def server-socket (atom nil))

(defn- open-server-socket
 ""
 [port]
 (when (or (nil? @server-socket)
           (and (not (nil? @server-socket))
                (.isClosed @server-socket))
        )
   (reset!
     server-socket
     (ServerSocket. port))
  )
 @server-socket)

(defn stop-server
 ""
 []
 (when (and @server-socket
            @main-thread)
   (try
     (.interrupt @main-thread)
     (.close @server-socket)
     (catch Exception e
      (println (.getMessage e))
      ))
  )
  (println "Server stopped"))

(defn- read-header
 ""
 [header]
 (let [header-vector (cstring/split
                       header
                       #"\r\n")
       request-start-line (get header-vector 0)
       request-start-line-vector (cstring/split
                                   request-start-line
                                   #" ")
       request-method (get request-start-line-vector 0)
       request-uri (get request-start-line-vector 1)
       request-protocol (get request-start-line-vector 2)
       header-vector (utils/remove-index-from-vector
                       header-vector
                       0)
       header-map (atom
                    {:request-method request-method
                     :request-uri request-uri
                     :request-protocol request-protocol})]
   (doseq [header-line header-vector]
     (let [header-line-vector (cstring/split header-line #": " 2)
           key-name (cstring/lower-case (get header-line-vector 0))
           key-value (get header-line-vector 1)]
       (swap!
         header-map
         assoc
         (keyword key-name)
         key-value))
    )
  @header-map))

(defn- pack-response
 ""
 [request
  response-map]
 (let [response (atom "")
       status-line (str
                     (:request-protocol request)
                     " "
                     (status-code (:status response-map))
                     "\r\n")
       headers (:headers response-map)]
   (swap!
     response
     str
     status-line)
   (doseq [map-key (keys headers)]
     (swap!
       response
       str
       map-key
       ": "
       (get headers map-key)
       "\r\n"))
   (when-let [body (:body response-map)]
     (swap!
       response
       str
       (eh/content-length)
       ": "
       (count (.getBytes body "UTF-8"))
       "\r\n\r\n"
       body))
   @response))

(defn- handler-fn
 ""
 [routing-fn
  request
  default-response-headers]
 (let [{request-method :request-method
        request-uri :request-uri
        request-protocol :request-protocol} request
        request-start-line (str
                             request-method
                             " "
                             request-uri)
        response (atom 
                   (routing-fn
                     request-start-line
                     request))
        access-control-allow-origin (get
                                      default-response-headers
                                      (rsh/access-control-allow-origin))
        access-control-allow-origin (if (set? access-control-allow-origin)
                                      (if (contains?
                                            access-control-allow-origin
                                            (:origin request))
                                        (:origin request)
                                        (first access-control-allow-origin))
                                      (when (= access-control-allow-origin
                                               (:origin request))
                                        (:origin request))
                                     )
        default-response-headers (assoc
                                   default-response-headers
                                   (rsh/access-control-allow-origin)
                                   access-control-allow-origin)]
   (swap!
     response
     update-in
     [:headers]
     conj
     default-response-headers)
   (pack-response
     request
     @response)))

(defn- accept-request
 ""
 [routing-fn
  client-socket
  default-response-headers]
 (let [input-stream (.getInputStream client-socket)
       available-bytes (.available input-stream)
       output-stream (.getOutputStream client-socket)
       reader-is (java.io.BufferedReader. (java.io.InputStreamReader. input-stream))
       read-char-array (make-array Character/TYPE available-bytes)
       debug (.read reader-is read-char-array 0 available-bytes)
       request (clojure.string/join "" read-char-array)
       [header
        body] (cstring/split request #"\r\n\r\n")
       header-map (read-header header)
       header-map-with-body (assoc
                              header-map
                              :body
                              body)
       response (handler-fn
                  routing-fn
                  header-map-with-body
                  default-response-headers)]
  (.write
    output-stream
    (.getBytes
      response
      "UTF-8"))
  (.close client-socket)
  )
 )

(defn start-server
 ""
 [routing-fn
  & [default-response-headers
     port]]
 (open-server-socket (or port
                         9000))
 (if (or (nil? @main-thread)
         (and (not (nil? @main-thread))
              (not (.isAlive @main-thread))
          ))
   (let [while-task (fn []
                      (try
                        (while true
                          (let [client-socket (.accept @server-socket)
                                task (fn []
                                       (accept-request
                                         routing-fn
                                         client-socket
                                         default-response-headers))]
                            (.start (Thread. task))
                           ))
                        (catch Exception e
                          (println (.getMessage e))
                         ))
                     )]
     (reset!
       main-thread
       (Thread. while-task))
     (.start
       @main-thread)
     (println "Server started"))
   (println "Server already started"))
  @main-thread)

