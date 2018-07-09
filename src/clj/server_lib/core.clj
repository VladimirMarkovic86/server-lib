(ns server-lib.core
  (:require [clojure.string :as cstring]
            [utils-lib.core :as utils]
            [ajax-lib.http.status-code :refer [status-code]]
            [ajax-lib.http.entity-header :as eh]
            [ajax-lib.http.mime-type :as mt]
            [ajax-lib.http.response-header :as rsh])
  (:import [java.net ServerSocket]))

(def main-thread
     (atom nil))

(def server-socket
     (atom nil))

(defn- open-server-socket
  "Open server socket on local machine"
  [port]
  (when (or (nil? @server-socket)
            (and (not (nil? @server-socket))
                 (.isClosed @server-socket))
         )
    (reset!
      server-socket
      (ServerSocket.
        port))
   )
  @server-socket)

(defn stop-server
 "Stop server"
 []
 (when (and @server-socket
            @main-thread)
   (try
     (.interrupt
       @main-thread)
     (.close
       @server-socket)
     (catch Exception e
       (println (.getMessage e))
      ))
  )
  (println "Server stopped"))

(defn- read-header
  "Read header of XMLHTTPRequest"
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
          (keyword
            key-name)
          key-value))
     )
    @header-map))

(defn- pack-response
  "Format response as string out of clojure's response-map"
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
  "Handle request by passing request to routing-fn function
   routing-fn - is function implemented in applications server side
   request - is XMLHTTPRequest string converted into clojure map
   default-response-headers - map of default response headers defined when starting server"
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
         default-response-headers (if access-control-allow-origin
                                    (assoc
                                      default-response-headers
                                      (rsh/access-control-allow-origin)
                                      access-control-allow-origin)
                                    default-response-headers)]
    (swap!
      response
      update-in
      [:headers]
      conj
      default-response-headers)
    (pack-response
      request
      @response))
 )

(defn- accept-request
  "Reads input stream from client socket
   converts request string into clojure map
   and passes it to routing-fn function for
   other processing"
  [routing-fn
   client-socket
   default-response-headers]
  (let [input-stream (.getInputStream
                       client-socket)
        available-bytes (.available
                          input-stream)
        output-stream (.getOutputStream
                        client-socket)
        body-separator [13 10 13 10]
        last-four-bytes (atom [0 0 0 0])
        header (atom [])
        read-stream (while (not= body-separator
                                 @last-four-bytes)
                      (let [read-byte (unchecked-byte
                                        (.read
                                          input-stream))]
                        (swap!
                          header
                          conj
                          read-byte)
                        (swap!
                          last-four-bytes
                          utils/remove-index-from-vector
                          0)
                        (swap!
                          last-four-bytes
                          conj
                          read-byte))
                     )
        remove-last-r-n (swap!
                          header
                          utils/remove-index-from-vector
                          (into
                            []
                            (range
                              (- (count @header)
                                 4)
                              (count @header))
                           ))
        header-string (String.
                        (byte-array
                          @header)
                        "UTF-8")
        header-map (read-header
                     header-string)
        content-length (when-let [content-length (:content-length header-map)]
                         (read-string content-length))
        body-bytes (atom [])
        read-stream (doseq [itr (range content-length)]
                      (let [read-byte (unchecked-byte
                                        (.read
                                          input-stream))]
                        (swap!
                          body-bytes
                          conj
                          read-byte))
                     )
        request (if-not (empty? @body-bytes)
                  (let [body (if (= (:content-type header-map)
                                    (mt/text-plain))
                               (String.
                                 (byte-array
                                   @body-bytes)
                                 "UTF-8")
                               @body-bytes)]
                    (assoc
                      header-map
                      :body
                      body))
                  header-map)
        response (handler-fn
                   routing-fn
                   request
                   default-response-headers)]
   ;(println response)
   (.write
     output-stream
     (.getBytes
       response
       "UTF-8"))
   (.close
     client-socket))
 )

(defn start-server
  "Starts listening on opeened server socket"
  [routing-fn
   & [default-response-headers
      port]]
  (open-server-socket
    (or port
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
                             (.start
                               (Thread. task))
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

