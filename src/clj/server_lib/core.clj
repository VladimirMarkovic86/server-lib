(ns server-lib.core
  (:require [clojure.string :as cstring]
            [utils-lib.core :as utils]
            [ajax-lib.http.status-code :as stc
                                       :refer [status-code]]
            [ajax-lib.http.general-header :as gh]
            [ajax-lib.http.entity-header :as eh]
            [ajax-lib.http.mime-type :as mt]
            [ajax-lib.http.response-header :as rsh]
            [clojure.java.io :as io])
  (:import [server_lib RejectedExecutionHandlerHTTPResponse]
           [java.net ServerSocket]
           [javax.net.ssl SSLServerSocket
                          KeyManagerFactory
                          SSLContext]
           [java.security KeyStore]
           [java.util.concurrent Executors]))

(def main-thread
     (atom nil))

(def server-socket
     (atom nil))

(def running
     (atom false))

(def thread-pool-size 4)

(def thread-pool
     (atom nil))

(def client-sockets
     (atom #{}))

(def public-dir-a
     (atom "public"))

(def port-a
     (atom 9000))

(defn- open-server-socket
  "Open server socket on local machine"
  [port
   & [{keystore-file-path :keystore-file-path
       keystore-type :keystore-type
       keystore-password :keystore-password
       ssl-context :ssl-context}]]
  (when (or (nil? @server-socket)
            (and (not (nil? @server-socket))
                 (.isClosed @server-socket))
         )
    (when (and keystore-file-path
               keystore-password)
      (try
        (let [ks (KeyStore/getInstance
                   (or keystore-type
                       "JKS"))
              ks-is (io/input-stream
                      (io/resource
                        keystore-file-path))
              pass-char-array (char-array
                                keystore-password)
              void (.load
                     ks
                     ks-is
                     pass-char-array)
              kmf (KeyManagerFactory/getInstance
                    (KeyManagerFactory/getDefaultAlgorithm))
              sc (SSLContext/getInstance
                   (or ssl-context
                       "TLSv1.2"))
              void (.init
                     kmf
                     ks
                     pass-char-array)
              void (.init
                     sc
                     (.getKeyManagers
                       kmf)
                     nil
                     nil)
              ssl-server-socket (.createServerSocket
                                  (.getServerSocketFactory
                                    sc)
                                  port)]
          (.setEnabledProtocols
            ssl-server-socket
            (into-array
              ["TLSv1"
               "TLSv1.1"
               "TLSv1.2"
               "SSLv3"]))
          (reset!
            server-socket
            ssl-server-socket))
        (catch Exception e
          (println (.getMessage
                     e))
          ))
     )
    (when-not (and keystore-file-path
                   keystore-password)
      (reset!
        server-socket
        (ServerSocket.
          port))
     ))
  @server-socket)

(defn stop-server
  "Stop server"
  []
  (when (and @server-socket
             @main-thread
             @running)
    (try
      (reset!
        running
        false)
      (future-cancel
        @main-thread)
      (doseq [client-socket @client-sockets]
        (try
          (.close
            client-socket)
          (catch Exception e
            (println (.getMessage e))
           ))
       )
      (swap!
        client-sockets
        empty)
      (.shutdownNow
        @thread-pool)
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
                                    #" "
                                    3)
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
        response-body (atom nil)
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
    (doseq [[m-key
             m-val] headers]
      (swap!
        response
        str
        m-key
        ": "
        m-val
        "\r\n"))
    (when-let [body (:body response-map)]
      (let [body (if (and (= (get headers (eh/content-type))
                             (mt/text-plain))
                          (not
                            (bytes?
                              body))
                      )
                   (.getBytes
                     body
                     "UTF-8")
                   body)]
        (swap!
          response
          str
          (eh/content-length)
          ": "
          (count
            body)
          "\r\n\r\n")
        (reset!
          response-body
          body))
     )
    [@response
     @response-body]))

(defn- read-file
  "Read file and recognize it's mime type"
  [file-path
   extension]
  (try
    (let [is (io/input-stream
               (io/resource
                 (str
                   @public-dir-a
                   file-path))
              )
          available-bytes (.available
                            is)
          ary (byte-array
                available-bytes)
          body (atom nil)
          headers (atom nil)
          mime-type (atom (mt/text-plain))]
      (when (= extension
               "css")
        (reset!
          mime-type
          (mt/text-css))
       )
      (when (contains?
              #{"js"
                "map"
                "cljs"}
              extension)
        (reset!
          mime-type
          (mt/text-javascript))
       )
      (when (= extension
               "html")
        (reset!
          mime-type
          (mt/text-html))
       )
      (.read
        is
        ary)
      (.close
        is)
      (reset!
        headers
        {(eh/content-type) @mime-type})
      (reset!
        body
        ary)
      {:status (stc/ok)
       :headers @headers
       :body @body})
    (catch Exception e
      (println (.getMessage e))
      {:status (stc/internal-server-error)
       :headers {(eh/content-type) (mt/text-plain)}
       :body (str {:status "error"
                   :error-message (.getMessage e)})}
     ))
 )

(defn- default-routing-fn
  "Default routing function for reading files recognized by request uri"
  [request
   response]
  (let [request-uri (:request-uri request)
        request-uri (if (= request-uri
                           "/")
                      "/index.html"
                      request-uri)
        extension-start (clojure.string/last-index-of
                          request-uri
                          ".")
        extension (when extension-start
                    (.substring
                      request-uri
                      (inc extension-start)
                      (count request-uri))
                   )]
    (when extension
      (let [response-map (read-file
                           request-uri
                           extension)
            response-map (update-in
                           response-map
                           [:headers]
                           conj
                           (:headers
                             @response))]
        (reset!
          response
          response-map))
     ))
 )

(defn- add-default-response-headers
  "Add default headers in every response"
  [response]
  (swap!
    response
    assoc-in
    [:headers
     (gh/connection)]
    "keep-alive")
  (swap!
    response
    assoc-in
    [:headers
     (gh/date)]
    (let [sdf (java.text.SimpleDateFormat.
                "E, dd MMM yyyy HH:mm:ss zzz")
          date (java.util.Date.)]
      (.setTimeZone
        sdf
        (java.util.TimeZone/getTimeZone
          "GMT"))
      (.format
        sdf
        date))
   )
  (swap!
    response
    assoc-in
    [:headers
     (rsh/server)]
    "cljserver"))

(defn- cors-check
  "Check is access allowed"
  [request
   response
   default-response-headers]
  (when default-response-headers
    (let [access-control-allow-origin (get
                                        default-response-headers
                                        (rsh/access-control-allow-origin))
          access-control-allow-origin (if (set? access-control-allow-origin)
                                        (when (contains?
                                                access-control-allow-origin
                                                (:origin request))
                                          (:origin request))
                                        (when (= access-control-allow-origin
                                                 (:origin request))
                                          (:origin request))
                                       )
          default-response-headers (assoc
                                     default-response-headers
                                     (rsh/access-control-allow-origin)
                                     access-control-allow-origin)]
      (if access-control-allow-origin
        (swap!
          response
          assoc
          :headers
          default-response-headers)
        (reset!
          response
          {:status (stc/forbidden)
           :headers {(eh/content-type) (mt/text-plain)}
           :body (str {:status "Error"
                       :message "Forbidden"})})
       ))
   ))

(defn- handler-fn
  "Handle request by passing request to routing-fn function
   routing-fn - is function implemented in applications server side
   request - is XMLHTTPRequest string converted into clojure map
   default-response-headers - map of default response headers defined when starting server"
  [routing-fn
   request
   default-response-headers
   reject]
  (if reject
    (pack-response
      request
      {:status (stc/not-found)
       :headers {(eh/content-type) (mt/text-plain)}
       :body (str {:status "Error"
                   :message "Try again later"})})
    (let [response (atom nil)]
      (cors-check
        request
        response
        default-response-headers)
      (when-not (:status
                  @response)
        (default-routing-fn
          request
          response))
      (when-not (:status
                  @response)
        (let [response-map (routing-fn
                             request)
              response-map (update-in
                             response-map
                             [:headers]
                             conj
                             (:headers
                               @response))]
          (reset!
            response
            response-map))
       )
      (add-default-response-headers
        response)
      (pack-response
        request
        @response))
   ))

(defn- accept-request
  "Reads input stream from client socket
   converts request string into clojure map
   and passes it to routing-fn function for
   other processing"
  [routing-fn
   client-socket
   default-response-headers
   reject]
  (try
    (let [input-stream (.getInputStream
                         client-socket)
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
          read-stream (when-let [content-length content-length]
                        (doseq [itr (range content-length)]
                          (let [read-byte (unchecked-byte
                                            (.read
                                              input-stream))]
                            (swap!
                              body-bytes
                              conj
                              read-byte))
                         ))
          request (if-not (empty? @body-bytes)
                    (let [body (if (= (:content-type header-map)
                                      (mt/text-plain))
                                 (String.
                                   (byte-array
                                     @body-bytes)
                                   "UTF-8")
                                 @body-bytes)]
                      ;(println body)
                      (assoc
                        header-map
                        :body
                        body))
                    header-map)
          [response-header
           response-body] (handler-fn
                            routing-fn
                            request
                            default-response-headers
                            reject)]
      ;(println response)
      (.write
        output-stream
        (.getBytes
          response-header
          "UTF-8"))
      (.write
        output-stream
        response-body))
    (catch Exception e
      (println (.getMessage e))
     )
    (finally
      (.close
        client-socket)
      (swap!
        client-sockets
        disj
        client-socket))
   ))

(defn- while-loop
  "While loop of accepting and responding on clients requests"
  [routing-fn
   & [default-response-headers]]
  (try
    (while @running
      (let [client-socket (.accept
                            @server-socket)]
        (swap!
          client-sockets
          conj
          client-socket)
        (.execute
          @thread-pool
          (fn [& [reject]]
            (accept-request
              routing-fn
              client-socket
              default-response-headers
              reject))
         ))
     )
    (catch Exception e
      (println (.getMessage e))
     ))
 )

(defn start-server
  "Starts listening on opened server socket"
  [routing-fn
   & [default-response-headers
      port
      https-conf
      public-dir]]
  (when public-dir
    (reset!
      public-dir-a
      public-dir))
  (when port
    (reset!
      port-a
      port))
  (try
    (open-server-socket
      @port-a
      https-conf)
    (if (or (nil? @main-thread)
            (and (not (nil? @main-thread))
                 (future-cancelled?
                   @main-thread)
                 (future-done?
                   @main-thread))
         )
      (do
        (reset!
          running
          true)
        (reset!
          thread-pool
          (java.util.concurrent.ThreadPoolExecutor.
            thread-pool-size
            thread-pool-size
            thread-pool-size
            java.util.concurrent.TimeUnit/SECONDS
            (java.util.concurrent.ArrayBlockingQueue.
              thread-pool-size))
         )
        (.setRejectedExecutionHandler
          @thread-pool
          (RejectedExecutionHandlerHTTPResponse.))
        (reset!
          main-thread
          (future
            (while-loop
              routing-fn
              default-response-headers))
         )
        (println "Server started")
        @main-thread)
      (println "Server already started"))
    (catch Exception e
      (println (.getMessage e))
     ))
 )

