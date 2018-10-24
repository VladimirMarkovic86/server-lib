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

(def thread-pool-size
     (atom 4))

(def thread-pool
     (atom nil))

(def client-sockets
     (atom #{}))

(def public-dir-a
     (atom "public"))

(def port-a
     (atom 9000))

(def keep-alive-message-period 25)

(defn pow
  "Square of value"
  [value]
  (Math/pow
    2
    value))

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
          (println
            (.getMessage
              e))
          #_(.printStackTrace
            e))
       ))
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

(defn decode-message
  "Decode message sent through websocket
   
   https://tools.ietf.org/html/rfc6455#section-5.2"
  [encoded-bytes
   key-array]
  (let [message (atom [])]
    (doseq [itr (range
                  (count encoded-bytes))]
      (swap!
        message
        conj
        (bit-xor
          (get
            encoded-bytes
            itr)
          (get
            key-array
            (bit-and
              itr
              0x3))
         ))
     )
    @message))

(defn encode-message
  "Encoding message to send it through websocket
  
   https://tools.ietf.org/html/rfc6455#section-5.2"
  [message
   & [first-byte
      user-agent]]
  (let [first-byte (or first-byte
                       -127)
        key-el-one (unchecked-byte
                     (int
                       (* (Math/random)
                          255))
                    )
        key-el-two (unchecked-byte
                     (int
                       (* (Math/random)
                          255))
                    )
        key-el-three (unchecked-byte
                       (int
                         (* (Math/random)
                            255))
                      )
        key-el-four (unchecked-byte
                      (int
                        (* (Math/random)
                           255))
                     )
        key-array (if user-agent
                    []
                    [key-el-one
                     key-el-two
                     key-el-three
                     key-el-four])
        message-bytes (if (= first-byte
                             -120)
                        (.getBytes
                          (str
                            "  "
                            message)
                          "UTF-8")
                        (.getBytes
                          message
                          "UTF-8"))
        encoded-message (if user-agent
                          (atom message-bytes)
                          (atom []))]
    (when-not user-agent
      (doseq [itr (range
                    (count
                      message-bytes))]
        (swap!
          encoded-message
          conj
          (bit-xor
            (get
              message-bytes
              itr)
            (get
              key-array
              (bit-and
                itr
                0x3))
           ))
       ))
    (let [message-count (count @encoded-message)
          ;debug (println message-count)
          length-bytes (cond
                         (< (dec (pow 7))
                            message-count
                            (pow 16))
                           (let [third-byte-count (/ message-count
                                                     (pow 8))
                                 third-byte (if (< third-byte-count
                                                   1)
                                               0
                                               (unchecked-byte
                                                 third-byte-count))
                                 message-count (- message-count
                                                  (* third-byte
                                                     (pow 8))
                                                )
                                 fourth-byte-count (/ message-count
                                                      (pow 0))
                                 fourth-byte (if (< fourth-byte-count
                                                    1)
                                              0
                                              (unchecked-byte
                                                fourth-byte-count))
                                 message-count (- message-count
                                                  (* fourth-byte
                                                     (pow 0))
                                                )]
                             ;(println third-byte)
                             ;(println fourth-byte)
                             [(if user-agent
                                126
                                (- 126
                                   128))
                              third-byte
                              fourth-byte])
                         (< (dec (pow 16))
                            message-count)
                           (let [third-byte-count (/ message-count
                                                     (pow 56))
                                 third-byte (if (< third-byte-count
                                                   1)
                                              0
                                              (unchecked-byte
                                                third-byte-count))
                                 message-count (- message-count
                                                  (* third-byte
                                                     (pow 56))
                                                )
                                 fourth-byte-count (/ message-count
                                                      (pow 48))
                                 fourth-byte (if (< fourth-byte-count
                                                    1)
                                               0
                                               (unchecked-byte
                                                 fourth-byte-count))
                                 message-count (- message-count
                                                  (* fourth-byte
                                                     (pow 48))
                                                )
                                 fifth-byte-count (/ message-count
                                                     (pow 40))
                                 fifth-byte (if (< fifth-byte-count
                                                   1)
                                              0
                                              (unchecked-byte
                                                fifth-byte-count))
                                 message-count (- message-count
                                                  (* fifth-byte
                                                     (pow 40))
                                                )
                                 sixth-byte-count (/ message-count
                                                     (pow 32))
                                 sixth-byte (if (< sixth-byte-count
                                                   1)
                                              0
                                              (unchecked-byte
                                                sixth-byte-count))
                                 message-count (- message-count
                                                  (* sixth-byte
                                                     (pow 32))
                                                )
                                 seventh-byte-count (/ message-count
                                                       (pow 24))
                                 seventh-byte (if (< seventh-byte-count
                                                     1)
                                                0
                                                (unchecked-byte
                                                  seventh-byte-count))
                                 message-count (- message-count
                                                  (* seventh-byte
                                                     (pow 24))
                                                )
                                 eighth-byte-count (/ message-count
                                                      (pow 16))
                                 eighth-byte (if (< eighth-byte-count
                                                    1)
                                              0
                                              (unchecked-byte
                                                eighth-byte-count))
                                 message-count (- message-count
                                                  (* eighth-byte
                                                     (pow 16))
                                                )
                                 nineth-byte-count (/ message-count
                                                      (pow 8))
                                 nineth-byte (if (< nineth-byte-count
                                                    1)
                                               0
                                               (unchecked-byte
                                                 nineth-byte-count))
                                 message-count (- message-count
                                                  (* nineth-byte
                                                     (pow 8))
                                                )
                                 tenth-byte-count (/ message-count
                                                     (pow 0))
                                 tenth-byte (if (< tenth-byte-count
                                                   1)
                                              0
                                              (unchecked-byte
                                                tenth-byte-count))
                                 message-count (- message-count
                                                  (* tenth-byte
                                                     (pow 0))
                                                )]
                             ;(println message-count)
                             ;(println third-byte)
                             ;(println fourth-byte)
                             ;(println fifth-byte)
                             ;(println sixth-byte)
                             ;(println seventh-byte)
                             ;(println eighth-byte)
                             ;(println nineth-byte)
                             ;(println tenth-byte)
                             [(if user-agent
                                127
                                (- 127
                                   128))
                              third-byte
                              fourth-byte
                              fifth-byte
                              sixth-byte
                              seventh-byte
                              eighth-byte
                              nineth-byte
                              tenth-byte])
                         :else
                         [(if user-agent
                            message-count
                            (- message-count
                               128))]
                        )
          message-array (byte-array
                          (apply
                            conj
                            (apply
                              conj
                              (apply
                                conj
                                [first-byte]
                                length-bytes)
                              key-array)
                            @encoded-message))]
      message-array))
 )

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
    (if (and (= (:upgrade request)
                "websocket"))
      (let [sec-websocket-key (:sec-websocket-key request)
            sec-websocket-accept (javax.xml.bind.DatatypeConverter/printBase64Binary
                                   (.digest
                                     (java.security.MessageDigest/getInstance
                                       "SHA-1")
                                     (.getBytes
                                       (str
                                         sec-websocket-key
                                         "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")
                                       "UTF-8"))
                                  )
          user-agent (:user-agent request)
          connect-message (encode-message
                            "Здраво"
                            nil
                            user-agent)]
        (pack-response
          request
          {:status 101
           :headers {"Connection" "Upgrade"
                     "Upgrade" "websocket"
                     "Sec-WebSocket-Accept" sec-websocket-accept}
           :body connect-message}))
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
                response-map (if (:headers
                                    response-map)
                               (update-in
                                 response-map
                                 [:headers]
                                 conj
                                 (:headers
                                   @response))
                               (assoc-in
                                 response-map
                                 [:headers]
                                 (:headers
                                   @response))
                               )]
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
 )

(defn- calculate-message-length
  "Calculate message length from first bytes
  
   https://tools.ietf.org/html/rfc6455#section-5.2"
  [second-byte
   input-stream
   user-agent]
  (case (int
          (+ second-byte
             (pow 7))
         )
    126 (let [third-byte (* (.read
                              input-stream)
                            (pow 8))
              fourth-byte (.read
                            input-stream)]
          ;(println third-byte)
          ;(println fourth-byte)
          (+ third-byte
             fourth-byte))
    127 (let [third-byte (* (.read
                              input-stream)
                            (pow 56))
              fourth-byte (* (.read
                               input-stream)
                             (pow 48))
              fifth-byte (* (.read
                              input-stream)
                            (pow 40))
              sixth-byte (* (.read
                              input-stream)
                            (pow 32))
              seventh-byte (* (.read
                                input-stream)
                              (pow 24))
              eighth-byte (* (.read
                               input-stream)
                             (pow 16))
              nineth-byte (* (.read
                               input-stream)
                             (pow 8))
              tenth-byte (.read
                           input-stream)]
          ;(println third-byte)
          ;(println fourth-byte)
          ;(println fifth-byte)
          ;(println sixth-byte)
          ;(println seventh-byte)
          ;(println eighth-byte)
          ;(println nineth-byte)
          ;(println tenth-byte)
          (+ third-byte
             fourth-byte
             fifth-byte
             sixth-byte
             seventh-byte
             eighth-byte
             nineth-byte
             tenth-byte))
    (int
      (+ second-byte
         (pow 7))
     ))
 )

(defn read-till-fin-is-one
  "Read from input stream all messages till FIN bit has value 1 (one)"
  [input-stream
   user-agent]
  (let [message (atom [])
        fin-atom (atom 0)]
    (while (not= @fin-atom
                 1)
      (let [w-first-byte (.read
                           input-stream)
            first-byte-binary (Long/toBinaryString
                                w-first-byte)
            first-byte-binary (let [first-byte-count (- 8
                                                        (count first-byte-binary))
                                    first-byte-atom (atom first-byte-binary)]
                                (doseq [itr (range first-byte-count)]
                                  (swap!
                                    first-byte-atom
                                    str
                                    0))
                                @first-byte-atom)
            fin (.charAt
                  first-byte-binary
                  7)
            rsv1 (.charAt
                   first-byte-binary
                   6)
            rsv2 (.charAt
                   first-byte-binary
                   5)
            rsv3 (.charAt
                   first-byte-binary
                   4)
            opcode3 (if (= (.charAt
                             first-byte-binary
                             3)
                           \1)
                      (pow 3)
                      0)
            opcode2 (if (= (.charAt
                             first-byte-binary
                             2)
                           \1)
                      (pow 2)
                      0)
            opcode1 (if (= (.charAt
                             first-byte-binary
                             1)
                           \1)
                      (pow 1)
                      0)
            opcode0 (if (= (.charAt
                             first-byte-binary
                             0)
                           \1)
                      (pow 0)
                      0)
            opcode (int
                     (+ opcode3
                        opcode2
                        opcode1
                        opcode0))
            second-byte (unchecked-byte
                          (.read
                            input-stream))
            message-length (calculate-message-length
                             second-byte
                             input-stream
                             user-agent)
            key-vector [(unchecked-byte
                          (.read
                            input-stream))
                        (unchecked-byte
                          (.read
                            input-stream))
                        (unchecked-byte
                          (.read
                            input-stream))
                        (unchecked-byte
                          (.read
                            input-stream))]
            encoded-vector (atom [])]
        ;(println (str key-vector))
        ;(println "FIN" fin)
        ;(println "RSV1" rsv1)
        ;(println "RSV2" rsv2)
        ;(println "RSV3" rsv3)
        ;(println "OPCODE" opcode)
        ;(println "Payload len" message-length)
        (if user-agent
          (swap!
            fin-atom
            +
            (if (or (= opcode
                       0)
                    (= opcode
                       1))
              (* opcode
                 1/2)
              1))
          (reset!
            fin-atom
            (read-string
              (str fin))
           ))
        (doseq [itr (range message-length)]
          (let [read-byte (.read
                            input-stream)]
            (swap!
              encoded-vector
              conj
              (unchecked-byte
                read-byte))
           ))
        (swap!
          message
          (fn [atom-value
               param-value]
            (apply
              conj
              atom-value
              param-value))
          (decode-message
            @encoded-vector
            key-vector))
       ))
   (String.
     (byte-array
       @message)
     "UTF-8"))
 )

(defn accept-web-socket-request-subprocess
  "Work with established websocket connection"
  [routing-fn
   client-socket
   header-map-with-body]
  (try
    (let [sub-running (atom true)
          input-stream (.getInputStream
                         client-socket)
          output-stream (.getOutputStream
                          client-socket)
          user-agent (:user-agent header-map-with-body)
          user-agent (clojure.string/index-of
                       user-agent
                       "Chrome")
          keep-alive (atom true)
          keep-alive-time-in-seconds (atom keep-alive-message-period)
          keep-alive-thread
           (future
             (while @keep-alive
               (while (< 0
                         @keep-alive-time-in-seconds)
                 (Thread/sleep 1000)
                 (swap!
                   keep-alive-time-in-seconds
                   dec))
               (.write
                 output-stream
                 (encode-message
                   (str
                     {:action "keep-alive"})
                   nil
                   user-agent))
               (.flush
                 output-stream)
               (reset!
                 keep-alive-time-in-seconds
                 keep-alive-message-period))
            )]
      (while @sub-running
        (let [decoded-message (read-till-fin-is-one
                                input-stream
                                user-agent)
              {request-method :request-method} header-map-with-body
              header-map-with-body (assoc
                                     header-map-with-body
                                     :request-method
                                     (str
                                       "ws "
                                       request-method))]
          (routing-fn
            (assoc
              header-map-with-body
              :websocket
               {:websocket-message decoded-message
                :websocket-message-length (count decoded-message)
                :websocket-output-fn (fn [server-message
                                          & [first-byte]]
                                       (try
                                         (.write
                                           output-stream
                                           (encode-message
                                             server-message
                                             first-byte
                                             user-agent))
                                         (.flush
                                           output-stream)
                                         (reset!
                                           keep-alive-time-in-seconds
                                           keep-alive-message-period)
                                         (when (= first-byte
                                                  -120)
                                           (reset!
                                             sub-running
                                             false)
                                           (.close
                                             client-socket)
                                           (swap!
                                             client-sockets
                                             disj
                                             client-socket)
                                           (reset!
                                             keep-alive
                                             false)
                                           (reset!
                                             keep-alive-time-in-seconds
                                             0)
                                           (future-cancel
                                             keep-alive-thread))
                                         (catch Exception e
                                           (println (.getMessage e))
                                          ))
                                       nil)})
           ))
       ))
    (catch Exception e
      (println (.getMessage e))
     ))
 )

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
      #_(println
        (str
          header-string
          "\n\n"
          response-header
          "\n\n"
          response-body))
      (.write
        output-stream
        (.getBytes
          response-header
          "UTF-8"))
      (.write
        output-stream
        response-body)
      (when (= (:upgrade request)
               "websocket")
        (accept-web-socket-request-subprocess
          routing-fn
          client-socket
          request))
     )
    (catch Exception e
      (println (.getMessage e))
      (println e))
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
            @thread-pool-size
            @thread-pool-size
            @thread-pool-size
            java.util.concurrent.TimeUnit/SECONDS
            (java.util.concurrent.ArrayBlockingQueue.
              @thread-pool-size))
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

