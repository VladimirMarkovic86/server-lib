(ns server-lib.core
  (:require [clojure.string :as cstring]
            [utils-lib.core :as utils]
            [ajax-lib.http.status-code :as stc
                                       :refer [status-code]]
            [ajax-lib.http.general-header :as gh]
            [ajax-lib.http.entity-header :as eh]
            [ajax-lib.http.mime-type :as mt]
            [ajax-lib.http.response-header :as rsh]
            [ajax-lib.http.request-header :as rh]
            [clojure.java.io :as io])
  (:import [server_lib RejectedExecutionHandlerHTTPResponse]
           [java.net ServerSocket
                     Socket]
           [javax.net.ssl SSLServerSocket
                          SSLSocket
                          KeyManagerFactory
                          SSLContext]
           [java.security KeyStore
                          MessageDigest]
           [java.util.concurrent Executors
                                 ThreadPoolExecutor
                                 TimeUnit
                                 ArrayBlockingQueue]
           [java.io FileInputStream
                    File
                    InputStream]
           [javax.xml.bind DatatypeConverter]))

(def main-thread
     (atom nil))

(def server-socket
     (atom nil))

(def running
     (atom false))

(def core-pool-size
     (atom 5))

(def maximum-pool-size
     (atom 10))

(def keep-thread-alive-time-in-seconds
     60)

(def array-blocking-queue-size
     (atom 100))

(def executor
     (atom nil))

(def client-sockets
     (atom #{}))

(def public-dir-a
     (atom "public"))

(def port-a
     (atom 9000))

(def keep-alive-message-period
     25)

(def cached-files
     (atom #{}))

(def keystore-type-set
     #{"JKS"
       "JCEKS"
       "PKCS12"
       "PKCS11"})

(def ssl-contex-set
     #{"TLSv1"
       "TLSv1.1"
       "TLSv1.2"
       "SSLv3"})

(def http-set
     #{"HTTP/1.0"
       "HTTP/1.1"
       ;;"HTTP/2.0"
       })

(def utf-8
     "UTF-8")

(defn pow
  "Square of value"
  [value]
  (Math/pow
    2
    value))

(defn parse-body
  "Read entity-body from request, convert from string to clojure data"
  [request-body
   content-type]
  (try
    (if (and request-body
             (string?
               request-body)
             (= content-type
                "text/clojurescript"))
      (read-string
        request-body)
      request-body)
    (catch Exception e
      (println (.getMessage e))
     ))
 )

(defn read-certificate
  "Read certificate if it exists"
  [keystore-file-path]
  (if (and keystore-file-path
           (string?
             keystore-file-path)
           (not
             (cstring/blank?
               keystore-file-path))
       )
    (try
      [(io/input-stream
         (io/resource
           keystore-file-path))]
      (catch Exception e
        (println
          (.getMessage
            e))
        [(io/input-stream
           (io/resource
             "certificate/default_certificate.jks"))
         "ultras12"]))
    [(io/input-stream
       (io/resource
         "certificate/default_certificate.jks"))
     "ultras12"]))

(defn open-server-socket
  "Open server socket on local machine"
  [port
   & [{keystore-file-path :keystore-file-path
       keystore-type :keystore-type
       keystore-password :keystore-password
       ssl-context :ssl-context}]]
  (when (or (nil?
              @server-socket)
            (and (not
                   (nil?
                     @server-socket))
                 (.isClosed
                   @server-socket))
         )
    (let [port (or port
                   @port-a)
          keystore-type (if (contains?
                              keystore-type-set
                              keystore-type)
                          keystore-type
                          "JKS")
          ssl-context (if (contains?
                            ssl-contex-set
                            ssl-context)
                        ssl-context
                        "TLSv1.2")]
      (if (and keystore-file-path
               (string?
                 keystore-file-path)
               (not
                 (cstring/blank?
                   keystore-file-path))
               keystore-password
               (string?
                 keystore-password)
               (not
                 (cstring/blank?
                   keystore-password))
           )
        (try
          (let [ks (KeyStore/getInstance
                     keystore-type)
                [ks-is
                 ks-is-password] (read-certificate
                                   keystore-file-path)
                keystore-password (if ks-is-password
                                    ks-is-password
                                    keystore-password)
                pass-char-array (char-array
                                  keystore-password)
                void (.load
                       ks
                       ks-is
                       pass-char-array)
                kmf (KeyManagerFactory/getInstance
                      (KeyManagerFactory/getDefaultAlgorithm))
                sc (SSLContext/getInstance
                     ssl-context)
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
                ssl-contex-set))
            (reset!
              server-socket
              ssl-server-socket))
          (catch Exception e
            (println
              (.getMessage
                e))
            (println e))
         )
        (reset!
          server-socket
          (ServerSocket.
            port))
       ))
   )
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
        @executor)
      (.close
        @server-socket)
      (catch Exception e
        (println
          (.getMessage
            e))
       ))
   )
  (println
    "Server stopped"))

(defn read-header
  "Read header of XMLHTTPRequest"
  [header]
  (when (and header
             (string?
               header)
             (not
               (cstring/blank?
                 header))
         )
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
          request-get-params (when (and request-uri
                                        (string?
                                          request-uri)
                                        (not
                                          (cstring/blank?
                                            request-uri))
                                    )
                               (cstring/split
                                 request-uri
                                 #"\?"
                                 2))
          request-uri (get
                        request-get-params
                        0)
          request-get-params (get
                               request-get-params
                               1)
          request-get-params (if request-get-params
                               (let [split-params (cstring/split
                                                    request-get-params
                                                    #"&")
                                     request-get-params-map (atom {})]
                                 (doseq [split-param split-params]
                                   (let [[key-name
                                          key-value] (cstring/split
                                                       split-param
                                                       #"="
                                                       2)]
                                     (swap!
                                       request-get-params-map
                                       assoc
                                       (keyword
                                         key-name)
                                       key-value))
                                  )
                                 @request-get-params-map)
                               {})
          header-map (atom
                       {:request-method request-method
                        :request-uri request-uri
                        :request-protocol request-protocol
                        :request-get-params request-get-params})]
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
 )

(defn pack-response
  "Format response as string out of clojure's response-map"
  [{request-protocol :request-protocol}
   {status :status
    headers :headers
    body :body}]
  (if (and (contains?
             http-set
             request-protocol)
           status
           (number?
             status)
           headers
           (map?
             headers))
    (let [response (atom "")
          response-body (atom nil)
          status-line (str
                        request-protocol
                        " "
                        (status-code
                          status)
                        "\r\n")
          content-type-header (get
                                headers
                                (eh/content-type))
          headers (dissoc
                    headers
                    (eh/content-type))]
      (swap!
        response
        str
        status-line)
      (doseq [[m-key
               m-val] headers]
        (if (vector?
              m-val)
          (doseq [mv m-val]
            (swap!
              response
              str
              m-key
              ": "
              mv
              "\r\n"))
          (swap!
            response
            str
            m-key
            ": "
            m-val
            "\r\n"))
       )
      (when-let [body body]
        (let [content-type-header-a (atom nil)]
          (when content-type-header
            (reset!
              content-type-header-a
              content-type-header))
          (when (and (nil?
                       @content-type-header-a)
                     (or (map?
                           body)
                         (vector?
                           body))
                 )
            (reset!
              content-type-header-a
              (mt/text-clojurescript))
           )
          (when (and (nil?
                       @content-type-header-a)
                     (or (string?
                           body)
                         (bytes?
                           body))
                 )
            (reset!
              content-type-header-a
              (mt/text-plain))
           )
          (when (nil?
                  @content-type-header-a)
            (reset!
              content-type-header-a
              (mt/text-plain))
           )
          (when (= @content-type-header-a
                   (mt/text-plain))
            (let [body (if (string?
                             body)
                         body
                         (str
                           body))]
              (reset!
                response-body
                (.getBytes
                  body
                  utf-8))
             ))
          (when (= @content-type-header-a
                   (mt/text-clojurescript))
            (reset!
              response-body
              (.getBytes
                (str
                  body)
                utf-8))
           )
          (when (nil?
                  @response-body)
            (reset!
              response-body
              body))
          (when-not (bytes?
                      @response-body)
            (reset!
              response-body
              (.getBytes
                (str
                  @response-body)
                utf-8))
           )
          (swap!
            response
            str
            (eh/content-type)
            ": "
            @content-type-header-a
            "\r\n"
            (eh/content-length)
            ": "
            (count
              @response-body)
            "\r\n\r\n"))
       )
      [@response
       @response-body])
   [(str
      "HTTP/1.1 404 Not Found\r\n"
      "Access-Control-Allow-Origin: https://sample:1613\r\n"
      "Access-Control-Allow-Methods: OPTIONS, GET, POST, DELETE, PUT\r\n"
      "Access-Control-Allow-Credentials: true\r\n"
      "Access-Control-Allow-Headers: Content-Type\r\n"
      "Connection: keep-alive\r\n"
;;      "Date: Mon, 01 Apr 2019, 12:19:23 GMT+01:00\r\n"
      "Server: cljserver\r\n"
      "Accept-Ranges: bytes\r\n")])
 )

(defn md5-checksum-fn
  "Generate checksum of read file"
  [ary]
  (let [ary (if (string?
                  ary)
              (.getBytes
                ary)
              (when (bytes?
                      ary)
                ary))]
    (when (bytes?
            ary)
      (let [capacity (if (< (count
                              ary)
                            1024)
                       (count
                         ary)
                       1024)
            byte-array-v (byte-array
                           capacity
                           ary)
            digest (MessageDigest/getInstance
                     "MD5")
            void (.update
                   digest
                   byte-array-v)
            digested (.digest
                       digest)]
        (.toUpperCase
          (DatatypeConverter/printHexBinary
            digested))
       ))
   ))

(defn get-ranges
  "Returns ranges from request header"
  [range-request]
  (if (and range-request
           (string?
             range-request)
           (< 5
              (count
                range-request))
       )
    (let [ranges-request (.substring
                           range-request
                           6)
          ranges-request (cstring/split
                           ranges-request
                           #",")
          first-range (first
                        ranges-request)
          [start-pos
           end-pos] (cstring/split
                      first-range
                      #"-"
                      2)
          start-pos-int (if (and start-pos
                                 (string?
                                   start-pos)
                                 (> (count
                                      start-pos)
                                    0))
                          (read-string
                            start-pos)
                          -1)
          start-pos-int (if (and (number?
                                   start-pos-int)
                                 (< 0
                                    start-pos-int))
                          start-pos-int
                          -1)
          end-pos-int (if (and end-pos
                               (string?
                                 end-pos)
                               (> (count
                                    end-pos)
                                  0))
                        (read-string
                          end-pos)
                        -1)
          end-pos-int (if (and (number?
                                 end-pos-int)
                               (< 0
                                  end-pos-int))
                        end-pos-int
                        -1)]
      (if (or (= start-pos-int
                 -1)
              (= end-pos-int
                 -1))
        [start-pos-int
         end-pos-int]
        (if (< start-pos-int
               end-pos-int)
          [start-pos-int
           end-pos-int]
          [-1
           -1]))
     )
    [-1
     -1]))

(defn read-file
  "Read file and recognize it's mime type"
  [file-path
   extension
   {range-request :range
    if-none-match :if-none-match
    cache-control :cache-control}
   & [is-absolute-path]]
  (if (and file-path
           (string?
             file-path)
           (not
             (cstring/blank?
               file-path))
       )
    (try
      (let [response-map (atom {})
            status-a (atom
                       (stc/ok))]
        (if (contains?
              @cached-files
              if-none-match)
          (reset!
            response-map
            {:status (stc/not-modified)
             :headers {(gh/cache-control) "max-age=86400"
                       (rsh/etag) if-none-match}})
          (let [is (if is-absolute-path
                     (FileInputStream.
                       (File.
                         file-path))
                     (io/input-stream
                       (io/resource
                         (str
                           @public-dir-a
                           file-path))
                      ))
                [start-pos
                 end-pos] (get-ranges
                            range-request)
                available-bytes (.available
                                  is)
                start-byte-position (if (not= start-pos
                                              -1)
                                      start-pos
                                      0)
                end-byte-position (if (not= end-pos
                                            -1)
                                    end-pos
                                    available-bytes)
                bytes-length (- end-byte-position
                                start-byte-position)
                ary (byte-array
                      bytes-length)
                skipped-bytes (.skip
                                is
                                start-byte-position)
                read-file-bytes (.read
                                  is
                                  ary)
                close-is (.close
                            is)
                md5-checksum (when (not= cache-control
                                         "no-cache")
                               (let [generated-md5 (md5-checksum-fn
                                                     ary)]
                                 (swap!
                                   cached-files
                                   conj
                                   generated-md5)
                                 generated-md5))
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
            (when (= extension
                     "mp4")
              (reset!
                mime-type
                (mt/video-mp4))
             )
            (when (= extension
                     "mkv")
              (reset!
                mime-type
                "video/mkv"))
            (when (contains?
                    #{"jpeg"
                      "jpg"}
                    extension)
              (reset!
                mime-type
                (mt/image-jpeg))
             )
            (when (= extension
                     "png")
              (reset!
                mime-type
                (mt/image-png))
             )
            (when (= extension
                     "gif")
              (reset!
                mime-type
                (mt/image-gif))
             )
            (when (= extension
                     "bmp")
              (reset!
                mime-type
                (mt/image-bmp))
             )
            (when (= extension
                     "webp")
              (reset!
                mime-type
                (mt/image-webp))
             )
            (when (= extension
                     "ico")
              (reset!
                mime-type
                (mt/image-x-icon))
             )
            (when (= extension
                     "pdf")
              (reset!
                mime-type
                (mt/application-pdf))
             )
            (if md5-checksum
              (reset!
                headers
                {(eh/content-type) @mime-type
                 (gh/cache-control) "max-age=86400"
                 (rsh/etag) md5-checksum})
              (reset!
                headers
                {(eh/content-type) @mime-type}))
            (reset!
              body
              ary)
            (when (not= start-byte-position
                        0)
              (reset!
                status-a
                (stc/partial-content))
              (swap!
                headers
                assoc
                (eh/content-range)
                (str
                  "bytes "
                  start-byte-position
                  "-"
                  end-byte-position
                  "/"
                  available-bytes))
             )
            (reset!
              response-map
              {:status @status-a
               :headers @headers
               :body @body}))
         )
        @response-map)
      (catch Exception e
        (println
          (.getMessage
            e))
        {:status (stc/internal-server-error)
         :headers {(eh/content-type) (mt/text-clojurescript)}
         :body {:status "error"
                :error-message (.getMessage
                                 e)}})
     )
    {:status (stc/not-found)
     :headers {(eh/content-type) (mt/text-clojurescript)}
     :body {:status "error"
            :error-message "File path not defined"}}))

(defn default-routing-fn
  "Default routing function for reading files recognized by request uri"
  [request
   response]
  (let [request-uri (:request-uri request)
        request-uri (if (= request-uri
                           "/")
                      "/index.html"
                      request-uri)
        extension-start (when (and request-uri
                                   (string?
                                     request-uri)
                                   (not
                                     (cstring/blank?
                                       request-uri))
                               )
                          (clojure.string/last-index-of
                            request-uri
                            "."))
        extension (when extension-start
                    (.substring
                      request-uri
                      (inc
                        extension-start)
                      (count
                        request-uri))
                   )]
    (when (and extension
               response
               (instance?
                 clojure.lang.Atom
                 response))
      (let [response-map (read-file
                           request-uri
                           extension
                           request)
            response-headers (atom {})
            response-map (update-in
                           response-map
                           [:headers]
                           conj
                           (:headers
                             @response))]
        (doseq [[header-key
                 header-value] (:headers response-map)]
          (swap!
            response-headers
            assoc
            header-key header-value))
        (doseq [[header-key
                 header-value] (:headers @response)]
          (swap!
            response-headers
            assoc
            header-key header-value))
        (let [response-map (assoc
                             response-map
                             :headers @response-headers)]
          (reset!
            response
            response-map))
       ))
   ))

(defn add-default-response-headers
  "Add default headers in every response"
  [response]
  (when (and response
             (instance?
               clojure.lang.Atom
               response)
             (map?
               @response))
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
      (utils/current-date))
    (swap!
      response
      assoc-in
      [:headers
       (rsh/server)]
      "cljserver")
    (swap!
      response
      assoc-in
      [:headers
       (rsh/accept-ranges)]
      "bytes"))
 )

(defn cors-check
  "Check is access allowed"
  [{origin :origin
    host :host}
   response
   default-response-headers]
  (when (and response
             (instance?
               clojure.lang.Atom
               response))
    (if origin
      (if default-response-headers
        (let [access-control-allow-origin (get
                                            default-response-headers
                                            (rsh/access-control-allow-origin))
              access-control-allow-origin (if (set?
                                                access-control-allow-origin)
                                            (when (contains?
                                                    access-control-allow-origin
                                                    origin)
                                              origin)
                                            (when (= access-control-allow-origin
                                                     origin)
                                              origin))
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
               :headers {(eh/content-type) (mt/text-clojurescript)}
               :body {:status "Error"
                      :message "Forbidden"}}))
         )
        (reset!
          response
          {:status (stc/forbidden)
           :headers {(eh/content-type) (mt/text-clojurescript)}
           :body {:status "Error"
                  :message "Forbidden"}}))
      (when default-response-headers
        (let [access-control-allow-origin (get
                                            default-response-headers
                                            (rsh/access-control-allow-origin))
              https-host-origin (str
                                  "https://"
                                  host)
              http-host-origin (str
                                 "http://"
                                 host)
              same-origin (if (set?
                                access-control-allow-origin)
                            (or (contains?
                                  access-control-allow-origin
                                  https-host-origin)
                                (contains?
                                  access-control-allow-origin
                                  http-host-origin))
                            (or (= access-control-allow-origin
                                   https-host-origin)
                                (= access-control-allow-origin
                                   http-host-origin))
                           )]
          (when-not same-origin
            (reset!
              response
              {:status (stc/forbidden)
               :headers {(eh/content-type) (mt/text-clojurescript)}
               :body {:status "Error"
                      :message "Forbidden"}}))
         ))
     ))
 )

(defn decode-message
  "Decode message sent through websocket
   
   https://tools.ietf.org/html/rfc6455#section-5.2"
  [encoded-bytes
   key-array]
  (when (and encoded-bytes
             (vector?
               encoded-bytes)
             key-array
             (vector?
               key-array)
             (= (count
                  key-array)
                4))
    (let [message (atom [])]
      (dotimes [i (count
                    encoded-bytes)]
        (swap!
          message
          conj
          (bit-xor
            (get
              encoded-bytes
              i)
            (get
              key-array
              (bit-and
                i
                0x3))
           ))
       )
      @message))
 )

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
        message (if message
                  (if (string?
                        message)
                    message
                    (if (or (map?
                              message)
                            (vector?
                              message))
                      (str
                        message)
                      (if (bytes?
                            message)
                        message
                        (str
                          message))
                     ))
                  "")
        message-bytes (if (= first-byte
                             -120)
                        (if (string?
                              message)
                          (.getBytes
                            (str
                              "  "
                              message)
                            utf-8)
                          (let [new-message (apply
                                              conj
                                              [32 32]
                                              message)
                                new-message (byte-array
                                              new-message)]
                            new-message))
                        (if (string?
                              message)
                          (.getBytes
                            message
                            utf-8)
                          message))
        encoded-message (if user-agent
                          (atom message-bytes)
                          (atom []))]
    (when-not user-agent
      (dotimes [i (count
                    message-bytes)]
        (swap!
          encoded-message
          conj
          (bit-xor
            (get
              message-bytes
              i)
            (get
              key-array
              (bit-and
                i
                0x3))
           ))
       ))
    (let [message-count (count
                          @encoded-message)
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

(defn handler-fn
  "Handle request by passing request to routing-fn function
   routing-fn - is function implemented in applications server side
   request - is XMLHTTPRequest string converted into clojure map
   default-response-headers - map of default response headers defined when starting server"
  [routing-fn
   request
   default-response-headers
   reject]
  (if reject
    (let [response (atom
                     {:status (stc/not-found)
                      :headers {(eh/content-type) (mt/text-clojurescript)}
                      :body {:status "Error"
                             :message "Try again later"}})]
      (add-default-response-headers
        response)
      (pack-response
        request
        @response))
    (if (= (:upgrade request)
           "websocket")
      (let [sec-websocket-key (:sec-websocket-key request)
            sec-websocket-accept (DatatypeConverter/printBase64Binary
                                   (.digest
                                     (MessageDigest/getInstance
                                       "SHA-1")
                                     (.getBytes
                                       (str
                                         sec-websocket-key
                                         "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")
                                       utf-8))
                                  )
          user-agent (:user-agent request)
          connect-message (encode-message
                            "Здраво"
                            nil
                            user-agent)]
        (pack-response
          request
          {:status (stc/switching-protocols)
           :headers {"Connection" "Upgrade"
                     "Upgrade" "websocket"
                     "Sec-WebSocket-Accept" sec-websocket-accept
                     "Content-Type" "bytes"}
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
          (let [response-map (if (fn?
                                   routing-fn)
                               (routing-fn
                                 request)
                               {:status (stc/not-found)
                                :headers {(eh/content-type) (mt/text-clojurescript)}
                                :body {:status "Error"
                                       :message "Try something else"}})
                response-map (if (and (:headers response-map)
                                      (map?
                                        (:headers response-map))
                                  )
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

(defn calculate-message-length
  "Calculate message length from first bytes
  
   https://tools.ietf.org/html/rfc6455#section-5.2"
  [payload-byte
   input-stream]
  (if (and payload-byte
           (number?
             payload-byte)
           (< -1
              payload-byte))
    (case payload-byte
      126 (if (and input-stream
                   (instance?
                     InputStream
                     input-stream)
                   (<= 2
                       (.available
                         input-stream))
               )
            (let [third-byte (* (.read
                                  input-stream)
                                (pow 8))
                  fourth-byte (.read
                                input-stream)]
              ;(println third-byte)
              ;(println fourth-byte)
              (+ third-byte
                 fourth-byte))
            0)
      127 (if (and input-stream
                   (instance?
                     InputStream
                     input-stream)
                   (<= 8
                       (.available
                         input-stream))
               )
            (let [third-byte (* (.read
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
            0)
      payload-byte)
    0))

(defn prepend-zeros
  "Prepend zeros to binary representation of byte"
  [byte-param]
  (if (and byte-param
           (number?
             byte-param))
    (let [byte-binary (Long/toBinaryString
                        byte-param)
          fraction (mod
                     (count
                       byte-binary)
                     8)
          byte-count (if (< 0
                            fraction)
                       (- 8
                          fraction)
                       0)
          byte-atom (atom
                      byte-binary)]
      (dotimes [i byte-count]
        (swap!
          byte-atom
          (fn [atom-value
               prepend-value]
            (str
              prepend-value
              atom-value))
          0))
      @byte-atom)
    "00000000"))

(defn positive-byte-value
  "Transforms negative byte value to positive, and positive stays the same"
  [byte-value]
  (if (and byte-value
           (number?
             byte-value))
    (let [byte-value (unchecked-byte
                       byte-value)]
      (if (< byte-value
             0)
        (+ 127
           (+ 129
              byte-value))
        byte-value))
    0))

(defn read-till-fin-is-one
  "Read from input stream all messages till FIN bit has value 1 (one)"
  [input-stream]
  (if (and input-stream
           (instance?
             InputStream
             input-stream)
       )
    (let [message (atom [])
          fin-atom (atom 0)
          opcode-atom (atom 0)]
      (while (not= @fin-atom
                   1)
        (let [w-first-byte (.read
                             input-stream)
              first-byte-binary (prepend-zeros
                                  w-first-byte)
              fin (.charAt
                    first-byte-binary
                    0)
              rsv1 (.charAt
                     first-byte-binary
                     1)
              rsv2 (.charAt
                     first-byte-binary
                     2)
              rsv3 (.charAt
                     first-byte-binary
                     3)
              opcode3 (if (= (.charAt
                               first-byte-binary
                               4)
                             \1)
                        (pow 3)
                        0)
              opcode2 (if (= (.charAt
                               first-byte-binary
                               5)
                             \1)
                        (pow 2)
                        0)
              opcode1 (if (= (.charAt
                               first-byte-binary
                               6)
                             \1)
                        (pow 1)
                        0)
              opcode0 (if (= (.charAt
                               first-byte-binary
                               7)
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
              second-byte-binary (prepend-zeros
                                   (positive-byte-value
                                     second-byte))
              mask-bit (str
                         (first
                           second-byte-binary))
              payload-length-bits (.substring
                                    second-byte-binary
                                    1)
              payload-byte (Byte/parseByte
                             payload-length-bits
                             2)
              message-length (calculate-message-length
                               payload-byte
                               input-stream)
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
          ;(println "First byte bits" first-byte-binary)
          ;(println "FIN" fin)
          ;(println "RSV1" rsv1)
          ;(println "RSV2" rsv2)
          ;(println "RSV3" rsv3)
          ;(println "OPCODE" opcode)
          ;(println "Second byte bits" second-byte-binary)
          ;(println "MASK" mask-bit)
          ;(println "Payload bits" payload-length-bits)
          ;(println "Payload byte" payload-byte)
          ;(println "Payload length" message-length)
          (reset!
            fin-atom
            (read-string
              (str
                fin))
           )
          (swap!
            opcode-atom
            +
            opcode)
          (dotimes [i message-length]
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
      (if (= @opcode-atom
             1)
        (String.
          (byte-array
            @message)
          utf-8)
        (byte-array
          @message))
     )
    ""))

(defn accept-web-socket-request-subprocess
  "Work with established websocket connection"
  [routing-fn
   client-socket
   header-map-with-body]
  (when (and client-socket
             (or (instance?
                   Socket
                   client-socket)
                 (instance?
                   SSLSocket
                   client-socket))
         )
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
                                  input-stream)
                decoded-message-length (count
                                         decoded-message)
                {request-method :request-method} header-map-with-body
                header-map-with-body (assoc
                                       header-map-with-body
                                       :request-method
                                       (str
                                         "ws "
                                         request-method))]
            (if (< 0
                   decoded-message-length)
              (routing-fn
                (assoc
                  header-map-with-body
                  :websocket
                   {:websocket-message decoded-message
                    :websocket-message-length decoded-message-length
                    :websocket-socket client-socket
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
                                          )})
               )
              (do
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
             ))
         ))
      (catch Exception e
        (println (.getMessage e))
       ))
   ))

(defn accept-request
  "Reads input stream from client socket
   converts request string into clojure map
   and passes it to routing-fn function for
   other processing"
  [routing-fn
   client-socket
   default-response-headers
   reject]
  (when (and client-socket
             (or (instance?
                   Socket
                   client-socket)
                 (instance?
                   SSLSocket
                   client-socket))
         )
    (try
      (let [input-stream (.getInputStream
                           client-socket)
            output-stream (.getOutputStream
                            client-socket)
            body-separator [13 10 13 10]
            last-four-bytes (atom [0 0 0 0])
            header (atom [])
            counter (atom 30)
            read-stream (while (and (not= body-separator
                                          @last-four-bytes)
                                    (not= @last-four-bytes
                                          [-1 -1 -1 -1]))
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
                         )]
        (when-not (= @last-four-bytes
                     [-1 -1 -1 -1])
          (swap!
            header
            utils/remove-index-from-vector
            (into
              []
              (range
                (- (count @header)
                   4)
                (count @header)) 
             ))
          (let [header-string (String.
                                (byte-array
                                  @header)
                                utf-8)
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
                request (if-not (empty?
                                  @body-bytes)
                          (let [body (if (contains?
                                           #{(mt/text-plain)
                                             (mt/text-clojurescript)}
                                           (:content-type header-map))
                                       (parse-body
                                         (String.
                                           (byte-array
                                             @body-bytes)
                                           utf-8)
                                         (:content-type header-map))
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
                utf-8))
            (if response-body
              (.write
                output-stream
                response-body)
              (.write
                output-stream
                (.getBytes
                  "\r\n"
                  utf-8)))
            (when (= (:upgrade request)
                     "websocket")
              (accept-web-socket-request-subprocess
                routing-fn
                client-socket
                request))
           ))
       )
      (catch Exception e
        (println (.getMessage e))
        #_(println e))
      (finally
        (.close
          client-socket)
        (swap!
          client-sockets
          disj
          client-socket))
     )
   )
  )

(defn while-loop
  "While loop of accepting and responding on clients requests"
  [routing-fn
   & [default-response-headers]]
  (when (and @server-socket
             (or (instance?
                   SSLServerSocket
                   @server-socket)
                 (instance?
                   ServerSocket
                   @server-socket))
             (not
               (.isClosed
                 @server-socket))
             @executor
             (instance?
               ThreadPoolExecutor
               @executor)
             (not
               (.isShutdown
                 @executor))
         )
    (try
      (while @running
        (let [client-socket (.accept
                              @server-socket)]
          (swap!
            client-sockets
            conj
            client-socket)
          (.execute
            @executor
            (fn [& [reject]]
              (accept-request
                routing-fn
                client-socket
                default-response-headers
                reject))
           ))
       )
      (catch Exception e
        (println
          (.getMessage
            e))
       ))
   ))

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
    (if (or (nil?
              @main-thread)
            (and (not
                   (nil?
                     @main-thread))
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
          executor
          (ThreadPoolExecutor.
            @core-pool-size
            @maximum-pool-size
            keep-thread-alive-time-in-seconds
            TimeUnit/SECONDS
            (ArrayBlockingQueue.
              @array-blocking-queue-size)
            (RejectedExecutionHandlerHTTPResponse.))
         )
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

