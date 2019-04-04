(ns server-lib.core-test
  (:require [clojure.test :refer :all]
            [server-lib.core :refer :all]
            [clojure.string :as cstring]
            [request-server-lib.core :as reqs]
            [ajax-lib.http.status-code :as stc]
            [ajax-lib.http.general-header :as gh]
            [ajax-lib.http.entity-header :as eh]
            [ajax-lib.http.mime-type :as mt]
            [ajax-lib.http.response-header :as rsh]))

(deftest test-pow
  (testing "Test pow"
    
    (let [result (pow
                   2)]
      (is
        (= result
           4.0)
       )
     )
    
    (let [result (pow
                   3)]
      (is
        (= result
           8.0)
       )
     )
    
    (let [result (pow
                   4)]
      (is
        (= result
           16.0)
       )
     )
    
    (let [result (pow
                   10)]
      (is
        (= result
           1024.0)
       )
     )
    
   ))

(deftest test-parse-body
  (testing "Test parse body"
    
    (let [request-with-body-i (str
                                {:test 1})
          request-with-body-ii {:test 1}
          request-with-body-iii "Test simple string"
          empty-request nil]
      
      (is
        (= (parse-body
             request-with-body-i
             "text/clojurescript")
           {:test 1}))
      
      (is
        (= (parse-body
             request-with-body-ii
             "application/clojurescript")
           {:test 1}))
      
      (is
        (= (parse-body
             request-with-body-iii
             "text/plain")
           "Test simple string"))
      
      (is
        (= (parse-body
             empty-request
             "")
           nil))
      
     )

   ))

(deftest test-read-certificate
  (testing "Test read certificate"
    
    (let [keystore-file-path nil
          [ks-is
           ks-is-password] (read-certificate
                             keystore-file-path)]
      
      (is
        (not
          (nil?
            ks-is))
       )
      
      (is
        (instance?
          java.io.BufferedInputStream
          ks-is)
       )
      
      (is
        (not
          (nil?
            ks-is-password))
       )
      
     )
    
    (let [keystore-file-path "certificate/default_certificate.jks"
          [ks-is
           ks-is-password] (read-certificate
                             keystore-file-path)]
      
      (is
        (not
          (nil?
            ks-is))
       )
      
      (is
        (instance?
          java.io.BufferedInputStream
          ks-is)
       )
      
      (is
        (nil?
          ks-is-password)
       )
      
     )
    
   ))

(deftest test-open-server-socket
  (testing "Test open server socket"
    
    (let [port nil
          https-config nil
          result (open-server-socket
                   port
                   https-config)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (instance?
          java.net.ServerSocket
          result)
       )
      
      (.close
        @server-socket)
      
      (reset!
        server-socket
        nil)
      
     )
    
    (let [port 9999
          https-config nil
          result (open-server-socket
                   port
                   https-config)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (instance?
          java.net.ServerSocket
          result)
       )
      
      (.close
        @server-socket)
      
      (reset!
        server-socket
        nil)
      
     )
    
    (let [port 9999
          https-config {:keystore-file-path ""
                        :keystore-type ""
                        :keystore-password ""
                        :ssl-context ""}
          result (open-server-socket
                   port
                   https-config)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (instance?
          java.net.ServerSocket
          result)
       )
      
      (.close
        @server-socket)
      
      (reset!
        server-socket
        nil)
      
     )
    
    (let [port 9999
          https-config {:keystore-file-path "certificate/sample_server.jks"
                        :keystore-type ""
                        :keystore-password "ultras12"
                        :ssl-context ""}
          result (open-server-socket
                   port
                   https-config)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (instance?
          sun.security.ssl.SSLServerSocketImpl
          result)
       )
      
      (.close
        @server-socket)
      
      (reset!
        server-socket
        nil)
      
     )
    
    (let [port 9999
          https-config {:keystore-file-path "certificate/default_certificate.jks"
                        :keystore-type ""
                        :keystore-password "ultras12"
                        :ssl-context ""}
          result (open-server-socket
                   port
                   https-config)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (instance?
          sun.security.ssl.SSLServerSocketImpl
          result)
       )
      
      (.close
        @server-socket)
      
      (reset!
        server-socket
        nil)
      
     )
    
    (let [port 9999
          https-config {:keystore-file-path "certificate/default_certificate.jks"
                        :keystore-type "JKS"
                        :keystore-password "ultras12"
                        :ssl-context ""}
          result (open-server-socket
                   port
                   https-config)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (instance?
          sun.security.ssl.SSLServerSocketImpl
          result)
       )
      
      (.close
        @server-socket)
      
      (reset!
        server-socket
        nil)
      
     )
    
    (let [port 9999
          https-config {:keystore-file-path "certificate/default_certificate.jks"
                        :keystore-type "JKS"
                        :keystore-password "ultras12"
                        :ssl-context "TLSv1.2"}
          result (open-server-socket
                   port
                   https-config)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (instance?
          sun.security.ssl.SSLServerSocketImpl
          result)
       )
      
      (.close
        @server-socket)
      
      (reset!
        server-socket
        nil)
      
     )
    
   ))

(deftest test-read-header
  (testing "Test read header"
  
    (let [header nil
          result (read-header
                   header)]
      
      (is
        (nil?
          result)
       )
      
     )
  
    (let [header 1
          result (read-header
                   header)]
      
      (is
        (nil?
          result)
       )
      
     )
  
    (let [header ""
          result (read-header
                   header)]
      
      (is
        (nil?
          result)
       )
      
     )
  
    (let [header "test"
          result (read-header
                   header)]
      
      (is
        (= result
           {:request-method "test"
            :request-uri nil
            :request-protocol nil
            :request-get-params {}}))
      
     )
  
    (let [header "GET / HTTP/1.1"
          result (read-header
                   header)]
      
      (is
        (= result
           {:request-method "GET"
            :request-uri "/"
            :request-protocol "HTTP/1.1"
            :request-get-params {}}))
      
     )
  
    (let [header "GET /test?param=value HTTP/1.1"
          result (read-header
                   header)]
      
      (is
        (= result
           {:request-method "GET"
            :request-uri "/test"
            :request-protocol "HTTP/1.1"
            :request-get-params {:param "value"}}))
      
     )
  
    (let [header "GET /test?param=value&param1=value1 HTTP/1.1"
          result (read-header
                   header)]
      
      (is
        (= result
           {:request-method "GET"
            :request-uri "/test"
            :request-protocol "HTTP/1.1"
            :request-get-params {:param "value"
                                 :param1 "value1"}}))
      
     )
  
    (let [header (str
                   "GET / HTTP/1.1\r\n"
                   "Host: ide:1614\r\n"
                   "User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:66.0) Gecko/20100101 Firefox/66.0\r\n"
                   "Accept: text/css,*/*;q=0.1\r\n"
                   "Accept-Language: sr,en;q=0.5\r\n"
                   "Accept-Encoding: gzip, deflate, br\r\n"
                   "Connection: keep-alive\r\n"
                   "Cookie: long-session=36b8b320-e17f-47ed-aede-44d62883eb90; long-session-visible=exists\r\n"
                   "Pragma: no-cache\r\n"
                   "Cache-Control: no-cache\r\n")
          result (read-header
                   header)]
      
      (is
        (= result
           {:request-uri "/"
            :user-agent "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:66.0) Gecko/20100101 Firefox/66.0"
            :pragma "no-cache"
            :request-protocol "HTTP/1.1"
            :host "ide:1614"
            :request-get-params {}
            :cache-control "no-cache"
            :cookie "long-session=36b8b320-e17f-47ed-aede-44d62883eb90; long-session-visible=exists"
            :accept-encoding "gzip, deflate, br"
            :connection "keep-alive"
            :accept-language "sr,en;q=0.5"
            :request-method "GET"
            :accept "text/css,*/*;q=0.1"})
       )
      
     )
    
   ))

(deftest test-pack-response
  (testing "Test pack response"
    
    (let [request nil
          response-map nil
          [response-headers
           response-body] (pack-response
                            request
                            response-map)]
      
      (is
        (not
          (nil?
            response-headers))
       )
      
      (is
        (= response-headers
           (str
             "HTTP/1.1 404 Not Found\r\n"
             "Access-Control-Allow-Origin: https://sample:1613\r\n"
             "Access-Control-Allow-Methods: OPTIONS, GET, POST, DELETE, PUT\r\n"
             "Access-Control-Allow-Credentials: true\r\n"
             "Access-Control-Allow-Headers: Content-Type\r\n"
             "Connection: keep-alive\r\n"
             "Server: cljserver\r\n"
             "Accept-Ranges: bytes\r\n"))
       )
      
      (is
        (nil?
          response-body)
       )
      
     )
    
    (let [request {:request-protocol "HTTP/1.1"}
          response-map nil
          [response-headers
           response-body] (pack-response
                            request
                            response-map)]
      
      (is
        (not
          (nil?
            response-headers))
       )
      
      (is
        (= response-headers
           (str
             "HTTP/1.1 404 Not Found\r\n"
             "Access-Control-Allow-Origin: https://sample:1613\r\n"
             "Access-Control-Allow-Methods: OPTIONS, GET, POST, DELETE, PUT\r\n"
             "Access-Control-Allow-Credentials: true\r\n"
             "Access-Control-Allow-Headers: Content-Type\r\n"
             "Connection: keep-alive\r\n"
             "Server: cljserver\r\n"
             "Accept-Ranges: bytes\r\n"))
       )
      
      (is
        (nil?
          response-body)
       )
      
     )
    
    (let [request {:request-protocol "HTTP/1.1"}
          response-map {:status 200
                        :headers nil
                        :body nil}
          [response-headers
           response-body] (pack-response
                            request
                            response-map)]
      
      (is
        (not
          (nil?
            response-headers))
       )
      
      (is
        (= response-headers
           (str
             "HTTP/1.1 404 Not Found\r\n"
             "Access-Control-Allow-Origin: https://sample:1613\r\n"
             "Access-Control-Allow-Methods: OPTIONS, GET, POST, DELETE, PUT\r\n"
             "Access-Control-Allow-Credentials: true\r\n"
             "Access-Control-Allow-Headers: Content-Type\r\n"
             "Connection: keep-alive\r\n"
             "Server: cljserver\r\n"
             "Accept-Ranges: bytes\r\n"))
       )
      
      (is
        (nil?
          response-body)
       )
      
     )
    
    (let [request {:request-protocol "HTTP/1.1"}
          response-map {:status 200
                        :headers {}
                        :body nil}
          [response-headers
           response-body] (pack-response
                            request
                            response-map)]
      
      (is
        (not
          (nil?
            response-headers))
       )
      
      (is
        (= response-headers
           (str
             "HTTP/1.1 200 OK\r\n"))
       )
      
      (is
        (nil?
          response-body)
       )
      
     )
    
    (let [request {:request-protocol "HTTP/1.1"}
          response-map {:status 200
                        :headers (sorted-map
                                   (rsh/server) "cljserver"
                                   (gh/connection) "keep-alive")
                        :body nil}
          [response-headers
           response-body] (pack-response
                            request
                            response-map)]
      
      (is
        (not
          (nil?
            response-headers))
       )
      
      (is
        (= response-headers
           (str
             "HTTP/1.1 200 OK\r\n"
             "Connection: keep-alive\r\n"
             "Server: cljserver\r\n"))
       )
      
      (is
        (nil?
          response-body)
       )
      
     )
    
    (let [request {:request-protocol "HTTP/1.1"}
          response-map {:status 200
                        :headers (sorted-map
                                   (rsh/server) "cljserver"
                                   (gh/connection) "keep-alive")
                        :body "Test body"}
          [response-headers
           response-body] (pack-response
                            request
                            response-map)]
      
      (is
        (not
          (nil?
            response-headers))
       )
      
      (is
        (= response-headers
           (str
             "HTTP/1.1 200 OK\r\n"
             "Connection: keep-alive\r\n"
             "Server: cljserver\r\n"
             "Content-Type: text/plain\r\n"
             "Content-Length: 9\r\n\r\n"))
       )
      
      (is
        (not
          (nil?
            response-body))
       )
      
      (is
        (bytes?
          response-body)
       )
      
      (is
        (= (String.
             response-body
             "UTF-8")
           "Test body")
       )
      
     )
    
    (let [request {:request-protocol "HTTP/1.1"}
          response-map {:status 200
                        :headers (sorted-map
                                   (gh/connection) "keep-alive"
                                   (eh/content-type) (mt/text-clojurescript)
                                   (rsh/server) "cljserver")
                        :body "Test body"}
          [response-headers
           response-body] (pack-response
                            request
                            response-map)]
      
      (is
        (not
          (nil?
            response-headers))
       )
      
      (is
        (= response-headers
           (str
             "HTTP/1.1 200 OK\r\n"
             "Connection: keep-alive\r\n"
             "Server: cljserver\r\n"
             "Content-Type: text/clojurescript\r\n"
             "Content-Length: 9\r\n\r\n"))
       )
      
      (is
        (not
          (nil?
            response-body))
       )
      
      (is
        (bytes?
          response-body)
       )
      
      (is
        (= (String.
             response-body
             "UTF-8")
           "Test body")
       )
      
     )
    
    (let [request {:request-protocol "HTTP/1.1"}
          response-map {:status 200
                        :headers (sorted-map
                                   (rsh/server) "cljserver"
                                   (gh/connection) "keep-alive")
                        :body {}}
          [response-headers
           response-body] (pack-response
                            request
                            response-map)]
      
      (is
        (not
          (nil?
            response-headers))
       )
      
      (is
        (= response-headers
           (str
             "HTTP/1.1 200 OK\r\n"
             "Connection: keep-alive\r\n"
             "Server: cljserver\r\n"
             "Content-Type: text/clojurescript\r\n"
             "Content-Length: 2\r\n\r\n"))
       )
      
      (is
        (not
          (nil?
            response-body))
       )
      
      (is
        (bytes?
          response-body)
       )
      
      (let [stringified-map (String.
                              response-body
                              "UTF-8")
            parsed-string (read-string
                            stringified-map)]
      
        (is
          (= stringified-map
             "{}")
         )
        
        (is
          (map?
            parsed-string)
         )
        
        (is
          (empty?
            parsed-string)
         )
       
       )
      
     )
    
    (let [request {:request-protocol "HTTP/1.1"}
          response-map {:status 200
                        :headers (sorted-map
                                   (rsh/server) "cljserver"
                                   (gh/connection) "keep-alive"
                                   (eh/content-type) (mt/text-plain))
                        :body {}}
          [response-headers
           response-body] (pack-response
                            request
                            response-map)]
      
      (is
        (not
          (nil?
            response-headers))
       )
      
      (is
        (= response-headers
           (str
             "HTTP/1.1 200 OK\r\n"
             "Connection: keep-alive\r\n"
             "Server: cljserver\r\n"
             "Content-Type: text/plain\r\n"
             "Content-Length: 2\r\n\r\n"))
       )
      
      (is
        (not
          (nil?
            response-body))
       )
      
      (is
        (bytes?
          response-body)
       )
      
      (let [stringified-map (String.
                              response-body
                              "UTF-8")
            parsed-string (read-string
                            stringified-map)]
      
        (is
          (= stringified-map
             "{}")
         )
        
        (is
          (map?
            parsed-string)
         )
        
        (is
          (empty?
            parsed-string)
         )
       
       )
      
     )
    
    (let [request {:request-protocol "HTTP/1.1"}
          response-map {:status 200
                        :headers (sorted-map
                                   (rsh/server) "cljserver"
                                   (gh/connection) "keep-alive")
                        :body {:test 1}}
          [response-headers
           response-body] (pack-response
                            request
                            response-map)]
      
      (is
        (not
          (nil?
            response-headers))
       )
      
      (is
        (= response-headers
           (str
             "HTTP/1.1 200 OK\r\n"
             "Connection: keep-alive\r\n"
             "Server: cljserver\r\n"
             "Content-Type: text/clojurescript\r\n"
             "Content-Length: 9\r\n\r\n"))
       )
      
      (is
        (not
          (nil?
            response-body))
       )
      
      (is
        (bytes?
          response-body)
       )
      
      (let [stringified-map (String.
                              response-body
                              "UTF-8")
            parsed-string (read-string
                            stringified-map)]
      
        (is
          (= stringified-map
             "{:test 1}")
         )
        
        (is
          (map?
            parsed-string)
         )
        
        (is
          (= parsed-string
             {:test 1})
         )
       
       )
      
     )
    
    (let [request {:request-protocol "HTTP/1.1"}
          response-map {:status 200
                        :headers (sorted-map
                                   (rsh/server) "cljserver"
                                   (gh/connection) "keep-alive")
                        :body [1 2 3]}
          [response-headers
           response-body] (pack-response
                            request
                            response-map)]
      
      (is
        (not
          (nil?
            response-headers))
       )
      
      (is
        (= response-headers
           (str
             "HTTP/1.1 200 OK\r\n"
             "Connection: keep-alive\r\n"
             "Server: cljserver\r\n"
             "Content-Type: text/clojurescript\r\n"
             "Content-Length: 7\r\n\r\n"))
       )
      
      (is
        (not
          (nil?
            response-body))
       )
      
      (is
        (bytes?
          response-body)
       )
      
      (let [stringified-map (String.
                              response-body
                              "UTF-8")
            parsed-string (read-string
                            stringified-map)]
      
        (is
          (= stringified-map
             "[1 2 3]")
         )
        
        (is
          (vector?
            parsed-string)
         )
        
        (is
          (= parsed-string
             [1 2 3])
         )
       
       )
      
     )
    
    (let [request {:request-protocol "HTTP/1.1"}
          response-map {:status 200
                        :headers (sorted-map
                                   (rsh/server) "cljserver"
                                   (gh/connection) "keep-alive")
                        :body 1}
          [response-headers
           response-body] (pack-response
                            request
                            response-map)]
      
      (is
        (not
          (nil?
            response-headers))
       )
      
      (is
        (= response-headers
           (str
             "HTTP/1.1 200 OK\r\n"
             "Connection: keep-alive\r\n"
             "Server: cljserver\r\n"
             "Content-Type: text/plain\r\n"
             "Content-Length: 1\r\n\r\n"))
       )
      
      (is
        (not
          (nil?
            response-body))
       )
      
      (is
        (bytes?
          response-body)
       )
      
      (let [stringified-map (String.
                              response-body
                              "UTF-8")
            parsed-string (read-string
                            stringified-map)]
      
        (is
          (= stringified-map
             "1")
         )
        
        (is
          (number?
            parsed-string)
         )
        
        (is
          (= parsed-string
             1)
         )
       
       )
      
     )
    
    (let [request {:request-protocol "HTTP/1.1"}
          response-body-raw (str
                              "<!DOCTYPE html>\n"
                              "<html>\n"
                              "  <head>\n"
                              "    <title>Test</title>\n"
                              "  </head>\n"
                              "  <body>\n"
                              "    <div></div>\n"
                              "  </body>\n"
                              "</html>\n")
          response-map {:status 200
                        :headers (sorted-map
                                   (rsh/server) "cljserver"
                                   (gh/connection) "keep-alive"
                                   (eh/content-type) (mt/text-html))
                        :body response-body-raw}
          [response-headers
           response-body] (pack-response
                            request
                            response-map)]
      
      (is
        (not
          (nil?
            response-headers))
       )
      
      (is
        (= response-headers
           (str
             "HTTP/1.1 200 OK\r\n"
             "Connection: keep-alive\r\n"
             "Server: cljserver\r\n"
             "Content-Type: text/html\r\n"
             "Content-Length: 109\r\n\r\n"))
       )
      
      (is
        (not
          (nil?
            response-body))
       )
      
      (is
        (bytes?
          response-body)
       )
      
      (let [stringified-body (String.
                               response-body
                               "UTF-8")]
      
        (is
          (= stringified-body
             response-body-raw)
         )
       
       )
      
     )
    
   ))

(deftest test-md5-checksum-fn
  (testing "Test md5 checksum fn"
    
    (let [ary nil
          result (md5-checksum-fn
                   ary)]
      
      (is
        (nil?
          result))
      
     )
    
    (let [ary ""
          result (md5-checksum-fn
                   ary)]
      
      (is
        (= result
           "D41D8CD98F00B204E9800998ECF8427E"))
      
     )
    
    (let [ary "123"
          result (md5-checksum-fn
                   ary)]
      
      (is
        (= result
           "202CB962AC59075B964B07152D234B70"))
      
     )
    
    (let [ary "321"
          result (md5-checksum-fn
                   ary)]
      
      (is
        (= result
           "CAF1A3DFB505FFED0D024130F58C5CFA"))
      
     )
    
    (let [ary (byte-array
                5
                [1 2 3 4 5])
          result (md5-checksum-fn
                   ary)]
      
      (is
        (= result
           "7CFDD07889B3295D6A550914AB35E068"))
      
     )
    
   ))

(deftest test-get-ranges
  (testing "Test get ranges"
    
    (let [request-range nil
          result (get-ranges
                   request-range)]
      
      (is
        (= result
           [-1
            -1])
       )
      
     )
    
    (let [request-range ""
          result (get-ranges
                   request-range)]
      
      (is
        (= result
           [-1
            -1])
       )
      
     )
    
    (let [request-range "byte"
          result (get-ranges
                   request-range)]
      
      (is
        (= result
           [-1
            -1])
       )
      
     )
    
    (let [request-range "bytes"
          result (get-ranges
                   request-range)]
      
      (is
        (= result
           [-1
            -1])
       )
      
     )
    
    (let [request-range "bytes="
          result (get-ranges
                   request-range)]
      
      (is
        (= result
           [-1
            -1])
       )
      
     )
    
    (let [request-range "bytes=a"
          result (get-ranges
                   request-range)]
      
      (is
        (= result
           [-1
            -1])
       )
      
     )
    
    (let [request-range "bytes=a-b"
          result (get-ranges
                   request-range)]
      
      (is
        (= result
           [-1
            -1])
       )
      
     )
    
    (let [request-range "bytes=1"
          result (get-ranges
                   request-range)]
      
      (is
        (= result
           [1
            -1])
       )
      
     )
    
    (let [request-range "bytes=1-2"
          result (get-ranges
                   request-range)]
      
      (is
        (= result
           [1
            2])
       )
      
     )
    
    (let [request-range "bytes=2-1"
          result (get-ranges
                   request-range)]
      
      (is
        (= result
           [-1
            -1])
       )
      
     )
    
    (let [request-range "bytes=2--10"
          result (get-ranges
                   request-range)]
      
      (is
        (= result
           [2
            -1])
       )
      
     )
    
    (let [request-range "bytes=1000-"
          result (get-ranges
                   request-range)]
      
      (is
        (= result
           [1000
            -1])
       )
      
     )
    
    (let [request-range "bytes=1000-2000"
          result (get-ranges
                   request-range)]
      
      (is
        (= result
           [1000
            2000])
       )
      
     )
    
   ))

(deftest test-read-file
  (testing "Test read file"
    
    (let [file-path nil
          extension nil
          request nil
          is-absolute-path nil
          {status :status
           {content-type (eh/content-type)} :headers
           {body-status :status
            error-message :error-message} :body} (read-file
                                                   file-path
                                                   extension
                                                   request
                                                   is-absolute-path)]
      
      (is
        (= status
           (stc/not-found))
       )
      
      (is
        (= content-type
           (mt/text-clojurescript))
       )
      
      (is
        (= body-status
           "error")
       )
      
     )
    
    (let [file-path ""
          extension nil
          request nil
          is-absolute-path nil
          {status :status
           {content-type (eh/content-type)} :headers
           {body-status :status
            error-message :error-message} :body} (read-file
                                                   file-path
                                                   extension
                                                   request
                                                   is-absolute-path)]
      
      (is
        (= status
           (stc/not-found))
       )
      
      (is
        (= content-type
           (mt/text-clojurescript))
       )
      
      (is
        (= body-status
           "error")
       )
      
     )
    
    (let [file-path "/index.html"
          extension nil
          request nil
          is-absolute-path nil
          {status :status
           {content-type (eh/content-type)} :headers
           response-body :body} (read-file
                                  file-path
                                  extension
                                  request
                                  is-absolute-path)]
      
      (is
        (= status
           (stc/ok))
       )
      
      (is
        (= content-type
           (mt/text-plain))
       )
      
      (is
        (not
          (nil?
            response-body))
       )
      
      (is
        (bytes?
          response-body)
       )
      
     )
    
    (let [file-path "/index.html"
          extension "html"
          request {:range ""
                   :if-none-match ""
                   :cache-control ""}
          is-absolute-path nil
          result (read-file
                   file-path
                   extension
                   request
                   is-absolute-path)
          {status :status
           {content-type (eh/content-type)
            cache-control (gh/cache-control)
            etag (rsh/etag)} :headers
           response-body :body} result]
      
      (is
        (= status
           (stc/ok))
       )
      
      (is
        (= content-type
           (mt/text-html))
       )
      
      (is
        (= cache-control
           "max-age=86400")
       )
      
      (is
        (= etag
           "770E35FE086632E82EAB39A8A3CE0878")
       )
      
      (is
        (not
          (nil?
            response-body))
       )
      
      (is
        (bytes?
          response-body)
       )
      
     )
    
    (let [file-path "/index.html"
          extension "html"
          request {:range "bytes=0-1024"
                   :if-none-match ""
                   :cache-control ""}
          is-absolute-path nil
          result (read-file
                   file-path
                   extension
                   request
                   is-absolute-path)
          {status :status
           {content-type (eh/content-type)
            cache-control (gh/cache-control)
            etag (rsh/etag)} :headers
           response-body :body} result]
      
      (is
        (= status
           (stc/ok))
       )
      
      (is
        (= content-type
           (mt/text-html))
       )
      
      (is
        (= cache-control
           "max-age=86400")
       )
      
      (is
        (= etag
           "5455F50788F4DC2B9CA7F380BADCA2B7")
       )
      
      (is
        (not
          (nil?
            response-body))
       )
      
      (is
        (bytes?
          response-body)
       )
      
      (is
        (= (count
             response-body)
           1024)
       )
      
     )
    
    (let [file-path "/index.html"
          extension "html"
          request {:range "bytes=0-1024"
                   :if-none-match ""
                   :cache-control "no-cache"}
          is-absolute-path nil
          result (read-file
                   file-path
                   extension
                   request
                   is-absolute-path)
          {status :status
           {content-type (eh/content-type)} :headers
           response-body :body} result]
      
      (is
        (= status
           (stc/ok))
       )
      
      (is
        (= content-type
           (mt/text-html))
       )
      
      (is
        (not
          (nil?
            response-body))
       )
      
      (is
        (bytes?
          response-body)
       )
      
      (is
        (= (count
             response-body)
           1024)
       )
      
     )
    
    (let [file-path "/index.html"
          extension "html"
          request {:range "bytes=0-1024"
                   :if-none-match "5455F50788F4DC2B9CA7F380BADCA2B7"
                   :cache-control ""}
          is-absolute-path nil
          result (read-file
                   file-path
                   extension
                   request
                   is-absolute-path)
          {status :status
           {content-type (eh/content-type)
            cache-control (gh/cache-control)
            etag (rsh/etag)} :headers
           response-body :body} result]
      
      (is
        (= status
           (stc/not-modified))
       )
      
      (is
        (= cache-control
           "max-age=86400")
       )
      
      (is
        (= etag
           "5455F50788F4DC2B9CA7F380BADCA2B7")
       )
      
      (is
        (nil?
          response-body)
       )
      
     )
    
    (let [file-path "/index.html"
          extension "html"
          request {:range "bytes=0-1024"
                   :if-none-match ""
                   :cache-control ""}
          is-absolute-path true
          result (read-file
                   file-path
                   extension
                   request
                   is-absolute-path)
          {status :status
           {content-type (eh/content-type)} :headers
           {body-status :status} :body} result]
      
      (is
        (= status
           (stc/internal-server-error))
       )
      
      (is
        (= content-type
           (mt/text-clojurescript))
       )
      
      (is
        (= body-status
           "error")
       )
      
     )
    
    (let [file-path "/etc/environment"
          extension "html"
          request {:range "bytes=0-1024"
                   :if-none-match ""
                   :cache-control ""}
          is-absolute-path true
          result (read-file
                   file-path
                   extension
                   request
                   is-absolute-path)
          {status :status
           {content-type (eh/content-type)} :headers
           response-body :body} result]
      
      (is
        (= status
           (stc/ok))
       )
      
      (is
        (= content-type
           (mt/text-html))
       )
      
      (is
        (not
          (nil?
            response-body))
       )
      
      (is
        (bytes?
          response-body)
       )
      
      (is
        (= (count
             response-body)
           1024)
       )
      
     )
    
   ))

(deftest test-default-routing-fn
  (testing "Test default routing fn"
    
    (let [request nil
          response nil
          result (default-routing-fn
                   request
                   response)]
      (is
        (nil?
          response)
       )
     )
    
    (let [request {:request-uri ""}
          response nil
          result (default-routing-fn
                   request
                   response)]
      (is
        (nil?
          response)
       )
     )
    
    (let [request {:request-uri "/"}
          response nil
          result (default-routing-fn
                   request
                   response)]
      (is
        (nil?
          response)
       )
     )
    
    (let [request {:request-uri "/"}
          response 1
          result (default-routing-fn
                   request
                   response)]
      (is
        (= response
           1)
       )
     )
    
    (let [request {:request-uri "/"}
          response (atom nil)
          result (default-routing-fn
                   request
                   response)
          {status :status
           {content-type (eh/content-type)
            cache-contol (gh/cache-control)} :headers
           response-body :body} @response]
      
      (is
        (= status
           (stc/ok))
       )
      
      (is
        (= content-type
           (mt/text-html))
       )
      
      (is
        (= cache-contol
           "max-age=86400")
       )
      
      (is
        (not
          (nil?
            response-body))
       )
      
      (is
        (bytes?
          response-body)
       )
      
     )
    
    (let [request {:request-uri "/"}
          response (atom {})
          result (default-routing-fn
                   request
                   response)
          {status :status
           {content-type (eh/content-type)
            cache-contol (gh/cache-control)} :headers
           response-body :body} @response]
      
      (is
        (= status
           (stc/ok))
       )
      
      (is
        (= content-type
           (mt/text-html))
       )
      
      (is
        (= cache-contol
           "max-age=86400")
       )
      
      (is
        (not
          (nil?
            response-body))
       )
      
      (is
        (bytes?
          response-body)
       )
      
     )
    
   ))

(deftest test-add-default-response-headers
  (testing "Test add default response headers"
    
    (let [response nil
          result (add-default-response-headers
                   response)]
      
      (is
        (nil?
          response)
       )
      
     )
    
    (let [response ""
          result (add-default-response-headers
                   response)]
      
      (is
        (= response
           "")
       )
      
     )
    
    (let [response (atom nil)
          result (add-default-response-headers
                   response)]
      
      (is
        (nil?
          @response)
       )
      
     )
    
    (let [response (atom {})
          result (add-default-response-headers
                   response)
          {{connection-value (gh/connection)
            date-value (gh/date)
            server-value (rsh/server)
            accept-ranges-value (rsh/accept-ranges)} :headers} @response]
      
      (is
        (= connection-value
           "keep-alive")
       )
      
      (let [sdf (java.text.SimpleDateFormat.
                  "E, dd MMM yyyy, HH:mm:ss zzz")
            current-date-obj (java.util.Date.)]
        
        (is
          (< (- (.getTime
                  current-date-obj)
                (.getTime
                  (.parse
                    sdf
                    date-value)))
             (* 1000
                60))
         )
        
        (is
          (instance?
            java.util.Date
            (.parse
              sdf
              date-value))
         )
        
       )
      
      (is
        (= server-value
           "cljserver")
       )
      
      (is
        (= accept-ranges-value
           "bytes")
       )
      
     )
    
    (let [response (atom {:response-field 1})
          result (add-default-response-headers
                   response)
          {response-field :response-field
           {connection-value (gh/connection)
            date-value (gh/date)
            server-value (rsh/server)
            accept-ranges-value (rsh/accept-ranges)} :headers} @response]
      
      (is
        (= response-field
           1)
       )
      
      (is
        (= connection-value
           "keep-alive")
       )
      
      (let [sdf (java.text.SimpleDateFormat.
                  "E, dd MMM yyyy, HH:mm:ss zzz")
            current-date-obj (java.util.Date.)]
        
        (is
          (< (- (.getTime
                  current-date-obj)
                (.getTime
                  (.parse
                    sdf
                    date-value)))
             (* 1000
                60))
         )
        
        (is
          (instance?
            java.util.Date
            (.parse
              sdf
              date-value))
         )
        
       )
      
      (is
        (= server-value
           "cljserver")
       )
      
      (is
        (= accept-ranges-value
           "bytes")
       )
      
     )
    
    (let [response (atom {:response-field 1
                          :headers {:header-field 2}})
          result (add-default-response-headers
                   response)
          {response-field :response-field
           {header-field :header-field
            connection-value (gh/connection)
            date-value (gh/date)
            server-value (rsh/server)
            accept-ranges-value (rsh/accept-ranges)} :headers} @response]
      
      (is
        (= response-field
           1)
       )
      
      (is
        (= header-field
           2)
       )
      
      (is
        (= connection-value
           "keep-alive")
       )
      
      (let [sdf (java.text.SimpleDateFormat.
                  "E, dd MMM yyyy, HH:mm:ss zzz")
            current-date-obj (java.util.Date.)]
        
        (is
          (< (- (.getTime
                  current-date-obj)
                (.getTime
                  (.parse
                    sdf
                    date-value)))
             (* 1000
                60))
         )
        
        (is
          (instance?
            java.util.Date
            (.parse
              sdf
              date-value))
         )
        
       )
      
      (is
        (= server-value
           "cljserver")
       )
      
      (is
        (= accept-ranges-value
           "bytes")
       )
      
     )
    
   ))

(deftest test-cors-check
  (testing "Test cors check"
    
    (let [request nil
          response nil
          default-response-headers nil
          result (cors-check
                   request
                   response
                   default-response-headers)]
      
      (is
        (nil?
          response)
       )
      
     )
    
    (let [request nil
          response {}
          default-response-headers nil
          result (cors-check
                   request
                   response
                   default-response-headers)]
      
      (is
        (= response
           {})
       )
      
     )
    
    (let [request nil
          response (atom {})
          default-response-headers nil
          result (cors-check
                   request
                   response
                   default-response-headers)]
      
      (is
        (= @response
           {})
       )
      
     )
    
    (let [request {:host "test:9999"
                   :origin nil}
          response (atom nil)
          default-response-headers
            {(rsh/access-control-allow-origin) #{"https://test1:9999"
                                                 "https://test2:9999"}}
          result (cors-check
                   request
                   response
                   default-response-headers)
          {status :status
           {content-type (eh/content-type)} :headers
           {body-status :status
            body-message :message} :body} @response]
      
      (is
        (= status
           (stc/forbidden))
       )
      
      (is
        (= content-type
           (mt/text-clojurescript))
       )
      
      (is
        (= body-status
           "Error")
       )
      
     )
    
    (let [request {:host "test1:9999"
                   :origin nil}
          response (atom nil)
          default-response-headers
            {(rsh/access-control-allow-origin) #{"https://test1:9999"
                                                 "https://test2:9999"}}
          result (cors-check
                   request
                   response
                   default-response-headers)]
      
      (is
        (nil?
          @response)
       )
      
     )
    
    (let [request {:host nil
                   :origin "https://test:9999"}
          response (atom nil)
          default-response-headers nil
          result (cors-check
                   request
                   response
                   default-response-headers)
          {status :status
           {content-type (eh/content-type)} :headers
           {body-status :status
            body-message :message} :body} @response]
      
      (is
        (= status
           (stc/forbidden))
       )
      
      (is
        (= content-type
           (mt/text-clojurescript))
       )
      
      (is
        (= body-status
           "Error")
       )
      
     )
    
    (let [request {:host nil
                   :origin "https://test:9999"}
          response (atom nil)
          default-response-headers
            {(rsh/access-control-allow-origin) #{"https://test1:9999"
                                                 "https://test2:9999"}}
          result (cors-check
                   request
                   response
                   default-response-headers)
          {status :status
           {content-type (eh/content-type)} :headers
           {body-status :status
            body-message :message} :body} @response]
      
      (is
        (= status
           (stc/forbidden))
       )
      
      (is
        (= content-type
           (mt/text-clojurescript))
       )
      
      (is
        (= body-status
           "Error")
       )
      
     )
    
    (let [request {:host nil
                   :origin "https://test1:9999"}
          response (atom nil)
          default-response-headers
            {(rsh/access-control-allow-origin) #{"https://test1:9999"
                                                 "https://test2:9999"}}
          result (cors-check
                   request
                   response
                   default-response-headers)
          {{access-control-allow-origin
             (rsh/access-control-allow-origin)} :headers} @response]
      
      (is
        (= access-control-allow-origin
           "https://test1:9999")
       )
      
     )
    
   ))

(deftest test-decode-message
  (testing "Test decode message"
    
    (let [encoded-bytes nil
          key-array nil
          result (decode-message
                   encoded-bytes
                   key-array)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [encoded-bytes ""
          key-array nil
          result (decode-message
                   encoded-bytes
                   key-array)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [encoded-bytes ""
          key-array ""
          result (decode-message
                   encoded-bytes
                   key-array)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [encoded-bytes "123"
          key-array ""
          result (decode-message
                   encoded-bytes
                   key-array)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [encoded-bytes [1 2 3 4 5]
          key-array ""
          result (decode-message
                   encoded-bytes
                   key-array)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [encoded-bytes [1 2 3 4 5]
          key-array [1 2 3 4]
          result (decode-message
                   encoded-bytes
                   key-array)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (= result
           [0 0 0 0 4])
       )
      
     )
    
    (let [encoded-bytes (encode-message
                          "Hello")
          encoded-bytes (rest
                          encoded-bytes)
          encoded-bytes (rest
                          encoded-bytes)
          encoded-bytes (into
                          []
                          encoded-bytes)
          key-array [(get
                       encoded-bytes
                       0)
                     (get
                       encoded-bytes
                       1)
                     (get
                       encoded-bytes
                       2)
                     (get
                       encoded-bytes
                       3)]
          encoded-bytes (rest
                          encoded-bytes)
          encoded-bytes (rest
                          encoded-bytes)
          encoded-bytes (rest
                          encoded-bytes)
          encoded-bytes (rest
                          encoded-bytes)
          encoded-bytes (into
                          []
                          encoded-bytes)
          result (decode-message
                   encoded-bytes
                   key-array)]
      
      (is
        (= (String.
             (byte-array
               result)
             utf-8)
           "Hello")
       )
      
     )
    
   ))

(deftest test-encode-message
  (testing "Test encode message"
    
    (let [message nil
          first-byte nil
          user-agent nil
          result (encode-message
                   message
                   first-byte
                   user-agent)
          result (rest
                   result)
          result (rest
                   result)
          result (into
                   []
                   result)
          key-vector [(get
                        result
                        0)
                      (get
                        result
                        1)
                      (get
                        result
                        2)
                      (get
                        result
                        3)]
          result (rest
                   result)
          result (rest
                   result)
          result (rest
                   result)
          result (rest
                   result)
          result (into
                   []
                   result)
          result (decode-message
                   result
                   key-vector)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (vector?
          result)
       )
      
      (is
        (empty?
          result)
       )
      
     )
    
    (let [message "Test"
          first-byte nil
          user-agent nil
          result (encode-message
                   message
                   first-byte
                   user-agent)
          result (rest
                   result)
          result (rest
                   result)
          result (into
                   []
                   result)
          key-vector [(get
                        result
                        0)
                      (get
                        result
                        1)
                      (get
                        result
                        2)
                      (get
                        result
                        3)]
          result (rest
                   result)
          result (rest
                   result)
          result (rest
                   result)
          result (rest
                   result)
          result (into
                   []
                   result)
          result (decode-message
                   result
                   key-vector)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (vector?
          result)
       )
      
      (is
        (= (String.
             (byte-array
               result)
             utf-8)
           "Test")
       )
      
     )
    
    (let [message "Test"
          first-byte -120
          user-agent nil
          result (encode-message
                   message
                   first-byte
                   user-agent)
          result (rest
                   result)
          result (rest
                   result)
          result (into
                   []
                   result)
          key-vector [(get
                        result
                        0)
                      (get
                        result
                        1)
                      (get
                        result
                        2)
                      (get
                        result
                        3)]
          result (rest
                   result)
          result (rest
                   result)
          result (rest
                   result)
          result (rest
                   result)
          result (into
                   []
                   result)
          result (decode-message
                   result
                   key-vector)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (vector?
          result)
       )
      
      (is
        (= (String.
             (byte-array
               result)
             utf-8)
           "  Test")
       )
      
     )
    
    (let [message {:test 1}
          first-byte nil
          user-agent nil
          result (encode-message
                   message
                   first-byte
                   user-agent)
          result (rest
                   result)
          result (rest
                   result)
          result (into
                   []
                   result)
          key-vector [(get
                        result
                        0)
                      (get
                        result
                        1)
                      (get
                        result
                        2)
                      (get
                        result
                        3)]
          result (rest
                   result)
          result (rest
                   result)
          result (rest
                   result)
          result (rest
                   result)
          result (into
                   []
                   result)
          result (decode-message
                   result
                   key-vector)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (vector?
          result)
       )
      
      (let [decoded (String.
                      (byte-array
                        result)
                      utf-8)]
        
        (is
          (= decoded
             "{:test 1}")
         )
        
        (is
          (= (read-string
               decoded)
             {:test 1})
         )
        
       )
      
     )
    
    (let [message [1 2 3]
          first-byte nil
          user-agent nil
          result (encode-message
                   message
                   first-byte
                   user-agent)
          result (rest
                   result)
          result (rest
                   result)
          result (into
                   []
                   result)
          key-vector [(get
                        result
                        0)
                      (get
                        result
                        1)
                      (get
                        result
                        2)
                      (get
                        result
                        3)]
          result (rest
                   result)
          result (rest
                   result)
          result (rest
                   result)
          result (rest
                   result)
          result (into
                   []
                   result)
          result (decode-message
                   result
                   key-vector)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (vector?
          result)
       )
      
      (let [decoded (String.
                      (byte-array
                        result)
                      utf-8)]
        
        (is
          (= decoded
             "[1 2 3]")
         )
        
        (is
          (= (read-string
               decoded)
             [1 2 3])
         )
        
       )
      
     )
    
    (let [message "Test"
          first-byte nil
          user-agent "Chrome"
          result (encode-message
                   message
                   first-byte
                   user-agent)
          result (rest
                   result)
          result (rest
                   result)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (seq?
          result)
       )
      
      (let [decoded (String.
                      (byte-array
                        result)
                      utf-8)]
        
        (is
          (= decoded
             "Test")
         )
        
       )
      
     )
    
   ))

(deftest test-handler-fn
  (testing "Test handler fn"
    
    (let [routing-fn nil
          request nil
          default-response-headers nil
          reject nil
          [result-response
           result-body] (handler-fn
                          routing-fn
                          request
                          default-response-headers
                          reject)]
      
      (is
        (= result-response
           (str
             "HTTP/1.1 404 Not Found\r\n"
             "Access-Control-Allow-Origin: https://sample:1613\r\n"
             "Access-Control-Allow-Methods: OPTIONS, GET, POST, DELETE, PUT\r\n"
             "Access-Control-Allow-Credentials: true\r\n"
             "Access-Control-Allow-Headers: Content-Type\r\n"
             "Connection: keep-alive\r\n"
             "Server: cljserver\r\n"
             "Accept-Ranges: bytes\r\n"))
       )
      
      (is
        (nil?
          result-body)
       )
      
     )
    
    (let [routing-fn (fn [param]
                       {:status (stc/ok)
                        :headers {(eh/content-type) (mt/text-clojurescript)}
                        :body {:status "success"}})
          request nil
          default-response-headers nil
          reject nil
          [result-response
           result-body] (handler-fn
                          routing-fn
                          request
                          default-response-headers
                          reject)]
      
      (is
        (= result-response
           (str
             "HTTP/1.1 404 Not Found\r\n"
             "Access-Control-Allow-Origin: https://sample:1613\r\n"
             "Access-Control-Allow-Methods: OPTIONS, GET, POST, DELETE, PUT\r\n"
             "Access-Control-Allow-Credentials: true\r\n"
             "Access-Control-Allow-Headers: Content-Type\r\n"
             "Connection: keep-alive\r\n"
             "Server: cljserver\r\n"
             "Accept-Ranges: bytes\r\n"))
       )
      
      (is
        (nil?
          result-body)
       )
      
     )
    
    (let [routing-fn (fn [param]
                       {:status (stc/ok)
                        :headers {(eh/content-type) (mt/text-clojurescript)}
                        :body {:status "success"}})
          request {:request-protocol "HTTP/1.1"}
          default-response-headers nil
          reject nil
          [result-response
           result-body] (handler-fn
                          routing-fn
                          request
                          default-response-headers
                          reject)]
      
      (is
        (number?
          (cstring/index-of
            result-response
            "HTTP/1.1 200 OK\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Connection: keep-alive\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Server: cljserver\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Accept-Ranges: bytes\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Content-Type: text/clojurescript\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Content-Length: 19\r\n"))
       )
      
      (is
        (not
          (nil?
            result-body))
       )
      
      (is
        (bytes?
          result-body)
       )
      
      (is
        (= (String.
             result-body
             utf-8)
           "{:status \"success\"}")
       )
      
     )
    
    (let [routing-fn (fn [param]
                       {:status (stc/ok)
                        :headers {(eh/content-type) (mt/text-clojurescript)}
                        :body {:status "success"}})
          request {:request-protocol "HTTP/1.1"}
          default-response-headers {"Default-Header-Name-1" "Default-Header-Value-1"}
          reject nil
          [result-response
           result-body] (handler-fn
                          routing-fn
                          request
                          default-response-headers
                          reject)]
      
      (is
        (number?
          (cstring/index-of
            result-response
            "HTTP/1.1 403 Forbidden\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Connection: keep-alive\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Server: cljserver\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Accept-Ranges: bytes\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Content-Type: text/clojurescript\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Content-Length: 39\r\n"))
       )
      
      (is
        (not
          (nil?
            result-body))
       )
      
      (is
        (bytes?
          result-body)
       )
      
      (is
        (= (String.
             result-body
             utf-8)
           "{:status \"Error\", :message \"Forbidden\"}")
       )
      
     )
    
    (let [routing-fn (fn [param]
                       {:status (stc/ok)
                        :headers {(eh/content-type) (mt/text-clojurescript)}
                        :body {:status "success"}})
          request {:request-protocol "HTTP/1.1"
                   :host "sample:1603"}
          default-response-headers {(rsh/access-control-allow-origin) "https://sample:1603"}
          reject nil
          [result-response
           result-body] (handler-fn
                          routing-fn
                          request
                          default-response-headers
                          reject)]
      
      (is
        (number?
          (cstring/index-of
            result-response
            "HTTP/1.1 200 OK\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Connection: keep-alive\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Server: cljserver\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Accept-Ranges: bytes\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Content-Type: text/clojurescript\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Content-Length: 19\r\n"))
       )
      
      (is
        (not
          (nil?
            result-body))
       )
      
      (is
        (bytes?
          result-body)
       )
      
      (is
        (= (String.
             result-body
             utf-8)
           "{:status \"success\"}")
       )
      
     )
    
    (let [routing-fn (fn [param]
                       {:status (stc/ok)
                        :headers {(eh/content-type) (mt/text-clojurescript)}
                        :body {:status "success"}})
          request {:request-protocol "HTTP/1.1"
                   :host "sample:1603"}
          default-response-headers {(rsh/access-control-allow-origin)
                                      #{"https://sample:1603"
                                        "http://sample:1603"}}
          reject nil
          [result-response
           result-body] (handler-fn
                          routing-fn
                          request
                          default-response-headers
                          reject)]
      
      (is
        (number?
          (cstring/index-of
            result-response
            "HTTP/1.1 200 OK\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Connection: keep-alive\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Server: cljserver\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Accept-Ranges: bytes\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Content-Type: text/clojurescript\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Content-Length: 19\r\n"))
       )
      
      (is
        (not
          (nil?
            result-body))
       )
      
      (is
        (bytes?
          result-body)
       )
      
      (is
        (= (String.
             result-body
             utf-8)
           "{:status \"success\"}")
       )
      
     )
    
    (let [routing-fn (fn [param]
                       {:status (stc/ok)
                        :headers {(eh/content-type) (mt/text-clojurescript)}
                        :body {:status "success"}})
          request {:request-protocol "HTTP/1.1"
                   :host "sample:1603"
                   :origin "https://sample:1603"}
          default-response-headers {(rsh/access-control-allow-origin)
                                      #{"https://sample:1603"
                                        "http://sample:1603"}}
          reject nil
          [result-response
           result-body] (handler-fn
                          routing-fn
                          request
                          default-response-headers
                          reject)]
      
      (is
        (number?
          (cstring/index-of
            result-response
            "HTTP/1.1 200 OK\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Access-Control-Allow-Origin: https://sample:1603\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Connection: keep-alive\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Server: cljserver\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Accept-Ranges: bytes\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Content-Type: text/clojurescript\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Content-Length: 19\r\n"))
       )
      
      (is
        (not
          (nil?
            result-body))
       )
      
      (is
        (bytes?
          result-body)
       )
      
      (is
        (= (String.
             result-body
             utf-8)
           "{:status \"success\"}")
       )
      
     )
    
    (let [routing-fn (fn [param]
                       {:status (stc/ok)
                        :headers {(eh/content-type) (mt/text-clojurescript)}
                        :body {:status "success"}})
          request {:request-protocol "HTTP/1.1"
                   :host "sample:1603"
                   :origin "https://sample:1603"}
          default-response-headers nil
          reject nil
          [result-response
           result-body] (handler-fn
                          routing-fn
                          request
                          default-response-headers
                          reject)]
      
      (is
        (number?
          (cstring/index-of
            result-response
            "HTTP/1.1 403 Forbidden\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Connection: keep-alive\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Server: cljserver\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Accept-Ranges: bytes\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Content-Type: text/clojurescript\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Content-Length: 39\r\n"))
       )
      
      (is
        (not
          (nil?
            result-body))
       )
      
      (is
        (bytes?
          result-body)
       )
      
      (is
        (= (String.
             result-body
             utf-8)
           "{:status \"Error\", :message \"Forbidden\"}")
       )
      
     )
    
    (let [routing-fn nil
          request {:request-protocol "HTTP/1.1"}
          default-response-headers nil
          reject true
          [result-response
           result-body] (handler-fn
                          routing-fn
                          request
                          default-response-headers
                          reject)]
      
      (is
        (number?
          (cstring/index-of
            result-response
            "HTTP/1.1 404 Not Found\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Connection: keep-alive\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Server: cljserver\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Accept-Ranges: bytes\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Content-Type: text/clojurescript\r\n"))
       )
      
      (is
        (number?
          (cstring/index-of
            result-response
            "Content-Length: 45\r\n"))
       )
      
      (is
        (not
          (nil?
            result-body))
       )
      
      (is
        (bytes?
          result-body)
       )
      
      (is
        (= (String.
             result-body
             utf-8)
           "{:status \"Error\", :message \"Try again later\"}")
       )
      
     )
    
   ))

(deftest test-calculate-message-length
  (testing "Test calculate message length"
    
    (let [second-byte nil
          input-stream nil
          result (calculate-message-length
                   second-byte
                   input-stream)]
      
      (is
        (= result
           0)
       )
      
     )
    
    (let [second-byte 126
          input-stream nil
          result (calculate-message-length
                   second-byte
                   input-stream)]
      
      (is
        (= result
           0)
       )
      
     )
    
    (let [second-byte 126
          input-stream (java.io.ByteArrayInputStream.
                         (byte-array
                           [0]))
          result (calculate-message-length
                   second-byte
                   input-stream)]
      
      (is
        (= result
           0)
       )
      
     )
    
    (let [second-byte 126
          input-stream (java.io.ByteArrayInputStream.
                         (byte-array
                           [0 1]))
          result (calculate-message-length
                   second-byte
                   input-stream)]
      
      (is
        (= result
           1.0)
       )
      
     )
    
    (let [second-byte 127
          input-stream nil
          result (calculate-message-length
                   second-byte
                   input-stream)]
      
      (is
        (= result
           0)
       )
      
     )
    
    (let [second-byte 127
          input-stream (java.io.ByteArrayInputStream.
                         (byte-array
                           [0]))
          result (calculate-message-length
                   second-byte
                   input-stream)]
      
      (is
        (= result
           0)
       )
      
     )
    
    (let [second-byte 127
          input-stream (java.io.ByteArrayInputStream.
                         (byte-array
                           [0 0 0 0 0 0 0]))
          result (calculate-message-length
                   second-byte
                   input-stream)]
      
      (is
        (= result
           0)
       )
      
     )
    
    (let [second-byte 127
          input-stream (java.io.ByteArrayInputStream.
                         (byte-array
                           [0 0 0 0 0 0 0 1]))
          result (calculate-message-length
                   second-byte
                   input-stream)]
      
      (is
        (= result
           1.0)
       )
      
     )
    
    (let [second-byte 127
          input-stream (java.io.ByteArrayInputStream.
                         (byte-array
                           [0 0 0 0 0 1 0 0]))
          result (calculate-message-length
                   second-byte
                   input-stream)]
      
      (is
        (= result
           65536.0)
       )
      
     )
    
    (let [second-byte -3
          input-stream nil
          result (calculate-message-length
                   second-byte
                   input-stream)]
      
      (is
        (= result
           0)
       )
      
     )
    
    (let [second-byte -4
          input-stream nil
          result (calculate-message-length
                   second-byte
                   input-stream)]
      
      (is
        (= result
           0)
       )
      
     )
    
   ))

(deftest test-positive-byte-value
  (testing "Test positive byte value"
    
    (let [byte-param nil
          result (positive-byte-value
                   byte-param)]
      
      (is
        (= result
           0)
       )
      
     )
    
    (let [byte-param 0
          result (positive-byte-value
                   byte-param)]
      
      (is
        (= result
           0)
       )
      
     )
    
    (let [byte-param 1
          result (positive-byte-value
                   byte-param)]
      
      (is
        (= result
           1)
       )
      
     )
    
    (let [byte-param 127
          result (positive-byte-value
                   byte-param)]
      
      (is
        (= result
           127)
       )
      
     )
    
    (let [byte-param 128
          result (positive-byte-value
                   byte-param)]
      
      (is
        (= result
           128)
       )
      
     )
    
    (let [byte-param -128
          result (positive-byte-value
                   byte-param)]
      
      (is
        (= result
           128)
       )
      
     )
    
    (let [byte-param -1
          result (positive-byte-value
                   byte-param)]
      
      (is
        (= result
           255)
       )
      
     )
    
    (let [byte-param -2
          result (positive-byte-value
                   byte-param)]
      
      (is
        (= result
           254)
       )
      
     )
    
    (let [byte-param 100000
          result (positive-byte-value
                   byte-param)]
      
      (is
        (= result
           160)
       )
      
     )
    
   ))

(deftest test-prepend-zeros
  (testing "Test prepend zeros"
    
    (let [result (prepend-zeros
                   nil)]
      
      (is
        (= result
           "00000000")
       )
      
     )
    
    (let [result (prepend-zeros
                   "")]
      
      (is
        (= result
           "00000000")
       )
      
     )
    
    (let [result (prepend-zeros
                   1)]
      
      (is
        (= result
           "00000001")
       )
      
     )
    
    (let [result (prepend-zeros
                   129)]
      
      (is
        (= result
           "10000001")
       )
      
     )
    
    (let [result (prepend-zeros
                   255)]
      
      (is
        (= result
           "11111111")
       )
      
     )
    
    (let [result (prepend-zeros
                   (Math/pow 2 15))]
      
      (is
        (= result
           "1000000000000000")
       )
      
     )
    
    (let [result (prepend-zeros
                   (Math/pow 2 16))]
      
      (is
        (= result
           "000000010000000000000000")
       )
      
     )
    
   ))

(deftest test-read-till-fin-is-one
  (testing "Test read till fin is one"
    
    (let [input-stream nil
          result (read-till-fin-is-one
                   input-stream)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (string?
          result)
       )
      
      (is
        (cstring/blank?
          result)
       )
      
     )
    
    (let [input-stream (java.io.ByteArrayInputStream.
                         (byte-array
                           []))
          result (read-till-fin-is-one
                   input-stream)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (bytes?
          result)
       )
      
      (is
        (= (count
             result)
           0)
       )
      
     )
    
    (let [input-stream (java.io.ByteArrayInputStream.
                         (byte-array
                           [129 -128]))
          result (read-till-fin-is-one
                   input-stream)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (string?
          result)
       )
      
      (is
        (cstring/blank?
          result)
       )
      
     )
    
    (let [input-stream (java.io.ByteArrayInputStream.
                         (byte-array
                           [129 -125 0 0 0 0 86 108 97 100 97]))
          result (read-till-fin-is-one
                   input-stream)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "Vla")
       )
      
     )
    
    (let [input-stream (java.io.ByteArrayInputStream.
                         (byte-array
                           [129 -123 0 0 0 0 86 108 97 100 97]))
          result (read-till-fin-is-one
                   input-stream)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "Vlada")
       )
      
     )
    
   ))

(deftest test-accept-web-socket-request-subprocess
  (testing "Test accept web socket request subprocess"
    
    (let [routing-fn nil
          client-socket nil
          header-map-with-body nil
          result (accept-web-socket-request-subprocess
                   routing-fn
                   client-socket
                   header-map-with-body)]
      
      (is
        (nil?
          result)
       )
      
     )
    
   ))

(deftest test-start-and-stop-server-while-loop-accept-request
  (testing "Test start and stop server, while loop and accept request"
    
    (let [routing-fn nil
          default-response-headers nil
          port nil
          https-conf nil
          public-dir nil
          result (start-server
                   routing-fn
                   default-response-headers
                   port
                   https-conf
                   public-dir)]
      
      (is
        (= @public-dir-a
           "public")
       )
      
      (is
        (= @port-a
           9000)
       )
      
      (is
        (not
          (nil?
            @server-socket))
       )
      
      (is
        (instance?
          java.net.ServerSocket
          @server-socket)
       )
      
      (is
        (false?
          (.isClosed
            @server-socket))
       )
      
      (is
        (not
          (nil?
            @main-thread))
       )
      
      (is
        (not
          (future-cancelled?
            @main-thread))
       )
      
      (is
        (not
          (future-done?
            @main-thread))
       )
      
      (is
        (true?
          @running)
       )
      
      (stop-server)
      
      (is
        (true?
          (.isClosed
            @server-socket))
       )
      
      (is
        (false?
          @running)
       )
      
      (is
        (future-cancelled?
          @main-thread)
       )
      
      (is
        (future-done?
          @main-thread)
       )
      
     )
    
    (let [routing-fn nil
          default-response-headers nil
          port 9999
          https-conf nil
          public-dir "test/js"
          result (start-server
                   routing-fn
                   default-response-headers
                   port
                   https-conf
                   public-dir)]
      
      (is
        (= @public-dir-a
           "test/js")
       )
      
      (is
        (= @port-a
           9999)
       )
      
      (is
        (not
          (nil?
            @server-socket))
       )
      
      (is
        (instance?
          java.net.ServerSocket
          @server-socket)
       )
      
      (is
        (false?
          (.isClosed
            @server-socket))
       )
      
      (is
        (not
          (nil?
            @main-thread))
       )
      
      (is
        (not
          (future-cancelled?
            @main-thread))
       )
      
      (is
        (not
          (future-done?
            @main-thread))
       )
      
      (is
        (true?
          @running)
       )
      
      (stop-server)
      
      (is
        (true?
          (.isClosed
            @server-socket))
       )
      
      (is
        (false?
          @running)
       )
      
      (is
        (future-cancelled?
          @main-thread)
       )
      
      (is
        (future-done?
          @main-thread)
       )
      
      (reset!
        public-dir-a
        "public")
      
     )
    
    (let [routing-fn nil
          default-response-headers nil
          port 9999
          https-conf {:keystore-file-path "./resources/certificate/default_certificate.jks"
                      :keystore-password "ultras12"
                      :keystore-type nil
                      :ssl-context nil}
          public-dir nil
          result (start-server
                   routing-fn
                   default-response-headers
                   port
                   https-conf
                   public-dir)]
      
      (is
        (= @port-a
           9999)
       )
      
      (is
        (not
          (nil?
            @server-socket))
       )
      
      (is
        (instance?
          java.net.ServerSocket
          @server-socket)
       )
      
      (is
        (false?
          (.isClosed
            @server-socket))
       )
      
      (is
        (not
          (nil?
            @main-thread))
       )
      
      (is
        (not
          (future-cancelled?
            @main-thread))
       )
      
      (is
        (not
          (future-done?
            @main-thread))
       )
      
      (is
        (true?
          @running)
       )
      
      (let [host "localhost"
            port @port-a
            request-method "POST"
            request-uri "/"
            certificate-config https-conf
            request-body nil
            processed-reponse (reqs/fire-request
                                host
                                port
                                request-method
                                request-uri
                                certificate-config
                                request-body)
            {protocol :protocol
             server :server
             status-code :status-code
             status :status} processed-reponse]
        
        (is
          (= protocol
             "HTTP/1.1")
         )
        
        (is
          (= server
             "cljserver")
         )
        
        (is
          (= status-code
             "403")
         )
        
        (is
          (= status
             "Forbidden")
         )
        
       )
      
      (stop-server)
      
      (is
        (true?
          (.isClosed
            @server-socket))
       )
      
      (is
        (false?
          @running)
       )
      
      (is
        (future-cancelled?
          @main-thread)
       )
      
      (is
        (future-done?
          @main-thread)
       )
      
     )
    
    (let [routing-fn nil
          default-response-headers nil
          port 9999
          https-conf {:keystore-file-path "./resources/certificate/default_certificate.jks"
                      :keystore-password "ultras12"
                      :keystore-type nil
                      :ssl-context nil}
          public-dir nil
          result (start-server
                   routing-fn
                   default-response-headers
                   port
                   https-conf
                   public-dir)]
      
      (is
        (= @port-a
           9999)
       )
      
      (is
        (not
          (nil?
            @server-socket))
       )
      
      (is
        (instance?
          java.net.ServerSocket
          @server-socket)
       )
      
      (is
        (false?
          (.isClosed
            @server-socket))
       )
      
      (is
        (not
          (nil?
            @main-thread))
       )
      
      (is
        (not
          (future-cancelled?
            @main-thread))
       )
      
      (is
        (not
          (future-done?
            @main-thread))
       )
      
      (is
        (true?
          @running)
       )
      
      (let [fire-request-thread (future
                                  (let [host "localhost"
                                        port @port-a
                                        request-method "POST"
                                        request-uri "/"
                                        certificate-config nil
                                        request-body nil
                                        processed-reponse (reqs/fire-request
                                                            host
                                                            port
                                                            request-method
                                                            request-uri
                                                            certificate-config
                                                            request-body)
                                        {protocol :protocol
                                         server :server
                                         status-code :status-code
                                         status :status} processed-reponse]
                                    
                                    (is
                                      (nil?
                                        processed-reponse)
                                     )
                                    
                                   ))]
        (stop-server)
        
        (is
          (true?
            (.isClosed
              @server-socket))
         )
        
        (is
          (false?
            @running)
         )
        
        (is
          (future-cancelled?
            @main-thread)
         )
        
        (is
          (future-done?
            @main-thread)
         )
        
        (future-cancel
          fire-request-thread)
       )

     )
    
    (let [routing-fn nil
          default-response-headers nil
          port 9999
          https-conf nil
          public-dir nil
          result (start-server
                   routing-fn
                   default-response-headers
                   port
                   https-conf
                   public-dir)]
      
      (is
        (= @port-a
           9999)
       )
      
      (is
        (not
          (nil?
            @server-socket))
       )
      
      (is
        (instance?
          java.net.ServerSocket
          @server-socket)
       )
      
      (is
        (false?
          (.isClosed
            @server-socket))
       )
      
      (is
        (not
          (nil?
            @main-thread))
       )
      
      (is
        (not
          (future-cancelled?
            @main-thread))
       )
      
      (is
        (not
          (future-done?
            @main-thread))
       )
      
      (is
        (true?
          @running)
       )
      
      (let [fire-request-thread (future
                                  (let [host "localhost"
                                        port @port-a
                                        request-method "POST"
                                        request-uri "/"
                                        certificate-config {:keystore-file-path "./resources/certificate/default_certificate.jks"
                                                            :keystore-password "ultras12"
                                                            :keystore-type nil
                                                            :ssl-context nil}
                                        request-body nil
                                        processed-reponse (reqs/fire-request
                                                            host
                                                            port
                                                            request-method
                                                            request-uri
                                                            certificate-config
                                                            request-body)
                                        {protocol :protocol
                                         server :server
                                         status-code :status-code
                                         status :status} processed-reponse]
                                    
                                    (is
                                      (nil?
                                        processed-reponse)
                                     )
                                    
                                   ))]
        
        (stop-server)
        
        (is
          (true?
            (.isClosed
              @server-socket))
         )
        
        (is
          (false?
            @running)
         )
        
        (is
          (future-cancelled?
            @main-thread)
         )
        
        (is
          (future-done?
            @main-thread)
         )
        
        (future-cancel
          fire-request-thread)
       )

     )
    
    (let [routing-fn nil
          default-response-headers nil
          port 9999
          https-conf nil
          public-dir nil
          result (start-server
                   routing-fn
                   default-response-headers
                   port
                   https-conf
                   public-dir)]
      
      (is
        (= @port-a
           9999)
       )
      
      (is
        (not
          (nil?
            @server-socket))
       )
      
      (is
        (instance?
          java.net.ServerSocket
          @server-socket)
       )
      
      (is
        (false?
          (.isClosed
            @server-socket))
       )
      
      (is
        (not
          (nil?
            @main-thread))
       )
      
      (is
        (not
          (future-cancelled?
            @main-thread))
       )
      
      (is
        (not
          (future-done?
            @main-thread))
       )
      
      (is
        (true?
          @running)
       )
      
      (let [host "localhost"
            port @port-a
            request-method "POST"
            request-uri "/"
            certificate-config nil
            request-body nil
            processed-reponse (reqs/fire-request
                                host
                                port
                                request-method
                                request-uri
                                certificate-config
                                request-body)
            {protocol :protocol
             server :server
             status-code :status-code
             status :status} processed-reponse]
        
        (is
          (= protocol
             "HTTP/1.1")
         )
        
        (is
          (= server
             "cljserver")
         )
        
        (is
          (= status-code
             "403")
         )
        
        (is
          (= status
             "Forbidden")
         )
        
       )
      
      (stop-server)
      
      (is
        (true?
          (.isClosed
            @server-socket))
       )
      
      (is
        (false?
          @running)
       )
      
      (is
        (future-cancelled?
          @main-thread)
       )
      
      (is
        (future-done?
          @main-thread)
       )
      
     )
    
   ))

