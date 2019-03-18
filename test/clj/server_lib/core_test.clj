(ns server-lib.core-test
  (:require [clojure.test :refer :all]
            [server-lib.core :refer :all]))

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

