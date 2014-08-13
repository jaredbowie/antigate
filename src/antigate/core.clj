(ns antigate.core
  (:require [clj-http.client :as client])
  )

(defn- submit-captcha [user-key image-full-path]
   (client/post "http://antigate.com/in.php"
               {:multipart [{:name "key" :content user-key}
                            {:name "method" :content "post"}
                            {:name "file" :content (clojure.java.io/file image-full-path)}]})
  )

(defn- get-captcha-id
  "assumes post body returns OK|CAPTCHA_ID_HERE"
  [post-result-body]
  (let [captcha-id (subs post-result-body 3)]
    captcha-id
    )
  )

(defn- check-post-result [post-result-body]
  (cond
   (= "OK|" (subs post-result-body 0 3)) true
   (= "ERROR_WRONG_USER_KEY" post-result-body) "user authorization key is invalid (its length is not 32 bytes as it should be)"
   (= "ERROR_KEY_DOES_NOT_EXIST" post-result-body) "you have set wrong user authorization key in request"
   (= "ERROR_ZERO_BALANCE" post-result-body) "account has zero or negative balance"
   (= "ERROR_NO_SLOT_AVAILABLE" post-result-body) "no idle captcha workers are available at the moment, please try a bit later or try increasing your bid here"
   (= "ERROR_ZERO_CAPTCHA_FILESIZE" post-result-body) "the size of the captcha you are uploading is zero"
   (= "ERROR_TOO_BIG_CAPTCHA_FILESIZE" post-result-body) "your captcha size is exceeding 100kb limit"
   (= "ERROR_WRONG_FILE_EXTENSION" post-result-body) "your captcha file has wrong extension, the only allowed extensions are gif,jpg,jpeg,png"
   (= "ERROR_IMAGE_TYPE_NOT_SUPPORTED" post-result-body) "Could not determine captcha file type, only allowed formats are JPG, GIF, PNG"
   (= "ERROR_IP_NOT_ALLOWED" post-result-body) "Request with current account key is not allowed from your IP. Please refer to IP list section located here"
   :else "Other error"
   )
  )

(defn- retrieve-captcha-status [user-key captcha-id]
  (client/get "http://antigate.com/res.php" {:query-params {:key user-key
                                                            :action "get"
                                                            :id captcha-id
                                                            }})
  )

(defn- wait-for-result [user-key captcha-id]
  (loop [times 0
         again true
         error true
         the-result ""
         ]
    (if (or (> times 20) (false? again))
      [the-result error]
      (do
        (Thread/sleep 5000)
        (let [captcha-status-body (:body (retrieve-captcha-status user-key captcha-id))]
          (cond
           (= captcha-status-body "CAPCHA_NOT_READY") (recur (+ 1 times)
                                                        true
                                                        true
                                                        "CAPCHA_NOT_READY"
                                                        )
           (= (subs captcha-status-body 0 3) "OK|") (recur (+ 1 times)
                                                    false
                                                    false
                                                    (subs captcha-status-body 3)
                                                    )
           (= captcha-status-body "ERROR_KEY_DOES_NOT_EXIST") (recur (+ 1 times)
                                                                false
                                                                true
                                                                "ERROR_KEY_DOES_NOT_EXIST"
                                                        )
           (= captcha-status-body "ERROR_WRONG_ID_FORMAT") (recur (+ 1 times)
                                                             false
                                                             true
                                                             "ERROR_WRONG_ID_FORMAT"
                                                        )
           (= captcha-status-body "ERROR_CAPTCHA_UNSOLVABLE") (recur (+ 1 times)
                                                                false
                                                                true
                                                                "ERROR_CAPTCHA_UNSOLVABLE"
                                                        )
           )
          )
        )
      )
    )
  )

(defn get-load-info
"returns a map of the load xml as floats"
  []
  (let [body-http-resp (:body (client/get "http://antigate.com/load.php"))
        [waiting waitingru load minbid minbidru average-recognition-time average-recognition-timeru] (map #(read-string %) (drop 1 (re-find #"<RESPONSE>\n<waiting>(\d+)</waiting>\n<waitingRU>(\d+)</waitingRU>\n<load>([\d\.]+)</load>\n<minbid>([\d\.]+)</minbid>\n<minbidRU>([\d\.]+)</minbidRU>\n<averageRecognitionTime>([\d\.]+)</averageRecognitionTime>\n<averageRecognitionTimeRU>([\d\.]+)</averageRecognitionTimeRU>\n</RESPONSE>" body-http-resp)))]
    {:waiting waiting :waitingru waitingru :load load :minbid minbid :minbidru minbidru :average-recognition-time average-recognition-time :average-recgonition-timeru average-recognition-timeru}
    )
  )

(defn report-bad-captcha-result [key captcha-id]
  (client/get "http://antigate.com/res.php" {:query-params {:key key
                                                            :action "reportbad"
                                                            :id captcha-id
                                                            }})
  )

(defn retrieve-current-account-balance [key]
  (client/get "http://antigate.com/res.php" {:query-params {:key key
                                                            :action "getbalance"
                                                            }})
  )

(defn solve-captcha
  "returns a map of the result: see below"
  [user-key image-full-path]
  (let [post-result (submit-captcha user-key image-full-path)
        post-result-body (:body post-result)
        post-result-message (check-post-result post-result-body)
        ]
    (if (true? post-result-message)
      (let [id (get-captcha-id post-result-body)
            [the-result error] (wait-for-result user-key id)
            ]
        {:error error :result the-result}
        )
      {:error true :result post-result-message}
      )
    )
  )
