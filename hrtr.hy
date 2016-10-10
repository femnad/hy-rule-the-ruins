#!/usr/bin/env hy
(import [json])
(import [os.path [expanduser]])
(import [time [sleep]])

(import [curses])
(import [requests])
(import [requests.auth [HTTPBasicAuth]])

(defn get-endpoint [credentials endpoint-type]
  (let [server (get credentials "server")]
    (.format "{}/api/v1/{}" server endpoint-type)))

(defn get-queue-endpoint [credentials]
  (get-endpoint credentials "register"))

(defn authenticated-get [credentials endpoint]
  (let [basic-auth (HTTPBasicAuth (get credentials "email")
                                           (get credentials "api-key"))]
    (requests.get endpoint :auth basic-auth)))

(defn authenticated-post [credentials endpoint data]
    (let [basic-auth (HTTPBasicAuth (get credentials "email")
                                           (get credentials "api-key"))]
      (requests.post endpoint
                     :auth basic-auth
                     :data data
                     :headers {"Content-Type"
                               "application/x-www-form-urlencoded"})))

(defn register-message-queue [credentials]
  (let [queue-endpoint (get-queue-endpoint credentials)
        response (authenticated-post credentials queue-endpoint
                                     {"event_types" "[\"message\"]"})
        json-response (.json response)]
    (list (map (fn [key] (get json-response key)) ["queue_id" "last_event_id"]))))

(defn get-events-from-queue [credentials queue-id last-event-id]
  (let [events-endpoint (get-endpoint credentials "events")
        endpoint (.format "{}?queue_id={}&last_event_id={}&dont_block=true" events-endpoint queue-id last-event-id)
        new-events (authenticated-get credentials endpoint)]
    (.json new-events)))

(defn format-message [message]
  (let [message-id (get message "id")
        message-data (get message "message")
        [sender stream topic content]
        (list (map (fn [k] (get message-data k))
                   ["sender_full_name" "display_recipient" "subject" "content"]))]
    [message-id (.format "{sender}:\n{stream} > {topic}\n{content}"
             :sender sender
             :stream (get-recipient stream)
             :topic topic
             :content content)]))

(defn get-new-messages [credentials queue-id last-event-id]
  (let [events (get-events-from-queue credentials queue-id last-event-id)]
    (get events "events")))

(defn send-private-message [credentials recipient content]
  (let [messages-endpoint (get-endpoint credentials "messages")
        message-payload (.format "type={}&to={}&content={}"
                                 "private" recipient content)]
    (authenticated-post credentials messages-endpoint message-payload)))

(defn process-messages [window messages &optional last-message-id]
  (if (empty? messages)
    (if (is None last-message-id)
      -1
      last-message-id)
    (let [[message-id formatted-message] (format-message (first messages))]
      (.addstr window formatted-message)
      (.addstr window "\n")
      (.refresh window)
      (process-messages window (list (rest messages)) message-id))))

(defn pre-message-loop [window credentials]
  (let [[queue-id last-event-id] (register-message-queue credentials)]
    (message-loop window credentials queue-id last-event-id)))

(defn message-loop [window credentials queue-id last-event-id]
  (let [new-last-message-id
        (process-messages window (get-new-messages credentials queue-id last-event-id))]
    (sleep 1)
    (message-loop window credentials queue-id new-last-message-id)))

(defn get-credentials [file-name]
  (with [f (open (expanduser file-name))]
        (let [creds-json (json.loads (.read f))
              [api-key email server] (map (fn [k] (get creds-json k))
                                          ["api-key" "email" "server"])]
          {"api-key" api-key "email" email "server" server})))

(defn get-recipient [recipient]
  (if (coll? recipient)
    (get (get recipient 0) "full_name")
    recipient))

(defmain [&rest args]
  (let [credentials (get-credentials "~/.hrtrrc")
        window (.initscr curses)]
    (do
     (.clear window)
     (.refresh window)
     (pre-message-loop window credentials))))
