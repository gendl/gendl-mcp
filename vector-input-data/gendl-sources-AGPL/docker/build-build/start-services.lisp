(in-package :common-lisp-user)


(flet ((true? (val) (and val (not (or (and (stringp val)
                                           (or (string-equal val "false")
                                               (string-equal val "0")
                                               (string-equal val "")
                                               (string-equal val "nil")))
                                      (and (numberp val) (zerop val)))))))
  (let ((start-http? (true? (uiop:getenv "START_HTTP")))
        (start-https? (true? (uiop:getenv "START_HTTPS")))
        (start-swank? (true? (uiop:getenv "START_SWANK")))
        (start-telnet? (true? (uiop:getenv "START_TELNET"))))

    (when start-http? (gendl:start-gendl!))
    (when start-https? (gwl:start-gwl :https? t))
    (when start-swank? (swank:create-server :port (parse-integer (uiop:getenv "SWANK_PORT"))
                                            :interface "0.0.0.0" :dont-close t))
    ;;

    (when start-telnet?
      (error "telnet not implemented yet")
      ;; when and if we get a working telent server.
      ;; (telnetd:telnetd :port (uiop:getenv "TELNET_PORT"))
      )


    (values)

    ))




