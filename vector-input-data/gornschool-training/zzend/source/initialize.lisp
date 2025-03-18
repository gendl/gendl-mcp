(in-package :gdl-user)

(unless (uiop:getenv "CI_COMMIT_SHA") (training-common:initialize-all))
