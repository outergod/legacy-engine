;;;; Engine - package.lisp
;;;; Copyright (C) 2011  Alexander Kahl <e-user@fsfe.org>
;;;; This file is part of Engine.
;;;; Engine is free software; you can redistribute it and/or modify it
;;;; under the terms of the GNU Affero General Public License as
;;;; published by the Free Software Foundation; either version 3 of the
;;;; License, or (at your option) any later version.
;;;;
;;;; Engine is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

; FIXME upstream
(in-package :hunchentoot)
(export (list 'content-stream 'set-timeouts 'acceptor-listen-socket))

; FIXME upstream
(in-package :flexichain)
(export 'cursors)

(in-package :engine-system)

(set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)

(defpackage :websocket
  (:use :cl :hunchentoot :cl-ppcre :alexandria :flexi-streams :trivial-utf-8 :bordeaux-threads)
  (:import-from :ironclad :digest-stream)
  (:export :websocket-acceptor :websocket-ssl-acceptor
           :websocket-handle-handshake :websocket-send-message :websocket-send-term
           :*websocket-handlers* :default-websocket-handler :*websocket-stream* :*websocket-stream-mutex*))

(defpackage :socket.io
  (:use :cl :hunchentoot :cl-ppcre :uuid :websocket :bordeaux-threads :json :alexandria)
  (:export :define-socket.io-handler :define-socket.io-event-handler :socket.io-on
           :getf-session :*socket.io-session*))

(defpackage engine
  (:use :cl :hunchentoot :osicat :osicat-sys :cl-who :websocket :socket.io
        :bordeaux-threads :cl-containers :trivial-garbage)
  (:import-from :css-lite :css)
  (:import-from :osicat-posix :exit)
  (:import-from :alexandria :required-argument :define-constant :when-let :if-let)
  (:shadowing-import-from :parenscript :chain :size)
  (:shadowing-import-from :flexichain :insert-sequence)
  (:use :parenscript :flexichain))
