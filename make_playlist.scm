#!/usr/bin/env -S steel --

(require "srfi/srfi-28/format.scm")

;; traverse-files : string? -> void?
(define (traverse-files path)
  void)

(let ([args (list-tail (command-line) 2)])
  (when (or (null? args)
            (empty? (filter is-dir? args)))
    (error! "Usage: make_playlist.scm [directory ...]"))
  (for-each (λ (arg)
               (unless (path-exists? arg)
                       (error! (format "Path not found: ~a" arg)))
               (traverse-files arg))
            args)
  (displayln "Playlisting complete."))
