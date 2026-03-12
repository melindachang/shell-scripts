#!/usr/bin/env -S steel --

(#%require-dylib "libsteel_audio_tags" (only-in extract-audio-tags
                                                regex-patch-audio-tag))

(require "steel/iterators")
(require "steel/result")
(require "srfi/srfi-28/format.scm")

(define g-supported-audio-file-types '("flac" "mp3"))

;; (non-comprehensive) mappings for find/replace patterns
(define g-regex-rules
  `((,(list->string '(#\[ #\x201C #\x201D #\x02DD #\x2033 #\x00BB #\x203A #\x00AB #\x2039 #\])) "\"") ; double quotes
    (,(list->string '(#\[ #\x2018 #\x2019 #\x2032 #\x0060 #\x00B4 #\])) "'") ; single quotes
    (,(list->string '(#\[ #\x2010 #\x2011 #\x2012 #\x2013 #\x2015 #\x2212 #\x058A #\x05BE #\])) "-"))) ; hyphens (dashes)

(define g-keys-to-sanitize
  '(originalalbumtitle
    originalartist
    albumtitlesortorder
    setsubtitle
    albumartistsortorder
    albumartist
    albumartists
    albumtitle
    tracktitle
    tracktitlesortorder
    tracksubtitle
    trackartist
    trackartistsortorder
    trackartists
    composer
    composersortorder))

;; sanitize-file : string? -> void?
(define (sanitize-file path)
  (regex-patch-audio-tag path
                         (map symbol->string g-keys-to-sanitize)
                         g-regex-rules))

;; is-audio? : string? -> bool?
(define (is-audio? path)
  (and (is-file? path)
       (member (path->extension path)
               g-supported-audio-file-types)))

;; traverse-files : string? -> void?
(define (traverse-files path)
  (cond
    [(is-dir? path)
     (let* ([entries (read-dir path)]
            [audio-files (filter is-audio? entries)])
       (when (not (null? audio-files))
             (for-each (λ (entry)
                       (sanitize-file entry))
                    audio-files))
       (for-each (λ (dir)
                    (when (is-dir? dir)
                          (traverse-files dir)))
                 entries))]
    [else
      (sanitize-file path)]))

(let ([args (list-tail (command-line) 2)])
  (when (null? args)
    (error! "Usage: sanitize_music.scm <file-or-directory...>"))
  (for-each (λ (arg)
               (unless (path-exists? arg)
                       (error! (format "Path not found: ~a" arg)))
               (traverse-files arg))
            args)
  (displayln "Sanitization complete."))
