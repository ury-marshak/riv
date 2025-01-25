#lang racket/gui

(require racket/draw racket/class racket/file)

;; State to hold the list of images and the current index
(define current-image #f)
(define image-list '())
(define current-index 0)

;; Define a frame for the application
(define frame
  (new frame%
       [label "Image Viewer"]
       [width 800]
       [height 600]))

;; Key event handler for navigation
(define  (my-key-handler canvas event)
  (printf "my-key-handler event: ~a\n" event)
  (define key (send event get-key-code))
  (cond
    [(or (eq? key 'q) (eq? key 'escape)) (send frame close)]
    [(eq? key 'left)
     (when (> current-index 0)
       (set! current-index (sub1 current-index))
       (update-image))]
    [(eq? key 'right)
     (when (< current-index (sub1 (length image-list)))
       (set! current-index (add1 current-index))
       (update-image))]))


(define image-canvas% 
  (class canvas%
    (super-new)
    (define/override (on-char event)
      (my-key-handler this event))
    (define/override (on-paint)
      (when (and current-image (path? current-image))
        (define bmp (make-object bitmap% current-image))
        (send (send this get-dc) draw-bitmap bmp 0 0)))
    ))

;; Set up the key handler after canvas creation
(define image-canvas 
  (new image-canvas%
       [parent frame]
       [enabled #t]))

;; Function to update the displayed image
(define (update-image)
  (when (and image-list (not (null? image-list)))
    (set! current-image (list-ref image-list current-index))
    (printf "current-image: ~a\n" current-image)
    (send image-canvas refresh)
    )
  )

;; Load images from a directory
(define (load-images dir)
  (let ((all-files (directory-list dir #:build? #t)))
    (set! image-list
          (filter (lambda (path-elm)
                    (member (path-get-extension path-elm) '(#".jpg" #".png" #".jpeg")))
                  all-files))
    (set! current-index 0)
    (update-image)))

;; Main function
(define (main)
  (define args (current-command-line-arguments))
  (cond
    [(= (vector-length args) 1)
     (define dir (vector-ref args 0))
     (when (directory-exists? dir)
       (load-images dir)
       (printf "loaded image-list: ~a\n" (length image-list))
       (send frame show #t)
        )]
    [else
     (printf "Usage: racket main.rkt <directory-path>\n")]))

;; Run the program
(main)
