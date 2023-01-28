;; Convert given PATTERN to pngs.
;; Following https://www.gimp.org/tutorials/Basic_Batch/ .
;; See also https://schemers.org/Documents/Standards/R5RS/HTML/ for basic Scheme functions.

;; TODO: This is unused, because of 2 TODOs below.
;; The ImageMagick conversion works reliably in batch mode, see convert-to-png.sh .

(define (convert-to-png pattern)
  (let* ((filelist (cadr (file-glob pattern 1))))
    (while (not (null? filelist))
      (let* ((filename (car filelist))
             (filename-without-ext (substring filename 0 (- (string-length filename) 4)))
             (export-filename (string-append filename-without-ext ".from-gimp.png"))
             (image (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
             ;; Necessary to correctly export images with multiple layers, like ButtonDisabled.xcf .
             ;; TODO: while it works, it seems to remove alpha from the layer (even it was already applied mask).
             (flattened-layer (gimp-image-flatten image))
             ;; Necessary to correctly export images with unapplied mask in xcf, like FrameThickWhite.xcf .
             ;; TODO: it fails to apply mask in resulting file.
             (gimp-layer-remove-mask flattened-layer MASK_APPLY)
             (drawable (car flattened-layer))
            )
        (gimp-message (string-append "Converting " filename " to " export-filename " using GIMP"))
        (gimp-file-save RUN-NONINTERACTIVE
                        image drawable export-filename export-filename)
        (gimp-image-delete image)
      )
      (set! filelist (cdr filelist))
    )
  )
)

(convert-to-png "*.xcf")
