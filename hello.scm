(use-modules (skribilo package man))
(use-modules (skribilo package slide))

(document :title [cat]
          :mdoc-section 1
          :mdoc-desc [concatenate and print files]

  (chapter :title [Synposis]
   [Here be dragons])

  (chapter :title [Description]
   [The ,(man-name) utility shall read files in sequence and shall write their contents to the standard output in the same sequence.]))
