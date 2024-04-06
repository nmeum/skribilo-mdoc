(use-modules (skribilo package man))

(document :title [cat]
          :mdoc-section 1
          :mdoc-desc [concatenate and print files]

  (section :title [Synposis]
   (man-name)
   (man-flags #\a #\b #\c)

   (p [Max Mustermann hat mal ,(bold [etwas]) kluges gesagt:])

   (blockquote [Das hier ist eine Blockquote foo bar])

   (p [Here be dragons]))

  (section :title [Description]
   [The ,(man-name) utility shall read files ,(bold [in sequence]) and shall write their contents to the standard output in the same sequence.]))
