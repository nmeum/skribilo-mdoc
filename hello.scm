(use-modules (skribilo package man))

(document :title [cat]
          :mdoc-section 1
          :mdoc-desc [concatenate and print files]

  (section :title [Synposis]
   (man-name)
   (man-flags [benstuv])
   (man-arg))

  (section :title [Description]
   [The ,(man-name) utility reads files sequentially, writing them to the standard output.
The ,(man-arg [file]) operands are processed in command-line order.
If ,(man-arg [file]) is a single dash ,(bold [-]) or absent, ,(man-name) reads from the standard input.]

   (p [The options are as follows:])

   (itemize
     (item [,(man-flags [b]) Number the lines, but don't count blank lines.])
     (item [,(man-flags [e]) Print a dollar sign at the end of each line.])))

  (section :title [Description]
   [The ,(man-name) utility shall read files ,(bold [in sequence]) and shall write their contents to the standard output in the same sequence.]))
