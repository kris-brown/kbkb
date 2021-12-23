# kbkb

A static site generated from a directory of TeX files
## HOW TO USE

 - Edit TeX files, found in `doc/`.
 - `stack build --exec kbkb` to compile and run (populates `site/`)
 - call with command line arg `pdf` if you want to generate pdfs too
   (takes long currently b/c we don't check whether the pdf has changed)
 - `rsync -rv site/ ksb@rice.stanford.edu:afs-home/WWW/phil --exclude '*._0.html' --exclude '*._1.html' --exclude '*._2.html'`
 - Sometimes need to clear: `src/clear.sh` and `ssh ksb@rice.stanford.edu rm -r afs-home/WWW/phil`

 - Caveats
  - Don't use unicode in titles of files
  - Leading digits of file titles are stripped (these are just used to order)

## DONE
- Convert directory of Latex into HTML + PDF
- Image/Tikz rendering
- Table of contents rendering
- Collapsible subsections
- Up / Prev / Next HTML buttons
- Preview on hover
- List of other pages that link to the current page
  - Go back to the specific part that linked it
  - Optionally include a description of the context of the link
- keyword parsing at the top (TAGS, TEX)
    - TAGS *could* be like "def" / "exercise"
    - TEX *could* specify nondefault things to compile LaTeX with
## TODO
- color code background based on tag
- Cache hash of tex before computing pdf, check before recomputing
- move buttons to a header
- 'Home page' (with overall ToC, search bar, references, ABOUT, filters)
    - search
    - filter by definition / proposition
- comment system + postgres
- mouseover delay 1 second (so it doesn't pop up immediately) and better placement of the pop up.