# KBKB: Knowledge Base

A static site generated from a directory of TeX files. Currently hosted [here](https://web.stanford.edu/~ksb/phil/doc/phil.html).

## HOW TO USE

- Edit TeX files, found in `doc/`.
- `stack build --exec "kbkb <FLAGS>"` to compile and run
  - flag `gen` populates `site/`
  - flag `sync` ssh copies `site/` to where the website is hosted
  - flag `pdf` will generate pdfs (currently broken)
  - flag `clear` will wipe out the local and remote `site/` folders

### Caveats
- Don't use unicode, apostrophes, periods, slashes in titles of files (so do
  `Exercise 1-1`, rather than `Exercise 1.1`)
- Leading digits of file titles are stripped (these are just used to order)

## DONE
- Convert directory of Latex into HTML + PDF
- Image/Tikz diagram, table of contents, footnotes, bibliography rendering
- Collapsible subsections
- Up / Prev / Next HTML buttons
- Preview links on hover
- List of other pages that link to the current page
  - Go back to the specific part that linked it
  - Optionally include a description of the context of the link
- Metadata parsing (file named `0` in a given folder)
    - Specify title, unique identifier, and tag.
    - Tags like "Def" / "Exercise" used to color the section
- Different interconvertable representations of the KB
  1. Directories of TeX files (good for writing long form)
  2. Normalized database (good for editing connections)
  3. Denormalized database (good for querying)

## TODO
- Parse markdown OR LaTeX
- Sections should be able to have text interspersed with nested sections, rather
  than only being able to have it at the start
- LaTeX errors are silent: prefer to fail noisily rather than just have blank
  space in final result
- Content: Brandom lectures, math textbooks
- Make more path-related things case-insensitive
- Automatically detect broken links
- Automatically verify no unicode, periods, apostrophes, slashes in titles of
  files
- Cache hash of tex before computing pdf, check before recomputing
- move buttons to a header
- 'Home page' (with overall ToC, search bar, references, ABOUT, filters)
    - search
    - filter by definition / proposition
- comment system + postgres
- mouseover delay 1 second (so it doesn't pop up immediately) and better
  placement of the pop up.
- Make the LaTeX compilation more parameterized (possibly with a TEX metadatum)
- Better document the code
- Integrate automatic flashcard generation from source
- Modify the regex that operates on internal links so that it matches even when
  there is no comment (separated by `|`), that way, we add the `.html` even in
  the other case (needed when linking to a non-leaf-node of the directory tree).

## Inspirations
- [Workflowy](https://www.workflowy.com/features/) / [Nested](https://orteil.dashnet.org/nested)
- [Stacks project](https://stacks.math.columbia.edu/)
- [Maddox's personal website](https://maddo.xxx/)