1) Copy the following files in this directory:
    ../pa6/interpreter.py
    ../pa6/grammar.py
    ../pa6/grammar_parser.py
    ../pa6/parser_generator.py
    ../pa6/util.py
    
2) Merge you library from ../pa6/library.164 with ours if you have customized yours.

3) Copy your CS164 grammar to pa7/cs164c.grm. You extends your old CS164 grammar
with native calls and OO method calls.

The browser files layout engine is implemented in the files within
tests/browser/ and is split across several *.164 files. You need to implement
Object:new within object.164 and the VBox height and layout algorithm within
vbox.164.

You can run the browser with ./browser.py. To load a specific TML file in the
browser, just specify it as a command-line argument:
  ./browser.py ./tests/tml/demo.tml

