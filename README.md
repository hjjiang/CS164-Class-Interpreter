CS164-Class-Interpreter
=======================

CS164 Class Interpreter

A language that is similar to python
Added support for TML = tiny markup language
-Available Tags:
 <HBox>: horizontal box
 <VBox>: vertical box
 <Link ref="">
 <Text>
 <Script src=""> loads scripts from a file
 <Img src="">

Sample bquery:
<Vbox onclick="bq('fontclass'):background('green')" background="yellow" klass="fontclass">
 Make me yellow
</Vbox>

Running ./browser.py will run a basic demo
Running ./browser.py FILENAME will run a specific file
Run ./browser.py ./demo.tml  for something interesting (though it uses alot of memory and might segfault)