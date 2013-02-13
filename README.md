Gausel
======

Solves a linear system symbolically and produces a LaTeX file containing all the information linked to the resolution as new commands.
The matrix coefficients can either be zeros, symbols (which are assumed to be different from 0) or/and integer/rational values.

Use sbt 0.12.1 to compile the source. Generate the jar with:
```
cd trunk/
sbt
> one-jar
```

The jar will be in
> target/scala_2.9.2/gausel_2.9.2-1.1-onejar.jar

Gausel takes files in a special syntax as input, take a look at the examples in examples/.
You can put LaTeX in the input file, as in examples/testwithtext.
It will be put in the body of the resulting LaTeX file.
You can also only specify the linear system (as in examples/testtextfree), the body of the LaTeX file will be empty.
The syntax itself is pretty much self explanatory.

Try it without doing anything with
```
java -jar release/v1.1/gausel.jar releases/v1.1/examples/testwithtext
```

Do not hesitate to contact the developper for questions / bugs / other things.


Changelog:

1.1:
- now uses sbt 0.12.1 with scala 2.9.2.
- now runs on windows, although the ansi escape sequence are not recognized (no color and weird 03331;1m appear in the terminal).
- less verbose.