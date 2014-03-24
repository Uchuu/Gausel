all: compileSort copyJar

compileSort:
	cd ../sort/ ; sbt packageBin

copyJar:
	mkdir -p lib
	cp `find ../sort/ -iname "sort_*.jar"` lib/sortLib.jar

run:
	sbt run
