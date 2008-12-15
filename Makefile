
CLASSPATH=/Library/tools/scala/scala-specs/specs-1.4.1.jar:lib/jmock-2.5.1.jar:lib/cglib-nodep-2.1_3.jar:.

all: clean compile test
    
test:
	cd build; \
	for f in $$(find sake -name '*Spec.class'); do ff=$$(echo $${f%.class} | tr / .); echo $$ff; \
		scala -cp ${CLASSPATH} $$ff || exit $$?; done

compile: build_dir
	scalac -d build -cp ${CLASSPATH} -unchecked -deprecation $$(find src spec -name '*.scala')

file:
	scalac -d build -cp ${CLASSPATH} -unchecked -deprecation src/sake/util/File.scala spec/sake/util/FileSpec.scala
	scala -cp build:${CLASSPATH} sake.util.FileSpec

files:
	scalac -d build -cp ${CLASSPATH} -unchecked -deprecation src/sake/util/File.scala src/sake/util/Files.scala spec/sake/util/FilesSpec.scala spec/sake/util/FakeFile.scala
	scala -cp build:${CLASSPATH} sake.util.FilesSpec

clean:
	rm -rf build

build_dir:
	mkdir -p build
