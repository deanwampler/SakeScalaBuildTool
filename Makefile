
CLASSPATH=/Library/tools/scala/scala-specs/specs-1.4.1.jar:lib/jmock-2.5.1.jar:lib/cglib-nodep-2.1_3.jar:.

all: clean compile test jar

clean:
	rm -rf build

compile: build_dir
	scalac -d build -cp ${CLASSPATH} -unchecked -deprecation $$(find src spec -name '*.scala')

test:
	cd build; \
	for f in $$(find sake -name '*Spec.class'); do ff=$$(echo $${f%.class} | tr / .); echo $$ff; \
		scala -cp ${CLASSPATH} $$ff || exit $$?; done

jar: lib_dir remove_jar
	cd build; \
	jar cf ../lib/sake.jar -C . `find . -type f`
    
build_dir:
	mkdir -p build

lib_dir:
	mkdir -p lib

remove_jar:
	rm -f lib/sake.jar    