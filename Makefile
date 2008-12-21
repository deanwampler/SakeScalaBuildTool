
CLASSPATH=lib/specs-1.4.1.jar:lib/junit-4.4.jar

all: clean compile test jar

clean:
	rm -rf build

compile: build_dir
	scalac -d build -cp ${CLASSPATH} -unchecked -deprecation $$(find src spec -name '*.scala')

test:
	scala -cp ${CLASSPATH}:build bin/specrunner.scala

jar: lib_dir remove_jar
	cd build; \
	jar cf ../lib/sake.jar -C . `find . -type f`
    
build_dir:
	mkdir -p build

lib_dir:
	mkdir -p lib

remove_jar:
	rm -f lib/sake.jar    