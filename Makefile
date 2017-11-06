# gnu make

CLASSPATH=build:lib/specs_2.8.0.RC7-1.6.5-SNAPSHOT.jar:lib/embedded-interpreter-0.1-2.8.0.RC7.jar:lib/junit-4.5.jar
SCALA_VERSION=2.8.0.RC7
VERSION=1.1

all: clean test jars

clean:
	rm -rf build

# For some reason, you still have to list sources, even with the -sourcepath src option.
# We try to order them by dependencies, so that the compiler is happy.
SRCS = \
	src/sake/util/*.scala \
	src/sake/environment/*.scala \
	src/sake/command/Result.scala src/sake/command/*.scala \
	src/sake/command/builtin/*.scala \
	src/sake/target/*.scala \
	src/sake/Project.scala src/sake/Main.scala
SPEC_SRCS = \
	spec/sake/util/*.scala \
	spec/sake/environment/*.scala \
	spec/sake/command/*.scala \
	spec/sake/command/builtin/*.scala \
	spec/sake/target/*.scala \
	spec/sake/*.scala

compile: build_dir
	scalac -d build -classpath ${CLASSPATH} -unchecked -deprecation $(SRCS)

test_compile:
	scalac -d build -classpath build:${CLASSPATH} -unchecked -deprecation $(SPEC_SRCS)

test: clean compile test_compile _test
_test:
	scala -classpath ${CLASSPATH} bin/specrunner.scala

jars: lib_dir remove_jar
	cd build; \
		jar cf ../lib/$(SCALA_VERSION)/sake-$(SCALA_VERSION)-$(VERSION).jar -C . `find . -type f`
	jar cf lib/$(SCALA_VERSION)/sake-$(SCALA_VERSION)-$(VERSION)-src.jar -C . Makefile README.* sake.scala *LICENSE `find src spec -type f`

build_dir:
	mkdir -p build

lib_dir:
	mkdir -p lib

remove_jar:
	rm -f lib/$(SCALA_VERSION)/sake-/$(SCALA_VERSION)-$(VERSION).jar
	rm -f lib/$(SCALA_VERSION)/sake-/$(SCALA_VERSION)-$(VERSION)-src.jar
