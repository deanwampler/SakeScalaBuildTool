# gnu make

SCALA_VERSION=2.12.4
SCALA_MINOR_VERSION=2.12
VERSION=2.0
CLASSES=target/scala-$(SCALA_MINOR_VERSION)/classes
CLASSPATH=$(CLASSES):lib/scalatest_2.12-3.0.4.jar:lib/embedded-interpreter-0.1-2.8.0.RC7.jar

all: clean test jars

clean:
	rm -rf target

SRCS := $(shell find src/main/scala -type f)
TEST_SRCS := $(shell find src/test/scala -type f)

compile: target_dir
	scalac -d $(PWD)/$(CLASSES) -classpath ${CLASSPATH} -unchecked -deprecation -feature $(SRCS)

test_compile:
	scalac -d $(PWD)/$(CLASSES) -classpath build:${CLASSPATH} -unchecked -deprecation -feature $(TEST_SRCS)

test: clean compile test_compile _test
_test:
	scala -classpath ${CLASSPATH} org.scalatest.tools.Runner

jars: lib_dir remove_jar
	cd $(CLASSES); \
		jar cf ../lib/$(SCALA_VERSION)/sake-$(SCALA_VERSION)-$(VERSION).jar -C . `find . -type f`
	jar cf lib/$(SCALA_VERSION)/sake-$(SCALA_VERSION)-$(VERSION)-src.jar -C . Makefile README.* sake.scala *LICENSE `find src spec -type f`

target_dir:
	mkdir -p $(CLASSES)

lib_dir:
	mkdir -p lib

remove_jar:
	rm -f lib/$(SCALA_VERSION)/sake-$(SCALA_VERSION)-$(VERSION)*.jar
