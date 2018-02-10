TESTS="set test in assembly := {}" # we don't want any tests to run on make
JAVA=/usr/lib/jvm/java-1.8.0-openjdk-amd64/ # the environment must use java 1.8
joos:
	sbt -java-home $(JAVA) $(TESTS) assembly

