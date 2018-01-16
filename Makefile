all: x86.jar latc_x86_64

x86.jar:
	_JAVA_OPTIONS="-Xms512m"
	./sbt/bin/sbt package && cp target/scala-2.12/parser_2.12-0.1.0-SNAPSHOT.jar x86.jar

latc_x86_64:
	echo 'java -cp x86.jar:$$(cat target/streams/compile/dependencyClasspath/\$$global/streams/export) Assembly $$1' > latc_x86_64
	chmod +x latc_x86_64

clean:
	rm x86.jar latc_x86_64
