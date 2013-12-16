SRC = .
SOURCES = $(shell ls $(SRC)/*.scala)
S = scala
SC = scalac
TARGET = parser
CP = $(TARGET):scalatest.jar
    SPEC = scala.RomanSpec

compile: $(SOURCES:.scala=.class)

%.class: %.scala
	@echo "Compiling $*.scala.."
	@$(SC) -cp $(CP) -d . $*.scala

test: compile
	@$(S) -cp $(CP) org.scalatest.tools.Runner -p . -o -s $(SPEC)

clean:
	@$(RM) $(SRC)/*.class
