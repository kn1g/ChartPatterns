CC = g++
CXXFLAGS = -std=c++17 -Wall -Wextra -O2
INCLUDES = -I./src
SOURCES = src/SHSDetector.cpp src/ISHSDetector.cpp src/FastFind.cpp src/FastFindII.cpp src/test_implementations.cpp
TARGET = test_implementations

# Get R configuration for linking
R_HOME=$(shell R RHOME)
R_LDFLAGS=$(shell ${R_HOME}/bin/R CMD config --ldflags)

# Sources and objects
SRCS = src/findPatterns.cpp src/FastFindII.cpp src/SHSDetector.cpp src/ISHSDetector.cpp src/safeLinearInterpolation.cpp
OBJS = $(SRCS:.cpp=.o)
TEST_SRC = src/test_findPatterns.cpp
TEST_OBJ = $(TEST_SRC:.cpp=.o)

# Target executable
TARGET = test_findPatterns

all: $(TARGET)

$(TARGET): $(TEST_OBJ) $(OBJS)
	$(CXX) -o $@ $^ $(R_LDFLAGS)

# Rule for compiling .cpp files
%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -f $(OBJS) $(TEST_OBJ) $(TARGET)

run: $(TARGET)
	./$(TARGET)

# Dependencies
src/test_findPatterns.o: src/test_findPatterns.cpp src/PatternDetector.hpp
src/findPatterns.o: src/findPatterns.cpp src/PatternDetector.hpp
src/FastFindII.o: src/FastFindII.cpp
src/SHSDetector.o: src/SHSDetector.cpp src/PatternDetector.hpp
src/ISHSDetector.o: src/ISHSDetector.cpp src/PatternDetector.hpp
src/safeLinearInterpolation.o: src/safeLinearInterpolation.cpp

.PHONY: all clean run 