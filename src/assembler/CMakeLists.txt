CMAKE_MINIMUM_REQUIRED( VERSION 2.8 )
PROJECT( assembler )

ADD_LIBRARY( assembler
    assembler.h
    assembler.cpp
    ast.h
)

TARGET_INCLUDE_DIRECTORIES( assembler
    INTERFACE PUBLIC
        ../
)