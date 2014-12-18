CMAKE_MINIMUM_REQUIRED( VERSION 2.8 )
PROJECT( compiler )

IF( CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_CURRENT_BINARY_DIR )
#    FILE( REMOVE_RECURSE CMakeFiles )
#    FILE( REMOVE CMakeCache.txt )

    MESSAGE( FATAL_ERROR "Do not run cmake from the source directory !!!!")
ENDIF()


add_subdirectory( src/parser )
add_subdirectory( src/assembler )
add_subdirectory( src/vm )

ADD_EXECUTABLE( compiler
    src/main.cpp
)

TARGET_LINK_LIBRARIES( compiler
    parsers
    assembler
    vm
)

TARGET_INCLUDE_DIRECTORIES( compiler
    INTERFACE PUBLIC
        src/
)