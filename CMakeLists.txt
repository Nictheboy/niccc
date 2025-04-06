cmake_minimum_required(VERSION 3.16)

project(niccc)
enable_testing()

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# add_compile_options(-fsanitize=address)
# add_compile_options(-g)
# add_link_options(-fsanitize=address)

if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3")
endif()

if(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4 /w14640")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /O2")
endif()

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

if(NOT TARGET libniccc)
    set(LIB_SOURCES
        src/tokenizer_spec.cpp
    )
    if(WIN32)
        set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)
    endif()
    add_library(libniccc SHARED ${LIB_SOURCES})
    set_target_properties(libniccc PROPERTIES POSITION_INDEPENDENT_CODE ON)
    target_include_directories(libniccc PUBLIC include)
    install(TARGETS libniccc)
endif()

if(NOT TARGET niccc)
    set(EXEC_SOURCES
        src/main.cpp
    )
    add_executable(niccc ${EXEC_SOURCES})
    target_link_libraries(niccc libniccc)
    install(TARGETS niccc)
endif()
