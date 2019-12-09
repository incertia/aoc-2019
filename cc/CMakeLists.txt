set (probs 001 002 003 004 005 006 009)

foreach (prob IN LISTS probs)
  add_executable (${prob} main.c ${prob}.c aoc.c intcode.c)
  target_compile_options (${prob}
    PRIVATE -std=c99 -Wall -pedantic -D_POSIX_C_SOURCE=200809L)
  if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    target_compile_options (${prob} PRIVATE -Werror)
  endif ()
endforeach (prob)
