set (cprobs 001 002 003 004 005 006 009 011 012 013 015 016)
set (cxxprobs 018)

add_library (aoc STATIC main.c aoc.c intcode.c)
if (CMAKE_BUILD_TYPE STREQUAL "Debug")
  target_compile_options (aoc PRIVATE -Werror -fsanitize=address)
  target_link_options (aoc PRIVATE -fsanitize=address)
endif ()

function (addprobs v ext std)
  foreach (prob IN LISTS ${v})
    add_executable (${prob} ${prob}.${ext})
    target_compile_options (${prob}
      PRIVATE -std=${std} -Wall -pedantic -D_POSIX_C_SOURCE=200809L)
    target_link_libraries (${prob} aoc)
    if (CMAKE_BUILD_TYPE STREQUAL "Debug")
      target_compile_options (${prob} PRIVATE -Werror -fsanitize=address)
      target_link_options (${prob} PRIVATE -fsanitize=address)
    endif ()
  endforeach (prob)
endfunction(addprobs)

addprobs (cprobs c c99)
addprobs (cxxprobs cpp c++11)
