set (cprobs 001 002 003 004 005 006 009 011 012 013 015 016 022 023)
set (cxxprobs 018 020)
set (gmpprobs 022)

add_library (aoc STATIC main.c aoc.c intcode.c)
target_compile_options (aoc
  PRIVATE -std=c99 -Wall -pedantic -D_POSIX_C_SOURCE=200809L)
target_compile_options (aoc PRIVATE $<$<CONFIG:RELEASE>:-O2>)
target_compile_options (aoc PRIVATE $<$<CONFIG:DEBUG>:-Werror
  -fsanitize=address>)
target_link_options (aoc PRIVATE $<$<CONFIG:DEBUG>:-fsanitize=address>)

function (addprobs v ext std)
  foreach (prob IN LISTS ${v})
    add_executable (${prob} ${prob}.${ext})
    target_compile_options (${prob}
      PRIVATE -std=${std} -Wall -pedantic -D_POSIX_C_SOURCE=200809L)
    target_link_libraries (${prob} aoc)
    target_compile_options (${prob} PRIVATE $<$<CONFIG:RELEASE>:-O2>)
    target_compile_options (${prob} PRIVATE $<$<CONFIG:DEBUG>:-Werror
      -fsanitize=address>)
    target_link_options (${prob} PRIVATE $<$<CONFIG:DEBUG>:-fsanitize=address>)
  endforeach (prob)
endfunction(addprobs)

addprobs (cprobs c c99)
addprobs (cxxprobs cpp c++17)

foreach (prob IN LISTS gmpprobs)
  target_link_options (${prob} PRIVATE -lgmp)
endforeach(prob)
