# Enable compilation database
set(CMAKE_EXPORT_COMPILE_COMMANDS ON CACHE BOOL "")

# Make implicit include directories explicit
string(REPLACE ":" ";" includes "$ENV{CMAKE_INCLUDE_PATH}")
file(STRINGS "$ENV{NIX_CC}/nix-support/orig-libc-dev" orig_libc_dev)
list(APPEND includes ${orig_libc_dev})
set(CMAKE_C_STANDARD_INCLUDE_DIRECTORIES ${includes} CACHE STRING "")
set(CMAKE_CXX_STANDARD_INCLUDE_DIRECTORIES ${includes} CACHE STRING "")
set(CMAKE_CUDA_STANDARD_INCLUDE_DIRECTORIES ${includes} CACHE STRING "")
